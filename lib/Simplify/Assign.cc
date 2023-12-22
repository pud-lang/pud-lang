#include <memory>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"

namespace Pud::AST {

/// 转换赋值表达式，特别是处理海象操作符（walrus，即 :=）。
///   `(expr := var)` -> `var = expr; var`
void SimplifyVisitor::visit(AssignExpr* expr) {
  // 这行断言确保只支持简单的赋值表达式。
  // seqassert(expr->var->get_id(), "only simple assignment expression are
  // supported");
  // 创建一个新的 AssignStmt，其左侧是变量，右侧是表达式。
  StmtPtr s = N<AssignStmt>(clone(expr->var), expr->expr);
  // 设置 avoid_domination 为 false 并与上下文中的值交换。
  // 海象操作符总是泄露其作用域。
  auto avoid_domination = false;
  std::swap(avoid_domination, ctx->avoid_domination);
  // 如果处于条件表达式中，则进入条件块，转换赋值语句和变量，并处理 SuiteStmt。
  if (ctx->is_conditional_expr) {
    ctx->enter_conditional_block();
    transform(s);
    transform(expr->var);
    SuiteStmt* suite = s->get_suite();
    if (!suite) {
      s = N<SuiteStmt>(s);
      suite = s->get_suite();
    }
    ctx->leave_conditional_block(&suite->stmts);
  } else {
    // 如果不在条件表达式中，只转换赋值语句和变量。
    s = transform(s);
    transform(expr->var);
  }
  // 恢复原来的 avoid_domination 值。
  // 设置结果为一个包含赋值语句和变量的 StmtExpr。
  std::swap(avoid_domination, ctx->avoid_domination);
  result_expr = N<StmtExpr>(std::vector<StmtPtr>{s}, expr->var);
}

// 处理不同类型的赋值语句。
void SimplifyVisitor::visit(AssignStmt* stmt) {
  // 初始化一个用于存储转换后的语句的向量。
  std::vector<StmtPtr> stmts;
  // 检查赋值语句的右侧是否是二元表达式，如果是且是原地操作（如
  // +=），则进行转换。
  if (stmt->rhs && stmt->rhs->get_binary() &&
      stmt->rhs->get_binary()->in_place) {
    // Update case: a += b
    // seqassert(!stmt->type, "invalid AssignStmt {}", stmt->to_string());
    stmts.push_back(transform_assignment(stmt->lhs, stmt->rhs, nullptr, true));
  } else if (stmt->type) {
    // 如果赋值语句有类型注解，则按类型进行转换。
    // Type case: `a: T = b, c` (no unpacking)
    stmts.push_back(transform_assignment(stmt->lhs, stmt->rhs, stmt->type));
  } else {
    // 否则，使用 unpack_assignments 处理解构赋值。
    unpack_assignments(stmt->lhs, stmt->rhs, stmts);
  }
  // 如果结果只有一个语句，直接返回这个语句；
  // 否则，返回一个包含所有语句的 SuiteStmt。
  result_stmt = stmts.size() == 1 ? stmts[0] : N<SuiteStmt>(stmts);
}

/// 转换删除操作。
///   `del a`    -> `a = type(a)()` and remove `a` from the context
///   `del a[x]` -> `a.__delitem__(x)`
void SimplifyVisitor::visit(DelStmt* stmt) {
  // 如果是删除索引操作（如 del a[x]），则转换为 __delitem__ 方法调用。
  if (auto idx = stmt->expr->get_index()) {
    result_stmt = N<ExprStmt>(transform(
        N<CallExpr>(N<DotExpr>(idx->expr, "__delitem__"), idx->index)));
  } else if (auto ei = stmt->expr->get_id()) {
    // 如果是删除变量（如 del a），则进行一系列更复杂的操作，包括标记变
    // 量为删除，检查作用域，从上下文中移除变量。
    // Assign `a` to `type(a)()` to mark it for deletion
    result_stmt = N<AssignStmt>(transform(clone(stmt->expr)),
                                transform(N<CallExpr>(N<CallExpr>(
                                    N<IdExpr>("type"), clone(stmt->expr)))));
    result_stmt->get_assign()->set_update();

    // Allow deletion *only* if the binding is dominated
    auto val = ctx->find(ei->value);
    if (!val)
      Err(Error::ID_NOT_FOUND, ei, ei->value);
    if (ctx->scope.blocks != val->scope)
      Err(Error::DEL_NOT_ALLOWED, ei, ei->value);
    ctx->remove(ei->value);
  } else {
    Err(Error::DEL_INVALID, stmt);
  }
}

/// 转换不同类型的赋值操作。
///   `a[x] = b`    -> `a.__setitem__(x, b)`
///   `a.x = b`     -> @c AssignMemberStmt
///   `a: type` = b -> @c AssignStmt
///   `a = b`       -> @c AssignStmt or @c UpdateStmt (see below)
auto SimplifyVisitor::transform_assignment(ExprPtr lhs, ExprPtr rhs,
                                           ExprPtr type, bool must_exist)
    -> StmtPtr {
  // 检查左侧表达式是否是索引操作，如 a[x]。
  if (auto idx = lhs->get_index()) {
    // Case: a[x] = b
    // seqassert(!type, "unexpected type annotation");
    if (auto b = rhs->get_binary()) {
      // 如果右侧是二元操作（如+=），并且需要原地
      // 更新（in_place），则进行特殊处理。
      if (must_exist && b->in_place && !b->rexpr->get_id()) {
        // 生成一个临时变量，用于存储索引值。
        auto var = ctx->cache->get_temporary_var("assign");
        // seqassert(rhs->get_binary(), "not a bin");
        // 将原地更新转换为一系列操作：先将索引值赋给临时变量，然后使用
        // __setitem__ 方法进行更新。这样做是为了处理原地操作符，如 a[x] += b。
        return transform(N<SuiteStmt>(
            N<AssignStmt>(N<IdExpr>(var), idx->index),
            N<ExprStmt>(N<CallExpr>(
                N<DotExpr>(idx->expr, "__setitem__"), N<IdExpr>(var),
                N<BinaryExpr>(N<IndexExpr>(idx->expr->clone(), N<IdExpr>(var)),
                              b->op, b->rexpr, true)))));
      }
    }
    // 对于普通的索引赋值，如 a[x] = b，转换为 __setitem__ 方法调用。
    return transform(N<ExprStmt>(
        N<CallExpr>(N<DotExpr>(idx->expr, "__setitem__"), idx->index, rhs)));
  }

  // 检查左侧表达式是否是成员访问操作，如 a.x。
  if (auto dot = lhs->get_dot()) {
    // Case: a.x = b
    // seqassert(!type, "unexpected type annotation");
    // 转换成员访问的表达式部分。
    transform(dot->expr, true);
    // 如果当前上下文是类成员，并且左侧表达式是类的实例（self），
    // 则记录下这次成员赋值操作。这有助于后续处理类成员的推断。
    auto deduced = ctx->get_class_base()
                       ? ctx->get_class_base()->deduced_members
                       : nullptr;
    if (deduced && dot->expr->is_id(ctx->get_base()->self_name) &&
        !in(*deduced, dot->member))
      deduced->push_back(dot->member);
    // 将成员赋值转换为 AssignMemberStmt
    return N<AssignMemberStmt>(dot->expr, dot->member, transform(rhs));
  }

  // 检查左侧表达式是否是有效的标识符。如果不是，报告错误。
  // Case: a (: t) = b
  auto e = lhs->get_id();
  if (!e)
    Err(Error::ASSIGN_INVALID, lhs);

  // 检查是否存在名称冲突。这是为了避免在局部作用域中错误地覆盖全局变量。
  // Example:
  // x = 1
  // def foo():
  //   print(x)  # x is seen here
  //   x = 2     # this should error
  if (in(ctx->seen_global_identifiers[ctx->get_base_name()], e->value))
    Err(Error::ASSIGN_LOCAL_REFERENCE,
        ctx->seen_global_identifiers[ctx->get_base_name()][e->value], e->value);

  auto val = ctx->find(e->value);
  must_exist |= val && val->no_shadow && !ctx->is_outer(val);
  if (must_exist) {
    // 查找当前变量名的绑定，如果需要的话，进行更深入的作用域搜索。
    val = ctx->find_dominating_binding(e->value);
    // 如果找到了有效的绑定，并且这个绑定是一个变量，且不在外部作用域中，
    // 则创建一个更新语句，否则报告错误。
    if (val && val->is_var() && !ctx->is_outer(val)) {
      auto s = N<AssignStmt>(transform(lhs, false), transform(rhs));
      if (ctx->get_base()->attributes &&
          ctx->get_base()->attributes->has(Attr::Atomic))
        s->set_atomic_update();
      else
        s->set_update();
      return s;
    } else {
      Err(Error::ASSIGN_LOCAL_REFERENCE, e, e->value);
    }
  }

  // 转换右侧表达式和类型（如果存在）。
  transform(rhs, true);
  transform_type(type, false);

  // 生成赋值语句的规范名称，并创建赋值语句。
  auto canonical = ctx->generate_canonical_name(e->value);
  auto assign = N<AssignStmt>(N<IdExpr>(canonical), rhs, type);
  val = nullptr;
  // 根据右侧表达式的类型，将这个赋值添加到上下文中。
  if (rhs && rhs->is_type()) {
    val = ctx->add_type(e->value, canonical, lhs->get_source_info());
  } else {
    val = ctx->add_var(e->value, canonical, lhs->get_source_info());
    // 处理静态类型和避免作用域支配的情况。
    if (auto st = get_static_generic(type.get()))
      val->static_type = st;
    if (ctx->avoid_domination)
      val->avoid_domination = true;
  }
  // 清理全局标识符，以避免潜在的名称冲突。
  ctx->seen_global_identifiers[ctx->get_base_name()].erase(e->value);

  // 在JIT模式下将所有toplevel顶层变量注册为全局变量
  // 判断是否应该将变量视为全局变量，并在必要时添加到全局上下文中。
  bool is_global =
      (ctx->cache->is_jit && val->is_global() && !val->is_generic()) ||
      (canonical == VAR_ARGV);
  if (is_global && !val->is_generic())
    ctx->cache->add_global(canonical);

  return assign;
}

/// 用于将复杂的赋值表达式（如 Python 风格的多元赋值或解构赋值）分解成一系列
/// 简单的赋值表达式。
/// (e.g., `a = b`, `a.x = b`, or `a[x] = b`).
/// @example
///   `(a, b) = c`     -> `a = c[0]; b = c[1]`
///   `a, b = c`       -> `a = c[0]; b = c[1]`
///   `[a, *x, b] = c` -> `a = c[0]; x = c[1:-1]; b = c[-1]`.
///   `a, b = c, d + foo()` -> `assign = (c, d + foo); a = assign[0]; b =
///   assign[1]`.
/// Each assignment is unpacked recursively to allow cases like `a, (b, c) = d`.
void SimplifyVisitor::unpack_assignments(const ExprPtr& lhs, ExprPtr rhs,
                                         std::vector<StmtPtr>& stmts) {
  // 初始化 left_side，一个存储左侧表达式的向量。
  std::vector<ExprPtr> left_side;
  if (auto et = lhs->get_tuple()) {
    // 如果 lhs 是元组（例如 (a, b)），将其元素添加到 left_side。
    // 为了后续处理每个元素的赋值。
    // Case: (a, b) = ...
    for (auto& i : et->items)
      left_side.push_back(i);
  } else if (auto el = lhs->get_list()) {
    // 如果 lhs 是列表（例如 [a, b]），也将其元素添加到 left_side。
    // 为了后续处理每个元素的赋值。
    // Case: [a, b] = ...
    for (auto& i : el->items)
      left_side.push_back(i);
  } else {
    // 如果 lhs 既不是元组也不是列表，则将其视为简单赋值（如 a = b），
    // 并调用 transform_assignment 进行转换。
    // Case: simple assignment (a = b, a.x = b, or a[x] = b)
    stmts.push_back(transform_assignment(clone(lhs), clone(rhs)));
    return;
  }

  // 获取右侧表达式的源代码信息。
  auto src_pos = rhs->get_source_info();
  if (!rhs->get_id()) {
    // 如果 rhs 不是简单的标识符（例如一个复杂的表达式或函数调用），
    // 则创建一个临时变量来存储这个表达式的值。这是为了避免重复计算 rhs。
    auto var = ctx->cache->get_temporary_var("assign");
    ExprPtr new_rhs = N<IdExpr>(src_pos, var);
    stmts.push_back(transform_assignment(new_rhs, clone(rhs)));
    rhs = new_rhs;
  }

  // 处理解构赋值:
  // 遍历 left_side 中的每个元素，直到遇到带 * 的表达式（如果有的话）。
  // 对于每个元素，创建一个索引表达式（例如 rhs[st]），然后递归调
  // 用 unpack_assignments。
  size_t st = 0;
  for (; st < left_side.size(); st++) {
    if (left_side[st]->get_star())
      break;
    // Transformation: `leftSide_st = rhs[st]` where `st` is static integer
    auto right_side =
        N<IndexExpr>(src_pos, clone(rhs), N<IntExpr>(src_pos, st));
    // `(a, (b, c)) = d)`
    unpack_assignments(left_side[st], right_side, stmts);
  }
  // 如果存在带 * 的表达式，如 *x，则使用切片表达式来处理（例如 rhs[1:-1]）。
  if (st < left_side.size() && left_side[st]->get_star()) {
    // StarExpr becomes SliceExpr (e.g., `b` in `(a, *b, c) = d` becomes
    // `d[1:-2]`)
    auto right_side = N<IndexExpr>(
        src_pos, clone(rhs),
        N<SliceExpr>(src_pos, N<IntExpr>(src_pos, st),
                     // this slice is either [st:] or [st:-lhs_len + st + 1]
                     left_side.size() == st + 1
                         ? nullptr
                         : N<IntExpr>(src_pos, -left_side.size() + st + 1),
                     nullptr));
    unpack_assignments(left_side[st]->get_star()->what, right_side, stmts);
    st += 1;
    // 对于之后的元素，使用负索引（例如 rhs[-1]）来访问 rhs 的剩余部分。
    for (; st < left_side.size(); st++) {
      // 如果再次遇到 StarExpr，则报告错误，因为 Python 语法不允许多个 *。
      if (left_side[st]->get_star())
        Err(Error::ASSIGN_MULTI_STAR, left_side[st]);
      right_side =
          N<IndexExpr>(src_pos, clone(rhs),
                       N<IntExpr>(src_pos, -int(left_side.size() - st)));
      unpack_assignments(left_side[st], right_side, stmts);
    }
  }
}

}  // namespace Pud::AST