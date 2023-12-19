#include <string>
#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"

namespace Pud::AST {

void SimplifyVisitor::visit(UnaryExpr* expr) { transform(expr->expr); }

// 转换二元表达式（如 a + b, c && d），特别处理 is 操作符。
void SimplifyVisitor::visit(BinaryExpr* expr) {
  // 先转换左操作数 expr->lexpr。如果操作符是 is，则允许类型作为操作数。
  transform(expr->lexpr, startswith(expr->op, "is"));
  // 保存当前是否在条件表达式上下文的状态。
  auto tmp = ctx->is_conditional_expr;
  // 对于 && 和 || 操作，将右操作数的转换视为条件性的（因为这些是短路操作符）。
  ctx->is_conditional_expr = expr->op == "&&" || expr->op == "||";
  // 然后转换右操作数 expr->rexpr。
  transform(expr->rexpr, startswith(expr->op, "is"));
  // 恢复原始的条件表达式上下文状态。
  ctx->is_conditional_expr = tmp;
}

// 转换链式二元表达式
//   `a <= b <= c` -> `(a <= (chain := b)) and (chain <= c)`
// 这种转换方法保证了表达式的逻辑一致性和执行效率。
void SimplifyVisitor::visit(ChainBinaryExpr* expr) {
  // 首先检查表达式数量是否至少为2。
  // seqassert(expr->exprs.size() >= 2, "not enough expressions in
  // ChainBinaryExpr");
  std::vector<ExprPtr> items;
  std::string prev;
  // 通过赋值将链式比较转换为逻辑与表达式，确保每个表达式只执行一次。
  for (int i = 1; i < expr->exprs.size(); i++) {
    auto l = prev.empty() ? clone(expr->exprs[i - 1].second) : N<IdExpr>(prev);
    prev = ctx->generate_canonical_name("chain");
    auto r = (i + 1 == expr->exprs.size())
                 ? clone(expr->exprs[i].second)
                 : N<StmtExpr>(N<AssignStmt>(N<IdExpr>(prev),
                                             clone(expr->exprs[i].second)),
                               N<IdExpr>(prev));
    items.emplace_back(N<BinaryExpr>(l, expr->exprs[i].first, r));
  }

  ExprPtr final = items.back();
  for (auto i = items.size() - 1; i-- > 0;) {
    final = N<BinaryExpr>(items[i], "&&", final);
  }
  result_expr = transform(final);
}

// 转换索引表达式（如 a[b]），特别处理 Tuple 和 Static 类型。
void SimplifyVisitor::visit(IndexExpr* expr) {
  // 特殊处理 tuple 类型的索引，如 Tuple[T1, T2]。
  if (expr->expr->is_id("tuple") || expr->expr->is_id(TYPE_TUPLE)) {
    auto* t = expr->index->get_tuple();
    expr->expr = NT<IdExpr>(TYPE_TUPLE);
  } else if (expr->expr->is_id("Static")) {
    // 对 Static 类型进行检查，确保其为 int 或 str 类型。
    if (!expr->index->is_id("int") && !expr->index->is_id("str")) {
      Err(Error::BAD_STATIC_TYPE, expr->index);
    }
    expr->mark_type();
    return;
  } else {
    transform(expr->expr, true);
  }

  // 索引表达式IndexExpr[i1, ..., iN]可能被内部表示为
  // IndexExpr[TupleExpr[i1, ..., iN]]，需要对每个元素 i 进行转换。
  std::vector<ExprPtr> items;
  bool is_tuple = expr->index->get_tuple();
  if (auto t = expr->index->get_tuple()) {
    items = t->items;
  } else {
    items.push_back(expr->index);
  }
  for (auto& i : items) {
    if (i->get_list() && expr->expr->is_type()) {
      // Special case: `A[[A, B], C]` -> `A[Tuple[A, B], C]` (e.g., in
      // `Function[...]`)
      i = N<IndexExpr>(N<IdExpr>(TYPE_TUPLE),
                       N<TupleExpr>(i->get_list()->items));
    }
    transform(i, true);
  }
  if (expr->expr->is_type()) {
    result_expr = N<InstantiateExpr>(expr->expr, items);
    result_expr->mark_type();
  } else {
    expr->index =
        (!is_tuple && items.size() == 1) ? items[0] : N<TupleExpr>(items);
  }
}

// 转换实例化表达式（如类型实例化）。
// 这个方法用于处理泛型和其他需要类型参数的复杂表达式。
void SimplifyVisitor::visit(InstantiateExpr* expr) {
  // 转换类型表达式
  transform_type(expr->type_expr);
  // 遍历并转换类型参数
  for (auto& tp : expr->type_params) {
    transform(tp, true);
  }
}

}  // namespace Pud::AST