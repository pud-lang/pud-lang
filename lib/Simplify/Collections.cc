#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"

namespace Pud::AST {

// 处理元组表达式。
void SimplifyVisitor::visit(TupleExpr* expr) {
  for (auto& i : expr->items) {
    // 对每个元素调用 transform 方法进行转换，其中 true 参
    // 数表示在某些情况下（例如 isinstance 构造）需要类型信息。
    transform(i, true);
  }
}

// 处理列表表达式。
void SimplifyVisitor::visit(ListExpr* expr) {
  for (auto& i : expr->items) {
    transform(i);
  }
}

// 处理集合表达式。
void SimplifyVisitor::visit(SetExpr* expr) {
  for (auto& i : expr->items) {
    transform(i);
  }
}

// 处理字典表达式。
void SimplifyVisitor::visit(DictExpr* expr) {
  for (auto& i : expr->items) {
    transform(i);
  }
}

/// 处理生成器表达式。
/// @example (lists and sets):
///   `[i+a for i in j if a]` -> ```gen = List()
///                                 for i in j: if a: gen.append(i+a)```
/// Generators are transformed to lambda calls.
/// @example
///   `(i+a for i in j if a)` -> ```def _lambda(j, a):
///                                   for i in j: yield i+a
///                                 _lambda(j, a).__iter__()```
void SimplifyVisitor::visit(GeneratorExpr* expr) {
  // 初始化一个用于存储语句的向量。
  std::vector<StmtPtr> stmts;

  // 克隆生成器表达式中的循环。
  auto loops = clone_nop(expr->loops);

  // 判断是否可以对列表生成器进行优化，条件是只有一个循环且没有任何条件表达式。
  bool can_optimize = expr->kind == GeneratorExpr::ListGenerator &&
                      loops.size() == 1 && loops[0].conds.empty();
  if (can_optimize) {
    auto iter = transform(loops[0].gen);
    IdExpr* id;
    if (iter->get_call() && (id = iter->get_call()->expr->get_id())) {
      // Turn off this optimization for static items
      can_optimize &=
          !startswith(id->value, "std.internal.types.range.staticrange");
      can_optimize &= !startswith(id->value, "statictuple");
    }
  }

  // 初始化指向最内层语句块的指针。
  SuiteStmt* prev = nullptr;
  // 避免支配变量名称的冲突。
  auto avoid_domination = true;
  std::swap(avoid_domination, ctx->avoid_domination);
  // 转换生成器体。
  auto suite = transform_generator_body(loops, prev);
  ExprPtr var = N<IdExpr>(ctx->cache->get_temporary_var("gen"));
  if (expr->kind == GeneratorExpr::ListGenerator) {
    // List comprehensions
    std::vector<ExprPtr> args;
    prev->stmts.push_back(N<ExprStmt>(
        N<CallExpr>(N<DotExpr>(clone(var), "append"), clone(expr->expr))));
    auto no_opt_stmt = N<SuiteStmt>(
        N<AssignStmt>(clone(var), N<CallExpr>(N<IdExpr>("List"))), suite);
    if (can_optimize) {
      // seqassert(suite->get_suite() && !suite->get_suite()->stmts.empty() &&
      //               CAST(suite->get_suite()->stmts[0], ForStmt),
      //           "bad comprehension transformation");
      auto optimize_var = ctx->cache->get_temporary_var("i");
      auto opt_suite = clone(suite);
      CAST(opt_suite->get_suite()->stmts[0], ForStmt)->iter =
          N<IdExpr>(optimize_var);

      auto opt_stmt = N<SuiteStmt>(
          N<AssignStmt>(N<IdExpr>(optimize_var), clone(expr->loops[0].gen)),
          N<AssignStmt>(clone(var),
                        N<CallExpr>(N<IdExpr>("List"),
                                    N<CallExpr>(N<DotExpr>(
                                        N<IdExpr>(optimize_var), "__len__")))),
          opt_suite);
      result_expr = transform(N<IfExpr>(
          N<CallExpr>(N<IdExpr>("hasattr"), clone(expr->loops[0].gen),
                      N<StringExpr>("__len__")),
          N<StmtExpr>(opt_stmt, clone(var)), N<StmtExpr>(no_opt_stmt, var)));
    } else {
      result_expr = transform(N<StmtExpr>(no_opt_stmt, var));
    }
  } else if (expr->kind == GeneratorExpr::SetGenerator) {
    // Set comprehensions
    stmts.push_back(
        transform(N<AssignStmt>(clone(var), N<CallExpr>(N<IdExpr>("Set")))));
    prev->stmts.push_back(N<ExprStmt>(
        N<CallExpr>(N<DotExpr>(clone(var), "add"), clone(expr->expr))));
    stmts.push_back(transform(suite));
    result_expr = N<StmtExpr>(stmts, transform(var));
  } else {
    // Generators: converted to lambda functions that yield the target
    // expression
    prev->stmts.push_back(N<YieldStmt>(clone(expr->expr)));
    stmts.push_back(suite);

    auto anon = make_anon_fn(stmts);
    if (auto call = anon->get_call()) {
      // seqassert(!call->args.empty() &&
      // call->args.back().value->get_ellipsis(),
      //           "bad lambda: {}", *call);
      call->args.pop_back();
    } else {
      anon = N<CallExpr>(anon);
    }
    result_expr = anon;
  }
  std::swap(avoid_domination, ctx->avoid_domination);
}

/// 处理字典生成器表达式。
/// @example
///   `{i+a: j+1 for i in j if a}` -> ```gen = Dict()
///                                      for i in j: if a: gen.__setitem__(i+a,
///                                      j+1)```
void SimplifyVisitor::visit(DictGeneratorExpr* expr) {
  SuiteStmt* prev = nullptr;
  auto avoid_domination = true;
  std::swap(avoid_domination, ctx->avoid_domination);
  auto suite = transform_generator_body(expr->loops, prev);

  std::vector<StmtPtr> stmts;
  ExprPtr var = N<IdExpr>(ctx->cache->get_temporary_var("gen"));
  stmts.push_back(
      transform(N<AssignStmt>(clone(var), N<CallExpr>(N<IdExpr>("Dict")))));
  prev->stmts.push_back(
      N<ExprStmt>(N<CallExpr>(N<DotExpr>(clone(var), "__setitem__"),
                              clone(expr->key), clone(expr->expr))));
  stmts.push_back(transform(suite));
  result_expr = N<StmtExpr>(stmts, transform(var));
  std::swap(avoid_domination, ctx->avoid_domination);
}

/// 转换生成器体。
/// @example
///   `for i in j if a for k in i if a if b` ->
///   `for i in j: if a: for k in i: if a: if b: [prev]`
auto SimplifyVisitor::transform_generator_body(
    const std::vector<GeneratorBody>& loops, SuiteStmt*& prev) -> StmtPtr {
  StmtPtr suite = N<SuiteStmt>();
  StmtPtr new_suite = nullptr;
  prev = dynamic_cast<SuiteStmt*>(suite.get());
  for (auto& l : loops) {
    new_suite = N<SuiteStmt>();
    auto next_prev = dynamic_cast<SuiteStmt*>(new_suite.get());

    auto for_stmt = N<ForStmt>(l.vars->clone(), l.gen->clone(), new_suite);
    prev->stmts.push_back(for_stmt);
    prev = next_prev;
    for (auto& cond : l.conds) {
      new_suite = N<SuiteStmt>();
      next_prev = dynamic_cast<SuiteStmt*>(new_suite.get());
      prev->stmts.push_back(N<IfStmt>(cond->clone(), new_suite));
      prev = next_prev;
    }
  }
  return suite;
}

}  // namespace Pud::AST