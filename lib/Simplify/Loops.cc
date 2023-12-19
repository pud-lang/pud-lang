#include <string>
#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Parse/Parser.h"
#include "Pud/Simplify/Simplify.h"

namespace Pud::AST {

// 确保 continue 语句在循环内部使用。
void SimplifyVisitor::visit(ContinueStmt* stmt) {
  // 检查当前上下文的基础结构（函数、类）是否包含循环结构。
  if (!ctx->get_base()->get_loop()) {
    // 如果不在循环内，则抛出 EXPECTED_LOOP 错误，指出
    // continue 语句的位置和类型。
    Err(Error::EXPECTED_LOOP, stmt, "continue");
  }
}

// 确保 break 语句在循环内部，并转换 break 语句以支持循环的 else 子句。
//   `break` -> `no_break = False; break`
void SimplifyVisitor::visit(BreakStmt* stmt) {
  // 首先检查 break 是否在循环内。
  if (!ctx->get_base()->get_loop()) {
    Err(Error::EXPECTED_LOOP, stmt, "break");
  }
  // 如果循环存在 else 子句，那么在执行 break 前添加一个赋值语句
  // （如 no_break = False），以标记循环提前退出。
  if (!ctx->get_base()->get_loop()->break_var.empty()) {
    result_stmt =
        N<SuiteStmt>(transform(N<AssignStmt>(
                         N<IdExpr>(ctx->get_base()->get_loop()->break_var),
                         N<BoolExpr>(false))),
                     N<BreakStmt>());
  }
}

/// 转换 while 循环，特别是包含 else 子句的情况。
///   `while cond: ...`           ->  `while cond.__bool__(): ...`
///   `while cond: ... else: ...` -> ```no_break = True
///                                     while cond.__bool__():
///                                       ...
///                                     if no_break: ...```
void SimplifyVisitor::visit(WhileStmt* stmt) {
  // 如果存在 else 子句，创建一个变量（如 no_break = True）来跟踪
  // 循环是否提前退出。
  std::string break_var;
  if (stmt->else_suite && stmt->else_suite->first_in_block()) {
    // no_break = True
    break_var = ctx->cache->get_temporary_var("no_break");
    prepend_stmts->push_back(
        transform(N<AssignStmt>(N<IdExpr>(break_var), N<BoolExpr>(true))));
  }

  ctx->enter_conditional_block();
  ctx->get_base()->loops.push_back({break_var, ctx->scope.blocks, {}});
  // 转换循环条件以确保其返回布尔值（通过调用 __bool__()）。
  stmt->cond = transform(N<CallExpr>(N<DotExpr>(stmt->cond, "__bool__")));
  transform_conditional_scope(stmt->suite);

  // Complete while-else clause
  if (stmt->else_suite && stmt->else_suite->first_in_block()) {
    result_stmt =
        N<SuiteStmt>(N<WhileStmt>(*stmt),
                     N<IfStmt>(transform(N<IdExpr>(break_var)),
                               transform_conditional_scope(stmt->else_suite)));
  }

  ctx->leave_conditional_block();
  // 如果存在 else 子句，则根据 no_break 变量的值来执行 else 子句。
  for (auto& var : ctx->get_base()->get_loop()->seen_vars) {
    ctx->find_dominating_binding(var);
  }
  ctx->get_base()->loops.pop_back();
}

/// 转换 for 循环，包括处理 else 子句和循环变量的赋值。
///   `for i, j in it: ...`        -> ```for tmp in it:
///                                        i, j = tmp
///                                        ...```
///   `for i in it: ... else: ...` -> ```no_break = True
///                                      for i in it: ...
///                                      if no_break: ...```
void SimplifyVisitor::visit(ForStmt* stmt) {
  // 转换和检查 for 循环的装饰器
  stmt->decorator = transform_for_decorator(stmt->decorator);

  std::string break_var;
  // 对循环的迭代器进行转换，例如，如果迭代器是一个复杂表达式，这一步
  // 骤确保它被正确解析。
  stmt->iter = transform(stmt->iter);

  // 如果存在 else 子句，同样创建一个 no_break 变量来跟踪循环是否提前退出。
  StmtPtr assign = nullptr;
  if (stmt->else_suite && stmt->else_suite->first_in_block()) {
    break_var = ctx->cache->get_temporary_var("no_break");
    assign = transform(N<AssignStmt>(N<IdExpr>(break_var), N<BoolExpr>(true)));
  }

  ctx->enter_conditional_block();
  ctx->get_base()->loops.push_back({break_var, ctx->scope.blocks, {}});
  std::string var_name;
  // 如果循环变量是一个简单的标识符，直接处理。
  if (auto i = stmt->var->get_id()) {
    auto val = ctx->add_var(i->value,
                            var_name = ctx->generate_canonical_name(i->value),
                            stmt->var->get_source_info());
    val->avoid_domination = ctx->avoid_domination;
    transform(stmt->var);
    stmt->suite = transform(N<SuiteStmt>(stmt->suite));
  } else {
    // 如果循环变量不是一个简单的标识符，如 for i, j in ...，则创建一个临时变量
    // 来存储迭代值，并将其解构到实际的循环变量中。
    var_name = ctx->cache->get_temporary_var("for");
    auto val = ctx->add_var(var_name, var_name, stmt->var->get_source_info());
    auto var = N<IdExpr>(var_name);
    std::vector<StmtPtr> stmts;
    // Add for_var = [for variables]
    stmts.push_back(N<AssignStmt>(stmt->var, clone(var)));
    stmt->var = var;
    stmts.push_back(stmt->suite);
    stmt->suite = transform(N<SuiteStmt>(stmts));
  }

  // 如果存在 else 子句，构建一个完整的 if 语句来在循环完成后检查 no_break
  // 变量， 以确定是否执行 else 块。
  if (stmt->else_suite && stmt->else_suite->first_in_block()) {
    result_stmt =
        N<SuiteStmt>(assign, N<ForStmt>(*stmt),
                     N<IfStmt>(transform(N<IdExpr>(break_var)),
                               transform_conditional_scope(stmt->else_suite)));
  }

  ctx->leave_conditional_block(&(stmt->suite->get_suite()->stmts));
  // 确保循环结束后，循环变量不会被错误地访问或支配。
  for (auto& var : ctx->get_base()->get_loop()->seen_vars) {
    ctx->find_dominating_binding(var);
  }
  ctx->get_base()->loops.pop_back();
}

/// 转换和检查 for 循环的装饰器，例如 OpenMP 装饰器。
///   `@par(num_threads=2, openmp="schedule(static)")` ->
///   `for_par(num_threads=2, schedule="static")`
auto SimplifyVisitor::transform_for_decorator(const ExprPtr& decorator)
    -> ExprPtr {
  if (!decorator) {
    return nullptr;
  }
  ExprPtr callee = decorator;
  if (auto c = callee->get_call()) {
    callee = c->expr;
  }
  if (!callee || !callee->is_id("par")) {
    Err(Error::LOOP_DECORATOR, decorator);
  }
  std::vector<CallExpr::Arg> args;
  std::string openmp;
  std::vector<CallExpr::Arg> omp;
  if (auto c = decorator->get_call()) {
    for (auto& a : c->args) {
      if (a.name == "openmp" ||
          (a.name.empty() && openmp.empty() && a.value->get_string())) {
        omp =
            Parse::parse_openmp(ctx->cache, a.value->get_string()->get_value(),
                                a.value->get_source_info());
      } else {
        args.push_back({a.name, transform(a.value)});
      }
    }
  }
  for (auto& a : omp) {
    args.push_back({a.name, transform(a.value)});
  }
  return N<CallExpr>(transform(N<IdExpr>("for_par")), args);
}

}  // namespace Pud::AST