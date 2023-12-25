#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"

namespace Pud::AST {

/// Transform asserts.
/// @example
///   `assert foo()` ->
///   `if not foo(): raise __internal__.seq_assert([file], [line], "")`
///   `assert foo(), msg` ->
///   `if not foo(): raise __internal__.seq_assert([file], [line], str(msg))`
/// Use `seq_assert_test` instead of `seq_assert` and do not raise anything
/// during unit testing (i.e., when the enclosing function is marked with
/// `@test`).
void SimplifyVisitor::visit(AssertStmt* stmt) {
  ExprPtr msg = N<StringExpr>("");
  if (stmt->message)
    msg = N<CallExpr>(N<IdExpr>("str"), clone(stmt->message));
  auto test =
      ctx->in_function() && (ctx->get_base()->attributes &&
                            ctx->get_base()->attributes->has(Attr::Test));
  auto ex = N<CallExpr>(
      N<DotExpr>("__internal__", test ? "seq_assert_test" : "seq_assert"),
      N<StringExpr>(stmt->get_source_info().file),
      N<IntExpr>(stmt->get_source_info().line), msg);
  auto cond = N<UnaryExpr>("!", clone(stmt->expr));
  if (test) {
    result_stmt = transform(N<IfStmt>(cond, N<ExprStmt>(ex)));
  } else {
    result_stmt = transform(N<IfStmt>(cond, N<ThrowStmt>(ex)));
  }
}

void SimplifyVisitor::visit(TryStmt* stmt) {
  transform_conditional_scope(stmt->suite);
  for (auto& c : stmt->catches) {
    ctx->enter_conditional_block();
    if (!c.var.empty()) {
      c.var = ctx->generate_canonical_name(c.var);
      ctx->add_var(ctx->cache->rev(c.var), c.var, c.suite->get_source_info());
    }
    transform(c.exc, true);
    transform_conditional_scope(c.suite);
    ctx->leave_conditional_block();
  }
  transform_conditional_scope(stmt->finally);
}

void SimplifyVisitor::visit(ThrowStmt* stmt) { transform(stmt->expr); }

/// Transform with statements.
/// @example
///   `with foo(), bar() as a: ...` ->
///   ```tmp = foo()
///      tmp.__enter__()
///      try:
///        a = bar()
///        a.__enter__()
///        try:
///          ...
///        finally:
///          a.__exit__()
///      finally:
///        tmp.__exit__()```
void SimplifyVisitor::visit(WithStmt* stmt) {
  // seqassert(!stmt->items.empty(), "stmt->items is empty");
  std::vector<StmtPtr> content;
  for (auto i = stmt->items.size(); i-- > 0;) {
    std::string var = stmt->vars[i].empty()
                          ? ctx->cache->get_temporary_var("with")
                          : stmt->vars[i];
    content = std::vector<StmtPtr>{
        N<AssignStmt>(N<IdExpr>(var), clone(stmt->items[i])),
        N<ExprStmt>(N<CallExpr>(N<DotExpr>(var, "__enter__"))),
        N<TryStmt>(
            !content.empty() ? N<SuiteStmt>(content) : clone(stmt->suite),
            std::vector<TryStmt::Catch>{},
            N<SuiteStmt>(
                N<ExprStmt>(N<CallExpr>(N<DotExpr>(var, "__exit__")))))};
  }
  result_stmt = transform(N<SuiteStmt>(content));
}

}  // namespace Pud::AST
