#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

namespace Pud::AST {

/// Call `ready` and `notReady` depending whether the provided static expression
/// can be
/// evaluated or not.
template <typename TT, typename TF>
auto evaluateStaticCondition(const ExprPtr& cond, TT ready, TF notReady) {
  seqassertn(cond->is_static(), "not a static condition");
  if (cond->static_value.evaluated) {
    bool isTrue = false;
    if (cond->static_value.type == StaticValue::STRING)
      isTrue = !cond->static_value.get_string().empty();
    else
      isTrue = cond->static_value.get_int();
    return ready(isTrue);
  } else {
    return notReady();
  }
}

/// Typecheck if expressions. Evaluate static if blocks if possible.
/// Also wrap the condition with `__bool__()` if needed and wrap both
/// conditional expressions. See @c wrap_expr for more details.
void TypecheckVisitor::visit(IfExpr* expr) {
  transform(expr->cond);

  // Static if evaluation
  if (expr->cond->is_static()) {
    result_expr = evaluateStaticCondition(
        expr->cond,
        [&](bool isTrue) {
          LOG_TYPECHECK("[static::cond] {}: {}", get_source_info(), isTrue);
          return transform(isTrue ? expr->ifexpr : expr->elsexpr);
        },
        [&]() -> ExprPtr {
          // Check if both subexpressions are static; if so, this if expression
          // is also static and should be marked as such
          auto i = transform(clone(expr->ifexpr));
          auto e = transform(clone(expr->elsexpr));
          if (i->is_static() && e->is_static()) {
            expr->static_value.type = i->static_value.type;
            unify(expr->type,
                  ctx->get_type(expr->static_value.type == StaticValue::INT
                                   ? "int"
                                   : "str"));
          }
          return nullptr;
        });
    if (result_expr)
      unify(expr->type, result_expr->get_type());
    else
      unify(expr->type, ctx->get_unbound());
    return;
  }

  transform(expr->ifexpr);
  transform(expr->elsexpr);
  // Add __bool__ wrapper
  while (expr->cond->type->get_class() && !expr->cond->type->is("bool"))
    expr->cond = transform(N<CallExpr>(N<DotExpr>(expr->cond, "__bool__")));
  // Add wrappers and unify both sides
  wrap_expr(expr->elsexpr, expr->ifexpr->get_type(), nullptr,
           /*allowUnwrap*/ false);
  wrap_expr(expr->ifexpr, expr->elsexpr->get_type(), nullptr,
           /*allowUnwrap*/ false);
  unify(expr->type, expr->ifexpr->get_type());
  unify(expr->type, expr->elsexpr->get_type());

  if (expr->cond->is_done() && expr->ifexpr->is_done() && expr->elsexpr->is_done())
    expr->set_done();
}

/// Typecheck if statements. Evaluate static if blocks if possible.
/// Also wrap the condition with `__bool__()` if needed.
/// See @c wrap_expr for more details.
void TypecheckVisitor::visit(IfStmt* stmt) {
  transform(stmt->cond);

  // Static if evaluation
  if (stmt->cond->is_static()) {
    result_stmt = evaluateStaticCondition(
        stmt->cond,
        [&](bool isTrue) {
          LOG_TYPECHECK("[static::cond] {}: {}", get_source_info(), isTrue);
          auto t = transform(isTrue ? stmt->if_suite : stmt->else_suite);
          return t ? t : transform(N<SuiteStmt>());
        },
        [&]() -> StmtPtr { return nullptr; });
    return;
  }

  while (stmt->cond->type->get_class() && !stmt->cond->type->is("bool"))
    stmt->cond = transform(N<CallExpr>(N<DotExpr>(stmt->cond, "__bool__")));
  ctx->block_level++;
  transform(stmt->if_suite);
  transform(stmt->else_suite);
  ctx->block_level--;

  if (stmt->cond->is_done() && (!stmt->if_suite || stmt->if_suite->is_done()) &&
      (!stmt->else_suite || stmt->else_suite->is_done()))
    stmt->set_done();
}

}  // namespace Pud::AST