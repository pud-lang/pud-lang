#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

namespace Pud::AST {

/// Typecheck try-except statements. Handle Python exceptions separately.
/// @example
///   ```try: ...
///      except python.Error as e: ...
///      except PyExc as f: ...
///      except ValueError as g: ...
///   ``` -> ```
///      try: ...
///      except ValueError as g: ...                   # ValueError
///      except PyExc as exc:
///        while True:
///          if isinstance(exc.pytype, python.Error):  # python.Error
///            e = exc.pytype; ...; break
///          f = exc; ...; break                       # PyExc
///          raise```
void TypecheckVisitor::visit(TryStmt *stmt) {
  ctx->block_level++;
  transform(stmt->suite);
  ctx->block_level--;

  std::vector<TryStmt::Catch> catches;
  auto pyVar = ctx->cache->get_temporary_var("pyexc");
  auto pyCatchStmt = N<WhileStmt>(N<BoolExpr>(true), N<SuiteStmt>());

  auto done = stmt->suite->is_done();
  for (auto &c : stmt->catches) {
    transform(c.exc);
    if (c.exc && c.exc->type->is("pyobj")) {
      // Transform python.Error exceptions
      if (!c.var.empty()) {
        c.suite = N<SuiteStmt>(
            N<AssignStmt>(N<IdExpr>(c.var), N<DotExpr>(N<IdExpr>(pyVar), "pytype")),
            c.suite);
      }
      c.suite =
          N<IfStmt>(N<CallExpr>(N<IdExpr>("isinstance"),
                                N<DotExpr>(N<IdExpr>(pyVar), "pytype"), clone(c.exc)),
                    N<SuiteStmt>(c.suite, N<BreakStmt>()), nullptr);
      pyCatchStmt->suite->get_suite()->stmts.push_back(c.suite);
    } else if (c.exc && c.exc->type->is("std.internal.types.error.PyError")) {
      // Transform PyExc exceptions
      if (!c.var.empty()) {
        c.suite =
            N<SuiteStmt>(N<AssignStmt>(N<IdExpr>(c.var), N<IdExpr>(pyVar)), c.suite);
      }
      c.suite = N<SuiteStmt>(c.suite, N<BreakStmt>());
      pyCatchStmt->suite->get_suite()->stmts.push_back(c.suite);
    } else {
      // Handle all other exceptions
      transform_type(c.exc);
      if (!c.var.empty()) {
        // Handle dominated except bindings
        auto changed = in(ctx->cache->replacements, c.var);
        while (auto s = in(ctx->cache->replacements, c.var))
          c.var = s->first, changed = s;
        if (changed && changed->second) {
          auto update =
              N<AssignStmt>(N<IdExpr>(format("{}.__used__", c.var)), N<BoolExpr>(true));
          update->set_update();
          c.suite = N<SuiteStmt>(update, c.suite);
        }
        if (changed)
          c.exc->set_attr(ExprAttr::Dominated);
        auto val = ctx->find(c.var);
        if (!changed)
          val = ctx->add(TypecheckItem::Var, c.var, c.exc->get_type());
        unify(val->type, c.exc->get_type());
      }
      ctx->block_level++;
      transform(c.suite);
      ctx->block_level--;
      done &= (!c.exc || c.exc->is_done()) && c.suite->is_done();
      catches.push_back(c);
    }
  }
  if (!pyCatchStmt->suite->get_suite()->stmts.empty()) {
    // Process PyError catches
    auto exc = NT<IdExpr>("std.internal.types.error.PyError");
    pyCatchStmt->suite->get_suite()->stmts.push_back(N<ThrowStmt>(nullptr));
    TryStmt::Catch c{pyVar, transform_type(exc), pyCatchStmt};

    auto val = ctx->add(TypecheckItem::Var, pyVar, c.exc->get_type());
    unify(val->type, c.exc->get_type());
    ctx->block_level++;
    transform(c.suite);
    ctx->block_level--;
    done &= (!c.exc || c.exc->is_done()) && c.suite->is_done();
    catches.push_back(c);
  }
  stmt->catches = catches;
  if (stmt->finally) {
    ctx->block_level++;
    transform(stmt->finally);
    ctx->block_level--;
    done &= stmt->finally->is_done();
  }

  if (done)
    stmt->set_done();
}

/// Transform `raise` statements.
/// @example
///   `raise exc` -> ```raise __internal__.set_header(exc, "fn", "file", line, col)```
void TypecheckVisitor::visit(ThrowStmt *stmt) {
  if (!stmt->expr) {
    stmt->set_done();
    return;
  }

  transform(stmt->expr);

  if (!(stmt->expr->get_call() &&
        stmt->expr->get_call()->expr->is_id("__internal__.set_header:0"))) {
    stmt->expr = transform(N<CallExpr>(
        N<DotExpr>(N<IdExpr>("__internal__"), "set_header"), stmt->expr,
        N<StringExpr>(ctx->get_realization_base()->name),
        N<StringExpr>(stmt->get_source_info().file), N<IntExpr>(stmt->get_source_info().line),
        N<IntExpr>(stmt->get_source_info().column)));
  }
  if (stmt->expr->is_done())
    stmt->set_done();
}


}