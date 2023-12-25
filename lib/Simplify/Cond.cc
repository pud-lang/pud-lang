#include <string>
#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"

namespace Pud::AST {

void SimplifyVisitor::visit(IfExpr* expr) {
  // C++ call order is not defined; make sure to transform the conditional first
  transform(expr->cond);
  auto tmp = ctx->is_conditional_expr;
  // Ensure that ifexpr and elsexpr are set as a potential short-circuit
  // expressions. Needed to ensure that variables defined within these
  // expressions are properly checked for their existence afterwards (e.g., `x`
  // will be created within `a if cond else (x := b)` only if `cond` is not
  // true)
  ctx->is_conditional_expr = true;
  transform(expr->ifexpr);
  transform(expr->elsexpr);
  ctx->is_conditional_expr = tmp;
}

void SimplifyVisitor::visit(IfStmt* stmt) {
  // seqassert(stmt->cond, "invalid if statement");
  transform(stmt->cond);
  // Ensure that conditional suites are marked and transformed in their own
  // scope
  transform_conditional_scope(stmt->if_suite);
  transform_conditional_scope(stmt->else_suite);
}

/// Simplify match statement by transforming it into a series of conditional
/// statements.
/// @example
///   ```match e:
///        case pattern1: ...
///        case pattern2 if guard: ...
///        ...``` ->
///   ```_match = e
///      while True:  # used to simulate goto statement with break
///        [pattern1 transformation]: (...; break)
///        [pattern2 transformation]: if guard: (...; break)
///        ...
///        break  # exit the loop no matter what```
/// The first pattern that matches the given expression will be used; other
/// patterns will not be used (i.e., there is no fall-through). See @c
/// transform_pattern for pattern transformations
void SimplifyVisitor::visit(MatchStmt* stmt) {
  auto var = ctx->cache->get_temporary_var("match");
  auto result = N<SuiteStmt>();
  result->stmts.push_back(N<AssignStmt>(N<IdExpr>(var), clone(stmt->what)));
  for (auto& c : stmt->cases) {
    ctx->enter_conditional_block();
    StmtPtr suite = N<SuiteStmt>(clone(c.suite), N<BreakStmt>());
    if (c.guard)
      suite = N<IfStmt>(clone(c.guard), suite);
    result->stmts.push_back(
        transform_pattern(N<IdExpr>(var), clone(c.pattern), suite));
    ctx->leave_conditional_block();
  }
  // Make sure to break even if there is no case _ to prevent infinite loop
  result->stmts.push_back(N<BreakStmt>());
  result_stmt = transform(N<WhileStmt>(N<BoolExpr>(true), result));
}

/// Transform a match pattern into a series of if statements.
/// @example
///   `case True`          -> `if isinstance(var, "bool"): if var == True`
///   `case 1`             -> `if isinstance(var, "int"): if var == 1`
///   `case 1...3`         -> ```if isinstance(var, "int"):
///                                if var >= 1: if var <= 3```
///   `case (1, pat)`      -> ```if isinstance(var, "Tuple"): if staticlen(var)
///   == 2:
///                                 if match(var[0], 1): if match(var[1],
///                                 pat)```
///   `case [1, ..., pat]` -> ```if isinstance(var, "List"): if len(var) >= 2:
///                                 if match(var[0], 1): if match(var[-1],
///                                 pat)```
///   `case 1 or pat`      -> `if match(var, 1): if match(var, pat)`
///                           (note: pattern suite is cloned for each `or`)
///   `case (x := pat)`    -> `(x = var; if match(var, pat))`
///   `case x`             -> `(x := var)`
///                           (only when `x` is not '_')
///   `case expr`          -> `if hasattr(typeof(var), "__match__"): if
///   var.__match__(foo())`
///                           (any expression that does not fit above patterns)
auto SimplifyVisitor::transform_pattern(const ExprPtr& var, ExprPtr pattern,
                                          StmtPtr suite) -> StmtPtr {
  // Convenience function to generate `isinstance(e, typ)` calls
  auto isinstance = [&](const ExprPtr& e, const std::string& typ) -> ExprPtr {
    return N<CallExpr>(N<IdExpr>("isinstance"), e->clone(), N<IdExpr>(typ));
  };
  // Convenience function to find the index of an ellipsis within a list pattern
  auto find_ellipsis = [&](const std::vector<ExprPtr>& items) {
    size_t i = items.size();
    for (auto it = 0; it < items.size(); it++)
      if (items[it]->get_ellipsis()) {
        if (i != items.size())
          Err(Error::MATCH_MULTI_ELLIPSIS, items[it],
            "multiple ellipses in pattern");
        i = it;
      }
    return i;
  };

  // See the above examples for transformation details
  if (pattern->get_int() || CAST(pattern, BoolExpr)) {
    // Bool and int patterns
    return N<IfStmt>(
        isinstance(var, CAST(pattern, BoolExpr) ? "bool" : "int"),
        N<IfStmt>(N<BinaryExpr>(var->clone(), "==", pattern), suite));
  } else if (auto er = CAST(pattern, RangeExpr)) {
    // Range pattern
    return N<IfStmt>(
        isinstance(var, "int"),
        N<IfStmt>(N<BinaryExpr>(var->clone(), ">=", clone(er->start)),
                  N<IfStmt>(N<BinaryExpr>(var->clone(), "<=", clone(er->stop)),
                            suite)));
  } else if (auto et = pattern->get_tuple()) {
    // Tuple pattern
    for (auto it = et->items.size(); it-- > 0;) {
      suite = transform_pattern(N<IndexExpr>(var->clone(), N<IntExpr>(it)),
                               clone(et->items[it]), suite);
    }
    return N<IfStmt>(
        isinstance(var, "Tuple"),
        N<IfStmt>(N<BinaryExpr>(N<CallExpr>(N<IdExpr>("staticlen"), clone(var)),
                                "==", N<IntExpr>(et->items.size())),
                  suite));
  } else if (auto el = pattern->get_list()) {
    // List pattern
    auto ellipsis = find_ellipsis(el->items), sz = el->items.size();
    std::string op;
    if (ellipsis == el->items.size()) {
      op = "==";
    } else {
      op = ">=", sz -= 1;
    }
    for (auto it = el->items.size(); it-- > ellipsis + 1;) {
      suite = transform_pattern(
          N<IndexExpr>(var->clone(), N<IntExpr>(it - el->items.size())),
          clone(el->items[it]), suite);
    }
    for (auto it = ellipsis; it-- > 0;) {
      suite = transform_pattern(N<IndexExpr>(var->clone(), N<IntExpr>(it)),
                               clone(el->items[it]), suite);
    }
    return N<IfStmt>(
        isinstance(var, "List"),
        N<IfStmt>(N<BinaryExpr>(N<CallExpr>(N<IdExpr>("len"), clone(var)), op,
                                N<IntExpr>(sz)),
                  suite));
  } else if (auto eb = pattern->get_binary()) {
    // Or pattern
    if (eb->op == "|") {
      return N<SuiteStmt>(
          transform_pattern(clone(var), clone(eb->lexpr), clone(suite)),
          transform_pattern(clone(var), clone(eb->rexpr), suite));
    }
  } else if (auto ea = CAST(pattern, AssignExpr)) {
    // Bound pattern
    // seqassert(ea->var->get_id(),
    //           "only simple assignment expressions are supported");
    return N<SuiteStmt>(
        N<AssignStmt>(clone(ea->var), clone(var)),
        transform_pattern(clone(var), clone(ea->expr), clone(suite)));
  } else if (auto ei = pattern->get_id()) {
    // Wildcard pattern
    if (ei->value != "_") {
      return N<SuiteStmt>(N<AssignStmt>(clone(pattern), clone(var)), suite);
    } else {
      return suite;
    }
  }
  pattern = transform(pattern);  // transform to check for pattern errors
  if (pattern->get_ellipsis())
    pattern = N<CallExpr>(N<IdExpr>("ellipsis"));
  // Fallback (`__match__`) pattern
  return N<IfStmt>(
      N<CallExpr>(N<IdExpr>("hasattr"), var->clone(),
                  N<StringExpr>("__match__"),
                  N<CallExpr>(N<IdExpr>("type"), pattern->clone())),
      N<IfStmt>(N<CallExpr>(N<DotExpr>(var->clone(), "__match__"), pattern),
                suite));
}

}  // namespace Pud::AST