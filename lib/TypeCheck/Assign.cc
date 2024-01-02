#include <string>
#include <tuple>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

namespace Pud::AST {

/// Transform assignments. Handle dominated assignments, forward declarations,
/// static assignments and type/function aliases. See @c wrap_expr for more
/// examples.
void TypecheckVisitor::visit(AssignStmt* stmt) {
  // Update statements are handled by @c visitUpdate
  if (stmt->is_update()) {
    transform_update(stmt);
    return;
  }

  seqassert(stmt->lhs->get_id(), "invalid AssignStmt {}", stmt->lhs);
  std::string lhs = stmt->lhs->get_id()->value;

  // Special case: this assignment has been dominated and is not a true
  // assignment but
  //               an update of the dominating binding.
  if (auto changed = in(ctx->cache->replacements, lhs)) {
    while (auto s = in(ctx->cache->replacements, lhs))
      lhs = changed->first, changed = s;
    if (stmt->rhs && changed->second) {
      // Mark the dominating binding as used: `var.__used__ = True`
      auto u = N<AssignStmt>(N<IdExpr>(fmt::format("{}.__used__", lhs)),
                             N<BoolExpr>(true));
      u->set_update();
      prepend_stmts->push_back(transform(u));
    } else if (changed->second && !stmt->rhs) {
      // This assignment was a declaration only. Just mark the dominating
      // binding as used: `var.__used__ = True`
      stmt->lhs = N<IdExpr>(fmt::format("{}.__used__", lhs));
      stmt->rhs = N<BoolExpr>(true);
    }
    seqassert(stmt->rhs, "bad domination statement: '{}'", stmt->to_string());
    // Change this to the update and follow the update logic
    stmt->set_update();
    transform_update(stmt);
    return;
  }

  transform(stmt->rhs);
  transform_type(stmt->type);
  if (!stmt->rhs) {
    // Forward declarations (e.g., dominating bindings, C imports etc.).
    // The type is unknown and will be deduced later
    unify(stmt->lhs->type, ctx->get_unbound(stmt->lhs->get_source_info()));
    if (stmt->type) {
      unify(stmt->lhs->type, ctx->instantiate(stmt->type->get_source_info(),
                                              stmt->type->get_type()));
    }
    ctx->add(TypecheckItem::Var, lhs, stmt->lhs->type);
    if (realize(stmt->lhs->type) || !stmt->type)
      stmt->set_done();
  } else if (stmt->type && stmt->type->get_type()->is_static_type()) {
    // Static assignments (e.g., `x: Static[int] = 5`)
    if (!stmt->rhs->is_static())
      Err(Error::EXPECTED_STATIC, stmt->rhs);
    seqassert(stmt->rhs->static_value.evaluated, "static not evaluated");
    unify(stmt->lhs->type, unify(stmt->type->type, Type::Type::make_static(
                                                       ctx->cache, stmt->rhs)));
    auto val = ctx->add(TypecheckItem::Var, lhs, stmt->lhs->type);
    if (in(ctx->cache->globals, lhs)) {
      // Make globals always visible!
      ctx->add_toplevel(lhs, val);
    }
    if (realize(stmt->lhs->type))
      stmt->set_done();
  } else {
    // Normal assignments
    unify(stmt->lhs->type, ctx->get_unbound());
    if (stmt->type) {
      unify(stmt->lhs->type, ctx->instantiate(stmt->type->get_source_info(),
                                              stmt->type->get_type()));
    }
    // Check if we can wrap the expression (e.g., `a: float = 3` -> `a =
    // float(3)`)
    if (wrap_expr(stmt->rhs, stmt->lhs->get_type()))
      unify(stmt->lhs->type, stmt->rhs->type);
    auto type = stmt->lhs->get_type();
    auto kind = TypecheckItem::Var;
    if (stmt->rhs->is_type())
      kind = TypecheckItem::Type;
    else if (type->get_func())
      kind = TypecheckItem::Func;
    // Generalize non-variable types. That way we can support cases like:
    // `a = foo(x, ...); a(1); a('s')`
    auto val = std::make_shared<TypecheckItem>(
        kind, kind != TypecheckItem::Var
                  ? type->generalize(ctx->typecheck_level - 1)
                  : type);

    if (in(ctx->cache->globals, lhs)) {
      // Make globals always visible!
      ctx->add_toplevel(lhs, val);
      if (kind != TypecheckItem::Var)
        ctx->cache->globals.erase(lhs);
    } else if (startswith(ctx->get_realization_base()->name, "._import_") &&
               kind == TypecheckItem::Type) {
      // Make import toplevel type aliases (e.g., `a = Ptr[byte]`) visible
      ctx->add_toplevel(lhs, val);
    } else {
      ctx->add(lhs, val);
    }

    if (stmt->lhs->get_id() && kind != TypecheckItem::Var) {
      // Special case: type/function renames
      stmt->rhs->type = nullptr;
      stmt->set_done();
    } else if (stmt->rhs->is_done() && realize(stmt->lhs->type)) {
      stmt->set_done();
    }
  }
}

/// Transform binding updates. Special handling is done for atomic or in-place
/// statements (e.g., `a += b`).
/// See @c transform_inplace_update and @c wrap_expr for details.
void TypecheckVisitor::transform_update(AssignStmt* stmt) {
  transform(stmt->lhs);
  if (stmt->lhs->is_static())
    Err(Error::ASSIGN_UNEXPECTED_STATIC, stmt->lhs);

  // Check inplace updates
  auto [in_place, in_placeExpr] = transform_inplace_update(stmt);
  if (in_place) {
    if (in_placeExpr) {
      result_stmt = N<ExprStmt>(in_placeExpr);
      if (in_placeExpr->is_done())
        result_stmt->set_done();
    }
    return;
  }

  transform(stmt->rhs);
  // Case: wrap expressions if needed (e.g. floats or optionals)
  if (wrap_expr(stmt->rhs, stmt->lhs->get_type()))
    unify(stmt->rhs->type, stmt->lhs->type);
  if (stmt->rhs->done && realize(stmt->lhs->type))
    stmt->set_done();
}

/// Typecheck instance member assignments (e.g., `a.b = c`) and handle optional
/// instances. Disallow tuple updates.
/// @example
///   `opt.foo = bar` -> `unwrap(opt).foo = wrap(bar)`
/// See @c wrap_expr for more examples.
void TypecheckVisitor::visit(AssignMemberStmt* stmt) {
  transform(stmt->lhs);

  if (auto lhs_class = stmt->lhs->get_type()->get_class()) {
    auto member = ctx->find_member(lhs_class, stmt->member);

    if (!member && stmt->lhs->is_type()) {
      // Case: class variables
      if (auto cls = in(ctx->cache->classes, lhs_class->name))
        if (auto var = in(cls->class_vars, stmt->member)) {
          auto a = N<AssignStmt>(N<IdExpr>(*var), transform(stmt->rhs));
          a->set_update();
          result_stmt = transform(a);
          return;
        }
    }
    if (!member && lhs_class->is(TYPE_OPTIONAL)) {
      // Unwrap optional and look up there
      result_stmt = transform(
          N<AssignMemberStmt>(N<CallExpr>(N<IdExpr>(FN_UNWRAP), stmt->lhs),
                              stmt->member, stmt->rhs));
      return;
    }

    if (!member)
      Err(Error::DOT_NO_ATTR, stmt->lhs, lhs_class->pretty_string(),
          stmt->member);
    if (lhs_class->get_record())
      Err(Error::ASSIGN_UNEXPECTED_FROZEN, stmt->lhs);

    transform(stmt->rhs);
    auto typ = ctx->instantiate(stmt->lhs->get_source_info(), member, lhs_class);
    if (!wrap_expr(stmt->rhs, typ))
      return;
    unify(stmt->rhs->type, typ);
    if (stmt->rhs->is_done())
      stmt->set_done();
  }
}

/// Transform in-place and atomic updates.
/// @example
///   `a += b` -> `a.__iadd__(a, b)` if `__iadd__` exists
///   Atomic operations (when the needed magics are available):
///   `a = b`         -> `type(a).__atomic_xchg__(__ptr__(a), b)`
///   `a += b`        -> `type(a).__atomic_add__(__ptr__(a), b)`
///   `a = min(a, b)` -> `type(a).__atomic_min__(__ptr__(a), b)` (same for
///   `max`)
/// @return a tuple indicating whether (1) the update statement can be replaced
/// with an
///         expression, and (2) the replacement expression.
std::pair<bool, ExprPtr> TypecheckVisitor::transform_inplace_update(
    AssignStmt* stmt) {
  // Case: in-place updates (e.g., `a += b`).
  // They are stored as `Update(a, Binary(a + b, in_place=true))`
  auto bin = stmt->rhs->get_binary();
  if (bin && bin->in_place) {
    transform(bin->lexpr);
    transform(bin->rexpr);
    if (bin->lexpr->type->get_class() && bin->rexpr->type->get_class()) {
      if (auto transformed =
              transform_binary_inplace_magic(bin, stmt->is_atomic_update())) {
        unify(stmt->rhs->type, transformed->type);
        return {true, transformed};
      } else if (!stmt->is_atomic_update()) {
        // If atomic, call normal magic and then use __atomic_xchg__ below
        return {false, nullptr};
      }
    } else {  // Not yet completed
      unify(stmt->lhs->type, unify(stmt->rhs->type, ctx->get_unbound()));
      return {true, nullptr};
    }
  }

  // Case: atomic min/max operations.
  // Note: check only `a = min(a, b)`; does NOT check `a = min(b, a)`
  auto lhs_class = stmt->lhs->get_type()->get_class();
  auto call = stmt->rhs->get_call();
  if (stmt->is_atomic_update() && call && stmt->lhs->get_id() &&
      (call->expr->is_id("min") || call->expr->is_id("max")) &&
      call->args.size() == 2 &&
      call->args[0].value->is_id(std::string(stmt->lhs->get_id()->value))) {
    // `type(a).__atomic_min__(__ptr__(a), b)`
    auto ptr_typ = ctx->instantiate_generic(stmt->lhs->get_source_info(),
                                            ctx->get_type("Ptr"), {lhs_class});
    call->args[1].value = transform(call->args[1].value);
    auto rhs_typ = call->args[1].value->get_type()->get_class();
    if (auto method = find_best_method(
            lhs_class, format("__atomic_{}__", call->expr->get_id()->value),
            {ptr_typ, rhs_typ})) {
      return {true, transform(N<CallExpr>(
                        N<IdExpr>(method->ast->name),
                        N<CallExpr>(N<IdExpr>("__ptr__"), stmt->lhs),
                        call->args[1].value))};
    }
  }

  // Case: atomic assignments
  if (stmt->is_atomic_update() && lhs_class) {
    // `type(a).__atomic_xchg__(__ptr__(a), b)`
    transform(stmt->rhs);
    if (auto rhs_class = stmt->rhs->get_type()->get_class()) {
      auto ptr_type = ctx->instantiate_generic(
          stmt->lhs->get_source_info(), ctx->get_type("Ptr"), {lhs_class});
      if (auto m = find_best_method(lhs_class, "__atomic_xchg__",
                                    {ptr_type, rhs_class})) {
        return {true, N<CallExpr>(N<IdExpr>(m->ast->name),
                                  N<CallExpr>(N<IdExpr>("__ptr__"), stmt->lhs),
                                  stmt->rhs)};
      }
    }
  }

  return {false, nullptr};
}

}  // namespace Pud::AST