#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

namespace Pud::AST {

/// Unify the function return type with `Generator[?]`.
/// The unbound type will be deduced from return/yield statements.
void TypecheckVisitor::visit(YieldExpr* expr) {
  unify(expr->type, ctx->get_unbound());
  unify(ctx->get_realization_base()->return_type,
        ctx->instantiate_generic(ctx->get_type("Generator"), {expr->type}));
  if (realize(expr->type))
    expr->set_done();
}

/// Typecheck return statements. Empty return is transformed to `return
/// NoneType()`. Also partialize functions if they are being returned. See @c
/// wrap_expr for more details.
void TypecheckVisitor::visit(ReturnStmt* stmt) {
  if (transform(stmt->expr)) {
    // Wrap expression to match the return type
    if (!ctx->get_realization_base()->return_type->get_unbound())
      if (!wrap_expr(stmt->expr, ctx->get_realization_base()->return_type)) {
        return;
      }

    // Special case: partialize functions if we are returning them
    if (stmt->expr->get_type()->get_func() &&
        !(ctx->get_realization_base()->return_type->get_class() &&
          ctx->get_realization_base()->return_type->is("Function"))) {
      stmt->expr = partialize_function(stmt->expr->type->get_func());
    }

    unify(ctx->get_realization_base()->return_type, stmt->expr->type);
  } else {
    // Just set the expr for the translation stage. However, do not unify the
    // return type! This might be a `return` in a generator.
    stmt->expr = transform(N<CallExpr>(N<IdExpr>("NoneType")));
  }

  // If we are not within conditional block, ignore later statements in this
  // function. Useful with static if statements.
  if (!ctx->block_level)
    ctx->return_early = true;

  if (stmt->expr->is_done())
    stmt->set_done();
}

/// Typecheck yield statements. Empty yields assume `NoneType`.
void TypecheckVisitor::visit(YieldStmt* stmt) {
  stmt->expr =
      transform(stmt->expr ? stmt->expr : N<CallExpr>(N<IdExpr>("NoneType")));
  unify(
      ctx->get_realization_base()->return_type,
      ctx->instantiate_generic(ctx->get_type("Generator"), {stmt->expr->type}));

  if (stmt->expr->is_done())
    stmt->set_done();
}

/// Parse a function stub and create a corresponding generic function type.
/// Also realize built-ins and extern C functions.
void TypecheckVisitor::visit(FunctionStmt* stmt) {
  // Function should be constructed only once
  stmt->set_done();

  auto func_typ = make_function_type(stmt);
  // If this is a class method, update the method lookup table
  bool is_class_member = !stmt->attributes.parent_class.empty();
  if (is_class_member) {
    auto m = ctx->cache->get_method(
        ctx->find(stmt->attributes.parent_class)->type->get_class(),
        ctx->cache->rev(stmt->name));
    bool found = false;
    for (auto& i : ctx->cache->overloads[m])
      if (i.name == stmt->name) {
        ctx->cache->functions[i.name].type = func_typ;
        found = true;
        break;
      }
    seqassert(found, "cannot find matching class method for {}", stmt->name);
  }

  // Update the visited table
  // Functions should always be visible, so add them to the toplevel
  ctx->add_toplevel(stmt->name, std::make_shared<TypecheckItem>(
                                    TypecheckItem::Func, func_typ));
  ctx->cache->functions[stmt->name].type = func_typ;

  // Ensure that functions with @C, @force_realize, and @export attributes can
  // be realized
  if (stmt->attributes.has(Attr::ForceRealize) ||
      stmt->attributes.has(Attr::Export) ||
      (stmt->attributes.has(Attr::C) && !stmt->attributes.has(Attr::CVarArg))) {
    if (!func_typ->can_realize())
      Err(Error::FN_REALIZE_BUILTIN, stmt);
  }

  // Debug information
  LOG_REALIZE("[stmt] added func {}: {}", stmt->name, func_typ);
}

auto TypecheckVisitor::make_function_type(FunctionStmt* stmt)
    -> Type::FuncTypePtr {
  // Handle generics
  bool is_class_member = !stmt->attributes.parent_class.empty();
  auto explicits = std::vector<Type::ClassType::Generic>();
  for (const auto& a : stmt->args) {
    if (a.status == Param::Generic) {
      // Generic and static types
      auto generic = ctx->get_unbound();
      generic->is_static = get_static_generic(a.type.get());
      auto typ_id = generic->get_link()->id;
      generic->generic_name = ctx->cache->rev(a.name);
      if (a.default_value) {
        auto def_type = transform_type(clone(a.default_value));
        generic->default_type = def_type->type;
      }
      ctx->add(TypecheckItem::Type, a.name, generic);
      explicits.emplace_back(a.name, ctx->cache->rev(a.name),
                             generic->generalize(ctx->typecheck_level), typ_id);
    }
  }

  // Prepare list of all generic types
  std::vector<Type::TypePtr> generics;
  Type::ClassTypePtr parent_class = nullptr;
  if (is_class_member && stmt->attributes.has(Attr::Method)) {
    // Get class generics (e.g., T for `class Cls[T]: def foo:`)
    auto parent_class_ast =
        ctx->cache->classes[stmt->attributes.parent_class].ast.get();
    parent_class =
        ctx->force_find(stmt->attributes.parent_class)->type->get_class();
    parent_class =
        parent_class->instantiate(ctx->typecheck_level - 1, nullptr, nullptr)
            ->get_class();
    seqassert(parent_class, "parent class not set");
    for (int i = 0, j = 0, k = 0; i < parent_class_ast->args.size(); i++) {
      if (parent_class_ast->args[i].status != Param::Normal) {
        generics.push_back(parent_class_ast->args[i].status == Param::Generic
                               ? parent_class->generics[j++].type
                               : parent_class->hidden_generics[k++].type);
        ctx->add(TypecheckItem::Type, parent_class_ast->args[i].name,
                 generics.back());
      }
    }
  }
  // Add function generics
  for (const auto& i : explicits)
    generics.push_back(ctx->find(i.name)->type);

  // Handle function arguments
  // Base type: `Function[[args,...], ret]`
  auto base_type = get_func_type_base(stmt->args.size() - explicits.size());
  ctx->typecheck_level++;
  if (stmt->ret) {
    unify(base_type->generics[1].type, transform_type(stmt->ret)->get_type());
  } else {
    generics.push_back(unify(base_type->generics[1].type, ctx->get_unbound()));
  }
  // Unify base type generics with argument types
  auto arg_type = base_type->generics[0].type->get_record();
  for (int ai = 0, aj = 0; ai < stmt->args.size(); ai++) {
    if (stmt->args[ai].status == Param::Normal && !stmt->args[ai].type) {
      if (parent_class && ai == 0 &&
          ctx->cache->rev(stmt->args[ai].name) == "self") {
        // Special case: self in methods
        unify(arg_type->args[aj], parent_class);
      } else {
        unify(arg_type->args[aj], ctx->get_unbound());
      }
      generics.push_back(arg_type->args[aj++]);
    } else if (stmt->args[ai].status == Param::Normal &&
               startswith(stmt->args[ai].name, "*")) {
      // Special case: `*args: type` and `**kwargs: type`. Do not add this type
      // to the signature (as the real type is `Tuple[type, ...]`); it will be
      // used during call typechecking
      unify(arg_type->args[aj], ctx->get_unbound());
      generics.push_back(arg_type->args[aj++]);
    } else if (stmt->args[ai].status == Param::Normal) {
      unify(arg_type->args[aj],
            transform_type(stmt->args[ai].type)->get_type());
      generics.push_back(arg_type->args[aj++]);
    }
  }
  ctx->typecheck_level--;

  // Generalize generics and remove them from the context
  for (const auto& g : generics) {
    for (auto& u : g->get_unbounds())
      if (u->get_unbound())
        u->get_unbound()->kind = Type::LinkType::Generic;
  }

  // Construct the type
  auto func_typ = std::make_shared<Type::FuncType>(
      base_type, ctx->cache->functions[stmt->name].ast.get(), explicits);

  func_typ->set_source_info(get_source_info());
  if (is_class_member && stmt->attributes.has(Attr::Method)) {
    func_typ->func_parent = ctx->find(stmt->attributes.parent_class)->type;
  }
  func_typ = std::static_pointer_cast<Type::FuncType>(
      func_typ->generalize(ctx->typecheck_level));
  return func_typ;
}

/// Make an empty partial call `fn(...)` for a given function.
auto TypecheckVisitor::partialize_function(const Type::FuncTypePtr& fn)
    -> ExprPtr {
  // Create function mask
  std::vector<char> mask(fn->ast->args.size(), 0);
  for (int i = 0, j = 0; i < fn->ast->args.size(); i++)
    if (fn->ast->args[i].status == Param::Generic) {
      if (!fn->func_generics[j].type->get_unbound())
        mask[i] = 1;
      j++;
    }

  // Generate partial class
  auto partial_type_name = generate_partial_stub(mask, fn.get());
  std::string var = ctx->cache->get_temporary_var("partial");
  // Generate kwtuple for potential **kwargs
  auto kw_name = generate_tuple(0, TYPE_KWTUPLE, {});
  // `partial = Partial.MASK((), KwTuple())`
  // (`()` for *args and `KwTuple()` for **kwargs)
  ExprPtr call = N<StmtExpr>(
      N<AssignStmt>(N<IdExpr>(var),
                    N<CallExpr>(N<IdExpr>(partial_type_name), N<TupleExpr>(),
                                N<CallExpr>(N<IdExpr>(kw_name)))),
      N<IdExpr>(var));
  call->set_attr(ExprAttr::Partial);
  transform(call);
  seqassert(call->type->get_partial(), "expected partial type");
  return call;
}

/// Generate and return `Function[Tuple[args...], ret]` type
auto TypecheckVisitor::get_func_type_base(size_t nargs)
    -> std::shared_ptr<Type::RecordType> {
  auto base_type =
      ctx->instantiate(ctx->force_find("Function")->type)->get_record();
  unify(base_type->generics[0].type,
        ctx->instantiate_tuple(nargs)->get_record());
  return base_type;
}

}  // namespace Pud::AST