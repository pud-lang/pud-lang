#include <string>
#include <tuple>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

namespace Pud::AST {

/// Just ensure that this expression is not independent of CallExpr where it is
/// handled.
void TypecheckVisitor::visit(StarExpr* expr) {
  Err(Error::UNEXPECTED_TYPE, expr, "star");
}

/// Just ensure that this expression is not independent of CallExpr where it is
/// handled.
void TypecheckVisitor::visit(KeywordStarExpr* expr) {
  Err(Error::UNEXPECTED_TYPE, expr, "kwstar");
}

/// Typechecks an ellipsis. Ellipses are typically replaced during the
/// typechecking; the only remaining ellipses are those that belong to
/// PipeExprs.
void TypecheckVisitor::visit(EllipsisExpr* expr) {
  unify(expr->type, ctx->get_unbound());
  if (expr->mode == EllipsisExpr::PIPE && realize(expr->type)) {
    expr->set_done();
  }

  if (expr->mode == EllipsisExpr::STANDALONE) {
    result_expr = transform(N<CallExpr>(N<IdExpr>("ellipsis")));
    unify(expr->type, result_expr->type);
  }
}

/// Typecheck a call expression. This is the most complex expression to
/// typecheck.
/// @example
///   `fn(1, 2, x=3, y=4)` -> `func(a=1, x=3, args=(2,), kwargs=KwArgs(y=4),
///   T=int)` `fn(arg1, ...)`      -> `(_v = Partial.N10(arg1); _v)`
/// See @c transform_call_args , @c get_callee_fn , @c call_reorder_arguments ,
///     @c typecheck_call_args , @c transform_special_call and @c wrap_expr for more
///     details.
void TypecheckVisitor::visit(CallExpr* expr) {
  // Transform and expand arguments. Return early if it cannot be done yet
  if (!transform_call_args(expr->args))
    return;

  // Check if this call is partial call
  PartialCallData part{
      !expr->args.empty() && expr->args.back().value->get_ellipsis() &&
      expr->args.back().value->get_ellipsis()->mode == EllipsisExpr::PARTIAL};
  // Transform the callee
  if (!part.isPartial) {
    // Intercept method calls (e.g. `obj.method`) for faster compilation
    // (because it avoids partial calls). This intercept passes the call
    // arguments to
    // @c transform_dot to select the best overload as well
    if (auto dot = expr->expr->get_dot()) {
      // Pick the best method overload
      if (auto edt = transform_dot(dot, &expr->args))
        expr->expr = edt;
    } else if (auto id = expr->expr->get_id()) {
      // Pick the best function overload
      auto overloads = in(ctx->cache->overloads, id->value);
      if (overloads && overloads->size() > 1) {
        if (auto best_method = get_best_overload(id, &expr->args)) {
          auto t = id->type;
          expr->expr = N<IdExpr>(best_method->ast->name);
          expr->expr->set_type(unify(t, ctx->instantiate(best_method)));
        }
      }
    }
  }
  transform(expr->expr);
  auto [callee_fn, new_expr] = get_callee_fn(expr, part);
  if ((result_expr = new_expr))
    return;
  if (!callee_fn)
    return;

  // Handle named and default arguments
  if ((result_expr = call_reorder_arguments(callee_fn, expr, part)))
    return;

  // Handle special calls
  if (!part.isPartial) {
    auto [isSpecial, specialExpr] = transform_special_call(expr);
    if (isSpecial) {
      unify(expr->type, ctx->get_unbound());
      result_expr = specialExpr;
      return;
    }
  }

  // Typecheck arguments with the function signature
  bool done = typecheck_call_args(callee_fn, expr->args);
  if (!part.isPartial && realize(callee_fn)) {
    // Previous unifications can qualify existing identifiers.
    // Transform again to get the full identifier
    transform(expr->expr);
  }
  done &= expr->expr->is_done();

  // Emit the final call
  if (part.isPartial) {
    // Case: partial call. `callee_fn(args...)` ->
    // `Partial.N<known>.<fn>(args...)`
    auto partial_type_name =
        generate_partial_stub(part.known, callee_fn->get_func().get());
    std::vector<ExprPtr> new_args;
    for (auto& r : expr->args)
      if (!r.value->get_ellipsis()) {
        new_args.push_back(r.value);
        new_args.back()->set_attr(ExprAttr::SequenceItem);
      }
    new_args.push_back(part.args);
    new_args.push_back(part.kwArgs);

    std::string var = ctx->cache->get_temporary_var("part");
    ExprPtr call = nullptr;
    if (!part.var.empty()) {
      // Callee is already a partial call
      auto stmts = expr->expr->get_stmt_expr()->stmts;
      stmts.push_back(N<AssignStmt>(
          N<IdExpr>(var), N<CallExpr>(N<IdExpr>(partial_type_name), new_args)));
      call = N<StmtExpr>(stmts, N<IdExpr>(var));
    } else {
      // New partial call: `(part = Partial.N<known>.<fn>(stored_args...);
      // part)`
      call = N<StmtExpr>(
          N<AssignStmt>(N<IdExpr>(var),
                        N<CallExpr>(N<IdExpr>(partial_type_name), new_args)),
          N<IdExpr>(var));
    }
    call->set_attr(ExprAttr::Partial);
    result_expr = transform(call);
  } else {
    // Case: normal function call
    unify(expr->type, callee_fn->get_ret_type());
    if (done)
      expr->set_done();
  }
}

/// Transform call arguments. Expand *args and **kwargs to the list of @c
/// CallExpr::Arg objects.
/// @return false if expansion could not be completed; true otherwise
auto TypecheckVisitor::transform_call_args(std::vector<CallExpr::Arg>& args) -> bool {
  for (auto ai = 0; ai < args.size();) {
    if (auto star = args[ai].value->get_star()) {
      // Case: *args expansion
      transform(star->what);
      auto typ = star->what->type->get_class();
      while (typ && typ->is(TYPE_OPTIONAL)) {
        star->what = transform(N<CallExpr>(N<IdExpr>(FN_UNWRAP), star->what));
        typ = star->what->type->get_class();
      }
      if (!typ)  // Process later
        return false;
      if (!typ->get_record())
        Err(Error::CALL_BAD_UNPACK, args[ai], typ->pretty_string());
      auto fields = get_class_fields(typ.get());
      for (size_t i = 0; i < typ->get_record()->args.size(); i++, ai++) {
        args.insert(
            args.begin() + ai,
            {"", transform(N<DotExpr>(clone(star->what), fields[i].name))});
      }
      args.erase(args.begin() + ai);
    } else if (auto kwstar = CAST(args[ai].value, KeywordStarExpr)) {
      // Case: **kwargs expansion
      kwstar->what = transform(kwstar->what);
      auto typ = kwstar->what->type->get_class();
      while (typ && typ->is(TYPE_OPTIONAL)) {
        kwstar->what =
            transform(N<CallExpr>(N<IdExpr>(FN_UNWRAP), kwstar->what));
        typ = kwstar->what->type->get_class();
      }
      if (!typ)
        return false;
      if (!typ->get_record() || typ->name == TYPE_TUPLE)
        Err(Error::CALL_BAD_KWUNPACK, args[ai], typ->pretty_string());
      auto fields = get_class_fields(typ.get());
      for (size_t i = 0; i < typ->get_record()->args.size(); i++, ai++) {
        args.insert(args.begin() + ai,
                    {fields[i].name, transform(N<DotExpr>(clone(kwstar->what),
                                                          fields[i].name))});
      }
      args.erase(args.begin() + ai);
    } else {
      // Case: normal argument (no expansion)
      transform(args[ai++].value);
    }
  }

  // Check if some argument names are reused after the expansion
  std::set<std::string> seen;
  for (auto& a : args)
    if (!a.name.empty()) {
      if (in(seen, a.name))
        Err(Error::CALL_REPEATED_NAME, a, a.name);
      seen.insert(a.name);
    }

  return true;
}

/// Extract the @c FuncType that represents the function to be called by the
/// callee. Also handle special callees: constructors and partial functions.
/// @return a pair with the callee's @c FuncType and the replacement expression
///         (when needed; otherwise nullptr).
std::pair<Type::FuncTypePtr, ExprPtr> TypecheckVisitor::get_callee_fn(
    CallExpr* expr, PartialCallData& part) {
  auto callee = expr->expr->type->get_class();
  if (!callee) {
    // Case: unknown callee, wait until it becomes known
    unify(expr->type, ctx->get_unbound());
    return {nullptr, nullptr};
  }

  if (expr->expr->is_type() && callee->get_record()) {
    // Case: tuple constructor. Transform to: `T.__new__(args)`
    return {nullptr, transform(N<CallExpr>(N<DotExpr>(expr->expr, "__new__"),
                                           expr->args))};
  }

  if (expr->expr->is_type()) {
    // Case: reference type constructor. Transform to
    // `ctr = T.__new__(); v.__init__(args)`
    ExprPtr var = N<IdExpr>(ctx->cache->get_temporary_var("ctr"));
    auto cls_name = expr->expr->type->get_class()->name;
    auto new_init = N<AssignStmt>(
        clone(var), N<CallExpr>(N<DotExpr>(expr->expr, "__new__")));
    auto e = N<StmtExpr>(N<SuiteStmt>(new_init), clone(var));
    auto init = N<ExprStmt>(
        N<CallExpr>(N<DotExpr>(clone(var), "__init__"), expr->args));
    e->stmts.emplace_back(init);
    return {nullptr, transform(e)};
  }

  auto callee_fn = callee->get_func();
  if (auto part_type = callee->get_partial()) {
    // Case: calling partial object `p`. Transform roughly to
    // `part = callee; partial_fn(*part.args, args...)`
    ExprPtr var = N<IdExpr>(part.var = ctx->cache->get_temporary_var("partcall"));
    expr->expr = transform(N<StmtExpr>(N<AssignStmt>(clone(var), expr->expr),
                                       N<IdExpr>(part_type->func->ast->name)));

    // Ensure that we got a function
    callee_fn = expr->expr->type->get_func();
    seqassert(callee_fn, "not a function: {}", expr->expr->type);

    // Unify partial generics with types known thus far
    for (size_t i = 0, j = 0, k = 0; i < part_type->known.size(); i++)
      if (part_type->func->ast->args[i].status == Param::Generic) {
        if (part_type->known[i])
          unify(callee_fn->func_generics[j].type,
                ctx->instantiate(part_type->func->func_generics[j].type));
        j++;
      } else if (part_type->known[i]) {
        unify(callee_fn->get_arg_types()[i - j], part_type->generics[k].type);
        k++;
      }
    part.known = part_type->known;
    return {callee_fn, nullptr};
  } else if (!callee->get_func()) {
    // Case: callee is not a function. Try __call__ method instead
    return {nullptr, transform(N<CallExpr>(N<DotExpr>(expr->expr, "__call__"),
                                           expr->args))};
  }
  return {callee_fn, nullptr};
}

/// Reorder the call arguments to match the signature order. Ensure that every
/// @c CallExpr::Arg has a set name. Form *args/**kwargs tuples if needed, and
/// use partial and default values where needed.
/// @example
///   `foo(1, 2, baz=3, baf=4)` -> `foo(a=1, baz=2, args=(3, ),
///   kwargs=KwArgs(baf=4))`
auto TypecheckVisitor::call_reorder_arguments(Type::FuncTypePtr callee_fn,
                                               CallExpr* expr,
                                               PartialCallData& part) -> ExprPtr {
  std::vector<CallExpr::Arg> args;  // stores ordered and processed arguments
  std::vector<ExprPtr>
      type_args;  // stores type and static arguments (e.g., `T: type`)
  auto new_mask = std::vector<char>(callee_fn->ast->args.size(), 1);

  // Extract pi-th partial argument from a partial object
  auto get_partial_arg = [&](size_t pi) {
    auto id = transform(N<IdExpr>(part.var));
    // Manually call @c transform_static_tuple_index to avoid spurious
    // InstantiateExpr
    auto ex =
        transform_static_tuple_index(id->type->get_class(), id, N<IntExpr>(pi));
    seqassert(ex.first && ex.second, "partial indexing failed: {}", id->type);
    return ex.second;
  };

  // Handle reordered arguments (see @c reorder_named_args for details)
  bool partial = false;
  auto reorder_fn = [&](int star_arg_index, int kwstar_arg_index,
                       const std::vector<std::vector<int>>& slots,
                       bool _partial) {
    partial = _partial;
    ctx->add_block();  // add function generics to typecheck default arguments
    add_function_generics(callee_fn->get_func().get());
    for (size_t si = 0, pi = 0; si < slots.size(); si++) {
      // Get the argument name to be used later
      auto rn = callee_fn->ast->args[si].name;
      trim_stars(rn);
      auto real_name = ctx->cache->rev(rn);

      if (callee_fn->ast->args[si].status == Param::Generic) {
        // Case: generic arguments. Populate type_args
        type_args.push_back(slots[si].empty() ? nullptr
                                             : expr->args[slots[si][0]].value);
        new_mask[si] = slots[si].empty() ? 0 : 1;
      } else if (si == star_arg_index &&
                 !(slots[si].size() == 1 &&
                   expr->args[slots[si][0]].value->has_attr(
                       ExprAttr::StarArgument))) {
        // Case: *args. Build the tuple that holds them all
        std::vector<ExprPtr> extra;
        if (!part.known.empty())
          extra.push_back(N<StarExpr>(get_partial_arg(-2)));
        for (auto& e : slots[si]) {
          extra.push_back(expr->args[e].value);
        }
        ExprPtr e = N<TupleExpr>(extra);
        e->set_attr(ExprAttr::StarArgument);
        if (!expr->expr->is_id("hasattr:0"))
          e = transform(e);
        if (partial) {
          part.args = e;
          args.push_back(
              {real_name, transform(N<EllipsisExpr>(EllipsisExpr::PARTIAL))});
          new_mask[si] = 0;
        } else {
          args.push_back({real_name, e});
        }
      } else if (si == kwstar_arg_index &&
                 !(slots[si].size() == 1 &&
                   expr->args[slots[si][0]].value->has_attr(
                       ExprAttr::KwStarArgument))) {
        // Case: **kwargs. Build the named tuple that holds them all
        std::vector<std::string> names;
        std::vector<CallExpr::Arg> values;
        if (!part.known.empty()) {
          auto e = get_partial_arg(-1);
          auto t = e->get_type()->get_record();
          seqassert(t && startswith(t->name, TYPE_KWTUPLE), "{} not a kwtuple",
                    e);
          auto ff = get_class_fields(t.get());
          for (int i = 0; i < t->get_record()->args.size(); i++) {
            names.emplace_back(ff[i].name);
            values.emplace_back(
                CallExpr::Arg(transform(N<DotExpr>(clone(e), ff[i].name))));
          }
        }
        for (auto& e : slots[si]) {
          names.emplace_back(expr->args[e].name);
          values.emplace_back(CallExpr::Arg(expr->args[e].value));
        }
        auto kw_name = generate_tuple(names.size(), TYPE_KWTUPLE, names);
        auto e = transform(N<CallExpr>(N<IdExpr>(kw_name), values));
        e->set_attr(ExprAttr::KwStarArgument);
        if (partial) {
          part.kwArgs = e;
          args.push_back(
              {real_name, transform(N<EllipsisExpr>(EllipsisExpr::PARTIAL))});
          new_mask[si] = 0;
        } else {
          args.push_back({real_name, e});
        }
      } else if (slots[si].empty()) {
        // Case: no argument. Check if the arguments is provided by the partial
        // type (if calling it) or if a default argument can be used
        if (!part.known.empty() && part.known[si]) {
          args.push_back({real_name, get_partial_arg(pi++)});
        } else if (partial) {
          args.push_back(
              {real_name, transform(N<EllipsisExpr>(EllipsisExpr::PARTIAL))});
          new_mask[si] = 0;
        } else {
          auto es = callee_fn->ast->args[si].default_value->to_string();
          if (in(ctx->default_call_depth, es))
            Err(Error::CALL_RECURSIVE_DEFAULT, expr,
              ctx->cache->rev(callee_fn->ast->args[si].name));
          ctx->default_call_depth.insert(es);
          args.push_back(
              {real_name,
               transform(clone(callee_fn->ast->args[si].default_value))});
          ctx->default_call_depth.erase(es);
        }
      } else {
        // Case: argument provided
        seqassert(slots[si].size() == 1, "call transformation failed");
        args.push_back({real_name, expr->args[slots[si][0]].value});
      }
    }
    ctx->pop_block();
    return 0;
  };

  // Reorder arguments if needed
  part.args = part.kwArgs =
      nullptr;  // Stores partial *args/**kwargs expression
  if (expr->has_attr(ExprAttr::OrderedCall) || expr->expr->is_id("superf")) {
    args = expr->args;
  } else {
    ctx->reorder_named_args(
        callee_fn.get(), expr->args, reorder_fn,
        [&](Error e, const SourceInfo& o, const std::string& error_msg) {
          raise_error(e, o, error_msg);
          return -1;
        },
        part.known);
  }

  // Populate partial data
  if (part.args != nullptr)
    part.args->set_attr(ExprAttr::SequenceItem);
  if (part.kwArgs != nullptr)
    part.kwArgs->set_attr(ExprAttr::SequenceItem);
  if (part.isPartial) {
    expr->args.pop_back();
    if (!part.args)
      part.args = transform(N<TupleExpr>());  // use ()
    if (!part.kwArgs) {
      auto kw_name = generate_tuple(0, TYPE_KWTUPLE, {});
      part.kwArgs = transform(N<CallExpr>(N<IdExpr>(kw_name)));  // use KwTuple()
    }
  }

  // Unify function type generics with the provided generics
  seqassert((expr->has_attr(ExprAttr::OrderedCall) && type_args.empty()) ||
                (!expr->has_attr(ExprAttr::OrderedCall) &&
                 type_args.size() == callee_fn->func_generics.size()),
            "bad vector sizes");
  if (!callee_fn->func_generics.empty()) {
    auto ni_generics = callee_fn->ast->get_non_inferrable_generics();
    for (size_t si = 0; !expr->has_attr(ExprAttr::OrderedCall) &&
                        si < callee_fn->func_generics.size();
         si++) {
      if (type_args[si]) {
        auto typ = type_args[si]->type;
        if (callee_fn->func_generics[si].type->is_static_type()) {
          if (!type_args[si]->is_static()) {
            Err(Error::EXPECTED_STATIC, type_args[si]);
          }
          typ = Type::Type::make_static(ctx->cache, type_args[si]);
        }
        unify(typ, callee_fn->func_generics[si].type);
      } else {
        if (callee_fn->func_generics[si].type->get_unbound() &&
            !callee_fn->ast->args[si].default_value && !partial &&
            in(ni_generics, callee_fn->func_generics[si].name)) {
          error("generic '{}' not provided",
                callee_fn->func_generics[si].nice_name);
        }
      }
    }
  }

  // Special case: function instantiation (e.g., `foo(T=int)`)
  auto cnt = 0;
  for (auto& t : type_args)
    if (t)
      cnt++;
  if (part.isPartial && cnt && cnt == expr->args.size()) {
    transform(
        expr->expr);  // transform again because it might have been changed
    unify(expr->type, expr->expr->get_type());
    // Return the callee with the corrected type and do not go further
    return expr->expr;
  }

  expr->args = args;
  expr->set_attr(ExprAttr::OrderedCall);
  part.known = new_mask;
  return nullptr;
}

/// Unify the call arguments' types with the function declaration signatures.
/// Also apply argument transformations to ensure the type compatibility and
/// handle default generics.
/// @example
///   `foo(1, 2)` -> `foo(1, Optional(2), T=int)`
auto TypecheckVisitor::typecheck_call_args(const Type::FuncTypePtr& callee_fn,
                                         std::vector<CallExpr::Arg>& args) -> bool {
  bool wrapping_done = true;  // tracks whether all arguments are wrapped
  std::vector<Type::TypePtr> replacements;  // list of replacement arguments
  for (size_t si = 0; si < callee_fn->get_arg_types().size(); si++) {
    if (startswith(callee_fn->ast->args[si].name, "*") &&
        callee_fn->ast->args[si].type && args[si].value->get_call()) {
      // Special case: `*args: type` and `**kwargs: type`
      auto typ = transform(clone(callee_fn->ast->args[si].type))->type;
      for (auto& ca : args[si].value->get_call()->args) {
        if (wrap_expr(ca.value, typ, callee_fn)) {
          unify(ca.value->type, typ);
        } else {
          wrapping_done = false;
        }
      }
      auto name = args[si].value->type->get_class()->name;
      args[si].value = transform(
          N<CallExpr>(N<IdExpr>(name), args[si].value->get_call()->args));
      replacements.push_back(args[si].value->type);
    } else {
      if (wrap_expr(args[si].value, callee_fn->get_arg_types()[si], callee_fn)) {
        unify(args[si].value->type, callee_fn->get_arg_types()[si]);
      } else {
        wrapping_done = false;
      }
      replacements.push_back(!callee_fn->get_arg_types()[si]->get_class()
                                 ? args[si].value->type
                                 : callee_fn->get_arg_types()[si]);
    }
  }

  // Realize arguments
  bool done = true;
  for (auto& a : args) {
    // Previous unifications can qualify existing identifiers.
    // Transform again to get the full identifier
    if (realize(a.value->type))
      transform(a.value);
    done &= a.value->is_done();
  }

  // Handle default generics
  for (size_t i = 0, j = 0; wrapping_done && i < callee_fn->ast->args.size(); i++)
    if (callee_fn->ast->args[i].status == Param::Generic) {
      if (callee_fn->ast->args[i].default_value &&
          callee_fn->func_generics[j].type->get_unbound()) {
        ctx->add_block();  // add function generics to typecheck default
                          // arguments
        add_function_generics(callee_fn->get_func().get());
        auto def = transform(clone(callee_fn->ast->args[i].default_value));
        ctx->pop_block();
        unify(callee_fn->func_generics[j].type,
              def->is_static() ? Type::Type::make_static(ctx->cache, def)
                              : def->get_type());
      }
      j++;
    }

  // Replace the arguments
  for (size_t si = 0; si < replacements.size(); si++) {
    if (replacements[si])
      callee_fn->get_arg_types()[si] = replacements[si];
  }

  return done;
}

/// Transform and typecheck the following special call expressions:
///   `superf(fn)`
///   `super()`
///   `__ptr__(var)`
///   `__array__[int](sz)`
///   `isinstance(obj, type)`
///   `staticlen(tup)`
///   `hasattr(obj, "attr")`
///   `getattr(obj, "attr")`
///   `type(obj)`
///   `compile_err("msg")`
/// See below for more details.
auto TypecheckVisitor::transform_special_call(
    CallExpr* expr) -> std::pair<bool, ExprPtr> {
  if (!expr->expr->get_id())
    return {false, nullptr};
  auto val = expr->expr->get_id()->value;
  if (val == "superf") {
    return {true, transform_super_f(expr)};
  } else if (val == "super:0") {
    return {true, transform_super()};
  } else if (val == "__ptr__") {
    return {true, transform_ptr(expr)};
  } else if (val == "__array__.__new__:0") {
    return {true, transform_array(expr)};
  } else if (val == "isinstance") {
    return {true, transform_is_instance(expr)};
  } else if (val == "staticlen") {
    return {true, transform_static_len(expr)};
  } else if (startswith(val, "hasattr:")) {
    return {true, transform_has_attr(expr)};
  } else if (val == "getattr") {
    return {true, transform_get_attr(expr)};
  } else if (val == "setattr") {
    return {true, transform_set_attr(expr)};
  } else if (val == "type.__new__:0") {
    return {true, transform_type_fn(expr)};
  } else if (val == "compile_error") {
    return {true, transform_compile_error(expr)};
  } else if (val == "tuple") {
    return {true, transform_tuple_fn(expr)};
  } else if (val == "__realized__") {
    return {true, transform_realized_fn(expr)};
  } else if (val == "std.internal.static.static_print") {
    return {false, transform_static_print_fn(expr)};
  } else if (val == "__has_rtti__") {
    return {true, transform_has_rtti_fn(expr)};
  } else {
    return transform_internal_static_fn(expr);
  }
}

/// Typecheck superf method. This method provides the access to the previous
/// matching overload.
/// @example
///   ```class cls:
///        def foo(): print('foo 1')
///        def foo():
///          superf()  # access the previous foo
///          print('foo 2')
///      cls.foo()```
///   prints "foo 1" followed by "foo 2"
auto TypecheckVisitor::transform_super_f(CallExpr* expr) -> ExprPtr {
  auto func = ctx->get_realization_base()->type->get_func();

  // Find list of matching superf methods
  std::vector<Type::FuncTypePtr> supers;
  if (!func->ast->attributes.parent_class.empty() &&
      !endswith(func->ast->name, ":dispatch")) {
    auto p = ctx->find(func->ast->attributes.parent_class)->type;
    if (p && p->get_class()) {
      if (auto c = in(ctx->cache->classes, p->get_class()->name)) {
        if (auto m = in(c->methods, ctx->cache->rev(func->ast->name))) {
          for (auto& overload : ctx->cache->overloads[*m]) {
            if (endswith(overload.name, ":dispatch"))
              continue;
            if (overload.name == func->ast->name)
              break;
            supers.emplace_back(ctx->cache->functions[overload.name].type);
          }
        }
      }
      std::reverse(supers.begin(), supers.end());
    }
  }
  if (supers.empty())
    Err(Error::CALL_SUPERF, expr);
  auto m = find_matching_methods(
      func->func_parent ? func->func_parent->get_class() : nullptr, supers,
      expr->args);
  if (m.empty())
    Err(Error::CALL_SUPERF, expr);
  return transform(N<CallExpr>(N<IdExpr>(m[0]->ast->name), expr->args));
}

/// Typecheck and transform super method. Replace it with the current self
/// object cast to the first inherited type.
/// TODO: only an empty super() is currently supported.
auto TypecheckVisitor::transform_super() -> ExprPtr {
  if (!ctx->get_realization_base()->type)
    Err(Error::CALL_SUPER_PARENT, get_source_info());
  auto func_typ = ctx->get_realization_base()->type->get_func();
  if (!func_typ || !func_typ->ast->has_attr(Attr::Method))
    Err(Error::CALL_SUPER_PARENT, get_source_info());
  if (func_typ->get_arg_types().empty())
    Err(Error::CALL_SUPER_PARENT, get_source_info());

  Type::ClassTypePtr typ = func_typ->get_arg_types()[0]->get_class();
  auto cands = ctx->cache->classes[typ->name].static_parent_classes;
  if (cands.empty()) {
    // Dynamic inheritance: use MRO
    // TODO: maybe super() should be split into two separate functions...
    auto vCands = ctx->cache->classes[typ->name].mro;
    if (vCands.size() < 2)
      Err(Error::CALL_SUPER_PARENT, get_source_info());

    auto super_typ = ctx->instantiate(vCands[1]->type, typ)->get_class();
    auto self = N<IdExpr>(func_typ->ast->args[0].name);
    self->type = typ;

    auto typ_expr = N<IdExpr>(super_typ->name);
    typ_expr->set_type(super_typ);
    return transform(
        N<CallExpr>(N<DotExpr>(N<IdExpr>("__internal__"), "class_super"), self,
                    typ_expr, N<IntExpr>(1)));
  }

  auto name = cands.front();  // the first inherited type
  auto super_typ = ctx->instantiate(ctx->force_find(name)->type)->get_class();
  if (typ->get_record()) {
    // Case: tuple types. Return `tuple(obj.args...)`
    std::vector<ExprPtr> members;
    for (auto& field : get_class_fields(super_typ.get()))
      members.push_back(
          N<DotExpr>(N<IdExpr>(func_typ->ast->args[0].name), field.name));
    ExprPtr e = transform(N<TupleExpr>(members));
    e->type = unify(super_typ, e->type);  // see super_tuple test for this line
    return e;
  } else {
    // Case: reference types. Return `__internal__.class_super(self, T)`
    auto self = N<IdExpr>(func_typ->ast->args[0].name);
    self->type = typ;
    return cast_to_super_class(self, super_typ);
  }
}

/// Typecheck __ptr__ method. This method creates a pointer to an object. Ensure
/// that the argument is a variable binding.
auto TypecheckVisitor::transform_ptr(CallExpr* expr) -> ExprPtr {
  auto id = expr->args[0].value->get_id();
  auto val = id ? ctx->find(id->value) : nullptr;
  if (!val || val->kind != TypecheckItem::Var)
    Err(Error::CALL_PTR_VAR, expr->args[0]);

  transform(expr->args[0].value);
  unify(expr->type, ctx->instantiate_generic(ctx->get_type("Ptr"),
                                            {expr->args[0].value->type}));
  if (expr->args[0].value->is_done())
    expr->set_done();
  return nullptr;
}

/// Typecheck __array__ method. This method creates a stack-allocated array via
/// alloca.
auto TypecheckVisitor::transform_array(CallExpr* expr) -> ExprPtr {
  auto arr_typ = expr->expr->type->get_func();
  unify(expr->type, ctx->instantiate_generic(
                        ctx->get_type("Array"),
                        {arr_typ->func_parent->get_class()->generics[0].type}));
  if (realize(expr->type))
    expr->set_done();
  return nullptr;
}

/// Transform isinstance method to a static boolean expression.
/// Special cases:
///   `isinstance(obj, ByVal)` is True if `type(obj)` is a tuple type
///   `isinstance(obj, ByRef)` is True if `type(obj)` is a reference type
auto TypecheckVisitor::transform_is_instance(CallExpr* expr) -> ExprPtr {
  expr->set_type(unify(expr->type, ctx->get_type("bool")));
  expr->static_value.type =
      StaticValue::INT;  // prevent branching until this is resolved
  transform(expr->args[0].value);
  auto typ = expr->args[0].value->type->get_class();
  if (!typ || !typ->can_realize())
    return nullptr;

  transform(expr->args[0].value);  // transform again to realize it

  auto& typ_expr = expr->args[1].value;
  if (auto c = typ_expr->get_call()) {
    // Handle `isinstance(obj, (type1, type2, ...))`
    if (typ_expr->orig_expr && typ_expr->orig_expr->get_tuple()) {
      ExprPtr result = transform(N<BoolExpr>(false));
      for (auto& i : typ_expr->orig_expr->get_tuple()->items) {
        result = transform(N<BinaryExpr>(
            result, "||",
            N<CallExpr>(N<IdExpr>("isinstance"), expr->args[0].value, i)));
      }
      return result;
    }
  }

  expr->static_value.type = StaticValue::INT;
  if (typ_expr->is_id(TYPE_TUPLE) || typ_expr->is_id("tuple")) {
    return transform(N<BoolExpr>(typ->name == TYPE_TUPLE));
  } else if (typ_expr->is_id("ByVal")) {
    return transform(N<BoolExpr>(typ->get_record() != nullptr));
  } else if (typ_expr->is_id("ByRef")) {
    return transform(N<BoolExpr>(typ->get_record() == nullptr));
  } else if (!typ_expr->type->get_union() && typ->get_union()) {
    auto union_types = typ->get_union()->get_realization_types();
    int tag = -1;
    for (size_t ui = 0; ui < union_types.size(); ui++) {
      if (typ_expr->type->unify(union_types[ui].get(), nullptr) >= 0) {
        tag = ui;
        break;
      }
    }
    if (tag == -1)
      return transform(N<BoolExpr>(false));
    return transform(
        N<BinaryExpr>(N<CallExpr>(N<IdExpr>("__internal__.union_get_tag:0"),
                                  expr->args[0].value),
                      "==", N<IntExpr>(tag)));
  } else if (typ_expr->type->is("pyobj") && !typ_expr->is_type()) {
    if (typ->is("pyobj")) {
      expr->static_value.type = StaticValue::NOT_STATIC;
      return transform(
          N<CallExpr>(N<IdExpr>("std.internal.python._isinstance:0"),
                      expr->args[0].value, expr->args[1].value));
    } else {
      return transform(N<BoolExpr>(false));
    }
  }

  transform_type(typ_expr);

  // Check super types (i.e., statically inherited) as well
  for (auto& tx : get_super_types(typ->get_class())) {
    Type::Type::Unification us;
    auto s = tx->unify(typ_expr->type.get(), &us);
    us.undo();
    if (s >= 0)
      return transform(N<BoolExpr>(true));
  }
  return transform(N<BoolExpr>(false));
}

/// Transform staticlen method to a static integer expression. This method
/// supports only static strings and tuple types.
auto TypecheckVisitor::transform_static_len(CallExpr* expr) -> ExprPtr {
  expr->static_value.type = StaticValue::INT;
  transform(expr->args[0].value);
  auto typ = expr->args[0].value->get_type();

  if (auto s = typ->get_static()) {
    // Case: staticlen on static strings
    if (s->expr->static_value.type != StaticValue::STRING)
      Err(Error::EXPECTED_STATIC_SPECIFIED, expr->args[0].value, "string");
    if (!s->expr->static_value.evaluated)
      return nullptr;
    return transform(N<IntExpr>(s->expr->static_value.get_string().size()));
  }
  if (!typ->get_class())
    return nullptr;
  if (typ->get_union()) {
    if (realize(typ))
      return transform(
          N<IntExpr>(typ->get_union()->get_realization_types().size()));
    return nullptr;
  }
  if (!typ->get_record())
    Err(Error::EXPECTED_TUPLE, expr->args[0].value);
  return transform(N<IntExpr>(typ->get_record()->args.size()));
}

/// Transform hasattr method to a static boolean expression.
/// This method also supports additional argument types that are used to check
/// for a matching overload (not available in Python).
auto TypecheckVisitor::transform_has_attr(CallExpr* expr) -> ExprPtr {
  expr->static_value.type = StaticValue::INT;
  auto typ = expr->args[0].value->get_type()->get_class();
  if (!typ)
    return nullptr;

  auto member = expr->expr->type->get_func()
                    ->func_generics[0]
                    .type->get_static()
                    ->evaluate()
                    .get_string();
  std::vector<std::pair<std::string, Type::TypePtr>> args{{"", typ}};
  if (expr->expr->is_id("hasattr:0")) {
    // Case: the first hasattr overload allows passing argument types via *args
    auto tup = expr->args[1].value->get_tuple();
    seqassert(tup, "not a tuple");
    for (auto& a : tup->items) {
      transform(a);
      if (!a->get_type()->get_class())
        return nullptr;
      args.push_back({"", a->get_type()});
    }
    auto kwtup = expr->args[2].value->orig_expr->get_call();
    seqassert(expr->args[2].value->orig_expr &&
                  expr->args[2].value->orig_expr->get_call(),
              "expected call: {}", expr->args[2].value->orig_expr);
    auto kw = expr->args[2].value->orig_expr->get_call();
    auto kw_cls = in(ctx->cache->classes,
                    expr->args[2].value->get_type()->get_class()->name);
    seqassert(kw_cls, "cannot find {}",
              expr->args[2].value->get_type()->get_class()->name);
    for (size_t i = 0; i < kw->args.size(); i++) {
      auto& a = kw->args[i].value;
      transform(a);
      if (!a->get_type()->get_class())
        return nullptr;
      args.push_back({kw_cls->fields[i].name, a->get_type()});
    }
  }

  bool exists = !ctx->find_method(typ->get_class().get(), member).empty() ||
                ctx->find_member(typ->get_class(), member);
  if (exists && args.size() > 1)
    exists &= find_best_method(typ, member, args) != nullptr;
  return transform(N<BoolExpr>(exists));
}

/// Transform getattr method to a DotExpr.
ExprPtr TypecheckVisitor::transform_get_attr(CallExpr* expr) {
  auto func_typ = expr->expr->type->get_func();
  auto static_typ = func_typ->func_generics[0].type->get_static();
  if (!static_typ->can_realize())
    return nullptr;
  return transform(
      N<DotExpr>(expr->args[0].value, static_typ->evaluate().get_string()));
}

/// Transform setattr method to a AssignMemberStmt.
ExprPtr TypecheckVisitor::transform_set_attr(CallExpr* expr) {
  auto func_typ = expr->expr->type->get_func();
  auto static_typ = func_typ->func_generics[0].type->get_static();
  if (!static_typ->can_realize())
    return nullptr;
  return transform(
      N<StmtExpr>(N<AssignMemberStmt>(expr->args[0].value,
                                      static_typ->evaluate().get_string(),
                                      expr->args[1].value),
                  N<CallExpr>(N<IdExpr>("NoneType"))));
}

/// Raise a compiler error.
ExprPtr TypecheckVisitor::transform_compile_error(CallExpr* expr) {
  auto func_typ = expr->expr->type->get_func();
  auto static_typ = func_typ->func_generics[0].type->get_static();
  if (static_typ->can_realize())
    Err(Error::CUSTOM, expr, static_typ->evaluate().get_string());
  return nullptr;
}

/// Convert a class to a tuple.
ExprPtr TypecheckVisitor::transform_tuple_fn(CallExpr* expr) {
  auto cls = expr->args.front().value->type->get_class();
  if (!cls)
    return nullptr;

  // tuple(ClassType) is a tuple type that corresponds to a class
  if (expr->args.front().value->is_type()) {
    if (!realize(cls))
      return expr->clone();

    std::vector<ExprPtr> items;
    for (auto& ft : get_class_fields(cls.get())) {
      auto t = ctx->instantiate(ft.type, cls);
      auto rt = realize(t);
      seqassert(rt, "cannot realize '{}' in {}", t, ft.name);
      items.push_back(NT<IdExpr>(t->realized_name()));
    }
    auto e = transform(NT<InstantiateExpr>(N<IdExpr>(TYPE_TUPLE), items));
    return e;
  }

  std::vector<ExprPtr> args;
  std::string var = ctx->cache->get_temporary_var("tup");
  for (auto& field : get_class_fields(cls.get()))
    args.emplace_back(N<DotExpr>(N<IdExpr>(var), field.name));

  return transform(
      N<StmtExpr>(N<AssignStmt>(N<IdExpr>(var), expr->args.front().value),
                  N<TupleExpr>(args)));
}

/// Transform type function to a type IdExpr identifier.
ExprPtr TypecheckVisitor::transform_type_fn(CallExpr* expr) {
  expr->mark_type();
  transform(expr->args[0].value);

  unify(expr->type, expr->args[0].value->get_type());

  if (!realize(expr->type))
    return nullptr;

  auto e = NT<IdExpr>(expr->type->realized_name());
  e->set_type(expr->type);
  e->set_done();
  return e;
}

/// Transform __realized__ function to a fully realized type identifier.
ExprPtr TypecheckVisitor::transform_realized_fn(CallExpr* expr) {
  auto call = transform(
      N<CallExpr>(expr->args[0].value, N<StarExpr>(expr->args[1].value)));
  if (!call->get_call()->expr->type->get_func())
    Err(Error::CALL_REALIZED_FN, expr->args[0].value);
  if (auto f = realize(call->get_call()->expr->type)) {
    auto e = N<IdExpr>(f->get_func()->realized_name());
    e->set_type(f);
    e->set_done();
    return e;
  }
  return nullptr;
}

/// Transform __static_print__ function to a fully realized type identifier.
ExprPtr TypecheckVisitor::transform_static_print_fn(CallExpr* expr) {
  auto& args = expr->args[0].value->get_call()->args;
  for (size_t i = 0; i < args.size(); i++) {
    realize(args[i].value->type);
    fmt::print(stderr, "[static_print] {}: {} := {}{} (iter: {})\n",
               get_source_info(), FormatVisitor::apply(args[i].value),
               args[i].value->type ? args[i].value->type->debug_string(1) : "-",
               args[i].value->is_static() ? " [static]" : "",
               ctx->get_realization_base()->iteration);
  }
  return nullptr;
}

/// Transform __has_rtti__ to a static boolean that indicates RTTI status of a
/// type.
ExprPtr TypecheckVisitor::transform_has_rtti_fn(CallExpr* expr) {
  expr->static_value.type = StaticValue::INT;
  auto func_typ = expr->expr->type->get_func();
  auto t = func_typ->func_generics[0].type->get_class();
  if (!t)
    return nullptr;
  auto c = in(ctx->cache->classes, t->name);
  seqassert(c, "bad class {}", t->name);
  return transform(N<BoolExpr>(const_cast<Cache::Class*>(c)->rtti));
}

// Transform internal.static calls
std::pair<bool, ExprPtr> TypecheckVisitor::transform_internal_static_fn(
    CallExpr* expr) {
  unify(expr->type, ctx->get_unbound());
  if (expr->expr->is_id("std.internal.static.fn_can_call")) {
    expr->static_value.type = StaticValue::INT;
    auto typ = expr->args[0].value->get_type()->get_class();
    if (!typ)
      return {true, nullptr};

    auto fn = expr->args[0].value->type->get_func();
    if (!fn)
      error("expected a function, got '{}'",
            expr->args[0].value->type->pretty_string());

    auto inargs = unpack_tuple_types(expr->args[1].value);
    auto kwargs = unpack_tuple_types(expr->args[2].value);
    seqassert(inargs && kwargs, "bad call to fn_can_call");

    std::vector<CallExpr::Arg> callArgs;
    for (auto& a : *inargs) {
      callArgs.push_back(
          {a.first, std::make_shared<NoneExpr>()});  // dummy expression
      callArgs.back().value->set_type(a.second);
    }
    for (auto& a : *kwargs) {
      callArgs.push_back(
          {a.first, std::make_shared<NoneExpr>()});  // dummy expression
      callArgs.back().value->set_type(a.second);
    }
    return {true, transform(N<BoolExpr>(can_call(fn, callArgs) >= 0))};
  } else if (expr->expr->is_id("std.internal.static.fn_arg_has_type")) {
    expr->static_value.type = StaticValue::INT;
    auto fn = ctx->extract_function(expr->args[0].value->type);
    if (!fn)
      error("expected a function, got '{}'",
            expr->args[0].value->type->pretty_string());
    auto idx =
        ctx->get_static_int(expr->expr->type->get_func()->func_generics[0].type);
    seqassert(idx, "expected a static integer");
    auto& args = fn->get_arg_types();
    return {true, transform(N<BoolExpr>(*idx >= 0 && *idx < args.size() &&
                                        args[*idx]->can_realize()))};
  } else if (expr->expr->is_id("std.internal.static.fn_arg_get_type")) {
    auto fn = ctx->extract_function(expr->args[0].value->type);
    if (!fn)
      error("expected a function, got '{}'",
            expr->args[0].value->type->pretty_string());
    auto idx =
        ctx->get_static_int(expr->expr->type->get_func()->func_generics[0].type);
    seqassert(idx, "expected a static integer");
    auto& args = fn->get_arg_types();
    if (*idx < 0 || *idx >= args.size() || !args[*idx]->can_realize())
      error("argument does not have type");
    return {true, transform(NT<IdExpr>(args[*idx]->realized_name()))};
  } else if (expr->expr->is_id("std.internal.static.fn_args")) {
    auto fn = ctx->extract_function(expr->args[0].value->type);
    if (!fn)
      error("expected a function, got '{}'",
            expr->args[0].value->type->pretty_string());
    std::vector<ExprPtr> v;
    for (size_t i = 0; i < fn->ast->args.size(); i++) {
      auto n = fn->ast->args[i].name;
      trim_stars(n);
      n = ctx->cache->rev(n);
      v.push_back(N<StringExpr>(n));
    }
    return {true, transform(N<TupleExpr>(v))};
  } else if (expr->expr->is_id("std.internal.static.fn_has_default")) {
    expr->static_value.type = StaticValue::INT;
    auto fn = ctx->extract_function(expr->args[0].value->type);
    if (!fn)
      error("expected a function, got '{}'",
            expr->args[0].value->type->pretty_string());
    auto idx =
        ctx->get_static_int(expr->expr->type->get_func()->func_generics[0].type);
    seqassert(idx, "expected a static integer");
    auto& args = fn->ast->args;
    if (*idx < 0 || *idx >= args.size())
      error("argument out of bounds");
    return {true, transform(N<IntExpr>(args[*idx].default_value != nullptr))};
  } else if (expr->expr->is_id("std.internal.static.fn_get_default")) {
    auto fn = ctx->extract_function(expr->args[0].value->type);
    if (!fn)
      error("expected a function, got '{}'",
            expr->args[0].value->type->pretty_string());
    auto idx =
        ctx->get_static_int(expr->expr->type->get_func()->func_generics[0].type);
    seqassert(idx, "expected a static integer");
    auto& args = fn->ast->args;
    if (*idx < 0 || *idx >= args.size())
      error("argument out of bounds");
    return {true, transform(args[*idx].default_value)};
  } else if (expr->expr->is_id("std.internal.static.fn_wrap_call_args")) {
    auto typ = expr->args[0].value->get_type()->get_class();
    if (!typ)
      return {true, nullptr};

    auto fn = ctx->extract_function(expr->args[0].value->type);
    if (!fn)
      error("expected a function, got '{}'",
            expr->args[0].value->type->pretty_string());

    std::vector<CallExpr::Arg> callArgs;
    if (auto tup = expr->args[1].value->orig_expr->get_tuple()) {
      for (auto& a : tup->items) {
        callArgs.push_back({"", a});
      }
    }
    if (auto kw = expr->args[1].value->orig_expr->get_call()) {
      auto kw_cls = in(ctx->cache->classes, expr->get_type()->get_class()->name);
      seqassert(kw_cls, "cannot find {}", expr->get_type()->get_class()->name);
      for (size_t i = 0; i < kw->args.size(); i++) {
        callArgs.push_back({kw_cls->fields[i].name, kw->args[i].value});
      }
    }
    auto zzz = transform(N<CallExpr>(N<IdExpr>(fn->ast->name), callArgs));
    if (!zzz->is_done())
      return {true, nullptr};

    std::vector<ExprPtr> tupArgs;
    for (auto& a : zzz->get_call()->args)
      tupArgs.push_back(a.value);
    return {true, transform(N<TupleExpr>(tupArgs))};
  } else if (expr->expr->is_id("std.internal.static.vars")) {
    auto func_typ = expr->expr->type->get_func();
    auto t = func_typ->func_generics[0].type->get_static();
    if (!t)
      return {true, nullptr};
    auto withIdx = t->evaluate().get_int();

    Type::ClassTypePtr typ = nullptr;
    std::vector<ExprPtr> tupleItems;
    auto e = transform(expr->args[0].value);
    if (!(typ = e->type->get_class()))
      return {true, nullptr};

    size_t idx = 0;
    for (auto& f : get_class_fields(typ.get())) {
      auto k = N<StringExpr>(f.name);
      auto v = N<DotExpr>(expr->args[0].value, f.name);
      if (withIdx) {
        auto i = N<IntExpr>(idx);
        tupleItems.push_back(N<TupleExpr>(std::vector<ExprPtr>{i, k, v}));
      } else {
        tupleItems.push_back(N<TupleExpr>(std::vector<ExprPtr>{k, v}));
      }
      idx++;
    }
    return {true, transform(N<TupleExpr>(tupleItems))};
  } else if (expr->expr->is_id("std.internal.static.tuple_type")) {
    auto func_typ = expr->expr->type->get_func();
    auto t = func_typ->func_generics[0].type;
    if (!t || !realize(t))
      return {true, nullptr};
    auto tn = func_typ->func_generics[1].type->get_static();
    if (!tn)
      return {true, nullptr};
    auto n = tn->evaluate().get_int();
    Type::TypePtr typ = nullptr;
    if (t->get_record()) {
      if (n < 0 || n >= t->get_record()->args.size())
        error("invalid index");
      typ = t->get_record()->args[n];
    } else {
      auto f = get_class_fields(t->get_class().get());
      if (n < 0 || n >= f.size())
        error("invalid index");
      typ = ctx->instantiate(f[n].type, t->get_class());
    }
    typ = realize(typ);
    return {true, transform(NT<IdExpr>(typ->realized_name()))};
  } else {
    return {false, nullptr};
  }
}

/// Get the list that describes the inheritance hierarchy of a given type.
/// The first type in the list is the most recently inherited type.
std::vector<Type::ClassTypePtr> TypecheckVisitor::get_super_types(
    const Type::ClassTypePtr& cls) {
  std::vector<Type::ClassTypePtr> result;
  if (!cls)
    return result;

  result.push_back(cls);
  for (auto& name : ctx->cache->classes[cls->name].static_parent_classes) {
    auto parentTyp = ctx->instantiate(ctx->force_find(name)->type)->get_class();
    for (auto& field : get_class_fields(cls.get())) {
      for (auto& parentField : get_class_fields(parentTyp.get()))
        if (field.name == parentField.name) {
          unify(ctx->instantiate(field.type, cls),
                ctx->instantiate(parentField.type, parentTyp));
          break;
        }
    }
    for (auto& t : get_super_types(parentTyp))
      result.push_back(t);
  }
  return result;
}

/// Find all generics on which a function depends on and add them to the current
/// context.
void TypecheckVisitor::add_function_generics(const Type::FuncType* t) {
  for (auto parent = t->func_parent; parent;) {
    if (auto f = parent->get_func()) {
      // Add parent function generics
      for (auto& g : f->func_generics) {
        // LOG("   -> {} := {}", g.name, g.type->debug_string(true));
        ctx->add(TypecheckItem::Type, g.name, g.type);
      }
      parent = f->func_parent;
    } else {
      // Add parent class generics
      seqassert(parent->get_class(), "not a class: {}", parent);
      for (auto& g : parent->get_class()->generics) {
        // LOG("   => {} := {}", g.name, g.type->debug_string(true));
        ctx->add(TypecheckItem::Type, g.name, g.type);
      }
      for (auto& g : parent->get_class()->hidden_generics) {
        // LOG("   :> {} := {}", g.name, g.type->debug_string(true));
        ctx->add(TypecheckItem::Type, g.name, g.type);
      }
      break;
    }
  }
  // Add function generics
  for (auto& g : t->func_generics) {
    // LOG("   >> {} := {}", g.name, g.type->debug_string(true));
    ctx->add(TypecheckItem::Type, g.name, g.type);
  }
}

/// Generate a partial type `Partial.N<mask>` for a given function.
/// @param mask a 0-1 vector whose size matches the number of function
/// arguments.
///             1 indicates that the argument has been provided and is cached
///             within the partial object.
/// @example
///   ```@tuple
///      class Partial.N101[T0, T2]:
///        item0: T0  # the first cached argument
///        item2: T2  # the third cached argument
std::string TypecheckVisitor::generate_partial_stub(const std::vector<char>& mask,
                                                  Type::FuncType* fn) {
  std::string strMask(mask.size(), '1');
  int tupleSize = 0, genericSize = 0;
  for (size_t i = 0; i < mask.size(); i++) {
    if (!mask[i])
      strMask[i] = '0';
    else if (fn->ast->args[i].status == Param::Normal)
      tupleSize++;
    else
      genericSize++;
  }
  auto typeName = format(TYPE_PARTIAL "{}.{}", strMask, fn->to_string());
  if (!ctx->find(typeName)) {
    ctx->cache->partials[typeName] = {fn->generalize(0)->get_func(), mask};
    generate_tuple(tupleSize + 2, typeName, {}, false);
  }
  return typeName;
}

}  // namespace Pud::AST