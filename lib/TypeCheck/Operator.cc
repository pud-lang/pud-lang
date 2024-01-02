#include <string>
#include <tuple>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

namespace Pud::AST {

/// Replace unary operators with the appropriate magic calls.
/// Also evaluate static expressions. See @c evaluate_static_unary for details.
void TypecheckVisitor::visit(UnaryExpr* expr) {
  transform(expr->expr);

  static std::unordered_map<StaticValue::Type, std::unordered_set<std::string>>
      static_ops = {{StaticValue::INT, {"-", "+", "!", "~"}},
                   {StaticValue::STRING, {"@"}}};
  // Handle static expressions
  if (expr->expr->is_static() &&
      in(static_ops[expr->expr->static_value.type], expr->op)) {
    result_expr = evaluate_static_unary(expr);
    return;
  }

  if (expr->op == "!") {
    // `not expr` -> `expr.__bool__().__invert__()`
    result_expr = transform(N<CallExpr>(N<DotExpr>(
        N<CallExpr>(N<DotExpr>(clone(expr->expr), "__bool__")), "__invert__")));
  } else {
    std::string magic;
    if (expr->op == "~")
      magic = "invert";
    else if (expr->op == "+")
      magic = "pos";
    else if (expr->op == "-")
      magic = "neg";
    else
      seqassert(false, "invalid unary operator '{}'", expr->op);
    result_expr = transform(
        N<CallExpr>(N<DotExpr>(clone(expr->expr), format("__{}__", magic))));
  }
}

/// Replace binary operators with the appropriate magic calls.
/// See @c transform_binary_simple , @c transform_binary_is , @c
/// transform_binary_magic and
/// @c transform_binary_inplace_magic for details.
/// Also evaluate static expressions. See @c evaluate_static_binary for details.
void TypecheckVisitor::visit(BinaryExpr* expr) {
  // Transform lexpr and rexpr. Ignore Nones for now
  if (!(startswith(expr->op, "is") && expr->lexpr->get_none()))
    transform(expr->lexpr);
  if (!(startswith(expr->op, "is") && expr->rexpr->get_none()))
    transform(expr->rexpr);

  static std::unordered_map<StaticValue::Type, std::unordered_set<std::string>>
      static_ops = {{StaticValue::INT,
                    {"<", "<=", ">", ">=", "==", "!=", "&&", "||", "+", "-",
                     "*", "//", "%", "&", "|", "^", ">>", "<<"}},
                   {StaticValue::STRING, {"==", "!=", "+"}}};
  if (expr->lexpr->is_static() && expr->rexpr->is_static() &&
      expr->lexpr->static_value.type == expr->rexpr->static_value.type &&
      in(static_ops[expr->rexpr->static_value.type], expr->op)) {
    // Handle static expressions
    result_expr = evaluate_static_binary(expr);
  } else if (auto e = transform_binary_simple(expr)) {
    // Case: simple binary expressions
    result_expr = e;
  } else if (expr->lexpr->get_type()->get_unbound() ||
             (expr->op != "is" && expr->rexpr->get_type()->get_unbound())) {
    // Case: types are unknown, so continue later
    unify(expr->type, ctx->get_unbound());
    return;
  } else if (expr->op == "is") {
    // Case: is operator
    result_expr = transform_binary_is(expr);
  } else {
    if (auto ei = transform_binary_inplace_magic(expr, false)) {
      // Case: in-place magic methods
      result_expr = ei;
    } else if (auto em = transform_binary_magic(expr)) {
      // Case: normal magic methods
      result_expr = em;
    } else if (expr->lexpr->get_type()->is(TYPE_OPTIONAL)) {
      // Special case: handle optionals if everything else fails.
      // Assumes that optionals have no relevant magics (except for __eq__)
      result_expr = transform(
          N<BinaryExpr>(N<CallExpr>(N<IdExpr>(FN_UNWRAP), expr->lexpr),
                        expr->op, expr->rexpr, expr->in_place));
    } else {
      // Nothing found: report an error
      Err(Error::OP_NO_MAGIC, expr, expr->op, expr->lexpr->type->pretty_string(),
        expr->rexpr->type->pretty_string());
    }
  }
}

/// Helper function that locates the pipe ellipsis within a collection of
/// (possibly nested) CallExprs.
/// @return  List of CallExprs and their locations within the parent CallExpr
///          needed to access the ellipsis.
/// @example `foo(bar(1, baz(...)))` returns `[{0, baz}, {1, bar}, {0, foo}]`
auto find_ellipsis(ExprPtr expr) -> std::vector<std::pair<size_t, ExprPtr>> {
  auto call = expr->get_call();
  if (!call)
    return {};
  for (size_t ai = 0; ai < call->args.size(); ai++) {
    if (auto el = call->args[ai].value->get_ellipsis()) {
      if (el->mode == EllipsisExpr::PIPE)
        return {{ai, expr}};
    } else if (call->args[ai].value->get_call()) {
      auto v = find_ellipsis(call->args[ai].value);
      if (!v.empty()) {
        v.emplace_back(ai, expr);
        return v;
      }
    }
  }
  return {};
}

/// Typecheck pipe expressions.
/// Each stage call `foo(x)` without an ellipsis will be transformed to
/// `foo(..., x)`. Stages that are not in the form of CallExpr will be
/// transformed to it (e.g., `foo`
/// -> `foo(...)`).
/// Special care is taken of stages that can expand to multiple stages (e.g., `a
/// |> foo` might become `a |> unwrap |> foo` to satisfy type constraints; see
/// @c wrap_expr for details).
void TypecheckVisitor::visit(PipeExpr* expr) {
  bool has_generator = false;

  // Return T if t is of type `Generator[T]`; otherwise just `type(t)`
  auto get_iterable_type = [&](Type::TypePtr t) {
    if (t->is("Generator")) {
      has_generator = true;
      return t->get_class()->generics[0].type;
    }
    return t;
  };

  // List of output types
  // (e.g., for `a|>b|>c` it is `[type(a), type(a|>b), type(a|>b|>c)]`).
  // Note: the generator types are completely preserved (i.e., not extracted)
  expr->in_types.clear();

  // Process the pipeline head
  auto in_type =
      transform(expr->items[0].expr)->type;  // input type to the next stage
  expr->in_types.push_back(in_type);
  in_type = get_iterable_type(in_type);
  auto done = expr->items[0].expr->is_done();
  for (size_t pi = 1; pi < expr->items.size(); pi++) {
    int in_type_pos = -1;  // ellipsis position
    ExprPtr* ec =
        &(expr->items[pi].expr);  // a pointer so that we can replace it
    while (
        auto se =
            (*ec)->get_stmt_expr())  // handle StmtExpr (e.g., in partial calls)
      ec = &(se->expr);

    if (auto call = (*ec)->get_call()) {
      // Case: a call. Find the position of the pipe ellipsis within it
      for (size_t ia = 0; in_type_pos == -1 && ia < call->args.size(); ia++)
        if (call->args[ia].value->get_ellipsis()) {
          in_type_pos = int(ia);
        }
      // No ellipses found? Prepend it as the first argument
      if (in_type_pos == -1) {
        call->args.insert(call->args.begin(),
                          {"", N<EllipsisExpr>(EllipsisExpr::PARTIAL)});
        in_type_pos = 0;
      }
    } else {
      // Case: not a call. Convert it to a call with a single ellipsis
      expr->items[pi].expr = N<CallExpr>(
          expr->items[pi].expr, N<EllipsisExpr>(EllipsisExpr::PARTIAL));
      ec = &expr->items[pi].expr;
      in_type_pos = 0;
    }

    // Set the ellipsis type
    auto el = (*ec)->get_call()->args[in_type_pos].value->get_ellipsis();
    el->mode = EllipsisExpr::PIPE;
    // Don't unify unbound inType yet (it might become a generator that needs to
    // be extracted)
    if (in_type && !in_type->get_unbound())
      unify(el->type, in_type);

    // Transform the call. Because a transformation might wrap the ellipsis in
    // layers, make sure to extract these layers and move them to the pipeline.
    // Example: `foo(...)` that is transformed to `foo(unwrap(...))` will become
    // `unwrap(...) |> foo(...)`
    transform(*ec);
    auto layers = find_ellipsis(*ec);
    seqassert(!layers.empty(), "can't find the ellipsis");
    if (layers.size() > 1) {
      // Prepend layers
      for (auto& [pos, prepend] : layers) {
        prepend->get_call()->args[pos].value =
            N<EllipsisExpr>(EllipsisExpr::PIPE);
        expr->items.insert(expr->items.begin() + pi++, {"|>", prepend});
      }
      // Rewind the loop (yes, the current expression will get transformed
      // again)
      /// TODO: avoid reevaluation
      expr->items.erase(expr->items.begin() + pi);
      pi = pi - layers.size() - 1;
      continue;
    }

    if ((*ec)->type)
      unify(expr->items[pi].expr->type, (*ec)->type);
    expr->items[pi].expr = *ec;
    in_type = expr->items[pi].expr->get_type();
    if (!realize(in_type))
      done = false;
    expr->in_types.push_back(in_type);

    // Do not extract the generator in the last stage of a pipeline
    if (pi + 1 < expr->items.size())
      in_type = get_iterable_type(in_type);
  }
  unify(expr->type, (has_generator ? ctx->get_type("NoneType") : in_type));
  if (done)
    expr->set_done();
}

/// Transform index expressions.
/// @example
///   `foo[T]`   -> Instantiate(foo, [T]) if `foo` is a type
///   `tup[1]`   -> `tup.item1` if `tup` is tuple
///   `foo[idx]` -> `foo.__getitem__(idx)`
///   expr.itemN or a sub-tuple if index is static (see
///   transform_static_tuple_index()),
void TypecheckVisitor::visit(IndexExpr* expr) {
  // Handle `Static[T]` constructs
  if (expr->expr->is_id("Static")) {
    auto typ = ctx->get_unbound();
    typ->is_static = get_static_generic(expr);
    unify(expr->type, typ);
    expr->set_done();
    return;
  }

  transform(expr->expr);
  seqassert(!expr->expr->is_type(), "index not converted to instantiate");
  auto cls = expr->expr->get_type()->get_class();
  if (!cls) {
    // Wait until the type becomes known
    unify(expr->type, ctx->get_unbound());
    return;
  }

  // Case: static tuple access
  auto [isTuple, tupleExpr] =
      transform_static_tuple_index(cls, expr->expr, expr->index);
  if (isTuple) {
    if (!tupleExpr) {
      unify(expr->type, ctx->get_unbound());
    } else {
      result_expr = tupleExpr;
    }
  } else {
    // Case: normal __getitem__
    result_expr = transform(
        N<CallExpr>(N<DotExpr>(expr->expr, "__getitem__"), expr->index));
  }
}

/// Transform an instantiation to canonical realized name.
/// @example
///   Instantiate(foo, [bar]) -> Id("foo[bar]")
void TypecheckVisitor::visit(InstantiateExpr* expr) {
  transform_type(expr->type_expr);

  std::shared_ptr<Type::StaticType> repeats = nullptr;
  if (expr->type_expr->is_id(TYPE_TUPLE) && !expr->type_params.empty()) {
    transform(expr->type_params[0]);
    if (expr->type_params[0]->static_value.type == StaticValue::INT) {
      repeats = Type::Type::make_static(ctx->cache, expr->type_params[0]);
    }
  }

  Type::TypePtr typ = nullptr;
  size_t type_params_size = expr->type_params.size() - (repeats != nullptr);
  if (expr->type_expr->is_id(TYPE_TUPLE)) {
    typ = ctx->instantiate_tuple(type_params_size);
  } else {
    typ = ctx->instantiate(expr->type_expr->get_source_info(),
                           expr->type_expr->get_type());
  }
  seqassert(typ->get_class(), "unknown type: {}", expr->type_expr);

  auto& generics = typ->get_class()->generics;
  bool is_union = typ->get_union() != nullptr;
  if (!is_union && type_params_size != generics.size())
    Err(Error::GENERICS_MISMATCH, expr, ctx->cache->rev(typ->get_class()->name),
      generics.size(), type_params_size);

  if (expr->type_expr->is_id(TYPE_CALLABLE)) {
    // Case: Callable[...] trait instantiation
    std::vector<Type::TypePtr> types;

    // Callable error checking.
    for (auto& typeParam : expr->type_params) {
      transform_type(typeParam);
      if (typeParam->type->is_static_type())
        Err(Error::INST_CALLABLE_STATIC, typeParam);
      types.push_back(typeParam->type);
    }
    auto typ = ctx->get_unbound();
    // Set up the Callable trait
    typ->get_link()->trait = std::make_shared<Type::CallableTrait>(ctx->cache, types);
    unify(expr->type, typ);
  } else if (expr->type_expr->is_id(TYPE_TYPEVAR)) {
    // Case: TypeVar[...] trait instantiation
    transform_type(expr->type_params[0]);
    auto typ = ctx->get_unbound();
    typ->get_link()->trait =
        std::make_shared<Type::TypeTrait>(expr->type_params[0]->type);
    unify(expr->type, typ);
  } else {
    for (size_t i = (repeats != nullptr); i < expr->type_params.size(); i++) {
      transform(expr->type_params[i]);
      Type::TypePtr t = nullptr;
      if (expr->type_params[i]->is_static()) {
        t = Type::Type::make_static(ctx->cache, expr->type_params[i]);
      } else {
        if (expr->type_params[i]->get_none())  // `None` -> `NoneType`
          transform_type(expr->type_params[i]);
        if (expr->type_params[i]->type->get_class() &&
            !expr->type_params[i]->is_type())
          Err(Error::EXPECTED_TYPE, expr->type_params[i], "type");
        t = ctx->instantiate(expr->type_params[i]->get_source_info(),
                             expr->type_params[i]->get_type());
      }
      if (is_union)
        typ->get_union()->add_type(t);
      else
        unify(t, generics[i - (repeats != nullptr)].type);
    }
    if (repeats) {
      typ->get_record()->repeats = repeats;
    }
    if (is_union) {
      typ->get_union()->seal();
    }
    unify(expr->type, typ);
  }
  expr->mark_type();

  // If the type is realizable, use the realized name instead of instantiation
  // (e.g. use Id("Ptr[byte]") instead of Instantiate(Ptr, {byte}))
  if (realize(expr->type)) {
    result_expr = N<IdExpr>(expr->type->realized_name());
    result_expr->set_type(expr->type);
    result_expr->set_done();
    if (expr->type_expr->is_type())
      result_expr->mark_type();
  }
}

/// Transform a slice expression.
/// @example
///   `start::step` -> `Slice(start, Optional.__new__(), step)`
void TypecheckVisitor::visit(SliceExpr* expr) {
  ExprPtr none = N<CallExpr>(N<DotExpr>(TYPE_OPTIONAL, "__new__"));
  result_expr = transform(N<CallExpr>(N<IdExpr>(TYPE_SLICE),
                                     expr->start ? expr->start : clone(none),
                                     expr->stop ? expr->stop : clone(none),
                                     expr->step ? expr->step : clone(none)));
}

/// Evaluate a static unary expression and return the resulting static
/// expression. If the expression cannot be evaluated yet, return nullptr.
/// Supported operators: (strings) not (ints) not, -, +
ExprPtr TypecheckVisitor::evaluate_static_unary(UnaryExpr* expr) {
  // Case: static strings
  if (expr->expr->static_value.type == StaticValue::STRING) {
    if (expr->op == "!") {
      if (expr->expr->static_value.evaluated) {
        bool value = expr->expr->static_value.get_string().empty();
        LOG_TYPECHECK("[cond::un] {}: {}", get_source_info(), value);
        return transform(N<BoolExpr>(value));
      } else {
        // Cannot be evaluated yet: just set the type
        unify(expr->type, ctx->get_type("bool"));
        if (!expr->is_static())
          expr->static_value.type = StaticValue::INT;
      }
    }
    return nullptr;
  }

  // Case: static integers
  if (expr->op == "-" || expr->op == "+" || expr->op == "!" ||
      expr->op == "~") {
    if (expr->expr->static_value.evaluated) {
      int64_t value = expr->expr->static_value.get_int();
      if (expr->op == "+")
        ;
      else if (expr->op == "-")
        value = -value;
      else if (expr->op == "~")
        value = ~value;
      else
        value = !bool(value);
      LOG_TYPECHECK("[cond::un] {}: {}", get_source_info(), value);
      if (expr->op == "!")
        return transform(N<BoolExpr>(bool(value)));
      else
        return transform(N<IntExpr>(value));
    } else {
      // Cannot be evaluated yet: just set the type
      unify(expr->type, ctx->get_type("int"));
      if (!expr->is_static())
        expr->static_value.type = StaticValue::INT;
    }
  }

  return nullptr;
}

/// Division and modulus implementations.
std::pair<int, int> divMod(const std::shared_ptr<TypeContext>& ctx, int a,
                           int b) {
  if (!b)
    Err(Error::STATIC_DIV_ZERO, ctx->get_source_info());
  if (ctx->cache->python_compat) {
    // Use Python implementation.
    int d = a / b;
    int m = a - d * b;
    if (m && ((b ^ m) < 0)) {
      m += b;
      d -= 1;
    }
    return {d, m};
  } else {
    // Use C implementation.
    return {a / b, a % b};
  }
}

/// Evaluate a static binary expression and return the resulting static
/// expression. If the expression cannot be evaluated yet, return nullptr.
/// Supported operators: (strings) +, ==, !=
///                      (ints) <, <=, >, >=, ==, !=, and, or, +, -, *, //, %,
///                      ^, |, &
ExprPtr TypecheckVisitor::evaluate_static_binary(BinaryExpr* expr) {
  // Case: static strings
  if (expr->rexpr->static_value.type == StaticValue::STRING) {
    if (expr->op == "+") {
      // `"a" + "b"` -> `"ab"`
      if (expr->lexpr->static_value.evaluated &&
          expr->rexpr->static_value.evaluated) {
        auto value = expr->lexpr->static_value.get_string() +
                     expr->rexpr->static_value.get_string();
        LOG_TYPECHECK("[cond::bin] {}: {}", get_source_info(), value);
        return transform(N<StringExpr>(value));
      } else {
        // Cannot be evaluated yet: just set the type
        if (!expr->is_static())
          expr->static_value.type = StaticValue::STRING;
        unify(expr->type, ctx->get_type("str"));
      }
    } else {
      // `"a" == "b"` -> `False` (also handles `!=`)
      if (expr->lexpr->static_value.evaluated &&
          expr->rexpr->static_value.evaluated) {
        bool eq = expr->lexpr->static_value.get_string() ==
                  expr->rexpr->static_value.get_string();
        bool value = expr->op == "==" ? eq : !eq;
        LOG_TYPECHECK("[cond::bin] {}: {}", get_source_info(), value);
        return transform(N<BoolExpr>(value));
      } else {
        // Cannot be evaluated yet: just set the type
        if (!expr->is_static())
          expr->static_value.type = StaticValue::INT;
        unify(expr->type, ctx->get_type("bool"));
      }
    }
    return nullptr;
  }

  // Case: static integers
  if (expr->lexpr->static_value.evaluated &&
      expr->rexpr->static_value.evaluated) {
    int64_t lvalue = expr->lexpr->static_value.get_int();
    int64_t rvalue = expr->rexpr->static_value.get_int();
    if (expr->op == "<")
      lvalue = lvalue < rvalue;
    else if (expr->op == "<=")
      lvalue = lvalue <= rvalue;
    else if (expr->op == ">")
      lvalue = lvalue > rvalue;
    else if (expr->op == ">=")
      lvalue = lvalue >= rvalue;
    else if (expr->op == "==")
      lvalue = lvalue == rvalue;
    else if (expr->op == "!=")
      lvalue = lvalue != rvalue;
    else if (expr->op == "&&")
      lvalue = lvalue && rvalue;
    else if (expr->op == "||")
      lvalue = lvalue || rvalue;
    else if (expr->op == "+")
      lvalue = lvalue + rvalue;
    else if (expr->op == "-")
      lvalue = lvalue - rvalue;
    else if (expr->op == "*")
      lvalue = lvalue * rvalue;
    else if (expr->op == "^")
      lvalue = lvalue ^ rvalue;
    else if (expr->op == "&")
      lvalue = lvalue & rvalue;
    else if (expr->op == "|")
      lvalue = lvalue | rvalue;
    else if (expr->op == ">>")
      lvalue = lvalue >> rvalue;
    else if (expr->op == "<<")
      lvalue = lvalue << rvalue;
    else if (expr->op == "//")
      lvalue = divMod(ctx, lvalue, rvalue).first;
    else if (expr->op == "%")
      lvalue = divMod(ctx, lvalue, rvalue).second;
    else
      seqassert(false, "unknown static operator {}", expr->op);
    LOG_TYPECHECK("[cond::bin] {}: {}", get_source_info(), lvalue);
    if (in(std::set<std::string>{"==", "!=", "<", "<=", ">", ">=", "&&", "||"},
           expr->op))
      return transform(N<BoolExpr>(bool(lvalue)));
    else
      return transform(N<IntExpr>(lvalue));
  } else {
    // Cannot be evaluated yet: just set the type
    if (!expr->is_static())
      expr->static_value.type = StaticValue::INT;
    unify(expr->type, ctx->get_type("int"));
  }

  return nullptr;
}

/// Transform a simple binary expression.
/// @example
///   `a and b`    -> `b if a else False`
///   `a or b`     -> `True if a else b`
///   `a in b`     -> `a.__contains__(b)`
///   `a not in b` -> `not (a in b)`
///   `a is not b` -> `not (a is b)`
ExprPtr TypecheckVisitor::transform_binary_simple(BinaryExpr* expr) {
  // Case: simple transformations
  if (expr->op == "&&") {
    return transform(N<IfExpr>(expr->lexpr,
                               N<CallExpr>(N<DotExpr>(expr->rexpr, "__bool__")),
                               N<BoolExpr>(false)));
  } else if (expr->op == "||") {
    return transform(
        N<IfExpr>(expr->lexpr, N<BoolExpr>(true),
                  N<CallExpr>(N<DotExpr>(expr->rexpr, "__bool__"))));
  } else if (expr->op == "not in") {
    return transform(N<CallExpr>(N<DotExpr>(
        N<CallExpr>(N<DotExpr>(expr->rexpr, "__contains__"), expr->lexpr),
        "__invert__")));
  } else if (expr->op == "in") {
    return transform(
        N<CallExpr>(N<DotExpr>(expr->rexpr, "__contains__"), expr->lexpr));
  } else if (expr->op == "is") {
    if (expr->lexpr->get_none() && expr->rexpr->get_none())
      return transform(N<BoolExpr>(true));
    else if (expr->lexpr->get_none())
      return transform(N<BinaryExpr>(expr->rexpr, "is", expr->lexpr));
  } else if (expr->op == "is not") {
    return transform(
        N<UnaryExpr>("!", N<BinaryExpr>(expr->lexpr, "is", expr->rexpr)));
  }
  return nullptr;
}

/// Transform a binary `is` expression by checking for type equality. Handle
/// special `is None` cаses as well. See inside for details.
ExprPtr TypecheckVisitor::transform_binary_is(BinaryExpr* expr) {
  seqassert(expr->op == "is", "not an is binary expression");

  // Case: `is None` expressions
  if (expr->rexpr->get_none()) {
    if (expr->lexpr->get_type()->is("NoneType"))
      return transform(N<BoolExpr>(true));
    if (!expr->lexpr->get_type()->is(TYPE_OPTIONAL)) {
      // lhs is not optional: `return False`
      return transform(N<BoolExpr>(false));
    } else {
      // Special case: Optional[Optional[... Optional[NoneType]]...] == NoneType
      auto g = expr->lexpr->get_type()->get_class();
      for (; g->generics[0].type->is("Optional");
           g = g->generics[0].type->get_class())
        ;
      if (g->generics[0].type->is("NoneType"))
        return transform(N<BoolExpr>(true));

      // lhs is optional: `return lhs.__has__().__invert__()`
      return transform(N<CallExpr>(N<DotExpr>(
          N<CallExpr>(N<DotExpr>(expr->lexpr, "__has__")), "__invert__")));
    }
  }

  // Check the type equality (operand types and __raw__ pointers must match).
  auto lc = realize(expr->lexpr->get_type());
  auto rc = realize(expr->rexpr->get_type());
  if (!lc || !rc) {
    // Types not known: return early
    unify(expr->type, ctx->get_type("bool"));
    return nullptr;
  }
  if (expr->lexpr->is_type() && expr->rexpr->is_type())
    return transform(N<BoolExpr>(lc->realized_name() == rc->realized_name()));
  if (!lc->get_record() && !rc->get_record()) {
    // Both reference types: `return lhs.__raw__() == rhs.__raw__()`
    return transform(
        N<BinaryExpr>(N<CallExpr>(N<DotExpr>(expr->lexpr, "__raw__")),
                      "==", N<CallExpr>(N<DotExpr>(expr->rexpr, "__raw__"))));
  }
  if (lc->get_class()->is(TYPE_OPTIONAL)) {
    // lhs is optional: `return lhs.__is_optional__(rhs)`
    return transform(
        N<CallExpr>(N<DotExpr>(expr->lexpr, "__is_optional__"), expr->rexpr));
  }
  if (rc->get_class()->is(TYPE_OPTIONAL)) {
    // rhs is optional: `return rhs.__is_optional__(lhs)`
    return transform(
        N<CallExpr>(N<DotExpr>(expr->rexpr, "__is_optional__"), expr->lexpr));
  }
  if (lc->realized_name() != rc->realized_name()) {
    // tuple names do not match: `return False`
    return transform(N<BoolExpr>(false));
  }
  // Same tuple types: `return lhs == rhs`
  return transform(N<BinaryExpr>(expr->lexpr, "==", expr->rexpr));
}

/// Return a binary magic opcode for the provided operator.
std::pair<std::string, std::string> TypecheckVisitor::get_magic(
    const std::string& op) {
  // Table of supported binary operations and the corresponding magic methods.
  static auto magics = std::unordered_map<std::string, std::string>{
      {"+", "add"},     {"-", "sub"},       {"*", "mul"},     {"**", "pow"},
      {"/", "truediv"}, {"//", "floordiv"}, {"@", "matmul"},  {"%", "mod"},
      {"<", "lt"},      {"<=", "le"},       {">", "gt"},      {">=", "ge"},
      {"==", "eq"},     {"!=", "ne"},       {"<<", "lshift"}, {">>", "rshift"},
      {"&", "and"},     {"|", "or"},        {"^", "xor"},
  };
  auto mi = magics.find(op);
  if (mi == magics.end())
    seqassert(false, "invalid binary operator '{}'", op);

  static auto rightMagics = std::unordered_map<std::string, std::string>{
      {"<", "gt"},  {"<=", "ge"}, {">", "lt"},
      {">=", "le"}, {"==", "eq"}, {"!=", "ne"},
  };
  auto rm = in(rightMagics, op);
  return {mi->second, rm ? *rm : "r" + mi->second};
}

/// Transform an in-place binary expression.
/// @example
///   `a op= b` -> `a.__iopmagic__(b)`
/// @param isAtomic if set, use atomic magics if available.
ExprPtr TypecheckVisitor::transform_binary_inplace_magic(BinaryExpr* expr,
                                                      bool isAtomic) {
  auto [magic, _] = get_magic(expr->op);
  auto lt = expr->lexpr->get_type()->get_class();
  auto rt = expr->rexpr->get_type()->get_class();
  seqassert(lt && rt, "lhs and rhs types not known");

  Type::FuncTypePtr method = nullptr;

  // Atomic operations: check if `lhs.__atomic_op__(Ptr[lhs], rhs)` exists
  if (isAtomic) {
    auto ptr = ctx->instantiate_generic(ctx->get_type("Ptr"), {lt});
    if ((method =
             find_best_method(lt, format("__atomic_{}__", magic), {ptr, rt}))) {
      expr->lexpr = N<CallExpr>(N<IdExpr>("__ptr__"), expr->lexpr);
    }
  }

  // In-place operations: check if `lhs.__iop__(lhs, rhs)` exists
  if (!method && expr->in_place) {
    method = find_best_method(lt, format("__i{}__", magic),
                            {expr->lexpr, expr->rexpr});
  }

  if (method)
    return transform(
        N<CallExpr>(N<IdExpr>(method->ast->name), expr->lexpr, expr->rexpr));
  return nullptr;
}

/// Transform a magic binary expression.
/// @example
///   `a op b` -> `a.__opmagic__(b)`
ExprPtr TypecheckVisitor::transform_binary_magic(BinaryExpr* expr) {
  auto [magic, rightMagic] = get_magic(expr->op);
  auto lt = expr->lexpr->get_type()->get_class();
  auto rt = expr->rexpr->get_type()->get_class();
  seqassert(lt && rt, "lhs and rhs types not known");

  if (!lt->is("pyobj") && rt->is("pyobj")) {
    // Special case: `obj op pyobj` -> `rhs.__rmagic__(lhs)` on lhs
    // Assumes that pyobj implements all left and right magics
    auto l = ctx->cache->get_temporary_var("l"),
         r = ctx->cache->get_temporary_var("r");
    return transform(N<StmtExpr>(
        N<AssignStmt>(N<IdExpr>(l), expr->lexpr),
        N<AssignStmt>(N<IdExpr>(r), expr->rexpr),
        N<CallExpr>(N<DotExpr>(N<IdExpr>(r), format("__{}__", rightMagic)),
                    N<IdExpr>(l))));
  }
  if (lt->get_union()) {
    // Special case: `union op obj` -> `union.__magic__(rhs)`
    return transform(N<CallExpr>(
        N<DotExpr>(expr->lexpr, format("__{}__", magic)), expr->rexpr));
  }

  // Normal operations: check if `lhs.__magic__(lhs, rhs)` exists
  if (auto method = find_best_method(lt, format("__{}__", magic),
                                   {expr->lexpr, expr->rexpr})) {
    // Normal case: `__magic__(lhs, rhs)`
    return transform(
        N<CallExpr>(N<IdExpr>(method->ast->name), expr->lexpr, expr->rexpr));
  }

  // Right-side magics: check if `rhs.__rmagic__(rhs, lhs)` exists
  if (auto method = find_best_method(rt, format("__{}__", rightMagic),
                                   {expr->rexpr, expr->lexpr})) {
    auto l = ctx->cache->get_temporary_var("l"),
         r = ctx->cache->get_temporary_var("r");
    return transform(N<StmtExpr>(
        N<AssignStmt>(N<IdExpr>(l), expr->lexpr),
        N<AssignStmt>(N<IdExpr>(r), expr->rexpr),
        N<CallExpr>(N<IdExpr>(method->ast->name), N<IdExpr>(r), N<IdExpr>(l))));
  }
  // 145

  return nullptr;
}

/// Given a tuple type and the expression `expr[index]`, check if an `index` is
/// static (integer or slice). If so, statically extract the specified tuple
/// item or a sub-tuple (if the index is a slice). Works only on normal tuples
/// and partial functions.
std::pair<bool, ExprPtr> TypecheckVisitor::transform_static_tuple_index(
    const Type::ClassTypePtr& tuple, const ExprPtr& expr, const ExprPtr& index) {
  if (!tuple->get_record())
    return {false, nullptr};
  if (tuple->name != TYPE_TUPLE && !startswith(tuple->name, TYPE_KWTUPLE) &&
      !startswith(tuple->name, TYPE_PARTIAL)) {
    if (tuple->is(TYPE_OPTIONAL)) {
      if (auto newTuple = tuple->generics[0].type->get_class()) {
        return transform_static_tuple_index(
            newTuple, transform(N<CallExpr>(N<IdExpr>(FN_UNWRAP), expr)),
            index);
      } else {
        return {true, nullptr};
      }
    }
    return {false, nullptr};
  }

  // Extract the static integer value from expression
  auto get_int = [&](int64_t* o, const ExprPtr& e) {
    if (!e)
      return true;
    auto f = transform(clone(e));
    if (f->static_value.type == StaticValue::INT) {
      seqassert(f->static_value.evaluated, "{} not evaluated", e);
      *o = f->static_value.get_int();
      return true;
    } else if (auto ei = f->get_int()) {
      *o = *(ei->int_value);
      return true;
    }
    return false;
  };

  auto classFields = get_class_fields(tuple.get());
  auto sz = int64_t(tuple->get_record()->args.size());
  int64_t start = 0, stop = sz, step = 1;
  if (get_int(&start, index)) {
    // Case: `tuple[int]`
    auto i = translate_index(start, stop);
    if (i < 0 || i >= stop)
      Err(Error::TUPLE_RANGE_BOUNDS, index, stop - 1, i);
    return {true, transform(N<DotExpr>(expr, classFields[i].name))};
  } else if (auto slice = CAST(index, SliceExpr)) {
    // Case: `tuple[int:int:int]`
    if (!get_int(&start, slice->start) || !get_int(&stop, slice->stop) ||
        !get_int(&step, slice->step))
      return {false, nullptr};

    // Adjust slice indices (Python slicing rules)
    if (slice->step && !slice->start)
      start = step > 0 ? 0 : (sz - 1);
    if (slice->step && !slice->stop)
      stop = step > 0 ? sz : -(sz + 1);
    slice_adjust_indices(sz, &start, &stop, step);

    // Generate a sub-tuple
    auto var = N<IdExpr>(ctx->cache->get_temporary_var("tup"));
    auto ass = N<AssignStmt>(var, expr);
    std::vector<ExprPtr> te;
    for (auto i = start; (step > 0) ? (i < stop) : (i > stop); i += step) {
      if (i < 0 || i >= sz)
        Err(Error::TUPLE_RANGE_BOUNDS, index, sz - 1, i);
      te.push_back(N<DotExpr>(clone(var), classFields[i].name));
    }
    ExprPtr e = transform(N<StmtExpr>(
        std::vector<StmtPtr>{ass},
        N<CallExpr>(N<DotExpr>(N<IdExpr>(TYPE_TUPLE), "__new__"), te)));
    return {true, e};
  }

  return {false, nullptr};
}

/// Follow Python indexing rules for static tuple indices.
/// Taken from
/// https://github.com/python/cpython/blob/main/Objects/sliceobject.c.
int64_t TypecheckVisitor::translate_index(int64_t idx, int64_t len, bool clamp) {
  if (idx < 0)
    idx += len;
  if (clamp) {
    if (idx < 0)
      idx = 0;
    if (idx > len)
      idx = len;
  } else if (idx < 0 || idx >= len) {
    Err(Error::TUPLE_RANGE_BOUNDS, get_source_info(), len - 1, idx);
  }
  return idx;
}

/// Follow Python slice indexing rules for static tuple indices.
/// Taken from
/// https://github.com/python/cpython/blob/main/Objects/sliceobject.c. Quote
/// (sliceobject.c:269): "this is harder to get right than you might think"
int64_t TypecheckVisitor::slice_adjust_indices(int64_t length, int64_t* start,
                                             int64_t* stop, int64_t step) {
  if (step == 0)
    Err(Error::SLICE_STEP_ZERO, get_source_info());

  if (*start < 0) {
    *start += length;
    if (*start < 0) {
      *start = (step < 0) ? -1 : 0;
    }
  } else if (*start >= length) {
    *start = (step < 0) ? length - 1 : length;
  }

  if (*stop < 0) {
    *stop += length;
    if (*stop < 0) {
      *stop = (step < 0) ? -1 : 0;
    }
  } else if (*stop >= length) {
    *stop = (step < 0) ? length - 1 : length;
  }

  if (step < 0) {
    if (*stop < *start) {
      return (*start - *stop - 1) / (-step) + 1;
    }
  } else {
    if (*start < *stop) {
      return (*stop - *start - 1) / step + 1;
    }
  }
  return 0;
}

}  // namespace Pud::AST