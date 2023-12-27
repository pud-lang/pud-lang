#include "Pud/TypeCheck/TypeCheck.h"

#include <memory>
#include <utility>
#include <vector>

#include "Pud/Common/Common.h"
#include "Pud/Simplify/Context.h"
#include "Pud/TypeCheck/Context.h"

namespace Pud::AST {

auto TypecheckVisitor::apply(Cache* cache, const StmtPtr& stmts) -> StmtPtr {
  if (!cache->type_ctx)
    cache->type_ctx = std::make_shared<TypeContext>(cache);
  TypecheckVisitor v(cache->type_ctx);
  auto s = v.infer_types(clone(stmts), true);
  if (!s) {
    v.error("cannot typecheck the program");
  }
  if (s->get_suite())
    v.prepare_vtables();
  return s;
}

/**************************************************************************************/

TypecheckVisitor::TypecheckVisitor(
    std::shared_ptr<TypeContext> ctx,
    const std::shared_ptr<std::vector<StmtPtr>>& stmts)
    : ctx(std::move(ctx)) {
  prepend_stmts = stmts ? stmts : std::make_shared<std::vector<StmtPtr>>();
}

/**************************************************************************************/

/// Transform an expression node.
auto TypecheckVisitor::transform(ExprPtr& expr) -> ExprPtr {
  if (!expr)
    return nullptr;

  auto typ = expr->type;
  if (!expr->done) {
    bool is_int_static = expr->static_value.type == StaticValue::INT;
    TypecheckVisitor v(ctx, prepend_stmts);
    v.set_source_info(expr->get_source_info());
    ctx->push_source_info(expr->get_source_info());
    expr->accept(v);
    ctx->pop_source_info();
    if (v.result_expr) {
      v.result_expr->attributes |= expr->attributes;
      v.result_expr->orig_expr = expr;
      expr = v.result_expr;
    }
    seqassert(expr->type, "type not set for {}", expr);
    if (!(is_int_static && expr->type->is("bool")))
      unify(typ, expr->type);
    if (expr->done) {
      ctx->changed_nodes++;
    }
  }
  realize(typ);
  LOG_TYPECHECK("[expr] {}: {}{}", get_source_info(), expr,
                expr->is_done() ? "[done]" : "");
  return expr;
}

/// Transform a type expression node.
/// Special case: replace `None` with `NoneType`
/// @throw @c ParserException if a node is not a type (use @c transform
/// instead).
auto TypecheckVisitor::transform_type(ExprPtr& expr) -> ExprPtr {
  if (expr && expr->get_none()) {
    expr = N<IdExpr>(expr->get_source_info(), "NoneType");
    expr->mark_type();
  }
  transform(expr);
  if (expr) {
    if (!expr->is_type() && expr->is_static()) {
      expr->set_type(Type::Type::make_static(ctx->cache, expr));
    } else if (!expr->is_type()) {
      Err(Error::EXPECTED_TYPE, expr, "type");
    } else {
      expr->set_type(ctx->instantiate(expr->get_type()));
    }
  }
  return expr;
}

void TypecheckVisitor::default_visit(Expr* e) {
  seqassert(false, "unexpected AST node {}", e->to_string());
}

/// Transform a statement node.
auto TypecheckVisitor::transform(StmtPtr& stmt) -> StmtPtr {
  if (!stmt || stmt->done)
    return stmt;

  TypecheckVisitor v(ctx);
  v.set_source_info(stmt->get_source_info());
  auto old_age = ctx->age;
  stmt->age = ctx->age = std::max(stmt->age, old_age);
  ctx->push_source_info(stmt->get_source_info());
  stmt->accept(v);
  ctx->pop_source_info();
  ctx->age = old_age;
  if (v.result_stmt)
    stmt = v.result_stmt;
  if (!v.prepend_stmts->empty()) {
    if (stmt)
      v.prepend_stmts->push_back(stmt);
    bool done = true;
    for (auto& s : *(v.prepend_stmts))
      done &= s->done;
    stmt = N<SuiteStmt>(*v.prepend_stmts);
    stmt->done = done;
  }
  if (stmt->done)
    ctx->changed_nodes++;
  return stmt;
}

void TypecheckVisitor::default_visit(Stmt* s) {
  seqassert(false, "unexpected AST node {}", s->to_string());
}

/**************************************************************************************/

/// Typecheck statement expressions.
void TypecheckVisitor::visit(StmtExpr* expr) {
  auto done = true;
  for (auto& s : expr->stmts) {
    transform(s);
    done &= s->is_done();
  }
  transform(expr->expr);
  unify(expr->type, expr->expr->type);
  if (done && expr->expr->is_done())
    expr->set_done();
}

/// Typecheck a list of statements.
void TypecheckVisitor::visit(SuiteStmt* stmt) {
  std::vector<StmtPtr> stmts;  // for filtering out nullptr statements
  auto done = true;
  for (auto& s : stmt->stmts) {
    if (ctx->return_early) {
      // If return_early is set (e.g., in the function) ignore the rest
      break;
    }
    if (transform(s)) {
      stmts.push_back(s);
      done &= stmts.back()->is_done();
    }
  }
  stmt->stmts = stmts;
  if (done)
    stmt->set_done();
}

/// Typecheck expression statements.
void TypecheckVisitor::visit(ExprStmt* stmt) {
  transform(stmt->expr);
  if (stmt->expr->is_done())
    stmt->set_done();
}

void TypecheckVisitor::visit(CommentStmt* stmt) { stmt->set_done(); }

/**************************************************************************************/

/// Select the best method indicated of an object that matches the given
/// argument types. See @c find_matching_methods for details.
auto TypecheckVisitor::find_best_method(const Type::ClassTypePtr& typ,
                                        const std::string& member,
                                        const std::vector<Type::TypePtr>& args)
    -> Type::FuncTypePtr {
  std::vector<CallExpr::Arg> call_args;
  for (auto& a : args) {
    call_args.push_back(
        {"", std::make_shared<NoneExpr>()});  // dummy expression
    call_args.back().value->set_type(a);
  }
  auto methods = ctx->find_method(typ.get(), member, false);
  auto m = find_matching_methods(typ, methods, call_args);
  return m.empty() ? nullptr : m[0];
}

/// Select the best method indicated of an object that matches the given
/// argument types. See @c find_matching_methods for details.
auto TypecheckVisitor::find_best_method(const Type::ClassTypePtr& typ,
                                        const std::string& member,
                                        const std::vector<ExprPtr>& args)
    -> Type::FuncTypePtr {
  std::vector<CallExpr::Arg> call_args;
  for (auto& a : args)
    call_args.push_back({"", a});
  auto methods = ctx->find_method(typ.get(), member, false);
  auto m = find_matching_methods(typ, methods, call_args);
  return m.empty() ? nullptr : m[0];
}

/// Select the best method indicated of an object that matches the given
/// argument types. See @c find_matching_methods for details.
auto TypecheckVisitor::find_best_method(
    const Type::ClassTypePtr& typ, const std::string& member,
    const std::vector<std::pair<std::string, Type::TypePtr>>& args)
    -> Type::FuncTypePtr {
  std::vector<CallExpr::Arg> call_args;
  for (auto& [n, a] : args) {
    call_args.push_back({n, std::make_shared<NoneExpr>()});  // dummy expression
    call_args.back().value->set_type(a);
  }
  auto methods = ctx->find_method(typ.get(), member, false);
  auto m = find_matching_methods(typ, methods, call_args);
  return m.empty() ? nullptr : m[0];
}

// Search expression tree for a identifier
class IdSearchVisitor : public CallbackASTVisitor<bool, bool> {
  std::string what;
  bool result;

 public:
  IdSearchVisitor(std::string what) : what(std::move(what)), result(false) {}
  auto transform(const std::shared_ptr<Expr>& expr) -> bool override {
    if (result)
      return result;
    IdSearchVisitor v(what);
    if (expr)
      expr->accept(v);
    return v.result;
  }
  auto transform(const std::shared_ptr<Stmt>& stmt) -> bool override {
    if (result)
      return result;
    IdSearchVisitor v(what);
    if (stmt)
      stmt->accept(v);
    return v.result;
  }
  void visit(IdExpr* expr) override {
    if (expr->value == what)
      result = true;
  }
};

/// Check if a function can be called with the given arguments.
/// See @c reorder_named_args for details.
auto TypecheckVisitor::can_call(const Type::FuncTypePtr& fn,
                                const std::vector<CallExpr::Arg>& args) -> int {
  std::vector<std::pair<Type::TypePtr, size_t>> reordered;
  auto ni_generics = fn->ast->get_non_inferrable_generics();
  auto score = ctx->reorder_named_args(
      fn.get(), args,
      [&](int s, int k, const std::vector<std::vector<int>>& slots, bool _) {
        for (int si = 0, gi = 0; si < slots.size(); si++) {
          if (fn->ast->args[si].status == Param::Generic) {
            if (slots[si].empty()) {
              // is this "real" type?
              if (in(ni_generics, fn->ast->args[si].name) &&
                  !fn->ast->args[si].default_value) {
                return -1;
              }
              reordered.push_back({nullptr, 0});
            } else {
              seqassert(gi < fn->func_generics.size(), "bad fn");
              if (!fn->func_generics[gi].type->is_static_type() &&
                  !args[slots[si][0]].value->is_type())
                return -1;
              reordered.push_back(
                  {args[slots[si][0]].value->type, slots[si][0]});
            }
            gi++;
          } else if (si == s || si == k || slots[si].size() != 1) {
            // Ignore *args, *kwargs and default arguments
            reordered.push_back({nullptr, 0});
          } else {
            reordered.push_back({args[slots[si][0]].value->type, slots[si][0]});
          }
        }
        return 0;
      },
      [](Error, const SourceInfo&, const std::string&) { return -1; });
  for (int ai = 0, mai = 0, gi = 0; score != -1 && ai < reordered.size();
       ai++) {
    auto expect_typ = fn->ast->args[ai].status == Param::Normal
                          ? fn->get_arg_types()[mai++]
                          : fn->func_generics[gi++].type;
    auto [argType, argTypeIdx] = reordered[ai];
    if (!argType)
      continue;
    if (fn->ast->args[ai].status != Param::Normal) {
      // Check if this is a good generic!
      if (expect_typ && expect_typ->is_static_type()) {
        if (!args[argTypeIdx].value->is_static()) {
          score = -1;
          break;
        } else {
          argType = Type::Type::make_static(ctx->cache, args[argTypeIdx].value);
        }
      } else {
        /// TODO: check if these are real types or if traits are satisfied
        continue;
      }
    }
    try {
      ExprPtr dummy = std::make_shared<IdExpr>("");
      dummy->type = argType;
      dummy->set_done();
      wrap_expr(dummy, expect_typ, fn);
      Type::Type::Unification undo;
      if (dummy->type->unify(expect_typ.get(), &undo) >= 0) {
        undo.undo();
      } else {
        score = -1;
      }
    } catch (const ParserException&) {
      // Ignore failed wraps
      score = -1;
    }
  }
  return score;
}

/// Select the best method among the provided methods given the list of
/// arguments. See @c reorder_named_args for details.
auto TypecheckVisitor::find_matching_methods(
    const Type::ClassTypePtr& typ,
    const std::vector<Type::FuncTypePtr>& methods,
    const std::vector<CallExpr::Arg>& args) -> std::vector<Type::FuncTypePtr> {
  // Pick the last method that accepts the given arguments.
  std::vector<Type::FuncTypePtr> results;
  for (const auto& mi : methods) {
    if (!mi)
      continue;  // avoid overloads that have not been seen yet
    auto method = ctx->instantiate(mi, typ)->get_func();
    int score = can_call(method, args);
    if (score != -1) {
      results.push_back(mi);
    }
  }
  return results;
}

/// Wrap an expression to coerce it to the expected type if the type of the
/// expression does not match it. Also unify types.
/// @example
///   expected `Generator`                -> `expr.__iter__()`
///   expected `float`, got `int`         -> `float(expr)`
///   expected `Optional[T]`, got `T`     -> `Optional(expr)`
///   expected `T`, got `Optional[T]`     -> `unwrap(expr)`
///   expected `Function`, got a function -> partialize function
///   expected `T`, got `Union[T...]`     -> `__internal__.get_union(expr, T)`
///   expected `Union[T...]`, got `T`     -> `__internal__.new_union(expr,
///   Union[T...])` expected base class, got derived    -> downcast to base
///   class
/// @param allowUnwrap allow optional unwrapping.
auto TypecheckVisitor::wrap_expr(ExprPtr& expr,
                                 const Type::TypePtr& expected_type,
                                 const Type::FuncTypePtr& callee,
                                 bool allow_unwrap) -> bool {
  auto expected_class = expected_type->get_class();
  auto expr_class = expr->get_type()->get_class();
  auto do_arg_wrap = !callee || !callee->ast->has_attr(
                                    "std.internal.attributes.no_argument_wrap");
  if (!do_arg_wrap)
    return true;
  auto doTypeWrap =
      !callee || !callee->ast->has_attr("std.internal.attributes.no_type_wrap");
  if (callee && expr->is_type()) {
    auto c = expr->type->get_class();
    if (!c)
      return false;
    if (doTypeWrap) {
      if (c->get_record())
        expr = transform(
            N<CallExpr>(expr, N<EllipsisExpr>(EllipsisExpr::PARTIAL)));
      else
        expr = transform(N<CallExpr>(
            N<IdExpr>("__internal__.class_ctr:0"),
            std::vector<CallExpr::Arg>{
                {"T", expr}, {"", N<EllipsisExpr>(EllipsisExpr::PARTIAL)}}));
    }
  }

  std::unordered_set<std::string> hints = {"Generator", "float", TYPE_OPTIONAL,
                                           "pyobj"};
  if (!expr_class && expected_class && in(hints, expected_class->name)) {
    return false;  // argument type not yet known.
  } else if (expected_class && expected_class->name == "Generator" &&
             expr_class->name != expected_class->name &&
             !expr->get_ellipsis()) {
    // Note: do not do this in pipelines (TODO: why?)
    expr = transform(N<CallExpr>(N<DotExpr>(expr, "__iter__")));
  } else if (expected_class && expected_class->name == "float" &&
             expr_class->name == "int") {
    expr = transform(N<CallExpr>(N<IdExpr>("float"), expr));
  } else if (expected_class && expected_class->name == TYPE_OPTIONAL &&
             expr_class->name != expected_class->name) {
    expr = transform(N<CallExpr>(N<IdExpr>(TYPE_OPTIONAL), expr));
  } else if (allow_unwrap && expected_class && expr_class &&
             expr_class->name == TYPE_OPTIONAL &&
             expr_class->name != expected_class->name) {  // unwrap optional
    expr = transform(N<CallExpr>(N<IdExpr>(FN_UNWRAP), expr));
  } else if (expected_class && expected_class->name == "pyobj" &&
             expr_class->name != expected_class->name) {  // wrap to pyobj
    expr = transform(N<CallExpr>(N<IdExpr>("pyobj"),
                                 N<CallExpr>(N<DotExpr>(expr, "__to_py__"))));
  } else if (allow_unwrap && expected_class && expr_class &&
             expr_class->name == "pyobj" &&
             expr_class->name != expected_class->name) {  // unwrap pyobj
    auto texpr = N<IdExpr>(expected_class->name);
    texpr->set_type(expected_type);
    expr = transform(
        N<CallExpr>(N<DotExpr>(texpr, "__from_py__"), N<DotExpr>(expr, "p")));
  } else if (callee && expr_class && expr->type->get_func() &&
             !(expected_class && expected_class->name == "Function")) {
    // Wrap raw Seq functions into Partial(...) call for easy realization.
    expr = partialize_function(expr->type->get_func());
  } else if (allow_unwrap && expr_class && expr->type->get_union() &&
             expected_class && !expected_class->get_union()) {
    // Extract union types via __internal__.get_union
    if (auto t = realize(expected_class)) {
      expr = transform(N<CallExpr>(N<IdExpr>("__internal__.get_union:0"), expr,
                                   N<IdExpr>(t->realized_name())));
    } else {
      return false;
    }
  } else if (expr_class && expected_class && expected_class->get_union()) {
    // Make union types via __internal__.new_union
    if (!expected_class->get_union()->is_sealed())
      expected_class->get_union()->add_type(expr_class);
    if (auto t = realize(expected_class)) {
      if (expected_class->unify(expr_class.get(), nullptr) == -1)
        expr = transform(N<CallExpr>(N<IdExpr>("__internal__.new_union:0"),
                                     expr, NT<IdExpr>(t->realized_name())));
    } else {
      return false;
    }
  } else if (expr_class && expected_class &&
             expr_class->name != expected_class->name) {
    // Cast derived classes to base classes
    auto& mros = ctx->cache->classes[expr_class->name].mro;
    for (size_t i = 1; i < mros.size(); i++) {
      auto t = ctx->instantiate(mros[i]->type, expr_class);
      if (t->unify(expected_class.get(), nullptr) >= 0) {
        if (!expr->is_id("")) {
          expr = cast_to_super_class(expr, expected_class, true);
        } else {  // Just checking can this be done
          expr->type = expected_class;
        }
        break;
      }
    }
  }
  return true;
}

/// Cast derived class to a base class.
auto TypecheckVisitor::cast_to_super_class(ExprPtr expr,
                                           Type::ClassTypePtr super_typ,
                                           bool is_virtual) -> ExprPtr {
  Type::ClassTypePtr typ = expr->type->get_class();
  for (auto& field : get_class_fields(typ.get())) {
    for (auto& parent_field : get_class_fields(super_typ.get()))
      if (field.name == parent_field.name) {
        unify(ctx->instantiate(field.type, typ),
              ctx->instantiate(parent_field.type, super_typ));
      }
  }
  realize(super_typ);
  auto typ_expr = N<IdExpr>(super_typ->name);
  typ_expr->set_type(super_typ);
  return transform(N<CallExpr>(
      N<DotExpr>(N<IdExpr>("__internal__"), "class_super"), expr, typ_expr));
}

/// Unpack a Tuple or KwTuple expression into (name, type) vector.
/// Name is empty when handling Tuple; otherwise it matches names of KwTuple.
auto TypecheckVisitor::unpack_tuple_types(ExprPtr expr)
    -> std::shared_ptr<std::vector<std::pair<std::string, Type::TypePtr>>> {
  auto ret =
      std::make_shared<std::vector<std::pair<std::string, Type::TypePtr>>>();
  if (auto tup = expr->orig_expr->get_tuple()) {
    for (auto& a : tup->items) {
      transform(a);
      if (!a->get_type()->get_class())
        return nullptr;
      ret->push_back({"", a->get_type()});
    }
  } else if (auto kw = expr->orig_expr->get_call()) {  // orig_expr?
    auto kw_cls = in(ctx->cache->classes, expr->get_type()->get_class()->name);
    seqassert(kw_cls, "cannot find {}", expr->get_type()->get_class()->name);
    for (size_t i = 0; i < kw->args.size(); i++) {
      auto& a = kw->args[i].value;
      transform(a);
      if (!a->get_type()->get_class())
        return nullptr;
      ret->push_back({kw_cls->fields[i].name, a->get_type()});
    }
  } else {
    return nullptr;
  }
  return ret;
}

auto TypecheckVisitor::get_class_fields(Type::ClassType* t)
    -> std::vector<Cache::Class::ClassField>& {
  seqassert(t && in(ctx->cache->classes, t->name), "cannot find '{}'",
            t ? t->name : "<null>");
  if (t->is(TYPE_TUPLE) && !t->get_record()->args.empty()) {
    auto key = ctx->generate_tuple(t->get_record()->args.size());
    return ctx->cache->classes[key].fields;
  } else {
    return ctx->cache->classes[t->name].fields;
  }
}

}  // namespace Pud::AST