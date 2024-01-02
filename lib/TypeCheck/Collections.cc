#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

namespace Pud::AST {

/// Transform a list `[a1, ..., aN]` to the corresponding statement expression.
/// See @c transform_comprehension
void TypecheckVisitor::visit(ListExpr* expr) {
  expr->set_type(ctx->get_unbound());
  if ((result_expr = transform_comprehension("std.internal.types.ptr.List",
                                           "append", expr->items))) {
    result_expr->set_attr(ExprAttr::List);
  }
}

/// Transform a set `{a1, ..., aN}` to the corresponding statement expression.
/// See @c transform_comprehension
void TypecheckVisitor::visit(SetExpr* expr) {
  expr->set_type(ctx->get_unbound());
  auto name = ctx->cache->imports[STDLIB_IMPORT].ctx->force_find("Set");
  if ((result_expr =
           transform_comprehension(name->canonical_name, "add", expr->items))) {
    result_expr->set_attr(ExprAttr::Set);
  }
}

/// Transform a dictionary `{k1: v1, ..., kN: vN}` to a corresponding statement
/// expression. See @c transform_comprehension
void TypecheckVisitor::visit(DictExpr* expr) {
  expr->set_type(ctx->get_unbound());
  auto name = ctx->cache->imports[STDLIB_IMPORT].ctx->force_find("Dict");
  if ((result_expr = transform_comprehension(name->canonical_name, "__setitem__",
                                           expr->items))) {
    result_expr->set_attr(ExprAttr::Dict);
  }
}

/// Transform a collection of type `type` to a statement expression:
///   `[a1, ..., aN]` -> `cont = [type](); (cont.[fn](a1); ...); cont`
/// Any star-expression within the collection will be expanded:
///   `[a, *b]` -> `cont.[fn](a); for i in b: cont.[fn](i)`.
/// @example
///   `[a, *b, c]`  -> ```cont = List(3)
///                       cont.append(a)
///                       for i in b: cont.append(i)
///                       cont.append(c)```
///   `{a, *b, c}`  -> ```cont = Set()
///                       cont.add(a)
///                       for i in b: cont.add(i)
///                       cont.add(c)```
///   `{a: 1, **d}` -> ```cont = Dict()
///                       cont.__setitem__((a, 1))
///                       for i in b.items(): cont.__setitem__((i[0], i[i]))```
ExprPtr TypecheckVisitor::transform_comprehension(const std::string& type,
                                                 const std::string& fn,
                                                 std::vector<ExprPtr>& items) {
  // Deduce the super type of the collection--- in other words, the least common
  // ancestor of all types in the collection. For example, `type([1, 1.2]) ==
  // type([1.2, 1]) == float` because float is an "ancestor" of int.
  auto superTyp = [&](const Type::ClassTypePtr& collectionCls,
                      const Type::ClassTypePtr& ti) -> Type::ClassTypePtr {
    if (!collectionCls)
      return ti;
    if (collectionCls->is("int") && ti->is("float")) {
      // Rule: int derives from float
      return ti;
    } else if (collectionCls->name != TYPE_OPTIONAL &&
               ti->name == TYPE_OPTIONAL) {
      // Rule: T derives from Optional[T]
      return ctx->instantiate_generic(ctx->get_type("Optional"), {collectionCls})
          ->get_class();
    } else if (!collectionCls->is("pyobj") && ti->is("pyobj")) {
      // Rule: anything derives from pyobj
      return ti;
    } else if (collectionCls->name != ti->name) {
      // Rule: subclass derives from superclass
      auto& mros = ctx->cache->classes[collectionCls->name].mro;
      for (size_t i = 1; i < mros.size(); i++) {
        auto t = ctx->instantiate(mros[i]->type, collectionCls);
        if (t->unify(ti.get(), nullptr) >= 0) {
          return ti;
          break;
        }
      }
    }
    return nullptr;
  };

  Type::TypePtr collectionTyp = ctx->get_unbound();
  bool done = true;
  bool isDict = endswith(type, "Dict");
  for (auto& i : items) {
    Type::ClassTypePtr typ = nullptr;
    if (!isDict && i->get_star()) {
      auto star = i->get_star();
      star->what = transform(N<CallExpr>(N<DotExpr>(star->what, "__iter__")));
      if (star->what->type->is("Generator"))
        typ = star->what->type->get_class()->generics[0].type->get_class();
    } else if (isDict && CAST(i, KeywordStarExpr)) {
      auto star = CAST(i, KeywordStarExpr);
      star->what = transform(N<CallExpr>(N<DotExpr>(star->what, "items")));
      if (star->what->type->is("Generator"))
        typ = star->what->type->get_class()->generics[0].type->get_class();
    } else {
      i = transform(i);
      typ = i->type->get_class();
    }
    if (!typ) {
      done = false;
      continue;
    }
    if (!collectionTyp->get_class()) {
      unify(collectionTyp, typ);
    } else if (!isDict) {
      if (auto t = superTyp(collectionTyp->get_class(), typ))
        collectionTyp = t;
    } else {
      seqassert(collectionTyp->get_record() &&
                    collectionTyp->get_record()->args.size() == 2,
                "bad dict");
      auto tt = unify(typ, ctx->instantiate_tuple(2))->get_record();
      auto nt = collectionTyp->get_record()->args;
      for (int di = 0; di < 2; di++) {
        if (!nt[di]->get_class())
          unify(nt[di], tt->args[di]);
        else if (auto dt =
                     superTyp(nt[di]->get_class(), tt->args[di]->get_class()))
          nt[di] = dt;
      }
      collectionTyp = ctx->instantiate_tuple(nt);
    }
  }
  if (!done)
    return nullptr;
  std::vector<StmtPtr> stmts;
  ExprPtr var = N<IdExpr>(ctx->cache->get_temporary_var("cont"));

  std::vector<ExprPtr> constructorArgs{};
  if (endswith(type, "List") && !items.empty()) {
    // Optimization: pre-allocate the list with the exact number of elements
    constructorArgs.push_back(N<IntExpr>(items.size()));
  }
  auto t = NT<IdExpr>(type);
  if (isDict && collectionTyp->get_record()) {
    t->set_type(ctx->instantiate_generic(ctx->get_type(type),
                                       collectionTyp->get_record()->args));
  } else if (isDict) {
    t->set_type(ctx->instantiate(ctx->get_type(type)));
  } else {
    t->set_type(ctx->instantiate_generic(ctx->get_type(type), {collectionTyp}));
  }
  stmts.push_back(
      transform(N<AssignStmt>(clone(var), N<CallExpr>(t, constructorArgs))));
  for (const auto& it : items) {
    if (!isDict && it->get_star()) {
      // Unpack star-expression by iterating over it
      // `*star` -> `for i in star: cont.[fn](i)`
      auto star = it->get_star();
      ExprPtr forVar = N<IdExpr>(ctx->cache->get_temporary_var("i"));
      star->what->set_attr(ExprAttr::StarSequenceItem);
      stmts.push_back(transform(
          N<ForStmt>(clone(forVar), star->what,
                     N<ExprStmt>(N<CallExpr>(N<DotExpr>(clone(var), fn),
                                             clone(forVar))))));
    } else if (isDict && CAST(it, KeywordStarExpr)) {
      // Expand kwstar-expression by iterating over it: see the example above
      auto star = CAST(it, KeywordStarExpr);
      ExprPtr forVar = N<IdExpr>(ctx->cache->get_temporary_var("it"));
      star->what->set_attr(ExprAttr::StarSequenceItem);
      stmts.push_back(transform(
          N<ForStmt>(clone(forVar), star->what,
                     N<ExprStmt>(N<CallExpr>(
                         N<DotExpr>(clone(var), fn),
                         N<IndexExpr>(clone(forVar), N<IntExpr>(0)),
                         N<IndexExpr>(clone(forVar), N<IntExpr>(1)))))));
    } else {
      it->set_attr(ExprAttr::SequenceItem);
      if (isDict) {
        stmts.push_back(transform(N<ExprStmt>(N<CallExpr>(
            N<DotExpr>(clone(var), fn), N<IndexExpr>(it, N<IntExpr>(0)),
            N<IndexExpr>(it, N<IntExpr>(1))))));
      } else {
        stmts.push_back(transform(
            N<ExprStmt>(N<CallExpr>(N<DotExpr>(clone(var), fn), it))));
      }
    }
  }
  return transform(N<StmtExpr>(stmts, var));
}

/// Transform tuples.
/// Generate tuple classes (e.g., `Tuple`) if not available.
/// @example
///   `(a1, ..., aN)` -> `Tuple.__new__(a1, ..., aN)`
void TypecheckVisitor::visit(TupleExpr* expr) {
  expr->set_type(ctx->get_unbound());
  for (int ai = 0; ai < expr->items.size(); ai++)
    if (auto star = expr->items[ai]->get_star()) {
      // Case: unpack star expressions (e.g., `*arg` -> `arg.item1, arg.item2,
      // ...`)
      transform(star->what);
      auto typ = star->what->type->get_class();
      while (typ && typ->is(TYPE_OPTIONAL)) {
        star->what = transform(N<CallExpr>(N<IdExpr>(FN_UNWRAP), star->what));
        typ = star->what->type->get_class();
      }
      if (!typ)
        return;  // continue later when the type becomes known
      if (!typ->get_record())
        Err(Error::CALL_BAD_UNPACK, star, typ->pretty_string());
      auto ff = get_class_fields(typ.get());
      for (int i = 0; i < typ->get_record()->args.size(); i++, ai++) {
        expr->items.insert(
            expr->items.begin() + ai,
            transform(N<DotExpr>(clone(star->what), ff[i].name)));
      }
      // Remove the star
      expr->items.erase(expr->items.begin() + ai);
      ai--;
    } else {
      expr->items[ai] = transform(expr->items[ai]);
    }
  auto s = ctx->generate_tuple(expr->items.size());
  result_expr = transform(N<CallExpr>(N<IdExpr>(s), clone(expr->items)));
  unify(expr->type, result_expr->type);
}

/// Transform a tuple generator expression.
/// @example
///   `tuple(expr for i in tuple_generator)` -> `Tuple.__new__(expr...)`
void TypecheckVisitor::visit(GeneratorExpr* expr) {
  seqassert(expr->kind == GeneratorExpr::Generator && expr->loops.size() == 1 &&
                expr->loops[0].conds.empty(),
            "invalid tuple generator");

  unify(expr->type, ctx->get_unbound());

  auto gen = transform(expr->loops[0].gen);
  if (!gen->type->can_realize())
    return;  // Wait until the iterator can be realized

  auto block = N<SuiteStmt>();
  // `tuple = tuple_generator`
  auto tupleVar = ctx->cache->get_temporary_var("tuple");
  block->stmts.push_back(N<AssignStmt>(N<IdExpr>(tupleVar), gen));

  seqassert(expr->loops[0].vars->get_id(), "tuple() not simplified");
  std::vector<std::string> vars{expr->loops[0].vars->get_id()->value};
  auto suiteVec = expr->expr->get_stmt_expr()
                      ? expr->expr->get_stmt_expr()->stmts[0]->get_suite()
                      : nullptr;
  auto oldSuite = suiteVec ? suiteVec->clone() : nullptr;
  for (int validI = 0; suiteVec && validI < suiteVec->stmts.size(); validI++) {
    if (auto a = suiteVec->stmts[validI]->get_assign())
      if (a->rhs && a->rhs->get_index())
        if (a->rhs->get_index()->expr->is_id(vars[0])) {
          vars.push_back(a->lhs->get_id()->value);
          suiteVec->stmts[validI] = nullptr;
          continue;
        }
    break;
  }
  if (vars.size() > 1)
    vars.erase(vars.begin());
  auto [ok, staticItems] = transform_static_loop_call(
      vars, expr->loops[0].gen,
      [&](StmtPtr wrap) { return N<StmtExpr>(wrap, clone(expr->expr)); });
  if (ok) {
    std::vector<ExprPtr> tupleItems;
    for (auto& i : staticItems)
      tupleItems.push_back(std::dynamic_pointer_cast<Expr>(i));
    result_expr = transform(N<StmtExpr>(block, N<TupleExpr>(tupleItems)));
    return;
  } else if (oldSuite) {
    expr->expr->get_stmt_expr()->stmts[0] = oldSuite;
  }

  auto tuple = gen->type->get_record();
  if (!tuple ||
      !(tuple->name == TYPE_TUPLE || startswith(tuple->name, TYPE_KWTUPLE)))
    Err(Error::CALL_BAD_ITER, gen, gen->type->pretty_string());

  // `a := tuple[i]; expr...` for each i
  std::vector<ExprPtr> items;
  items.reserve(tuple->args.size());
  for (int ai = 0; ai < tuple->args.size(); ai++) {
    items.emplace_back(N<StmtExpr>(
        N<AssignStmt>(clone(expr->loops[0].vars),
                      N<IndexExpr>(N<IdExpr>(tupleVar), N<IntExpr>(ai))),
        clone(expr->expr)));
  }

  // `((a := tuple[0]; expr), (a := tuple[1]; expr), ...)`
  result_expr = transform(N<StmtExpr>(block, N<TupleExpr>(items)));
}

}  // namespace Pud::AST