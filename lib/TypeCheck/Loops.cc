#include <string>
#include <tuple>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

namespace Pud::AST {

/// Nothing to typecheck; just call set_done
void TypecheckVisitor::visit(BreakStmt* stmt) {
  stmt->set_done();
  if (!ctx->static_loops.back().empty()) {
    auto a =
        N<AssignStmt>(N<IdExpr>(ctx->static_loops.back()), N<BoolExpr>(false));
    a->set_update();
    result_stmt = transform(N<SuiteStmt>(a, stmt->clone()));
  }
}

/// Nothing to typecheck; just call set_done
void TypecheckVisitor::visit(ContinueStmt* stmt) {
  stmt->set_done();
  if (!ctx->static_loops.back().empty()) {
    result_stmt = N<BreakStmt>();
    result_stmt->set_done();
  }
}

/// Typecheck while statements.
void TypecheckVisitor::visit(WhileStmt* stmt) {
  ctx->static_loops.push_back(stmt->goto_var.empty() ? "" : stmt->goto_var);
  transform(stmt->cond);
  ctx->block_level++;
  transform(stmt->suite);
  ctx->block_level--;
  ctx->static_loops.pop_back();

  if (stmt->cond->is_done() && stmt->suite->is_done())
    stmt->set_done();
}

/// Typecheck for statements. Wrap the iterator expression with `__iter__` if
/// needed. See @c transformHeterogenousTupleFor for iterating heterogenous
/// tuples.
void TypecheckVisitor::visit(ForStmt* stmt) {
  transform(stmt->decorator);
  transform(stmt->iter);

  // Extract the iterator type of the for
  auto iterType = stmt->iter->get_type()->get_class();
  if (!iterType)
    return;  // wait until the iterator is known

  if ((result_stmt = transform_static_for_loop(stmt)))
    return;

  bool maybeHeterogenous =
      iterType->name == TYPE_TUPLE || startswith(iterType->name, TYPE_KWTUPLE);
  if (maybeHeterogenous && !iterType->can_realize()) {
    return;  // wait until the tuple is fully realizable
  } else if (maybeHeterogenous && iterType->get_heterogenous_tuple()) {
    // Case: iterating a heterogenous tuple
    result_stmt = transform_heterogenous_tuple_for(stmt);
    return;
  }

  // Case: iterating a non-generator. Wrap with `__iter__`
  if (iterType->name != "Generator" && !stmt->wrapped) {
    stmt->iter = transform(N<CallExpr>(N<DotExpr>(stmt->iter, "__iter__")));
    iterType = stmt->iter->get_type()->get_class();
    stmt->wrapped = true;
  }

  auto var = stmt->var->get_id();
  seqassert(var, "corrupt for variable: {}", stmt->var);

  // Handle dominated for bindings
  auto changed = in(ctx->cache->replacements, var->value);
  while (auto s = in(ctx->cache->replacements, var->value))
    var->value = s->first, changed = s;
  if (changed && changed->second) {
    auto u = N<AssignStmt>(N<IdExpr>(format("{}.__used__", var->value)),
                           N<BoolExpr>(true));
    u->set_update();
    stmt->suite = N<SuiteStmt>(u, stmt->suite);
  }
  if (changed)
    var->set_attr(ExprAttr::Dominated);

  // Unify iterator variable and the iterator type
  auto val = ctx->find(var->value);
  if (!changed)
    val = ctx->add(TypecheckItem::Var, var->value,
                   ctx->get_unbound(stmt->var->get_source_info()));
  if (iterType && iterType->name != "Generator")
    Err(Error::EXPECTED_GENERATOR, stmt->iter);
  unify(stmt->var->type,
        iterType ? unify(val->type, iterType->generics[0].type) : val->type);

  ctx->static_loops.emplace_back();
  ctx->block_level++;
  transform(stmt->suite);
  ctx->block_level--;
  ctx->static_loops.pop_back();

  if (stmt->iter->is_done() && stmt->suite->is_done())
    stmt->set_done();
}

/// Handle heterogeneous tuple iteration.
/// @example
///   `for i in tuple_expr: <suite>` ->
///   ```tuple = tuple_expr
///      for cnt in range(<tuple length>):
///        if cnt == 0:
///          i = t[0]; <suite>
///        if cnt == 1:
///          i = t[1]; <suite> ...```
/// A separate suite is generated  for each tuple member.
StmtPtr TypecheckVisitor::transform_heterogenous_tuple_for(ForStmt* stmt) {
  auto block = N<SuiteStmt>();
  // `tuple = <tuple expression>`
  auto tupleVar = ctx->cache->get_temporary_var("tuple");
  block->stmts.push_back(N<AssignStmt>(N<IdExpr>(tupleVar), stmt->iter));

  auto tupleArgs =
      stmt->iter->get_type()->get_class()->get_heterogenous_tuple()->args;
  auto cntVar = ctx->cache->get_temporary_var("idx");
  std::vector<StmtPtr> forBlock;
  for (size_t ai = 0; ai < tupleArgs.size(); ai++) {
    // `if cnt == ai: (var = tuple[ai]; <suite>)`
    forBlock.push_back(N<IfStmt>(
        N<BinaryExpr>(N<IdExpr>(cntVar), "==", N<IntExpr>(ai)),
        N<SuiteStmt>(
            N<AssignStmt>(clone(stmt->var),
                          N<IndexExpr>(N<IdExpr>(tupleVar), N<IntExpr>(ai))),
            clone(stmt->suite))));
  }
  // `for cnt in range(tuple_size): ...`
  block->stmts.push_back(
      N<ForStmt>(N<IdExpr>(cntVar),
                 N<CallExpr>(N<IdExpr>("std.internal.types.range.range"),
                             N<IntExpr>(tupleArgs.size())),
                 N<SuiteStmt>(forBlock)));

  ctx->block_level++;
  transform(block);
  ctx->block_level--;

  return block;
}

/// Handle static for constructs.
/// @example
///   `for i in statictuple(1, x): <suite>` ->
///   ```loop = True
///      while loop:
///        while loop:
///          i: Static[int] = 1; <suite>; break
///        while loop:
///          i = x; <suite>; break
///        loop = False   # also set to False on break
/// A separate suite is generated for each static iteration.
StmtPtr TypecheckVisitor::transform_static_for_loop(ForStmt* stmt) {
  auto var = stmt->var->get_id()->value;
  if (!stmt->iter->get_call() || !stmt->iter->get_call()->expr->get_id())
    return nullptr;
  auto iter = stmt->iter->get_call()->expr->get_id();
  auto loopVar = ctx->cache->get_temporary_var("loop");

  std::vector<std::string> vars{var};
  auto suiteVec = stmt->suite->get_suite();
  auto oldSuite = suiteVec ? suiteVec->clone() : nullptr;
  for (int validI = 0; suiteVec && validI < suiteVec->stmts.size(); validI++) {
    if (auto a = suiteVec->stmts[validI]->get_assign())
      if (a->rhs && a->rhs->get_index())
        if (a->rhs->get_index()->expr->is_id(var)) {
          vars.push_back(a->lhs->get_id()->value);
          suiteVec->stmts[validI] = nullptr;
          continue;
        }
    break;
  }
  if (vars.size() > 1)
    vars.erase(vars.begin());
  auto [ok, items] =
      transform_static_loop_call(vars, stmt->iter, [&](StmtPtr assigns) {
        auto brk = N<BreakStmt>();
        brk->set_done();  // Avoid transforming this one to continue
        // var [: Static] := expr; suite...
        auto loop = N<WhileStmt>(
            N<IdExpr>(loopVar), N<SuiteStmt>(assigns, clone(stmt->suite), brk));
        loop->goto_var = loopVar;
        return loop;
      });
  if (!ok) {
    if (oldSuite)
      stmt->suite = oldSuite;
    return nullptr;
  }

  // Close the loop
  ctx->block_level++;
  auto a = N<AssignStmt>(N<IdExpr>(loopVar), N<BoolExpr>(false));
  a->set_update();
  auto block = N<SuiteStmt>();
  for (auto& i : items)
    block->stmts.push_back(std::dynamic_pointer_cast<Stmt>(i));
  block->stmts.push_back(a);
  auto loop = transform(
      N<SuiteStmt>(N<AssignStmt>(N<IdExpr>(loopVar), N<BoolExpr>(true)),
                   N<WhileStmt>(N<IdExpr>(loopVar), block)));
  ctx->block_level--;
  return loop;
}

std::pair<bool, std::vector<std::shared_ptr<Pud::SourceObject>>>
TypecheckVisitor::transform_static_loop_call(
    const std::vector<std::string>& vars, ExprPtr iter,
    std::function<std::shared_ptr<Pud::SourceObject>(StmtPtr)> wrap) {
  if (!iter->get_call())
    return {false, {}};
  auto fn = iter->get_call()->expr->get_id();
  if (!fn || vars.empty())
    return {false, {}};

  auto stmt = N<AssignStmt>(N<IdExpr>(vars[0]), nullptr, nullptr);
  std::vector<std::shared_ptr<Pud::SourceObject>> block;
  if (startswith(fn->value, "statictuple:0")) {
    auto& args = iter->get_call()->args[0].value->get_call()->args;
    if (vars.size() != 1)
      error("expected one item");
    for (size_t i = 0; i < args.size(); i++) {
      stmt->rhs = args[i].value;
      if (stmt->rhs->is_static()) {
        stmt->type = NT<IndexExpr>(
            N<IdExpr>("Static"),
            N<IdExpr>(stmt->rhs->static_value.type == StaticValue::INT ? "int"
                                                                      : "str"));
      } else {
        stmt->type = nullptr;
      }
      block.push_back(wrap(stmt->clone()));
    }
  } else if (fn &&
             startswith(fn->value, "std.internal.types.range.staticrange:0")) {
    if (vars.size() != 1)
      error("expected one item");
    int st = fn->type->get_func()
                 ->func_generics[0]
                 .type->get_static()
                 ->evaluate()
                 .get_int();
    int ed = fn->type->get_func()
                 ->func_generics[1]
                 .type->get_static()
                 ->evaluate()
                 .get_int();
    int step = fn->type->get_func()
                   ->func_generics[2]
                   .type->get_static()
                   ->evaluate()
                   .get_int();
    if (abs(st - ed) / abs(step) > MAX_STATIC_ITER)
      Err(Error::STATIC_RANGE_BOUNDS, fn, MAX_STATIC_ITER,
        abs(st - ed) / abs(step));
    for (int i = st; step > 0 ? i < ed : i > ed; i += step) {
      stmt->rhs = N<IntExpr>(i);
      stmt->type = NT<IndexExpr>(N<IdExpr>("Static"), N<IdExpr>("int"));
      block.push_back(wrap(stmt->clone()));
    }
  } else if (fn &&
             startswith(fn->value, "std.internal.types.range.staticrange:1")) {
    if (vars.size() != 1)
      error("expected one item");
    int ed = fn->type->get_func()
                 ->func_generics[0]
                 .type->get_static()
                 ->evaluate()
                 .get_int();
    if (ed > MAX_STATIC_ITER)
      Err(Error::STATIC_RANGE_BOUNDS, fn, MAX_STATIC_ITER, ed);
    for (int i = 0; i < ed; i++) {
      stmt->rhs = N<IntExpr>(i);
      stmt->type = NT<IndexExpr>(N<IdExpr>("Static"), N<IdExpr>("int"));
      block.push_back(wrap(stmt->clone()));
    }
  } else if (fn && startswith(fn->value, "std.internal.static.fn_overloads")) {
    if (vars.size() != 1)
      error("expected one item");
    if (auto fna = ctx->get_function_args(fn->type)) {
      auto [generics, args] = *fna;
      auto typ = generics[0]->get_class();
      auto name = ctx->get_static_string(generics[1]);
      seqassert(name, "bad static string");
      if (auto n = in(ctx->cache->classes[typ->name].methods, *name)) {
        auto& mt = ctx->cache->overloads[*n];
        for (int mti = int(mt.size()) - 1; mti >= 0; mti--) {
          auto& method = mt[mti];
          if (endswith(method.name, ":dispatch") ||
              !ctx->cache->functions[method.name].type)
            continue;
          if (method.age <= ctx->age) {
            if (typ->get_heterogenous_tuple()) {
              auto& ast = ctx->cache->functions[method.name].ast;
              if (ast->has_attr("autogenerated") &&
                  (endswith(ast->name, ".__iter__:0") ||
                   endswith(ast->name, ".__getitem__:0"))) {
                // ignore __getitem__ and other heterogenuous methods
                continue;
              }
            }
            stmt->rhs = N<IdExpr>(method.name);
            block.push_back(wrap(stmt->clone()));
          }
        }
      }
    } else {
      error("bad call to fn_overloads");
    }
  } else if (fn &&
             startswith(fn->value, "std.internal.builtin.staticenumerate")) {
    if (vars.size() != 2)
      error("expected two items");
    if (auto fna = ctx->get_function_args(fn->type)) {
      auto [generics, args] = *fna;
      auto typ = args[0]->get_record();
      if (!typ)
        error("staticenumerate needs a tuple");
      for (size_t i = 0; i < typ->args.size(); i++) {
        auto b = N<SuiteStmt>(
            {N<AssignStmt>(
                 N<IdExpr>(vars[0]), N<IntExpr>(i),
                 NT<IndexExpr>(NT<IdExpr>("Static"), NT<IdExpr>("int"))),
             N<AssignStmt>(N<IdExpr>(vars[1]),
                           N<IndexExpr>(iter->get_call()->args[0].value->clone(),
                                        N<IntExpr>(i)))});
        block.push_back(wrap(b));
      }
    } else {
      error("bad call to staticenumerate");
    }
  } else if (fn && startswith(fn->value, "std.internal.internal.vars:0")) {
    if (auto fna = ctx->get_function_args(fn->type)) {
      auto [generics, args] = *fna;

      auto withIdx = generics[0]->get_static()->evaluate().get_int() != 0 ? 1 : 0;
      if (!withIdx && vars.size() != 2)
        error("expected two items");
      else if (withIdx && vars.size() != 3)
        error("expected three items");
      auto typ = args[0]->get_class();
      size_t idx = 0;
      for (auto& f : get_class_fields(typ.get())) {
        std::vector<StmtPtr> stmts;
        if (withIdx) {
          stmts.push_back(N<AssignStmt>(
              N<IdExpr>(vars[0]), N<IntExpr>(idx),
              NT<IndexExpr>(NT<IdExpr>("Static"), NT<IdExpr>("int"))));
        }
        stmts.push_back(N<AssignStmt>(
            N<IdExpr>(vars[withIdx]), N<StringExpr>(f.name),
            NT<IndexExpr>(NT<IdExpr>("Static"), NT<IdExpr>("str"))));
        stmts.push_back(N<AssignStmt>(
            N<IdExpr>(vars[withIdx + 1]),
            N<DotExpr>(iter->get_call()->args[0].value->clone(), f.name)));
        auto b = N<SuiteStmt>(stmts);
        block.push_back(wrap(b));
        idx++;
      }
    } else {
      error("bad call to vars");
    }
  } else if (fn && startswith(fn->value, "std.internal.static.vars_types:0")) {
    if (auto fna = ctx->get_function_args(fn->type)) {
      auto [generics, args] = *fna;

      auto typ = realize(generics[0]->get_class());
      auto withIdx = generics[1]->get_static()->evaluate().get_int() != 0 ? 1 : 0;
      if (!withIdx && vars.size() != 1)
        error("expected one item");
      else if (withIdx && vars.size() != 2)
        error("expected two items");

      seqassert(typ, "vars_types expects a realizable type, got '{}' instead",
                generics[0]);
      size_t idx = 0;
      for (auto& f : get_class_fields(typ->get_class().get())) {
        auto ta = realize(ctx->instantiate(f.type, typ->get_class()));
        seqassert(ta, "cannot realize '{}'", f.type->debug_string(1));
        std::vector<StmtPtr> stmts;
        if (withIdx) {
          stmts.push_back(N<AssignStmt>(
              N<IdExpr>(vars[0]), N<IntExpr>(idx),
              NT<IndexExpr>(NT<IdExpr>("Static"), NT<IdExpr>("int"))));
        }
        stmts.push_back(N<AssignStmt>(N<IdExpr>(vars[withIdx]),
                                      NT<IdExpr>(ta->realized_name())));
        auto b = N<SuiteStmt>(stmts);
        block.push_back(wrap(b));
        idx++;
      }
    } else {
      error("bad call to vars");
    }
  } else {
    return {false, {}};
  }
  return {true, block};
}

}  // namespace Pud::AST