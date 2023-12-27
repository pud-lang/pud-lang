#include "Pud/Translate/Translate.h"

#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/Common/Common.h"
#include "Pud/IR/Util/Cloning.h"
#include "Pud/Transform/Parallel/Schedule.h"
#include "Pud/Translate/Context.h"

using fmt::format;
using Pud::IR::cast;
using Pud::IR::Transform::Parallel::OMPSched;

namespace Pud::AST {

TranslateVisitor::TranslateVisitor(std::shared_ptr<TranslateContext> ctx)
    : ctx(std::move(ctx)), result(nullptr) {}

auto TranslateVisitor::apply(Cache* cache, const StmtPtr& stmts) -> IR::Func* {
  IR::BodiedFunc* main = nullptr;
  if (cache->is_jit) {
    auto fn_name = format("_jit_{}", cache->jit_cell);
    main = cache->module->Nr<IR::BodiedFunc>(fn_name);
    main->set_source_info({"<jit>", 0, 0, 0});
    main->set_global();
    auto ir_type = cache->module->unsafe_get_func_type(
        fn_name, cache->classes["NoneType"].realizations["NoneType"]->ir, {},
        false);
    main->realize(ir_type, {});
    main->set_jit();
  } else {
    main = cast<IR::BodiedFunc>(cache->module->get_main_func());
    auto path = get_absolute_path(cache->module0);
    main->set_source_info({path, 0, 0, 0});
  }

  auto block = cache->module->Nr<IR::SeriesFlow>("body");
  main->set_body(block);

  if (!cache->codegen_ctx)
    cache->codegen_ctx = std::make_shared<TranslateContext>(cache);
  cache->codegen_ctx->bases = {main};
  cache->codegen_ctx->series = {block};

  for (auto& g : cache->globals)
    if (!g.second) {
      g.second = g.first == VAR_ARGV
                     ? cache->codegen_ctx->get_module()->get_arg_var()
                     : cache->codegen_ctx->get_module()->N<IR::Var>(
                           SourceInfo(), nullptr, true, false, g.first);
      cache->codegen_ctx->add(TranslateItem::Var, g.first, g.second);
    }

  auto tv = TranslateVisitor(cache->codegen_ctx);
  tv.transform(stmts);
  for (auto& [fn, f] : cache->functions)
    if (startswith(fn, TYPE_TUPLE)) {
      tv.transform_function_realizations(fn, f.ast->attributes.has(Attr::LLVM));
    }
  cache->populate_python_module();
  return main;
}

/************************************************************************************/

auto TranslateVisitor::transform(const ExprPtr& expr) -> IR::Value* {
  TranslateVisitor v(ctx);
  v.set_source_info(expr->get_source_info());

  Type::PartialType* p = nullptr;
  if (expr->attributes) {
    if (expr->has_attr(ExprAttr::List) || expr->has_attr(ExprAttr::Set) ||
        expr->has_attr(ExprAttr::Dict) || expr->has_attr(ExprAttr::Partial)) {
      ctx->seq_items.emplace_back();
    }
    if (expr->has_attr(ExprAttr::Partial))
      p = expr->type->get_partial().get();
  }

  expr->accept(v);
  IR::Value* ir = v.result;

  if (expr->attributes) {
    if (expr->has_attr(ExprAttr::List) || expr->has_attr(ExprAttr::Set)) {
      std::vector<IR::LiteralElement> v;
      for (auto& p : ctx->seq_items.back()) {
        seqassert(p.first <= ExprAttr::StarSequenceItem,
                  "invalid list/set element");
        v.push_back(IR::LiteralElement{p.second,
                                       p.first == ExprAttr::StarSequenceItem});
      }
      if (expr->has_attr(ExprAttr::List))
        ir->set_attribute(std::make_unique<IR::ListLiteralAttribute>(v));
      else
        ir->set_attribute(std::make_unique<IR::SetLiteralAttribute>(v));
      ctx->seq_items.pop_back();
    }
    if (expr->has_attr(ExprAttr::Dict)) {
      std::vector<IR::DictLiteralAttribute::KeyValuePair> v;
      for (int pi = 0; pi < ctx->seq_items.back().size(); pi++) {
        auto& p = ctx->seq_items.back()[pi];
        if (p.first == ExprAttr::StarSequenceItem) {
          v.push_back({p.second, nullptr});
        } else {
          seqassert(
              p.first == ExprAttr::SequenceItem &&
                  pi + 1 < ctx->seq_items.back().size() &&
                  ctx->seq_items.back()[pi + 1].first == ExprAttr::SequenceItem,
              "invalid dict element");
          v.push_back({p.second, ctx->seq_items.back()[pi + 1].second});
          pi++;
        }
      }
      ir->set_attribute(std::make_unique<IR::DictLiteralAttribute>(v));
      ctx->seq_items.pop_back();
    }
    if (expr->has_attr(ExprAttr::Partial)) {
      std::vector<IR::Value*> v;
      seqassert(p, "invalid partial element");
      int j = 0;
      for (int i = 0; i < p->known.size(); i++) {
        if (p->known[i] && p->func->ast->args[i].status == Param::Normal) {
          seqassert(
              j < ctx->seq_items.back().size() &&
                  ctx->seq_items.back()[j].first == ExprAttr::SequenceItem,
              "invalid partial element");
          v.push_back(ctx->seq_items.back()[j++].second);
        } else if (p->func->ast->args[i].status == Param::Normal) {
          v.push_back({nullptr});
        }
      }
      ir->set_attribute(std::make_unique<IR::PartialFunctionAttribute>(
          p->func->ast->name, v));
      ctx->seq_items.pop_back();
    }
    if (expr->has_attr(ExprAttr::SequenceItem)) {
      ctx->seq_items.back().push_back({ExprAttr::SequenceItem, ir});
    }
    if (expr->has_attr(ExprAttr::StarSequenceItem)) {
      ctx->seq_items.back().push_back({ExprAttr::StarSequenceItem, ir});
    }
  }

  return ir;
}

void TranslateVisitor::default_visit(Expr* n) {
  seqassert(false, "invalid node {}", n->to_string());
}

void TranslateVisitor::visit(NoneExpr* expr) {
  auto f = expr->type->realized_name() + ":Optional.__new__:0";
  auto val = ctx->find(f);
  seqassert(val, "cannot find '{}'", f);
  result =
      make<IR::CallInstr>(expr, make<IR::VarValue>(expr, val->get_func()),
                          std::vector<IR::Value*>{});
}

void TranslateVisitor::visit(BoolExpr* expr) {
  result = make<IR::BoolConst>(expr, expr->value, get_type(expr->get_type()));
}

void TranslateVisitor::visit(IntExpr* expr) {
  result =
      make<IR::IntConst>(expr, *(expr->int_value), get_type(expr->get_type()));
}

void TranslateVisitor::visit(FloatExpr* expr) {
  result = make<IR::FloatConst>(expr, *(expr->float_value),
                                get_type(expr->get_type()));
}

void TranslateVisitor::visit(StringExpr* expr) {
  result = make<IR::StringConst>(expr, expr->get_value(),
                                 get_type(expr->get_type()));
}

void TranslateVisitor::visit(IdExpr* expr) {
  auto val = ctx->find(expr->value);
  seqassert(val, "cannot find '{}'", expr->value);
  if (expr->value == "__vtable_size__") {
    // LOG("[] __vtable_size__={}", ctx->cache->class_realization_count + 2);
    result = make<IR::IntConst>(expr, ctx->cache->class_realization_count + 2,
                                get_type(expr->get_type()));
  } else if (auto* v = val->get_var()) {
    result = make<IR::VarValue>(expr, v);
  } else if (auto* f = val->get_func()) {
    result = make<IR::VarValue>(expr, f);
  }
}

void TranslateVisitor::visit(IfExpr* expr) {
  auto cond = transform(expr->cond);
  auto ifexpr = transform(expr->ifexpr);
  auto elsexpr = transform(expr->elsexpr);
  result = make<IR::TernaryInstr>(expr, cond, ifexpr, elsexpr);
}

void TranslateVisitor::visit(CallExpr* expr) {
  if (expr->expr->is_id("__ptr__")) {
    seqassert(expr->args[0].value->get_id(), "expected IdExpr, got {}",
              expr->args[0].value);
    auto val = ctx->find(expr->args[0].value->get_id()->value);
    seqassert(val && val->get_var(), "{} is not a variable",
              expr->args[0].value->get_id()->value);
    result = make<IR::PointerValue>(expr, val->get_var());
    return;
  } else if (expr->expr->is_id("__array__.__new__:0")) {
    auto fnt = expr->expr->type->get_func();
    auto szt = fnt->func_generics[0].type->get_static();
    auto sz = szt->evaluate().get_int();
    auto typ = fnt->func_parent->get_class()->generics[0].type;

    auto* array_type = ctx->get_module()->unsafe_get_array_type(get_type(typ));
    array_type->set_ast_type(expr->get_type());
    result = make<IR::StackAllocInstr>(expr, array_type, sz);
    return;
  } else if (expr->expr->get_id() &&
             startswith(expr->expr->get_id()->value,
                        "__internal__.yield_in_no_suspend:0")) {
    result = make<IR::YieldInInstr>(expr, get_type(expr->get_type()), false);
    return;
  }

  auto ft = expr->expr->type->get_func();
  seqassert(ft, "not calling function: {}", ft);
  auto callee = transform(expr->expr);
  bool is_variadic = ft->ast->has_attr(Attr::CVarArg);
  std::vector<IR::Value*> items;
  for (int i = 0; i < expr->args.size(); i++) {
    seqassert(!expr->args[i].value->get_ellipsis(), "ellipsis not elided");
    if (i + 1 == expr->args.size() && is_variadic) {
      auto call = expr->args[i].value->get_call();
      seqassert(call && call->expr->get_id() &&
                    startswith(call->expr->get_id()->value,
                               std::string(TYPE_TUPLE) + "["),
                "expected *args tuple: '{}'", call->to_string());
      for (auto& arg : call->args)
        items.emplace_back(transform(arg.value));
    } else {
      items.emplace_back(transform(expr->args[i].value));
    }
  }
  result = make<IR::CallInstr>(expr, callee, std::move(items));
}

void TranslateVisitor::visit(DotExpr* expr) {
  if (expr->member == "__atomic__" || expr->member == "__elemsize__" ||
      expr->member == "__contents_atomic__") {
    seqassert(expr->expr->get_id(), "expected IdExpr, got {}", expr->expr);
    auto type = ctx->find(expr->expr->get_id()->value)->get_type();
    seqassert(type, "{} is not a type", expr->expr->get_id()->value);
    result = make<IR::TypePropertyInstr>(
        expr, type,
        expr->member == "__atomic__"
            ? IR::TypePropertyInstr::Property::IS_ATOMIC
            : (expr->member == "__contents_atomic__"
                   ? IR::TypePropertyInstr::Property::IS_CONTENT_ATOMIC
                   : IR::TypePropertyInstr::Property::SIZEOF));
  } else {
    result = make<IR::ExtractInstr>(expr, transform(expr->expr), expr->member);
  }
}

void TranslateVisitor::visit(YieldExpr* expr) {
  result = make<IR::YieldInInstr>(expr, get_type(expr->get_type()));
}

void TranslateVisitor::visit(PipeExpr* expr) {
  auto is_gen = [](const IR::Value* v) -> bool {
    auto* type = v->get_type();
    if (IR::is_a<IR::Types::GeneratorType>(type))
      return true;
    else if (auto* fn = cast<IR::Types::FuncType>(type)) {
      return IR::is_a<IR::Types::GeneratorType>(fn->get_return_type());
    }
    return false;
  };

  std::vector<IR::PipelineFlow::Stage> stages;
  auto* first_stage = transform(expr->items[0].expr);
  auto first_is_gen = is_gen(first_stage);
  stages.emplace_back(first_stage, std::vector<IR::Value*>(), first_is_gen, false);

  // Pipeline without generators (just function call sugar)
  auto simple_pipeline = !first_is_gen;
  for (auto i = 1; i < expr->items.size(); i++) {
    auto call = expr->items[i].expr->get_call();
    seqassert(call, "{} is not a call", expr->items[i].expr);

    auto fn = transform(call->expr);
    if (i + 1 != expr->items.size())
      simple_pipeline &= !is_gen(fn);

    std::vector<IR::Value*> args;
    args.reserve(call->args.size());
    for (auto& a : call->args)
      args.emplace_back(a.value->get_ellipsis() ? nullptr : transform(a.value));
    stages.emplace_back(fn, args, is_gen(fn), false);
  }

  if (simple_pipeline) {
    // Transform a |> b |> c to c(b(a))
    IR::Util::CloneVisitor cv(ctx->get_module());
    result = cv.clone(stages[0].get_callee());
    for (auto i = 1; i < stages.size(); ++i) {
      std::vector<IR::Value*> new_args;
      for (auto arg : stages[i])
        new_args.push_back(arg ? cv.clone(arg) : result);
      result =
          make<IR::CallInstr>(expr, cv.clone(stages[i].get_callee()), new_args);
    }
  } else {
    for (int i = 0; i < expr->items.size(); i++)
      if (expr->items[i].op == "||>")
        stages[i].set_parallel();
    // This is a statement in IR.
    ctx->get_series()->push_back(make<IR::PipelineFlow>(expr, stages));
  }
}

void TranslateVisitor::visit(StmtExpr* expr) {
  auto* body_series = make<IR::SeriesFlow>(expr, "body");
  ctx->add_series(body_series);
  for (auto& s : expr->stmts)
    transform(s);
  ctx->pop_series();
  result = make<IR::FlowInstr>(expr, body_series, transform(expr->expr));
}

/************************************************************************************/

auto TranslateVisitor::transform(const StmtPtr& stmt) -> IR::Value* {
  TranslateVisitor v(ctx);
  v.set_source_info(stmt->get_source_info());
  stmt->accept(v);
  if (v.result)
    ctx->get_series()->push_back(v.result);
  return v.result;
}

void TranslateVisitor::default_visit(Stmt* n) {
  seqassert(false, "invalid node {}", n->to_string());
}

void TranslateVisitor::visit(SuiteStmt* stmt) {
  for (auto& s : stmt->stmts)
    transform(s);
}

void TranslateVisitor::visit(BreakStmt* stmt) {
  result = make<IR::BreakInstr>(stmt);
}

void TranslateVisitor::visit(ContinueStmt* stmt) {
  result = make<IR::ContinueInstr>(stmt);
}

void TranslateVisitor::visit(ExprStmt* stmt) {
  if (stmt->expr->get_call() &&
      stmt->expr->get_call()->expr->is_id("__internal__.yield_final:0")) {
    result = make<IR::YieldInstr>(
        stmt, transform(stmt->expr->get_call()->args[0].value), true);
    ctx->get_base()->set_generator();
  } else {
    result = transform(stmt->expr);
  }
}

void TranslateVisitor::visit(AssignStmt* stmt) {
  if (stmt->lhs && stmt->lhs->is_id(VAR_ARGV))
    return;

  if (stmt->is_update()) {
    seqassert(stmt->lhs->get_id(), "expected IdExpr, got {}", stmt->lhs);
    auto val = ctx->find(stmt->lhs->get_id()->value);
    seqassert(val && val->get_var(), "{} is not a variable",
              stmt->lhs->get_id()->value);
    result = make<IR::AssignInstr>(stmt, val->get_var(), transform(stmt->rhs));
    return;
  }

  seqassert(stmt->lhs->get_id(), "expected IdExpr, got {}", stmt->lhs);
  auto var = stmt->lhs->get_id()->value;
  if (!stmt->rhs || (!stmt->rhs->is_type() && stmt->rhs->type)) {
    auto is_global = in(ctx->cache->globals, var);
    IR::Var* v = nullptr;

    // dead declaration due to static compilation
    if (!stmt->rhs && !stmt->type && !stmt->lhs->type->get_class())
      return;

    if (is_global) {
      seqassert(ctx->find(var) && ctx->find(var)->get_var(),
                "cannot find global '{}'", var);
      v = ctx->find(var)->get_var();
      v->set_source_info(stmt->get_source_info());
      v->set_type(get_type((stmt->rhs ? stmt->rhs : stmt->lhs)->get_type()));
    } else {
      v = make<IR::Var>(
          stmt, get_type((stmt->rhs ? stmt->rhs : stmt->lhs)->get_type()),
          false, false, var);
      ctx->get_base()->push_back(v);
      ctx->add(TranslateItem::Var, var, v);
    }
    // Check if it is a C variable
    if (stmt->lhs->has_attr(ExprAttr::ExternVar)) {
      v->set_external();
      v->set_name(ctx->cache->rev(var));
      v->set_global();
      return;
    }

    if (stmt->rhs)
      result = make<IR::AssignInstr>(stmt, v, transform(stmt->rhs));
  }
}

void TranslateVisitor::visit(AssignMemberStmt* stmt) {
  result = make<IR::InsertInstr>(stmt, transform(stmt->lhs), stmt->member,
                                 transform(stmt->rhs));
}

void TranslateVisitor::visit(ReturnStmt* stmt) {
  result =
      make<IR::ReturnInstr>(stmt, stmt->expr ? transform(stmt->expr) : nullptr);
}

void TranslateVisitor::visit(YieldStmt* stmt) {
  result =
      make<IR::YieldInstr>(stmt, stmt->expr ? transform(stmt->expr) : nullptr);
  ctx->get_base()->set_generator();
}

void TranslateVisitor::visit(WhileStmt* stmt) {
  auto loop = make<IR::WhileFlow>(stmt, transform(stmt->cond),
                                  make<IR::SeriesFlow>(stmt, "body"));
  ctx->add_series(cast<IR::SeriesFlow>(loop->get_body()));
  transform(stmt->suite);
  ctx->pop_series();
  result = loop;
}

void TranslateVisitor::visit(ForStmt* stmt) {
  std::unique_ptr<OMPSched> os = nullptr;
  if (stmt->decorator) {
    os = std::make_unique<OMPSched>();
    auto c = stmt->decorator->get_call();
    seqassert(c, "for par is not a call: {}", stmt->decorator);
    auto fc = c->expr->get_type()->get_func();
    seqassert(fc && fc->ast->name == "std.openmp.for_par:0",
              "for par is not a function");
    auto schedule =
        fc->func_generics[0].type->get_static()->expr->static_value.get_string();
    bool ordered =
        fc->func_generics[1].type->get_static()->expr->static_value.get_int();
    auto threads = transform(c->args[0].value);
    auto chunk = transform(c->args[1].value);
    int64_t collapse =
        fc->func_generics[2].type->get_static()->expr->static_value.get_int();
    bool gpu =
        fc->func_generics[3].type->get_static()->expr->static_value.get_int();
    os = std::make_unique<OMPSched>(schedule, threads, chunk, ordered, collapse,
                                    gpu);
  }

  seqassert(stmt->var->get_id(), "expected IdExpr, got {}", stmt->var);
  auto var_name = stmt->var->get_id()->value;
  IR::Var* var = nullptr;
  if (!ctx->find(var_name) || !stmt->var->has_attr(ExprAttr::Dominated)) {
    var = make<IR::Var>(stmt, get_type(stmt->var->get_type()), false, false,
                        var_name);
  } else {
    var = ctx->find(var_name)->get_var();
  }
  ctx->get_base()->push_back(var);
  auto body_series = make<IR::SeriesFlow>(stmt, "body");

  auto loop = make<IR::ForFlow>(stmt, transform(stmt->iter), body_series, var);
  if (os)
    loop->set_schedule(std::move(os));
  ctx->add(TranslateItem::Var, var_name, var);
  ctx->add_series(cast<IR::SeriesFlow>(loop->get_body()));
  transform(stmt->suite);
  ctx->pop_series();
  result = loop;
}

void TranslateVisitor::visit(IfStmt* stmt) {
  auto cond = transform(stmt->cond);
  auto true_series = make<IR::SeriesFlow>(stmt, "ifstmt_true");
  ctx->add_series(true_series);
  transform(stmt->if_suite);
  ctx->pop_series();

  IR::SeriesFlow* false_series = nullptr;
  if (stmt->else_suite) {
    false_series = make<IR::SeriesFlow>(stmt, "ifstmt_false");
    ctx->add_series(false_series);
    transform(stmt->else_suite);
    ctx->pop_series();
  }
  result = make<IR::IfFlow>(stmt, cond, true_series, false_series);
}

void TranslateVisitor::visit(TryStmt* stmt) {
  auto* body_series = make<IR::SeriesFlow>(stmt, "body");
  ctx->add_series(body_series);
  transform(stmt->suite);
  ctx->pop_series();

  auto finally_series = make<IR::SeriesFlow>(stmt, "finally");
  if (stmt->finally) {
    ctx->add_series(finally_series);
    transform(stmt->finally);
    ctx->pop_series();
  }

  auto* tc = make<IR::TryCatchFlow>(stmt, body_series, finally_series);
  for (auto& c : stmt->catches) {
    auto* catch_body = make<IR::SeriesFlow>(stmt, "catch");
    auto* exc_type = c.exc ? get_type(c.exc->get_type()) : nullptr;
    IR::Var* catch_var = nullptr;
    if (!c.var.empty()) {
      if (!ctx->find(c.var) || !c.exc->has_attr(ExprAttr::Dominated)) {
        catch_var = make<IR::Var>(stmt, exc_type, false, false, c.var);
      } else {
        catch_var = ctx->find(c.var)->get_var();
      }
      ctx->add(TranslateItem::Var, c.var, catch_var);
      ctx->get_base()->push_back(catch_var);
    }
    ctx->add_series(catch_body);
    transform(c.suite);
    ctx->pop_series();
    tc->push_back(IR::TryCatchFlow::Catch(catch_body, exc_type, catch_var));
  }
  result = tc;
}

void TranslateVisitor::visit(ThrowStmt* stmt) {
  result =
      make<IR::ThrowInstr>(stmt, stmt->expr ? transform(stmt->expr) : nullptr);
}

void TranslateVisitor::visit(FunctionStmt* stmt) {
  // Process all realizations.
  transform_function_realizations(stmt->name, stmt->attributes.has(Attr::LLVM));
}

void TranslateVisitor::visit(ClassStmt* stmt) {
  // Nothing to see here, as all type handles are already generated.
  // Methods will be handled by FunctionStmt visitor.
}

/************************************************************************************/

Pud::IR::Types::Type* TranslateVisitor::get_type(const Type::TypePtr& t) {
  seqassert(t && t->get_class(), "{} is not a class", t);
  std::string name = t->get_class()->realized_type_name();
  auto i = ctx->find(name);
  seqassert(i, "type {} not realized", t);
  return i->get_type();
}

void TranslateVisitor::transform_function_realizations(const std::string& name,
                                                       bool is_llvm) {
  for (auto& real : ctx->cache->functions[name].realizations) {
    if (!in(ctx->cache->pending_realizations, make_pair(name, real.first)))
      continue;
    ctx->cache->pending_realizations.erase(make_pair(name, real.first));

    LOG_TYPECHECK("[translate] generating fn {}", real.first);
    real.second->ir->set_source_info(get_source_info());
    const auto& ast = real.second->ast;
    seqassert(ast, "AST not set for {}", real.first);
    if (!is_llvm)
      transform_function(real.second->type.get(), ast.get(), real.second->ir);
    else
      transform_llvm_function(real.second->type.get(), ast.get(),
                            real.second->ir);
  }
}

void TranslateVisitor::transform_function(Type::FuncType* type,
                                         FunctionStmt* ast, IR::Func* func) {
  std::vector<std::string> names;
  std::vector<int> indices;
  for (int i = 0, j = 0; i < ast->args.size(); i++)
    if (ast->args[i].status == Param::Normal) {
      if (!type->get_arg_types()[j]->get_func()) {
        names.push_back(ctx->cache->reverse_identifier_lookup[ast->args[i].name]);
        indices.push_back(i);
      }
      j++;
    }
  if (ast->has_attr(Attr::CVarArg)) {
    names.pop_back();
    indices.pop_back();
  }
  // TODO: refactor IR attribute API
  std::map<std::string, std::string> attr;
  attr[".module"] = ast->attributes.module;
  for (auto& a : ast->attributes.custom_attr) {
    attr[a] = "";
  }
  func->set_attribute(std::make_unique<IR::KeyValueAttribute>(attr));
  for (int i = 0; i < names.size(); i++)
    func->get_arg_var(names[i])->set_source_info(
        ast->args[indices[i]].get_source_info());
  // func->setUnmangledName(ctx->cache->reverse_identifier_lookup[type->ast->name]);
  if (!ast->attributes.has(Attr::C) && !ast->attributes.has(Attr::Internal)) {
    ctx->add_block();
    for (auto i = 0; i < names.size(); i++)
      ctx->add(TranslateItem::Var, ast->args[indices[i]].name,
               func->get_arg_var(names[i]));
    auto body = make<IR::SeriesFlow>(ast, "body");
    ctx->bases.push_back(cast<IR::BodiedFunc>(func));
    ctx->add_series(body);
    transform(ast->suite);
    ctx->pop_series();
    ctx->bases.pop_back();
    cast<IR::BodiedFunc>(func)->set_body(body);
    ctx->pop_block();
  }
}

void TranslateVisitor::transform_llvm_function(Type::FuncType* type,
                                             FunctionStmt* ast,
                                             IR::Func* func) {
  std::vector<std::string> names;
  std::vector<int> indices;
  for (int i = 0, j = 1; i < ast->args.size(); i++)
    if (ast->args[i].status == Param::Normal) {
      names.push_back(ctx->cache->reverse_identifier_lookup[ast->args[i].name]);
      indices.push_back(i);
      j++;
    }
  auto f = cast<IR::LLVMFunc>(func);
  // TODO: refactor IR attribute API
  std::map<std::string, std::string> attr;
  attr[".module"] = ast->attributes.module;
  for (auto& a : ast->attributes.custom_attr)
    attr[a] = "";
  func->set_attribute(std::make_unique<IR::KeyValueAttribute>(attr));
  for (int i = 0; i < names.size(); i++)
    func->get_arg_var(names[i])->set_source_info(
        ast->args[indices[i]].get_source_info());

  seqassert(ast->suite->first_in_block() &&
                ast->suite->first_in_block()->get_expr() &&
                ast->suite->first_in_block()->get_expr()->expr->get_string(),
            "LLVM function does not begin with a string");
  std::istringstream sin(
      ast->suite->first_in_block()->get_expr()->expr->get_string()->get_value());
  std::vector<IR::Types::Generic> literals;
  auto& ss = ast->suite->get_suite()->stmts;
  for (int i = 1; i < ss.size(); i++) {
    if (auto* ei =
            ss[i]->get_expr()->expr->get_int()) {  // static integer expression
      literals.emplace_back(*(ei->int_value));
    } else if (auto* es =
                   ss[i]->get_expr()->expr->get_string()) {  // static string
      literals.emplace_back(es->get_value());
    } else {
      seqassert(ss[i]->get_expr()->expr->get_type(),
                "invalid LLVM type argument: {}",
                ss[i]->get_expr()->to_string());
      literals.emplace_back(get_type(ss[i]->get_expr()->expr->get_type()));
    }
  }
  bool is_declare = true;
  std::string declare;
  std::vector<std::string> lines;
  for (std::string l; getline(sin, l);) {
    std::string lp = l;
    ltrim(lp);
    rtrim(lp);
    // Extract declares and constants.
    if (is_declare && !startswith(lp, "declare ") && !startswith(lp, "@")) {
      bool is_const = lp.find("private constant") != std::string::npos;
      if (!is_const) {
        is_declare = false;
        if (!lp.empty() && lp.back() != ':')
          lines.emplace_back("entry:");
      }
    }
    if (is_declare)
      declare += lp + "\n";
    else
      lines.emplace_back(l);
  }
  f->set_llvm_body(join(lines, "\n"));
  f->set_llvm_declarations(declare);
  f->set_llvm_literals(literals);
  // func->setUnmangledName(ctx->cache->reverse_identifier_lookup[type->ast->name]);
}

}  // namespace Pud::AST