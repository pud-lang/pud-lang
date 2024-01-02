#include <limits>
#include <map>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

const int MAX_TYPECHECK_ITER = 1000;

namespace Pud::AST {

/// Unify types a (passed by reference) and b.
/// Destructive operation as it modifies both a and b. If types cannot be
/// unified, raise an error.
/// @param a Type (by reference)
/// @param b Type
/// @return a
Type::TypePtr TypecheckVisitor::unify(Type::TypePtr& a, const Type::TypePtr& b) {
  if (!a)
    return a = b;
  seqassert(b, "rhs is nullptr");
  Type::Type::Unification undo;
  if (a->unify(b.get(), &undo) >= 0) {
    return a;
  } else {
    undo.undo();
  }
  a->unify(b.get(), &undo);
  Err(Error::TYPE_UNIFY, get_source_info(), a->pretty_string(), b->pretty_string());
  return nullptr;
}

/// Infer all types within a StmtPtr. Implements the LTS-DI typechecking.
/// @param isToplevel set if typechecking the program toplevel.
StmtPtr TypecheckVisitor::infer_types(StmtPtr result, bool isToplevel) {
  if (!result)
    return nullptr;

  for (ctx->get_realization_base()->iteration = 1;;
       ctx->get_realization_base()->iteration++) {
    LOG_TYPECHECK("[iter] {} :: {}", ctx->get_realization_base()->name,
                  ctx->get_realization_base()->iteration);
    if (ctx->get_realization_base()->iteration >= MAX_TYPECHECK_ITER) {
      error(result, "cannot typecheck '{}' in reasonable time",
            ctx->get_realization_base()->name.empty()
                ? "toplevel"
                : ctx->cache->rev(ctx->get_realization_base()->name));
    }

    // Keep iterating until:
    //   (1) success: the statement is marked as done; or
    //   (2) failure: no expression or statements were marked as done during an
    //                iteration (i.e., changed_nodes is zero)
    ctx->typecheck_level++;
    auto changed_nodes = ctx->changed_nodes;
    ctx->changed_nodes = 0;
    auto return_early = ctx->return_early;
    ctx->return_early = false;
    TypecheckVisitor(ctx).transform(result);
    std::swap(ctx->changed_nodes, changed_nodes);
    std::swap(ctx->return_early, return_early);
    ctx->typecheck_level--;

    if (ctx->get_realization_base()->iteration == 1 && isToplevel) {
      // Realize all @force_realize functions
      for (auto& f : ctx->cache->functions) {
        auto& attr = f.second.ast->attributes;
        if (f.second.type && f.second.realizations.empty() &&
            (attr.has(Attr::ForceRealize) || attr.has(Attr::Export) ||
             (attr.has(Attr::C) && !attr.has(Attr::CVarArg)))) {
          seqassert(f.second.type->can_realize(), "cannot realize {}", f.first);
          realize(ctx->instantiate(f.second.type)->get_func());
          seqassert(!f.second.realizations.empty(), "cannot realize {}",
                    f.first);
        }
      }
    }

    if (result->is_done()) {
      break;
    } else if (changed_nodes) {
      continue;
    } else {
      // Special case: nothing was changed, however there are unbound types that
      // have default values (e.g., generics with default values). Unify those
      // types with their default values and then run another round to see if
      // anything changed.
      bool anotherRound = false;
      // Special case: return type might have default as well (e.g., Union)
      if (ctx->get_realization_base()->return_type)
        ctx->get_realization_base()->pending_defaults.insert(
            ctx->get_realization_base()->return_type);
      for (auto& unbound : ctx->get_realization_base()->pending_defaults) {
        if (auto tu = unbound->get_union()) {
          // Seal all dynamic unions after the iteration is over
          if (!tu->is_sealed()) {
            tu->seal();
            anotherRound = true;
          }
        } else if (auto u = unbound->get_link()) {
          Type::Type::Unification undo;
          if (u->default_type && u->unify(u->default_type.get(), &undo) >= 0)
            anotherRound = true;
        }
      }
      ctx->get_realization_base()->pending_defaults.clear();
      if (anotherRound)
        continue;

      // Nothing helps. Return nullptr.
      return nullptr;
    }
  }

  return result;
}

/// Realize a type and create IR type stub. If type is a function type, also
/// realize the underlying function and generate IR function stub.
/// @return realized type or nullptr if the type cannot be realized
Type::TypePtr TypecheckVisitor::realize(Type::TypePtr typ) {
  if (!typ || !typ->can_realize()) {
    return nullptr;
  }

  if (typ->get_static()) {
    // Nothing to realize here
    return typ;
  }

  try {
    if (auto f = typ->get_func()) {
      if (auto ret = realize_func(f.get())) {
        // Realize Function[..] type as well
        realize_type(ret->get_class().get());
        return unify(ret, typ);  // Needed for return type unification
      }
    } else if (auto c = typ->get_class()) {
      auto t = realize_type(c.get());
      if (auto p = typ->get_partial()) {
        // Ensure that the partial type is preserved
        t = std::make_shared<Type::PartialType>(t->get_record(), p->func, p->known);
      }
      if (t) {
        return unify(t, typ);
      }
    }
  } catch (ParserException& e) {
    if (e.error_code == Error::MAX_REALIZATION)
      throw;
    if (auto f = typ->get_func()) {
      if (f->ast->attributes.has(Attr::HiddenFromUser)) {
        e.locations.back() = get_source_info();
      } else {
        std::vector<std::string> args;
        for (size_t i = 0, ai = 0, gi = 0; i < f->ast->args.size(); i++) {
          auto an = f->ast->args[i].name;
          auto ns = trim_stars(an);
          args.push_back(
              fmt::format("{}{}: {}", std::string(ns, '*'), ctx->cache->rev(an),
                          f->ast->args[i].status == Param::Generic
                              ? f->func_generics[gi++].type->pretty_string()
                              : f->get_arg_types()[ai++]->pretty_string()));
        }
        auto name = f->ast->name;
        std::string name_args;
        if (startswith(name, "._import_")) {
          name = name.substr(9);
          auto p = name.rfind('_');
          if (p != std::string::npos)
            name = name.substr(0, p);
          name = "<import " + name + ">";
        } else {
          name = ctx->cache->rev(f->ast->name);
          name_args = fmt::format("({})", fmt::join(args, ", "));
        }
        e.track_realize(fmt::format("{}{}", name, name_args), get_source_info());
      }
    } else {
      e.track_realize(typ->pretty_string(), get_source_info());
    }
    throw;
  }
  return nullptr;
}

/// Realize a type and create IR type stub.
/// @return realized type or nullptr if the type cannot be realized
Type::TypePtr TypecheckVisitor::realize_type(Type::ClassType* type) {
  if (!type || !type->can_realize())
    return nullptr;

  if (auto tr = type->get_record())
    tr->flatten();

  // Check if the type fields are all initialized
  // (sometimes that's not the case: e.g., `class X: x: List[X]`)
  for (auto field : get_class_fields(type)) {
    if (!field.type)
      return nullptr;
  }

  // Check if the type was already realized
  if (auto r = in(ctx->cache->classes[type->name].realizations,
                  type->realized_type_name())) {
    return (*r)->type->get_class();
  }

  auto realized = type->get_class();
  if (type->get_func()) {
    // Just realize the function stub
    realized = std::make_shared<Type::RecordType>(realized, type->get_func()->args);
  }

  // Realize generics
  for (auto& e : realized->generics) {
    if (!realize(e.type))
      return nullptr;
  }

  LOG_REALIZE("[realize] ty {} -> {}", realized->name,
              realized->realized_type_name());

  // Realizations should always be visible, so add them to the toplevel
  ctx->add_toplevel(
      realized->realized_type_name(),
      std::make_shared<TypecheckItem>(TypecheckItem::Type, realized));
  auto realization = ctx->cache->classes[realized->name]
                         .realizations[realized->realized_type_name()] =
      std::make_shared<Cache::Class::ClassRealization>();
  realization->type = realized;
  realization->id = ctx->cache->class_realization_count++;

  // Realize tuple arguments
  if (auto tr = realized->get_record()) {
    for (auto& a : tr->args)
      realize(a);
  }

  // Create LLVM stub
  auto lt = make_ir_type(realized.get());

  // Realize fields
  std::vector<IR::Types::Type*> typeArgs;     // needed for IR
  std::vector<std::string> names;             // needed for IR
  std::map<std::string, SourceInfo> memberInfo;  // needed for IR
  if (realized->is(TYPE_TUPLE))
    realized->get_record()->flatten();
  int i = 0;
  for (auto& field : get_class_fields(realized.get())) {
    auto ftyp = ctx->instantiate(field.type, realized);
    // HACK: repeated tuples have no generics so this is needed to fix the
    // instantiation above
    if (realized->is(TYPE_TUPLE))
      unify(ftyp, realized->get_record()->args[i]);

    if (!realize(ftyp))
      Err(Error::TYPE_CANNOT_REALIZE_ATTR, get_source_info(), field.name,
        ftyp->pretty_string());
    realization->fields.emplace_back(field.name, ftyp);
    names.emplace_back(field.name);
    typeArgs.emplace_back(make_ir_type(ftyp->get_class().get()));
    memberInfo[field.name] = field.type->get_source_info();
    i++;
  }

  // Set IR attributes
  if (auto* cls = IR::cast<IR::Types::RefType>(lt))
    if (!names.empty()) {
      cls->get_contents()->realize(typeArgs, names);
      cls->set_attribute(std::make_unique<IR::MemberAttribute>(memberInfo));
      cls->get_contents()->set_attribute(
          std::make_unique<IR::MemberAttribute>(memberInfo));
    }

  // Fix for partial types
  if (auto p = type->get_partial()) {
    auto pt =
        std::make_shared<Type::PartialType>(realized->get_record(), p->func, p->known);
    ctx->add_toplevel(pt->realized_name(),
                     std::make_shared<TypecheckItem>(TypecheckItem::Type, pt));
    ctx->cache->classes[pt->name].realizations[pt->realized_name()] =
        ctx->cache->classes[realized->name]
            .realizations[realized->realized_type_name()];
  }

  return realized;
}

Type::TypePtr TypecheckVisitor::realize_func(Type::FuncType* type,
                                             bool force) {
  auto& realizations = ctx->cache->functions[type->ast->name].realizations;
  if (auto r = in(realizations, type->realized_name())) {
    if (!force) {
      return (*r)->type;
    }
  }

  if (ctx->get_realization_depth() > MAX_REALIZATION_DEPTH) {
    Err(Error::MAX_REALIZATION, get_source_info(), ctx->cache->rev(type->ast->name));
  }

  LOG_REALIZE("[realize] fn {} -> {} : base {} ; depth = {}", type->ast->name,
              type->realized_name(), ctx->get_realization_stack_name(),
              ctx->get_realization_depth());
  get_logger().level++;
  ctx->add_block();
  ctx->typecheck_level++;

  // Find function parents
  ctx->realization_bases.push_back(
      {type->ast->name, type->get_func(), type->get_ret_type()});

  // Clone the generic AST that is to be realized
  auto ast = generate_special_ast(type);
  add_function_generics(type);

  // Internal functions have no AST that can be realized
  bool hasAst = ast->suite && !ast->attributes.has(Attr::Internal);
  // Add function arguments
  for (size_t i = 0, j = 0; hasAst && i < ast->args.size(); i++)
    if (ast->args[i].status == Param::Normal) {
      std::string varName = ast->args[i].name;
      trim_stars(varName);
      ctx->add(TypecheckItem::Var, varName,
               std::make_shared<Type::LinkType>(type->get_arg_types()[j++]));
    }

  // Populate realization table in advance to support recursive realizations
  auto key = type->realized_name();  // note: the key might change later
  IR::Func* oldIR = nullptr;  // Get it if it was already made (force mode)
  if (auto i = in(realizations, key))
    oldIR = (*i)->ir;
  auto r = realizations[key] =
      std::make_shared<Cache::Function::FunctionRealization>();
  r->type = type->get_func();
  r->ir = oldIR;

  // Realizations should always be visible, so add them to the toplevel
  ctx->add_toplevel(key, std::make_shared<TypecheckItem>(TypecheckItem::Func,
                                                        type->get_func()));

  if (hasAst) {
    auto oldBlockLevel = ctx->block_level;
    ctx->block_level = 0;
    auto ret = infer_types(ast->suite);
    ctx->block_level = oldBlockLevel;

    if (!ret) {
      realizations.erase(key);
      if (!startswith(ast->name, "._lambda")) {
        // Lambda typecheck failures are "ignored" as they are treated as
        // statements, not functions.
        // TODO: generalize this further.
        // LOG("{}", ast->suite->to_string(2));
        error("cannot typecheck the program");
      }
      ctx->realization_bases.pop_back();
      ctx->pop_block();
      ctx->typecheck_level--;
      get_logger().level--;
      return nullptr;  // inference must be delayed
    }

    // Use NoneType as the return type when the return type is not specified and
    // function has no return statement
    if (!ast->ret && type->get_ret_type()->get_unbound())
      unify(type->get_ret_type(), ctx->get_type("NoneType"));
  }
  // Realize the return type
  auto ret = realize(type->get_ret_type());
  seqassert(ret, "cannot realize return type '{}'", type->get_ret_type());

  std::vector<Param> args;
  for (auto& i : ast->args) {
    std::string varName = i.name;
    trim_stars(varName);
    args.emplace_back(Param{varName, nullptr, nullptr, i.status});
  }
  r->ast = N<FunctionStmt>(ast->get_source_info(), r->type->realized_name(), nullptr,
                           args, ast->suite);
  r->ast->attributes = ast->attributes;

  if (!in(ctx->cache->pending_realizations,
          make_pair(type->ast->name, type->realized_name()))) {
    if (!r->ir)
      r->ir = make_ir_function(r);
    realizations[type->realized_name()] = r;
  } else {
    realizations[key] = realizations[type->realized_name()];
  }
  if (force)
    realizations[type->realized_name()]->ast = r->ast;
  ctx->add_toplevel(
      type->realized_name(),
      std::make_shared<TypecheckItem>(TypecheckItem::Func, type->get_func()));
  ctx->realization_bases.pop_back();
  ctx->pop_block();
  ctx->typecheck_level--;
  get_logger().level--;

  return type->get_func();
}

/// Generate ASTs for all __internal__ functions that deal with vtable
/// generation. Intended to be called once the typechecking is done.
/// TODO: add JIT compatibility.
StmtPtr TypecheckVisitor::prepare_vtables() {
  auto rep = "__internal__.class_populate_vtables:0";  // see internal.codon
  auto& initFn = ctx->cache->functions[rep];
  auto suite = N<SuiteStmt>();
  for (auto& [_, cls] : ctx->cache->classes) {
    for (auto& [r, real] : cls.realizations) {
      size_t vtSz = 0;
      for (auto& [base, vtable] : real->vtables) {
        if (!vtable.ir)
          vtSz += vtable.table.size();
      }
      if (!vtSz)
        continue;
      // __internal__.class_set_rtti_vtable(real.ID, size, real.type)
      suite->stmts.push_back(N<ExprStmt>(N<CallExpr>(
          N<IdExpr>("__internal__.class_set_rtti_vtable:0"),
          N<IntExpr>(real->id), N<IntExpr>(vtSz + 2), NT<IdExpr>(r))));
      // LOG("[poly] {} -> {}", r, real->id);
      vtSz = 0;
      for (auto& [base, vtable] : real->vtables) {
        if (!vtable.ir) {
          for (auto& [k, v] : vtable.table) {
            auto& [fn, id] = v;
            std::vector<ExprPtr> ids;
            for (auto& t : fn->get_arg_types())
              ids.push_back(NT<IdExpr>(t->realized_name()));
            // p[real.ID].__setitem__(f.ID, Function[<TYPE_F>](f).__raw__())
            LOG_REALIZE("[poly] vtable[{}][{}] = {}", real->id, vtSz + id, fn);
            suite->stmts.push_back(N<ExprStmt>(N<CallExpr>(
                N<IdExpr>("__internal__.class_set_rtti_vtable_fn:0"),
                N<IntExpr>(real->id), N<IntExpr>(vtSz + id),
                N<CallExpr>(N<DotExpr>(
                    N<CallExpr>(
                        NT<InstantiateExpr>(
                            NT<IdExpr>("Function"),
                            std::vector<ExprPtr>{
                                NT<InstantiateExpr>(NT<IdExpr>(TYPE_TUPLE),
                                                    ids),
                                NT<IdExpr>(fn->get_ret_type()->realized_name())}),
                        N<IdExpr>(fn->realized_name())),
                    "__raw__")),
                NT<IdExpr>(r))));
          }
          vtSz += vtable.table.size();
        }
      }
    }
  }
  initFn.ast->suite = suite;
  auto typ = initFn.realizations.begin()->second->type;
  LOG_REALIZE("[poly] {} : {}", typ, suite->to_string(2));
  typ->ast = initFn.ast.get();
  realize_func(typ.get(), true);

  auto& initDist =
      ctx->cache->functions["__internal__.class_base_derived_dist:0"];
  // def class_base_derived_dist(B, D):
  //   return Tuple[<types before B is reached in D>].__elemsize__
  auto oldAst = initDist.ast;
  for (auto& [_, real] : initDist.realizations) {
    auto t = real->type;
    auto baseTyp = t->func_generics[0].type->get_class();
    auto derivedTyp = t->func_generics[1].type->get_class();

    const auto& fields = get_class_fields(derivedTyp.get());
    auto types = std::vector<ExprPtr>{};
    auto found = false;
    for (auto& f : fields) {
      if (f.base_class == baseTyp->name) {
        found = true;
        break;
      } else {
        auto ft = realize(ctx->instantiate(f.type, derivedTyp));
        types.push_back(NT<IdExpr>(ft->realized_name()));
      }
    }
    seqassert(found || get_class_fields(baseTyp.get()).empty(),
              "cannot find distance between {} and {}", derivedTyp->name,
              baseTyp->name);
    StmtPtr suite = N<ReturnStmt>(N<DotExpr>(
        NT<InstantiateExpr>(NT<IdExpr>(TYPE_TUPLE), types), "__elemsize__"));
    LOG_REALIZE("[poly] {} : {}", t, *suite);
    initDist.ast->suite = suite;
    t->ast = initDist.ast.get();
    realize_func(t.get(), true);
  }
  initDist.ast = oldAst;

  return nullptr;
}

/// Generate thunks in all derived classes for a given virtual function (must be
/// fully realizable) and the corresponding base class.
/// @return unique thunk ID.
size_t TypecheckVisitor::get_realization_id(Type::ClassType* cp,
                                          Type::FuncType* fp) {
  seqassert(
      cp->can_realize() && fp->can_realize() && fp->get_ret_type()->can_realize(),
      "{} not realized", fp->debug_string(1));

  // TODO: ugly, ugly; surely needs refactoring

  // Function signature for storing thunks
  auto sig = [](Type::FuncType* fp) {
    std::vector<std::string> gs;
    for (auto& a : fp->get_arg_types())
      gs.push_back(a->realized_name());
    gs.push_back("|");
    for (auto& a : fp->func_generics)
      if (!a.name.empty())
        gs.push_back(a.type->realized_name());
    return join(gs, ",");
  };

  // Set up the base class information
  auto baseCls = cp->name;
  auto fnName = ctx->cache->rev(fp->ast->name);
  auto key = make_pair(fnName, sig(fp));
  auto& vt = ctx->cache->classes[baseCls]
                 .realizations[cp->realized_name()]
                 ->vtables[cp->realized_name()];

  // Add or extract thunk ID
  size_t vid = 0;
  if (auto i = in(vt.table, key)) {
    vid = i->second;
  } else {
    vid = vt.table.size() + 1;
    vt.table[key] = {fp->get_func(), vid};
  }

  // Iterate through all derived classes and instantiate the corresponding thunk
  for (auto& [clsName, cls] : ctx->cache->classes) {
    bool inMro = false;
    for (auto& m : cls.mro)
      if (m->type && m->type->get_class() &&
          m->type->get_class()->name == baseCls) {
        inMro = true;
        break;
      }
    if (clsName != baseCls && inMro) {
      for (auto& [_, real] : cls.realizations) {
        auto& vtable = real->vtables[baseCls];

        auto ct =
            ctx->instantiate(ctx->force_find(clsName)->type, cp->get_class())
                ->get_class();
        std::vector<Type::TypePtr> args = fp->get_arg_types();
        args[0] = ct;
        auto m = find_best_method(ct, fnName, args);
        if (!m) {
          // Print a nice error message
          std::vector<std::string> a;
          for (auto& t : args)
            a.emplace_back(fmt::format("{}", t->pretty_string()));
          std::string argsNice = fmt::format("({})", fmt::join(a, ", "));
          Err(Error::DOT_NO_ATTR_ARGS, get_source_info(), ct->pretty_string(), fnName,
            argsNice);
        }

        std::vector<std::string> ns;
        for (auto& a : args)
          ns.push_back(a->realized_name());

        // Thunk name: _thunk.<BASE>.<FN>.<ARGS>
        auto thunkName = format("_thunk.{}.{}.{}", baseCls, m->ast->name,
                                fmt::join(ns, "."));
        if (in(ctx->cache->functions, thunkName))
          continue;

        // Thunk contents:
        // def _thunk.<BASE>.<FN>.<ARGS>(self, <ARGS...>):
        //   return <FN>(
        //     __internal__.class_base_to_derived(self, <BASE>, <DERIVED>),
        //     <ARGS...>)
        std::vector<Param> fnArgs;
        fnArgs.emplace_back(fp->ast->args[0].name,
                            N<IdExpr>(cp->realized_name()), nullptr);
        for (size_t i = 1; i < args.size(); i++)
          fnArgs.emplace_back(fp->ast->args[i].name,
                              N<IdExpr>(args[i]->realized_name()), nullptr);
        std::vector<ExprPtr> callArgs;
        callArgs.emplace_back(N<CallExpr>(
            N<IdExpr>("__internal__.class_base_to_derived:0"),
            N<IdExpr>(fp->ast->args[0].name), N<IdExpr>(cp->realized_name()),
            N<IdExpr>(real->type->realized_name())));
        for (size_t i = 1; i < args.size(); i++)
          callArgs.emplace_back(N<IdExpr>(fp->ast->args[i].name));
        auto thunkAst = N<FunctionStmt>(
            thunkName, nullptr, fnArgs,
            N<SuiteStmt>(
                N<ReturnStmt>(N<CallExpr>(N<IdExpr>(m->ast->name), callArgs))),
            Attr({"std.internal.attributes.inline", Attr::ForceRealize}));
        auto& thunkFn = ctx->cache->functions[thunkAst->name];
        thunkFn.ast = std::static_pointer_cast<FunctionStmt>(thunkAst->clone());

        transform(thunkAst);
        prepend_stmts->push_back(thunkAst);
        auto ti = ctx->instantiate(thunkFn.type)->get_func();
        auto tm = realize_func(ti.get(), true);
        seqassert(tm, "bad thunk {}", thunkFn.type);
        vtable.table[key] = {tm->get_func(), vid};
      }
    }
  }
  return vid;
}

/// Make IR node for a realized type.
IR::Types::Type* TypecheckVisitor::make_ir_type(Type::ClassType* t) {
  // Realize if not, and return cached value if it exists
  auto realized_name = t->realized_type_name();
  if (!in(ctx->cache->classes[t->name].realizations, realized_name))
    realize(t->get_class());
  if (auto l = ctx->cache->classes[t->name].realizations[realized_name]->ir)
    return l;

  auto force_find_ir_type = [&](const Type::TypePtr& tt) {
    auto t = tt->get_class();
    seqassert(t && in(ctx->cache->classes[t->name].realizations,
                      t->realized_type_name()),
              "{} not realized", tt);
    auto l =
        ctx->cache->classes[t->name].realizations[t->realized_type_name()]->ir;
    seqassert(l, "no LLVM type for {}", t);
    return l;
  };

  // Prepare generics and statics
  std::vector<IR::Types::Type*> types;
  std::vector<StaticValue*> statics;
  for (auto& m : t->generics) {
    if (auto s = m.type->get_static()) {
      seqassert(s->expr->static_value.evaluated, "static not realized");
      statics.push_back(&(s->expr->static_value));
    } else {
      types.push_back(force_find_ir_type(m.type));
    }
  }

  // Get the IR type
  auto* module = ctx->cache->module;
  IR::Types::Type* handle = nullptr;

  if (t->name == "bool") {
    handle = module->get_bool_type();
  } else if (t->name == "byte") {
    handle = module->get_byte_type();
  } else if (t->name == "int") {
    handle = module->get_int_type();
  } else if (t->name == "float") {
    handle = module->get_float_type();
  } else if (t->name == "float32") {
    handle = module->get_float32_type();
  } else if (t->name == "float16") {
    handle = module->get_float16_type();
  } else if (t->name == "bfloat16") {
    handle = module->get_bfloat16_type();
  } else if (t->name == "float128") {
    handle = module->get_float128_type();
  } else if (t->name == "str") {
    handle = module->get_string_type();
  } else if (t->name == "Int" || t->name == "UInt") {
    handle =
        module->Nr<IR::Types::IntNType>(statics[0]->get_int(), t->name == "Int");
  } else if (t->name == "Ptr") {
    seqassert(types.size() == 1 && statics.empty(), "bad generics/statics");
    handle = module->unsafe_get_pointer_type(types[0]);
  } else if (t->name == "Generator") {
    seqassert(types.size() == 1 && statics.empty(), "bad generics/statics");
    handle = module->unsafe_get_generator_type(types[0]);
  } else if (t->name == TYPE_OPTIONAL) {
    seqassert(types.size() == 1 && statics.empty(), "bad generics/statics");
    handle = module->unsafe_get_optional_type(types[0]);
  } else if (t->name == "NoneType") {
    seqassert(types.empty() && statics.empty(), "bad generics/statics");
    auto record = IR::cast<IR::Types::RecordType>(
        module->unsafe_get_membered_type(realized_name));
    record->realize({}, {});
    handle = record;
  } else if (t->name == "Union") {
    seqassert(!types.empty() && statics.empty(), "bad union");
    auto unionTypes = t->get_union()->get_realization_types();
    std::vector<IR::Types::Type*> unionVec;
    unionVec.reserve(unionTypes.size());
    for (auto& u : unionTypes)
      unionVec.emplace_back(force_find_ir_type(u));
    handle = module->unsafe_get_union_type(unionVec);
  } else if (t->name == "Function") {
    types.clear();
    for (auto& m : t->generics[0].type->get_record()->args)
      types.push_back(force_find_ir_type(m));
    auto ret = force_find_ir_type(t->generics[1].type);
    handle = module->unsafe_get_func_type(realized_name, ret, types);
  } else if (t->name == "std.experimental.simd.Vec") {
    seqassert(types.size() == 1 && statics.size() == 1, "bad generics/statics");
    handle = module->unsafe_get_vector_type(statics[0]->get_int(), types[0]);
  } else if (auto tr = t->get_record()) {
    seqassert(tr->get_repeats() >= 0, "repeats not resolved: '{}'",
              tr->debug_string(2));
    tr->flatten();
    std::vector<IR::Types::Type*> typeArgs;
    std::vector<std::string> names;
    std::map<std::string, SourceInfo> memberInfo;
    for (int ai = 0; ai < tr->args.size(); ai++) {
      auto n = t->name == TYPE_TUPLE
                   ? format("item{}", ai + 1)
                   : ctx->cache->classes[t->name].fields[ai].name;
      names.emplace_back(n);
      typeArgs.emplace_back(force_find_ir_type(tr->args[ai]));
      memberInfo[n] =
          t->name == TYPE_TUPLE
              ? tr->get_source_info()
              : ctx->cache->classes[t->name].fields[ai].type->get_source_info();
    }
    auto record = IR::cast<IR::Types::RecordType>(
        module->unsafe_get_membered_type(realized_name));
    record->realize(typeArgs, names);
    handle = record;
    handle->set_attribute(
        std::make_unique<IR::MemberAttribute>(std::move(memberInfo)));
  } else {
    // Type arguments will be populated afterwards to avoid infinite loop with
    // recursive reference types (e.g., `class X: x: Optional[X]`)
    handle = module->unsafe_get_membered_type(realized_name, true);
    if (ctx->cache->classes[t->name].rtti) {
      // LOG("RTTI: {}", t->name);
      IR::cast<IR::Types::RefType>(handle)->set_polymorphic();
    }
  }
  handle->set_source_info(t->get_source_info());
  handle->set_ast_type(
      std::const_pointer_cast<Type::Type>(t->shared_from_this()));
  return ctx->cache->classes[t->name].realizations[realized_name]->ir = handle;
}

/// Make IR node for a realized function.
IR::Func* TypecheckVisitor::make_ir_function(
    const std::shared_ptr<Cache::Function::FunctionRealization>& r) {
  IR::Func* fn = nullptr;
  // Create and store a function IR node and a realized AST for IR passes
  if (r->ast->attributes.has(Attr::Internal)) {
    // e.g., __new__, Ptr.__new__, etc.
    fn = ctx->cache->module->Nr<IR::InternalFunc>(r->type->ast->name);
  } else if (r->ast->attributes.has(Attr::LLVM)) {
    fn = ctx->cache->module->Nr<IR::LLVMFunc>(r->type->realized_name());
  } else if (r->ast->attributes.has(Attr::C)) {
    fn = ctx->cache->module->Nr<IR::ExternalFunc>(r->type->realized_name());
  } else {
    fn = ctx->cache->module->Nr<IR::BodiedFunc>(r->type->realized_name());
  }
  fn->set_unmangled_name(ctx->cache->reverse_identifier_lookup[r->type->ast->name]);
  auto parent = r->type->func_parent;
  if (!r->ast->attributes.parent_class.empty() &&
      !r->ast->attributes.has(Attr::Method)) {
    // Hack for non-generic methods
    parent = ctx->find(r->ast->attributes.parent_class)->type;
  }
  if (parent && parent->can_realize()) {
    realize(parent);
    fn->set_parent_type(make_ir_type(parent->get_class().get()));
  }
  fn->set_global();
  // Mark this realization as pending (i.e., realized but not translated)
  ctx->cache->pending_realizations.insert(
      {r->type->ast->name, r->type->realized_name()});

  seqassert(!r->type || r->ast->args.size() == r->type->get_arg_types().size() +
                                                   r->type->func_generics.size(),
            "type/AST argument mismatch");

  // Populate the IR node
  std::vector<std::string> names;
  std::vector<Pud::IR::Types::Type*> types;
  for (size_t i = 0, j = 0; i < r->ast->args.size(); i++) {
    if (r->ast->args[i].status == Param::Normal) {
      if (!r->type->get_arg_types()[j]->get_func()) {
        types.push_back(
            make_ir_type(r->type->get_arg_types()[j]->get_class().get()));
        names.push_back(
            ctx->cache->reverse_identifier_lookup[r->ast->args[i].name]);
      }
      j++;
    }
  }
  if (r->ast->has_attr(Attr::CVarArg)) {
    types.pop_back();
    names.pop_back();
  }
  auto irType = ctx->cache->module->unsafe_get_func_type(
      r->type->realized_name(),
      make_ir_type(r->type->get_ret_type()->get_class().get()), types,
      r->ast->has_attr(Attr::CVarArg));
  irType->set_ast_type(r->type->get_func());
  fn->realize(irType, names);
  return fn;
}

/// Generate ASTs for dynamically generated functions.
std::shared_ptr<FunctionStmt> TypecheckVisitor::generate_special_ast(
    Type::FuncType* type) {
  // Clone the generic AST that is to be realized
  auto ast = std::dynamic_pointer_cast<FunctionStmt>(
      clone(ctx->cache->functions[type->ast->name].ast));

  if (ast->has_attr("autogenerated") && endswith(ast->name, ".__iter__:0") &&
      type->get_arg_types()[0]->get_heterogenous_tuple()) {
    // Special case: do not realize auto-generated heterogenous __iter__
    Err(Error::EXPECTED_TYPE, get_source_info(), "iterable");
  } else if (ast->has_attr("autogenerated") &&
             endswith(ast->name, ".__getitem__:0") &&
             type->get_arg_types()[0]->get_heterogenous_tuple()) {
    // Special case: do not realize auto-generated heterogenous __getitem__
    Err(Error::EXPECTED_TYPE, get_source_info(), "iterable");
  } else if (startswith(ast->name, "Function.__call_internal__")) {
    // Special case: Function.__call_internal__
    /// TODO: move to IR one day
    std::vector<StmtPtr> items;
    items.push_back(nullptr);
    std::vector<std::string> ll;
    std::vector<std::string> lla;
    auto& as = type->get_arg_types()[1]->get_record()->args;
    auto ag = ast->args[1].name;
    trim_stars(ag);
    for (int i = 0; i < as.size(); i++) {
      ll.push_back(format("%{} = extractvalue {{}} %args, {}", i, i));
      items.push_back(N<ExprStmt>(N<IdExpr>(ag)));
    }
    items.push_back(N<ExprStmt>(N<IdExpr>("TR")));
    for (int i = 0; i < as.size(); i++) {
      items.push_back(N<ExprStmt>(N<IndexExpr>(N<IdExpr>(ag), N<IntExpr>(i))));
      lla.push_back(format("{{}} %{}", i));
    }
    items.push_back(N<ExprStmt>(N<IdExpr>("TR")));
    ll.push_back(format("%{} = call {{}} %self({})", as.size(), combine2(lla)));
    ll.push_back(format("ret {{}} %{}", as.size()));
    items[0] = N<ExprStmt>(N<StringExpr>(combine2(ll, "\n")));
    ast->suite = N<SuiteStmt>(items);
  } else if (startswith(ast->name, "Union.__new__:0")) {
    auto unionType = type->func_parent->get_union();
    seqassert(unionType, "expected union, got {}", type->func_parent);

    StmtPtr suite =
        N<ReturnStmt>(N<CallExpr>(N<IdExpr>("__internal__.new_union:0"),
                                  N<IdExpr>(type->ast->args[0].name),
                                  N<IdExpr>(unionType->realized_type_name())));
    ast->suite = suite;
  } else if (startswith(ast->name, "__internal__.new_union:0")) {
    // Special case: __internal__.new_union
    // def __internal__.new_union(value, U[T0, ..., TN]):
    //   if isinstance(value, T0):
    //     return __internal__.union_make(0, value, U[T0, ..., TN])
    //   if isinstance(value, Union[T0]):
    //     return __internal__.union_make(
    //       0, __internal__.get_union(value, T0), U[T0, ..., TN])
    //   ... <for all T0...TN> ...
    //   compile_error("invalid union constructor")
    auto unionType = type->func_generics[0].type->get_union();
    auto unionTypes = unionType->get_realization_types();

    auto objVar = ast->args[0].name;
    auto suite = N<SuiteStmt>();
    int tag = 0;
    for (auto& t : unionTypes) {
      suite->stmts.push_back(N<IfStmt>(
          N<CallExpr>(N<IdExpr>("isinstance"), N<IdExpr>(objVar),
                      NT<IdExpr>(t->realized_name())),
          N<ReturnStmt>(N<CallExpr>(
              N<IdExpr>("__internal__.union_make:0"), N<IntExpr>(tag),
              N<IdExpr>(objVar), N<IdExpr>(unionType->realized_type_name())))));
      // Check for Union[T]
      suite->stmts.push_back(N<IfStmt>(
          N<CallExpr>(N<IdExpr>("isinstance"), N<IdExpr>(objVar),
                      NT<InstantiateExpr>(
                          NT<IdExpr>("Union"),
                          std::vector<ExprPtr>{NT<IdExpr>(t->realized_name())})),
          N<ReturnStmt>(N<CallExpr>(
              N<IdExpr>("__internal__.union_make:0"), N<IntExpr>(tag),
              N<CallExpr>(N<IdExpr>("__internal__.get_union:0"),
                          N<IdExpr>(objVar), NT<IdExpr>(t->realized_name())),
              N<IdExpr>(unionType->realized_type_name())))));
      tag++;
    }
    suite->stmts.push_back(
        N<ExprStmt>(N<CallExpr>(N<IdExpr>("compile_error"),
                                N<StringExpr>("invalid union constructor"))));
    ast->suite = suite;
  } else if (startswith(ast->name, "__internal__.get_union:0")) {
    // Special case: __internal__.get_union
    // def __internal__.new_union(union: Union[T0,...,TN], T):
    //   if __internal__.union_get_tag(union) == 0:
    //     return __internal__.union_get_data(union, T0)
    //   ... <for all T0...TN>
    //   raise TypeError("getter")
    auto unionType = type->get_arg_types()[0]->get_union();
    auto unionTypes = unionType->get_realization_types();

    auto target_type = type->func_generics[0].type;
    auto selfVar = ast->args[0].name;
    auto suite = N<SuiteStmt>();
    int tag = 0;
    for (auto t : unionTypes) {
      if (t->realized_name() == target_type->realized_name()) {
        suite->stmts.push_back(N<IfStmt>(
            N<BinaryExpr>(N<CallExpr>(N<IdExpr>("__internal__.union_get_tag:0"),
                                      N<IdExpr>(selfVar)),
                          "==", N<IntExpr>(tag)),
            N<ReturnStmt>(N<CallExpr>(
                N<IdExpr>("__internal__.union_get_data:0"), N<IdExpr>(selfVar),
                NT<IdExpr>(t->realized_name())))));
      }
      tag++;
    }
    suite->stmts.push_back(N<ThrowStmt>(
        N<CallExpr>(N<IdExpr>("std.internal.types.error.TypeError"),
                    N<StringExpr>("invalid union getter"))));
    ast->suite = suite;
  } else if (startswith(ast->name, "__internal__._get_union_method:0")) {
    // def __internal__._get_union_method(union: Union[T0,...,TN], method,
    // *args, **kw):
    //   if __internal__.union_get_tag(union) == 0:
    //     return __internal__.union_get_data(union, T0).method(*args, **kw)
    //   ... <for all T0...TN>
    //   raise TypeError("call")
    auto szt = type->func_generics[0].type->get_static();
    auto fnName = szt->evaluate().get_string();
    auto unionType = type->get_arg_types()[0]->get_union();
    auto unionTypes = unionType->get_realization_types();

    auto selfVar = ast->args[0].name;
    auto suite = N<SuiteStmt>();
    int tag = 0;
    for (auto& t : unionTypes) {
      auto callee = N<DotExpr>(
          N<CallExpr>(N<IdExpr>("__internal__.union_get_data:0"),
                      N<IdExpr>(selfVar), NT<IdExpr>(t->realized_name())),
          fnName);
      auto args = N<StarExpr>(N<IdExpr>(ast->args[2].name.substr(1)));
      auto kwargs = N<KeywordStarExpr>(N<IdExpr>(ast->args[3].name.substr(2)));
      std::vector<CallExpr::Arg> callArgs;
      ExprPtr check =
          N<CallExpr>(N<IdExpr>("hasattr"), NT<IdExpr>(t->realized_name()),
                      N<StringExpr>(fnName), args->clone(), kwargs->clone());
      suite->stmts.push_back(N<IfStmt>(
          N<BinaryExpr>(
              check, "&&",
              N<BinaryExpr>(
                  N<CallExpr>(N<IdExpr>("__internal__.union_get_tag:0"),
                              N<IdExpr>(selfVar)),
                  "==", N<IntExpr>(tag))),
          N<SuiteStmt>(N<ReturnStmt>(N<CallExpr>(callee, args, kwargs)))));
      tag++;
    }
    suite->stmts.push_back(N<ThrowStmt>(
        N<CallExpr>(N<IdExpr>("std.internal.types.error.TypeError"),
                    N<StringExpr>("invalid union call"))));
    // suite->stmts.push_back(N<ReturnStmt>(N<NoneExpr>()));

    auto ret = ctx->instantiate(ctx->get_type("Union"));
    unify(type->get_ret_type(), ret);
    ast->suite = suite;
  } else if (startswith(ast->name, "__internal__.get_union_first:0")) {
    // def __internal__.get_union_first(union: Union[T0]):
    //   return __internal__.union_get_data(union, T0)
    auto unionType = type->get_arg_types()[0]->get_union();
    auto unionTypes = unionType->get_realization_types();

    auto selfVar = ast->args[0].name;
    auto suite = N<SuiteStmt>(N<ReturnStmt>(N<CallExpr>(
        N<IdExpr>("__internal__.union_get_data:0"), N<IdExpr>(selfVar),
        NT<IdExpr>(unionTypes[0]->realized_name()))));
    ast->suite = suite;
  }
  return ast;
}

}  // namespace Pud::AST