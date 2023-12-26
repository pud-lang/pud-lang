#include "Pud/TypeCheck/Context.h"

#include <map>
#include <memory>
#include <unordered_map>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/Common/Common.h"
#include "Pud/Format/Format.h"
#include "Pud/Simplify/Context.h"
#include "Pud/TypeCheck/TypeCheck.h"

namespace Pud::AST {

TypeContext::TypeContext(Cache* cache)
    : Context<TypecheckItem>(""),
      cache(cache),
      typecheck_level(0),
      age(0),
      block_level(0),
      return_early(false),
      changed_nodes(0) {
  realization_bases.push_back({"", nullptr, nullptr});
  push_source_info(
      cache->generate_source_info());  // Always have source_info() around
}

auto TypeContext::add(TypecheckItem::Kind kind, const std::string& name,
                      const Pud::Type::TypePtr& type)
    -> std::shared_ptr<TypecheckItem> {
  auto t = std::make_shared<TypecheckItem>(kind, type);
  add(name, t);
  return t;
}

auto TypeContext::find(const std::string& name) const
    -> std::shared_ptr<TypecheckItem> {
  if (auto t = Context<TypecheckItem>::find(name))
    return t;
  if (in(cache->globals, name))
    return std::make_shared<TypecheckItem>(TypecheckItem::Var, get_unbound());
  return nullptr;
}

auto TypeContext::force_find(const std::string& name) const
    -> std::shared_ptr<TypecheckItem> {
  auto t = find(name);
  // seqassert(t, "cannot find '{}'", name);
  return t;
}

auto TypeContext::get_type(const std::string& name) const
    -> Pud::Type::TypePtr {
  return force_find(name)->type;
}

auto TypeContext::get_realization_base() -> TypeContext::RealizationBase* {
  return &(realization_bases.back());
}

auto TypeContext::get_realization_depth() const -> size_t {
  return realization_bases.size();
}

auto TypeContext::get_realization_stack_name() const -> std::string {
  if (realization_bases.empty())
    return "";
  std::vector<std::string> s;
  for (auto& b : realization_bases)
    if (b.type)
      s.push_back(b.type->realized_name());
  return join(s, ":");
}

auto TypeContext::get_unbound(const SourceInfo& source_info, int level) const
    -> std::shared_ptr<Pud::Type::LinkType> {
  auto typ = std::make_shared<Pud::Type::LinkType>(
      cache, Pud::Type::LinkType::Unbound, cache->unbound_count++, level,
      nullptr);
  typ->set_source_info(source_info);
  return typ;
}

auto TypeContext::get_unbound(const SourceInfo& source_info) const
    -> std::shared_ptr<Pud::Type::LinkType> {
  return get_unbound(source_info, typecheck_level);
}

auto TypeContext::get_unbound() const -> std::shared_ptr<Pud::Type::LinkType> {
  return get_unbound(get_source_info(), typecheck_level);
}

auto TypeContext::instantiate(const SourceInfo& source_info,
                              const Pud::Type::TypePtr& type,
                              const Pud::Type::ClassTypePtr& generics)
    -> Pud::Type::TypePtr {
  // seqassert(type, "type is null");
  std::unordered_map<int, Pud::Type::TypePtr> generic_cache;
  if (generics) {
    for (auto& g : generics->generics) {
      if (g.type && !(g.type->get_link() && g.type->get_link()->kind ==
                                                Pud::Type::LinkType::Generic)) {
        generic_cache[g.id] = g.type;
      }
    }
  }
  auto t = type->instantiate(typecheck_level, &(cache->unbound_count),
                             &generic_cache);
  for (auto& i : generic_cache) {
    if (auto l = i.second->get_link()) {
      i.second->set_source_info(source_info);
      if (l->default_type) {
        get_realization_base()->pending_defaults.insert(i.second);
      }
    }
  }
  if (t->get_union() && !t->get_union()->is_sealed()) {
    t->set_source_info(source_info);
    get_realization_base()->pending_defaults.insert(t);
  }
  if (auto r = t->get_record())
    if (r->repeats && r->repeats->can_realize())
      r->flatten();
  return t;
}

auto TypeContext::instantiate_generic(
    const SourceInfo& source_info, const Pud::Type::TypePtr& root,
    const std::vector<Pud::Type::TypePtr>& generics) -> Pud::Type::TypePtr {
  auto c = root->get_class();
  // seqassert(c, "root class is null");
  // dummy generic type
  auto g = std::make_shared<Pud::Type::ClassType>(cache, "", "");
  if (generics.size() != c->generics.size()) {
    Err(Error::GENERICS_MISMATCH, source_info, cache->rev(c->name),
        c->generics.size(), generics.size());
  }
  for (int i = 0; i < c->generics.size(); i++) {
    // seqassert(c->generics[i].type, "generic is null");
    g->generics.emplace_back("", "", generics[i], c->generics[i].id);
  }
  return instantiate(source_info, root, g);
}

auto TypeContext::instantiate_tuple(
    const SourceInfo& source_info,
    const std::vector<Pud::Type::TypePtr>& generics)
    -> std::shared_ptr<Pud::Type::RecordType> {
  auto key = generate_tuple(generics.size());
  auto root = force_find(key)->type->get_record();
  return instantiate_generic(source_info, root, generics)->get_record();
}

auto TypeContext::generate_tuple(size_t n) -> std::string {
  auto key = fmt::format("_{}:{}", TYPE_TUPLE, n);
  if (!in(cache->classes, key)) {
    cache->classes[key].fields.clear();
    cache->classes[key].ast = std::static_pointer_cast<ClassStmt>(
        clone(cache->classes[TYPE_TUPLE].ast));
    auto root =
        std::make_shared<Pud::Type::RecordType>(cache, TYPE_TUPLE, TYPE_TUPLE);
    for (size_t i = 0; i < n; i++) {  // generate unique ID
      auto g = get_unbound()->get_link();
      g->kind = Pud::Type::LinkType::Generic;
      g->generic_name = fmt::format("T{}", i + 1);
      auto gn = cache->imports[MAIN_IMPORT].ctx->generate_canonical_name(
          g->generic_name);
      root->generics.emplace_back(gn, g->generic_name, g, g->id);
      root->args.emplace_back(g);
      cache->classes[key].ast->args.emplace_back(
          g->generic_name, std::make_shared<IdExpr>("type"), nullptr,
          Param::Generic);
      cache->classes[key].fields.push_back(
          Cache::Class::ClassField{fmt::format("item{}", i + 1), g, ""});
    }
    std::vector<ExprPtr> e_type_args;
    for (size_t i = 0; i < n; i++)
      e_type_args.push_back(
          std::make_shared<IdExpr>(fmt::format("T{}", i + 1)));
    auto eType = std::make_shared<InstantiateExpr>(
        std::make_shared<IdExpr>(TYPE_TUPLE), e_type_args);
    eType->type = root;
    cache->classes[key].mro = {eType};
    add_toplevel(key,
                 std::make_shared<TypecheckItem>(TypecheckItem::Type, root));
  }
  return key;
}

auto TypeContext::instantiate_tuple(size_t n)
    -> std::shared_ptr<Pud::Type::RecordType> {
  std::vector<Pud::Type::TypePtr> t(n);
  for (size_t i = 0; i < n; i++) {
    auto g = get_unbound()->get_link();
    g->generic_name = fmt::format("T{}", i + 1);
    t[i] = g;
  }
  return instantiate_tuple(get_source_info(), t);
}

auto TypeContext::find_method(Pud::Type::ClassType* type,
                              const std::string& method, bool hide_shadowed)
    -> std::vector<Pud::Type::FuncTypePtr> {
  auto type_name = type->name;
  if (type->is(TYPE_TUPLE)) {
    auto sz = type->get_record()->get_repeats();
    if (sz != -1)
      type->get_record()->flatten();
    sz = int64_t(type->get_record()->args.size());
    type_name = fmt::format("_{}:{}", TYPE_TUPLE, sz);
    if (in(cache->classes[TYPE_TUPLE].methods, method) &&
        !in(cache->classes[type_name].methods, method)) {
      auto type = force_find(type_name)->type;

      cache->classes[type_name].methods[method] =
          cache->classes[TYPE_TUPLE].methods[method];
      auto& o = cache->overloads[cache->classes[type_name].methods[method]];
      auto f = cache->functions[o[0].name];
      f.realizations.clear();

      // seqassert(f.type, "tuple fn type not yet set");
      f.ast->attributes.parent_class = type_name;
      f.ast = std::static_pointer_cast<FunctionStmt>(clone(f.ast));
      f.ast->name = fmt::format(
          "{}{}", f.ast->name.substr(0, f.ast->name.size() - 1), sz);
      f.ast->attributes.set(Attr::Method);

      auto e_type = clone(cache->classes[type_name].mro[0]);
      e_type->type = nullptr;
      for (auto& a : f.ast->args)
        if (a.type && a.type->is_id(TYPE_TUPLE)) {
          a.type = e_type;
        }
      if (f.ast->ret && f.ast->ret->is_id(TYPE_TUPLE))
        f.ast->ret = e_type;
      // TODO: resurrect Tuple[N].__new__(defaults...)
      if (method == "__new__") {
        for (size_t i = 0; i < sz; i++) {
          auto n = fmt::format("item{}", i + 1);
          f.ast->args.emplace_back(
              cache->imports[MAIN_IMPORT].ctx->generate_canonical_name(n),
              std::make_shared<IdExpr>(fmt::format("T{}", i + 1))
              // std::make_shared<CallExpr>(
              // std::make_shared<IdExpr>(format("T{}", i + 1)))
          );
        }
      }
      cache->reverse_identifier_lookup[f.ast->name] = method;
      cache->functions[f.ast->name] = f;
      cache->functions[f.ast->name].type =
          TypecheckVisitor(cache->type_ctx).make_function_type(f.ast.get());
      add_toplevel(f.ast->name, std::make_shared<TypecheckItem>(
                                    TypecheckItem::Func,
                                    cache->functions[f.ast->name].type));
      o.push_back(Cache::Overload{f.ast->name, 0});
    }
  }

  std::vector<Pud::Type::FuncTypePtr> vv;
  std::unordered_set<std::string> signature_loci;

  auto populate = [&](const auto& cls) {
    auto t = in(cls.methods, method);
    if (!t)
      return;
    auto mt = cache->overloads[*t];
    for (int mti = int(mt.size()) - 1; mti >= 0; mti--) {
      auto& method = mt[mti];
      if (endswith(method.name, ":dispatch") ||
          !cache->functions[method.name].type)
        continue;
      if (method.age <= age) {
        if (hide_shadowed) {
          auto sig = cache->functions[method.name].ast->signature();
          if (!in(signature_loci, sig)) {
            signature_loci.insert(sig);
            vv.emplace_back(cache->functions[method.name].type);
          }
        } else {
          vv.emplace_back(cache->functions[method.name].type);
        }
      }
    }
  };
  if (auto cls = in(cache->classes, type_name)) {
    for (auto& pt : cls->mro) {
      if (auto pc = pt->type->get_class()) {
        auto mc = in(cache->classes, pc->name);
        // seqassert(mc, "class '{}' not found", pc->name);
        populate(*mc);
      }
    }
  }
  return vv;
}

auto TypeContext::find_member(const Pud::Type::ClassTypePtr& type,
                              const std::string& member) const
    -> Pud::Type::TypePtr {
  if (type->is(TYPE_TUPLE)) {
    if (!startswith(member, "item") || member.size() < 5)
      return nullptr;
    int id = 0;
    for (int i = 4; i < member.size(); i++) {
      if (member[i] >= '0' + (i == 4) && member[i] <= '9')
        id = id * 10 + member[i] - '0';
      else
        return nullptr;
    }
    auto sz = type->get_record()->get_repeats();
    if (sz != -1)
      type->get_record()->flatten();
    if (id < 1 || id > type->get_record()->args.size())
      return nullptr;
    return type->get_record()->args[id - 1];
  }
  if (auto cls = in(cache->classes, type->name)) {
    for (auto& pt : cls->mro) {
      if (auto pc = pt->type->get_class()) {
        auto mc = in(cache->classes, pc->name);
        // seqassert(mc, "class '{}' not found", pc->name);
        for (auto& mm : mc->fields) {
          if (mm.name == member)
            return mm.type;
        }
      }
    }
  }
  return nullptr;
}

auto TypeContext::reorder_named_args(Pud::Type::FuncType* func,
                                     const std::vector<CallExpr::Arg>& args,
                                     const ReorderDoneFn& on_done,
                                     const ReorderErrorFn& on_error,
                                     const std::vector<char>& known) -> int {
  // See https://docs.python.org/3.6/reference/expressions.html#calls for
  // details. Final score:
  //  - +1 for each matched argument
  //  -  0 for *args/**kwargs/default arguments
  //  - -1 for failed match
  int score = 0;

  // 0. Find *args and **kwargs
  // True if there is a trailing ellipsis (full partial: fn(all_args, ...))
  bool partial =
      !args.empty() && args.back().value->get_ellipsis() &&
      args.back().value->get_ellipsis()->mode != EllipsisExpr::PIPE &&
      args.back().name.empty();

  int star_arg_index = -1;
  int kwstar_arg_index = -1;
  for (int i = 0; i < func->ast->args.size(); i++) {
    if (startswith(func->ast->args[i].name, "**"))
      kwstar_arg_index = i, score -= 2;
    else if (startswith(func->ast->args[i].name, "*"))
      star_arg_index = i, score -= 2;
  }

  // 1. Assign positional arguments to slots
  // Each slot contains a list of arg's indices
  std::vector<std::vector<int>> slots(func->ast->args.size());
  // seqassert(known.empty() || func->ast->args.size() == known.size(),
  //          "bad 'known' string");
  std::vector<int> extra;
  std::map<std::string, int> named_args;
  std::map<std::string, int>
      extra_named_args;  // keep the map--- we need it sorted!
  for (int ai = 0, si = 0; ai < args.size() - partial; ai++) {
    if (args[ai].name.empty()) {
      while (!known.empty() && si < slots.size() && known[si])
        si++;
      if (si < slots.size() && (star_arg_index == -1 || si < star_arg_index))
        slots[si++] = {ai};
      else
        extra.emplace_back(ai);
    } else {
      named_args[args[ai].name] = ai;
    }
  }
  score += 2 * int(slots.size() - func->func_generics.size());

  for (auto ai : std::vector<int>{std::max(star_arg_index, kwstar_arg_index),
                                  std::min(star_arg_index, kwstar_arg_index)})
    if (ai != -1 && !slots[ai].empty()) {
      extra.insert(extra.begin(), ai);
      slots[ai].clear();
    }

  // 2. Assign named arguments to slots
  if (!named_args.empty()) {
    std::map<std::string, int> slot_names;
    for (int i = 0; i < func->ast->args.size(); i++)
      if (known.empty() || !known[i]) {
        slot_names[cache->reverse_identifier_lookup[func->ast->args[i].name]] =
            i;
      }
    for (auto& n : named_args) {
      if (!in(slot_names, n.first))
        extra_named_args[n.first] = n.second;
      else if (slots[slot_names[n.first]].empty())
        slots[slot_names[n.first]].push_back(n.second);
      else
        return on_error(Error::CALL_REPEATED_NAME,
                        args[n.second].value->get_source_info(),
                        Emsg(Error::CALL_REPEATED_NAME, n.first));
    }
  }

  // 3. Fill in *args, if present
  if (!extra.empty() && star_arg_index == -1)
    return on_error(Error::CALL_ARGS_MANY, get_source_info(),
                    Emsg(Error::CALL_ARGS_MANY, cache->rev(func->ast->name),
                         func->ast->args.size(), args.size() - partial));

  if (star_arg_index != -1)
    slots[star_arg_index] = extra;

  // 4. Fill in **kwargs, if present
  if (!extra_named_args.empty() && kwstar_arg_index == -1)
    return on_error(
        Error::CALL_ARGS_INVALID,
        args[extra_named_args.begin()->second].value->get_source_info(),
        Emsg(Error::CALL_ARGS_INVALID, extra_named_args.begin()->first,
             cache->rev(func->ast->name)));
  if (kwstar_arg_index != -1)
    for (auto& e : extra_named_args)
      slots[kwstar_arg_index].push_back(e.second);

  // 5. Fill in the default arguments
  for (auto i = 0; i < func->ast->args.size(); i++)
    if (slots[i].empty() && i != star_arg_index && i != kwstar_arg_index) {
      if (func->ast->args[i].status == Param::Normal &&
          (func->ast->args[i].default_value || (!known.empty() && known[i])))
        score -= 2;
      else if (!partial && func->ast->args[i].status == Param::Normal)
        return on_error(
            Error::CALL_ARGS_MISSING, get_source_info(),
            Emsg(Error::CALL_ARGS_MISSING, cache->rev(func->ast->name),
                 cache->reverse_identifier_lookup[func->ast->args[i].name]));
    }
  auto s = on_done(star_arg_index, kwstar_arg_index, slots, partial);
  return s != -1 ? score + s : -1;
}

void TypeContext::dump(int pad) {
  auto ordered =
      std::map<std::string, decltype(map)::mapped_type>(map.begin(), map.end());
  LOG("base: {}", get_realization_stack_name());
  for (auto& i : ordered) {
    std::string s;
    auto t = i.second.front();
    LOG("{}{:.<25} {}", std::string(pad * 2, ' '), i.first, t->type);
  }
}

auto TypeContext::debug_info() -> std::string {
  return fmt::format("[{}:i{}@{}]", get_realization_base()->name,
                     get_realization_base()->iteration, get_source_info());
}

auto TypeContext::get_function_args(Pud::Type::TypePtr t)
    -> std::shared_ptr<std::pair<std::vector<Pud::Type::TypePtr>,
                                 std::vector<Pud::Type::TypePtr>>> {
  if (!t->get_func())
    return nullptr;
  auto fn = t->get_func();
  auto ret = std::make_shared<std::pair<std::vector<Pud::Type::TypePtr>,
                                        std::vector<Pud::Type::TypePtr>>>();
  for (auto& t : fn->func_generics)
    ret->first.push_back(t.type);
  for (auto& t : fn->generics[0].type->get_record()->args)
    ret->second.push_back(t);
  return ret;
}

auto TypeContext::get_static_string(Pud::Type::TypePtr t)
    -> std::shared_ptr<std::string> {
  if (auto s = t->get_static()) {
    auto r = s->evaluate();
    if (r.type == StaticValue::STRING)
      return std::make_shared<std::string>(r.get_string());
  }
  return nullptr;
}

auto TypeContext::get_static_int(Pud::Type::TypePtr t)
    -> std::shared_ptr<int64_t> {
  if (auto s = t->get_static()) {
    auto r = s->evaluate();
    if (r.type == StaticValue::INT)
      return std::make_shared<int64_t>(r.get_int());
  }
  return nullptr;
}

auto TypeContext::extract_function(Pud::Type::TypePtr t)
    -> Pud::Type::FuncTypePtr {
  if (auto f = t->get_func())
    return f;
  if (auto p = t->get_partial())
    return p->func;
  return nullptr;
}

}  // namespace Pud::AST