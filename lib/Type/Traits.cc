#include "Pud/Type/Traits.h"

#include <memory>
#include <string>
#include <vector>

#include "Pud/AST/Cache.h"
#include "Pud/TypeCheck/TypeCheck.h"

namespace Pud::Type {

Trait::Trait(const std::shared_ptr<Type>& type) : Type(type) {}

Trait::Trait(AST::Cache* cache) : Type(cache) {}

auto Trait::can_realize() const -> bool { return false; }

auto Trait::is_instantiated() const -> bool { return false; }

auto Trait::realized_name() const -> std::string { return ""; }

CallableTrait::CallableTrait(AST::Cache* cache, std::vector<TypePtr> args)
    : Trait(cache), args(std::move(args)) {}

auto CallableTrait::unify(Type* typ, Unification* undo) -> int {
  if (auto tr = typ->get_record()) {
    // 如果 typ 是 NoneType，则认为是兼容的
    if (tr->name == "NoneType") {
      return 1;
    }
    // 如果 typ 不是 Function 类型且不是部分类型（PartialType），则不兼容
    if (tr->name != "Function" && !tr->get_partial()) {
      return -1;
    }
    // 如果 args 为空（即没有参数类型），则认为是兼容的
    if (args.empty()) {
      return 1;
    }

    std::vector<char> known;
    auto tr_fun = tr;
    // 如果 typ 是部分类型，则获取该部分类型的已知参数并实例化其函数类型
    if (auto pt = tr->get_partial()) {
      int ic = 0;
      std::unordered_map<int, TypePtr> c;
      tr_fun = pt->func->instantiate(0, &ic, &c)->get_record();
      known = pt->known;
    } else {
      known =
          std::vector<char>(tr->generics[0].type->get_record()->args.size(), 0);
    }

    // 获取内部参数类型列表 (in_args) 和待比较类型的参数类型列表 (tr_in_args)。
    auto& in_args = args[0]->get_record()->args;
    auto& tr_in_args = tr_fun->generics[0].type->get_record()->args;
    auto* tr_ast = tr_fun->get_func() ? tr_fun->get_func()->ast : nullptr;
    size_t star = tr_in_args.size();
    size_t kw_star = tr_in_args.size();
    size_t total = 0;
    if (tr_ast) {
      // 如果有 *args 或 **kwargs，计算总参数数量和星号位置。
      star = tr_ast->get_star_args();
      kw_star = tr_ast->get_kw_star_args();
      if (kw_star < tr_ast->args.size() && star >= tr_in_args.size()) {
        star -= 1;
      }
      size_t pre_star = 0;
      for (size_t fi = 0; fi < tr_ast->args.size(); fi++) {
        if (fi != kw_star && !known[fi] &&
            tr_ast->args[fi].status == AST::Param::Normal) {
          total++;
          if (fi < star) {
            pre_star++;
          }
        }
      }
      if (pre_star < total) {
        if (in_args.size() < pre_star) {
          return -1;
        }
      } else if (in_args.size() != total) {
        return -1;
      }
    } else {
      total = star = tr_in_args.size();
      if (in_args.size() != total) {
        return -1;
      }
    }
    // 比较每个已知参数类型，使用递归调用 unify。
    size_t i = 0;
    for (size_t fi = 0; i < in_args.size() && fi < star; fi++) {
      if (!known[fi] && tr_ast->args[fi].status == AST::Param::Normal) {
        if (in_args[i++]->unify(tr_in_args[fi].get(), undo) == -1) {
          return -1;
        }
      }
    }
    if (auto pf = tr_fun->get_func()) {
      if (star < tr_in_args.size() - (kw_star < tr_in_args.size())) {
        std::vector<TypePtr> star_arg_types;
        if (auto tp = tr->get_partial()) {
          auto ts = tp->args[tp->args.size() - 2]->get_record();
          // seqassert(ts, "bad partial *args/**kwargs");
          star_arg_types = ts->args;
        }
        star_arg_types.insert(star_arg_types.end(), in_args.begin() + i,
                              in_args.end());

        auto tv = TypecheckVisitor(cache->type_ctx);
        auto t =
            cache->type_ctx->instantiate_tuple(star_arg_types)->get_class();
        if (t->unify(tr_in_args[star].get(), undo) == -1) {
          return -1;
        }
      }
      if (kw_star < tr_in_args.size()) {
        auto tv = TypecheckVisitor(cache->type_ctx);
        std::vector<std::string> names;
        std::vector<TypePtr> star_arg_types;
        if (auto tp = tr->get_partial()) {
          auto ts = tp->args.back()->get_record();
          // seqassert(ts, "bad partial *args/**kwargs");
          auto ff = tv.get_class_fields(ts.get());
          for (size_t i = 0; i < ts->args.size(); i++) {
            names.emplace_back(ff[i].name);
            star_arg_types.emplace_back(ts->args[i]);
          }
        }
        auto name =
            tv.generate_tuple(star_arg_types.size(), TYPE_KWTUPLE, names);
        auto t = cache->type_ctx->force_find(name)->type;
        t = cache->type_ctx->instantiate_generic(t, star_arg_types)
                ->get_class();
        if (t->unify(tr_in_args[kw_star].get(), undo) == -1) {
          return -1;
        }
      }

      if (undo && pf->can_realize()) {
        auto rf = TypecheckVisitor(cache->type_ctx).realize(pf);
        pf->unify(rf.get(), undo);
      }
      if (args[1]->unify(pf->get_ret_type().get(), undo) == -1) {
        return -1;
      }
    }
    return 1;
  } else if (auto tl = typ->get_link()) {
    if (tl->kind == LinkType::Link) {
      return unify(tl->type.get(), undo);
    }
    if (tl->kind == LinkType::Unbound) {
      if (tl->trait) {
        auto* tt = dynamic_cast<CallableTrait*>(tl->trait.get());
        if (!tt || tt->args.size() != args.size()) {
          return -1;
        }
        for (int i = 0; i < args.size(); i++) {
          if (args[i]->unify(tt->args[i].get(), undo) == -1) {
            return -1;
          }
        }
      }
      return 1;
    }
  }
  return -1;
}

auto CallableTrait::generalize(int at_level) -> TypePtr {
  auto g = args;
  for (auto& t : g) {
    t = t ? t->generalize(at_level) : nullptr;
  }
  auto c = std::make_shared<CallableTrait>(cache, g);
  c->set_source_info(get_source_info());
  return c;
}

auto CallableTrait::instantiate(int at_level, int* unbound_count,
                                std::unordered_map<int, TypePtr>* cache)
    -> TypePtr {
  auto g = args;
  for (auto& t : g) {
    t = t ? t->instantiate(at_level, unbound_count, cache) : nullptr;
  }
  auto c = std::make_shared<CallableTrait>(this->cache, g);
  c->set_source_info(get_source_info());
  return c;
}

auto CallableTrait::debug_string(char mode) const -> std::string {
  auto s = args[0]->debug_string(mode);
  return fmt::format("Callable[{},{}]",
                     startswith(s, "Tuple") ? s.substr(5) : s,
                     args[1]->debug_string(mode));
}

TypeTrait::TypeTrait(TypePtr typ) : Trait(typ), type(std::move(typ)) {}

auto TypeTrait::unify(Type* typ, Unification* undo) -> int {
  return typ->unify(type.get(), undo);
}

auto TypeTrait::generalize(int at_level) -> TypePtr {
  auto c = std::make_shared<TypeTrait>(type->generalize(at_level));
  c->set_source_info(get_source_info());
  return c;
}

auto TypeTrait::instantiate(int at_level, int* unbound_count,
                            std::unordered_map<int, TypePtr>* cache)
    -> TypePtr {
  auto c = std::make_shared<TypeTrait>(
      type->instantiate(at_level, unbound_count, cache));
  c->set_source_info(get_source_info());
  return c;
}

auto TypeTrait::debug_string(char mode) const -> std::string {
  return fmt::format("Trait[{}]", type->debug_string(mode));
}

}  // namespace Pud::Type