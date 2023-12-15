#include <memory>
#include <string>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/TypeCheck/TypeCheck.h"

namespace Pud::Type {

UnionType::UnionType(AST::Cache* cache) : RecordType(cache, "Union", "Union") {
  for (size_t i = 0; i < 256; i++) {
    pending_types.emplace_back(
        std::make_shared<LinkType>(cache, LinkType::Generic, i, 0, nullptr));
  }
}

UnionType::UnionType(AST::Cache* cache,
                     const std::vector<ClassType::Generic>& generics,
                     const std::vector<TypePtr>& pending_types)
    : RecordType(cache, "Union", "Union", generics),
      pending_types(pending_types) {}

auto UnionType::unify(Type* typ, Unification* undo) -> int {
  if (typ->get_union()) {
    auto tr = typ->get_union();
    if (!is_sealed() && !tr->is_sealed()) {
      for (size_t i = 0; i < pending_types.size(); i++) {
        if (pending_types[i]->unify(tr->pending_types[i].get(), undo) == -1) {
          return -1;
        }
      }
      return RecordType::unify(typ, undo);
    } else if (!is_sealed()) {
      return tr->unify(this, undo);
    } else if (!tr->is_sealed()) {
      if (tr->pending_types[0]->get_link() &&
          tr->pending_types[0]->get_link()->kind == LinkType::Unbound) {
        return RecordType::unify(tr.get(), undo);
      }
      return -1;
    }
    if (!can_realize() || !tr->can_realize()) {
      return 0;
    }

    auto u1 = get_realization_types();
    auto u2 = tr->get_realization_types();
    if (u1.size() != u2.size()) {
      return -1;
    }
    int s1 = 2;
    int s = 0;
    for (size_t i = 0; i < u1.size(); i++) {
      s = u1[i]->unify(u2[i].get(), undo);
      if (s == -1) {
        return -1;
      }
      s1 += s;
    }
    return s1;
  } else if (auto tl = typ->get_link()) {
    return tl->unify(this, undo);
  }
  return -1;
}

auto UnionType::generalize(int at_level) -> TypePtr {
  auto r = RecordType::generalize(at_level);
  auto p = pending_types;
  for (auto& t : p)
    t = t->generalize(at_level);
  auto t = std::make_shared<UnionType>(cache, r->get_class()->generics, p);
  t->set_source_info(get_source_info());
  return t;
}

auto UnionType::instantiate(int at_level, int* unbound_count,
                            std::unordered_map<int, TypePtr>* cache)
    -> TypePtr {
  auto r = RecordType::instantiate(at_level, unbound_count, cache);
  auto p = pending_types;
  for (auto& t : p)
    t = t->instantiate(at_level, unbound_count, cache);
  auto t =
      std::make_shared<UnionType>(this->cache, r->get_class()->generics, p);
  t->set_source_info(get_source_info());
  return t;
}

auto UnionType::debug_string(char mode) const -> std::string {
  if (mode == 2) {
    return this->RecordType::debug_string(mode);
  }
  if (!generics[0].type->get_record()) {
    return this->RecordType::debug_string(mode);
  }

  std::set<std::string> gss;
  for (auto& a : generics[0].type->get_record()->args) {
    gss.insert(a->debug_string(mode));
  }
  std::string s;
  for (auto& i : gss) {
    s += "," + i;
  }
  return fmt::format("{}{}", name,
                     s.empty() ? "" : fmt::format("[{}]", s.substr(1)));
}

auto UnionType::can_realize() const -> bool {
  return is_sealed() && RecordType::can_realize();
}

auto UnionType::realized_name() const -> std::string {
  // seqassert(can_realize(), "cannot realize {}", to_string());
  std::set<std::string> gss;
  for (auto& a : generics[0].type->get_record()->args)
    gss.insert(a->realized_name());
  std::string s;
  for (auto& i : gss)
    s += "," + i;
  return fmt::format("{}{}", name,
                     s.empty() ? "" : fmt::format("[{}]", s.substr(1)));
}

auto UnionType::realized_type_name() const -> std::string {
  return realized_name();
}

void UnionType::add_type(TypePtr typ) {
  // seqassert(!is_sealed(), "union already sealed");
  if (this == typ.get())
    return;
  if (auto tu = typ->get_union()) {
    if (tu->is_sealed()) {
      for (auto& t : tu->generics[0].type->get_record()->args)
        add_type(t);
    } else {
      for (auto& t : tu->pending_types) {
        if (t->get_link() && t->get_link()->kind == LinkType::Unbound)
          break;
        else
          add_type(t);
      }
    }
  } else {
    // Find first pending generic to which we can attach this!
    Unification us;
    for (auto& t : pending_types)
      if (auto l = t->get_link()) {
        if (l->kind == LinkType::Unbound) {
          t->unify(typ.get(), &us);
          return;
        }
      }
    Err(Error::UNION_TOO_BIG, this);
  }
}

auto UnionType::is_sealed() const -> bool {
  return generics[0].type->get_record() != nullptr;
}

void UnionType::seal() {
  // seqassert(!is_sealed(), "union already sealed");
  auto tv = TypecheckVisitor(cache->type_ctx);

  size_t i;
  for (i = 0; i < pending_types.size(); i++)
    if (pending_types[i]->get_link() &&
        pending_types[i]->get_link()->kind == LinkType::Unbound)
      break;
  std::vector<TypePtr> type_set(pending_types.begin(),
                                pending_types.begin() + i);
  auto t = cache->type_ctx->instantiate_tuple(type_set);
  Unification us;
  generics[0].type->unify(t.get(), &us);
}

auto UnionType::get_realization_types() -> std::vector<Pud::Type::TypePtr> {
  // seqassert(can_realize(), "cannot realize {}", debug_string(1));
  std::map<std::string, Pud::Type::TypePtr> union_types;
  for (auto& u : generics[0].type->get_record()->args)
    union_types[u->realized_name()] = u;
  std::vector<Pud::Type::TypePtr> r;
  r.reserve(union_types.size());
  for (auto& [_, t] : union_types)
    r.emplace_back(t);
  return r;
}

}  // namespace Pud::Type