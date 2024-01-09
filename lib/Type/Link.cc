#include "Pud/Type/Link.h"

#include <memory>
#include <unordered_map>

#include "Pud/AST/Expr.h"
#include "Pud/Common/Error.h"
#include "Pud/Type/Static.h"
#include "Pud/Type/Type.h"
#include "Pud/TypeCheck/TypeCheck.h"

namespace Pud::Type {

LinkType::LinkType(AST::Cache* cache, Kind kind, int id, int level,
                   TypePtr type, char is_static, std::shared_ptr<Trait> trait,
                   TypePtr default_type, std::string generic_name)
    : Type(cache),
      kind(kind),
      id(id),
      level(level),
      type(std::move(type)),
      is_static(is_static),
      trait(std::move(trait)),
      generic_name(std::move(generic_name)),
      default_type(std::move(default_type)) {
  seqassert((this->type && kind == Link) || (!this->type && kind == Generic) ||
                (!this->type && kind == Unbound),
            "inconsistent link state");
}

LinkType::LinkType(TypePtr type)
    : Type(type),
      kind(Link),
      id(0),
      level(0),
      type(std::move(type)),
      is_static(0),
      trait(nullptr),
      default_type(nullptr) {
  seqassert(this->type, "link to nullptr");
}

auto LinkType::unify(Type* typ, Unification* undo) -> int {
  if (kind == Kind::Link) {
    // 当 LinkType 的 kind 为 Link 时，表示这个 LinkType 实例已经是
    // 一个确定的类型（已经链接到了另一个类型）。在这种情况下，直接
    // 调用链接的类型的 unify 函数来进行统一操作。
    // 这里的思想是遵循已建立的链接，将统一操作委托给链接的实际类型。
    return type->unify(typ, undo);
  } else if (kind == Kind::Generic) {
    // Generic 类型代表泛型类型，这些类型在未指定具体类型参数之前不
    // 能进行统一操作。
    return -1;
  } else {
    // 处理 Unbound 类型的情况。
    // 未绑定类型是一种尚未确定具体类型的占位符。在类型系统中，这些
    // 占位符用于代表那些尚未确定的类型。

    // 检查当前类型和参数 typ 是否都是静态类型或都不是。
    if (is_static_type() != typ->is_static_type()) {
      return -1;
    }
    // 静态类型
    if (auto ts = typ->get_static()) {
      if (ts->expr->get_id()) {
        return unify(ts->generics[0].type.get(), undo);
      }
    }
    // 链接类型
    if (auto t = typ->get_link()) {
      if (t->kind == Kind::Link) {
        // Link 类型，递归调用 typ 的类型的 unify
        return t->type->unify(this, undo);
      } else if (t->kind == Kind::Generic) {
        // 泛型类型不能直接与其他类型统一。
        return -1;
      } else {
        // Unbound 类型，那么根据它们的 id 进行比较和处理。
        if (id == t->id) {
          return 1;
        } else if (id < t->id) {
          // 始终将较新的类型合并到较旧类型中
          return t->unify(this, undo);
        }
      }
    }
    // 防止递归统一
    if (occurs(typ, undo)) {
      return -1;
    }

    // 处理特性和统一的破坏性部分
    // 如果当前 LinkType 有特性（trait），则尝试与 typ 的特性进行统一。
    // 如果统一失败，则整个统一操作失败。
    if (trait && trait->unify(typ, undo) == -1) {
      return -1;
    }

    seqassert(!type,
              "type has been already unified or is in inconsistent state");

    // 当 undo 不为空时，表示需要记录撤销信息。
    if (undo) {
      undo->linked.push_back(this);
      // 更新类型状态
      kind = Kind::Link;
      assert((!typ->get_link() || typ->get_link()->kind != Kind::Unbound ||
              typ->get_link()->id <= id) &&
             "type unification is not consistent");
      type = typ->follow();
      // 如果有特性需要撤销则记录
      if (auto t = type->get_link()) {
        if (trait && t->kind == Kind::Unbound && !t->trait) {
          undo->traits.push_back(t.get());
          t->trait = trait;
        }
      }
    }

    return 0;
  }
}

auto LinkType::generalize(int at_level) -> TypePtr {
  if (kind == Kind::Generic) {
    return shared_from_this();
  } else if (kind == Kind::Unbound) {
    if (level >= at_level) {
      return std::make_shared<LinkType>(
          cache, Kind::Generic, id, 0, nullptr, is_static,
          trait ? std::static_pointer_cast<Trait>(trait->generalize(at_level))
                : nullptr,
          default_type ? default_type->generalize(at_level) : nullptr,
          generic_name);
    } else {
      return shared_from_this();
    }
  } else {
    seqassert(type, "link is null");
    return type->generalize(at_level);
  }
}

auto LinkType::instantiate(int at_level, int* unbound_count,
                           std::unordered_map<int, TypePtr>* cache) -> TypePtr {
  if (kind == Kind::Generic) {
    // 在处理泛型类的多个实例时，如果一个泛型类已经被实例化为特定类型，如
    // GenericClass[int]，那么再次请求同样的实例化应返回相同的对象。
    // 缓存机制提高了效率，避免了重复工作，并保证了类型的一致性。

    if (cache && cache->find(id) != cache->end())
      return (*cache)[id];
    auto t = std::make_shared<LinkType>(
        this->cache, Kind::Unbound, unbound_count ? (*unbound_count)++ : id,
        at_level, nullptr, is_static,
        trait ? std::static_pointer_cast<Trait>(
                    trait->instantiate(at_level, unbound_count, cache))
              : nullptr,
        default_type ? default_type->instantiate(at_level, unbound_count, cache)
                     : nullptr,
        generic_name);
    if (cache)
      (*cache)[id] = t;
    return t;
  } else if (kind == Kind::Unbound) {
    // 在实例化过程中，未绑定的类型表示它仍然是开放的。
    // 保留其未绑定状态允许它在未来被绑定到具体的类型。
    return shared_from_this();
  } else {
    seqassert(type, "link is null");
    return type->instantiate(at_level, unbound_count, cache);
  }
}

auto LinkType::follow() -> TypePtr {
  if (kind == Kind::Link) {
    return type->follow();
  } else {
    return shared_from_this();
  }
}

auto LinkType::get_unbounds() const -> std::vector<TypePtr> {
  if (kind == Kind::Unbound) {
    return {std::const_pointer_cast<Type>(shared_from_this())};
  } else if (kind == Kind::Link) {
    return type->get_unbounds();
  }
  return {};
}

auto LinkType::can_realize() const -> bool {
  if (kind != Kind::Link) {
    return false;
  } else {
    return type->can_realize();
  }
}

auto LinkType::is_instantiated() const -> bool {
  return kind == Kind::Link && type->is_instantiated();
}

auto LinkType::debug_string(char /*mode*/) const -> std::string { return ""; }

auto LinkType::realized_name() const -> std::string {
  if (kind == Unbound || kind == Generic) {
    return "?";
  }
  seqassert(kind == Link, "unexpected generic link");
  return type->realized_name();
}

auto LinkType::get_link() -> std::shared_ptr<LinkType> {
  return std::static_pointer_cast<LinkType>(shared_from_this());
}

auto LinkType::get_func() -> std::shared_ptr<FuncType> {
  return kind == Kind::Link ? type->get_func() : nullptr;
}

auto LinkType::get_partial() -> std::shared_ptr<PartialType> {
  return kind == Kind::Link ? type->get_partial() : nullptr;
}

auto LinkType::get_class() -> std::shared_ptr<ClassType> {
  return kind == Kind::Link ? type->get_class() : nullptr;
}

auto LinkType::get_record() -> std::shared_ptr<RecordType> {
  return kind == Kind::Link ? type->get_record() : nullptr;
}

auto LinkType::get_static() -> std::shared_ptr<StaticType> {
  return kind == Kind::Link ? type->get_static() : nullptr;
}

auto LinkType::get_union() -> std::shared_ptr<UnionType> {
  return kind == Kind::Link ? type->get_union() : nullptr;
}

auto LinkType::get_unbound() -> std::shared_ptr<LinkType> {
  if (kind == Kind::Unbound) {
    return std::static_pointer_cast<LinkType>(shared_from_this());
  }
  if (kind == Kind::Link) {
    return type->get_unbound();
  }
  return nullptr;
}

// 防止递归类型：检查类型变量是否在另一个类型中出现是防止类型定义成为无限递归的关键。
// 例如，防止定义 type T = T 这样的类型。
// 类型变量级别调整：通过调整类型变量的级别，算法可以正确处理泛型的作用域和实例化，
// 确保类型变量的约束在适当的范围内应用。
// 支持多种类型：通过处理不同的类型（链接类型、静态类型、类类型等），这个方法确保了
// 类型系统的灵活性和泛化能力。
auto LinkType::occurs(Type* typ, Type::Unification* undo) -> bool {
  if (auto tl = typ->get_link()) {
    // 如果它是未绑定的（Unbound），检查其 ID 是否与当前 LinkType 的 ID 相同。
    // 如果是，表示发生了递归，返回 true。
    if (tl->kind == Kind::Unbound) {
      if (tl->id == id) {
        return true;
      }
      // 如果存在与之相关联的特征（trait），递归检查该特征。
      if (tl->trait && occurs(tl->trait.get(), undo)) {
        return true;
      }
      // 如果提供了撤销（undo）操作，且链接类型的级别高于当前级别，
      // 则降低链接类型的级别并记录撤销操作。
      if (undo && tl->level > level) {
        undo->leveled.emplace_back(make_pair(tl.get(), tl->level));
        tl->level = level;
      }
      return false;
    } else if (tl->kind == Link) {
      // 如果链接到另一个类型，递归检查该类型。
      return occurs(tl->type.get(), undo);
    } else {
      return false;
    }
  } else if (auto ts = typ->get_static()) {
    // 如果给定的类型 typ 是静态类型（StaticType），遍历其所有泛型，
    // 并递归检查每个泛型是否包含当前类型变量。
    for (auto& g : ts->generics)
      if (g.type && occurs(g.type.get(), undo))
        return true;
    return false;
  }
  if (auto tc = typ->get_class()) {
    // 遍历其所有泛型，递归检查每个泛型是否包含当前类型变量。
    for (auto& g : tc->generics)
      if (g.type && occurs(g.type.get(), undo))
        return true;
    // 如果它是记录类型（RecordType），遍历其所有参数类型，
    // 递归检查每个参数是否包含当前类型变量。
    if (auto tr = typ->get_record())
      for (auto& t : tr->args)
        if (occurs(t.get(), undo))
          return true;
    return false;
  } else {
    return false;
  }
}

}  // namespace Pud::Type