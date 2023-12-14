#include "Pud/Type/Class.h"

#include <algorithm>
#include <memory>
#include <string>
#include <vector>

#include "Pud/Common/Error.h"
#include "Pud/Type/Link.h"
#include "Pud/Type/Static.h"
#include "Pud/TypeCheck/TypeCheck.h"

namespace Pud::Type {

ClassType::ClassType(AST::Cache* cache, std::string name, std::string nice_name,
                     std::vector<Generic> generics,
                     std::vector<Generic> hidden_generics)
    : Type(cache),
      name(std::move(name)),
      nice_name(std::move(nice_name)),
      generics(std::move(generics)),
      hidden_generics(std::move(hidden_generics)) {}

ClassType::ClassType(const ClassTypePtr& base)
    : Type(base),
      name(base->name),
      nice_name(base->nice_name),
      generics(base->generics),
      hidden_generics(base->hidden_generics) {}

auto ClassType::unify(Type* typ, Unification* undo) -> int {
  if (auto tc = typ->get_class()) {
    if (name != tc->name) {
      return -1;
    }

    // s1 的初始化值为 3 可能是为了表示类型名称和泛型数量匹配时
    // 的基础分数。在类型系统中，分数通常用于表示类型匹配的程度
    // 或者类型匹配操作的复杂性。初始化为 3 可能意味着类型名称匹
    // 配和泛型数量匹配本身就值得一定分数。这是一种设计选择，用
    // 于量化类型匹配的程度。
    int s1 = 3;
    int s = 0;
    // 如果泛型的数量不同，则这两个类型不兼容。
    if (generics.size() != tc->generics.size()) {
      return -1;
    }

    // 对每个泛型参数递归地进行统一操作。如果任何泛型参数不兼容，
    // 则整个类型不兼容。
    for (int i = 0; i < generics.size(); ++i) {
      s = generics[i].type->unify(tc->generics[i].type.get(), undo);
      if (s == -1) {
        return -1;
      }
      s1 += s;
    }

    return s1;
  } else if (auto tl = typ->get_link()) {
    return tl->unify(this, undo);
  } else {
    return -1;
  }
}

/*
at_level 参数在泛化（generalize）和实例化（instantiate）方法中用于跟踪
类型变量的作用域。在类型系统中，泛型参数可能具有不同的作用域级别。

class MyClass<T>:
    def method1(self):
        # 在这里，T 有一个特定的作用域级别

    def method2<U>(self):
        # U 是一个新的泛型参数，具有不同的作用域级别
        # T 仍然是有效的，但在不同的作用域级别

在这个例子中，T 和 U 是泛型参数，但它们位于不同的作用域级别。
at_level 参数在泛化和实例化过程中就用于区分这些不同级别的泛型参数。
*/

auto ClassType::generalize(int at_level) -> TypePtr {
  auto g = generics;
  auto hg = hidden_generics;
  for (auto& t : g) {
    t.type = t.type ? t.type->generalize(at_level) : nullptr;
  }
  for (auto& t : hg) {
    t.type = t.type ? t.type->generalize(at_level) : nullptr;
  }
  auto c = std::make_shared<ClassType>(cache, name, nice_name, g, hg);
  c->set_source_info(get_source_info());
  return c;
}

auto ClassType::instantiate(int at_level, int* unbound_count,
                            std::unordered_map<int, TypePtr>* cache)
    -> TypePtr {
  auto g = generics;
  auto hg = hidden_generics;
  for (auto& t : g) {
    t.type =
        t.type ? t.type->instantiate(at_level, unbound_count, cache) : nullptr;
  }
  for (auto& t : hg) {
    t.type =
        t.type ? t.type->instantiate(at_level, unbound_count, cache) : nullptr;
  }
  auto c = std::make_shared<ClassType>(this->cache, name, nice_name, g, hg);
  c->set_source_info(get_source_info());
  return c;
}

auto ClassType::get_unbounds() const -> std::vector<TypePtr> {
  std::vector<TypePtr> u;
  for (const auto& t : generics) {
    if (t.type) {
      auto tu = t.type->get_unbounds();
      u.insert(u.begin(), tu.begin(), tu.end());
    }
  }
  for (const auto& t : hidden_generics) {
    if (t.type) {
      auto tu = t.type->get_unbounds();
      u.insert(u.begin(), tu.begin(), tu.end());
    }
  }
  return u;
}

auto ClassType::can_realize() const -> bool {
  return std::all_of(
             generics.begin(), generics.end(),
             [](auto& t) { return !t.type || t.type->can_realize(); }) &&
         std::all_of(hidden_generics.begin(), hidden_generics.end(),
                     [](auto& t) { return !t.type || t.type->can_realize(); });
}

auto ClassType::is_instantiated() const -> bool {
  return std::all_of(
             generics.begin(), generics.end(),
             [](auto& t) { return !t.type || t.type->is_instantiated(); }) &&
         std::all_of(
             hidden_generics.begin(), hidden_generics.end(),
             [](auto& t) { return !t.type || t.type->is_instantiated(); });
}

auto ClassType::debug_string(char mode) const -> std::string { return ""; }

auto ClassType::realized_name() const -> std::string { return ""; }

auto ClassType::realized_type_name() const -> std::string { return ""; }

RecordType::RecordType(AST::Cache* cache, std::string name,
                       std::string nice_name, std::vector<Generic> generics,
                       std::vector<TypePtr> args, bool no_tuple,
                       const std::shared_ptr<StaticType>& repeats)
    : ClassType(cache, std::move(name), std::move(nice_name),
                std::move(generics)),
      args(std::move(args)),
      no_tuple(false),
      repeats(repeats) {}

RecordType::RecordType(const ClassTypePtr& base, std::vector<TypePtr> args,
                       bool no_tuple,
                       const std::shared_ptr<StaticType>& repeats)
    : ClassType(base),
      args(std::move(args)),
      no_tuple(no_tuple),
      repeats(repeats) {}

auto RecordType::unify(Type* typ, Unification* undo) -> int {
  if (auto tr = typ->get_record()) {
    // int <-> Int[64]
    // 如果记录类型是 int 类型，且被比较的类型是 Int（或相反），
    // 则进行特殊处理。这通常是为了处理内置类型与用户定义类型之间的不一致。
    if (name == "int" && tr->name == "Int") {
      return tr->unify(this, undo);
    }
    if (tr->name == "int" && name == "Int") {
      auto t64 = std::make_shared<StaticType>(64);
      return generics[0].type->unify(t64.get(), undo);
    }

    // 如果任何一个类型具有重复（例如，元组的元素可以重复），
    // 则它们的重复次数必须匹配。
    if (repeats || tr->repeats) {
      if (!repeats && tr->repeats) {
        auto n = std::make_shared<StaticType>(args.size());
        if (tr->repeats->unify(n.get(), undo) == -1) {
          return -1;
        }
      } else if (!tr->repeats) {
        auto n = std::make_shared<StaticType>(tr->args.size());
        if (repeats->unify(n.get(), undo) == -1) {
          return -1;
        }
      } else {
        if (repeats->unify(tr->repeats.get(), undo) == -1) {
          return -1;
        }
      }
    }

    if (get_repeats() != -1) {
      flatten();
    }
    if (tr->get_repeats() != -1) {
      tr->flatten();
    }

    int s1 = 2;
    int s = 0;
    if (args.size() != tr->args.size()) {
      return -1;
    }
    for (int i = 0; i < args.size(); i++) {
      s = args[i]->unify(tr->args[i].get(), undo);
      if (s != -1) {
        s1 += s;
      } else {
        return -1;
      }
    }

    // Handle Tuple<->@tuple: when unifying tuples, only record members matter.
    if (name == TYPE_TUPLE || tr->name == TYPE_TUPLE) {
      if (!args.empty() ||
          (!no_tuple && !tr->no_tuple)) {  // prevent POD<->() unification
        return s1 + static_cast<int>(name == tr->name);
      } else {
        return -1;
      }
    }
    return this->ClassType::unify(tr.get(), undo);
  } else if (auto t = typ->get_link()) {
    return t->unify(this, undo);
  } else {
    return -1;
  }
}

auto RecordType::generalize(int at_level) -> TypePtr {
  auto c = std::static_pointer_cast<ClassType>(
      this->ClassType::generalize(at_level));
  auto a = args;
  for (auto& t : a) {
    t = t->generalize(at_level);
  }
  auto r = repeats ? repeats->generalize(at_level)->get_static() : nullptr;
  return std::make_shared<RecordType>(c, a, no_tuple, r);
}

auto RecordType::instantiate(int at_level, int* unbound_count,
                             std::unordered_map<int, TypePtr>* cache)
    -> TypePtr {
  auto c = std::static_pointer_cast<ClassType>(
      this->ClassType::instantiate(at_level, unbound_count, cache));
  auto a = args;
  for (auto& t : a) {
    t = t->instantiate(at_level, unbound_count, cache);
  }
  auto r =
      repeats
          ? repeats->instantiate(at_level, unbound_count, cache)->get_static()
          : nullptr;
  return std::make_shared<RecordType>(c, a, no_tuple, r);
}

auto RecordType::get_unbounds() const -> std::vector<TypePtr> {
  std::vector<TypePtr> u;
  if (repeats) {
    auto tu = repeats->get_unbounds();
    u.insert(u.begin(), tu.begin(), tu.end());
  }
  for (const auto& a : args) {
    auto tu = a->get_unbounds();
    u.insert(u.begin(), tu.begin(), tu.end());
  }
  auto tu = this->ClassType::get_unbounds();
  u.insert(u.begin(), tu.begin(), tu.end());
  return u;
}

auto RecordType::can_realize() const -> bool {
  return get_repeats() >= 0 &&
         std::all_of(args.begin(), args.end(),
                     [](auto& a) { return a->can_realize(); }) &&
         this->ClassType::can_realize();
}

auto RecordType::is_instantiated() const -> bool {
  return (!repeats || repeats->is_instantiated()) &&
         std::all_of(args.begin(), args.end(),
                     [](auto& a) { return a->is_instantiated(); }) &&
         this->ClassType::is_instantiated();
}

auto RecordType::realized_name() const -> std::string { return ""; }

auto RecordType::debug_string(char mode) const -> std::string { return ""; }

auto RecordType::realized_type_name() const -> std::string {
  return realized_name();
}

auto RecordType::get_heterogenous_tuple() -> std::shared_ptr<RecordType> {
  assert(can_realize() && "{} not realizable");
  if (args.size() > 1) {
    std::string first = args[0]->realized_name();
    for (int i = 1; i < args.size(); i++) {
      if (args[i]->realized_name() != first) {
        return get_record();
      }
    }
  }
  return nullptr;
}

auto RecordType::get_repeats() const -> int64_t {
  if (!repeats) {
    return 1;
  }
  if (repeats->can_realize()) {
    return std::max(repeats->evaluate().get_int(), static_cast<int64_t>(0));
  }
  return -1;
}

void RecordType::flatten() {
  auto n = get_repeats();
  assert(n >= 0 && "bad call to flatten");

  auto a = args;
  args.clear();
  for (int64_t i = 0; i < n; i++) {
    args.insert(args.end(), a.begin(), a.end());
  }

  repeats = nullptr;
}

}  // namespace Pud::Type