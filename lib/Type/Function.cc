#include <memory>
#include <string>
#include <vector>

#include "Pud/Format/Format.h"
#include "Pud/Type/Class.h"
#include "Pud/TypeCheck/TypeCheck.h"

namespace Pud::Type {

FuncType::FuncType(const std::shared_ptr<RecordType>& base_type,
                   AST::FunctionStmt* ast, std::vector<Generic> func_generics,
                   TypePtr func_parent)
    : RecordType(*base_type),
      ast(ast),
      func_generics(std::move(func_generics)),
      func_parent(std::move(func_parent)) {}

// FuncType 的统一通常用于以下场景：
// 类型检查: 当编译器需要确认两个函数类型是否兼容时，
// 例如在函数重载或泛型函数调用中。
// 类型推导: 在泛型编程中，编译器需要根据使用情况推导泛型参数的具体类型。
// 假设有两个 FuncType 实例 A 和 B，其中：
// A 表示函数 foo[T](arg: T)。
// B 表示函数 foo[int](arg: int)。
// 在某些情况下，比如在调用 foo 时传入了一个 int 类型的参数，
// 编译器需要判断 A 是否可以与 B 统一。通过 FuncType::unify 方法，
// 编译器会检查函数名是否相同（都是 foo），泛型参数是否可以统一（T 可以是
// int）， 从而确定这两个函数类型是否兼容。如果兼容，则可以推断 T 为 int 类型，
// 从而将泛型函数 foo[T] 实例化为具体的 foo[int]。
auto FuncType::unify(Type* typ, Unification* undo) -> int {
  // 自检比较: 如果 this（当前实例）与传入的 typ 是同一个实例（即地址相同），
  // 则立即返回 0。这意味着类型已经是统一的。
  if (this == typ) {
    return 0;
  }
  // 初始化分数: s1 初始化为 2。
  // 这个值会根据后续比较的结果递增。分数用于表示统一过程中的兼容程度。
  int s1 = 2;
  int s = 0;
  if (auto t = typ->get_func()) {
    // 检查函数名 (ast->name) 是否匹配。检查父类型是否匹配。
    if (ast->name != t->ast->name ||
        (bool(func_parent) ^ bool(t->func_parent))) {
      return -1;
    }
    // 如果都有父类型，则调用 func_parent->unify 方法来统一这些父类型。
    s = func_parent->unify(t->func_parent.get(), undo);
    if (func_parent && s == -1) {
      return -1;
    }
    s1 += s;
    // 检查func_generics是否匹配
    seqassert(func_generics.size() == t->func_generics.size(),
              "generic size mismatch for {}", ast->name);
    // 确保两个 FuncType 实例的泛型参数数量相同。
    // 遍历泛型参数，分别调用 unify 方法来统一这些参数。
    for (int i = 0; i < func_generics.size(); i++) {
      s = func_generics[i].type->unify(t->func_generics[i].type.get(), undo);
      if (s == -1) {
        return -1;
      }
      s1 += s;
    }
  }
  // 调用基类（RecordType）的 unify 方法。
  // 这是因为 FuncType 是 RecordType 的子类，需要确保基类部分也能统一。
  s = this->RecordType::unify(typ, undo);
  return s == -1 ? s : s1 + s;
}

auto FuncType::generalize(int at_level) -> TypePtr {
  auto g = func_generics;
  // 遍历 func_generics，对每个泛型参数调用 generalize 方法。
  // 这会将类型参数提升到一个更一般的级别。
  // 例如，一个特定的 int 类型可能会变成一个未绑定的泛型 T。
  for (auto& t : g) {
    t.type = t.type ? t.type->generalize(at_level) : nullptr;
  }
  // 如果 func_parent 存在，则也对其调用 generalize。
  auto p = func_parent ? func_parent->generalize(at_level) : nullptr;
  return std::make_shared<FuncType>(std::static_pointer_cast<RecordType>(
                                        this->RecordType::generalize(at_level)),
                                    ast, g, p);
}

auto FuncType::instantiate(int at_level, int* unbound_count,
                           std::unordered_map<int, TypePtr>* cache) -> TypePtr {
  auto g = func_generics;
  // 遍历 func_generics，对每个泛型参数调用 instantiate 方法。
  // 这会将泛型参数替换为具体的类型。
  // 例如，泛型 T 可能会被替换为具体的 int 类型。
  for (auto& t : g)
    if (t.type) {
      t.type = t.type->instantiate(at_level, unbound_count, cache);
      if (cache && cache->find(t.id) == cache->end())
        (*cache)[t.id] = t.type;
    }
  auto p = func_parent
               ? func_parent->instantiate(at_level, unbound_count, cache)
               : nullptr;
  return std::make_shared<FuncType>(
      std::static_pointer_cast<RecordType>(
          this->RecordType::instantiate(at_level, unbound_count, cache)),
      ast, g, p);
}

auto FuncType::get_unbounds() const -> std::vector<TypePtr> {
  std::vector<TypePtr> u;
  // 遍历函数泛型，对于每个有类型的泛型，添加其未绑定的类型变量到结果列表。
  for (auto& t : func_generics)
    if (t.type) {
      auto tu = t.type->get_unbounds();
      u.insert(u.begin(), tu.begin(), tu.end());
    }
  // 如果有父函数类型（func_parent），也添加其未绑定的类型变量。
  if (func_parent) {
    auto tu = func_parent->get_unbounds();
    u.insert(u.begin(), tu.begin(), tu.end());
  }
  // 遍历所有参数类型，添加其未绑定的类型变量。
  for (auto& a : get_arg_types()) {
    auto tu = a->get_unbounds();
    u.insert(u.begin(), tu.begin(), tu.end());
  }
  return u;
}

// 此方法判断函数类型是否可以实现。它确保所有参数类型都可以实现，
// 并且所有泛型（如果存在）也都可以实现
auto FuncType::can_realize() const -> bool {
  // skip_self 标志用于跳过自身类型的检查。
  bool skip_self = ast->has_attr(AST::Attr::RealizeWithoutSelf);

  // 遍历所有参数类型，确保每个参数都可以实现。
  auto args = get_arg_types();
  for (int ai = skip_self; ai < args.size(); ai++)
    if (!args[ai]->get_func() && !args[ai]->can_realize())
      return false;
  // 对于泛型，确保它们都可以实现。
  bool generics =
      std::all_of(func_generics.begin(), func_generics.end(),
                  [](auto& a) { return !a.type || a.type->can_realize(); });
  // 如果有父函数类型，也检查它是否可以实现。
  if (!skip_self)
    generics &= (!func_parent || func_parent->can_realize());
  return generics;
}

auto FuncType::realized_type_name() const -> std::string {
  return this->ClassType::realized_name();
}

auto FuncType::is_instantiated() const -> bool {
  TypePtr removed = nullptr;
  // 遍历所有泛型和父函数类型，确保它们都已实例化。
  auto ret_type = get_ret_type();
  if (ret_type->get_func() && ret_type->get_func()->func_parent.get() == this) {
    removed = ret_type->get_func()->func_parent;
    ret_type->get_func()->func_parent = nullptr;
  }
  auto res = std::all_of(func_generics.begin(), func_generics.end(),
                         [](auto& a) {
                           return !a.type || a.type->is_instantiated();
                         }) &&
             (!func_parent || func_parent->is_instantiated()) &&
             this->RecordType::is_instantiated();
  if (removed)
    ret_type->get_func()->func_parent = removed;
  return res;
}

auto FuncType::debug_string(char mode) const -> std::string {
  std::vector<std::string> gs;
  // 对于每个泛型，生成其调试字符串。
  for (auto& a : func_generics)
    if (!a.name.empty())
      gs.push_back(a.type->debug_string(mode));
  std::string s = join(gs, ",");
  std::vector<std::string> as;
  // 对于每个参数类型，生成其调试字符串。
  // 根据 mode 参数生成不同级别的调试信息。
  if (mode == 2)
    as.push_back(get_ret_type()->debug_string(mode));
  for (auto& a : get_arg_types())
    as.push_back(a->debug_string(mode));
  std::string a = join(as, ",");
  s = s.empty() ? a : join(std::vector<std::string>{a, s}, ",");

  auto fnname = ast->name;
  if (mode == 0) {
    fnname = cache->rev(ast->name);
    // 如果有父函数类型，包含它的名称。
    // if (func_parent)
    // fnname = fmt::format("{}.{}", func_parent->debug_string(mode), fnname);
  }
  return fmt::format("{}{}", fnname, s.empty() ? "" : fmt::format("[{}]", s));
}

auto FuncType::realized_name() const -> std::string {
  std::vector<std::string> gs;
  // 对于每个泛型，生成其实现后的名称。
  for (auto& a : func_generics)
    if (!a.name.empty())
      gs.push_back(a.type->realized_name());
  std::string s = join(gs, ",");
  std::vector<std::string> as;
  // 对于每个参数类型，生成其实现后的名称。
  for (auto& a : get_arg_types())
    as.push_back(a->get_func() ? a->get_func()->realized_name()
                               : a->realized_name());
  std::string a = join(as, ",");
  s = s.empty() ? a : join(std::vector<std::string>{a, s}, ",");
  // 如果有父函数类型，包含其实现后的名称。
  return fmt::format("{}{}{}",
                     func_parent ? func_parent->realized_name() + ":" : "",
                     ast->name, s.empty() ? "" : fmt::format("[{}]", s));
}

PartialType::PartialType(const std::shared_ptr<RecordType>& base_type,
                         std::shared_ptr<FuncType> func,
                         std::vector<char> known)
    : RecordType(*base_type), func(std::move(func)), known(std::move(known)) {}

auto PartialType::unify(Type* typ, Unification* us) -> int {
  return this->RecordType::unify(typ, us);
}

auto PartialType::generalize(int at_level) -> TypePtr {
  return std::make_shared<PartialType>(
      std::static_pointer_cast<RecordType>(
          this->RecordType::generalize(at_level)),
      func, known);
}

auto PartialType::instantiate(int at_level, int* unbound_count,
                              std::unordered_map<int, TypePtr>* cache)
    -> TypePtr {
  auto rec = std::static_pointer_cast<RecordType>(
      this->RecordType::instantiate(at_level, unbound_count, cache));
  return std::make_shared<PartialType>(rec, func, known);
}

auto PartialType::debug_string(char mode) const -> std::string {
  std::vector<std::string> gs;
  for (auto& a : generics)
    if (!a.name.empty())
      gs.push_back(a.type->debug_string(mode));
  std::vector<std::string> as;
  int i = 0;
  int gi = 0;
  for (; i < known.size(); i++)
    if (func->ast->args[i].status == AST::Param::Normal) {
      if (!known[i])
        as.emplace_back("...");
      else
        as.emplace_back(gs[gi++]);
    }
  auto fnname = func->ast->name;
  if (mode == 0) {
    fnname = cache->rev(func->ast->name);
    // if (func->func_parent)
    // fnname = fmt::format("{}.{}", func->func_parent->debug_string(mode),
    // fnname);
  } else if (mode == 2) {
    fnname = func->debug_string(mode);
  }
  return fmt::format("{}[{}{}]", fnname, join(as, ","),
                     mode == 2 ? fmt::format(";{}", join(gs, ",")) : "");
}

auto PartialType::realized_name() const -> std::string {
  std::vector<std::string> gs;
  gs.push_back(func->ast->name);
  for (auto& a : generics)
    if (!a.name.empty())
      gs.push_back(a.type->realized_name());
  std::string s = join(gs, ",");
  return fmt::format("{}{}", name, s.empty() ? "" : fmt::format("[{}]", s));
}

}  // namespace Pud::Type