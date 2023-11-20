#ifndef PUD_TYPE_LINK_H
#define PUD_TYPE_LINK_H

#include "Pud/Type/Traits.h"
#include "Pud/Type/Type.h"

namespace Pud::Type {

/// 表示不同类型的链接，包括未绑定类型、泛型类型和其他类型的链接。
/// 在类型推断和类型检查的过程中非常重要。
struct LinkType : public Type {
  // 未绑定（Unbound）、泛型（Generic）或链接（Link）。
  enum Kind : uint8_t { Unbound, Generic, Link };

  LinkType(Kind kind, int id, int level = 0, TypePtr type = nullptr,
           char is_static = 0, std::shared_ptr<Trait> trait = nullptr,
           TypePtr default_type = nullptr, std::string generic_name = "");

  explicit LinkType(TypePtr type);

  auto unify(Type* typ, Unification* undo) -> int override;
  auto generalize(int at_level) -> TypePtr override;
  auto instantiate(int at_level, int* unbound_count,
                   std::unordered_map<int, TypePtr>* cache) -> TypePtr override;

  auto follow() -> TypePtr override;
  auto get_unbounds() const -> std::vector<TypePtr> override;
  auto can_realize() const -> bool override;
  auto is_instantiated() const -> bool override;
  auto debug_string(char mode) const -> std::string override;
  auto realized_name() const -> std::string override;

  auto get_link() -> std::shared_ptr<LinkType> override;
  auto get_func() -> std::shared_ptr<FuncType> override;
  auto get_partial() -> std::shared_ptr<PartialType> override;
  auto get_class() -> std::shared_ptr<ClassType> override;
  auto get_record() -> std::shared_ptr<RecordType> override;
  auto get_static() -> std::shared_ptr<StaticType> override;
  auto get_union() -> std::shared_ptr<UnionType> override;
  auto get_unbound() -> std::shared_ptr<LinkType> override;

  // 未绑定或泛型类型的唯一标识符。
  int id;
  // 未绑定类型的类型检查级别。
  int level;
  // link的类型。
  Kind kind;
  // 指向的类型。如果是未知的（未绑定或泛型），则为nullptr。
  TypePtr type;
  // 如果类型是静态类型（如Int[N: int]中的N）则大于0，否则为0。
  char is_static;
  // 在统一前，未绑定类型需要满足的可选特征。
  std::shared_ptr<Trait> trait;
  // 泛型类型的泛型名称，用于美观打印。
  std::string generic_name;
  // 如果未绑定的类型未解决，将使用的类型。
  TypePtr default_type;

 private:
  /// 检查类型是否在给定类型内部出现，用于防止递归统一。例如list[?1]中的?1。
  auto occurs(Type* typ, Type::Unification* undo) -> bool;
};

/*
1. Unbound 类型
- 情景：当一个变量或类型参数在定义时未明确其具体类型。
- 例子：在函数定义中，参数类型未指定，如 def func(x): ... 中的 x。
在这种情况下，x 的类型是 Unbound，因为在定义时并没有指定它应该是什么类型。
2. Generic 类型
- 情景：用于定义泛型类或函数，其中类型参数可以被任何类型替换。
- 例子：定义一个泛型类 class GenericClass(Generic[T]): ...。
这里 T 是一个泛型参数，可以在类的实例化时被任何具体类型替换。
3. Link 类型
- 情景：当一个类型参数或变量已经绑定到一个具体的类型。
- 例子：在函数体内，一个参数的类型被推断为具体类型。
例如，def func(x): return x + 1 中的 x 可以被推断为 int 类型，
因此在这个上下文中，x 是 Link 类型，链接到 int。
*/

}  // namespace Pud::Type

#endif  // PUD_TYPE_LINK_H