#ifndef PUD_TYPE_LINK_H
#define PUD_TYPE_LINK_H

#include "Pud/Type/Traits.h"
#include "Pud/Type/Type.h"

namespace Pud::Type {

/// 表示不同类型的链接，包括未绑定类型、泛型类型和其他类型的链接。
/// 在类型推断和类型检查的过程中非常重要。
struct LinkType : public Type {
  // 未绑定（Unbound）、泛型（Generic）或链接（Link）。
  enum Kind { Unbound, Generic, Link } kind;
  // 未绑定或泛型类型的唯一标识符。
  int id;
  // 未绑定类型的类型检查级别。
  int level;
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

 public:
  LinkType(Kind kind, int id, int level = 0, TypePtr type = nullptr,
           char is_static = 0, std::shared_ptr<Trait> trait = nullptr,
           TypePtr default_type = nullptr, std::string generic_name = "");
  /// Convenience constructor for linked types.
  explicit LinkType(TypePtr type);

 public:
  int unify(Type* typ, Unification* undodo) override;
  TypePtr generalize(int atLevel) override;
  TypePtr instantiate(int atLevel, int* unboundCount,
                      std::unordered_map<int, TypePtr>* cache) override;

 public:
  TypePtr follow() override;
  std::vector<TypePtr> getUnbounds() const override;
  bool canRealize() const override;
  bool isInstantiated() const override;
  std::string debugString(char mode) const override;
  std::string realizedName() const override;

  std::shared_ptr<LinkType> getLink() override;
  std::shared_ptr<FuncType> getFunc() override;
  std::shared_ptr<PartialType> getPartial() override;
  std::shared_ptr<ClassType> getClass() override;
  std::shared_ptr<RecordType> getRecord() override;
  std::shared_ptr<StaticType> getStatic() override;
  std::shared_ptr<UnionType> getUnion() override;
  std::shared_ptr<LinkType> getUnbound() override;

 private:
  /// Checks if a current (unbound) type occurs within a given type.
  /// Needed to prevent a recursive unification (e.g. ?1 with list[?1]).
  bool occurs(Type* typ, Type::Unification* undo);
};

}  // namespace Pud::Type

#endif  // PUD_TYPE_LINK_H