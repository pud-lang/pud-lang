#ifndef PUD_TYPE_TRAITS_H
#define PUD_TYPE_TRAITS_H

#include "Pud/Type/Type.h"

namespace Pud::Type {

/// 表示和处理特定的类型特征（Traits）。
struct Trait : public Type {
  auto can_realize() const -> bool override;
  auto is_instantiated() const -> bool override;
  auto realized_name() const -> std::string override;

 protected:
  explicit Trait() = default;
  explicit Trait(const std::shared_ptr<Type>&);
};

/// 代表可调用的类型特征，如函数类型。
struct CallableTrait : public Trait {
  explicit CallableTrait(std::vector<TypePtr> args);
  auto unify(Type* typ, Unification* undo) -> int override;
  auto generalize(int at_level) -> TypePtr override;
  auto instantiate(int at_level, int* unbound_count,
                   std::unordered_map<int, TypePtr>* cache) -> TypePtr override;
  auto debug_string(char mode) const -> std::string override;

  std::vector<TypePtr> args;  // 存储参数类型和返回类型。
};

struct TypeTrait : public Trait {
  TypePtr type;

 public:
  explicit TypeTrait(TypePtr type);
  auto unify(Type* typ, Unification* undo) -> int override;
  auto generalize(int at_level) -> TypePtr override;
  auto instantiate(int at_level, int* unbound_count,
                   std::unordered_map<int, TypePtr>* cache) -> TypePtr override;
  auto debug_string(char mode) const -> std::string override;
};

struct VariableTupleTrait : public Trait {
  TypePtr size;

 public:
  explicit VariableTupleTrait(TypePtr size);
  auto unify(Type* typ, Unification* undo) -> int override;
  auto generalize(int at_level) -> TypePtr override;
  auto instantiate(int at_level, int* unbound_count,
                   std::unordered_map<int, TypePtr>* cache) -> TypePtr override;
  auto debug_string(char mode) const -> std::string override;
};

}  // namespace Pud::Type

#endif  // PUD_TYPE_TRAITS_H