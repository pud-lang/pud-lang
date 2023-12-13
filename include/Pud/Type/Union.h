#ifndef PUD_TYPE_UNION_H
#define PUD_TYPE_UNION_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "Pud/Type/Class.h"

namespace Pud::Type {

struct UnionType : public RecordType {
  std::vector<TypePtr> pending_types;

  explicit UnionType();
  UnionType(const std::vector<ClassType::Generic>&,
            const std::vector<TypePtr>&);

 public:
  auto unify(Type* typ, Unification* undo) -> int override;
  auto generalize(int at_level) -> TypePtr override;
  auto instantiate(int at_level, int* unbound_count,
                   std::unordered_map<int, TypePtr>* cache) -> TypePtr override;

 public:
  auto can_realize() const -> bool override;
  auto debug_string(char mode) const -> std::string override;
  auto realized_name() const -> std::string override;
  auto realized_type_name() const -> std::string override;
  auto is_sealed() const -> bool;

  auto get_union() -> std::shared_ptr<UnionType> override {
    return std::static_pointer_cast<UnionType>(shared_from_this());
  }

  void add_type(TypePtr typ);
  void seal();
  auto get_realization_types() -> std::vector<Pud::Type::TypePtr>;
};

}  // namespace Pud::Type

#endif  // PUD_TYPE_UNION_H