#ifndef PUD_TYPE_UNION_H
#define PUD_TYPE_UNION_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "Pud/Type/Class.h"

namespace Pud::Type {

// 用于表示类型联合（type union）的类，在类型系统中用于表示一个值可以是多种类型之一。
// def process(data: Union[int, str]):
//     pass
// process 函数接受一个可以是 int 或 str 类型的参数 data。
// 在类型系统内部，这将表示为一个 UnionType，其中包含 int 和 str 类型作为其成员。
struct UnionType : public RecordType {
  std::vector<TypePtr> pending_types;

  explicit UnionType(AST::Cache* cache);
  UnionType(AST::Cache* cache, const std::vector<ClassType::Generic>&,
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
  // 检查 UnionType 是否已经封闭。封闭的联合类型不能再添加新的类型成员。
  auto is_sealed() const -> bool;

  auto get_union() -> std::shared_ptr<UnionType> override {
    return std::static_pointer_cast<UnionType>(shared_from_this());
  }

  // 向 UnionType 添加一个新的类型。如果 UnionType 已经封闭（sealed），则不能添加新类型。
  void add_type(TypePtr typ);
  // 封闭 UnionType，这意味着不再接受新的类型成员。
  // 此操作会从 pending_types 中提取所有非未绑定类型，并将它们设置为联合类型的成员。
  void seal();
  auto get_realization_types() -> std::vector<Pud::Type::TypePtr>;
};

}  // namespace Pud::Type

#endif  // PUD_TYPE_UNION_H