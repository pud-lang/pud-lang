#ifndef PUD_TYPE_STATIC_H
#define PUD_TYPE_STATIC_H

#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Pud/Type/Class.h"

namespace Pud::AST {
struct StaticValue;
}  // namespace Pud::AST

namespace Pud::Type {

// 代表一个静态整数类型。
// 这通常是用于编译时计算的类型，如模板或泛型编程中的整数类型参数。
// (e.g. N in def foo[N: int])
struct StaticType : public Type {
  // 存储静态类型依赖的静态变量列表。
  // 例如，对于表达式 A+B+2，generics 是 {A, B}。
  std::vector<ClassType::Generic> generics;
  // 用于存储需要求值的静态表达式。如果没有表达式，则可以为 nullptr。
  std::shared_ptr<AST::Expr> expr;

  StaticType(AST::Cache* cache, std::vector<ClassType::Generic> generics,
             const std::shared_ptr<AST::Expr>& expr);
  // 一个只接受表达式的便利构造函数，它解析表达式并填充静态类型的 generics。
  StaticType(AST::Cache* cache, const std::shared_ptr<AST::Expr>& expr);
  // 一个接受已知求值结果（整数 i 或字符串 s）的构造函数。
  explicit StaticType(AST::Cache* cache, int64_t i);
  explicit StaticType(AST::Cache* cache, const std::string& s);

 public:
  auto unify(Type* typ, Unification* undo) -> int override;
  auto generalize(int at_level) -> TypePtr override;
  auto instantiate(int at_level, int* unbound_count,
                   std::unordered_map<int, TypePtr>* cache) -> TypePtr override;

 public:
  auto get_unbounds() const -> std::vector<TypePtr> override;
  auto can_realize() const -> bool override;
  auto is_instantiated() const -> bool override;
  auto debug_string(char mode) const -> std::string override;
  auto realized_name() const -> std::string override;

  auto evaluate() const -> AST::StaticValue;
  auto get_static() -> std::shared_ptr<StaticType> override {
    return std::static_pointer_cast<StaticType>(shared_from_this());
  }

 private:
  void parse_expr(const std::shared_ptr<AST::Expr>& e,
                  std::unordered_set<std::string>& seen);
};

}  // namespace Pud::Type

#endif  // PUD_TYPE_STATIC_H