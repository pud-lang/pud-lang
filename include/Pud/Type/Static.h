#ifndef PUD_TYPE_STATIC_H
#define PUD_TYPE_STATIC_H

#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Pud/AST/Expr.h"
#include "Pud/Type/Class.h"

namespace Pud::Type {

/**
 * A static integer type (e.g. N in def foo[N: int]). Usually an integer, but
 * can point to a static expression.
 */
struct StaticType : public Type {
  /// List of static variables that a type depends on
  /// (e.g. for A+B+2, generics are {A, B}).
  std::vector<ClassType::Generic> generics;
  /// A static expression that needs to be evaluated.
  /// Can be nullptr if there is no expression.
  std::shared_ptr<AST::Expr> expr;

  StaticType(std::vector<ClassType::Generic> generics,
             const std::shared_ptr<AST::Expr>& expr);
  /// Convenience function that parses expr and populates static type generics.
  StaticType(const std::shared_ptr<AST::Expr>& expr);
  /// Convenience function for static types whose evaluation is already known.
  explicit StaticType(int64_t i);
  explicit StaticType(const std::string& s);

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