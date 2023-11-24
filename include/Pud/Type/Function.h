#ifndef PUD_TYPE_FUNCTION_H
#define PUD_TYPE_FUNCTION_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "Pud/AST/Stmt.h"
#include "Pud/Type/Class.h"

namespace Pud::Type {

/**
 * A generic type that represents a Seq function instantiation.
 * It inherits RecordType that realizes Callable[...].
 *
 * ⚠️ This is not a function pointer (Function[...]) type.
 */
struct FuncType : public RecordType {
  /// Canonical AST node.
  AST::FunctionStmt* ast;
  /// Function generics (e.g. T in def foo[T](...)).
  std::vector<ClassType::Generic> func_generics;
  /// Enclosing class or a function.
  TypePtr func_parent;

 public:
  FuncType(const std::shared_ptr<RecordType>& base_type, AST::FunctionStmt* ast,
           std::vector<ClassType::Generic> func_generics =
               std::vector<ClassType::Generic>(),
           TypePtr func_parent = nullptr);

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
  auto realized_type_name() const -> std::string override;

  auto get_func() -> std::shared_ptr<FuncType> override {
    return std::static_pointer_cast<FuncType>(shared_from_this());
  }

  auto get_arg_types() const -> std::vector<TypePtr>& {
    return generics[0].type->get_record()->args;
  }
  auto get_ret_type() const -> TypePtr { return generics[1].type; }
};
using FuncTypePtr = std::shared_ptr<FuncType>;

/**
 * A generic type that represents a partial Seq function instantiation.
 * It inherits RecordType that realizes Tuple[...].
 *
 * Note: partials only work on Seq functions. Function pointer partials
 *       will become a partials of Function.__call__ Seq function.
 */
struct PartialType : public RecordType {
  /// Seq function that is being partialized. Always generic (not instantiated).
  FuncTypePtr func;
  /// Arguments that are already provided (1 for known argument, 0 for
  /// expecting).
  std::vector<char> known;

 public:
  PartialType(const std::shared_ptr<RecordType>& base_type,
              std::shared_ptr<FuncType> func, std::vector<char> known);

 public:
  auto unify(Type* typ, Unification* us) -> int override;
  auto generalize(int at_level) -> TypePtr override;
  auto instantiate(int at_level, int* unbound_count,
                   std::unordered_map<int, TypePtr>* cache) -> TypePtr override;

  auto debug_string(char mode) const -> std::string override;
  auto realized_name() const -> std::string override;

 public:
  auto get_partial() -> std::shared_ptr<PartialType> override {
    return std::static_pointer_cast<PartialType>(shared_from_this());
  }
};
using PartialTypePtr = std::shared_ptr<PartialType>;

}  // namespace Pud::Type

#endif  // PUD_TYPE_FUNCTION_H