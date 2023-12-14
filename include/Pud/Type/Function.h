#ifndef PUD_TYPE_FUNCTION_H
#define PUD_TYPE_FUNCTION_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "Pud/Type/Class.h"
#include "Pud/Type/Type.h"

namespace Pud::AST {
struct FunctionStmt;
}  // namespace Pud::AST

namespace Pud::Type {

// FuncType 不是一个函数指针类型，而是具体表示一个函数的类型。
struct FuncType : public RecordType {
  // 指向AST（抽象语法树）中的 FunctionStmt 结构体，表示函数的代码结构。
  AST::FunctionStmt* ast;
  // 函数泛型参数列表。例如，def foo[T](...) 中的 T。
  std::vector<ClassType::Generic> func_generics;
  // 表示函数的上下文，可能是一个类或者另一个函数。
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

// 代表一个部分实例化的函数。它实现了元组类型。
struct PartialType : public RecordType {
  // 指向部分实例化的函数。这是一个泛型函数，还未完全实例化。
  FuncTypePtr func;
  // 指示哪些参数已经提供的标记数组。如果参数已知，则对应位置为1，否则为0。
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