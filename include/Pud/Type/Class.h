#ifndef PUD_TYPE_CLASS_H
#define PUD_TYPE_CLASS_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "Pud/Type/Type.h"

namespace Pud::Type {

/// 代表一个通用的类引用类型。它用于表示程序中定义的任何类。
struct ClassType : public Type {
  // 泛型参数列表。每个泛型由其唯一ID定义。
  struct Generic {
    // 类的名称
    std::string name;
    // 用于美化打印的名称
    std::string nice_name;
    // 唯一泛型id
    int id;
    // 指向实现类型或者泛型LinkType
    TypePtr type;

    Generic(std::string name, std::string nice_name, TypePtr type, int id)
        : name(std::move(name)),
          nice_name(std::move(nice_name)),
          id(id),
          type(std::move(type)) {}
  };

  std::string name;
  std::string nice_name;
  // 泛型参数列表
  std::vector<Generic> generics;
  // 隐藏的泛型参数列表
  std::vector<Generic> hidden_generics;

  std::string _rn;

  explicit ClassType(AST::Cache* cache, std::string name, std::string nice_name,
                     std::vector<Generic> generics = {},
                     std::vector<Generic> hidden_generics = {});
  explicit ClassType(const std::shared_ptr<ClassType>& base);

 public:
  auto unify(Type* typ, Unification* undo) -> int override;
  auto generalize(int at_level) -> TypePtr override;
  auto instantiate(int at_level, int* unbound_count,
                   std::unordered_map<int, TypePtr>* cache) -> TypePtr override;

 public:
  // 获取未绑定的类型列表。
  auto get_unbounds() const -> std::vector<TypePtr> override;
  // 检查当前类型是否可以实现（实例化）。
  auto can_realize() const -> bool override;
  // 检查当前类型是否已实例化。
  auto is_instantiated() const -> bool override;
  // 生成类型的调试字符串。
  auto debug_string(char mode) const -> std::string override;
  // 获取实现（实例化）类型的名称。
  auto realized_name() const -> std::string override;
  // 获取实现类型的名称，可以由子类重写。
  virtual auto realized_type_name() const -> std::string;

  auto get_class() -> std::shared_ptr<ClassType> override {
    return std::static_pointer_cast<ClassType>(shared_from_this());
  }
};

using ClassTypePtr = std::shared_ptr<ClassType>;

// 表示一个记录或元组类型。
struct RecordType : public ClassType {
  // 元组中的元素类型列表。
  std::vector<TypePtr> args;
  // 标记是否为非元组类型。
  bool no_tuple;
  // 表示元组的重复类型（如果有的话）。
  std::shared_ptr<StaticType> repeats = nullptr;

  explicit RecordType(AST::Cache* cache, std::string name, std::string nice_name,
                      std::vector<ClassType::Generic> generics =
                          std::vector<ClassType::Generic>(),
                      std::vector<TypePtr> args = std::vector<TypePtr>(),
                      bool no_tuple = false,
                      const std::shared_ptr<StaticType>& repeats = nullptr);
  RecordType(const ClassTypePtr& base, std::vector<TypePtr> args,
             bool no_tuple = false,
             const std::shared_ptr<StaticType>& repeats = nullptr);

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

  auto get_record() -> std::shared_ptr<RecordType> override {
    return std::static_pointer_cast<RecordType>(shared_from_this());
  }
  auto get_heterogenous_tuple() -> std::shared_ptr<RecordType> override;

  auto get_repeats() const -> int64_t;
  void flatten();
};

}  // namespace Pud::Type

#endif  // PUD_TYPE_CLASS_H