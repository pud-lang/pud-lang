#ifndef PUD_IR_BASE_H
#define PUD_IR_BASE_H

#include <cstdint>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>

#include "Pud/Common/Error.h"
#include "Pud/IR/Attribute.h"
#include "Pud/IR/Util/Iterators.h"
#include "Pud/IR/Util/Visitor.h"

namespace Pud::IR {

using id_t = std::int64_t;

class Func;
class Module;

// IdMixin 类是一个Mixin模式类，为IR节点提供唯一标识符（ID）。
class IdMixin {
 private:
  // 用于生成下一个ID。
  static id_t current_id;

 protected:
  // 存储该实例的ID。
  id_t id;

 public:
  // 用于重置ID计数器。
  static void reset_id();

  // 为每个新实例分配一个唯一的ID。
  IdMixin() : id(current_id++) {}

  // 返回节点的ID。
  virtual auto get_id() const -> id_t { return id; }
};

// Node 类是IR节点的基类。它提供了节点的基本属性和行为。
class Node {
 private:
  // 节点的名称。
  std::string name;
  // 属性映射，用于存储节点的附加信息。
  std::map<std::string, std::unique_ptr<Attribute>> attributes;
  // 指向所属模块的指针。
  Module* module = nullptr;
  // 用于节点替换的指针。
  Node* replacement = nullptr;

 public:
  static const char NodeId;

  explicit Node(std::string name = "") : name(std::move(name)) {}

  // 返回该节点类型的唯一标识符。这是LLVM风格的RTTI的一部分。
  static auto node_id() -> const void* { return &NodeId; }

  // 检查此节点是否可以转换为另一类型的节点。
  virtual auto is_convertible(const void* other) const -> bool {
    if (has_replacement()) {
      return get_actual()->is_convertible(other);
    }
    return other == node_id();
  }

  // 检查此节点是否可以被视为 Target 类型的节点。
  template <typename Target>
  auto is() const -> bool {
    return is_convertible(Target::nodeId());
  }

  // 尝试将此节点转换为 Target 类型的节点。
  template <typename Target>
  auto as() -> Target* {
    return is_convertible(Target::nodeId()) ? static_cast<Target*>(get_actual())
                                            : nullptr;
  }

  template <typename Target>
  auto as() const -> const Target* {
    return is_convertible(Target::nodeId())
               ? static_cast<const Target*>(get_actual())
               : nullptr;
  }

  // 获取节点的名称。
  auto get_name() const -> const std::string& { return get_actual()->name; }

  // 设置节点的名称。
  void set_name(std::string n) { get_actual()->name = std::move(n); }

  // 接受访问者模式的访问。
  virtual void accept(Util::Visitor& v) = 0;

  virtual void accept(Util::ConstVisitor& v) const = 0;

  // 处理节点的属性。
  void set_attribute(std::unique_ptr<Attribute> value, const std::string& key) {
    get_actual()->attributes[key] = std::move(value);
  }

  template <typename AttributeType>
  void set_attribute(std::unique_ptr<AttributeType> value) {
    set_attribute(std::move(value), AttributeType::AttributeName);
  }

  auto has_attribute(const std::string& n) const -> bool {
    const auto* actual = get_actual();
    return actual->attributes.find(n) != actual->attributes.end();
  }

  template <typename AttributeType>
  auto has_attribute() const -> bool {
    return has_attribute(AttributeType::AttributeName);
  }

  auto get_attribute(const std::string& key) -> Attribute* {
    auto* actual = get_actual();

    auto it = actual->attributes.find(key);
    return it != actual->attributes.end() ? it->second.get() : nullptr;
  }

  auto get_attribute(const std::string& key) const -> const Attribute* {
    const auto* actual = get_actual();

    auto it = actual->attributes.find(key);
    return it != actual->attributes.end() ? it->second.get() : nullptr;
  }

  template <typename AttributeType>
  auto get_attribute() -> AttributeType* {
    return static_cast<AttributeType*>(
        get_attribute(AttributeType::AttributeName));
  }

  template <typename AttributeType>
  auto get_attribute() const -> const AttributeType* {
    return static_cast<const AttributeType*>(
        get_attribute(AttributeType::AttributeName));
  }

  auto attributes_begin() {
    return Util::map_key_adaptor(get_actual()->attributes.begin());
  }

  auto attributes_end() {
    return Util::map_key_adaptor(get_actual()->attributes.end());
  }

  auto attributes_begin() const {
    return Util::const_map_key_adaptor(get_actual()->attributes.begin());
  }

  auto attributes_end() const {
    return Util::const_map_key_adaptor(get_actual()->attributes.end());
  }

  // 设置节点的源信息（例如，源代码中的位置）。
  void set_source_info(Pud::SourceInfo s) {
    set_attribute(std::make_unique<SourceInfoAttribute>(std::move(s)));
  }

  // 获取节点的源信息。
  auto get_source_info() const -> Pud::SourceInfo {
    return get_attribute<SourceInfoAttribute>()
               ? get_attribute<SourceInfoAttribute>()->info
               : Pud::SourceInfo();
  }

  // 返回节点的文本表示。
  virtual auto reference_string() const -> std::string {
    return get_actual()->name;
  }

  // 获取节点所属的模块。
  auto get_module() const -> Module* { return get_actual()->module; }

  // 设置节点所属的模块。
  void set_module(Module* m) { get_actual()->module = m; }

  friend auto operator<<(std::ostream& os, const Node& a) -> std::ostream&;

  // 检查该节点是否有替代节点。
  auto has_replacement() const -> bool { return replacement != nullptr; }

  // 处理节点使用的值。
  virtual auto get_used_values() -> std::vector<Value*> { return {}; }

  virtual auto get_used_values() const -> std::vector<const Value*> {
    return {};
  }

  virtual auto replace_used_value(id_t id, Value* new_value) -> int {
    return 0;
  }

  auto replace_used_value(Value* old, Value* new_value) -> int;

  // 处理节点使用的类型。
  virtual auto get_used_types() const -> std::vector<Types::Type*> {
    return {};
  }

  virtual auto replace_used_type(const std::string& name, Types::Type* new_type)
      -> int {
    return 0;
  }

  auto replace_used_type(Types::Type* old, Types::Type* new_type) -> int;

  // 处理节点使用的变量。
  virtual auto get_used_variables() -> std::vector<Var*> { return {}; }

  virtual auto get_used_variables() const -> std::vector<const Var*> {
    return {};
  }

  virtual auto replace_used_variable(id_t id, Var* new_var) -> int { return 0; }

  auto replace_used_variable(Var* old, Var* new_var) -> int;

  // 提供访问者模式的扩展功能。
  template <typename, typename>
  friend class AcceptorExtend;
  // 为节点提供可替换性，允许节点在不改变其在IR中的位置的情况下被替换。
  template <typename>
  friend class ReplaceableNodeBase;

 private:
  // 获取此节点的实际节点（考虑到可能的替代节点）。
  auto get_actual() -> Node* {
    return replacement ? replacement->get_actual() : this;
  }
  auto get_actual() const -> const Node* {
    return replacement ? replacement->get_actual() : this;
  }
};

// AcceptorExtend 和 ReplaceableNodeBase，它们用于构建一个面向对
// 象的中间表示（IR）系统，类似于LLVM使用的系统。这些类提供了运行
// 时类型识别（RTTI）、访问者模式支持以及节点替换功能。

// 这两个类的主要用途是在IR系统中提供灵活的节点处理机制。
// AcceptorExtend 通过访问者模式支持多态行为，而
// ReplaceableNodeBase 允许在优化过程中灵活地替换IR节点。
// 例如，一个编译器在优化阶段可能会用更高效的操作替换某些节点，
// 而这个替换操作可以通过这些类轻松实现。

// 在实际应用中，你可以创建一个节点类，继承自 ReplaceableNodeBase，
// 并为其提供特定于该节点类型的方法和属性。然后，你可以使用 accept
// 方法来实现节点特定的访问者行为，例如，用于代码生成或优化的逻辑。
// 如果需要，在优化过程中可以使用 replace_all 方法替换节点。

template <typename Derived, typename Parent>
class AcceptorExtend : public Parent {
 public:
  using Parent::Parent;

  static auto node_id() -> const void* { return &Derived::NodeId; }

  // 判断节点是否可以转换为其他类型的节点。
  // 如果有替换节点，会在替换节点上调用该方法。
  virtual auto is_convertible(const void* other) const -> bool {
    if (Node::has_replacement()) {
      return Node::get_actual()->is_convertible(other);
    }

    return other == node_id() || Parent::is_convertible(other);
  }

  // 接受访问者对象的方法。如果存在替换节点，将访问者转发给替换节点；
  // 否则，将自身传递给访问者。
  void accept(Util::Visitor& v) {
    if (Node::has_replacement()) {
      Node::get_actual()->accept(v);
    } else {
      v.visit(static_cast<Derived*>(this));
    }
  }

  void accept(Util::ConstVisitor& v) const {
    if (Node::has_replacement()) {
      Node::get_actual()->accept(v);
    } else {
      v.visit(static_cast<const Derived*>(this));
    }
  }
};

// 可替换节点的基础类。继承自 AcceptorExtend，添加了节点替换的功能。
template <typename Derived>
class ReplaceableNodeBase : public AcceptorExtend<Derived, Node> {
 private:
  // 指示节点是否可以被替换。
  bool replaceable = true;

 public:
  using AcceptorExtend<Derived, Node>::AcceptorExtend;

  static const char NodeId;

  // 获取当前节点的实际表示（考虑到可能的替换）。它返回 Derived 类型的指针。
  auto get_actual() -> Derived* {
    return Node::replacement
               ? static_cast<Derived*>(Node::replacement)->get_actual()
               : static_cast<Derived*>(this);
  }

  auto get_actual() const -> const Derived* {
    return Node::replacement
               ? static_cast<const Derived*>(Node::replacement)->get_actual()
               : static_cast<const Derived*>(this);
  }

  // 将该节点替换为另一个节点。只有当 replaceable 为真时，才可以调用此方法。
  void replace_all(Derived* v) {
    assert(replaceable && "node {} not replaceable" && *v);
    Node::replacement = v;
  }

  // 检查节点是否可以被替换。
  auto is_replaceable() const -> bool { return replaceable; }

  // 设置节点是否可以被替换。
  void set_replaceable(bool v = true) { replaceable = v; }
};

template <typename Derived>
const char ReplaceableNodeBase<Derived>::NodeId = 0;

template <typename Desired>
auto cast(Node* other) -> Desired* {
  return other != nullptr ? other->as<Desired>() : nullptr;
}

template <typename Desired>
auto cast(const Node* other) -> const Desired* {
  return other != nullptr ? other->as<Desired>() : nullptr;
}

template <typename Desired>
auto is_a(Node* other) -> bool {
  return other && other->is<Desired>();
}

template <typename Desired>
auto is_a(const Node* other) -> bool {
  return other && other->is<Desired>();
}

}  // namespace Pud::IR

#endif  // PUD_IR_BASE_H