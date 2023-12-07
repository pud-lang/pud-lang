#ifndef PUD_IR_TYPES_TYPES_H
#define PUD_IR_TYPES_TYPES_H

#include <algorithm>
#include <cstring>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/IR/Base.h"
#include "Pud/IR/Util/Packs.h"
#include "Pud/IR/Util/Visitor.h"

namespace Pud::IR {

class Value;

namespace Types {

class Type;

// 用于存储不同类型的值：静态整数、静态字符串或Type 对象。
// 它基本上是一个可存储不同类型数据的通用容器。
// 在类型系统中用于表示泛型或多态性质，允许在运行时处理不同类型的值。
class Generic {
 private:
  // 用于存储不同类型的值（静态整数、静态字符串或类型指针）。
  union {
    int64_t static_value;
    char* static_string_value;
    Types::Type* type_value;
  } value;
  // 标识 Generic 实例当前存储的值类型。
  enum { STATIC, STATIC_STR, TYPE } tag;

 public:
  Generic(int64_t static_value) : value(), tag(STATIC) {
    value.static_value = static_value;
  }
  Generic(const std::string& static_value) : value(), tag(STATIC_STR) {
    value.static_string_value = new char[static_value.size() + 1];
    strncpy(value.static_string_value, static_value.data(),
            static_value.size());
    value.static_string_value[static_value.size()] = 0;
  }
  Generic(Types::Type* type_value) : value(), tag(TYPE) {
    value.type_value = type_value;
  }
  Generic(const Types::Generic&) = default;
  ~Generic() {
    // if (tag == STATIC_STR)
    //   delete[] value.static_string_value;
  }

  // 检查 Generic 对象存储的值类型。
  auto is_type() const -> bool { return tag == TYPE; }

  auto is_static() const -> bool { return tag == STATIC; }

  auto is_static_str() const -> bool { return tag == STATIC_STR; }

  // 获取存储的值。
  auto get_static_value() const -> int64_t { return value.static_value; }

  auto get_static_string_value() const -> std::string {
    return value.static_string_value;
  }

  auto get_type_value() const -> Types::Type* { return value.type_value; }
};

// Type 类是编译器中间表示(IR)中所有类型的基类，提供通用的类型操作和属性。
// 在IR中，Type 类及其派生类用于精确地表示程序中使用的数据类型。
// 这对于正确生成和优化代码至关重要。
// 例如，了解一个变量是否是原子类型可以帮助编译器优化内存管理和垃圾回收策略。
// 此外，类型信息对于函数调用、类型转换和其他许多编译时和运行时操作是必不可少的。
class Type : public ReplaceableNodeBase<Type> {
 private:
  // 与此类型相关的抽象语法树(AST)节点。
  Pud::Type::TypePtr ast_type;

 public:
  static const char NodeId;

  using ReplaceableNodeBase::ReplaceableNodeBase;

  virtual ~Type() noexcept = default;

  std::vector<Type*> get_used_types() const final {
    return get_actual()->do_get_used_types();
  }
  auto replace_used_type(const std::string& name, Type* newType) -> int final {
    assert(false && "types not replaceable");
    return -1;
  }
  using Node::replace_used_type;

  // 检查这个 Type 是否与另一个 Type 相同。
  auto is(Types::Type* other) const -> bool {
    return get_name() == other->get_name();
  }

  // 检查类型是否原子，即是否不包含指向动态分配内存的指针，这对垃圾回收很重要。
  auto is_atomic() const -> bool { return get_actual()->do_is_atomic(); }

  auto is_content_atomic() const -> bool {
    return get_actual()->do_is_content_atomic();
  }

  // 获取与此类型相关的AST节点。
  auto get_ast_type() const -> Pud::Type::TypePtr {
    return get_actual()->ast_type;
  }
  // 设置与此类型相关的AST节点。
  void set_ast_type(Pud::Type::TypePtr t) {
    get_actual()->ast_type = std::move(t);
  }

  // 获取此类型使用的泛型。
  auto get_generics() const -> std::vector<Generic> {
    return get_actual()->do_get_generics();
  }

  // 使用提供的参数构造此类型的一个实例。
  auto construct(std::vector<Value*> args) -> Value* {
    return get_actual()->do_construct(std::move(args));
  }
  template <typename... Args>
  auto operator()(Args&&... args) -> Value* {
    std::vector<Value*> dst;
    Util::strip_pack(dst, std::forward<Args>(args)...);
    return construct(dst);
  }

 private:
  virtual auto do_get_generics() const -> std::vector<Generic>;

  virtual auto do_get_used_types() const -> std::vector<Type*> { return {}; }
  virtual auto do_is_atomic() const -> bool = 0;
  virtual auto do_is_content_atomic() const -> bool { return true; }

  virtual auto do_construct(std::vector<Value*> args) -> Value*;
};

}  // namespace Types

}  // namespace Pud::IR

#endif  // PUD_IR_TYPES_TYPES_H