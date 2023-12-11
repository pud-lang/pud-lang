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

  auto get_used_types() const -> std::vector<Type*> final {
    return get_actual()->do_get_used_types();
  }
  auto replace_used_type(const std::string& name, Type* new_type) -> int final {
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

// 派生所有原始（基础）数据类型。
class PrimitiveType : public AcceptorExtend<PrimitiveType, Type> {
 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

 private:
  auto do_is_atomic() const -> bool final { return true; }
};

// Int type (64-bit signed integer)
class IntType : public AcceptorExtend<IntType, PrimitiveType> {
 public:
  static const char NodeId;

  IntType() : AcceptorExtend("int") {}
};

// Float type (64-bit double)
class FloatType : public AcceptorExtend<FloatType, PrimitiveType> {
 public:
  static const char NodeId;

  FloatType() : AcceptorExtend("float") {}
};

// Float32 type (32-bit float)
class Float32Type : public AcceptorExtend<Float32Type, PrimitiveType> {
 public:
  static const char NodeId;

  Float32Type() : AcceptorExtend("float32") {}
};

// Float16 type (16-bit float)
class Float16Type : public AcceptorExtend<Float16Type, PrimitiveType> {
 public:
  static const char NodeId;

  Float16Type() : AcceptorExtend("float16") {}
};

// BFloat16 type (16-bit brain float)
class BFloat16Type : public AcceptorExtend<BFloat16Type, PrimitiveType> {
 public:
  static const char NodeId;

  BFloat16Type() : AcceptorExtend("bfloat16") {}
};

// Float128 type (128-bit float)
class Float128Type : public AcceptorExtend<Float128Type, PrimitiveType> {
 public:
  static const char NodeId;

  Float128Type() : AcceptorExtend("float128") {}
};

// Bool type (8-bit unsigned integer; either 0 or 1)
class BoolType : public AcceptorExtend<BoolType, PrimitiveType> {
 public:
  static const char NodeId;

  BoolType() : AcceptorExtend("bool") {}
};

// Byte type (8-bit unsigned integer)
class ByteType : public AcceptorExtend<ByteType, PrimitiveType> {
 public:
  static const char NodeId;

  ByteType() : AcceptorExtend("byte") {}
};

// Void type
class VoidType : public AcceptorExtend<VoidType, PrimitiveType> {
 public:
  static const char NodeId;

  VoidType() : AcceptorExtend("void") {}
};

// 用于派生包含字段或成员的复合数据类型。
class MemberedType : public AcceptorExtend<MemberedType, Type> {
 public:
  static const char NodeId;

  class Field {
   private:
    std::string name;
    Type* type;

   public:
    Field(std::string name, Type* type) : name(std::move(name)), type(type) {}

    auto get_name() const -> const std::string& { return name; }

    auto get_type() const -> Type* { return type; }
  };

  using const_iterator = std::vector<Field>::const_iterator;
  using const_reference = std::vector<Field>::const_reference;

  explicit MemberedType(std::string name) : AcceptorExtend(std::move(name)) {}

  virtual auto get_member_type(const std::string& name) const -> Type* = 0;

  virtual auto get_member_index(const std::string& name) const -> int = 0;

  virtual auto begin() const -> const_iterator = 0;

  virtual auto end() const -> const_iterator = 0;

  virtual auto front() const -> const_reference = 0;

  virtual auto back() const -> const_reference = 0;

  virtual void realize(std::vector<Type*> types,
                       std::vector<std::string> names) = 0;
};

// 表示类似于C结构体的数据类型，拥有一系列字段。
class RecordType : public AcceptorExtend<RecordType, MemberedType> {
 private:
  std::vector<Field> fields;

 public:
  static const char NodeId;

  RecordType(std::string name, std::vector<Type*> field_types,
             std::vector<std::string> field_names);

  RecordType(std::string name, std::vector<Type*> types);

  explicit RecordType(std::string name) : AcceptorExtend(std::move(name)) {}

  auto get_member_type(const std::string& n) const -> Type* override;
  auto get_member_index(const std::string& n) const -> int override;

  auto begin() const -> const_iterator override { return fields.begin(); }
  auto end() const -> const_iterator override { return fields.end(); }
  auto front() const -> const_reference override { return fields.front(); }
  auto back() const -> const_reference override { return fields.back(); }

  void realize(std::vector<Type*> types,
               std::vector<std::string> names) override;

 private:
  auto do_get_used_types() const -> std::vector<Type*> override;

  auto do_is_atomic() const -> bool override {
    return !std::any_of(fields.begin(), fields.end(), [](auto& field) {
      return !field.get_type()->is_atomic();
    });
  }
};

// 表示引用类型，类似于Python类，包含内部结构和可能的多态性。
class RefType : public AcceptorExtend<RefType, MemberedType> {
 private:
  /// the internal contents of the type
  Type* contents;
  /// true if type is polymorphic and needs RTTI
  bool polymorphic;

 public:
  static const char NodeId;

  RefType(std::string name, RecordType* contents, bool polymorphic = false)
      : AcceptorExtend(std::move(name)),
        contents(contents),
        polymorphic(polymorphic) {}

  auto is_polymorphic() const -> bool { return polymorphic; }

  void set_polymorphic(bool p = true) { polymorphic = p; }

  auto get_member_type(const std::string& n) const -> Type* override {
    return get_contents()->get_member_type(n);
  }
  auto get_member_index(const std::string& n) const -> int override {
    return get_contents()->get_member_index(n);
  }

  auto begin() const -> const_iterator override {
    return get_contents()->begin();
  }
  auto end() const -> const_iterator override { return get_contents()->end(); }
  auto front() const -> const_reference override {
    return get_contents()->front();
  }
  auto back() const -> const_reference override {
    return get_contents()->back();
  }

  auto get_contents() const -> RecordType* {
    return cast<RecordType>(contents);
  }

  void set_contents(RecordType* t) { contents = t; }

  void realize(std::vector<Type*> types,
               std::vector<std::string> names) override {
    get_contents()->realize(std::move(types), std::move(names));
  }

 private:
  auto do_get_used_types() const -> std::vector<Type*> override {
    return {contents};
  }

  auto do_is_atomic() const -> bool override { return false; }

  auto do_is_content_atomic() const -> bool override;

  auto do_construct(std::vector<Value*> args) -> Value* override;
};

// 表示函数类型，包括返回类型和参数类型。
class FuncType : public AcceptorExtend<FuncType, Type> {
 public:
  using const_iterator = std::vector<Type*>::const_iterator;
  using const_reference = std::vector<Type*>::const_reference;

 private:
  /// return type
  Type* ret_type;
  /// argument types
  std::vector<Type*> arg_types;
  /// whether the function is variadic (e.g. "printf" in C)
  bool variadic;

 public:
  static const char NodeId;

  FuncType(std::string name, Type* ret_type, std::vector<Type*> arg_types,
           bool variadic = false)
      : AcceptorExtend(std::move(name)),
        ret_type(ret_type),
        arg_types(std::move(arg_types)),
        variadic(variadic) {}

  auto get_return_type() const -> Type* { return ret_type; }

  auto is_variadic() const -> bool { return variadic; }

  auto begin() const -> const_iterator { return arg_types.begin(); }
  auto end() const -> const_iterator { return arg_types.end(); }
  auto front() const -> const_reference { return arg_types.front(); }
  auto back() const -> const_reference { return arg_types.back(); }

 private:
  auto do_get_generics() const -> std::vector<Generic> override;

  auto do_get_used_types() const -> std::vector<Type*> override;

  auto do_is_atomic() const -> bool override { return false; }
};

// 用于派生从另一个类型派生的类型，如指针或可选类型。
class DerivedType : public AcceptorExtend<DerivedType, Type> {
 private:
  /// the base type
  Type* base;

 public:
  static const char NodeId;

  explicit DerivedType(std::string name, Type* base)
      : AcceptorExtend(std::move(name)), base(base) {}

  /// @return the type's base
  auto get_base() const -> Type* { return base; }

 private:
  auto do_is_atomic() const -> bool override { return base->is_atomic(); }

  auto do_get_used_types() const -> std::vector<Type*> override {
    return {base};
  }
};

/// Type of a pointer to another CIR type
class PointerType : public AcceptorExtend<PointerType, DerivedType> {
 public:
  static const char NodeId;

  /// Constructs a pointer type.
  /// @param base the type's base
  explicit PointerType(Type* base)
      : AcceptorExtend(get_instance_name(base), base) {}

  static auto get_instance_name(Type* base) -> std::string;

 private:
  auto do_is_atomic() const -> bool override { return false; }
};

/// Type of an optional containing another CIR type
class OptionalType : public AcceptorExtend<OptionalType, DerivedType> {
 public:
  static const char NodeId;

  /// Constructs an optional type.
  /// @param base the type's base
  explicit OptionalType(Type* base)
      : AcceptorExtend(get_instance_name(base), base) {}

  static auto get_instance_name(Type* base) -> std::string;

 private:
  auto do_is_atomic() const -> bool override { return get_base()->is_atomic(); }
};

/// Type of a generator yielding another CIR type
class GeneratorType : public AcceptorExtend<GeneratorType, DerivedType> {
 public:
  static const char NodeId;

  /// Constructs a generator type.
  /// @param base the type's base
  explicit GeneratorType(Type* base)
      : AcceptorExtend(get_instance_name(base), base) {}

  static auto get_instance_name(Type* base) -> std::string;

 private:
  auto do_is_atomic() const -> bool override { return false; }
};

/// Type of a variably sized integer
class IntNType : public AcceptorExtend<IntNType, PrimitiveType> {
 private:
  /// length of the integer
  unsigned len;
  /// whether the variable is signed
  bool sign;

 public:
  static const char NodeId;

  static const unsigned MAX_LEN = 2048;

  /// Constructs a variably sized integer type.
  /// @param len the length of the integer
  /// @param sign true if signed, false otherwise
  IntNType(unsigned len, bool sign)
      : AcceptorExtend(get_instance_name(len, sign)), len(len), sign(sign) {}

  /// @return the length of the integer
  auto get_len() const -> unsigned { return len; }
  /// @return true if signed
  auto is_signed() const -> bool { return sign; }

  /// @return the name of the opposite signed corresponding type
  auto opposite_sign_name() const -> std::string {
    return get_instance_name(len, !sign);
  }

  static auto get_instance_name(unsigned len, bool sign) -> std::string;
};

/// Type of a vector of primitives
class VectorType : public AcceptorExtend<VectorType, PrimitiveType> {
 private:
  /// number of elements
  unsigned count;
  /// base type
  PrimitiveType* base;

 public:
  static const char NodeId;

  /// Constructs a vector type.
  /// @param count the number of elements
  /// @param base the base type
  VectorType(unsigned count, PrimitiveType* base)
      : AcceptorExtend(get_instance_name(count, base)),
        count(count),
        base(base) {}

  /// @return the count of the vector
  auto get_count() const -> unsigned { return count; }
  /// @return the base type of the vector
  auto get_base() const -> PrimitiveType* { return base; }

  static auto get_instance_name(unsigned count, PrimitiveType* base)
      -> std::string;
};

class UnionType : public AcceptorExtend<UnionType, Type> {
 private:
  /// alternative types
  std::vector<Types::Type*> types;

 public:
  static const char NodeId;

  using const_iterator = std::vector<Types::Type*>::const_iterator;
  using const_reference = std::vector<Types::Type*>::const_reference;

  /// Constructs a UnionType.
  /// @param types the alternative types (must be sorted by caller)
  explicit UnionType(std::vector<Types::Type*> types)
      : AcceptorExtend(), types(std::move(types)) {}

  auto begin() const -> const_iterator { return types.begin(); }
  auto end() const -> const_iterator { return types.end(); }
  auto front() const -> const_reference { return types.front(); }
  auto back() const -> const_reference { return types.back(); }

  static auto get_instance_name(const std::vector<Types::Type*>& types)
      -> std::string;

 private:
  auto do_get_used_types() const -> std::vector<Types::Type*> override {
    return types;
  }

  auto do_is_atomic() const -> bool override {
    return !std::any_of(types.begin(), types.end(),
                        [](auto* type) { return !type->is_atomic(); });
  }
};

}  // namespace Types

}  // namespace Pud::IR

#endif  // PUD_IR_TYPES_TYPES_H