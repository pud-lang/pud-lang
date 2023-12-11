#ifndef PUD_IR_VALUE_H
#define PUD_IR_VALUE_H

#include "Pud/IR/Base.h"
#include "Pud/IR/Types/Types.h"
#include "Pud/IR/Util/Packs.h"

namespace Pud::IR {

class Func;

// 在编译器的IR中代表一个值。
// 它是变量、常量、表达式结果的抽象表示，使得编译器可以构建和操作复杂的程序逻辑。
// 通过提供操作符重载和类型转换，它允许编译器在高级别上处理各种计算。
class Value : public ReplaceableNodeBase<Value>, public IdMixin {
 public:
  static const char NodeId;

  explicit Value(std::string name = "")
      : ReplaceableNodeBase(std::move(name)) {}

  virtual ~Value() noexcept = default;

  auto reference_string() const -> std::string final {
    return fmt::format(FMT_STRING("{}.{}"), get_name(), get_id());
  }

  // 返回此值所依赖的其他值的列表。
  auto get_used_values() -> std::vector<Value*> final {
    return get_actual()->do_get_used_values();
  }
  auto get_used_values() const -> std::vector<const Value*> final {
    auto ret = get_actual()->do_get_used_values();
    return std::vector<const Value*>(ret.begin(), ret.end());
  }
  // 允许替换使用中的值。
  auto replace_used_value(id_t id, Value* new_value) -> int final {
    return get_actual()->do_replace_used_value(id, new_value);
  }
  using Node::replace_used_value;

  // 返回此值所依赖的类型的列表。
  auto get_used_types() const -> std::vector<Types::Type*> final {
    return get_actual()->do_get_used_types();
  }
  // 允许替换使用中的类型。
  auto replace_used_type(const std::string& name, Types::Type* new_type)
      -> int final {
    return get_actual()->do_replace_used_types(name, new_type);
  }
  using Node::replace_used_type;

  // 返回此值所依赖的变量的列表。
  auto get_used_variables() -> std::vector<Var*> final {
    return get_actual()->do_get_used_variables();
  }
  auto get_used_variables() const -> std::vector<const Var*> final {
    auto ret = get_actual()->do_get_used_variables();
    return std::vector<const Var*>(ret.begin(), ret.end());
  }
  // 允许替换使用中的变量。
  auto replace_used_variable(id_t id, Var* new_var) -> int final {
    return get_actual()->do_replace_used_variable(id, new_var);
  }
  using Node::replace_used_variable;

  // 返回值的类型。
  auto get_type() const -> Types::Type* { return get_actual()->do_get_type(); }

  auto get_id() const -> id_t override { return get_actual()->id; }

  auto operator==(Value& other) -> Value*;
  auto operator!=(Value& other) -> Value*;
  auto operator<(Value& other) -> Value*;
  auto operator>(Value& other) -> Value*;
  auto operator<=(Value& other) -> Value*;
  auto operator>=(Value& other) -> Value*;

  auto operator+() -> Value*;
  auto operator-() -> Value*;
  auto operator~() -> Value*;

  auto operator+(Value& other) -> Value*;
  auto operator-(Value& other) -> Value*;
  auto operator*(Value& other) -> Value*;
  auto mat_mul(Value& other) -> Value*;
  auto true_div(Value& other) -> Value*;
  auto operator/(Value& other) -> Value*;
  auto operator%(Value& other) -> Value*;
  auto pow(Value& other) -> Value*;
  auto operator<<(Value& other) -> Value*;
  auto operator>>(Value& other) -> Value*;
  auto operator&(Value& other) -> Value*;
  auto operator|(Value& other) -> Value*;
  auto operator^(Value& other) -> Value*;

  auto operator||(Value& other) -> Value*;
  auto operator&&(Value& other) -> Value*;

  // 允许将 Value 实例作为函数调用，接受可变参数。
  template <typename... Args>
  auto operator()(Args&&... args) -> Value* {
    std::vector<Value*> dst;
    Util::strip_pack(dst, std::forward<Args>(args)...);
    return do_call(dst);
  }
  // 提供数组或映射访问的功能。
  auto operator[](Value& other) -> Value*;

  auto to_int() -> Value*;
  auto to_float() -> Value*;
  auto to_bool() -> Value*;
  auto to_str() -> Value*;

  // 对于集合类型的值，提供长度和迭代器的功能。
  auto len() -> Value*;
  auto iter() -> Value*;

 private:
  // 执行一元和二元操作。
  auto do_unary_op(const std::string& name) -> Value*;
  auto do_binary_op(const std::string& name, Value& other) -> Value*;

  // 处理函数调用。
  auto do_call(const std::vector<Value*>& args) -> Value*;

  virtual auto do_get_type() const -> Types::Type* = 0;

  virtual auto do_get_used_values() const -> std::vector<Value*> { return {}; }
  virtual auto do_replace_used_value(id_t id, Value* new_value) -> int {
    return 0;
  }

  virtual auto do_get_used_types() const -> std::vector<Types::Type*> {
    return {};
  }
  virtual auto do_replace_used_types(const std::string& name,
                                     Types::Type* new_type) -> int {
    return 0;
  }

  virtual auto do_get_used_variables() const -> std::vector<Var*> { return {}; }
  virtual auto do_replace_used_variable(id_t id, Var* new_var) -> int {
    return 0;
  }
};
}  // namespace Pud::IR

#endif  // PUD_IR_VALUE_H