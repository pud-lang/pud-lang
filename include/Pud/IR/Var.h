#ifndef PUD_IR_VAR_H
#define PUD_IR_VAR_H

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "Pud/IR/Types/Types.h"
#include "Pud/IR/Value.h"

namespace Pud::IR {

class Func;

// 代表一个变量
class Var : public ReplaceableNodeBase<Var>, public IdMixin {
 private:
  // 变量的类型
  Types::Type* type;
  // 如果变量是全局的，则为 true。
  bool global;
  // 如果变量是外部的（例如来自外部库或外部声明），则为 true。
  bool external;

 public:
  static const char NodeId;

  explicit Var(Types::Type* type, bool global = false, bool external = false,
               std::string name = "")
      : ReplaceableNodeBase(std::move(name)),
        type(type),
        global(global),
        external(external) {}

  virtual ~Var() noexcept = default;

  auto get_used_values() -> std::vector<Value*> final {
    return get_actual()->do_get_used_values();
  }
  auto get_used_values() const -> std::vector<const Value*> final {
    auto ret = get_actual()->do_get_used_values();
    return std::vector<const Value*>(ret.begin(), ret.end());
  }
  auto replace_used_value(id_t id, Value* new_value) -> int final {
    return do_replace_used_value(id, new_value);
  }
  using Node::replace_used_value;

  auto get_used_types() const -> std::vector<Types::Type*> final {
    return get_actual()->do_get_used_types();
  }
  auto replace_used_type(const std::string& name, Types::Type* new_type)
      -> int final {
    return get_actual()->do_replace_used_type(name, new_type);
  }
  using Node::replace_used_type;

  auto get_used_variables() -> std::vector<Var*> final {
    return do_get_used_variables();
  }
  auto get_used_variables() const -> std::vector<const Var*> final {
    auto ret = do_get_used_variables();
    return std::vector<const Var*>(ret.begin(), ret.end());
  }
  auto replace_used_variable(id_t id, Var* new_var) -> int final {
    return get_actual()->do_replace_used_variable(id, new_var);
  }
  using Node::replace_used_variable;

  auto get_type() const -> Types::Type* { return get_actual()->type; }

  void set_type(Types::Type* t) { get_actual()->type = t; }

  auto is_global() const -> bool { return get_actual()->global; }

  void set_global(bool v = true) { get_actual()->global = v; }

  auto is_external() const -> bool { return get_actual()->external; }

  void set_external(bool v = true) { get_actual()->external = v; }

  auto reference_string() const -> std::string final {
    return fmt::format(FMT_STRING("{}.{}"), get_name(), get_id());
  }

  auto get_id() const -> id_t override { return get_actual()->id; }

 protected:
  virtual auto do_get_used_values() const -> std::vector<Value*> { return {}; }
  virtual auto do_replace_used_value(id_t id, Value* new_value) -> int {
    return 0;
  }

  virtual auto do_get_used_types() const -> std::vector<Types::Type*> {
    return {type};
  }
  virtual auto do_replace_used_type(const std::string& name,
                                    Types::Type* new_type) -> int;

  virtual auto do_get_used_variables() const -> std::vector<Var*> { return {}; }
  virtual auto do_replace_used_variable(id_t id, Var* new_var) -> int {
    return 0;
  }
};

// 代表一个包含未拥有变量引用的值。
class VarValue : public AcceptorExtend<VarValue, Value> {
 private:
  // 指向被引用的变量。
  Var* val;

 public:
  static const char NodeId;

  explicit VarValue(Var* val, std::string name = "")
      : AcceptorExtend(std::move(name)), val(val) {}

  auto get_var() -> Var* { return val; }

  auto get_var() const -> const Var* { return val; }

  void set_var(Var* v) { val = v; }

 private:
  auto do_get_type() const -> Types::Type* override { return val->get_type(); }

  auto do_get_used_variables() const -> std::vector<Var*> override {
    return {val};
  }
  auto do_replace_used_variable(id_t id, Var* new_var) -> int override;
};

// 类似于 VarValue，但代表一个指针值。
class PointerValue : public AcceptorExtend<PointerValue, Value> {
 private:
  // 指向被引用的变量。
  Var* val;

 public:
  static const char NodeId;

  explicit PointerValue(Var* val, std::string name = "")
      : AcceptorExtend(std::move(name)), val(val) {}

  auto get_var() -> Var* { return val; }

  auto get_var() const -> const Var* { return val; }

  void set_var(Var* v) { val = v; }

 private:
  auto do_get_type() const -> Types::Type* override;

  auto do_get_used_variables() const -> std::vector<Var*> override {
    return {val};
  }
  auto do_replace_used_variable(id_t id, Var* new_var) -> int override;
};

}  // namespace Pud::IR

#endif  // PUD_IR_VAR_H