#ifndef PUD_IR_CONST_H
#define PUD_IR_CONST_H

#include "Pud/IR/Module.h"
#include "Pud/IR/Value.h"

namespace Pud::IR {

/// CIR constant base. Once created, constants are immutable.
class Const : public AcceptorExtend<Const, Value> {
 private:
  /// the type
  Types::Type* type;

 public:
  static const char NodeId;

  /// Constructs a constant.
  /// @param type the type
  /// @param name the name
  explicit Const(Types::Type* type, std::string name = "")
      : AcceptorExtend(std::move(name)), type(type) {}

 private:
  Types::Type* do_get_type() const override { return type; }

  std::vector<Types::Type*> do_get_used_types() const override {
    return {type};
  }
  int do_replace_used_type(const std::string& name,
                           Types::Type* new_type) override;
};

template <typename ValueType>
class TemplatedConst : public AcceptorExtend<TemplatedConst<ValueType>, Const> {
 private:
  ValueType val;

 public:
  static const char NodeId;

  using AcceptorExtend<TemplatedConst<ValueType>, Const>::get_module;
  using AcceptorExtend<TemplatedConst<ValueType>, Const>::get_source_info;
  using AcceptorExtend<TemplatedConst<ValueType>, Const>::get_type;

  TemplatedConst(ValueType v, Types::Type* type, std::string name = "")
      : AcceptorExtend<TemplatedConst<ValueType>, Const>(type, std::move(name)),
        val(v) {}

  /// @return the internal value.
  ValueType get_val() const { return val; }
  /// Sets the value.
  /// @param v the value
  void set_val(ValueType v) { val = v; }
};

using IntConst = TemplatedConst<int64_t>;
using FloatConst = TemplatedConst<double>;
using BoolConst = TemplatedConst<bool>;
using StringConst = TemplatedConst<std::string>;

template <typename T>
const char TemplatedConst<T>::NodeId = 0;

template <>
class TemplatedConst<std::string>
    : public AcceptorExtend<TemplatedConst<std::string>, Const> {
 private:
  std::string val;

 public:
  static const char NodeId;

  TemplatedConst(std::string v, Types::Type* type, std::string name = "")
      : AcceptorExtend(type, std::move(name)), val(std::move(v)) {}

  /// @return the internal value.
  std::string get_val() const { return val; }
  /// Sets the value.
  /// @param v the value
  void set_val(std::string v) { val = std::move(v); }
};

}  // namespace Pud::IR

#endif  // PUD_IR_CONST_H