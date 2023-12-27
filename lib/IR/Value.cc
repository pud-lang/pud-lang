#include "Pud/IR/Value.h"

#include "Pud/IR/Instr.h"
#include "Pud/IR/Module.h"

namespace Pud::IR {

const char Value::NodeId = 0;

auto Value::operator==(Value& other) -> Value* {
  return do_binary_op(Module::EQ_MAGIC_NAME, other);
}

auto Value::operator!=(Value& other) -> Value* {
  return do_binary_op(Module::NE_MAGIC_NAME, other);
}

auto Value::operator<(Value& other) -> Value* {
  return do_binary_op(Module::LT_MAGIC_NAME, other);
}

auto Value::operator>(Value& other) -> Value* {
  return do_binary_op(Module::GT_MAGIC_NAME, other);
}

auto Value::operator<=(Value& other) -> Value* {
  return do_binary_op(Module::LE_MAGIC_NAME, other);
}

auto Value::operator>=(Value& other) -> Value* {
  return do_binary_op(Module::GE_MAGIC_NAME, other);
}

auto Value::operator+() -> Value* {
  return do_unary_op(Module::POS_MAGIC_NAME);
}

auto Value::operator-() -> Value* {
  return do_unary_op(Module::NEG_MAGIC_NAME);
}

auto Value::operator~() -> Value* {
  return do_unary_op(Module::INVERT_MAGIC_NAME);
}

auto Value::operator+(Value& other) -> Value* {
  return do_binary_op(Module::ADD_MAGIC_NAME, other);
}

auto Value::operator-(Value& other) -> Value* {
  return do_binary_op(Module::SUB_MAGIC_NAME, other);
}

auto Value::operator*(Value& other) -> Value* {
  return do_binary_op(Module::MUL_MAGIC_NAME, other);
}

auto Value::mat_mul(Value& other) -> Value* {
  return do_binary_op(Module::MATMUL_MAGIC_NAME, other);
}

auto Value::true_div(Value& other) -> Value* {
  return do_binary_op(Module::TRUE_DIV_MAGIC_NAME, other);
}

auto Value::operator/(Value& other) -> Value* {
  return do_binary_op(Module::FLOOR_DIV_MAGIC_NAME, other);
}

auto Value::operator%(Value& other) -> Value* {
  return do_binary_op(Module::MOD_MAGIC_NAME, other);
}

auto Value::pow(Value& other) -> Value* {
  return do_binary_op(Module::POW_MAGIC_NAME, other);
}

auto Value::operator<<(Value& other) -> Value* {
  return do_binary_op(Module::LSHIFT_MAGIC_NAME, other);
}

auto Value::operator>>(Value& other) -> Value* {
  return do_binary_op(Module::RSHIFT_MAGIC_NAME, other);
}

auto Value::operator&(Value& other) -> Value* {
  return do_binary_op(Module::AND_MAGIC_NAME, other);
}

auto Value::operator|(Value& other) -> Value* {
  return do_binary_op(Module::OR_MAGIC_NAME, other);
}

auto Value::operator^(Value& other) -> Value* {
  return do_binary_op(Module::XOR_MAGIC_NAME, other);
}

auto Value::operator||(Value& other) -> Value* {
  auto* module = get_module();
  return module->Nr<TernaryInstr>(to_bool(), module->get_bool(true),
                                  other.to_bool());
}

auto Value::operator&&(Value& other) -> Value* {
  auto* module = get_module();
  return module->Nr<TernaryInstr>(to_bool(), other.to_bool(),
                                  module->get_bool(false));
}

auto Value::operator[](Value& other) -> Value* {
  return do_binary_op(Module::GETITEM_MAGIC_NAME, other);
}

auto Value::to_int() -> Value* { return do_unary_op(Module::INT_MAGIC_NAME); }

auto Value::to_float() -> Value* {
  return do_unary_op(Module::FLOAT_MAGIC_NAME);
}

auto Value::to_bool() -> Value* { return do_unary_op(Module::BOOL_MAGIC_NAME); }

auto Value::to_str() -> Value* { return do_unary_op(Module::REPR_MAGIC_NAME); }

auto Value::len() -> Value* { return do_unary_op(Module::LEN_MAGIC_NAME); }

auto Value::iter() -> Value* { return do_unary_op(Module::ITER_MAGIC_NAME); }

auto Value::do_unary_op(const std::string& name) -> Value* {
  auto* module = get_module();
  auto* fn = module->get_or_realize_method(
      get_type(), name, std::vector<Types::Type*>{get_type()});

  if (!fn)
    return nullptr;

  auto* fn_val = module->Nr<VarValue>(fn);
  return (*fn_val)(*this);
}

auto Value::do_binary_op(const std::string& name, Value& other) -> Value* {
  auto* module = get_module();
  auto* fn = module->get_or_realize_method(
      get_type(), name,
      std::vector<Types::Type*>{get_type(), other.get_type()});

  if (!fn)
    return nullptr;

  auto* fn_val = module->Nr<VarValue>(fn);
  return (*fn_val)(*this, other);
}

auto Value::do_call(const std::vector<Value*>& args) -> Value* {
  auto* module = get_module();
  return module->Nr<CallInstr>(this, args);
}

}  // namespace Pud::IR