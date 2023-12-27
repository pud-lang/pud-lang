#include "Pud/IR/Var.h"

#include "Pud/IR/Module.h"

namespace Pud::IR {

const char Var::NodeId = 0;

auto Var::do_replace_used_type(const std::string& name, Types::Type* new_type)
    -> int {
  if (type->get_name() == name) {
    type = new_type;
    return 1;
  }
  return 0;
}

const char VarValue::NodeId = 0;

auto VarValue::do_replace_used_variable(id_t id, Var* new_var) -> int {
  if (val->get_id() == id) {
    val = new_var;
    return 1;
  }
  return 0;
}

const char PointerValue::NodeId = 0;

auto PointerValue::do_get_type() const -> Types::Type* {
  return get_module()->get_pointer_type(val->get_type());
}

auto PointerValue::do_replace_used_variable(id_t id, Var* new_var) -> int {
  if (val->get_id() == id) {
    val = new_var;
    return 1;
  }
  return 0;
}

}  // namespace Pud::IR