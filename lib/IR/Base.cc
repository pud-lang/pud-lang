#include "Pud/IR/Base.h"

#include "Pud/IR/Types/Types.h"
#include "Pud/IR/Util/Format.h"
#include "Pud/IR/Value.h"
#include "Pud/IR/Var.h"

namespace Pud::IR {

id_t IdMixin::current_id = 0;

void IdMixin::reset_id() { current_id = 0; }

const char Node::NodeId = 0;

auto operator<<(std::ostream& os, const Node& other) -> std::ostream& {
  return Util::format(os, &other);
}

auto Node::replace_used_value(Value* old, Value* new_value) -> int {
  return replace_used_value(old->get_id(), new_value);
}

auto Node::replace_used_type(Types::Type* old, Types::Type* new_type) -> int {
  return replace_used_type(old->get_name(), new_type);
}

auto Node::replace_used_variable(Var* old, Var* new_var) -> int {
  return replace_used_variable(old->get_id(), new_var);
}

}  // namespace Pud::IR