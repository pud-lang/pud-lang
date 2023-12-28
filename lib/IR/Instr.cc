#include "Pud/IR/Instr.h"

#include "Pud/Common/Common.h"
#include "Pud/IR/Module.h"
#include "Pud/IR/Util/Iterators.h"

namespace Pud::IR {
namespace {
auto find_and_replace(id_t id, Pud::IR::Value* new_val,
                      std::vector<Pud::IR::Value*>& values) -> int {
  auto replacements = 0;
  for (auto& value : values) {
    if (value->get_id() == id) {
      value = new_val;
      ++replacements;
    }
  }
  return replacements;
}
}  // namespace

const char Instr::NodeId = 0;

auto Instr::do_get_type() const -> Types::Type* {
  return get_module()->get_none_type();
}

const char AssignInstr::NodeId = 0;

auto AssignInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  if (rhs->get_id() == id) {
    rhs = new_value;
    return 1;
  }
  return 0;
}

auto AssignInstr::do_replace_used_variable(id_t id, Var* new_var) -> int {
  if (lhs->get_id() == id) {
    lhs = new_var;
    return 1;
  }
  return 0;
}

const char ExtractInstr::NodeId = 0;

auto ExtractInstr::do_get_type() const -> Types::Type* {
  auto* membered_type = cast<Types::MemberedType>(val->get_type());
  seqassert(membered_type, "{} is not a membered type", *val->get_type());
  return membered_type->get_member_type(field);
}

auto ExtractInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  if (val->get_id() == id) {
    val = new_value;
    return 1;
  }
  return 0;
}

const char InsertInstr::NodeId = 0;

auto InsertInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  auto replacements = 0;
  if (lhs->get_id() == id) {
    lhs = new_value;
    ++replacements;
  }
  if (rhs->get_id() == id) {
    rhs = new_value;
    ++replacements;
  }
  return replacements;
}

const char CallInstr::NodeId = 0;

auto CallInstr::do_get_type() const -> Types::Type* {
  auto* func_type = cast<Types::FuncType>(callee->get_type());
  seqassert(func_type, "{} is not a function type", *callee->get_type());
  return func_type->get_return_type();
}

auto CallInstr::do_get_used_values() const -> std::vector<Value*> {
  std::vector<Value*> ret(args.begin(), args.end());
  ret.push_back(callee);
  return ret;
}

auto CallInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  auto replacements = 0;
  if (callee->get_id() == id) {
    callee = new_value;
    ++replacements;
  }
  replacements += find_and_replace(id, new_value, args);
  return replacements;
}

const char StackAllocInstr::NodeId = 0;

auto StackAllocInstr::do_replace_used_type(const std::string& name,
                                           Types::Type* new_type) -> int {
  if (array_type->get_name() == name) {
    array_type = new_type;
    return 1;
  }
  return 0;
}

const char TypePropertyInstr::NodeId = 0;

auto TypePropertyInstr::do_get_type() const -> Types::Type* {
  switch (property) {
    case Property::IS_ATOMIC:
      return get_module()->get_bool_type();
    case Property::IS_CONTENT_ATOMIC:
      return get_module()->get_bool_type();
    case Property::SIZEOF:
      return get_module()->get_int_type();
    default:
      return get_module()->get_none_type();
  }
}

auto TypePropertyInstr::do_replace_used_type(const std::string& name,
                                             Types::Type* new_type) -> int {
  if (inspect_type->get_name() == name) {
    inspect_type = new_type;
    return 1;
  }
  return 0;
}

const char YieldInInstr::NodeId = 0;

auto YieldInInstr::do_replace_used_type(const std::string& name,
                                        Types::Type* new_type) -> int {
  if (type->get_name() == name) {
    type = new_type;
    return 1;
  }
  return 0;
}

const char TernaryInstr::NodeId = 0;

auto TernaryInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  auto replacements = 0;
  if (cond->get_id() == id) {
    cond = new_value;
    ++replacements;
  }
  if (true_value->get_id() == id) {
    true_value = new_value;
    ++replacements;
  }
  if (false_value->get_id() == id) {
    false_value = new_value;
    ++replacements;
  }
  return replacements;
}

const char ControlFlowInstr::NodeId = 0;

const char BreakInstr::NodeId = 0;

auto BreakInstr::do_get_used_values() const -> std::vector<Value*> {
  if (loop)
    return {loop};
  return {};
}

auto BreakInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  if (loop && loop->get_id() == id) {
    auto* f = cast<Flow>(new_value);
    seqassert(f, "{} is not a flow", *new_value);
    loop = f;
    return 1;
  }
  return 0;
}

const char ContinueInstr::NodeId = 0;

auto ContinueInstr::do_get_used_values() const -> std::vector<Value*> {
  if (loop)
    return {loop};
  return {};
}

auto ContinueInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  if (loop && loop->get_id() == id) {
    auto* f = cast<Flow>(new_value);
    seqassert(f, "{} is not a flow", *new_value);
    loop = f;
    return 1;
  }
  return 0;
}

const char ReturnInstr::NodeId = 0;

auto ReturnInstr::do_get_used_values() const -> std::vector<Value*> {
  if (value)
    return {value};
  return {};
}

auto ReturnInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  auto replacements = 0;
  if (value && value->get_id() == id) {
    set_value(new_value);
    ++replacements;
  }
  return replacements;
}

const char YieldInstr::NodeId = 0;

auto YieldInstr::do_get_used_values() const -> std::vector<Value*> {
  if (value)
    return {value};
  return {};
}

auto YieldInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  if (value && value->get_id() == id) {
    set_value(new_value);
    return 1;
  }
  return 0;
}

const char ThrowInstr::NodeId = 0;

auto ThrowInstr::do_get_used_values() const -> std::vector<Value*> {
  if (value)
    return {value};
  return {};
}

auto ThrowInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  if (value && value->get_id() == id) {
    set_value(new_value);
    return 1;
  }
  return 0;
}

const char FlowInstr::NodeId = 0;

auto FlowInstr::do_replace_used_value(id_t id, Value* new_value) -> int {
  auto replacements = 0;
  if (flow->get_id() == id) {
    auto* f = cast<Flow>(new_value);
    seqassert(f, "{} is not a flow", *new_value);
    set_flow(f);
    ++replacements;
  }
  if (val->get_id() == id) {
    set_value(new_value);
    ++replacements;
  }
  return replacements;
}

}  // namespace Pud::IR