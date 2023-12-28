#include "Pud/IR/Func.h"

#include <algorithm>

#include "Pud/Common/Common.h"
#include "Pud/IR/Module.h"
#include "Pud/IR/Util/Iterators.h"
#include "Pud/IR/Util/Operator.h"
#include "Pud/IR/Util/Visitor.h"
#include "Pud/IR/Var.h"

namespace Pud::IR {

namespace {
auto find_and_replace(id_t id, Pud::IR::Var* new_val,
                      std::list<Pud::IR::Var*>& values) -> int {
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

const char Func::NodeId = 0;

void Func::realize(Types::Type* new_type,
                   const std::vector<std::string>& names) {
  auto* func_type = cast<Types::FuncType>(new_type);
  seqassert(func_type, "{} is not a function type", *new_type);

  set_type(func_type);
  args.clear();

  auto i = 0;
  for (auto* t : *func_type) {
    args.push_back(get_module()->Nr<Var>(t, false, false, names[i]));
    ++i;
  }
}

auto Func::get_arg_var(const std::string& n) -> Var* {
  auto it = std::find_if(args.begin(), args.end(),
                         [n](auto* other) { return other->get_name() == n; });
  return (it != args.end()) ? *it : nullptr;
}

auto Func::do_get_used_variables() const -> std::vector<Var*> {
  std::vector<Var*> ret(args.begin(), args.end());
  return ret;
}

auto Func::do_replace_used_variable(id_t id, Var* new_var) -> int {
  return find_and_replace(id, new_var, args);
}

auto Func::do_get_used_types() const -> std::vector<Types::Type*> {
  std::vector<Types::Type*> ret;

  for (auto* t : Var::get_used_types())
    ret.push_back(const_cast<Types::Type*>(t));

  if (parent_type)
    ret.push_back(parent_type);

  return ret;
}

auto Func::do_replace_used_type(const std::string& name, Types::Type* new_type)
    -> int {
  auto count = Var::replace_used_type(name, new_type);
  if (parent_type && parent_type->get_name() == name) {
    parent_type = new_type;
    ++count;
  }
  return count;
}

const char BodiedFunc::NodeId = 0;

auto BodiedFunc::do_replace_used_value(id_t id, Value* new_value) -> int {
  if (body && body->get_id() == id) {
    auto* flow = cast<Flow>(new_value);
    seqassert(flow, "{} is not a flow", *new_value);
    body = flow;
    return 1;
  }
  return 0;
}

auto BodiedFunc::do_get_used_variables() const -> std::vector<Var*> {
  auto ret = Func::do_get_used_variables();
  ret.insert(ret.end(), symbols.begin(), symbols.end());
  return ret;
}

auto BodiedFunc::do_replace_used_variable(id_t id, Var* new_var) -> int {
  return Func::do_replace_used_variable(id, new_var) +
         find_and_replace(id, new_var, symbols);
}

const char ExternalFunc::NodeId = 0;

const char InternalFunc::NodeId = 0;

const char LLVMFunc::NodeId = 0;

auto LLVMFunc::do_get_used_types() const -> std::vector<Types::Type*> {
  std::vector<Types::Type*> ret;

  for (auto* t : Func::get_used_types())
    ret.push_back(const_cast<Types::Type*>(t));

  for (auto& l : llvm_literals)
    if (l.is_type())
      ret.push_back(const_cast<Types::Type*>(l.get_type_value()));

  return ret;
}

auto LLVMFunc::do_replace_used_type(const std::string& name,
                                    Types::Type* new_type) -> int {
  auto count = Var::do_replace_used_type(name, new_type);
  for (auto& l : llvm_literals)
    if (l.is_type() && l.get_type_value()->get_name() == name) {
      l = new_type;
      ++count;
    }
  return count;
}

}  // namespace Pud::IR