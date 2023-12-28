#include "Pud/IR/Flow.h"

#include <fmt/ostream.h>

#include "Pud/Common/Common.h"
#include "Pud/IR/Module.h"
#include "Pud/IR/Util/Iterators.h"

namespace Pud::IR {

namespace {
auto find_and_replace(id_t id, Pud::IR::Value* new_val,
                      std::list<Pud::IR::Value*>& values) -> int {
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

const char Flow::NodeId = 0;

auto Flow::do_get_type() const -> Types::Type* {
  return get_module()->get_none_type();
}

const char SeriesFlow::NodeId = 0;

auto SeriesFlow::do_replace_used_value(id_t id, Value* new_value) -> int {
  return find_and_replace(id, new_value, series);
}

const char WhileFlow::NodeId = 0;

auto WhileFlow::do_replace_used_value(id_t id, Value* new_value) -> int {
  auto replacements = 0;

  if (cond->get_id() == id) {
    cond = new_value;
    ++replacements;
  }
  if (body->get_id() == id) {
    auto* f = cast<Flow>(new_value);
    seqassert(f, "{} is not a flow", *new_value);
    body = f;
    ++replacements;
  }
  return replacements;
}

const char ForFlow::NodeId = 0;

auto ForFlow::do_get_used_values() const -> std::vector<Value*> {
  std::vector<Value*> ret;
  if (is_parallel())
    ret = get_schedule()->get_used_values();
  ret.push_back(iter);
  ret.push_back(body);
  return ret;
}

auto ForFlow::do_replace_used_value(id_t id, Value* new_value) -> int {
  auto count = 0;
  if (is_parallel())
    count += get_schedule()->replace_used_value(id, new_value);
  if (iter->get_id() == id) {
    iter = new_value;
    ++count;
  }
  if (body->get_id() == id) {
    auto* f = cast<Flow>(new_value);
    seqassert(f, "{} is not a flow", *new_value);
    body = f;
    ++count;
  }
  return count;
}

auto ForFlow::do_replace_used_variable(id_t id, Var* new_var) -> int {
  if (var->get_id() == id) {
    var = new_var;
    return 1;
  }
  return 0;
}

const char ImperativeForFlow::NodeId = 0;

auto ImperativeForFlow::do_get_used_values() const -> std::vector<Value*> {
  std::vector<Value*> ret;
  if (is_parallel())
    ret = get_schedule()->get_used_values();
  ret.push_back(start);
  ret.push_back(end);
  ret.push_back(body);
  return ret;
}

auto ImperativeForFlow::do_replace_used_value(id_t id, Value* new_value)
    -> int {
  auto count = 0;
  if (is_parallel())
    count += get_schedule()->replace_used_value(id, new_value);
  if (body->get_id() == id) {
    auto* f = cast<Flow>(new_value);
    seqassert(f, "{} is not a flow", *new_value);
    body = f;
    ++count;
  }
  if (start->get_id() == id) {
    start = new_value;
    ++count;
  }
  if (end->get_id() == id) {
    end = new_value;
    ++count;
  }
  return count;
}

auto ImperativeForFlow::do_replace_used_variable(id_t id, Var* new_var) -> int {
  if (var->get_id() == id) {
    var = new_var;
    return 1;
  }
  return 0;
}

const char IfFlow::NodeId = 0;

auto IfFlow::do_get_used_values() const -> std::vector<Value*> {
  std::vector<Value*> ret = {cond, true_branch};
  if (false_branch)
    ret.push_back(false_branch);
  return ret;
}

auto IfFlow::do_replace_used_value(id_t id, Value* new_value) -> int {
  auto replacements = 0;

  if (cond->get_id() == id) {
    cond = new_value;
    ++replacements;
  }
  if (true_branch->get_id() == id) {
    auto* f = cast<Flow>(new_value);
    seqassert(f, "{} is not a flow", *new_value);
    true_branch = f;
    ++replacements;
  }
  if (false_branch && false_branch->get_id() == id) {
    auto* f = cast<Flow>(new_value);
    seqassert(f, "{} is not a flow", *new_value);
    false_branch = f;
    ++replacements;
  }

  return replacements;
}

const char TryCatchFlow::NodeId = 0;

auto TryCatchFlow::do_get_used_values() const -> std::vector<Value*> {
  std::vector<Value*> ret = {body};
  if (finally)
    ret.push_back(finally);

  for (auto& c : catches)
    ret.push_back(
        const_cast<Value*>(static_cast<const Value*>(c.get_handler())));
  return ret;
}

auto TryCatchFlow::do_replace_used_value(id_t id, Value* new_value) -> int {
  auto replacements = 0;

  if (body->get_id() == id) {
    auto* f = cast<Flow>(new_value);
    seqassert(f, "{} is not a flow", *new_value);
    body = f;
    ++replacements;
  }
  if (finally && finally->get_id() == id) {
    auto* f = cast<Flow>(new_value);
    seqassert(f, "{} is not a flow", *new_value);
    finally = f;
    ++replacements;
  }

  for (auto& c : catches) {
    if (c.get_handler()->get_id() == id) {
      auto* f = cast<Flow>(new_value);
      seqassert(f, "{} is not a flow", *new_value);
      c.set_handler(f);
      ++replacements;
    }
  }

  return replacements;
}

auto TryCatchFlow::do_get_used_types() const -> std::vector<Types::Type*> {
  std::vector<Types::Type*> ret;
  for (auto& c : catches) {
    if (auto* t = c.get_type())
      ret.push_back(const_cast<Types::Type*>(t));
  }
  return ret;
}

auto TryCatchFlow::do_replace_used_type(const std::string& name,
                                        Types::Type* new_type) -> int {
  auto count = 0;
  for (auto& c : catches) {
    if (c.get_type()->get_name() == name) {
      c.set_type(new_type);
      ++count;
    }
  }
  return count;
}

auto TryCatchFlow::do_get_used_variables() const -> std::vector<Var*> {
  std::vector<Var*> ret;
  for (auto& c : catches) {
    if (auto* t = c.get_var())
      ret.push_back(const_cast<Var*>(t));
  }
  return ret;
}

auto TryCatchFlow::do_replace_used_variable(id_t id, Var* new_var) -> int {
  auto count = 0;
  for (auto& c : catches) {
    if (c.get_var()->get_id() == id) {
      c.set_var(new_var);
      ++count;
    }
  }
  return count;
}

const char PipelineFlow::NodeId = 0;

auto PipelineFlow::Stage::get_output_type() const -> Types::Type* {
  if (args.empty()) {
    return callee->get_type();
  } else {
    auto* func_type = cast<Types::FuncType>(callee->get_type());
    seqassertn(func_type, "{} is not a function type", *callee->get_type());
    return func_type->get_return_type();
  }
}

auto PipelineFlow::Stage::get_output_element_type() const -> Types::Type* {
  if (is_generator()) {
    Types::GeneratorType* gen_type = nullptr;
    if (args.empty()) {
      gen_type = cast<Types::GeneratorType>(callee->get_type());
      return gen_type->get_base();
    } else {
      auto* func_type = cast<Types::FuncType>(callee->get_type());
      seqassertn(func_type, "{} is not a function type", *callee->get_type());
      gen_type = cast<Types::GeneratorType>(func_type->get_return_type());
    }
    seqassertn(gen_type, "generator type not found");
    return gen_type->get_base();
  } else if (args.empty()) {
    return callee->get_type();
  } else {
    auto* func_type = cast<Types::FuncType>(callee->get_type());
    seqassertn(func_type, "{} is not a function type", *callee->get_type());
    return func_type->get_return_type();
  }
}

auto PipelineFlow::do_get_used_values() const -> std::vector<Value*> {
  std::vector<Value*> ret;
  for (auto& s : stages) {
    ret.push_back(const_cast<Value*>(s.get_callee()));
    for (auto* arg : s.args)
      if (arg)
        ret.push_back(arg);
  }
  return ret;
}

auto PipelineFlow::do_replace_used_value(id_t id, Value* new_value) -> int {
  auto replacements = 0;

  for (auto& c : stages) {
    if (c.get_callee()->get_id() == id) {
      c.set_callee(new_value);
      ++replacements;
    }
    for (auto& s : c.args)
      if (s && s->get_id() == id) {
        s = new_value;
        ++replacements;
      }
  }

  return replacements;
}

}  // namespace Pud::IR