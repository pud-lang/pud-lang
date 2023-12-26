#ifndef PUD_IR_INSTR_H
#define PUD_IR_INSTR_H

#include <memory>
#include <string>

#include "Pud/IR/Flow.h"
#include "Pud/IR/Types/Types.h"
#include "Pud/IR/Util/Iterators.h"
#include "Pud/IR/Value.h"
#include "Pud/IR/Var.h"

namespace Pud::IR {

/// CIR object representing an "instruction," or discrete operation in the
/// context of a block.
class Instr : public AcceptorExtend<Instr, Value> {
 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

 private:
  auto do_get_type() const -> Types::Type* override;
};

/// Instr representing setting a memory location.
class AssignInstr : public AcceptorExtend<AssignInstr, Instr> {
 private:
  /// the left-hand side
  Var* lhs;
  /// the right-hand side
  Value* rhs;

 public:
  static const char NodeId;

  /// Constructs an assign instruction.
  /// @param lhs the left-hand side
  /// @param rhs the right-hand side
  /// @param field the field being set, may be empty
  /// @param name the instruction's name
  AssignInstr(Var* lhs, Value* rhs, std::string name = "")
      : AcceptorExtend(std::move(name)), lhs(lhs), rhs(rhs) {}

  /// @return the left-hand side
  auto get_lhs() -> Var* { return lhs; }
  /// @return the left-hand side
  auto get_lhs() const -> const Var* { return lhs; }
  /// Sets the left-hand side
  /// @param l the new value
  void set_lhs(Var* v) { lhs = v; }

  /// @return the right-hand side
  auto get_rhs() -> Value* { return rhs; }
  /// @return the right-hand side
  auto get_rhs() const -> const Value* { return rhs; }
  /// Sets the right-hand side
  /// @param l the new value
  void set_rhs(Value* v) { rhs = v; }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override {
    return {rhs};
  }
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;

  auto do_get_used_variables() const -> std::vector<Var*> override {
    return {lhs};
  }
  auto do_replace_used_variable(id_t id, Var* new_var) -> int override;
};

/// Instr representing loading the field of a value.
class ExtractInstr : public AcceptorExtend<ExtractInstr, Instr> {
 private:
  /// the value being manipulated
  Value* val;
  /// the field
  std::string field;

 public:
  static const char NodeId;

  /// Constructs a load instruction.
  /// @param val the value being manipulated
  /// @param field the field
  /// @param name the instruction's name
  explicit ExtractInstr(Value* val, std::string field, std::string name = "")
      : AcceptorExtend(std::move(name)), val(val), field(std::move(field)) {}

  /// @return the location
  auto get_val() -> Value* { return val; }
  /// @return the location
  auto get_val() const -> const Value* { return val; }
  /// Sets the location.
  /// @param p the new value
  void set_val(Value* p) { val = p; }

  /// @return the field
  auto get_field() const -> const std::string& { return field; }
  /// Sets the field.
  /// @param f the new field
  void set_field(std::string f) { field = std::move(f); }

 protected:
  auto do_get_type() const -> Types::Type* override;
  auto do_get_used_values() const -> std::vector<Value*> override {
    return {val};
  }
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

/// Instr representing setting the field of a value.
class InsertInstr : public AcceptorExtend<InsertInstr, Instr> {
 private:
  /// the value being manipulated
  Value* lhs;
  /// the field
  std::string field;
  /// the value being inserted
  Value* rhs;

 public:
  static const char NodeId;

  /// Constructs a load instruction.
  /// @param lhs the value being manipulated
  /// @param field the field
  /// @param rhs the new value
  /// @param name the instruction's name
  explicit InsertInstr(Value* lhs, std::string field, Value* rhs,
                       std::string name = "")
      : AcceptorExtend(std::move(name)),
        lhs(lhs),
        field(std::move(field)),
        rhs(rhs) {}

  /// @return the left-hand side
  auto get_lhs() -> Value* { return lhs; }
  /// @return the left-hand side
  auto get_lhs() const -> const Value* { return lhs; }
  /// Sets the left-hand side.
  /// @param p the new value
  void set_lhs(Value* p) { lhs = p; }

  /// @return the right-hand side
  auto get_rhs() -> Value* { return rhs; }
  /// @return the right-hand side
  auto get_rhs() const -> const Value* { return rhs; }
  /// Sets the right-hand side.
  /// @param p the new value
  void set_rhs(Value* p) { rhs = p; }

  /// @return the field
  auto get_field() const -> const std::string& { return field; }
  /// Sets the field.
  /// @param f the new field
  void set_field(std::string f) { field = std::move(f); }

 protected:
  auto do_get_type() const -> Types::Type* override { return lhs->get_type(); }
  auto do_get_used_values() const -> std::vector<Value*> override {
    return {lhs, rhs};
  }
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

/// Instr representing calling a function.
class CallInstr : public AcceptorExtend<CallInstr, Instr> {
 private:
  /// the function
  Value* callee;
  /// the arguments
  std::vector<Value*> args;

 public:
  static const char NodeId;

  /// Constructs a call instruction.
  /// @param callee the function
  /// @param args the arguments
  /// @param name the instruction's name
  CallInstr(Value* callee, std::vector<Value*> args, std::string name = "")
      : AcceptorExtend(std::move(name)),
        callee(callee),
        args(std::move(args)) {}

  /// Constructs a call instruction with no arguments.
  /// @param callee the function
  /// @param name the instruction's name
  explicit CallInstr(Value* callee, std::string name = "")
      : CallInstr(callee, {}, std::move(name)) {}

  /// @return the callee
  auto get_callee() -> Value* { return callee; }
  /// @return the callee
  auto get_callee() const -> const Value* { return callee; }
  /// Sets the callee.
  /// @param c the new value
  void set_callee(Value* c) { callee = c; }

  /// @return an iterator to the first argument
  auto begin() { return args.begin(); }
  /// @return an iterator beyond the last argument
  auto end() { return args.end(); }
  /// @return an iterator to the first argument
  auto begin() const { return args.begin(); }
  /// @return an iterator beyond the last argument
  auto end() const { return args.end(); }

  /// @return a pointer to the first argument
  auto front() -> Value* { return args.front(); }
  /// @return a pointer to the last argument
  auto back() -> Value* { return args.back(); }
  /// @return a pointer to the first argument
  auto front() const -> const Value* { return args.front(); }
  /// @return a pointer to the last argument
  auto back() const -> const Value* { return args.back(); }

  /// Inserts an argument at the given position.
  /// @param pos the position
  /// @param v the argument
  /// @return an iterator to the newly added argument
  template <typename It>
  auto insert(It pos, Value* v) {
    return args.insert(pos, v);
  }
  /// Appends an argument.
  /// @param v the argument
  void push_back(Value* v) { args.push_back(v); }

  /// Sets the args.
  /// @param v the new args vector
  void set_args(std::vector<Value*> v) { args = std::move(v); }

  /// @return the number of arguments
  auto num_args() const -> int { return args.size(); }

 protected:
  auto do_get_type() const -> Types::Type* override;
  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

/// Instr representing allocating an array on the stack.
class StackAllocInstr : public AcceptorExtend<StackAllocInstr, Instr> {
 private:
  /// the array type
  Types::Type* array_type;
  /// number of elements to allocate
  int64_t count;

 public:
  static const char NodeId;

  /// Constructs a stack allocation instruction.
  /// @param array_type the type of the array
  /// @param count the number of elements
  /// @param name the name
  StackAllocInstr(Types::Type* array_type, int64_t count, std::string name = "")
      : AcceptorExtend(std::move(name)), array_type(array_type), count(count) {}

  /// @return the count
  auto get_count() const -> int64_t { return count; }
  /// Sets the count.
  /// @param c the new value
  void set_count(int64_t c) { count = c; }

  /// @return the array type
  auto get_array_type() -> Types::Type* { return array_type; }
  /// @return the array type
  auto get_array_type() const -> Types::Type* { return array_type; }
  /// Sets the array type.
  /// @param t the new type
  void setArrayType(Types::Type* t) { array_type = t; }

 protected:
  auto do_get_type() const -> Types::Type* override { return array_type; }
  auto do_get_used_types() const -> std::vector<Types::Type*> override {
    return {array_type};
  }
  auto do_replace_used_type(const std::string& name, Types::Type* new_type)
      -> int override;
};

/// Instr representing getting information about a type.
class TypePropertyInstr : public AcceptorExtend<TypePropertyInstr, Instr> {
 public:
  enum Property { IS_ATOMIC, IS_CONTENT_ATOMIC, SIZEOF };

 private:
  /// the type being inspected
  Types::Type* inspect_type;
  /// the property being checked
  Property property;

 public:
  static const char NodeId;

  /// Constructs a type property instruction.
  /// @param type the type being inspected
  /// @param name the name
  explicit TypePropertyInstr(Types::Type* type, Property property,
                             std::string name = "")
      : AcceptorExtend(std::move(name)),
        inspect_type(type),
        property(property) {}

  /// @return the type being inspected
  auto get_inspect_type() -> Types::Type* { return inspect_type; }
  /// @return the type being inspected
  auto get_inspect_type() const -> Types::Type* { return inspect_type; }
  /// Sets the type being inspected
  /// @param t the new type
  void setInspectType(Types::Type* t) { inspect_type = t; }

  /// @return the property being inspected
  auto get_property() const -> Property { return property; }
  /// Sets the property.
  /// @param p the new value
  void set_property(Property p) { property = p; }

 protected:
  auto do_get_type() const -> Types::Type* override;
  auto do_get_used_types() const -> std::vector<Types::Type*> override {
    return {inspect_type};
  }
  auto do_replace_used_type(const std::string& name, Types::Type* new_type)
      -> int override;
};

/// Instr representing a Python yield expression.
class YieldInInstr : public AcceptorExtend<YieldInInstr, Instr> {
 private:
  /// the type of the value being yielded in.
  Types::Type* type;
  /// whether or not to suspend
  bool suspend;

 public:
  static const char NodeId;

  /// Constructs a yield in instruction.
  /// @param type the type of the value being yielded in
  /// @param suspend whether to suspend
  /// @param name the instruction's name
  explicit YieldInInstr(Types::Type* type, bool suspend = true,
                        std::string name = "")
      : AcceptorExtend(std::move(name)), type(type), suspend(suspend) {}

  /// @return true if the instruction suspends
  auto is_suspending() const -> bool { return suspend; }
  /// Sets the instruction suspending flag.
  /// @param v the new value
  void set_suspending(bool v = true) { suspend = v; }

  /// Sets the type being inspected
  /// @param t the new type
  void set_type(Types::Type* t) { type = t; }

 protected:
  auto do_get_type() const -> Types::Type* override { return type; }
  auto do_get_used_types() const -> std::vector<Types::Type*> override {
    return {type};
  }
  auto do_replace_used_type(const std::string& name, Types::Type* new_type)
      -> int override;
};

/// Instr representing a ternary operator.
class TernaryInstr : public AcceptorExtend<TernaryInstr, Instr> {
 private:
  /// the condition
  Value* cond;
  /// the true value
  Value* true_value;
  /// the false value
  Value* false_value;

 public:
  static const char NodeId;

  /// Constructs a ternary instruction.
  /// @param cond the condition
  /// @param true_value the true value
  /// @param false_value the false value
  /// @param name the instruction's name
  TernaryInstr(Value* cond, Value* true_value, Value* false_value,
               std::string name = "")
      : AcceptorExtend(std::move(name)),
        cond(cond),
        true_value(true_value),
        false_value(false_value) {}

  /// @return the condition
  auto get_cond() -> Value* { return cond; }
  /// @return the condition
  auto get_cond() const -> const Value* { return cond; }
  /// Sets the condition.
  /// @param v the new value
  void set_cond(Value* v) { cond = v; }

  /// @return the condition
  auto get_true_value() -> Value* { return true_value; }
  /// @return the condition
  auto get_true_value() const -> const Value* { return true_value; }
  /// Sets the true value.
  /// @param v the new value
  void set_true_value(Value* v) { true_value = v; }

  /// @return the false value
  auto get_false_value() -> Value* { return false_value; }
  /// @return the false value
  auto get_false_value() const -> const Value* { return false_value; }
  /// Sets the value.
  /// @param v the new value
  void set_false_value(Value* v) { false_value = v; }

 protected:
  auto do_get_type() const -> Types::Type* override {
    return true_value->get_type();
  }
  auto do_get_used_values() const -> std::vector<Value*> override {
    return {cond, true_value, false_value};
  }
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

/// Base for control flow instructions
class ControlFlowInstr : public AcceptorExtend<ControlFlowInstr, Instr> {
 public:
  static const char NodeId;
  using AcceptorExtend::AcceptorExtend;
};

/// Instr representing a break statement.
class BreakInstr : public AcceptorExtend<BreakInstr, ControlFlowInstr> {
 private:
  /// the loop being broken, nullptr if the immediate ancestor
  Value* loop;

 public:
  static const char NodeId;

  /// Constructs a break instruction.
  /// @param loop the loop being broken, nullptr if immediate ancestor
  /// @param name the instruction's name
  explicit BreakInstr(Value* loop = nullptr, std::string name = "")
      : AcceptorExtend(std::move(name)), loop(loop) {}

  /// @return the loop, nullptr if immediate ancestor
  auto get_loop() const -> Value* { return loop; }
  /// Sets the loop id.
  /// @param v the new loop, nullptr if immediate ancestor
  void set_loop(Value* v) { loop = v; }

  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

/// Instr representing a continue statement.
class ContinueInstr : public AcceptorExtend<ContinueInstr, ControlFlowInstr> {
 private:
  /// the loop being continued, nullptr if the immediate ancestor
  Value* loop;

 public:
  static const char NodeId;

  /// Constructs a continue instruction.
  /// @param loop the loop being continued, nullptr if immediate ancestor
  /// @param name the instruction's name
  explicit ContinueInstr(Value* loop = nullptr, std::string name = "")
      : AcceptorExtend(std::move(name)), loop(loop) {}

  /// @return the loop, nullptr if immediate ancestor
  auto get_loop() const -> Value* { return loop; }
  /// Sets the loop id.
  /// @param v the new loop, -1 if immediate ancestor
  void set_loop(Value* v) { loop = v; }

  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

/// Instr representing a return statement.
class ReturnInstr : public AcceptorExtend<ReturnInstr, ControlFlowInstr> {
 private:
  /// the value
  Value* value;

 public:
  static const char NodeId;

  explicit ReturnInstr(Value* value = nullptr, std::string name = "")
      : AcceptorExtend(std::move(name)), value(value) {}

  /// @return the value
  auto get_value() -> Value* { return value; }
  /// @return the value
  auto get_value() const -> const Value* { return value; }
  /// Sets the value.
  /// @param v the new value
  void set_value(Value* v) { value = v; }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

class YieldInstr : public AcceptorExtend<YieldInstr, Instr> {
 private:
  /// the value
  Value* value;
  /// whether this yield is final
  bool final;

 public:
  static const char NodeId;

  explicit YieldInstr(Value* value = nullptr, bool final = false,
                      std::string name = "")
      : AcceptorExtend(std::move(name)), value(value), final(final) {}

  /// @return the value
  auto get_value() -> Value* { return value; }
  /// @return the value
  auto get_value() const -> const Value* { return value; }
  /// Sets the value.
  /// @param v the new value
  void set_value(Value* v) { value = v; }

  /// @return if this yield is final
  auto is_final() const -> bool { return final; }
  /// Sets whether this yield is final.
  /// @param f true if final
  void set_final(bool f = true) { final = f; }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

class ThrowInstr : public AcceptorExtend<ThrowInstr, Instr> {
 private:
  /// the value
  Value* value;

 public:
  static const char NodeId;

  explicit ThrowInstr(Value* value = nullptr, std::string name = "")
      : AcceptorExtend(std::move(name)), value(value) {}

  /// @return the value
  auto get_value() -> Value* { return value; }
  /// @return the value
  auto get_value() const -> const Value* { return value; }
  /// Sets the value.
  /// @param v the new value
  void set_value(Value* v) { value = v; }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

/// Instr that contains a flow and value.
class FlowInstr : public AcceptorExtend<FlowInstr, Instr> {
 private:
  /// the flow
  Value* flow;
  /// the output value
  Value* val;

 public:
  static const char NodeId;

  /// Constructs a flow value.
  /// @param flow the flow
  /// @param val the output value
  /// @param name the name
  explicit FlowInstr(Flow* flow, Value* val, std::string name = "")
      : AcceptorExtend(std::move(name)), flow(flow), val(val) {}

  /// @return the flow
  auto get_flow() -> Flow* { return cast<Flow>(flow); }
  /// @return the flow
  auto get_flow() const -> const Flow* { return cast<Flow>(flow); }
  /// Sets the flow.
  /// @param f the new flow
  void set_flow(Flow* f) { flow = f; }

  /// @return the value
  auto get_value() -> Value* { return val; }
  /// @return the value
  auto get_value() const -> const Value* { return val; }
  /// Sets the value.
  /// @param v the new value
  void set_value(Value* v) { val = v; }

 protected:
  auto do_get_type() const -> Types::Type* override { return val->get_type(); }
  auto do_get_used_values() const -> std::vector<Value*> override {
    return {flow, val};
  }
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

}  // namespace Pud::IR

#endif  // PUD_IR_INSTR_H