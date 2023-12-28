#ifndef PUD_IR_UTIL_OPERATOR_H
#define PUD_IR_UTIL_OPERATOR_H

#include <unordered_set>

#include "Pud/IR/IR.h"
#include "Pud/IR/Util/Visitor.h"

#define LAMBDA_VISIT(x)                 \
  virtual void handle(Pud::IR::x* v) {} \
  void visit(Pud::IR::x* v) override {  \
    if (children_first)                 \
      process_children(v);              \
    pre_hook(v);                        \
    handle(v);                          \
    post_hook(v);                       \
    if (!children_first)                \
      process_children(v);              \
  }

namespace Pud::IR::Util {

/// Pass that visits all values in a module.
class Operator : public Visitor {
 private:
  /// IDs of previously visited nodes
  std::unordered_set<id_t> seen;
  /// stack of IR nodes being visited
  std::vector<Node*> node_stack;
  /// stack of iterators
  std::vector<decltype(SeriesFlow().begin())> it_stack;
  /// true if should visit children first
  bool children_first;

 protected:
  void default_visit(Node*) override {}

 public:
  /// Constructs an operator.
  /// @param children_first true if children should be visited first
  explicit Operator(bool children_first = false)
      : children_first(children_first) {}

  virtual ~Operator() noexcept = default;

  /// This function is applied to all nodes before handling the node
  /// itself. It provides a way to write one function that gets
  /// applied to every visited node.
  /// @param node the node
  virtual void pre_hook(Node* node) {}
  /// This function is applied to all nodes after handling the node
  /// itself. It provides a way to write one function that gets
  /// applied to every visited node.
  /// @param node the node
  virtual void post_hook(Node* node) {}

  void visit(Module* m) override {
    node_stack.push_back(m);
    node_stack.push_back(m->get_main_func());
    process(m->get_main_func());
    node_stack.pop_back();
    for (auto* s : *m) {
      node_stack.push_back(s);
      process(s);
      node_stack.pop_back();
    }
    node_stack.pop_back();
  }

  void visit(BodiedFunc* f) override {
    if (f->get_body()) {
      seen.insert(f->get_body()->get_id());
      process(f->get_body());
    }
  }

  LAMBDA_VISIT(VarValue);
  LAMBDA_VISIT(PointerValue);

  void visit(Pud::IR::SeriesFlow* v) override {
    if (children_first)
      process_series_flow_children(v);
    pre_hook(v);
    handle(v);
    post_hook(v);
    if (!children_first)
      process_series_flow_children(v);
  }

  virtual void handle(Pud::IR::SeriesFlow* v) {}
  LAMBDA_VISIT(IfFlow);
  LAMBDA_VISIT(WhileFlow);
  LAMBDA_VISIT(ForFlow);
  LAMBDA_VISIT(ImperativeForFlow);
  LAMBDA_VISIT(TryCatchFlow);
  LAMBDA_VISIT(PipelineFlow);
  LAMBDA_VISIT(DSL::CustomFlow);

  LAMBDA_VISIT(TemplatedConst<int64_t>);
  LAMBDA_VISIT(TemplatedConst<double>);
  LAMBDA_VISIT(TemplatedConst<bool>);
  LAMBDA_VISIT(TemplatedConst<std::string>);
  LAMBDA_VISIT(DSL::CustomConst);

  LAMBDA_VISIT(Instr);
  LAMBDA_VISIT(AssignInstr);
  LAMBDA_VISIT(ExtractInstr);
  LAMBDA_VISIT(InsertInstr);
  LAMBDA_VISIT(CallInstr);
  LAMBDA_VISIT(StackAllocInstr);
  LAMBDA_VISIT(TypePropertyInstr);
  LAMBDA_VISIT(YieldInInstr);
  LAMBDA_VISIT(TernaryInstr);
  LAMBDA_VISIT(BreakInstr);
  LAMBDA_VISIT(ContinueInstr);
  LAMBDA_VISIT(ReturnInstr);
  LAMBDA_VISIT(YieldInstr);
  LAMBDA_VISIT(ThrowInstr);
  LAMBDA_VISIT(FlowInstr);
  LAMBDA_VISIT(DSL::CustomInstr);

  template <typename Node>
  void process(Node* v) {
    v->accept(*this);
  }

  /// Return the parent of the current node.
  /// @param level the number of levels up from the current node
  template <typename Desired = Node>
  auto get_parent(int level = 0) -> Desired* {
    return cast<Desired>(node_stack[node_stack.size() - level - 1]);
  }
  /// @return current depth in the tree
  auto depth() const -> int { return node_stack.size(); }

  /// @tparam Desired the desired type
  /// @return the last encountered example of the desired type
  template <typename Desired>
  auto find_last() -> Desired* {
    for (auto it = node_stack.rbegin(); it != node_stack.rend(); ++it) {
      if (auto* v = cast<Desired>(*it))
        return v;
    }
    return nullptr;
  }
  /// @return the last encountered function
  auto get_parent_func() -> Func* { return find_last<Func>(); }

  /// @return an iterator to the first parent
  auto parent_begin() const { return node_stack.begin(); }
  /// @return an iterator beyond the last parent
  auto parent_end() const { return node_stack.end(); }

  /// @param v the value
  /// @return whether we have visited ("seen") the given value
  auto saw(const Value* v) const -> bool {
    return seen.find(v->get_id()) != seen.end();
  }
  /// Avoid visiting the given value in the future.
  /// @param v the value
  void see(const Value* v) { seen.insert(v->get_id()); }

  /// Inserts the new value before the current position in the last seen
  /// SeriesFlow.
  /// @param v the new value
  auto insertBefore(Value* v) {
    return find_last<SeriesFlow>()->insert(it_stack.back(), v);
  }
  /// Inserts the new value after the current position in the last seen
  /// SeriesFlow.
  /// @param v the new value, which is marked seen
  auto insertAfter(Value* v) {
    auto new_pos = it_stack.back();
    ++new_pos;
    see(v);

    return find_last<SeriesFlow>()->insert(new_pos, v);
  }

  /// Resets the operator.
  void reset() {
    seen.clear();
    node_stack.clear();
    it_stack.clear();
  }

 private:
  void process_children(Value* v) {
    node_stack.push_back(v);
    for (auto* c : v->get_used_values()) {
      if (saw(c))
        continue;
      see(c);
      process(c);
    }
    node_stack.pop_back();
  }

  void process_series_flow_children(Pud::IR::SeriesFlow* v) {
    node_stack.push_back(v);
    for (auto it = v->begin(); it != v->end(); ++it) {
      it_stack.push_back(it);
      process(*it);
      it_stack.pop_back();
    }
    node_stack.pop_back();
  }
};

}  // namespace Pud::IR::Util

#undef LAMBDA_VISIT

#endif  // PUD_IR_UTIL_OPERATOR_H