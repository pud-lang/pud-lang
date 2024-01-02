#ifndef PUD_IR_UTIL_CLONING_H
#define PUD_IR_UTIL_CLONING_H

#include <unordered_map>

#include "Pud/IR/IR.h"
#include "Pud/IR/Util/Visitor.h"

namespace Pud::IR::Util {

class CloneVisitor : public ConstVisitor {
 private:
  /// the clone context
  std::unordered_map<int, Node*> ctx;
  /// the result
  Node* result;
  /// the module
  Module* module;
  /// true if break/continue loops should be cloned
  bool clone_loop;

 public:
  /// Constructs a clone visitor.
  /// @param module the module
  /// @param clone_loop true if break/continue loops should be cloned
  explicit CloneVisitor(Module* module, bool clone_loop = true)
      : ctx(), result(nullptr), module(module), clone_loop(clone_loop) {}

  virtual ~CloneVisitor() noexcept = default;

  void visit(const Var* v) override;

  void visit(const BodiedFunc* v) override;
  void visit(const ExternalFunc* v) override;
  void visit(const InternalFunc* v) override;
  void visit(const LLVMFunc* v) override;

  void visit(const VarValue* v) override;
  void visit(const PointerValue* v) override;

  void visit(const SeriesFlow* v) override;
  void visit(const IfFlow* v) override;
  void visit(const WhileFlow* v) override;
  void visit(const ForFlow* v) override;
  void visit(const ImperativeForFlow* v) override;
  void visit(const TryCatchFlow* v) override;
  void visit(const PipelineFlow* v) override;
  void visit(const DSL::CustomFlow* v) override;

  void visit(const IntConst* v) override;
  void visit(const FloatConst* v) override;
  void visit(const BoolConst* v) override;
  void visit(const StringConst* v) override;
  void visit(const DSL::CustomConst* v) override;

  void visit(const AssignInstr* v) override;
  void visit(const ExtractInstr* v) override;
  void visit(const InsertInstr* v) override;
  void visit(const CallInstr* v) override;
  void visit(const StackAllocInstr* v) override;
  void visit(const TypePropertyInstr* v) override;
  void visit(const YieldInInstr* v) override;
  void visit(const TernaryInstr* v) override;
  void visit(const BreakInstr* v) override;
  void visit(const ContinueInstr* v) override;
  void visit(const ReturnInstr* v) override;
  void visit(const YieldInstr* v) override;
  void visit(const ThrowInstr* v) override;
  void visit(const FlowInstr* v) override;
  void visit(const DSL::CustomInstr* v) override;

  /// Clones a value, returning the previous value if other has already been
  /// cloned.
  /// @param other the original
  /// @param cloneTo the function to clone locals to, or null if none
  /// @param remaps variable re-mappings
  /// @return the clone
  auto clone(const Value* other, BodiedFunc* cloneTo = nullptr,
             const std::unordered_map<id_t, Var*>& remaps = {}) -> Value*;

  /// Returns the original unless the variable has been force cloned.
  /// @param other the original
  /// @return the original or the previous clone
  auto clone(const Var* other) -> Var*;

  /// Clones a flow, returning the previous value if other has already been
  /// cloned.
  /// @param other the original
  /// @return the clone
  auto clone(const Flow* other) -> Flow* {
    return cast<Flow>(clone(static_cast<const Value*>(other)));
  }

  /// Forces a clone. No difference for values but ensures that variables are
  /// actually cloned.
  /// @param other the original
  /// @return the clone
  template <typename NodeType>
  auto force_clone(const NodeType* other) -> NodeType* {
    if (!other)
      return nullptr;

    auto id = other->get_id();
    if (ctx.find(id) == ctx.end()) {
      other->accept(*this);
      ctx[id] = result;

      for (auto it = other->attributes_begin(); it != other->attributes_end();
           ++it) {
        const auto* attr = other->getAttribute(*it);
        if (attr->needsClone()) {
          ctx[id]->set_attribute(attr->forceClone(*this), *it);
        }
      }
    }
    return cast<NodeType>(ctx[id]);
  }

  /// Remaps a clone.
  /// @param original the original
  /// @param newVal the clone
  template <typename NodeType>
  void force_remap(const NodeType* original, const NodeType* newVal) {
    ctx[original->get_id()] = const_cast<NodeType*>(newVal);
  }

  auto clone(const PipelineFlow::Stage& other) -> PipelineFlow::Stage {
    std::vector<Value*> args;
    for (const auto* a : other)
      args.push_back(clone(a));
    return {clone(other.get_callee()), std::move(args), other.is_generator(),
            other.is_parallel()};
  }

 private:
  template <typename NodeType, typename... Args>
  auto Nt(const NodeType* source, Args... args) -> NodeType* {
    return module->N<NodeType>(source, std::forward<Args>(args)...,
                               source->getName());
  }
};

}  // namespace Pud::IR::Util

#endif  // PUD_IR_UTIL_CLONING_H