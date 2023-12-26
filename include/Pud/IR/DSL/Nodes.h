#ifndef PUD_IR_DSL_NODES_H
#define PUD_IR_DSL_NODES_H

#include <memory>

#include "Pud/IR/Base.h"
#include "Pud/IR/Const.h"
#include "Pud/IR/Instr.h"
#include "Pud/IR/Util/SideEffect.h"

namespace Pud::IR {

namespace Util {
class CloneVisitor;
}  // namespace Util

namespace DSL {

namespace Codegen {
struct CFBuilder;
struct TypeBuilder;
struct ValueBuilder;
}  // namespace Codegen

namespace Types {

/// DSL type.
class CustomType : public AcceptorExtend<CustomType, IR::Types::Type> {
 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

  /// @return the type builder
  virtual auto get_builder() const -> std::unique_ptr<Codegen::TypeBuilder> = 0;

  /// Compares DSL nodes.
  /// @param v the other node
  /// @return true if they match
  virtual auto match(const Type* v) const -> bool = 0;

  /// Format the DSL node.
  /// @param os the output stream
  virtual auto do_format(std::ostream& os) const -> std::ostream& = 0;
};

}  // namespace Types

/// DSL constant.
class CustomConst : public AcceptorExtend<CustomConst, Const> {
 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

  /// @return the value builder
  virtual auto get_builder() const
      -> std::unique_ptr<Codegen::ValueBuilder> = 0;
  /// Compares DSL nodes.
  /// @param v the other node
  /// @return true if they match
  virtual auto match(const Value* v) const -> bool = 0;
  /// Clones the value.
  /// @param cv the clone visitor
  /// @return a clone of the object
  virtual auto do_clone(Util::CloneVisitor& cv) const -> Value* = 0;

  /// Format the DSL node.
  /// @param os the output stream
  virtual auto do_format(std::ostream& os) const -> std::ostream& = 0;
};

/// DSL flow.
class CustomFlow : public AcceptorExtend<CustomFlow, Flow> {
 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

  /// @return the value builder
  virtual auto get_builder() const
      -> std::unique_ptr<Codegen::ValueBuilder> = 0;
  /// Compares DSL nodes.
  /// @param v the other node
  /// @return true if they match
  virtual auto match(const Value* v) const -> bool = 0;
  /// Clones the value.
  /// @param cv the clone visitor
  /// @return a clone of the object
  virtual auto do_clone(Util::CloneVisitor& cv) const -> Value* = 0;
  /// @return the control-flow builder
  virtual auto get_cfbuilder() const -> std::unique_ptr<Codegen::CFBuilder> = 0;
  /// Query this custom node for its side effect properties. If "local"
  /// is true, then the return value should reflect this node and this
  /// node alone, otherwise the value should reflect functions containing
  /// this node in their bodies. For example, a "break" instruction has
  /// side effects locally, but functions containing "break" might still
  /// be side effect free, hence the distinction.
  /// @param local true if result should reflect only this node
  /// @return this node's side effect status
  virtual auto get_side_effect_status(bool local = true) const
      -> Util::SideEffectStatus {
    return Util::SideEffectStatus::UNKNOWN;
  }

  /// Format the DSL node.
  /// @param os the output stream
  virtual auto do_format(std::ostream& os) const -> std::ostream& = 0;
};

/// DSL instruction.
class CustomInstr : public AcceptorExtend<CustomInstr, Instr> {
 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

  /// @return the value builder
  virtual auto get_builder() const
      -> std::unique_ptr<Codegen::ValueBuilder> = 0;
  /// Compares DSL nodes.
  /// @param v the other node
  /// @return true if they match
  virtual auto match(const Value* v) const -> bool = 0;
  /// Clones the value.
  /// @param cv the clone visitor
  /// @return a clone of the object
  virtual auto do_clone(Util::CloneVisitor& cv) const -> Value* = 0;
  /// @return the control-flow builder
  virtual auto get_cfbuilder() const -> std::unique_ptr<Codegen::CFBuilder> = 0;
  /// Query this custom node for its side effect properties. If "local"
  /// is true, then the return value should reflect this node and this
  /// node alone, otherwise the value should reflect functions containing
  /// this node in their bodies. For example, a "break" instruction has
  /// side effects locally, but functions containing "break" might still
  /// be side effect free, hence the distinction.
  /// @param local true if result should reflect only this node
  /// @return this node's side effect status
  virtual auto get_side_effect_status(bool local = true) const
      -> Util::SideEffectStatus {
    return Util::SideEffectStatus::UNKNOWN;
  }

  /// Format the DSL node.
  /// @param os the output stream
  virtual auto do_format(std::ostream& os) const -> std::ostream& = 0;
};

}  // namespace DSL
}  // namespace Pud::IR

#endif  // PUD_IR_DSL_NODES_H