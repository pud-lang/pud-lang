#ifndef PUD_IR_UTIL_VISITOR_H
#define PUD_IR_UTIL_VISITOR_H

#include <memory>
#include <stdexcept>
#include <string>

#define VISIT(x) virtual void visit(Pud::IR::x*)
#define CONST_VISIT(x) virtual void visit(const Pud::IR::x*)

namespace Pud::IR {

class Node;

namespace Types {
class Type;
class PrimitiveType;
class IntType;
class FloatType;
class Float32Type;
class Float16Type;
class BFloat16Type;
class Float128Type;
class BoolType;
class ByteType;
class VoidType;
class RecordType;
class RefType;
class FuncType;
class OptionalType;
class PointerType;
class GeneratorType;
class IntNType;
class VectorType;
class UnionType;
}  // namespace Types

namespace DSL {

namespace Types {
class CustomType;
}

class CustomConst;
class CustomFlow;
class CustomInstr;
}  // namespace DSL

class Module;

class Var;

class Func;
class BodiedFunc;
class ExternalFunc;
class InternalFunc;
class LLVMFunc;

class Value;
class VarValue;
class PointerValue;

class Flow;
class SeriesFlow;
class IfFlow;
class WhileFlow;
class ForFlow;
class ImperativeForFlow;
class TryCatchFlow;
class PipelineFlow;

class Const;

template <typename ValueType>
class TemplatedConst;

class Instr;
class AssignInstr;
class ExtractInstr;
class InsertInstr;
class CallInstr;
class StackAllocInstr;
class TypePropertyInstr;
class YieldInInstr;
class TernaryInstr;
class BreakInstr;
class ContinueInstr;
class ReturnInstr;
class YieldInstr;
class ThrowInstr;
class FlowInstr;

namespace Util {

class Visitor {
 protected:
  virtual void default_visit(Pud::IR::Node*) {
    throw std::runtime_error("cannot visit node");
  }

 public:
  virtual ~Visitor() noexcept = default;

  VISIT(Module);

  VISIT(Var);

  VISIT(Func);
  VISIT(BodiedFunc);
  VISIT(ExternalFunc);
  VISIT(InternalFunc);
  VISIT(LLVMFunc);

  VISIT(Value);
  VISIT(VarValue);
  VISIT(PointerValue);

  VISIT(Flow);
  VISIT(SeriesFlow);
  VISIT(IfFlow);
  VISIT(WhileFlow);
  VISIT(ForFlow);
  VISIT(ImperativeForFlow);
  VISIT(TryCatchFlow);
  VISIT(PipelineFlow);
  VISIT(DSL::CustomFlow);

  VISIT(Const);
  VISIT(TemplatedConst<int64_t>);
  VISIT(TemplatedConst<double>);
  VISIT(TemplatedConst<bool>);
  VISIT(TemplatedConst<std::string>);
  VISIT(DSL::CustomConst);

  VISIT(Instr);
  VISIT(AssignInstr);
  VISIT(ExtractInstr);
  VISIT(InsertInstr);
  VISIT(CallInstr);
  VISIT(StackAllocInstr);
  VISIT(TypePropertyInstr);
  VISIT(YieldInInstr);
  VISIT(TernaryInstr);
  VISIT(BreakInstr);
  VISIT(ContinueInstr);
  VISIT(ReturnInstr);
  VISIT(YieldInstr);
  VISIT(ThrowInstr);
  VISIT(FlowInstr);
  VISIT(DSL::CustomInstr);

  VISIT(Types::Type);
  VISIT(Types::PrimitiveType);
  VISIT(Types::IntType);
  VISIT(Types::FloatType);
  VISIT(Types::Float32Type);
  VISIT(Types::Float16Type);
  VISIT(Types::BFloat16Type);
  VISIT(Types::Float128Type);
  VISIT(Types::BoolType);
  VISIT(Types::ByteType);
  VISIT(Types::VoidType);
  VISIT(Types::RecordType);
  VISIT(Types::RefType);
  VISIT(Types::FuncType);
  VISIT(Types::OptionalType);
  VISIT(Types::PointerType);
  VISIT(Types::GeneratorType);
  VISIT(Types::IntNType);
  VISIT(Types::VectorType);
  VISIT(Types::UnionType);
  VISIT(DSL::Types::CustomType);
};

class ConstVisitor {
 protected:
  virtual void default_visit(const Pud::IR::Node*) {
    throw std::runtime_error("cannot visit const node");
  }

 public:
  virtual ~ConstVisitor() noexcept = default;

  CONST_VISIT(Module);

  CONST_VISIT(Var);

  CONST_VISIT(Func);
  CONST_VISIT(BodiedFunc);
  CONST_VISIT(ExternalFunc);
  CONST_VISIT(InternalFunc);
  CONST_VISIT(LLVMFunc);

  CONST_VISIT(Value);
  CONST_VISIT(VarValue);
  CONST_VISIT(PointerValue);

  CONST_VISIT(Flow);
  CONST_VISIT(SeriesFlow);
  CONST_VISIT(IfFlow);
  CONST_VISIT(WhileFlow);
  CONST_VISIT(ForFlow);
  CONST_VISIT(ImperativeForFlow);
  CONST_VISIT(TryCatchFlow);
  CONST_VISIT(PipelineFlow);
  CONST_VISIT(DSL::CustomFlow);

  CONST_VISIT(Const);
  CONST_VISIT(TemplatedConst<int64_t>);
  CONST_VISIT(TemplatedConst<double>);
  CONST_VISIT(TemplatedConst<bool>);
  CONST_VISIT(TemplatedConst<std::string>);
  CONST_VISIT(DSL::CustomConst);

  CONST_VISIT(Instr);
  CONST_VISIT(AssignInstr);
  CONST_VISIT(ExtractInstr);
  CONST_VISIT(InsertInstr);
  CONST_VISIT(CallInstr);
  CONST_VISIT(StackAllocInstr);
  CONST_VISIT(TypePropertyInstr);
  CONST_VISIT(YieldInInstr);
  CONST_VISIT(TernaryInstr);
  CONST_VISIT(BreakInstr);
  CONST_VISIT(ContinueInstr);
  CONST_VISIT(ReturnInstr);
  CONST_VISIT(YieldInstr);
  CONST_VISIT(ThrowInstr);
  CONST_VISIT(FlowInstr);
  CONST_VISIT(DSL::CustomInstr);

  CONST_VISIT(Types::Type);
  CONST_VISIT(Types::PrimitiveType);
  CONST_VISIT(Types::IntType);
  CONST_VISIT(Types::FloatType);
  CONST_VISIT(Types::Float32Type);
  CONST_VISIT(Types::Float16Type);
  CONST_VISIT(Types::BFloat16Type);
  CONST_VISIT(Types::Float128Type);
  CONST_VISIT(Types::BoolType);
  CONST_VISIT(Types::ByteType);
  CONST_VISIT(Types::VoidType);
  CONST_VISIT(Types::RecordType);
  CONST_VISIT(Types::RefType);
  CONST_VISIT(Types::FuncType);
  CONST_VISIT(Types::OptionalType);
  CONST_VISIT(Types::PointerType);
  CONST_VISIT(Types::GeneratorType);
  CONST_VISIT(Types::IntNType);
  CONST_VISIT(Types::VectorType);
  CONST_VISIT(Types::UnionType);
  CONST_VISIT(DSL::Types::CustomType);
};

}  // namespace Util
}  // namespace Pud::IR

#undef VISIT
#undef CONST_VISIT

#endif  // PUD_IR_UTIL_VISITOR_H