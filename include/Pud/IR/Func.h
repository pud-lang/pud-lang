#ifndef PUD_IR_FUNC_H
#define PUD_IR_FUNC_H

#include "Pud/IR/Flow.h"
#include "Pud/IR/Util/Iterators.h"
#include "Pud/IR/Var.h"

namespace Pud::IR {

// 用于表示一个 CIR 函数。
class Func : public AcceptorExtend<Func, Var> {
 private:
  // 存储函数的原始（未经修饰的）名称。
  std::string unmangled_name;
  // 标记函数是否是生成器。
  bool generator;
  // 如果函数是方法，则指向其父类型；如果不是，则为 nullptr。
  Types::Type* parent_type;

 protected:
  // 表示函数参数的列表。
  std::list<Var*> args;

  auto do_get_used_variables() const -> std::vector<Var*> override;
  auto do_replace_used_variable(id_t id, Var* new_var) -> int override;

  auto do_get_used_types() const -> std::vector<Types::Type*> override;
  auto do_replace_used_type(const std::string& name, Types::Type* new_type)
      -> int override;

 public:
  static const char NodeId;

  explicit Func(std::string name = "")
      : AcceptorExtend(nullptr, true, false, std::move(name)),
        generator(false),
        parent_type(nullptr) {}

  // 用于初始化函数的新类型和参数名称。
  void realize(Types::Type* new_type, const std::vector<std::string>& names);

  auto arg_begin() { return args.begin(); }
  auto arg_end() { return args.end(); }
  auto arg_begin() const { return args.begin(); }
  auto arg_end() const { return args.end(); }

  auto arg_front() -> Var* { return args.front(); }
  auto arg_back() -> Var* { return args.back(); }
  auto arg_back() const -> const Var* { return args.back(); }
  auto arg_front() const -> const Var* { return args.front(); }

  // 获取和设置函数的未修饰名称。
  auto get_unmangled_name() const -> std::string { return unmangled_name; }
  void set_unmangled_name(std::string v) { unmangled_name = std::move(v); }

  auto is_generator() const -> bool { return generator; }
  void set_generator(bool v = true) { generator = v; }

  auto get_arg_var(const std::string& n) -> Var*;

  auto get_parent_type() const -> Types::Type* { return parent_type; }
  void set_parent_type(Types::Type* p) { parent_type = p; }
};

// 表示一个具有具体实体的函数。
class BodiedFunc : public AcceptorExtend<BodiedFunc, Func> {
 private:
  // 在函数内部定义和使用的变量列表。
  std::list<Var*> symbols;
  // 指向函数体的指针。
  Value* body = nullptr;
  // 标记函数是否为即时编译（JIT）输入。
  bool jit = false;

 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

  auto begin() { return symbols.begin(); }
  auto end() { return symbols.end(); }
  auto begin() const { return symbols.begin(); }
  auto end() const { return symbols.end(); }

  auto front() -> Var* { return symbols.front(); }
  auto back() -> Var* { return symbols.back(); }
  auto front() const -> const Var* { return symbols.front(); }
  auto back() const -> const Var* { return symbols.back(); }

  template <typename It>
  auto insert(It pos, Var* v) {
    return symbols.insert(pos, v);
  }
  void push_back(Var* v) { symbols.push_back(v); }

  template <typename It>
  auto erase(It pos) {
    return symbols.erase(pos);
  }

  auto get_body() -> Flow* { return cast<Flow>(body); }
  auto get_body() const -> const Flow* { return cast<Flow>(body); }
  void set_body(Flow* b) { body = b; }

  auto is_jit() const -> bool { return jit; }
  void set_jit(bool v = true) { jit = v; }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override {
    return body ? std::vector<Value*>{body} : std::vector<Value*>{};
  }
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;

  auto do_get_used_variables() const -> std::vector<Var*> override;
  auto do_replace_used_variable(id_t id, Var* new_var) -> int override;
};

// 表示一个外部函数。
class ExternalFunc : public AcceptorExtend<ExternalFunc, Func> {
 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

  // 检查函数是否为可变参数函数。
  auto is_variadic() const -> bool {
    return cast<Types::FuncType>(get_type())->is_variadic();
  }
};

/// Internal, LLVM-only function.
class InternalFunc : public AcceptorExtend<InternalFunc, Func> {
 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;
};

// 表示内部 LLVM 函数。
class LLVMFunc : public AcceptorExtend<LLVMFunc, Func> {
 private:
  // 需要格式化到函数体中的字面量。
  std::vector<Types::Generic> llvm_literals;
  // llvm-only 函数的声明。
  std::string llvm_declares;
  // llvm-only 函数的体。
  std::string llvm_body;

 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

  // 设置 LLVM 字面量。
  void set_llvm_literals(std::vector<Types::Generic> v) {
    llvm_literals = std::move(v);
  }

  auto literal_begin() { return llvm_literals.begin(); }
  auto literal_end() { return llvm_literals.end(); }
  auto literal_begin() const { return llvm_literals.begin(); }
  auto literal_end() const { return llvm_literals.end(); }

  auto literal_front() -> auto& { return llvm_literals.front(); }
  auto literal_back() -> auto& { return llvm_literals.back(); }
  auto literal_front() const -> auto& { return llvm_literals.front(); }
  auto literal_back() const -> auto& { return llvm_literals.back(); }

  // 获取和设置 LLVM 声明。
  auto get_llvm_declarations() const -> const std::string& {
    return llvm_declares;
  }
  void set_llvm_declarations(std::string v) { llvm_declares = std::move(v); }
  // 获取和设置 LLVM 函数体。
  auto get_llvm_body() const -> const std::string& { return llvm_body; }
  void set_llvm_body(std::string v) { llvm_body = std::move(v); }

 protected:
  auto do_get_used_types() const -> std::vector<Types::Type*> override;
  auto do_replace_used_type(const std::string& name, Types::Type* new_type)
      -> int override;
};

}  // namespace Pud::IR

#endif  // PUD_IR_FUNC_H