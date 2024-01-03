#ifndef PUD_JIT_JIT_H
#define PUD_JIT_JIT_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "Pud/AST/Cache.h"
#include "Pud/Compiler/Compiler.h"
#include "Pud/IR/Var.h"
#include "Pud/JIT/Engine.h"
#include "Pud/JIT/JITExtern.h"
#include "Pud/LLVM/LLVMVisitor.h"
#include "Pud/Runtime/Runtime.h"
#include "Pud/Transform/Manager.h"

namespace Pud::Jit {

class JIT {
 public:
  struct PythonData {
    IR::Types::Type* cobj;
    std::unordered_map<std::string, IR::Func*> cache;

    PythonData();
    IR::Types::Type* get_c_obj_type(IR::Module* M);
  };

 private:
  std::unique_ptr<Compiler> compiler;
  std::unique_ptr<Engine> engine;
  std::unique_ptr<PythonData> pydata;
  std::string mode;

 public:
  explicit JIT(const std::string& argv0, const std::string& mode = "");

  auto get_compiler() const -> Compiler* { return compiler.get(); }
  auto get_engine() const -> Engine* { return engine.get(); }

  // General
  auto init() -> llvm::Error;
  auto compile(const IR::Func* input) -> llvm::Error;
  auto compile(const std::string& code, const std::string& file = "",
               int line = 0) -> llvm::Expected<IR::Func*>;
  auto address(const IR::Func* input) -> llvm::Expected<void*>;
  auto run(const IR::Func* input) -> llvm::Expected<std::string>;
  auto execute(const std::string& code, const std::string& file = "",
               int line = 0, bool debug = false) -> llvm::Expected<std::string>;

  // Python
  auto run_python_wrapper(const IR::Func* wrapper, void* arg)
      -> llvm::Expected<void*>;
  auto get_wrapper_func(const std::string& name,
                        const std::vector<std::string>& types)
      -> llvm::Expected<IR::Func*>;
  auto execute_python(const std::string& name,
                      const std::vector<std::string>& types,
                      const std::string& pyModule,
                      const std::vector<std::string>& pyVars, void* arg,
                      bool debug) -> JITResult;
  auto execute_safe(const std::string& code, const std::string& file, int line,
                    bool debug) -> JITResult;

  // Errors
  auto handle_jit_error(const Runtime::JITError& e) -> llvm::Error;
};

}  // namespace Pud::Jit

#endif  // PUD_JIT_JIT_H