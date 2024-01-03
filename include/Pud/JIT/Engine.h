#ifndef PUD_JIT_ENGINE_H
#define PUD_JIT_ENGINE_H

#include <memory>
#include <vector>

#include "Pud/Compiler/DebugListener.h"
#include "Pud/LLVM/LLVM.h"

namespace Pud::Jit {

class Engine {
 private:
  std::unique_ptr<llvm::orc::ExecutionSession> sess;
  std::unique_ptr<llvm::orc::EPCIndirectionUtils> epciu;

  llvm::DataLayout layout;
  llvm::orc::MangleAndInterner mangle;

  llvm::orc::RTDyldObjectLinkingLayer object_layer;
  llvm::orc::IRCompileLayer compile_layer;
  llvm::orc::IRTransformLayer optimize_layer;
  llvm::orc::CompileOnDemandLayer cod_layer;

  llvm::orc::JITDylib& main_jd;

  std::unique_ptr<DebugListener> db_listener;

  static void handle_lazy_call_through_error();

  static auto optimize_module(llvm::orc::ThreadSafeModule module,
                              const llvm::orc::MaterializationResponsibility& R)
      -> llvm::Expected<llvm::orc::ThreadSafeModule>;

 public:
  Engine(std::unique_ptr<llvm::orc::ExecutionSession> sess,
         std::unique_ptr<llvm::orc::EPCIndirectionUtils> epciu,
         llvm::orc::JITTargetMachineBuilder jtmb, llvm::DataLayout layout);

  ~Engine();

  static auto create() -> llvm::Expected<std::unique_ptr<Engine>>;

  auto get_data_layout() const -> const llvm::DataLayout& { return layout; }

  auto get_main_jit_dylib() -> llvm::orc::JITDylib& { return main_jd; }

  auto get_debug_listener() const -> DebugListener* {
    return db_listener.get();
  }

  auto add_module(llvm::orc::ThreadSafeModule module,
                  llvm::orc::ResourceTrackerSP rt = nullptr) -> llvm::Error;

  auto lookup(llvm::StringRef name) -> llvm::Expected<llvm::JITEvaluatedSymbol>;
};

}  // namespace Pud::JIT

#endif  // PUD_JIT_ENGINE_H