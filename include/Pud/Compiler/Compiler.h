#ifndef PUD_COMPILER_COMPILER_H
#define PUD_COMPILER_COMPILER_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "Pud/LLVM/LLVMVisitor.h"
#include "Pud/IR/Module.h"
#include "Pud/Transform/Manager.h"
#include "Pud/Common/Common.h"
#include "Pud/DSL/Plugin.h"
#include "Pud/AST/Cache.h"

namespace Pud {

class Compiler {
 public:
  enum Mode {
    DEBUG,
    RELEASE,
    JIT,
  };

 private:
  std::string argv0;
  bool debug;
  bool pyNumerics;
  bool pyExtension;
  std::string input;
  std::unique_ptr<PluginManager> plm;
  std::unique_ptr<AST::Cache> cache;
  std::unique_ptr<IR::Module> module;
  std::unique_ptr<IR::Transform::PassManager> pm;
  std::unique_ptr<IR::LLVMVisitor> llvisitor;

  llvm::Error parse(
      bool isCode, const std::string& file, const std::string& code,
      int startLine, int testFlags,
      const std::unordered_map<std::string, std::string>& defines);
};

}  // namespace Pud

#endif  // PUD_COMPILER_COMPILER_H