#ifndef PUD_DSL_DSL_H
#define PUD_DSL_DSL_H

#include <functional>
#include <string>
#include <vector>

#include "Pud/AST/Cache.h"
#include "Pud/IR/IR.h"
#include "Pud/Transform/Manager.h"
#include "Pud/Transform/Pass.h"
#include "llvm/Passes/PassBuilder.h"

namespace Pud {

/// Base class for DSL plugins. Plugins will return an instance of
/// a child of this class, which defines various characteristics of
/// the DSL, like keywords and IR passes.
class DSL {
 public:
  /// General information about this plugin.
  struct Info {
    /// Extension name
    std::string name;
    /// Extension description
    std::string description;
    /// Extension version
    std::string version;
    /// Extension URL
    std::string url;
    /// Supported Codon versions (semver range)
    std::string supported;
    /// Plugin stdlib path
    std::string stdlib_path;
    /// Plugin dynamic library path
    std::string dylib_path;
    /// Linker arguments (to replace "-l dylibPath" if present)
    std::vector<std::string> link_args;
  };

  using KeywordCallback =
      std::function<AST::StmtPtr(AST::SimplifyVisitor*, AST::CustomStmt*)>;

  struct ExprKeyword {
    std::string keyword;
    KeywordCallback callback;
  };

  struct BlockKeyword {
    std::string keyword;
    KeywordCallback callback;
    bool has_expr;
  };

  virtual ~DSL() noexcept = default;

  /// Registers this DSL's IR passes with the given pass manager.
  /// @param pm the pass manager to add the passes to
  /// @param debug true if compiling in debug mode
  virtual void add_ir_passes(IR::Transform::PassManager* pm, bool debug) {}

  /// Registers this DSL's LLVM passes with the given pass builder.
  /// @param pb the pass builder to add the passes to
  /// @param debug true if compiling in debug mode
  virtual void add_llvm_passes(llvm::PassBuilder* pb, bool debug) {}

  /// Returns a vector of "expression keywords", defined as keywords of
  /// the form "keyword <expr>".
  /// @return this DSL's expression keywords
  virtual auto get_expr_keywords() -> std::vector<ExprKeyword> { return {}; }

  /// Returns a vector of "block keywords", defined as keywords of the
  /// form "keyword <expr>: <block of code>".
  /// @return this DSL's block keywords
  virtual auto get_block_keywords() -> std::vector<BlockKeyword> { return {}; }
};

}  // namespace Pud

#endif  // PUD_DSL_DSL_H