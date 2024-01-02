#ifndef PUD_TRANSFORM_PASS_H
#define PUD_TRANSFORM_PASS_H

#include "Pud/IR/Module.h"
#include "Pud/IR/Util/Operator.h"

namespace Pud::IR {

namespace Analyze {
struct Result;
}

namespace Transform {

class PassManager;

/// General pass base class.
class Pass {
 private:
  PassManager* manager = nullptr;

 public:
  virtual ~Pass() noexcept = default;

  /// @return a unique key for this pass
  virtual auto get_key() const -> std::string = 0;

  /// Execute the pass.
  /// @param module the module
  virtual void run(Module* module) = 0;

  /// Determine if pass should repeat.
  /// @param num how many times this pass has already run
  /// @return true if pass should repeat
  virtual auto should_repeat(int num) const -> bool { return false; }

  /// Sets the manager.
  /// @param mng the new manager
  virtual void set_manager(PassManager* mng) { manager = mng; }
  /// Returns the result of a given analysis.
  /// @param key the analysis key
  /// @return the analysis result
  template <typename AnalysisType>
  auto get_analysis_result(const std::string& key) -> AnalysisType* {
    return static_cast<AnalysisType*>(do_get_analysis(key));
  }

 private:
  auto do_get_analysis(const std::string& key) -> Analyze::Result*;
};

class PassGroup : public Pass {
 private:
  int repeat;
  std::vector<std::unique_ptr<Pass>> passes;

 public:
  explicit PassGroup(int repeat = 0,
                     std::vector<std::unique_ptr<Pass>> passes = {})
      : Pass(), repeat(repeat), passes(std::move(passes)) {}

  virtual ~PassGroup() noexcept = default;

  void push_back(std::unique_ptr<Pass> p) { passes.push_back(std::move(p)); }

  /// @return default number of times pass should repeat
  auto get_repeat() const -> int { return repeat; }

  /// Sets the default number of times pass should repeat.
  /// @param r number of repeats
  void set_repeat(int r) { repeat = r; }

  auto should_repeat(int num) const -> bool override { return num < repeat; }

  void run(Module* module) override;

  void set_manager(PassManager* mng) override;
};

/// Pass that runs a single Operator.
class OperatorPass : public Pass, public Util::Operator {
 public:
  /// Constructs an operator pass.
  /// @param children_first true if children should be iterated first
  explicit OperatorPass(bool children_first = false)
      : Util::Operator(children_first) {}

  void run(Module* module) override {
    reset();
    process(module);
  }
};

}  // namespace Transform

}  // namespace Pud::IR

#endif  // PUD_TRANSFORM_PASS_H