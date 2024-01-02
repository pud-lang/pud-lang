#ifndef PUD_ANALYZE_ANALYSIS_H
#define PUD_ANALYZE_ANALYSIS_H

#include <memory>

#include "Pud/IR/Module.h"
#include "Pud/Transform/Pass.h"

namespace Pud::IR::Analyze {

/// Analysis result base struct.
struct Result {
  virtual ~Result() noexcept = default;
};

/// Base class for IR analyses.
class Analysis {
 private:
  Transform::PassManager* manager = nullptr;

 public:
  virtual ~Analysis() noexcept = default;

  /// @return a unique key for this pass
  virtual auto get_key() const -> std::string = 0;

  /// Execute the analysis.
  /// @param module the module
  virtual auto run(const Module* module) -> std::unique_ptr<Result> = 0;

  /// Sets the manager.
  /// @param mng the new manager
  void set_manager(Transform::PassManager* mng) { manager = mng; }
  /// Returns the result of a given analysis.
  /// @param key the analysis key
  template <typename AnalysisType>
  auto get_analysis_result(const std::string& key) -> AnalysisType* {
    return static_cast<AnalysisType*>(do_get_analysis(key));
  }

 private:
  auto do_get_analysis(const std::string& key) -> Analyze::Result*;
};

}  // namespace Pud::IR::Analyze

#endif  // PUD_ANALYZE_ANALYSIS_H