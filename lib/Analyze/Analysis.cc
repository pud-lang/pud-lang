#include "Pud/Analyze/Analysis.h"

#include "Pud/Transform/Manager.h"

namespace Pud::IR::Analyze {

auto Analysis::do_get_analysis(const std::string& key) -> Result* {
  return manager ? manager->get_analysis_result(key) : nullptr;
}

}  // namespace Pud::IR::Analyze