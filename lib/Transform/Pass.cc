#include "Pud/Transform/Pass.h"

#include "Pud/Transform/Manager.h"

namespace Pud::IR::Transform {

auto Pass::do_get_analysis(const std::string& key) -> Analyze::Result* {
  return manager ? manager->get_analysis_result(key) : nullptr;
}

void PassGroup::run(Module* module) {
  for (auto& p : passes)
    p->run(module);
}

void PassGroup::set_manager(PassManager* mng) {
  Pass::set_manager(mng);
  for (auto& p : passes)
    p->set_manager(mng);
}

}  // namespace Pud::IR::Transform