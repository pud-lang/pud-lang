#ifndef PUD_TRANSFORM_PYTHONIC_LIST_H
#define PUD_TRANSFORM_PYTHONIC_LIST_H

#include "Pud/Transform/Pass.h"

namespace Pud::IR::Transform::Pythonic {

/// Pass to optimize list1 + list2 + ...
/// Also handles list slices and list literals efficiently.
class ListAdditionOptimization : public OperatorPass {
 public:
  static const std::string KEY;
  auto get_key() const -> std::string override { return KEY; }
  void handle(CallInstr* v) override;
};

}  // namespace Pud::IR::Transform::Pythonic

#endif  // PUD_TRANSFORM_PYTHONIC_LIST_H