#ifndef PUD_TRANSFORM_PYTHONIC_STR_H
#define PUD_TRANSFORM_PYTHONIC_STR_H

#include "Pud/Transform/Pass.h"

namespace Pud::IR::Transform::Pythonic {

/// Pass to optimize str1 + str2 + ...
class StrAdditionOptimization : public OperatorPass {
 public:
  static const std::string KEY;
  auto get_key() const -> std::string override { return KEY; }
  void handle(CallInstr* v) override;
};

}  // namespace Pud::IR::Transform::Pythonic

#endif  // PUD_TRANSFORM_PYTHONIC_STR_H