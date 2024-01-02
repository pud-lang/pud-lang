#ifndef PUD_TRANSFORM_PYTHONIC_GENERATOR_H
#define PUD_TRANSFORM_PYTHONIC_GENERATOR_H

#include "Pud/Transform/Pass.h"

namespace Pud::IR::Transform::Pythonic {

/// Pass to optimize passing a generator to some built-in functions
/// like sum(), any() or all(), which will be converted to regular
/// for-loops.
class GeneratorArgumentOptimization : public OperatorPass {
 public:
  static const std::string KEY;
  auto get_key() const -> std::string override { return KEY; }
  void handle(CallInstr* v) override;
};

}  // namespace Pud::IR::Transform::Pythonic

#endif  // PUD_TRANSFORM_PYTHONIC_GENERATOR_H