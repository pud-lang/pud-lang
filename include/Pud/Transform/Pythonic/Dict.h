#ifndef PUD_TRANSFORM_PYTHONIC_DICT_H
#define PUD_TRANSFORM_PYTHONIC_DICT_H

#include "Pud/Transform/Pass.h"

namespace Pud::IR::Transform::Pythonic {

/// Pass to optimize calls of form d[x] = func(d[x], any).
/// This will work on any dictionary-like object that implements _do_op and
/// _do_op_throws as well as getters.
class DictArithmeticOptimization : public OperatorPass {
 public:
  static const std::string KEY;
  auto get_key() const -> std::string override { return KEY; }
  void handle(CallInstr* v) override;
};

}  // namespace Pud::IR::Transform::Pythonic

#endif  // PUD_TRANSFORM_PYTHONIC_DICT_H