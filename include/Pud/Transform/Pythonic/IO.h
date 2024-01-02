#ifndef PUD_TRANSFORM_PYTHONIC_IO_H
#define PUD_TRANSFORM_PYTHONIC_IO_H

#include "Pud/Transform/Pass.h"

namespace Pud::IR::Transform::Pythonic {

/// Pass to optimize print str.cat(...) or file.write(str.cat(...)).
class IOCatOptimization : public OperatorPass {
 public:
  static const std::string KEY;
  auto get_key() const -> std::string override { return KEY; }
  void handle(CallInstr* v) override;
};

}  // namespace Pud::IR::Transform::Pythonic

#endif  // PUD_TRANSFORM_PYTHONIC_IO_H