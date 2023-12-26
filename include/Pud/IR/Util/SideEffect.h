#ifndef PUD_IR_UTIL_SIDE_EFFECT_H
#define PUD_IR_UTIL_SIDE_EFFECT_H

#include <string>

namespace Pud::IR::Util {

enum SideEffectStatus {
  PURE = 0,
  NO_SIDE_EFFECT,
  NO_CAPTURE,
  UNKNOWN,
};

extern const std::string NON_PURE_ATTR;
extern const std::string PURE_ATTR;
extern const std::string NO_SIDE_EFFECT_ATTR;
extern const std::string NO_CAPTURE_ATTR;
extern const std::string DERIVES_ATTR;
extern const std::string SELF_CAPTURES_ATTR;

}  // namespace Pud::IR::Util

#endif  // PUD_IR_UTIL_SIDE_EFFECT_H