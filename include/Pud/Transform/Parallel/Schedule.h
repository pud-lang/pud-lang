#ifndef PUD_TRANSFORM_PARALLEL_SCHEDULE_H
#define PUD_TRANSFORM_PARALLEL_SCHEDULE_H

#include "Pud/IR/Value.h"

namespace Pud::IR {
class Value;

namespace Transform::Parallel {

struct OMPSched {
  int code;
  bool dynamic;
  IR::Value* threads;
  IR::Value* chunk;
  bool ordered;
  int64_t collapse;
  bool gpu;

  explicit OMPSched(int code = -1, bool dynamic = false,
                    IR::Value* threads = nullptr, IR::Value* chunk = nullptr,
                    bool ordered = false, int64_t collapse = 0,
                    bool gpu = false);
  explicit OMPSched(const std::string& code, IR::Value* threads = nullptr,
                    IR::Value* chunk = nullptr, bool ordered = false,
                    int64_t collapse = 0, bool gpu = false);
  OMPSched(const OMPSched& s) = default;

  auto get_used_values() const -> std::vector<IR::Value*>;
  auto replace_used_value(id_t id, IR::Value* new_value) -> int;
};

}  // namespace Transform::Parallel
}  // namespace Pud::IR

#endif  // PUD_TRANSFORM_PARALLEL_SCHEDULE_H