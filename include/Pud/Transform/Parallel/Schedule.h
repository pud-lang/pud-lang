#ifndef PUD_TRANSFORM_PARALLEL_SCHEDULE_H
#define PUD_TRANSFORM_PARALLEL_SCHEDULE_H

#include "Pud/IR/Value.h"

namespace Pud::IR {
class Value;
}  // namespace Pud::IR

namespace Pud::Transform::Parallel {

struct Schedule {
  int code;
  bool dynamic;
  IR::Value* threads;
  IR::Value* chunk;
  bool ordered;
  int64_t collapse;
  bool gpu;

  explicit Schedule(int code = -1, bool dynamic = false,
                    IR::Value* threads = nullptr, IR::Value* chunk = nullptr,
                    bool ordered = false, int64_t collapse = 0,
                    bool gpu = false);
  explicit Schedule(const std::string& code, IR::Value* threads = nullptr,
                    IR::Value* chunk = nullptr, bool ordered = false,
                    int64_t collapse = 0, bool gpu = false);
  Schedule(const Schedule& s) = default;

  auto get_used_values() const -> std::vector<IR::Value*>;
  auto replace_used_value(id_t id, IR::Value* new_value) -> int;
};

}  // namespace Pud::Transform::Parallel

#endif  // PUD_TRANSFORM_PARALLEL_SCHEDULE_H