#ifndef PUD_COMMON_TIMER_H
#define PUD_COMMON_TIMER_H

#include <chrono>
#include <string>

#include "Pud/Common/Log.h"

namespace Pud {

class Timer {
 private:
  using clock_type = std::chrono::high_resolution_clock;
  std::string name;
  std::chrono::time_point<clock_type> start, end;

 public:
  bool logged;

 public:
  void log() {
    if (!logged) {
      LOG_TIME("[T] {} = {:.3f}", name, elapsed());
      logged = true;
    }
  }

  double elapsed(
      std::chrono::time_point<clock_type> end = clock_type::now()) const {
    return std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
               .count() /
           1000.0;
  }

  Timer(std::string name)
      : name(std::move(name)), start(), end(), logged(false) {
    start = clock_type::now();
  }

  ~Timer() { log(); }
};

}  // namespace Pud

#endif  // PUD_COMMON_TIMER_H