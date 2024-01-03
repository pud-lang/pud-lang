#ifndef PUD_JIT_JIT_EXTERN_H
#define PUD_JIT_JIT_EXTERN_H

#include <string>
#include <vector>

namespace Pud::Jit {

class JIT;

struct JITResult {
  void *result;
  std::string message;

  operator bool() const { return message.empty(); }
  static auto success(void *result) -> JITResult { return {result, ""}; }
  static auto error(const std::string &message) -> JITResult { return {nullptr, message}; }
};

auto jit_init(const std::string &name) -> JIT *;

auto jit_execute_python(JIT *jit, const std::string &name,
                           const std::vector<std::string> &types,
                           const std::string &py_module,
                           const std::vector<std::string> &py_vars, void *arg,
                           bool debug) -> JITResult;

auto jit_execute_safe(JIT *jit, const std::string &code, const std::string &file,
                         int line, bool debug) -> JITResult;

auto get_jit_library() -> std::string;

}


#endif  // PUD_JIT_JIT_EXTERN_H