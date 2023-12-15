#ifndef PUD_RUNTIME_RUNTIME_H
#define PUD_RUNTIME_RUNTIME_H

#include <unwind.h>

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#define PUD_FLAG_DEBUG (1 << 0)           // compiled/running in debug mode
#define PUD_FLAG_CAPTURE_OUTPUT (1 << 1)  // capture writes to stdout/stderr
#define PUD_FLAG_STANDALONE (1 << 2)  // compiled as a standalone object/binary

#define PUD_FUNC extern "C"

using pud_int_t = int64_t;

struct pud_str_t {
  pud_int_t len;
  char* str;
};

struct pud_time_t {
  int16_t year;
  int16_t yday;
  int8_t sec;
  int8_t min;
  int8_t hour;
  int8_t mday;
  int8_t mon;
  int8_t wday;
  int8_t isdst;
};

PUD_FUNC int pud_flags;

PUD_FUNC void pud_init(int flags);

PUD_FUNC auto pud_is_macos() -> bool;
PUD_FUNC auto pud_pid() -> pud_int_t;
PUD_FUNC auto pud_time() -> pud_int_t;
PUD_FUNC auto pud_time_monotonic() -> pud_int_t;
PUD_FUNC auto pud_time_highres() -> pud_int_t;
PUD_FUNC auto pud_localtime(pud_int_t secs, pud_time_t* output) -> bool;
PUD_FUNC auto pud_gmtime(pud_int_t secs, pud_time_t* output) -> bool;
PUD_FUNC auto pud_mktime(pud_time_t* time) -> pud_int_t;
PUD_FUNC void pud_sleep(double secs);
PUD_FUNC auto pud_env() -> char**;
PUD_FUNC void pud_assert_failed(pud_str_t file, pud_int_t line);

PUD_FUNC auto pud_alloc(size_t n) -> void*;
PUD_FUNC auto pud_alloc_atomic(size_t n) -> void*;
PUD_FUNC auto pud_calloc(size_t m, size_t n) -> void*;
PUD_FUNC auto pud_calloc_atomic(size_t m, size_t n) -> void*;
PUD_FUNC auto pud_realloc(void* p, size_t newsize, size_t oldsize) -> void*;
PUD_FUNC void pud_free(void* p);
PUD_FUNC void pud_register_finalizer(void* p, void (*f)(void* obj, void* data));

PUD_FUNC void pud_gc_add_roots(void* start, void* end);
PUD_FUNC void pud_gc_remove_roots(void* start, void* end);
PUD_FUNC void pud_gc_clear_roots();
PUD_FUNC void pud_gc_exclude_static_roots(void* start, void* end);

PUD_FUNC auto pud_alloc_exc(int type, void* obj) -> void*;
PUD_FUNC void pud_throw(void* exc);
PUD_FUNC auto pud_personality(int version, _Unwind_Action actions,
                              uint64_t exception_class,
                              _Unwind_Exception* exception_object,
                              _Unwind_Context* context) -> _Unwind_Reason_Code;
PUD_FUNC auto pud_exc_offset() -> int64_t;
PUD_FUNC auto pud_exc_class() -> uint64_t;

PUD_FUNC auto pud_str_int(pud_int_t n, pud_str_t format, bool* error)
    -> pud_str_t;
PUD_FUNC auto pud_str_uint(pud_int_t n, pud_str_t format, bool* error)
    -> pud_str_t;
PUD_FUNC auto pud_str_float(double f, pud_str_t format, bool* error)
    -> pud_str_t;
PUD_FUNC auto pud_str_ptr(void* p, pud_str_t format, bool* error) -> pud_str_t;
PUD_FUNC auto pud_str_str(pud_str_t s, pud_str_t format, bool* error)
    -> pud_str_t;

PUD_FUNC auto pud_stdin() -> void*;
PUD_FUNC auto pud_stdout() -> void*;
PUD_FUNC auto pud_stderr() -> void*;

PUD_FUNC void pud_print(pud_str_t str);
PUD_FUNC void pud_print_full(pud_str_t str, FILE* fo);

PUD_FUNC auto pud_lock_new() -> void*;
PUD_FUNC auto pud_lock_new() -> void*;
PUD_FUNC auto pud_lock_acquire(void* lock, bool block, double timeout) -> bool;
PUD_FUNC void pud_lock_release(void* lock);
PUD_FUNC auto pud_rlock_new() -> void*;
PUD_FUNC auto pud_rlock_acquire(void* lock, bool block, double timeout) -> bool;
PUD_FUNC void pud_rlock_release(void* lock);

namespace Pud::Runtime {
class JITError : public std::runtime_error {
 private:
  std::string output;
  std::string type;
  std::string file;
  int line;
  int col;
  std::vector<uintptr_t> backtrace;

 public:
  JITError(std::string output, const std::string& what, std::string type,
           std::string file, int line, int col,
           std::vector<uintptr_t> backtrace = {})
      : std::runtime_error(what),
        output(std::move(output)),
        type(std::move(type)),
        file(std::move(file)),
        line(line),
        col(col),
        backtrace(std::move(backtrace)) {}

  auto get_output() const -> std::string { return output; }
  auto get_type() const -> std::string { return type; }
  auto get_file() const -> std::string { return file; }
  auto get_line() const -> int { return line; }
  auto get_col() const -> int { return col; }
  auto get_backtrace() const -> std::vector<uintptr_t> { return backtrace; }
};

auto make_backtrace_frame_string(uintptr_t pc, const std::string& func = "",
                                 const std::string& file = "", int line = 0,
                                 int col = 0) -> std::string;

auto get_captured_output() -> std::string;

void set_jit_error_callback(std::function<void(const JITError&)> callback);
}  // namespace Pud::Runtime

#endif  // PUD_RUNTIME_RUNTIME_H