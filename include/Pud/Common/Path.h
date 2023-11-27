#ifndef PUD_COMMON_PATH_H
#define PUD_COMMON_PATH_H

#include <memory>
#include <string>
#include <vector>

namespace Pud {

/// @return The absolute canonical path of a given path.
auto get_absolute_path(const std::string& path) -> std::string;

/// Detect an absolute path of the current executable (whose argv0 is known).
/// @return Absolute executable path or argv0 if one cannot be found.
auto executable_path(const char* argv0) -> std::string;
/// Detect an absolute path of the current libcodonc.
/// @return Absolute executable path or argv0 if one cannot be found.
auto library_path() -> std::string;

struct ImportFile {
  enum Status : uint8_t { STDLIB, PACKAGE };
  Status status;
  /// Absolute path of an import.
  std::string path;
  /// Module name (e.g. foo.bar.baz).
  std::string module;
};

auto get_import_file(const std::string& argv0, const std::string& what,
                     const std::string& relative_to, bool force_stdlib = false,
                     const std::string& module0 = "",
                     const std::vector<std::string>& plugins = {})
    -> std::shared_ptr<ImportFile>;
}  // namespace Pud

#endif  // PUD_COMMON_PATH_H