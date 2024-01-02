#ifndef PUD_DSL_PLUGIN_H
#define PUD_DSL_PLUGIN_H

#include <functional>
#include <memory>
#include <string>
#include <vector>

#include "Pud/Common/Error.h"
#include "Pud/DSL/DSL.h"
#include "Pud/IR/Util/Iterators.h"
#include "llvm/Support/DynamicLibrary.h"

namespace Pud {

/// Plugin metadata
struct Plugin {
  /// the associated DSL
  std::unique_ptr<DSL> dsl;
  /// plugin information
  DSL::Info info;
  /// library handle
  llvm::sys::DynamicLibrary handle;

  Plugin(std::unique_ptr<DSL> dsl, DSL::Info info,
         llvm::sys::DynamicLibrary handle)
      : dsl(std::move(dsl)), info(std::move(info)), handle(std::move(handle)) {}
};

/// Manager for loading, applying and unloading plugins.
class PluginManager {
 private:
  /// Codon executable location
  std::string argv0;
  /// vector of loaded plugins
  std::vector<std::unique_ptr<Plugin>> plugins;

 public:
  /// Constructs a plugin manager
  PluginManager(const std::string& argv0) : argv0(argv0), plugins() {}

  /// @return iterator to the first plugin
  auto begin() { return IR::Util::raw_ptr_adaptor(plugins.begin()); }
  /// @return iterator beyond the last plugin
  auto end() { return IR::Util::raw_ptr_adaptor(plugins.end()); }
  /// @return const iterator to the first plugin
  auto begin() const {
    return IR::Util::const_raw_ptr_adaptor(plugins.begin());
  }
  /// @return const iterator beyond the last plugin
  auto end() const { return IR::Util::const_raw_ptr_adaptor(plugins.end()); }

  /// Loads the plugin at the given load path.
  /// @param path path to plugin directory containing "plugin.toml" file
  /// @return plugin pointer if successful, plugin error otherwise
  llvm::Expected<Plugin*> load(const std::string& path);
};

}  // namespace Pud

#endif  // PUD_DSL_PLUGIN_H