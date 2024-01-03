#ifndef PUD_COMPILER_DEBUG_LISTENER_H
#define PUD_COMPILER_DEBUG_LISTENER_H

#include <map>
#include <memory>
#include <mutex>
#include <vector>

#include "Pud/LLVM/LLVM.h"

namespace Pud {

/// Debug info tracker for MCJIT.
class DebugListener : public llvm::JITEventListener {
 public:
  class ObjectInfo {
   private:
    ObjectKey key;
    std::unique_ptr<llvm::object::ObjectFile> object;
    std::unique_ptr<llvm::MemoryBuffer> buffer;
    uintptr_t start;
    uintptr_t stop;

   public:
    ObjectInfo(ObjectKey key, std::unique_ptr<llvm::object::ObjectFile> object,
               std::unique_ptr<llvm::MemoryBuffer> buffer, uintptr_t start,
               uintptr_t stop)
        : key(key),
          object(std::move(object)),
          buffer(std::move(buffer)),
          start(start),
          stop(stop) {}

    auto get_key() const -> ObjectKey { return key; }
    auto get_object() const -> const llvm::object::ObjectFile& {
      return *object;
    }
    auto get_start() const -> uintptr_t { return start; }
    auto get_stop() const -> uintptr_t { return stop; }
    auto contains(uintptr_t pc) const -> bool {
      return start <= pc && pc < stop;
    }
  };

 private:
  std::vector<ObjectInfo> objects;

  void notifyObjectLoaded(
      ObjectKey key, const llvm::object::ObjectFile& obj,
      const llvm::RuntimeDyld::LoadedObjectInfo& L) override;
  void notifyFreeingObject(ObjectKey key) override;

 public:
  DebugListener() : llvm::JITEventListener(), objects() {}

  auto symbolize(uintptr_t pc) -> llvm::Expected<llvm::DILineInfo>;
  auto get_pretty_backtrace(uintptr_t pc) -> llvm::Expected<std::string>;
  auto get_pretty_backtrace(const std::vector<uintptr_t>& backtrace)
      -> std::string;
};

/// Debug info tracker for JITLink. Adapted from Julia's implementation:
/// https://github.com/JuliaLang/julia/blob/master/src/jitlayers.cpp
class DebugPlugin : public llvm::orc::ObjectLinkingLayer::Plugin {
  struct JITObjectInfo {
    std::unique_ptr<llvm::MemoryBuffer> backing_buffer;
    std::unique_ptr<llvm::object::ObjectFile> object;
    llvm::StringMap<uint64_t> section_load_addresses;
  };

  std::mutex plugin_mutex;
  std::map<llvm::orc::MaterializationResponsibility*,
           std::unique_ptr<JITObjectInfo>>
      pending_objs;
  std::map<llvm::orc::ResourceKey, std::vector<std::unique_ptr<JITObjectInfo>>>
      registered_objs;

 public:
  void notifyMaterializing(llvm::orc::MaterializationResponsibility& mr,
                           llvm::jitlink::LinkGraph& graph,
                           llvm::jitlink::JITLinkContext& ctx,
                           llvm::MemoryBufferRef inputObject) override;
  auto notifyEmitted(llvm::orc::MaterializationResponsibility& mr)
      -> llvm::Error override;
  auto notifyFailed(llvm::orc::MaterializationResponsibility& mr)
      -> llvm::Error override;
  auto notifyRemovingResources(llvm::orc::ResourceKey key)
      -> llvm::Error override;
  void notifyTransferringResources(llvm::orc::ResourceKey dstKey,
                                   llvm::orc::ResourceKey srcKey) override;
  void modifyPassConfig(llvm::orc::MaterializationResponsibility& mr,
                        llvm::jitlink::LinkGraph&,
                        llvm::jitlink::PassConfiguration& config) override;

  auto symbolize(uintptr_t pc) -> llvm::Expected<llvm::DILineInfo>;
  auto get_pretty_backtrace(uintptr_t pc) -> llvm::Expected<std::string>;
  auto get_pretty_backtrace(const std::vector<uintptr_t>& backtrace)
      -> std::string;
};

}  // namespace Pud

#endif  // PUD_COMPILER_DEBUG_LISTENER_H