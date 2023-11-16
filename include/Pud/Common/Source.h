#ifndef PUD_COMMON_SOURCE_H
#define PUD_COMMON_SOURCE_H

#include <string>

namespace Pud {

struct SourceInfo {
  SourceInfo(std::string file, int line, int col, int len)
      : file(std::move(file)), line(line), column(col), length(len), id(0) {
    static int next_id = 0;
    id = next_id++;
  }

  SourceInfo() : SourceInfo("", 0, 0, 0) {}

  auto operator==(const SourceInfo& src) const -> bool { return id == src.id; }

  std::string file;
  int line;
  int column;
  int length;
  int id;  // 用于区分不同的实例。
};

struct SourceObject {
 public:
  SourceObject() = default;

  SourceObject(const SourceObject& src) { set_source_info(src.info_); }

  virtual ~SourceObject() = default;

  auto get_source_info() const -> SourceInfo { return info_; }

  void set_source_info(SourceInfo info) { info_ = std::move(info); }

 private:
  SourceInfo info_;
};

}  // namespace Pud

#endif  // PUD_COMMON_SOURCE_H