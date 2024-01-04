#ifndef PUD_AST_CONTEXT_H
#define PUD_AST_CONTEXT_H

#include <deque>
#include <list>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Pud/AST/AST.h"

namespace Pud::AST {

// 用于表示变量表或转换上下文，主要用于在编译器设计中处理变量的作用域和生命周期。
// T：变量类型，表示存储在上下文中的对象类型。
// 通过维护一个栈结构，支持嵌套作用域，允许在不同作用域中有相同名称的变量。
template <typename T>
class Context : public std::enable_shared_from_this<Context<T>> {
 public:
  using Item = std::shared_ptr<T>;

 protected:
  // 一个映射类型，用哈希表存储字符串到变量列表的映射。
  using Map = std::unordered_map<std::string, std::list<Item>>;
  // 标识符到对象栈的映射，其中对象栈表示嵌套作用域中的对象。
  Map map;
  // 栈结构，用于跟踪当前和外层的作用域。
  std::deque<std::list<std::string>> stack;

 private:
  // 当前上下文的标志集合。
  std::unordered_set<std::string> flags;
  // 当前模块的绝对路径。
  std::string filename;
  // 用于获取当前表达式的源信息的 SourceInfo 栈。
  std::vector<SourceInfo> src_infos;

 public:
  explicit Context(std::string filename) : filename(std::move(filename)) {
    stack.push_front(std::list<std::string>());
  }
  virtual ~Context() = default;

  // 向当前作用域添加一个新变量。
  virtual void add(const std::string& name, const Item& var) {
    seqassertn(!name.empty(), "adding an empty identifier");
    map[name].push_front(var);
    stack.front().push_back(name);
  }

  // 从当前作用域中移除一个变量。
  void remove(const std::string& name) {
    remove_from_map(name);
    for (auto& s : stack) {
      auto i = std::find(s.begin(), s.end(), name);
      if (i != s.end()) {
        s.erase(i);
        return;
      }
    }
  }

  // 在当前上下文中查找一个变量。
  virtual auto find(const std::string& name) const -> Item {
    auto it = map.find(name);
    return it != map.end() ? it->second.front() : nullptr;
  }

  // 添加一个新的块（作用域层级）。
  virtual void add_block() { stack.push_front(std::list<std::string>()); }
  // 移除当前的块（作用域层级）。
  virtual void pop_block() {
    for (auto& name : stack.front()) {
      remove_from_map(name);
    }
    stack.pop_front();
  }

  // 获取和设置当前模块的文件名。
  auto get_filename() const -> std::string { return filename; }
  void set_filename(std::string file) { filename = std::move(file); }

  // 允许基于范围的循环遍历上下文。
  auto begin() -> typename Map::iterator { return map.begin(); }
  auto end() -> typename Map::iterator { return map.end(); }

  virtual void dump() {}

 private:
  // 从映射中移除一个标识符。
  void remove_from_map(const std::string& name) {
    auto i = map.find(name);
    if (i == map.end()) {
      return;
    }
    seqassertn(i->second.size(), "identifier {} not found in the map", name);
    i->second.pop_front();
    if (!i->second.size()) {
      map.erase(name);
    }
  }

 public:
  // 用于管理源代码信息的栈。
  void push_source_info(SourceInfo s) { src_infos.emplace_back(std::move(s)); }
  void pop_source_info() { src_infos.pop_back(); }
  auto get_source_info() const -> SourceInfo { return src_infos.back(); }
};

}  // namespace Pud::AST

#endif  // PUD_PARSE_CONTEXT_H