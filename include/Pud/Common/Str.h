#ifndef PUD_COMMON_STR_H
#define PUD_COMMON_STR_H

#include <fmt/format.h>

#include <algorithm>
#include <cinttypes>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace Pud {
/// 将字符串str按照分隔符delim分割成多个子字符串。
/// split("apple,banana,orange", ',')会返回一个包
/// 含["apple", "banana", "orange"]的向量。
auto split(const std::string& str, char delim) -> std::vector<std::string>;
/// 将C风格字符串中的特殊字符（如换行\n）转换为其转义形式（如\\n）。
/// escape("Line1\nLine2")将返回"Line1\\nLine2"。
auto escape(const std::string& str) -> std::string;
/// 执行escape函数的逆操作，将转义序列还原为原始字符。
/// unescape("Line1\\nLine2")将返回"Line1\nLine2"。
auto unescape(const std::string& str) -> std::string;
/// 将F字符串中的大括号转义，例如{变为{{，}变为}}。
/// escape_fstring_braces("{value}", 0, 7)将返回"{{value}}"。
auto escape_fstring_braces(const std::string& str, int start, int len)
    -> std::string;
/// 寻找字符串s中星号（*）的位置。
/// 如果s是"*apple", find_star(s)返回0。
auto find_star(const std::string& s) -> int;
/// 检查字符串str是否以prefix开始。
auto startswith(const std::string& str, const std::string& prefix) -> size_t;
/// 检查字符串str是否以suffix结束。
auto endswith(const std::string& str, const std::string& suffix) -> size_t;
/// 从字符串的左侧移除空白字符。
/// std::string s = " hello ";，调用ltrim(s);会修改s为"hello "。
void ltrim(std::string& str);
/// 从字符串的右侧移除空白字符。
void rtrim(std::string& str);
/// 移除字符串前面的星号，并返回星号数量。
/// std::string s = "***apple"; trim_stars(s);会修改s为"apple"并返回3。
auto trim_stars(std::string& str) -> int;
/// 检查字符串是否只包含数字字符。
auto isdigit(const std::string& str) -> bool;

/// 将集合中的元素以指定的分隔符连接成字符串。
/// join(std::vector<std::string>{"a", "b", "c"}, ", ")返回"a, b, c"。
template <typename T>
auto join(const T& items, const std::string& delim = " ", size_t start = 0,
          size_t end = (1ULL << 31)) -> std::string {
  std::string s;
  if (end > items.size()) {
    end = items.size();
  }
  for (int i = start; i < end; i++) {
    s += (i > start ? delim : "") + items[i];
  }
  return s;
}

/// 类似于join，但是它调用了每个元素的to_string方法来获取字符串表示。
template <typename T>
auto combine(const std::vector<T>& items, const std::string& delim = " ")
    -> std::string {
  std::string s;
  for (int i = 0; i < items.size(); i++) {
    if (items[i]) {
      s += (i ? delim : "") + items[i]->to_string();
    }
  }
  return s;
}

/// 类似于combine，但使用fmt::format进行格式化。
template <typename T>
auto combine2(const std::vector<T>& items, const std::string& delim = ",",
              int start = 0, int end = -1) -> std::string {
  std::string s;
  if (end == -1) {
    end = items.size();
  }
  for (int i = start; i < end; i++) {
    s += (i ? delim : "") + fmt::format("{}", items[i]);
  }
  return s;
}

/// 检查一个元素是否存在于一个集合中。
template <typename T, typename U>
auto in(const std::vector<T>& vec, const U& item, size_t start = 0)
    -> const T* {
  auto f = std::find(vec.begin() + start, vec.end(), item);
  return f != vec.end() ? &(*f) : nullptr;
}
template <typename T, typename U>
auto in(const std::set<T>& s, const U& item) -> const T* {
  auto f = s.find(item);
  return f != s.end() ? &(*f) : nullptr;
}
template <typename T, typename U>
auto in(const std::unordered_set<T>& s, const U& item) -> const T* {
  auto f = s.find(item);
  return f != s.end() ? &(*f) : nullptr;
}
template <typename K, typename V, typename U>
auto in(const std::map<K, V>& m, const U& item) -> const V* {
  auto f = m.find(item);
  return f != m.end() ? &(f->second) : nullptr;
}
template <typename K, typename V, typename U>
auto in(const std::unordered_map<K, V>& m, const U& item) -> const V* {
  auto f = m.find(item);
  return f != m.end() ? &(f->second) : nullptr;
}

/// 对集合中的每个元素应用一个函数，并返回结果集合。
/// vmap(std::vector<int>{1, 2, 3}, [](int x) { return x * x; })返回包含[1, 4,
/// 9]的向量。
template <typename T, typename F>
auto vmap(const std::vector<T>& c, F&& f) {
  std::vector<std::result_of_t<F(const T&)>> ret;
  std::transform(std::begin(c), std::end(c), std::inserter(ret, std::end(ret)),
                 f);
  return ret;
}

}  // namespace Pud

#endif  // PUD_COMMON_STR_H