#include "Pud/Common/Str.h"

#include <sstream>

namespace Pud {

auto split(const std::string& s, char delim) -> std::vector<std::string> {
  std::vector<std::string> items;
  std::string item;
  std::istringstream iss(s);
  while (std::getline(iss, item, delim)) {
    items.push_back(item);
  }
  return items;
}

auto escape(const std::string& str) -> std::string {
  std::string r;
  r.reserve(str.size());
  for (unsigned char c : str) {
    switch (c) {
      case '\a':
        r += "\\a";
        break;
      case '\b':
        r += "\\b";
        break;
      case '\f':
        r += "\\f";
        break;
      case '\n':
        r += "\\n";
        break;
      case '\r':
        r += "\\r";
        break;
      case '\t':
        r += "\\t";
        break;
      case '\v':
        r += "\\v";
        break;
      case '\'':
        r += "\\'";
        break;
      case '\\':
        r += "\\\\";
        break;
      default:
        if (c < 32 || c >= 127) {
          r += fmt::format("\\x{:x}", c);
        } else {
          r += c;
        }
    }
  }
  return r;
}

auto unescape(const std::string& str) -> std::string {
  std::string r;
  r.reserve(str.size());
  for (int i = 0; i < str.size(); i++) {
    if (str[i] == '\\' && i + 1 < str.size()) {
      switch (str[i + 1]) {
        case 'a':
          r += '\a';
          i++;
          break;
        case 'b':
          r += '\b';
          i++;
          break;
        case 'f':
          r += '\f';
          i++;
          break;
        case 'n':
          r += '\n';
          i++;
          break;
        case 'r':
          r += '\r';
          i++;
          break;
        case 't':
          r += '\t';
          i++;
          break;
        case 'v':
          r += '\v';
          i++;
          break;
        case '"':
          r += '\"';
          i++;
          break;
        case '\'':
          r += '\'';
          i++;
          break;
        case '\\':
          r += '\\';
          i++;
          break;
        case 'x': {
          if (i + 3 > str.size()) {
            throw std::invalid_argument("invalid \\x code");
          }
          size_t pos = 0;
          auto code = std::stoi(str.substr(i + 2, 2), &pos, 16);
          r += static_cast<char>(code);
          i += pos + 1;
          break;
        }
        default:
          if (str[i + 1] >= '0' && str[i + 1] <= '7') {
            size_t pos = 0;
            auto code = std::stoi(str.substr(i + 1, 3), &pos, 8);
            r += static_cast<char>(code);
            i += pos;
          } else {
            r += str[i];
          }
      }
    } else {
      r += str[i];
    }
  }
  return r;
}

auto escape_fstring_braces(const std::string& str, int start, int len)
    -> std::string {
  std::string t;
  t.reserve(len);
  for (int i = start; i < start + len; i++) {
    if (str[i] == '{') {
      t += "{{";
    } else if (str[i] == '}') {
      t += "}}";
    } else {
      t += str[i];
    }
  }
  return t;
}

auto find_star(const std::string& s) -> int {
  int i = 0;
  for (; i < s.size(); i++) {
    if (s[i] == ' ' || s[i] == ')') {
      break;
    }
  }
  return i;
}

auto startswith(const std::string& str, const std::string& prefix) -> size_t {
  return (str.size() >= prefix.size() && str.substr(0, prefix.size()) == prefix)
             ? prefix.size()
             : 0;
}

auto endswith(const std::string& str, const std::string& suffix) -> size_t {
  return (str.size() >= suffix.size() &&
          str.substr(str.size() - suffix.size()) == suffix)
             ? suffix.size()
             : 0;
}

void ltrim(std::string& str) {
  str.erase(str.begin(),
            std::find_if(str.begin(), str.end(),
                         [](unsigned char ch) { return !std::isspace(ch); }));
}

void rtrim(std::string& str) {
  /// https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
  str.erase(std::find_if(str.rbegin(), str.rend(),
                         [](unsigned char ch) { return !std::isspace(ch); })
                .base(),
            str.end());
}

auto trim_stars(std::string& str) -> int {
  int stars = 0;
  for (; stars < str.size() && str[stars] == '*'; stars++) {
    ;
  }
  str = str.substr(stars);
  return stars;
}

auto isdigit(const std::string& str) -> bool {
  return std::all_of(str.begin(), str.end(), ::isdigit);
}

}  // namespace Pud