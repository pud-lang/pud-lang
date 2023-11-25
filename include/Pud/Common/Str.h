#ifndef PUD_COMMON_STR_H
#define PUD_COMMON_STR_H

#include <fmt/format.h>

#include <cinttypes>
#include <string>
#include <vector>

namespace Pud {

inline auto escape(const std::string& str) -> std::string {
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

inline auto unescape(const std::string& str) -> std::string {
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

}  // namespace Pud

#endif  // PUD_COMMON_STR_H