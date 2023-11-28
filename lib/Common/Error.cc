#include "Pud/Common/Error.h"

namespace Pud {

char ParserErrorInfo::ID = 0;

char RuntimeErrorInfo::ID = 0;

char PluginErrorInfo::ID = 0;

char IOErrorInfo::ID = 0;

void raise_error(const char* format) { throw ParserException(format); }

void raise_error(int e, const Pud::SourceInfo& info, const char* format) {
  throw ParserException(e, format, info);
}

void raise_error(int e, const Pud::SourceInfo& info,
                 const std::string& format) {
  throw ParserException(e, format, info);
}

}  // namespace Pud