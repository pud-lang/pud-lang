#ifndef PUD_PARSE_PARSER_H
#define PUD_PARSE_PARSER_H

#include <string>

#include "Pud/AST/Expr.h"
#include "Pud/AST/Stmt.h"

namespace Pud::Parse {

class Parser {
 public:
  /// Parse a Seq code block with the appropriate file and position offsets.
  static auto parse_code(const std::string& file, const std::string& code,
                         int line_offset = 0) -> Pud::AST::StmtPtr;
  /// Parse a Seq code expression.
  /// @return pair of ExprPtr and a string indicating format specification
  /// (empty if not available).
  auto parse_expr(const std::string& code, const SourceInfo& offset)
      -> std::pair<Pud::AST::ExprPtr, std::string>;
  /// Parse a Seq file.
  auto parse_file(const std::string& file) -> Pud::AST::StmtPtr;
};

}  // namespace Pud::Parse

#endif  // PUD_PARSE_PARSER_H