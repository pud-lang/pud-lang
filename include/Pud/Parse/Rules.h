#ifndef PUD_PARSE_RULES_H
#define PUD_PARSE_RULES_H

#include <any>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <memory>
#include <stack>
#include <string>
#include <vector>

#include "Pud/Parse/peglib.h"

namespace Pud::Parse {

struct ParseContext {
  std::stack<int> indent;
  int parens;
  int line_offset, col_offset;
  ParseContext(int parens = 0, int line_offset = 0,  // NOLINT(*)
               int col_offset = 0)
      : parens(parens), line_offset(line_offset), col_offset(col_offset) {}

  bool has_custom_stmt_keyword(const std::string &kwd, bool has_expr) const {
    return false;
  }
  bool has_custom_expr_stmt(const std::string &kwd) const {
    return false;
  }
};

}  // namespace Pud::Parse

void init_pud_rules(peg::Grammar&);
void init_pud_actions(peg::Grammar&);

#endif  // PUD_PARSE_RULES_H