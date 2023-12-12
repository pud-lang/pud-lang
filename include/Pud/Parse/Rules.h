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

#include "Pud/AST/Cache.h"
#include "Pud/Parse/peglib.h"

namespace Pud::Parse {

struct ParseContext {
  AST::Cache* cache;
  std::stack<int> indent;
  int parens;
  int line_offset, col_offset;
  ParseContext(AST::Cache* cache, int parens = 0, int line_offset = 0,
               int col_offset = 0)
      : cache(cache),
        parens(parens),
        line_offset(line_offset),
        col_offset(col_offset) {}

  auto has_custom_stmt_keyword(const std::string& kwd, bool has_expr) const
      -> bool {
    auto i = cache->custom_block_stmts.find(kwd);
    if (i != cache->custom_block_stmts.end()) {
      return i->second.first == has_expr;
    }
    return false;
  }

  auto has_custom_expr_stmt(const std::string& kwd) const -> bool {
    return in(cache->custom_expr_stmts, kwd);
  }
};

}  // namespace Pud::Parse

void init_pud_rules(peg::Grammar&);
void init_pud_actions(peg::Grammar&);

#endif  // PUD_PARSE_RULES_H