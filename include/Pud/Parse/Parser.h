#ifndef PUD_PARSE_PARSER_H
#define PUD_PARSE_PARSER_H

#include <string>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"

namespace Pud::Parse {

auto parse_code(AST::Cache* cache, const std::string& file,
                const std::string& code, int line_offset = 0)
    -> Pud::AST::StmtPtr;

auto parse_expr(AST::Cache* cache, const std::string& code,
                const SourceInfo& offset)
    -> std::pair<Pud::AST::ExprPtr, std::string>;

auto parse_file(AST::Cache* cache, const std::string& file)
    -> Pud::AST::StmtPtr;

auto parse_openmp(AST::Cache* cache, const std::string& code,
                  const SourceInfo& loc) -> std::vector<AST::CallExpr::Arg>;

}  // namespace Pud::Parse

#endif  // PUD_PARSE_PARSER_H