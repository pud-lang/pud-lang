#include "Pud/Parse/Parser.h"

#include <any>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/Common/Common.h"
#include "Pud/Format/Format.h"
#include "Pud/Parse/Rules.h"

double total_peg = 0.0;

namespace Pud::Parse {

static std::shared_ptr<peg::Grammar> grammar(nullptr);
static std::shared_ptr<peg::Grammar> omp_grammar(nullptr);

auto init_parser() -> std::shared_ptr<peg::Grammar> {
  auto g = std::make_shared<peg::Grammar>();
  init_pud_rules(*g);
  init_pud_actions(*g);
  ~(*g)["NLP"] <=
      peg::usr([](const char* s, size_t n, peg::SemanticValues&, std::any& dt) {
        auto e = (n >= 1 && s[0] == '\\' ? 1 : -1);
        if (std::any_cast<ParseContext&>(dt).parens && e == -1)
          e = 0;
        return e;
      });
  for (auto& x : *g) {
    auto v = peg::LinkReferences(*g, x.second.params);
    x.second.accept(v);
  }
  (*g)["program"].enablePackratParsing = true;
  (*g)["fstring"].enablePackratParsing = true;
  for (auto& rule :
       std::vector<std::string>{"arguments", "slices", "genexp", "parentheses",
                                "star_parens", "generics", "with_parens_item",
                                "params", "from_as_parens", "from_params"}) {
    (*g)[rule].enter = [](const peg::Context&, const char*, size_t,
                          std::any& dt) {
      std::any_cast<ParseContext&>(dt).parens++;
    };
    (*g)[rule.c_str()].leave = [](const peg::Context&, const char*, size_t,
                                  size_t, std::any&, std::any& dt) {
      std::any_cast<ParseContext&>(dt).parens--;
    };
  }
  return g;
}

template <typename T>
auto parse_code(AST::Cache* cache, const std::string& file,
                const std::string& code, int line_offset, int col_offset,
                const std::string& rule) -> T {
  Timer t("");
  t.logged = true;
  // Initialize
  if (!grammar) {
    grammar = init_parser();
  }

  std::vector<std::tuple<size_t, size_t, std::string>> errors;
  auto log = [&](size_t line, size_t col, const std::string& msg,
                 const std::string&) {
    size_t ed = msg.size();
    if (startswith(msg, "syntax error, unexpected")) {
      auto i = msg.find(", expecting");
      if (i != std::string::npos) {
        ed = i;
      }
    }
    errors.emplace_back(line, col, msg.substr(0, ed));
  };
  T result;
  auto ctx = std::make_any<ParseContext>(cache, 0, line_offset, col_offset);
  auto r = (*grammar)[rule].parse_and_get_value(code.c_str(), code.size(), ctx,
                                                result, file.c_str(), log);
  auto ret = r.ret && r.len == code.size();
  if (!ret) {
    r.error_info.output_log(log, code.c_str(), code.size());
  }
  total_peg += t.elapsed();
  ParserException ex;
  if (!errors.empty()) {
    for (auto& e : errors) {
      ex.track(fmt::format("{}", std::get<2>(e)),
               SourceInfo(file, std::get<0>(e), std::get<1>(e), 0));
    }
    throw ex;
    return T();
  }
  return result;
}

auto parse_code(AST::Cache* cache, const std::string& file,
                const std::string& code, int line_offset) -> AST::StmtPtr {
  return parse_code<AST::StmtPtr>(cache, file, code + "\n", line_offset, 0,
                                  "program");
}

auto parse_expr(AST::Cache* cache, const std::string& code,
                const Pud::SourceInfo& offset)
    -> std::pair<AST::ExprPtr, std::string> {
  auto new_code = code;
  ltrim(new_code);
  rtrim(new_code);
  auto e = parse_code<std::pair<AST::ExprPtr, std::string>>(
      cache, offset.file, new_code, offset.line, offset.column, "fstring");
  return e;
}

auto parse_file(AST::Cache* cache, const std::string& file) -> AST::StmtPtr {
  std::vector<std::string> lines;
  std::string code;
  if (file == "-") {
    for (std::string line; getline(std::cin, line);) {
      lines.push_back(line);
      code += line + "\n";
    }
  } else {
    std::ifstream fin(file);
    if (!fin) {
      Err(Error::COMPILER_NO_FILE, SourceInfo(), file);
    }
    for (std::string line; getline(fin, line);) {
      lines.push_back(line);
      code += line + "\n";
    }
    fin.close();
  }

  cache->imports[file].content = lines;
  auto result = parse_code(cache, file, code);
  // For debugging purposes:
  // LOG("peg/{} :=  {}", file, result);
  return result;
}

auto init_openmp_parser() -> std::shared_ptr<peg::Grammar> {
  auto g = std::make_shared<peg::Grammar>();
  init_omp_rules(*g);
  init_omp_actions(*g);
  for (auto& x : *g) {
    auto v = peg::LinkReferences(*g, x.second.params);
    x.second.accept(v);
  }
  (*g)["pragma"].enablePackratParsing = true;
  return g;
}

auto parse_openmp(AST::Cache* cache, const std::string& code,
                  const Pud::SourceInfo& loc)
    -> std::vector<AST::CallExpr::Arg> {
  if (!omp_grammar) {
    omp_grammar = init_openmp_parser();
  }

  std::vector<std::tuple<size_t, size_t, std::string>> errors;
  auto log = [&](size_t line, size_t col, const std::string& msg,
                 const std::string&) { errors.emplace_back(line, col, msg); };
  std::vector<AST::CallExpr::Arg> result;
  auto ctx = std::make_any<ParseContext>(cache, 0, 0, 0);
  auto r = (*omp_grammar)["pragma"].parse_and_get_value(
      code.c_str(), code.size(), ctx, result, "", log);
  auto ret = r.ret && r.len == code.size();
  if (!ret) {
    r.error_info.output_log(log, code.c_str(), code.size());
  }
  ParserException ex;
  if (!errors.empty()) {
    ex.track(fmt::format("openmp {}", std::get<2>(errors[0])), loc);
    throw ex;
  }
  return result;
}

}  // namespace Pud::Parse
