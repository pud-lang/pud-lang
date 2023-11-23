#include "Pud/Parse/Rules.h"

#include <any>

#include "Pud/AST/Expr.h"
#include "Pud/AST/Stmt.h"
#include "Pud/AST/Types.h"
#include "Pud/Common/Source.h"

using namespace std;
using namespace Pud::AST;
using namespace Pud::Parse;

#define V0 VS[0]
#define V1 VS[1]
#define V2 VS[2]
#define ac std::any_cast
#define ac_expr std::any_cast<ExprPtr>
#define ac_stmt std::any_cast<StmtPtr>
#define SemVals peg::SemanticValues

template <typename F>
auto vmap(const peg::SemanticValues& c, F&& f) {
  return vmap(static_cast<const vector<any>&>(c), f);
}

template <typename Tn, typename Tsv, typename... Ts>
auto ast(Tsv& s, Ts&&... args) {
  auto t = make_shared<Tn>(std::forward<Ts>(args)...);
  t->set_source_info(s);
  return std::static_pointer_cast<typename Tn::BaseType>(t);
}

auto chain(peg::SemanticValues& VS, const Pud::SourceInfo& LOC) {
  auto b = ac_expr(V0);
  for (int i = 1; i < VS.size(); i++)
    b = ast<BinaryExpr>(LOC, b, VS.token_to_string(i - 1), ac_expr(VS[i]));
  return b;
}

auto wrap_tuple(peg::SemanticValues& VS, const Pud::SourceInfo& LOC) {
  if (VS.size() == 1 && VS.tokens.empty())
    return ac_expr(V0);
  return ast<TupleExpr>(LOC, VS.transform<ExprPtr>());
}

void init_pud_rules(peg::Grammar& grammar) {
  using namespace peg;
  using peg::seq;
  using vc = vector<pair<char32_t, char32_t>>;

  // clang-format off
/*
https://docs.python.org/3/library/string.html#formatspec
cfg:
format_spec     ::=  [[fill]align][sign]["z"]["#"]["0"][width][grouping_option]["." precision][type]
fill            ::=  <any character>
align           ::=  "<" | ">" | "=" | "^"
sign            ::=  "+" | "-" | " "
width           ::=  digit+
grouping_option ::=  "_" | ","
precision       ::=  digit+
type            ::=  "b" | "c" | "d" | "e" | "E" | "f" | "F" | "g" | "G" | "n" | "o" | "s" | "x" | "X" | "%"

peg:
format_spec <- ([<>=^] / [^{}] [<>=^])? [+-]? 'z'? '#'? '0'? [0-9]* [_,]* ('.' [0-9]+)? [bcdeEfFgGnosxX%]? {
  return string(VS.sv());
}

16进制数据表示 ASCII 字符码：

0x3c = <, 0x3e = >, 0x3d = =, 0x5e = ^：对应填充和对齐部分。
0x2b = +, 0x2d = -：对应符号部分。
0x30 - 0x39：表示 0 到 9 的数字。
0x5f = _, 0x2c = ,：千位分隔符。
其他部分如 0x62 = b, 0x63 = c, 0x64 = d 等对应类型字符。
*/
  // clang-format on

  grammar["format_spec"] <=
      seq(opt(cho(
              cls(vc{{0x3c, 0x3c}, {0x3e, 0x3e}, {0x3d, 0x3d}, {0x5e, 0x5e}}),
              seq(ncls(vc{{0x7b, 0x7b}, {0x7d, 0x7d}}),
                  cls(vc{{0x3c, 0x3c},
                         {0x3e, 0x3e},
                         {0x3d, 0x3d},
                         {0x5e, 0x5e}})))),
          opt(cls(vc{{0x2b, 0x2b}, {0x2d, 0x2d}})), opt(lit("z")),
          opt(lit("#")), opt(lit("0")), zom(cls(vc{{0x30, 0x39}})),
          zom(cls(vc{{0x5f, 0x5f}, {0x2c, 0x2c}})),
          opt(seq(lit("."), oom(cls(vc{{0x30, 0x39}})))),
          opt(cls(vc{{0x62, 0x62},
                     {0x63, 0x63},
                     {0x64, 0x64},
                     {0x65, 0x65},
                     {0x45, 0x45},
                     {0x66, 0x66},
                     {0x46, 0x46},
                     {0x67, 0x67},
                     {0x47, 0x47},
                     {0x6e, 0x6e},
                     {0x6f, 0x6f},
                     {0x73, 0x73},
                     {0x78, 0x78},
                     {0x58, 0x58},
                     {0x25, 0x25}})));
  grammar["format_spec"].name = "format_spec";

  // 0x20 是空格字符，0x9 是制表符 (tab)。
  // ~EXTERNDENT <- <[ \t]*>
  ~grammar["EXTERNDENT"] <= tok(zom(cls(vc{{0x20, 0x20}, {0x9, 0x9}})));
  grammar["EXTERNDENT"].name = "EXTERNDENT";
  grammar["EXTERNDENT"].enable_memoize = false;

  // ~DEDENT <- <[ \t]*>
  ~grammar["DEDENT"] <= tok(zom(cls(vc{{0x20, 0x20}, {0x9, 0x9}})));
  grammar["DEDENT"].name = "DEDENT";
  grammar["DEDENT"].enable_memoize = false;

  // listcomp <- '[' _ named_expression SPACE for_if_clauses _ ']'
  // 例子：[num * 2 for num in range(10) if num % 2 == 0]
  grammar["listcomp"] <=
      seq(lit("["), ref(grammar, "_"), ref(grammar, "named_expression"),
          ref(grammar, "SPACE"), ref(grammar, "for_if_clauses"),
          ref(grammar, "_"), lit("]"));
  grammar["listcomp"].name = "listcomp";

  // extern_decorators <-
  //   decorators? ('@' _ <'llvm'/'python'> _ EOL SAMEDENT) decorators?
  grammar["extern_decorators"] <=
      seq(opt(ref(grammar, "decorators")),
          seq(lit("@"), ref(grammar, "_"), tok(cho(lit("llvm"), lit("python"))),
              ref(grammar, "_"), ref(grammar, "EOL"), ref(grammar, "SAMEDENT")),
          opt(ref(grammar, "decorators")));
  grammar["extern_decorators"].name = "extern_decorators";
  grammar["extern_decorators"].enable_memoize = false;

  // ~keyword <- <
  //   'False' / 'else' / 'import' / 'pass' / 'None' / 'break' / 'except' / 'in'
  //   / 'raise' / 'True' / 'class' / 'finally' / 'is' / 'return' / 'and' /
  //   'continue' / 'for' / 'as' / 'lambda' / 'try' / 'def' / 'from' / 'while'
  //   / 'assert' / 'del' / 'global' / 'not' / 'with' / 'elif' / 'if' / 'or' /
  //   'yield'
  // >
  ~grammar["keyword"] <=
      tok(cho(lit("False"), lit("else"), lit("import"), lit("pass"),
              lit("None"), lit("break"), lit("except"), lit("in"), lit("raise"),
              lit("True"), lit("class"), lit("finally"), lit("is"),
              lit("return"), lit("and"), lit("continue"), lit("for"), lit("as"),
              lit("lambda"), lit("try"), lit("def"), lit("from"), lit("while"),
              lit("assert"), lit("del"), lit("global"), lit("not"), lit("with"),
              lit("elif"), lit("if"), lit("or"), lit("yield")));
  grammar["keyword"].name = "keyword";

  // from_id <-
  //   / dot_name _ ':' _ expression
  //   / dot_name _ from_params (_ '->' _ expression)?
  //   / dot_name
  // 例子：from module import Class:Type
  //       from module import function(param1, param2):ReturnType
  //       from module import variable
  grammar["from_id"] <=
      cho(seq(ref(grammar, "dot_name"), ref(grammar, "_"), lit(":"),
              ref(grammar, "_"), ref(grammar, "expression")),
          seq(ref(grammar, "dot_name"), ref(grammar, "_"),
              ref(grammar, "from_params"),
              opt(seq(ref(grammar, "_"), lit("->"), ref(grammar, "_"),
                      ref(grammar, "expression")))),
          ref(grammar, "dot_name"));
  grammar["from_id"].name = "from_id";

  // base_class_args <- '(' _ tlist(',', expression)? _ ')'
  // 例子：(1, 2, 3)
  grammar["base_class_args"] <=
      seq(lit("("), ref(grammar, "_"),
          opt(ref(grammar, "tlist", "", true,
                  {lit(","), ref(grammar, "expression")})),
          ref(grammar, "_"), lit(")"));
  grammar["base_class_args"].name = "base_class_args";

  // kwarg_or_starred <-
  //   / NAME _ '=' _ expression
  //   / starred_expression
  grammar["kwarg_or_starred"] <=
      cho(seq(ref(grammar, "NAME"), ref(grammar, "_"), lit("="),
              ref(grammar, "_"), ref(grammar, "expression")),
          ref(grammar, "starred_expression"));
  grammar["kwarg_or_starred"].name = "kwarg_or_starred";

  // COMMENT <- <'#' (!EOL .)*>
  // 在 PEG 中，~ 用于标识词法规则，这些规则通常用于定义如何解析基本
  // 文本结构，例如空格、数字、标点符号等。
  ~grammar["COMMENT"] <=
      tok(seq(lit("#"), zom(seq(npd(ref(grammar, "EOL")), dot()))));
  grammar["COMMENT"].name = "COMMENT";

  // import_from <-
  //   / 'from' SPACE (_ <'.'>)* (_ dot_name)? SPACE 'import' SPACE '*'
  //   / 'from' SPACE (_ <'.'>)* (_ dot_name)? SPACE 'import' SPACE
  //     (from_as_parens / from_as_items)
  grammar["import_from"] <=
      cho(seq(lit("from"), ref(grammar, "SPACE"),
              zom(seq(ref(grammar, "_"), tok(lit(".")))),
              opt(seq(ref(grammar, "_"), ref(grammar, "dot_name"))),
              ref(grammar, "SPACE"), lit("import"), ref(grammar, "SPACE"),
              lit("*")),
          seq(lit("from"), ref(grammar, "SPACE"),
              zom(seq(ref(grammar, "_"), tok(lit(".")))),
              opt(seq(ref(grammar, "_"), ref(grammar, "dot_name"))),
              ref(grammar, "SPACE"), lit("import"), ref(grammar, "SPACE"),
              cho(ref(grammar, "from_as_parens"),
                  ref(grammar, "from_as_items"))));
  grammar["import_from"].name = "import_from";

  // param_name <- <'**' / '*'>? _ NAME
  grammar["param_name"] <= seq(opt(tok(cho(lit("**"), lit("*")))),
                               ref(grammar, "_"), ref(grammar, "NAME"));
  grammar["param_name"].name = "param_name";

  // star_target <-
  //   / '*' _ !'*' star_target
  //   / star_parens
  //   / primary
  grammar["star_target"] <= cho(seq(lit("*"), ref(grammar, "_"), npd(lit("*")),
                                    ref(grammar, "star_target")),
                                ref(grammar, "star_parens"),
                                ref(grammar, "primary"));
  grammar["star_target"].name = "star_target";

  // excepts <- (SAMEDENT except_block)+
  grammar["excepts"] <=
      oom(seq(ref(grammar, "SAMEDENT"), ref(grammar, "except_block")));
  grammar["excepts"].name = "excepts";
  grammar["excepts"].enable_memoize = false;

  // EXPFLOAT <- (PTFLOAT / DECINT) [eE] <'+' / '-'>? DECINT
  grammar["EXPFLOAT"] <=
      seq(cho(ref(grammar, "PTFLOAT"), ref(grammar, "DECINT")),
          cls(vc{{0x65, 0x65}, {0x45, 0x45}}),
          opt(tok(cho(lit("+"), lit("-")))), ref(grammar, "DECINT"));
  grammar["EXPFLOAT"].name = "EXPFLOAT";

  // statement <- SAMEDENT compound_stmt / SAMEDENT simple_stmt
  grammar["statement"] <=
      cho(seq(ref(grammar, "SAMEDENT"), ref(grammar, "compound_stmt")),
          seq(ref(grammar, "SAMEDENT"), ref(grammar, "simple_stmt")));
  grammar["statement"].name = "statement";
  grammar["statement"].enable_memoize = false;

  // with_parens_item <- '(' _ tlist(',', as_item) _ ')'
  grammar["with_parens_item"] <=
      seq(lit("("), ref(grammar, "_"),
          ref(grammar, "tlist", "", true, {lit(","), ref(grammar, "as_item")}),
          ref(grammar, "_"), lit(")"));
  grammar["with_parens_item"].name = "with_parens_item";

  // for_stmt <- ('for' SPACE star_targets)
  //             (SPACE 'in' SPACE star_expressions _ ':' _ suite)
  //             (SAMEDENT 'else' (SPACE 'not' SPACE 'break')* _ ':' _ suite)?
  grammar["for_stmt"] <=
      seq(seq(lit("for"), ref(grammar, "SPACE"), ref(grammar, "star_targets")),
          seq(ref(grammar, "SPACE"), lit("in"), ref(grammar, "SPACE"),
              ref(grammar, "star_expressions"), ref(grammar, "_"), lit(":"),
              ref(grammar, "_"), ref(grammar, "suite")),
          opt(seq(ref(grammar, "SAMEDENT"), lit("else"),
                  zom(seq(ref(grammar, "SPACE"), lit("not"),
                          ref(grammar, "SPACE"), lit("break"))),
                  ref(grammar, "_"), lit(":"), ref(grammar, "_"),
                  ref(grammar, "suite"))));
  grammar["for_stmt"].name = "for_stmt";
  grammar["for_stmt"].enable_memoize = false;

  // ~SPACE <- ([ \t]+ / COMMENT / NLP EOL) SPACE?
  ~grammar["SPACE"] <=
      seq(cho(oom(cls(vc{{0x20, 0x20}, {0x9, 0x9}})), ref(grammar, "COMMENT"),
              seq(ref(grammar, "NLP"), ref(grammar, "EOL"))),
          opt(ref(grammar, "SPACE")));
  grammar["SPACE"].name = "SPACE";

  // params <- '(' _ tlist(',', param)? _ ')'
  grammar["params"] <= seq(lit("("), ref(grammar, "_"),
                           opt(ref(grammar, "tlist", "", true,
                                   {lit(","), ref(grammar, "param")})),
                           ref(grammar, "_"), lit(")"));
  grammar["params"].name = "params";

  // expression <-
  //   / lambdef
  //   / disjunction SPACE 'if' SPACE disjunction SPACE 'else' SPACE expression
  //   / pipe
  grammar["expression"] <=
      cho(ref(grammar, "lambdef"),
          seq(ref(grammar, "disjunction"), ref(grammar, "SPACE"), lit("if"),
              ref(grammar, "SPACE"), ref(grammar, "disjunction"),
              ref(grammar, "SPACE"), lit("else"), ref(grammar, "SPACE"),
              ref(grammar, "expression")),
          ref(grammar, "pipe"));
  grammar["expression"].name = "expression";

  // kwargs <-
  //   / list(',', kwarg_or_starred) _ ',' _ list(',', kwarg_or_double_starred)
  //   / list(',', kwarg_or_starred)
  //   / list(',', kwarg_or_double_starred)
  grammar["kwargs"] <=
      cho(seq(ref(grammar, "list", "", true,
                  {lit(","), ref(grammar, "kwarg_or_starred")}),
              ref(grammar, "_"), lit(","), ref(grammar, "_"),
              ref(grammar, "list", "", true,
                  {lit(","), ref(grammar, "kwarg_or_double_starred")})),
          ref(grammar, "list", "", true,
              {lit(","), ref(grammar, "kwarg_or_starred")}),
          ref(grammar, "list", "", true,
              {lit(","), ref(grammar, "kwarg_or_double_starred")}));
  grammar["kwargs"].name = "kwargs";

  // ~empty_line <- [ \t]* EOL
  ~grammar["empty_line"] <=
      seq(zom(cls(vc{{0x20, 0x20}, {0x9, 0x9}})), ref(grammar, "EOL"));
  grammar["empty_line"].name = "empty_line";

  // suite <- (simple_stmt / (_ EOL)+ &INDENT statements (_ EOL)* &DEDENT)
  grammar["suite"] <=
      cho(ref(grammar, "simple_stmt"),
          seq(oom(seq(ref(grammar, "_"), ref(grammar, "EOL"))),
              apd(ref(grammar, "INDENT")), ref(grammar, "statements"),
              zom(seq(ref(grammar, "_"), ref(grammar, "EOL"))),
              apd(ref(grammar, "DEDENT"))));
  grammar["suite"].name = "suite";
  grammar["suite"].enable_memoize = false;

  // from_param <- expression
  grammar["from_param"] <= ref(grammar, "expression");
  grammar["from_param"].name = "from_param";

  // listexpr <- '[' _ tlist(',', star_named_expression)? _ ']'
  grammar["listexpr"] <=
      seq(lit("["), ref(grammar, "_"),
          opt(ref(grammar, "tlist", "", true,
                  {lit(","), ref(grammar, "star_named_expression")})),
          ref(grammar, "_"), lit("]"));
  grammar["listexpr"].name = "listexpr";

  // decorators <- decorator+
  grammar["decorators"] <= oom(ref(grammar, "decorator"));
  grammar["decorators"].name = "decorators";
  grammar["decorators"].enable_memoize = false;

  // decorator <- ('@' _ !(('llvm' / 'python') _ EOL) named_expression _ EOL
  // SAMEDENT)
  grammar["decorator"] <= seq(lit("@"), ref(grammar, "_"),
                              npd(seq(cho(lit("llvm"), lit("python")),
                                      ref(grammar, "_"), ref(grammar, "EOL"))),
                              ref(grammar, "named_expression"),
                              ref(grammar, "_"), ref(grammar, "EOL"),
                              ref(grammar, "SAMEDENT"));
  grammar["decorator"].name = "decorator";
  grammar["decorator"].enable_memoize = false;

  // small_stmt <-
  //   / assignment
  //   / 'pass' &(SPACE / ';' / EOL)
  //   / 'break' &(SPACE / ';' / EOL)
  //   / 'continue' &(SPACE / ';' / EOL)
  //   / global_stmt
  //   / nonlocal_stmt
  //   / yield_stmt &(SPACE / ';' / EOL)
  //   / assert_stmt
  //   / del_stmt
  //   / return_stmt &(SPACE / ';' / EOL)
  //   / raise_stmt &(SPACE / ';' / EOL)
  //   / print_stmt
  //   / import_stmt
  //   / expressions &(_ ';' / _ EOL)
  //   / custom_small_stmt
  grammar["small_stmt"] <=
      cho(ref(grammar, "assignment"),
          seq(lit("pass"),
              apd(cho(ref(grammar, "SPACE"), lit(";"), ref(grammar, "EOL")))),
          seq(lit("break"),
              apd(cho(ref(grammar, "SPACE"), lit(";"), ref(grammar, "EOL")))),
          seq(lit("continue"),
              apd(cho(ref(grammar, "SPACE"), lit(";"), ref(grammar, "EOL")))),
          ref(grammar, "global_stmt"), ref(grammar, "nonlocal_stmt"),
          seq(ref(grammar, "yield_stmt"),
              apd(cho(ref(grammar, "SPACE"), lit(";"), ref(grammar, "EOL")))),
          ref(grammar, "assert_stmt"), ref(grammar, "del_stmt"),
          seq(ref(grammar, "return_stmt"),
              apd(cho(ref(grammar, "SPACE"), lit(";"), ref(grammar, "EOL")))),
          seq(ref(grammar, "raise_stmt"),
              apd(cho(ref(grammar, "SPACE"), lit(";"), ref(grammar, "EOL")))),
          ref(grammar, "print_stmt"), ref(grammar, "import_stmt"),
          seq(ref(grammar, "expressions"),
              apd(cho(seq(ref(grammar, "_"), lit(";")),
                      seq(ref(grammar, "_"), ref(grammar, "EOL"))))),
          ref(grammar, "custom_small_stmt"));
  grammar["small_stmt"].name = "small_stmt";

  // PTFLOAT <- DECINT? '.' DECINT / DECINT '.'
  grammar["PTFLOAT"] <=
      cho(seq(opt(ref(grammar, "DECINT")), lit("."), ref(grammar, "DECINT")),
          seq(ref(grammar, "DECINT"), lit(".")));
  grammar["PTFLOAT"].name = "PTFLOAT";

  // STR <- <
  //   '"""' (!'"""' CHAR)*       '"""'   /  '\'\'\'' (!'\'\'\'' CHAR)* '\'\'\''
  //   /
  //   '"'   (!('"' / EOL) CHAR)* '"'     /  '\''     (!('\'' / EOL) CHAR)* '\''
  // >
  grammar["STR"] <=
      tok(cho(
          seq(lit("\"\"\""), zom(seq(npd(lit("\"\"\"")), ref(grammar, "CHAR"))),
              lit("\"\"\"")),
          seq(lit("'''"), zom(seq(npd(lit("'''")), ref(grammar, "CHAR"))),
              lit("'''")),
          seq(lit("\""),
              zom(seq(npd(cho(lit("\""), ref(grammar, "EOL"))),
                      ref(grammar, "CHAR"))),
              lit("\"")),
          seq(lit("'"),
              zom(seq(npd(cho(lit("'"), ref(grammar, "EOL"))),
                      ref(grammar, "CHAR"))),
              lit("'"))));
  grammar["STR"].name = "STR";

  // except_block <-
  //   / 'except' SPACE expression (SPACE 'as' SPACE NAME)? _ ':' _ suite
  //   / 'except' _ ':' _ suite
  grammar["except_block"] <=
      cho(seq(lit("except"), ref(grammar, "SPACE"), ref(grammar, "expression"),
              opt(seq(ref(grammar, "SPACE"), lit("as"), ref(grammar, "SPACE"),
                      ref(grammar, "NAME"))),
              ref(grammar, "_"), lit(":"), ref(grammar, "_"),
              ref(grammar, "suite")),
          seq(lit("except"), ref(grammar, "_"), lit(":"), ref(grammar, "_"),
              ref(grammar, "suite")));
  grammar["except_block"].name = "except_block";
  grammar["except_block"].enable_memoize = false;

  // dot_name <- id (_ '.' _ NAME)*
  grammar["dot_name"] <= seq(ref(grammar, "id"),
                             zom(seq(ref(grammar, "_"), lit("."),
                                     ref(grammar, "_"), ref(grammar, "NAME"))));
  grammar["dot_name"].name = "dot_name";

  // conjunction <-
  //   / inversion (SPACE 'and' SPACE inversion)+
  //   / inversion
  grammar["conjunction"] <=
      cho(seq(ref(grammar, "inversion"),
              oom(seq(ref(grammar, "SPACE"), lit("and"), ref(grammar, "SPACE"),
                      ref(grammar, "inversion")))),
          ref(grammar, "inversion"));
  grammar["conjunction"].name = "conjunction";

  // del_stmt <- 'del' SPACE tlist(',', expression)
  grammar["del_stmt"] <= seq(lit("del"), ref(grammar, "SPACE"),
                             ref(grammar, "tlist", "", true,
                                 {lit(","), ref(grammar, "expression")}));
  grammar["del_stmt"].name = "del_stmt";

  // from_params <- '(' _ tlist(',', from_param)? _ ')'
  grammar["from_params"] <=
      seq(lit("("), ref(grammar, "_"),
          opt(ref(grammar, "tlist", "", true,
                  {lit(","), ref(grammar, "from_param")})),
          ref(grammar, "_"), lit(")"));
  grammar["from_params"].name = "from_params";

  // ~EOL <- <[\r][\n] / [\r\n]>
  ~grammar["EOL"] <= tok(cho(seq(cls(vc{{0xd, 0xd}}), cls(vc{{0xa, 0xa}})),
                             cls(vc{{0xd, 0xd}, {0xa, 0xa}})));
  grammar["EOL"].name = "EOL";

  // match_stmt <- 'match' SPACE expression _ ':' (_ EOL)+
  //               &INDENT (SAMEDENT case)+ (_ EOL)* &DEDENT
  grammar["match_stmt"] <=
      seq(lit("match"), ref(grammar, "SPACE"), ref(grammar, "expression"),
          ref(grammar, "_"), lit(":"),
          oom(seq(ref(grammar, "_"), ref(grammar, "EOL"))),
          apd(ref(grammar, "INDENT")),
          oom(seq(ref(grammar, "SAMEDENT"), ref(grammar, "case"))),
          zom(seq(ref(grammar, "_"), ref(grammar, "EOL"))),
          apd(ref(grammar, "DEDENT")));
  grammar["match_stmt"].name = "match_stmt";
  grammar["match_stmt"].enable_memoize = false;

  // statements <- ((_ EOL)* statement)+
  grammar["statements"] <=
      oom(seq(zom(seq(ref(grammar, "_"), ref(grammar, "EOL"))),
              ref(grammar, "statement")));
  grammar["statements"].name = "statements";
  grammar["statements"].enable_memoize = false;

  // star_expression <-
  //   / '*' _ bitwise_or
  //   / expression
  grammar["star_expression"] <=
      cho(seq(lit("*"), ref(grammar, "_"), ref(grammar, "bitwise_or")),
          ref(grammar, "expression"));
  grammar["star_expression"].name = "star_expression";

  // tlist(c, e) <- e (_ c _ e)* (_ <c>)?
  // 这个宏能够匹配像列表一样的结构，其中元素由特定的分隔符分隔。
  // 例如，如果 e 代表数字，而 c 是逗号，那么 tlist(c, e) 可以匹配
  // 像 1, 2, 3 这样的数字列表。
  grammar["tlist"] <= seq(ref(grammar, "e"),
                          zom(seq(ref(grammar, "_"), ref(grammar, "c"),
                                  ref(grammar, "_"), ref(grammar, "e"))),
                          opt(seq(ref(grammar, "_"), tok(ref(grammar, "c")))));
  grammar["tlist"].name = "tlist";
  grammar["tlist"].is_macro = true;
  grammar["tlist"].params = {"c", "e"};

  // assert_stmt <- 'assert' SPACE expression (_ ',' _ expression)?
  grammar["assert_stmt"] <=
      seq(lit("assert"), ref(grammar, "SPACE"), ref(grammar, "expression"),
          opt(seq(ref(grammar, "_"), lit(","), ref(grammar, "_"),
                  ref(grammar, "expression"))));
  grammar["assert_stmt"].name = "assert_stmt";

  // expressions <- tlist(',', expression)
  grammar["expressions"] <=
      ref(grammar, "tlist", "", true, {lit(","), ref(grammar, "expression")});
  grammar["expressions"].name = "expressions";

  // ~SAMEDENT <- <[ \t]*>
  ~grammar["SAMEDENT"] <= tok(zom(cls(vc{{0x20, 0x20}, {0x9, 0x9}})));
  grammar["SAMEDENT"].name = "SAMEDENT";
  grammar["SAMEDENT"].enable_memoize = false;

  // list(c, e)  <- e (_ c _ e)*
  grammar["list"] <=
      seq(ref(grammar, "e"), zom(seq(ref(grammar, "_"), ref(grammar, "c"),
                                     ref(grammar, "_"), ref(grammar, "e"))));
  grammar["list"].name = "list";
  grammar["list"].is_macro = true;
  grammar["list"].params = {"c", "e"};

  // starred_expression <- '*' _ expression
  grammar["starred_expression"] <=
      seq(lit("*"), ref(grammar, "_"), ref(grammar, "expression"));
  grammar["starred_expression"].name = "starred_expression";

  // compare_op_bitwise_or <-
  //   / SPACE 'not' SPACE 'in' SPACE bitwise_or
  //   / SPACE 'is' SPACE 'not' SPACE bitwise_or
  //   / SPACE <'in' / 'is'> SPACE bitwise_or
  //   / _ <'==' / '!=' / '<=' / '<' / '>=' / '>'> _ bitwise_or
  grammar["compare_op_bitwise_or"] <=
      cho(seq(ref(grammar, "SPACE"), lit("not"), ref(grammar, "SPACE"),
              lit("in"), ref(grammar, "SPACE"), ref(grammar, "bitwise_or")),
          seq(ref(grammar, "SPACE"), lit("is"), ref(grammar, "SPACE"),
              lit("not"), ref(grammar, "SPACE"), ref(grammar, "bitwise_or")),
          seq(ref(grammar, "SPACE"), tok(cho(lit("in"), lit("is"))),
              ref(grammar, "SPACE"), ref(grammar, "bitwise_or")),
          seq(ref(grammar, "_"),
              tok(cho(lit("=="), lit("!="), lit("<="), lit("<"), lit(">="),
                      lit(">"))),
              ref(grammar, "_"), ref(grammar, "bitwise_or")));
  grammar["compare_op_bitwise_or"].name = "compare_op_bitwise_or";

  // factor <-
  //   / <'+' / '-' / '~'> _ factor
  grammar["factor"] <= cho(seq(tok(cho(lit("+"), lit("-"), lit("~"))),
                               ref(grammar, "_"), ref(grammar, "factor")),
                           ref(grammar, "power"));
  grammar["factor"].name = "factor";

  // fstring <- star_expressions _ (':' format_spec)? _ !.
  grammar["fstring"] <= seq(ref(grammar, "star_expressions"), ref(grammar, "_"),
                            opt(seq(lit(":"), ref(grammar, "format_spec"))),
                            ref(grammar, "_"), npd(dot()));
  grammar["fstring"].name = "fstring";

  // slice_part <- expression?
  grammar["slice_part"] <= opt(ref(grammar, "expression"));
  grammar["slice_part"].name = "slice_part";

  // atom <-
  //   / STRING (SPACE STRING)*
  //   / id
  //   / 'True'
  //   / 'False'
  //   / 'None'
  //   / INT _ '...' _ INT
  //   / FLOAT NAME?
  //   / INT NAME?
  //   / parentheses
  //   / '...'
  grammar["atom"] <=
      cho(seq(ref(grammar, "STRING"),
              zom(seq(ref(grammar, "SPACE"), ref(grammar, "STRING")))),
          ref(grammar, "id"), lit("True"), lit("False"), lit("None"),
          seq(ref(grammar, "INT"), ref(grammar, "_"), lit("..."),
              ref(grammar, "_"), ref(grammar, "INT")),
          seq(ref(grammar, "FLOAT"), opt(ref(grammar, "NAME"))),
          seq(ref(grammar, "INT"), opt(ref(grammar, "NAME"))),
          ref(grammar, "parentheses"), lit("..."));
  grammar["atom"].name = "atom";

  // class_def <- 'class' SPACE NAME _ class_args? _ ':' _ suite
  grammar["class_def"] <=
      seq(lit("class"), ref(grammar, "SPACE"), ref(grammar, "NAME"),
          ref(grammar, "_"), opt(ref(grammar, "class_args")), ref(grammar, "_"),
          lit(":"), ref(grammar, "_"), ref(grammar, "suite"));
  grammar["class_def"].name = "class_def";
  grammar["class_def"].enable_memoize = false;

  // generics <- '[' _ tlist(',', param) _ ']'
  grammar["generics"] <=
      seq(lit("["), ref(grammar, "_"),
          ref(grammar, "tlist", "", true, {lit(","), ref(grammar, "param")}),
          ref(grammar, "_"), lit("]"));
  grammar["generics"].name = "generics";

  // program <- (statements (_ EOL)* / (_ EOL)*) !.
  grammar["program"] <=
      seq(cho(seq(ref(grammar, "statements"),
                  zom(seq(ref(grammar, "_"), ref(grammar, "EOL")))),
              zom(seq(ref(grammar, "_"), ref(grammar, "EOL")))),
          npd(dot()));
  grammar["program"].name = "program";
  grammar["program"].enable_memoize = false;

  // dict <- '{' _ tlist(',', double_starred_kvpair)? _ '}'
  grammar["dict"] <=
      seq(lit("{"), ref(grammar, "_"),
          opt(ref(grammar, "tlist", "", true,
                  {lit(","), ref(grammar, "double_starred_kvpair")})),
          ref(grammar, "_"), lit("}"));
  grammar["dict"].name = "dict";

  // with_stmt <- 'with' SPACE (with_parens_item / with_item) _ ':' _ suite
  grammar["with_stmt"] <=
      seq(lit("with"), ref(grammar, "SPACE"),
          cho(ref(grammar, "with_parens_item"), ref(grammar, "with_item")),
          ref(grammar, "_"), lit(":"), ref(grammar, "_"),
          ref(grammar, "suite"));
  grammar["with_stmt"].name = "with_stmt";
  grammar["with_stmt"].enable_memoize = false;

  // assignment <-
  //   / id _ ':' _ expression (_ '=' _ star_expressions)?
  //   / (star_targets _ (!'==' '=') _)+ star_expressions !(_ '=')
  //   / star_expression _ augassign '=' ^ _ star_expressions
  grammar["assignment"] <=
      cho(seq(ref(grammar, "id"), ref(grammar, "_"), lit(":"),
              ref(grammar, "_"), ref(grammar, "expression"),
              opt(seq(ref(grammar, "_"), lit("="), ref(grammar, "_"),
                      ref(grammar, "star_expressions")))),
          seq(oom(seq(ref(grammar, "star_targets"), ref(grammar, "_"),
                      seq(npd(lit("==")), lit("=")), ref(grammar, "_"))),
              ref(grammar, "star_expressions"),
              npd(seq(ref(grammar, "_"), lit("=")))),
          seq(ref(grammar, "star_expression"), ref(grammar, "_"),
              ref(grammar, "augassign"), lit("="), cut(), ref(grammar, "_"),
              ref(grammar, "star_expressions")));
  grammar["assignment"].name = "assignment";

  // args <- (simple_args (_ ',' _ kwargs)? / kwargs)
  grammar["args"] <=
      cho(seq(ref(grammar, "simple_args"),
              opt(seq(ref(grammar, "_"), lit(","), ref(grammar, "_"),
                      ref(grammar, "kwargs")))),
          ref(grammar, "kwargs"));
  grammar["args"].name = "args";

  // kwarg_or_double_starred <-
  //  / NAME _ '=' _ expression
  //  / '**' _ expression
  grammar["kwarg_or_double_starred"] <=
      cho(seq(ref(grammar, "NAME"), ref(grammar, "_"), lit("="),
              ref(grammar, "_"), ref(grammar, "expression")),
          seq(lit("**"), ref(grammar, "_"), ref(grammar, "expression")));
  grammar["kwarg_or_double_starred"].name = "kwarg_or_double_starred";

  // peglib内置宏，需要参考代码。
  ~grammar["%recover"] <= ref(grammar, "x");
  grammar["%recover"].name = "%recover";
  grammar["%recover"].is_macro = true;
  grammar["%recover"].params = {"x"};

  // augassign <- <
  //   '+' / '-' / '**' / '*' / '@' / '//' / '/' / '%' / '&' / '|' / '^' / '<<'
  //   / '>>'
  // >
  grammar["augassign"] <=
      tok(cho(lit("+"), lit("-"), lit("**"), lit("*"), lit("@"), lit("//"),
              lit("/"), lit("%"), lit("&"), lit("|"), lit("^"), lit("<<"),
              lit(">>")));
  grammar["augassign"].name = "augassign";

  // term <- factor (_ <'*' / '//' / '/' / '%' / '@'> _ factor)*
  grammar["term"] <=
      seq(ref(grammar, "factor"),
          zom(seq(ref(grammar, "_"),
                  tok(cho(lit("*"), lit("//"), lit("/"), lit("%"), lit("@"))),
                  ref(grammar, "_"), ref(grammar, "factor"))));
  grammar["term"].name = "term";

  // star_targets <- tlist(',', star_target)
  grammar["star_targets"] <=
      ref(grammar, "tlist", "", true, {lit(","), ref(grammar, "star_target")});
  grammar["star_targets"].name = "star_targets";

  // return_stmt <- 'return' (SPACE expressions)?
  grammar["return_stmt"] <=
      seq(lit("return"),
          opt(seq(ref(grammar, "SPACE"), ref(grammar, "expressions"))));
  grammar["return_stmt"].name = "return_stmt";

  // print_stmt <-
  //   / 'print' SPACE star_expression (_ ',' _ star_expression)* (_ <','>)?
  //   / 'print' _ &EOL
  grammar["print_stmt"] <=
      cho(seq(lit("print"), ref(grammar, "SPACE"),
              ref(grammar, "star_expression"),
              zom(seq(ref(grammar, "_"), lit(","), ref(grammar, "_"),
                      ref(grammar, "star_expression"))),
              opt(seq(ref(grammar, "_"), tok(lit(","))))),
          seq(lit("print"), ref(grammar, "_"), apd(ref(grammar, "EOL"))));
  grammar["print_stmt"].name = "print_stmt";

  // sum <- term (_ <'+' / '-'>   _ term)*
  grammar["sum"] <= seq(ref(grammar, "term"),
                        zom(seq(ref(grammar, "_"), tok(cho(lit("+"), lit("-"))),
                                ref(grammar, "_"), ref(grammar, "term"))));
  grammar["sum"].name = "sum";

  // kvpair <- expression _ ':' _ expression
  grammar["kvpair"] <= seq(ref(grammar, "expression"), ref(grammar, "_"),
                           lit(":"), ref(grammar, "_"),
                           ref(grammar, "expression"));
  grammar["kvpair"].name = "kvpair";

  // function_def <-
  //   / 'def' SPACE NAME _ generics _ params (_ '->' _ expression)? _ ':'
  //   / 'def' SPACE NAME _ params (_ '->' _ expression)? _ ':'
  grammar["function_def"] <=
      cho(seq(lit("def"), ref(grammar, "SPACE"), ref(grammar, "NAME"),
              ref(grammar, "_"), ref(grammar, "generics"), ref(grammar, "_"),
              ref(grammar, "params"),
              opt(seq(ref(grammar, "_"), lit("->"), ref(grammar, "_"),
                      ref(grammar, "expression"))),
              ref(grammar, "_"), lit(":")),
          seq(lit("def"), ref(grammar, "SPACE"), ref(grammar, "NAME"),
              ref(grammar, "_"), ref(grammar, "params"),
              opt(seq(ref(grammar, "_"), lit("->"), ref(grammar, "_"),
                      ref(grammar, "expression"))),
              ref(grammar, "_"), lit(":")));
  grammar["function_def"].name = "function_def";

  // for <- decorator? for_stmt
  grammar["for"] <=
      seq(opt(ref(grammar, "decorator")), ref(grammar, "for_stmt"));
  grammar["for"].name = "for";
  grammar["for"].enable_memoize = false;

  // extern <- (empty_line* EXTERNDENT (!EOL .)* EOL empty_line*)+
  grammar["extern"] <=
      oom(seq(zom(ref(grammar, "empty_line")), ref(grammar, "EXTERNDENT"),
              zom(seq(npd(ref(grammar, "EOL")), dot())), ref(grammar, "EOL"),
              zom(ref(grammar, "empty_line"))));
  grammar["extern"].name = "extern";
  grammar["extern"].enable_memoize = false;

  // class <- decorators? class_def
  grammar["class"] <=
      seq(opt(ref(grammar, "decorators")), ref(grammar, "class_def"));
  grammar["class"].name = "class";
  grammar["class"].enable_memoize = false;

  // yield_stmt <-
  //   / 'yield' SPACE 'from' SPACE expression
  //   / 'yield' (SPACE expressions)?
  grammar["yield_stmt"] <=
      cho(seq(lit("yield"), ref(grammar, "SPACE"), lit("from"),
              ref(grammar, "SPACE"), ref(grammar, "expression")),
          seq(lit("yield"),
              opt(seq(ref(grammar, "SPACE"), ref(grammar, "expressions")))));
  grammar["yield_stmt"].name = "yield_stmt";

  // try_stmt <-
  //   / ('try' _ ':' _ suite)
  //     excepts
  //     (SAMEDENT 'finally' _ ':' _ suite)?
  //   / ('try' _ ':' _ suite) (SAMEDENT 'finally' _ ':' _ suite)?
  grammar["try_stmt"] <=
      cho(seq(seq(lit("try"), ref(grammar, "_"), lit(":"), ref(grammar, "_"),
                  ref(grammar, "suite")),
              ref(grammar, "excepts"),
              opt(seq(ref(grammar, "SAMEDENT"), lit("finally"),
                      ref(grammar, "_"), lit(":"), ref(grammar, "_"),
                      ref(grammar, "suite")))),
          seq(seq(lit("try"), ref(grammar, "_"), lit(":"), ref(grammar, "_"),
                  ref(grammar, "suite")),
              opt(seq(ref(grammar, "SAMEDENT"), lit("finally"),
                      ref(grammar, "_"), lit(":"), ref(grammar, "_"),
                      ref(grammar, "suite")))));
  grammar["try_stmt"].name = "try_stmt";
  grammar["try_stmt"].enable_memoize = false;

  // slices <- '[' _ tlist(',', slice) _ ']'
  grammar["slices"] <=
      seq(lit("["), ref(grammar, "_"),
          ref(grammar, "tlist", "", true, {lit(","), ref(grammar, "slice")}),
          ref(grammar, "_"), lit("]"));
  grammar["slices"].name = "slices";

  // as_item <-
  //   / expression SPACE 'as' SPACE id &(_ (',' / ')' / ':'))
  //   / expression
  grammar["as_item"] <=
      cho(seq(ref(grammar, "expression"), ref(grammar, "SPACE"), lit("as"),
              ref(grammar, "SPACE"), ref(grammar, "id"),
              apd(seq(ref(grammar, "_"), cho(lit(","), lit(")"), lit(":"))))),
          ref(grammar, "expression"));
  grammar["as_item"].name = "as_item";

  // NAME <-
  //   / keyword [a-zA-Z_0-9]+
  //   / !keyword <[a-zA-Z_] [a-zA-Z_0-9]*>
  grammar["NAME"] <=
      cho(seq(ref(grammar, "keyword"),
              oom(cls(
                  vc{{0x61, 0x7a}, {0x41, 0x5a}, {0x5f, 0x5f}, {0x30, 0x39}}))),
          seq(npd(ref(grammar, "keyword")),
              tok(seq(cls(vc{{0x61, 0x7a}, {0x41, 0x5a}, {0x5f, 0x5f}}),
                      zom(cls(vc{{0x61, 0x7a},
                                 {0x41, 0x5a},
                                 {0x5f, 0x5f},
                                 {0x30, 0x39}}))))));
  grammar["NAME"].name = "NAME";

  // CHAR <- ('\\' . / .)
  grammar["CHAR"] <= cho(seq(lit("\\"), dot()), dot());
  grammar["CHAR"].name = "CHAR";

  // global_stmt <- 'global' SPACE tlist(',', NAME)
  grammar["global_stmt"] <=
      seq(lit("global"), ref(grammar, "SPACE"),
          ref(grammar, "tlist", "", true, {lit(","), ref(grammar, "NAME")}));
  grammar["global_stmt"].name = "global_stmt";

  // from_as_items <- list(',', from_as)
  grammar["from_as_items"] <=
      ref(grammar, "list", "", true, {lit(","), ref(grammar, "from_as")});
  grammar["from_as_items"].name = "from_as_items";

  // from_as_parens <- '(' _ tlist(',', from_as) _ ')'
  grammar["from_as_parens"] <=
      seq(lit("("), ref(grammar, "_"),
          ref(grammar, "tlist", "", true, {lit(","), ref(grammar, "from_as")}),
          ref(grammar, "_"), lit(")"));
  grammar["from_as_parens"].name = "from_as_parens";

  // INT <- (BININT / HEXINT / DECINT)
  grammar["INT"] <= cho(ref(grammar, "BININT"), ref(grammar, "HEXINT"),
                        ref(grammar, "DECINT"));
  grammar["INT"].name = "INT";

  // from_as <- from_id (SPACE 'as' SPACE NAME)?
  grammar["from_as"] <=
      seq(ref(grammar, "from_id"),
          opt(seq(ref(grammar, "SPACE"), lit("as"), ref(grammar, "SPACE"),
                  ref(grammar, "NAME"))));
  grammar["from_as"].name = "from_as";

  // import_name <- 'import' SPACE list(',', as_name)
  grammar["import_name"] <=
      seq(lit("import"), ref(grammar, "SPACE"),
          ref(grammar, "list", "", true, {lit(","), ref(grammar, "as_name")}));
  grammar["import_name"].name = "import_name";

  // if_stmt <- ('if' SPACE named_expression _ ':' _ suite)
  //            (SAMEDENT 'elif' SPACE named_expression _ ':' _ suite)*
  //            (SAMEDENT 'else' _ ':' _ suite)?
  grammar["if_stmt"] <=
      seq(seq(lit("if"), ref(grammar, "SPACE"),
              ref(grammar, "named_expression"), ref(grammar, "_"), lit(":"),
              ref(grammar, "_"), ref(grammar, "suite")),
          zom(seq(ref(grammar, "SAMEDENT"), lit("elif"), ref(grammar, "SPACE"),
                  ref(grammar, "named_expression"), ref(grammar, "_"), lit(":"),
                  ref(grammar, "_"), ref(grammar, "suite"))),
          opt(seq(ref(grammar, "SAMEDENT"), lit("else"), ref(grammar, "_"),
                  lit(":"), ref(grammar, "_"), ref(grammar, "suite"))));
  grammar["if_stmt"].name = "if_stmt";
  grammar["if_stmt"].enable_memoize = false;

  // function <-
  //   / extern_decorators function_def (_ EOL)+ &INDENT extern (_ EOL)* &DEDENT
  //   / decorators? function_def _ suite
  grammar["function"] <=
      cho(seq(ref(grammar, "extern_decorators"), ref(grammar, "function_def"),
              oom(seq(ref(grammar, "_"), ref(grammar, "EOL"))),
              apd(ref(grammar, "INDENT")), ref(grammar, "extern"),
              zom(seq(ref(grammar, "_"), ref(grammar, "EOL"))),
              apd(ref(grammar, "DEDENT"))),
          seq(opt(ref(grammar, "decorators")), ref(grammar, "function_def"),
              ref(grammar, "_"), ref(grammar, "suite")));
  grammar["function"].name = "function";
  grammar["function"].enable_memoize = false;

  // case <-
  //   / 'case' SPACE expression SPACE 'if' SPACE pipe _ ':' _ suite
  //   / 'case' SPACE expression _ ':' _ suite
  grammar["case"] <=
      cho(seq(lit("case"), ref(grammar, "SPACE"), ref(grammar, "expression"),
              ref(grammar, "SPACE"), lit("if"), ref(grammar, "SPACE"),
              ref(grammar, "pipe"), ref(grammar, "_"), lit(":"),
              ref(grammar, "_"), ref(grammar, "suite")),
          seq(lit("case"), ref(grammar, "SPACE"), ref(grammar, "expression"),
              ref(grammar, "_"), lit(":"), ref(grammar, "_"),
              ref(grammar, "suite")));
  grammar["case"].name = "case";
  grammar["case"].enable_memoize = false;

  // custom_stmt <-
  //   / NAME SPACE expression _ ':' _ suite
  //   / NAME _ ':' _ suite
  grammar["custom_stmt"] <=
      cho(seq(ref(grammar, "NAME"), ref(grammar, "SPACE"),
              ref(grammar, "expression"), ref(grammar, "_"), lit(":"),
              ref(grammar, "_"), ref(grammar, "suite")),
          seq(ref(grammar, "NAME"), ref(grammar, "_"), lit(":"),
              ref(grammar, "_"), ref(grammar, "suite")));
  grammar["custom_stmt"].name = "custom_stmt";
  grammar["custom_stmt"].enable_memoize = false;

  // genexp <- '(' _ named_expression SPACE for_if_clauses _ ')'
  grammar["genexp"] <=
      seq(lit("("), ref(grammar, "_"), ref(grammar, "named_expression"),
          ref(grammar, "SPACE"), ref(grammar, "for_if_clauses"),
          ref(grammar, "_"), lit(")"));
  grammar["genexp"].name = "genexp";

  // custom_small_stmt <- NAME SPACE expressions
  grammar["custom_small_stmt"] <= seq(ref(grammar, "NAME"),
                                      ref(grammar, "SPACE"),
                                      ref(grammar, "expressions"));
  grammar["custom_small_stmt"].name = "custom_small_stmt";

  // pipe <-
  //   / disjunction (_ <'|>' / '||>'> _ disjunction)+
  //   / disjunction
  grammar["pipe"] <=
      cho(seq(ref(grammar, "disjunction"),
              oom(seq(ref(grammar, "_"), tok(cho(lit("|>"), lit("||>"))),
                      ref(grammar, "_"), ref(grammar, "disjunction")))),
          ref(grammar, "disjunction"));
  grammar["pipe"].name = "pipe";

  // lambdef <-
  //   / 'lambda' SPACE list(',', NAME) _ ':' _ expression
  //   / 'lambda' _ ':' _ expression
  grammar["lambdef"] <=
      cho(seq(lit("lambda"), ref(grammar, "SPACE"),
              ref(grammar, "list", "", true, {lit(","), ref(grammar, "NAME")}),
              ref(grammar, "_"), lit(":"), ref(grammar, "_"),
              ref(grammar, "expression")),
          seq(lit("lambda"), ref(grammar, "_"), lit(":"), ref(grammar, "_"),
              ref(grammar, "expression")));
  grammar["lambdef"].name = "lambdef";

  // disjunction <-
  //   / conjunction (SPACE 'or' SPACE conjunction)+
  //   / conjunction
  grammar["disjunction"] <=
      cho(seq(ref(grammar, "conjunction"),
              oom(seq(ref(grammar, "SPACE"), lit("or"), ref(grammar, "SPACE"),
                      ref(grammar, "conjunction")))),
          ref(grammar, "conjunction"));
  grammar["disjunction"].name = "disjunction";

  // inversion <-
  //   / 'not' SPACE inversion
  //   / comparison
  grammar["inversion"] <=
      cho(seq(lit("not"), ref(grammar, "SPACE"), ref(grammar, "inversion")),
          ref(grammar, "comparison"));
  grammar["inversion"].name = "inversion";

  // comparison <- bitwise_or compare_op_bitwise_or*
  grammar["comparison"] <= seq(ref(grammar, "bitwise_or"),
                               zom(ref(grammar, "compare_op_bitwise_or")));
  grammar["comparison"].name = "comparison";

  // bitwise_or  <- bitwise_xor (_ <'|'> _ bitwise_xor)*
  grammar["bitwise_or"] <=
      seq(ref(grammar, "bitwise_xor"),
          zom(seq(ref(grammar, "_"), tok(lit("|")), ref(grammar, "_"),
                  ref(grammar, "bitwise_xor"))));
  grammar["bitwise_or"].name = "bitwise_or";

  // with_item <- list(',', as_item)
  grammar["with_item"] <=
      ref(grammar, "list", "", true, {lit(","), ref(grammar, "as_item")});
  grammar["with_item"].name = "with_item";

  // bitwise_xor <- bitwise_and (_ <'^'> _ bitwise_and)*
  grammar["bitwise_xor"] <=
      seq(ref(grammar, "bitwise_and"),
          zom(seq(ref(grammar, "_"), tok(lit("^")), ref(grammar, "_"),
                  ref(grammar, "bitwise_and"))));
  grammar["bitwise_xor"].name = "bitwise_xor";

  // param <-
  //   / param_name _ ':' _ expression (_ '=' _ expression)?
  //   / param_name (_ '=' _ expression)?
  grammar["param"] <=
      cho(seq(ref(grammar, "param_name"), ref(grammar, "_"), lit(":"),
              ref(grammar, "_"), ref(grammar, "expression"),
              opt(seq(ref(grammar, "_"), lit("="), ref(grammar, "_"),
                      ref(grammar, "expression")))),
          seq(ref(grammar, "param_name"),
              opt(seq(ref(grammar, "_"), lit("="), ref(grammar, "_"),
                      ref(grammar, "expression")))));
  grammar["param"].name = "param";

  // FLOAT <- (EXPFLOAT / PTFLOAT)
  grammar["FLOAT"] <= cho(ref(grammar, "EXPFLOAT"), ref(grammar, "PTFLOAT"));
  grammar["FLOAT"].name = "FLOAT";

  // shift_expr <- sum  (_ <'<<' / '>>'> _ sum )*
  grammar["shift_expr"] <=
      seq(ref(grammar, "sum"),
          zom(seq(ref(grammar, "_"), tok(cho(lit("<<"), lit(">>"))),
                  ref(grammar, "_"), ref(grammar, "sum"))));
  grammar["shift_expr"].name = "shift_expr";

  // power <-
  //   / primary _ <'**'> _ factor
  //   / primary
  grammar["power"] <=
      cho(seq(ref(grammar, "primary"), ref(grammar, "_"), tok(lit("**")),
              ref(grammar, "_"), ref(grammar, "factor")),
          ref(grammar, "primary"));
  grammar["power"].name = "power";

  // as_name <- dot_name (SPACE 'as' SPACE NAME)?
  grammar["as_name"] <=
      seq(ref(grammar, "dot_name"),
          opt(seq(ref(grammar, "SPACE"), lit("as"), ref(grammar, "SPACE"),
                  ref(grammar, "NAME"))));
  grammar["as_name"].name = "as_name";

  // primary <- atom (_ primary_tail)*
  grammar["primary"] <=
      seq(ref(grammar, "atom"),
          zom(seq(ref(grammar, "_"), ref(grammar, "primary_tail"))));
  grammar["primary"].name = "primary";

  // primary_tail <-
  //   / '.' _ NAME
  //   / genexp
  //   / arguments
  //   / slices
  grammar["primary_tail"] <=
      cho(seq(lit("."), ref(grammar, "_"), ref(grammar, "NAME")),
          ref(grammar, "genexp"), ref(grammar, "arguments"),
          ref(grammar, "slices"));
  grammar["primary_tail"].name = "primary_tail";

  // slice <-
  //   / slice_part _ ':' _ slice_part (_ ':' _ slice_part)?
  grammar["slice"] <=
      cho(seq(ref(grammar, "slice_part"), ref(grammar, "_"), lit(":"),
              ref(grammar, "_"), ref(grammar, "slice_part"),
              opt(seq(ref(grammar, "_"), lit(":"), ref(grammar, "_"),
                      ref(grammar, "slice_part")))),
          ref(grammar, "expression"));
  grammar["slice"].name = "slice";

  // class_args <-
  //   / generics _ base_class_args
  //   / generics
  //   / base_class_args
  grammar["class_args"] <= cho(seq(ref(grammar, "generics"), ref(grammar, "_"),
                                   ref(grammar, "base_class_args")),
                               ref(grammar, "generics"),
                               ref(grammar, "base_class_args"));
  grammar["class_args"].name = "class_args";

  // setcomp <- '{' _ named_expression SPACE for_if_clauses _ '}'
  grammar["setcomp"] <=
      seq(lit("{"), ref(grammar, "_"), ref(grammar, "named_expression"),
          ref(grammar, "SPACE"), ref(grammar, "for_if_clauses"),
          ref(grammar, "_"), lit("}"));
  grammar["setcomp"].name = "setcomp";

  // BININT <- <'0' [bB] [0-1] ('_'* [0-1])*>
  grammar["BININT"] <= tok(seq(lit("0"), cls(vc{{0x62, 0x62}, {0x42, 0x42}}),
                               cls(vc{{0x30, 0x31}}),
                               zom(seq(zom(lit("_")), cls(vc{{0x30, 0x31}})))));
  grammar["BININT"].name = "BININT";

  // parentheses <- (
  //   tuple / yield / named / genexp / listexpr / listcomp / dict / set /
  //   dictcomp / setcomp
  // )
  grammar["parentheses"] <=
      cho(ref(grammar, "tuple"), ref(grammar, "yield"), ref(grammar, "named"),
          ref(grammar, "genexp"), ref(grammar, "listexpr"),
          ref(grammar, "listcomp"), ref(grammar, "dict"), ref(grammar, "set"),
          ref(grammar, "dictcomp"), ref(grammar, "setcomp"));
  grammar["parentheses"].name = "parentheses";

  // tuple <-
  //   / '(' _ ')'
  //   / '(' _ tlist(',', star_named_expression) _ ')'
  grammar["tuple"] <=
      cho(seq(lit("("), ref(grammar, "_"), lit(")")),
          seq(lit("("), ref(grammar, "_"),
              ref(grammar, "tlist", "", true,
                  {lit(","), ref(grammar, "star_named_expression")}),
              ref(grammar, "_"), lit(")")));
  grammar["tuple"].name = "tuple";

  // ~INDENT <- <[ \t]*>
  ~grammar["INDENT"] <= tok(zom(cls(vc{{0x20, 0x20}, {0x9, 0x9}})));
  grammar["INDENT"].name = "INDENT";
  grammar["INDENT"].enable_memoize = false;

  // import_stmt <- import_name / import_from
  grammar["import_stmt"] <=
      cho(ref(grammar, "import_name"), ref(grammar, "import_from"));
  grammar["import_stmt"].name = "import_stmt";

  // double_starred_kvpair <-
  //   / '**' _ bitwise_or
  //   / kvpair
  grammar["double_starred_kvpair"] <=
      cho(seq(lit("**"), ref(grammar, "_"), ref(grammar, "bitwise_or")),
          ref(grammar, "kvpair"));
  grammar["double_starred_kvpair"].name = "double_starred_kvpair";

  // raise_stmt <- 'raise' (SPACE expression)?
  grammar["raise_stmt"] <=
      seq(lit("raise"),
          opt(seq(ref(grammar, "SPACE"), ref(grammar, "expression"))));
  grammar["raise_stmt"].name = "raise_stmt";

  // yield <- '(' _ 'yield' _ ')'
  grammar["yield"] <= seq(lit("("), ref(grammar, "_"), lit("yield"),
                          ref(grammar, "_"), lit(")"));
  grammar["yield"].name = "yield";

  // simple_stmt <- tlist(';', small_stmt) _ EOL
  grammar["simple_stmt"] <= seq(ref(grammar, "tlist", "", true,
                                    {lit(";"), ref(grammar, "small_stmt")}),
                                ref(grammar, "_"), ref(grammar, "EOL"));
  grammar["simple_stmt"].name = "simple_stmt";

  // set <- '{' _ tlist(',', star_named_expression) _ '}'
  grammar["set"] <= seq(lit("{"), ref(grammar, "_"),
                        ref(grammar, "tlist", "", true,
                            {lit(","), ref(grammar, "star_named_expression")}),
                        ref(grammar, "_"), lit("}"));
  grammar["set"].name = "set";

  // while_stmt <- ('while' SPACE named_expression _ ':' _ suite)
  //               (SAMEDENT 'else' (SPACE 'not' SPACE 'break')*  _ ':' _
  //               suite)?
  grammar["while_stmt"] <=
      seq(seq(lit("while"), ref(grammar, "SPACE"),
              ref(grammar, "named_expression"), ref(grammar, "_"), lit(":"),
              ref(grammar, "_"), ref(grammar, "suite")),
          opt(seq(ref(grammar, "SAMEDENT"), lit("else"),
                  zom(seq(ref(grammar, "SPACE"), lit("not"),
                          ref(grammar, "SPACE"), lit("break"))),
                  ref(grammar, "_"), lit(":"), ref(grammar, "_"),
                  ref(grammar, "suite"))));
  grammar["while_stmt"].name = "while_stmt";
  grammar["while_stmt"].enable_memoize = false;

  // dictcomp <- '{' _ kvpair SPACE for_if_clauses _ '}'
  grammar["dictcomp"] <= seq(lit("{"), ref(grammar, "_"),
                             ref(grammar, "kvpair"), ref(grammar, "SPACE"),
                             ref(grammar, "for_if_clauses"), ref(grammar, "_"),
                             lit("}"));
  grammar["dictcomp"].name = "dictcomp";

  // for_if_clauses <- for_if_clause (SPACE for_if_clause)*
  grammar["for_if_clauses"] <=
      seq(ref(grammar, "for_if_clause"),
          zom(seq(ref(grammar, "SPACE"), ref(grammar, "for_if_clause"))));
  grammar["for_if_clauses"].name = "for_if_clauses";

  // HEXINT <- <'0' [xX] [0-9a-fA-F] ('_'? [0-9a-fA-F])*>
  grammar["HEXINT"] <=
      tok(seq(lit("0"), cls(vc{{0x78, 0x78}, {0x58, 0x58}}),
              cls(vc{{0x30, 0x39}, {0x61, 0x66}, {0x41, 0x46}}),
              zom(seq(opt(lit("_")),
                      cls(vc{{0x30, 0x39}, {0x61, 0x66}, {0x41, 0x46}})))));
  grammar["HEXINT"].name = "HEXINT";

  // for_if_clause <- 'for' SPACE star_targets SPACE 'in' SPACE disjunction
  //                  (SPACE 'if' SPACE disjunction)*
  grammar["for_if_clause"] <=
      seq(lit("for"), ref(grammar, "SPACE"), ref(grammar, "star_targets"),
          ref(grammar, "SPACE"), lit("in"), ref(grammar, "SPACE"),
          ref(grammar, "disjunction"),
          zom(seq(ref(grammar, "SPACE"), lit("if"), ref(grammar, "SPACE"),
                  ref(grammar, "disjunction"))));
  grammar["for_if_clause"].name = "for_if_clause";

  // star_parens <-
  //   / '(' _ tlist(',', star_target) _ ')'
  //   / '[' _ tlist(',', star_target) _ ']'
  grammar["star_parens"] <=
      cho(seq(lit("("), ref(grammar, "_"),
              ref(grammar, "tlist", "", true,
                  {lit(","), ref(grammar, "star_target")}),
              ref(grammar, "_"), lit(")")),
          seq(lit("["), ref(grammar, "_"),
              ref(grammar, "tlist", "", true,
                  {lit(","), ref(grammar, "star_target")}),
              ref(grammar, "_"), lit("]")));
  grammar["star_parens"].name = "star_parens";

  // compound_stmt <-
  //   / function
  //   / if_stmt
  //   / class
  //   / with_stmt
  //   / for
  //   / try_stmt
  //   / while_stmt
  //   / match_stmt
  //   / custom_stmt
  grammar["compound_stmt"] <=
      cho(ref(grammar, "function"), ref(grammar, "if_stmt"),
          ref(grammar, "class"), ref(grammar, "with_stmt"), ref(grammar, "for"),
          ref(grammar, "try_stmt"), ref(grammar, "while_stmt"),
          ref(grammar, "match_stmt"), ref(grammar, "custom_stmt"));
  grammar["compound_stmt"].name = "compound_stmt";
  grammar["compound_stmt"].enable_memoize = false;

  // star_expressions <- tlist(',', star_expression)
  grammar["star_expressions"] <=
      ref(grammar, "tlist", "", true,
          {lit(","), ref(grammar, "star_expression")});
  grammar["star_expressions"].name = "star_expressions";

  // star_named_expression <-
  //   / '*' _ bitwise_or
  //   / named_expression
  grammar["star_named_expression"] <=
      cho(seq(lit("*"), ref(grammar, "_"), ref(grammar, "bitwise_or")),
          ref(grammar, "named_expression"));
  grammar["star_named_expression"].name = "star_named_expression";

  // named_expression <-
  //   / NAME _ ':=' _ ^ expression
  //   / expression !(_ ':=')
  grammar["named_expression"] <=
      cho(seq(ref(grammar, "NAME"), ref(grammar, "_"), lit(":="),
              ref(grammar, "_"), cut(), ref(grammar, "expression")),
          seq(ref(grammar, "expression"),
              npd(seq(ref(grammar, "_"), lit(":=")))));
  grammar["named_expression"].name = "named_expression";

  // nonlocal_stmt <- 'nonlocal' SPACE tlist(',', NAME)
  grammar["nonlocal_stmt"] <=
      seq(lit("nonlocal"), ref(grammar, "SPACE"),
          ref(grammar, "tlist", "", true, {lit(","), ref(grammar, "NAME")}));
  grammar["nonlocal_stmt"].name = "nonlocal_stmt";

  // bitwise_and <- shift_expr  (_ <'&'> _ shift_expr )*
  grammar["bitwise_and"] <=
      seq(ref(grammar, "shift_expr"),
          zom(seq(ref(grammar, "_"), tok(lit("&")), ref(grammar, "_"),
                  ref(grammar, "shift_expr"))));
  grammar["bitwise_and"].name = "bitwise_and";

  // arguments <- '(' _ tlist(',', args)? _ ')'
  grammar["arguments"] <= seq(lit("("), ref(grammar, "_"),
                              opt(ref(grammar, "tlist", "", true,
                                      {lit(","), ref(grammar, "args")})),
                              ref(grammar, "_"), lit(")"));
  grammar["arguments"].name = "arguments";

  // simple_args <- list(',', (starred_expression / named_expression !(_ '=')))
  grammar["simple_args"] <=
      ref(grammar, "list", "", true,
          {lit(","), cho(ref(grammar, "starred_expression"),
                         seq(ref(grammar, "named_expression"),
                             npd(seq(ref(grammar, "_"), lit("=")))))});
  grammar["simple_args"].name = "simple_args";

  // id <- NAME
  grammar["id"] <= ref(grammar, "NAME");
  grammar["id"].name = "id";

  // ~_ <- SPACE?
  ~grammar["_"] <= opt(ref(grammar, "SPACE"));
  grammar["_"].name = "_";

  // DECINT <- <[0-9] ('_'? [0-9])*>
  grammar["DECINT"] <= tok(seq(cls(vc{{0x30, 0x39}}),
                               zom(seq(opt(lit("_")), cls(vc{{0x30, 0x39}})))));
  grammar["DECINT"].name = "DECINT";

  // named <- '(' _ named_expression _ ')'
  grammar["named"] <= seq(lit("("), ref(grammar, "_"),
                          ref(grammar, "named_expression"), ref(grammar, "_"),
                          lit(")"));
  grammar["named"].name = "named";

  // STRING <- <NAME? STR>
  grammar["STRING"] <= tok(seq(opt(ref(grammar, "NAME")), ref(grammar, "STR")));
  grammar["STRING"].name = "STRING";
}

auto fn_format_spec(peg::SemanticValues& VS, any& DT) {
  return string(VS.sv());
};

auto fn_EXTERNDENT(peg::SemanticValues& VS, any& DT){

};

auto pred_EXTERNDENT(const peg::SemanticValues& VS, const any& DT,
                     std::string& MSG) {
  const auto& CTX = any_cast<const ParseContext&>(DT);

  return !(!CTX.indent.size() && VS.sv().size()) &&
         !(CTX.indent.size() && VS.sv().size() < CTX.indent.top());
};

auto fn_DEDENT(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  CTX.indent.pop();
};

auto pred_DEDENT(const peg::SemanticValues& VS, const any& DT,
                 std::string& MSG) {
  const auto& CTX = any_cast<const ParseContext&>(DT);

  if (!(CTX.indent.size() && VS.sv().size() < CTX.indent.top())) {
    MSG = "unexpected dedent";
    return false;
  }
  return true;
};

auto fn_listcomp(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<GeneratorExpr>(LOC, GeneratorExpr::ListGenerator, ac_expr(V0),
                            ac<SemVals>(V1).transform<GeneratorBody>());
};

auto fn_extern_decorators(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  vector<ExprPtr> vs{ast<IdExpr>(LOC, VS.token_to_string())};
  for (auto& v : VS) {
    auto nv = ac<vector<ExprPtr>>(v);
    vs.insert(vs.end(), nv.begin(), nv.end());
  }
  return vs;
};

auto fn_from_id(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return tuple(ac_expr(V0), vector<Param>(), ac_expr(V1), false);
  }
  if (VS.choice() == 1) {
    return tuple(ac_expr(V0), ac<SemVals>(V1).transform<Param>(),
                 VS.size() > 2 ? ac_expr(V2) : ast<IdExpr>(LOC, "NoneType"),
                 true);
  }
  if (VS.choice() == 2) {
    return tuple(ac_expr(V0), vector<Param>{}, (ExprPtr) nullptr, true);
  }
};

auto fn_base_class_args(peg::SemanticValues& VS, any& DT) {
  return VS.transform<ExprPtr>();
};

auto fn_kwarg_or_starred(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return CallExpr::Arg(LOC, ac<string>(V0), ac_expr(V1));
  }
  if (VS.choice() == 1) {
    return CallExpr::Arg(ac_expr(V0));
  }
};

auto fn_import_from(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<ImportStmt>(LOC, VS.size() == 1 ? ac_expr(V0) : nullptr,
                           ast<IdExpr>(LOC, "*"), vector<Param>{}, nullptr, "",
                           int(VS.tokens.size()));
  }
  if (VS.choice() == 1) {
    auto f = VS.size() == 2 ? ac_expr(V0) : nullptr;
    return ast<SuiteStmt>(
        LOC, vmap(ac<SemVals>(VS.size() == 2 ? V1 : V0), [&](const any& i) {
          auto p = ac<pair<any, string>>(i);
          auto t = ac<tuple<ExprPtr, vector<Param>, ExprPtr, bool>>(p.first);
          return ast<ImportStmt>(LOC, f, get<0>(t), move(get<1>(t)), get<2>(t),
                                 p.second, int(VS.tokens.size()), get<3>(t));
        }));
  }
};

auto fn_param_name(peg::SemanticValues& VS, any& DT) {
  return (!VS.tokens.empty() ? VS.token_to_string() : "") + ac<string>(V0);
};

auto fn_star_target(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<StarExpr>(LOC, ac_expr(V0));
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
  if (VS.choice() == 2) {
    return ac_expr(V0);
  }
};

auto fn_excepts(peg::SemanticValues& VS, any& DT) { return VS; };

auto fn_with_parens_item(peg::SemanticValues& VS, any& DT) { return VS; };

auto fn_for_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<ForStmt>(LOC, ac_expr(V0), ac_expr(V1), ac_stmt(V2),
                      VS.size() > 3 ? ac_stmt(VS[3]) : nullptr);
};

auto fn_params(peg::SemanticValues& VS, any& DT) { return VS; };

auto fn_expression(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ac_expr(V0);
  }
  if (VS.choice() == 1) {
    return ast<IfExpr>(LOC, ac_expr(V1), ac_expr(V0), ac_expr(V2));
  }
  if (VS.choice() == 2) {
    return ac_expr(V0);
  }
};

auto fn_kwargs(peg::SemanticValues& VS, any& DT) {
  if (VS.choice() == 0) {
    return VS.transform<CallExpr::Arg>();
  }
  if (VS.choice() == 1) {
    return VS.transform<CallExpr::Arg>();
  }
  if (VS.choice() == 2) {
    return VS.transform<CallExpr::Arg>();
  }
};

auto fn_suite(peg::SemanticValues& VS, any& DT) { return ac_stmt(V0); };

auto fn_from_param(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return Param(LOC, "", ac_expr(V0), nullptr);
};

auto fn_listexpr(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<ListExpr>(LOC, VS.transform<ExprPtr>());
};

auto fn_decorators(peg::SemanticValues& VS, any& DT) {
  return VS.transform<ExprPtr>();
};

auto fn_decorator(peg::SemanticValues& VS, any& DT) { return ac_expr(V0); };

auto fn_small_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0)
    return V0;
  if (VS.choice() == 1) {
    return any(ast<SuiteStmt>(LOC));
  }
  if (VS.choice() == 2) {
    return any(ast<BreakStmt>(LOC));
  }
  if (VS.choice() == 3) {
    return any(ast<ContinueStmt>(LOC));
  }
  if (VS.choice() == 4)
    return V0;
  if (VS.choice() == 5)
    return V0;
  if (VS.choice() == 6)
    return V0;
  if (VS.choice() == 7)
    return V0;
  if (VS.choice() == 8)
    return V0;
  if (VS.choice() == 9)
    return V0;
  if (VS.choice() == 10)
    return V0;
  if (VS.choice() == 11)
    return V0;
  if (VS.choice() == 12)
    return V0;
  if (VS.choice() == 13) {
    return any(ast<ExprStmt>(LOC, ac_expr(V0)));
  }
  if (VS.choice() == 14)
    return V0;
};

auto fn_STR(peg::SemanticValues& VS, any& DT) {
  string s;
  s.reserve(VS.size());
  for (auto& v : VS)
    s.append(ac<string>(v));
  return s;
};

auto fn_except_block(peg::SemanticValues& VS, any& DT) {
  if (VS.choice() == 0) {
    if (VS.size() == 3)
      return TryStmt::Catch{ac<string>(V1), ac_expr(V0), ac_stmt(V2)};
    else
      return TryStmt::Catch{"", ac_expr(V0), ac_stmt(V1)};
  }
  if (VS.choice() == 1) {
    return TryStmt::Catch{"", nullptr, ac_stmt(V0)};
  }
};

auto fn_dot_name(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.size() == 1)
    return ac_expr(V0);
  auto dot = ast<DotExpr>(LOC, ac_expr(V0), ac<string>(V1));
  for (int i = 2; i < VS.size(); i++)
    dot = ast<DotExpr>(LOC, dot, ac<string>(VS[i]));
  return dot;
};

auto fn_conjunction(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    auto b = ast<BinaryExpr>(LOC, ac_expr(V0), "&&", ac_expr(V1));
    for (int i = 2; i < VS.size(); i++)
      b = ast<BinaryExpr>(LOC, b, "&&", ac_expr(VS[i]));
    return b;
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
};

auto fn_del_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<SuiteStmt>(LOC, vmap(VS, [&](const any& i) {
                          return ast<DelStmt>(LOC, ac_expr(i));
                        }));
};

auto fn_from_params(peg::SemanticValues& VS, any& DT) { return VS; };

auto fn_match_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<MatchStmt>(LOC, ac_expr(V0),
                        VS.transform<MatchStmt::MatchCase>(1));
};

auto fn_statements(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<SuiteStmt>(LOC, VS.transform<StmtPtr>());
};

auto fn_star_expression(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<StarExpr>(LOC, ac_expr(V0));
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
};

auto fn_assert_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<AssertStmt>(LOC, ac_expr(V0),
                         VS.size() > 1 ? ac_expr(V1) : nullptr);
};

auto fn_expressions(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return wrap_tuple(VS, LOC);
};

auto fn_SAMEDENT(peg::SemanticValues& VS, any& DT){

};

auto pred_SAMEDENT(const peg::SemanticValues& VS, const any& DT,
                   std::string& MSG) {
  const auto& CTX = any_cast<const ParseContext&>(DT);

  return !(!CTX.indent.size() && VS.sv().size()) &&
         !(CTX.indent.size() && VS.sv().size() != CTX.indent.top());
};

auto fn_starred_expression(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return ast<StarExpr>(LOC, ac_expr(V0));
};

auto fn_compare_op_bitwise_or(peg::SemanticValues& VS, any& DT) {
  if (VS.choice() == 0) {
    return pair(string("not in"), ac_expr(V0));
  }
  if (VS.choice() == 1) {
    return pair(string("is not"), ac_expr(V0));
  }
  if (VS.choice() == 2) {
    return pair(VS.token_to_string(), ac_expr(V0));
  }
  if (VS.choice() == 3) {
    return pair(VS.token_to_string(), ac_expr(V0));
  }
};

auto fn_factor(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<UnaryExpr>(LOC, VS.token_to_string(), ac_expr(V0));
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
};

auto fn_fstring(peg::SemanticValues& VS, any& DT) {
  return make_pair(ac_expr(V0), VS.size() == 1 ? "" : ac<string>(V1));
};

auto fn_slice_part(peg::SemanticValues& VS, any& DT) {
  return VS.size() ? V0 : make_any<ExprPtr>(nullptr);
};

auto fn_atom(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<StringExpr>(LOC, VS.transform<pair<string, string>>());
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
  if (VS.choice() == 2) {
    return ast<BoolExpr>(LOC, true);
  }
  if (VS.choice() == 3) {
    return ast<BoolExpr>(LOC, false);
  }
  if (VS.choice() == 4) {
    return ast<NoneExpr>(LOC);
  }
  if (VS.choice() == 5) {
    return ast<RangeExpr>(LOC, ast<IntExpr>(LOC, ac<string>(V0)),
                          ast<IntExpr>(LOC, ac<string>(V1)));
  }
  if (VS.choice() == 6) {
    return ast<FloatExpr>(LOC, ac<string>(V0),
                          VS.size() > 1 ? ac<string>(V1) : "");
  }
  if (VS.choice() == 7) {
    return ast<IntExpr>(LOC, ac<string>(V0),
                        VS.size() > 1 ? ac<string>(V1) : "");
  }
  if (VS.choice() == 8) {
    return ac_expr(V0);
  }
  if (VS.choice() == 9) {
    return ast<EllipsisExpr>(LOC);
  }
};

auto fn_class_def(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  vector<Param> generics;
  vector<ExprPtr> baseClasses;
  if (VS.size() == 3)
    std::tie(generics, baseClasses) =
        ac<pair<vector<Param>, vector<ExprPtr>>>(V1);
  vector<Param> args;
  auto suite = make_shared<SuiteStmt>();
  auto s =
      const_cast<SuiteStmt*>(ac_stmt(VS.size() == 3 ? V2 : V1)->get_suite());
  assert(s && "not a suite");
  for (auto& i : s->stmts) {
    if (auto a = const_cast<AssignStmt*>(i->get_assign()))
      if (a->lhs->get_id()) {
        args.push_back(Param(a->get_source_info(), a->lhs->get_id()->value,
                             move(a->type), move(a->rhs)));
        continue;
      }
    suite->stmts.push_back(i);
  }
  for (auto& p : generics)
    args.push_back(p);
  return ast<ClassStmt>(LOC, ac<string>(V0), move(args), suite,
                        vector<ExprPtr>{}, baseClasses);
};

auto fn_generics(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  vector<Param> params;
  for (auto& p : VS) {
    auto v = ac<Param>(p);
    v.status = Param::Generic;
    if (!v.type)
      v.type = ast<IdExpr>(LOC, "type");
    params.push_back(v);
  }
  return params;
};

auto fn_program(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.empty())
    return ast<SuiteStmt>(LOC);
  return ac_stmt(V0);
};

auto fn_dict(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<DictExpr>(LOC, VS.transform<ExprPtr>());
};

auto fn_with_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<WithStmt>(LOC, ac<SemVals>(V0).transform<pair<ExprPtr, ExprPtr>>(),
                       ac_stmt(V1));
};

auto fn_assignment(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<AssignStmt>(LOC, ac_expr(V0),
                           VS.size() > 2 ? ac_expr(V2) : nullptr, ac_expr(V1));
  }
  if (VS.choice() == 1) {
    vector<StmtPtr> stmts;
    for (int i = int(VS.size()) - 2; i >= 0; i--)
      stmts.push_back(ast<AssignStmt>(LOC, ac_expr(VS[i]), ac_expr(VS[i + 1])));
    return ast<SuiteStmt>(LOC, move(stmts));
  }
  if (VS.choice() == 2) {
    return ast<AssignStmt>(LOC, ac_expr(V0),
                           ast<BinaryExpr>(LOC, clone(ac_expr(V0)),
                                           ac<string>(V1), ac_expr(V2), true));
  }
};

auto fn_args(peg::SemanticValues& VS, any& DT) {
  auto args = ac<vector<CallExpr::Arg>>(V0);
  if (VS.size() > 1) {
    auto v = ac<vector<CallExpr::Arg>>(V1);
    args.insert(args.end(), v.begin(), v.end());
  }
  return args;
};

auto fn_kwarg_or_double_starred(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return CallExpr::Arg(LOC, ac<string>(V0), ac_expr(V1));
  }
  if (VS.choice() == 1) {
    return CallExpr::Arg(ast<KeywordStarExpr>(LOC, ac_expr(V0)));
  }
};

auto fn_augassign(peg::SemanticValues& VS, any& DT) {
  return VS.token_to_string();
};

auto fn_term(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return chain(VS, LOC);
};

auto fn_star_targets(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return wrap_tuple(VS, LOC);
};

auto fn_return_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<ReturnStmt>(LOC, !VS.empty() ? ac_expr(V0) : nullptr);
};

auto fn_print_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<PrintStmt>(LOC, VS.transform<ExprPtr>(), !VS.tokens.empty());
  }
  if (VS.choice() == 1) {
    return ast<PrintStmt>(LOC, vector<ExprPtr>{}, false);
  }
};

auto fn_sum(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return chain(VS, LOC);
};

auto fn_kvpair(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<TupleExpr>(LOC, std::vector<ExprPtr>{ac_expr(V0), ac_expr(V1)});
};

auto fn_function_def(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    auto params = ac<SemVals>(V2).transform<Param>();
    for (auto& p : ac<vector<Param>>(V1))
      params.push_back(p);
    return ast<FunctionStmt>(LOC, ac<string>(V0),
                             VS.size() == 4 ? ac_expr(VS[3]) : nullptr, params,
                             nullptr);
  }
  if (VS.choice() == 1) {
    return ast<FunctionStmt>(LOC, ac<string>(V0),
                             VS.size() == 3 ? ac_expr(VS[2]) : nullptr,
                             ac<SemVals>(V1).transform<Param>(), nullptr);
  }
};

auto fn_for(peg::SemanticValues& VS, any& DT) {
  if (VS.size() > 1) {
    auto s = dynamic_pointer_cast<ForStmt>(ac_stmt(V1));
    s->decorator = ac_expr(V0);
    return static_pointer_cast<Stmt>(s);
  }
  return ac_stmt(V0);
};

auto fn_extern(peg::SemanticValues& VS, any& DT) { return string(VS.sv()); };

auto fn_class(peg::SemanticValues& VS, any& DT) {
  if (VS.size() == 2) {
    auto fn = ac_stmt(V1);
    dynamic_pointer_cast<ClassStmt>(fn)->decorators = ac<vector<ExprPtr>>(V0);
    dynamic_pointer_cast<ClassStmt>(fn)->parse_decorators();
    return fn;
  }
  return ac_stmt(V0);
};

auto fn_yield_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<YieldFromStmt>(LOC, ac_expr(V0));
  }
  if (VS.choice() == 1) {
    return ast<YieldStmt>(LOC, !VS.empty() ? ac_expr(V0) : nullptr);
  }
};

auto fn_try_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<TryStmt>(LOC, ac_stmt(V0),
                        ac<SemVals>(V1).transform<TryStmt::Catch>(),
                        VS.size() > 2 ? ac_stmt(V2) : nullptr);
  }
  if (VS.choice() == 1) {
    return ast<TryStmt>(LOC, ac_stmt(V0), vector<TryStmt::Catch>{},
                        VS.size() > 1 ? ac_stmt(V1) : nullptr);
  }
};

auto fn_slices(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return wrap_tuple(VS, LOC);
};

auto fn_as_item(peg::SemanticValues& VS, any& DT) {
  if (VS.choice() == 0) {
    return pair(ac_expr(V0), ac_expr(V1));
  }
  if (VS.choice() == 1) {
    return pair(ac_expr(V0), (ExprPtr) nullptr);
  }
};

auto fn_NAME(peg::SemanticValues& VS, any& DT) {
  if (VS.choice() == 0) {
    return string(VS.sv());
  }
  if (VS.choice() == 1) {
    return VS.token_to_string();
  }
};

auto fn_CHAR(peg::SemanticValues& VS, any& DT) { return string(VS.sv()); };

auto fn_global_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<SuiteStmt>(LOC, vmap(VS, [&](const any& i) {
                          return ast<GlobalStmt>(LOC, ac<string>(i), false);
                        }));
};

auto fn_from_as_items(peg::SemanticValues& VS, any& DT) { return VS; };

auto fn_from_as_parens(peg::SemanticValues& VS, any& DT) { return VS; };

auto fn_INT(peg::SemanticValues& VS, any& DT) { return string(VS.sv()); };

auto fn_from_as(peg::SemanticValues& VS, any& DT) {
  return pair(V0, VS.size() > 1 ? ac<string>(V1) : "");
};

auto fn_import_name(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<SuiteStmt>(LOC, vmap(VS.transform<pair<ExprPtr, string>>(),
                                  [&](const pair<ExprPtr, string>& i) {
                                    return ast<ImportStmt>(
                                        LOC, i.first, nullptr, vector<Param>{},
                                        nullptr, i.second);
                                  }));
};

auto fn_if_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  shared_ptr<Stmt> stmt = ast<IfStmt>(LOC, nullptr, nullptr);
  IfStmt* p = (IfStmt*)stmt.get();
  for (int i = 0; i < VS.size(); i += 2) {
    if (i == VS.size() - 1) {
      p->else_suite = ac_stmt(VS[i]);
    } else {
      if (i) {
        p->else_suite = ast<IfStmt>(LOC, nullptr, nullptr);
        p = (IfStmt*)(p->else_ssuite.get());
      }
      p->cond = ac_expr(VS[i]);
      p->if_suite = ac_stmt(VS[i + 1]);
    }
  }
  return stmt;
};

auto fn_function(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    auto fn = dynamic_pointer_cast<FunctionStmt>(ac_stmt(V1));
    fn->decorators = ac<vector<ExprPtr>>(V0);
    fn->suite = ast<ExprStmt>(LOC, ast<StringExpr>(LOC, ac<string>(V2)));
    fn->parse_decorators();
    return static_pointer_cast<Stmt>(fn);
  }
  if (VS.choice() == 1) {
    auto fn =
        dynamic_pointer_cast<FunctionStmt>(ac_stmt(VS.size() > 2 ? V1 : V0));
    if (VS.size() > 2)
      fn->decorators = ac<vector<ExprPtr>>(V0);
    fn->suite = ac_stmt(VS.size() > 2 ? V2 : V1);
    fn->parse_decorators();
    return static_pointer_cast<Stmt>(fn);
  }
};

auto fn_case(peg::SemanticValues& VS, any& DT) {
  if (VS.choice() == 0) {
    return MatchStmt::MatchCase{ac_expr(V0), ac_expr(V1), ac_stmt(V2)};
  }
  if (VS.choice() == 1) {
    return MatchStmt::MatchCase{ac_expr(V0), nullptr, ac_stmt(V1)};
  }
};

auto fn_custom_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<CustomStmt>(LOC, ac<string>(V0), ac_expr(V1), ac_stmt(V2));
  }
  if (VS.choice() == 1) {
    return ast<CustomStmt>(LOC, ac<string>(V0), nullptr, ac_stmt(V2));
  }
};

auto pred_custom_stmt(const peg::SemanticValues& VS, const any& DT,
                      std::string& MSG) {
  const auto& CTX = any_cast<const ParseContext&>(DT);

  auto kwd = ac<string>(V0);
  return CTX.hasCustomStmtKeyword(kwd, VS.choice() == 0);  // ignore it
};

auto fn_genexp(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<GeneratorExpr>(LOC, GeneratorExpr::Generator, ac_expr(V0),
                            ac<SemVals>(V1).transform<GeneratorBody>());
};

auto fn_custom_small_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return any(ast<CustomStmt>(LOC, ac<string>(V0), ac_expr(V1), nullptr));
};

auto pred_custom_small_stmt(const peg::SemanticValues& VS, const any& DT,
                            std::string& MSG) {
  const auto& CTX = any_cast<const ParseContext&>(DT);

  auto kwd = ac<string>(V0);
  return CTX.hasCustomExprStmt(kwd);  // ignore it
};

auto fn_pipe(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    vector<PipeExpr::Pipe> v;
    for (int i = 0; i < VS.size(); i++)
      v.push_back(
          PipeExpr::Pipe{i ? VS.token_to_string(i - 1) : "", ac_expr(VS[i])});
    return ast<PipeExpr>(LOC, move(v));
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
};

auto fn_lambdef(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<LambdaExpr>(LOC, VS.transform<string>(0, VS.size() - 1),
                           ac_expr(VS.back()));
  }
  if (VS.choice() == 1) {
    return ast<LambdaExpr>(LOC, vector<string>{}, ac_expr(VS.back()));
  }
};

auto fn_disjunction(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    auto b = ast<BinaryExpr>(LOC, ac_expr(V0), "||", ac_expr(V1));
    for (int i = 2; i < VS.size(); i++)
      b = ast<BinaryExpr>(LOC, b, "||", ac_expr(VS[i]));
    return b;
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
};

auto fn_inversion(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<UnaryExpr>(LOC, "!", ac_expr(V0));
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
};

auto fn_comparison(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.size() == 1) {
    return ac_expr(V0);
  } else if (VS.size() == 2) {
    auto p = ac<pair<string, ExprPtr>>(V1);
    return ast<BinaryExpr>(LOC, ac_expr(V0), p.first, p.second);
  } else {
    vector<pair<string, ExprPtr>> v{pair(string(), ac_expr(V0))};
    auto vp = VS.transform<pair<string, ExprPtr>>(1);
    v.insert(v.end(), vp.begin(), vp.end());
    return ast<ChainBinaryExpr>(LOC, move(v));
  }
};

auto fn_bitwise_or(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return chain(VS, LOC);
};

auto fn_with_item(peg::SemanticValues& VS, any& DT) { return VS; };

auto fn_bitwise_xor(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return chain(VS, LOC);
};

auto fn_param(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return Param(LOC, ac<string>(V0), ac_expr(V1),
                 VS.size() > 2 ? ac_expr(V2) : nullptr);
  }
  if (VS.choice() == 1) {
    return Param(LOC, ac<string>(V0), nullptr,
                 VS.size() > 1 ? ac_expr(V1) : nullptr);
  }
};

auto fn_FLOAT(peg::SemanticValues& VS, any& DT) { return string(VS.sv()); };

auto fn_shift_expr(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return chain(VS, LOC);
};

auto fn_power(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<BinaryExpr>(LOC, ac_expr(V0), "**", ac_expr(V1));
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
};

auto fn_as_name(peg::SemanticValues& VS, any& DT) {
  return pair(ac_expr(V0), VS.size() > 1 ? ac<string>(V1) : "");
};

auto fn_primary(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  auto e = ac<ExprPtr>(V0);
  for (int i = 1; i < VS.size(); i++) {
    auto p = ac<pair<int, any>>(VS[i]);
    if (p.first == 0)
      e = ast<DotExpr>(LOC, e, ac<string>(p.second));
    else if (p.first == 1)
      e = ast<CallExpr>(LOC, e, ac_expr(p.second));
    else if (p.first == 2)
      e = ast<CallExpr>(LOC, e, ac<vector<CallExpr::Arg>>(p.second));
    else
      e = ast<IndexExpr>(LOC, e, ac_expr(p.second));
  }
  return e;
};

auto fn_primary_tail(peg::SemanticValues& VS, any& DT) {
  if (VS.choice() == 0) {
    return pair(0, V0);
  }
  if (VS.choice() == 1) {
    return pair(1, V0);
  }
  if (VS.choice() == 2) {
    return pair(2, VS.size() ? V0 : any(vector<CallExpr::Arg>{}));
  }
  if (VS.choice() == 3) {
    return pair(3, V0);
  }
};

auto fn_slice(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<SliceExpr>(LOC, ac_expr(V0), ac_expr(V1),
                          VS.size() > 2 ? ac_expr(V2) : nullptr);
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
};

auto fn_class_args(peg::SemanticValues& VS, any& DT) {
  if (VS.choice() == 0) {
    return make_pair(ac<vector<Param>>(V0), ac<vector<ExprPtr>>(V1));
  }
  if (VS.choice() == 1) {
    return make_pair(ac<vector<Param>>(V0), vector<ExprPtr>{});
  }
  if (VS.choice() == 2) {
    return make_pair(vector<Param>{}, ac<vector<ExprPtr>>(V0));
  }
};

auto fn_setcomp(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<GeneratorExpr>(LOC, GeneratorExpr::SetGenerator, ac_expr(V0),
                            ac<SemVals>(V1).transform<GeneratorBody>());
};

auto fn_tuple(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<TupleExpr>(LOC, VS.transform<ExprPtr>());
  }
  if (VS.choice() == 1) {
    return wrap_tuple(VS, LOC);
  }
};

auto fn_INDENT(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  CTX.indent.push(VS.sv().size());
};

auto pred_INDENT(const peg::SemanticValues& VS, const any& DT,
                 std::string& MSG) {
  const auto& CTX = any_cast<const ParseContext&>(DT);

  if (!(CTX.indent.empty() && VS.sv().size()) &&
      !(!CTX.indent.empty() && VS.sv().size() > CTX.indent.top())) {
    MSG = "unexpected indentation";
    return false;
  }
  return true;
};

auto fn_double_starred_kvpair(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<KeywordStarExpr>(LOC, ac_expr(V0));
  }
  if (VS.choice() == 1) {
    return ac<ExprPtr>(V0);
  }
};

auto fn_raise_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<ThrowStmt>(LOC, !VS.empty() ? ac_expr(V0) : nullptr);
};

auto fn_yield(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return ast<YieldExpr>(LOC);
};

auto fn_simple_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<SuiteStmt>(LOC, VS.transform<StmtPtr>());
};

auto fn_set(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<SetExpr>(LOC, VS.transform<ExprPtr>());
};

auto fn_while_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<WhileStmt>(LOC, ac_expr(V0), ac_stmt(V1),
                        VS.size() > 2 ? ac_stmt(V2) : nullptr);
};

auto fn_dictcomp(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  auto p = ac<ExprPtr>(V0);
  return ast<DictGeneratorExpr>(LOC, p->getTuple()->items[0],
                                p->getTuple()->items[1],
                                ac<SemVals>(V1).transform<GeneratorBody>());
};

auto fn_for_if_clauses(peg::SemanticValues& VS, any& DT) { return VS; };

auto fn_for_if_clause(peg::SemanticValues& VS, any& DT) {
  return GeneratorBody{ac_expr(V0), ac_expr(V1), VS.transform<ExprPtr>(2)};
};

auto fn_star_parens(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return wrap_tuple(VS, LOC);
  }
  if (VS.choice() == 1) {
    return wrap_tuple(VS, LOC);
  }
};

auto fn_star_expressions(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return wrap_tuple(VS, LOC);
};

auto fn_star_named_expression(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<StarExpr>(LOC, ac_expr(V0));
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
};

auto fn_named_expression(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  if (VS.choice() == 0) {
    return ast<AssignExpr>(LOC, ast<IdExpr>(LOC, ac<string>(V0)), ac_expr(V1));
  }
  if (VS.choice() == 1) {
    return ac_expr(V0);
  }
};

auto fn_nonlocal_stmt(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());

  return ast<SuiteStmt>(LOC, vmap(VS, [&](const any& i) {
                          return ast<GlobalStmt>(LOC, ac<string>(i), true);
                        }));
};

auto fn_bitwise_and(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return chain(VS, LOC);
};

auto fn_arguments(peg::SemanticValues& VS, any& DT) {
  vector<CallExpr::Arg> result;
  for (auto& v : VS)
    for (auto& i : ac<vector<CallExpr::Arg>>(v))
      result.push_back(i);
  return result;
};

auto fn_simple_args(peg::SemanticValues& VS, any& DT) {
  return vmap(VS, [](auto& i) { return CallExpr::Arg(ac_expr(i)); });
};

auto fn_id(peg::SemanticValues& VS, any& DT) {
  auto& CTX = any_cast<ParseContext&>(DT);
  const auto& LI = VS.line_info();
  auto LOC = Pud::SourceInfo(VS.path, LI.first + CTX.line_offset,
                             LI.second + CTX.col_offset, VS.sv().size());
  return ast<IdExpr>(LOC, ac<string>(V0));
};

auto fn_STRING(peg::SemanticValues& VS, any& DT) {
  auto p = pair(ac<string>(VS.size() > 1 ? V1 : V0),
                VS.size() > 1 ? ac<string>(V0) : "");
  if (p.second != "r" && p.second != "R") {
    p.first = unescape(p.first);
  } else {
    p.second = "";
  }
  return p;
};

auto pred_STRING(const peg::SemanticValues& VS, const any& DT,
                 std::string& MSG) {
  auto p = pair(ac<string>(VS.size() > 1 ? V1 : V0),
                VS.size() > 1 ? ac<string>(V0) : "");
  if (p.second != "r" && p.second != "R")
    try {
      p.first = unescape(p.first);
    } catch (std::invalid_argument& e) {
      MSG = "invalid code in a string";
      return false;
    } catch (std::out_of_range&) {
      MSG = "invalid code in a string";
      return false;
    }
  return true;
};

void init_pud_actions(peg::Grammar& P) {
  grammar["format_spec"] = fn_format_spec;
  grammar["EXTERNDENT"] = fn_EXTERNDENT;
  grammar["EXTERNDENT"].predicate = pred_EXTERNDENT;
  grammar["DEDENT"] = fn_DEDENT;
  grammar["DEDENT"].predicate = pred_DEDENT;
  grammar["listcomp"] = fn_listcomp;
  grammar["extern_decorators"] = fn_extern_decorators;
  grammar["from_id"] = fn_from_id;
  grammar["base_class_args"] = fn_base_class_args;
  grammar["kwarg_or_starred"] = fn_kwarg_or_starred;
  grammar["import_from"] = fn_import_from;
  grammar["param_name"] = fn_param_name;
  grammar["star_target"] = fn_star_target;
  grammar["excepts"] = fn_excepts;
  grammar["with_parens_item"] = fn_with_parens_item;
  grammar["for_stmt"] = fn_for_stmt;
  grammar["params"] = fn_params;
  grammar["expression"] = fn_expression;
  grammar["kwargs"] = fn_kwargs;
  grammar["suite"] = fn_suite;
  grammar["from_param"] = fn_from_param;
  grammar["listexpr"] = fn_listexpr;
  grammar["decorators"] = fn_decorators;
  grammar["decorator"] = fn_decorator;
  grammar["small_stmt"] = fn_small_stmt;
  grammar["STR"] = fn_STR;
  grammar["except_block"] = fn_except_block;
  grammar["dot_name"] = fn_dot_name;
  grammar["conjunction"] = fn_conjunction;
  grammar["del_stmt"] = fn_del_stmt;
  grammar["from_params"] = fn_from_params;
  grammar["match_stmt"] = fn_match_stmt;
  grammar["statements"] = fn_statements;
  grammar["star_expression"] = fn_star_expression;
  grammar["assert_stmt"] = fn_assert_stmt;
  grammar["expressions"] = fn_expressions;
  grammar["SAMEDENT"] = fn_SAMEDENT;
  grammar["SAMEDENT"].predicate = pred_SAMEDENT;
  grammar["starred_expression"] = fn_starred_expression;
  grammar["compare_op_bitwise_or"] = fn_compare_op_bitwise_or;
  grammar["factor"] = fn_factor;
  grammar["fstring"] = fn_fstring;
  grammar["slice_part"] = fn_slice_part;
  grammar["atom"] = fn_atom;
  grammar["class_def"] = fn_class_def;
  grammar["generics"] = fn_generics;
  grammar["program"] = fn_program;
  grammar["dict"] = fn_dict;
  grammar["with_stmt"] = fn_with_stmt;
  grammar["assignment"] = fn_assignment;
  grammar["args"] = fn_args;
  grammar["kwarg_or_double_starred"] = fn_kwarg_or_double_starred;
  grammar["augassign"] = fn_augassign;
  grammar["term"] = fn_term;
  grammar["star_targets"] = fn_star_targets;
  grammar["return_stmt"] = fn_return_stmt;
  grammar["print_stmt"] = fn_print_stmt;
  grammar["sum"] = fn_sum;
  grammar["kvpair"] = fn_kvpair;
  grammar["function_def"] = fn_function_def;
  grammar["for"] = fn_for;
  grammar["extern"] = fn_extern;
  grammar["class"] = fn_class;
  grammar["yield_stmt"] = fn_yield_stmt;
  grammar["try_stmt"] = fn_try_stmt;
  grammar["slices"] = fn_slices;
  grammar["as_item"] = fn_as_item;
  grammar["NAME"] = fn_NAME;
  grammar["CHAR"] = fn_CHAR;
  grammar["global_stmt"] = fn_global_stmt;
  grammar["from_as_items"] = fn_from_as_items;
  grammar["from_as_parens"] = fn_from_as_parens;
  grammar["INT"] = fn_INT;
  grammar["from_as"] = fn_from_as;
  grammar["import_name"] = fn_import_name;
  grammar["if_stmt"] = fn_if_stmt;
  grammar["function"] = fn_function;
  grammar["case"] = fn_case;
  grammar["custom_stmt"] = fn_custom_stmt;
  grammar["custom_stmt"].predicate = pred_custom_stmt;
  grammar["genexp"] = fn_genexp;
  grammar["custom_small_stmt"] = fn_custom_small_stmt;
  grammar["custom_small_stmt"].predicate = pred_custom_small_stmt;
  grammar["pipe"] = fn_pipe;
  grammar["lambdef"] = fn_lambdef;
  grammar["disjunction"] = fn_disjunction;
  grammar["inversion"] = fn_inversion;
  grammar["comparison"] = fn_comparison;
  grammar["bitwise_or"] = fn_bitwise_or;
  grammar["with_item"] = fn_with_item;
  grammar["bitwise_xor"] = fn_bitwise_xor;
  grammar["param"] = fn_param;
  grammar["FLOAT"] = fn_FLOAT;
  grammar["shift_expr"] = fn_shift_expr;
  grammar["power"] = fn_power;
  grammar["as_name"] = fn_as_name;
  grammar["primary"] = fn_primary;
  grammar["primary_tail"] = fn_primary_tail;
  grammar["slice"] = fn_slice;
  grammar["class_args"] = fn_class_args;
  grammar["setcomp"] = fn_setcomp;
  grammar["tuple"] = fn_tuple;
  grammar["INDENT"] = fn_INDENT;
  grammar["INDENT"].predicate = pred_INDENT;
  grammar["double_starred_kvpair"] = fn_double_starred_kvpair;
  grammar["raise_stmt"] = fn_raise_stmt;
  grammar["yield"] = fn_yield;
  grammar["simple_stmt"] = fn_simple_stmt;
  grammar["set"] = fn_set;
  grammar["while_stmt"] = fn_while_stmt;
  grammar["dictcomp"] = fn_dictcomp;
  grammar["for_if_clauses"] = fn_for_if_clauses;
  grammar["for_if_clause"] = fn_for_if_clause;
  grammar["star_parens"] = fn_star_parens;
  grammar["star_expressions"] = fn_star_expressions;
  grammar["star_named_expression"] = fn_star_named_expression;
  grammar["named_expression"] = fn_named_expression;
  grammar["nonlocal_stmt"] = fn_nonlocal_stmt;
  grammar["bitwise_and"] = fn_bitwise_and;
  grammar["arguments"] = fn_arguments;
  grammar["simple_args"] = fn_simple_args;
  grammar["id"] = fn_id;
  grammar["STRING"] = fn_STRING;
  grammar["STRING"].predicate = pred_STRING;
}
