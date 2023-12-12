#include <any>

#include "Pud/Parse/Rules.h"
using namespace std;
using namespace Pud;
using namespace Pud::AST;
using namespace Pud::Parse;

#define V0 VS[0]
#define V1 VS[1]
#define ac std::any_cast

void init_omp_rules(peg::Grammar& grammar) {
  using namespace peg;
  using peg::seq;
  using vc = vector<pair<char32_t, char32_t>>;

  // ~_ <- SPACE*
  ~grammar["_"] <= zom(ref(grammar, "SPACE"));
  grammar["_"].name = "_";

  // ~SPACE <- [ \t]+
  ~grammar["SPACE"] <= oom(cls(vc{{0x20, 0x20}, {0x9, 0x9}}));
  grammar["SPACE"].name = "SPACE";

  // schedule_kind <- ("static" / "dynamic" / "guided" / "auto" / "runtime") {
  //   return VS.token_to_string();
  // }
  grammar["schedule_kind"] <= cho(lit("static"), lit("dynamic"), lit("guided"),
                                  lit("auto"), lit("runtime"));
  grammar["schedule_kind"].name = "schedule_kind";

  // clause <-
  //   / "schedule" _ "(" _ schedule_kind (_ "," _ int)? _ ")" {
  //     vector<CallExpr::Arg> v{{"schedule",
  //     make_shared<StringExpr>(ac<string>(V0))}}; if (VS.size() > 1)
  //       v.push_back({"chunk_size", make_shared<IntExpr>(ac<int>(V1))});
  //     return v;
  //   }
  //   / "num_threads" _ "(" _ int _ ")" {
  //     return vector<CallExpr::Arg>{{"num_threads",
  //     make_shared<IntExpr>(ac<int>(V0))}};
  //   }
  //   / "ordered" {
  //     return vector<CallExpr::Arg>{{"ordered", make_shared<BoolExpr>(true)}};
  //   }
  //   / "collapse" {
  //     return vector<CallExpr::Arg>{{"collapse",
  //     make_shared<IntExpr>(ac<int>(V0))}};
  //   }
  //   / "gpu" {
  //     return vector<CallExpr::Arg>{{"gpu", make_shared<BoolExpr>(true)}};
  //   }
  grammar["clause"] <= cho(seq(lit("schedule"), ref(grammar, "_"), lit("("),
                               ref(grammar, "_"), ref(grammar, "schedule_kind"),
                               opt(seq(ref(grammar, "_"), lit(","),
                                       ref(grammar, "_"), ref(grammar, "int"))),
                               ref(grammar, "_"), lit(")")),
                           seq(lit("num_threads"), ref(grammar, "_"), lit("("),
                               ref(grammar, "_"), ref(grammar, "int"),
                               ref(grammar, "_"), lit(")")),
                           lit("ordered"), lit("collapse"), lit("gpu"));
  grammar["clause"].name = "clause";

  // pragma <- "omp"? _ "parallel"? _ (clause _)* {
  //   vector<CallExpr::Arg> v;
  //   for (auto &i: VS) {
  //     auto vi = ac<vector<CallExpr::Arg>>(i);
  //     v.insert(v.end(), vi.begin(), vi.end());
  //   }
  //   return v;
  // }
  grammar["pragma"] <= seq(opt(lit("omp")), ref(grammar, "_"),
                           opt(lit("parallel")), ref(grammar, "_"),
                           zom(seq(ref(grammar, "clause"), ref(grammar, "_"))));
  grammar["pragma"].name = "pragma";

  // int <- [1-9] [0-9]* {
  //   return stoi(VS.token_to_string());
  // }
  grammar["int"] <= seq(cls(vc{{0x31, 0x39}}), zom(cls(vc{{0x30, 0x39}})));
  grammar["int"].name = "int";
  ~grammar["%recover"] <= ref(grammar, "x");
  grammar["%recover"].name = "%recover";
  grammar["%recover"].is_macro = true;
  grammar["%recover"].params = {"x"};
}

auto fn_schedule_kind(peg::SemanticValues& VS, any& DT) {
  return VS.token_to_string();
};

auto fn_clause(peg::SemanticValues& VS, any& DT) {
  if (VS.choice() == 0) {
    vector<CallExpr::Arg> v{
        {"schedule", make_shared<StringExpr>(ac<string>(V0))}};
    if (VS.size() > 1)
      v.push_back({"chunk_size", make_shared<IntExpr>(ac<int>(V1))});
    return v;
  }
  if (VS.choice() == 1) {
    return vector<CallExpr::Arg>{
        {"num_threads", make_shared<IntExpr>(ac<int>(V0))}};
  }
  if (VS.choice() == 2) {
    return vector<CallExpr::Arg>{{"ordered", make_shared<BoolExpr>(true)}};
  }
  if (VS.choice() == 3) {
    return vector<CallExpr::Arg>{
        {"collapse", make_shared<IntExpr>(ac<int>(V0))}};
  }
  if (VS.choice() == 4) {
    return vector<CallExpr::Arg>{{"gpu", make_shared<BoolExpr>(true)}};
  }
};

auto fn_pragma(peg::SemanticValues& VS, any& DT) {
  vector<CallExpr::Arg> v;
  for (auto& i : VS) {
    auto vi = ac<vector<CallExpr::Arg>>(i);
    v.insert(v.end(), vi.begin(), vi.end());
  }
  return v;
};

auto fn_int(peg::SemanticValues& VS, any& DT) {
  return stoi(VS.token_to_string());
};

void init_omp_actions(peg::Grammar& grammar) {
  grammar["schedule_kind"] = fn_schedule_kind;
  grammar["clause"] = fn_clause;
  grammar["pragma"] = fn_pragma;
  grammar["int"] = fn_int;
}