#include "Pud/AST/Stmt.h"

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "Pud/AST/ASTVisitor.h"
#include "Pud/Common/Clone.h"
#include "Pud/Common/Error.h"
#include "Pud/Common/Str.h"

namespace Pud::AST {

const int INDENT_SIZE = 2;

Stmt::Stmt() : done(false), age(-1) {}
Stmt::Stmt(const Pud::SourceInfo& s) : done(false), age(-1) {
  set_source_info(s);
}
auto Stmt::to_string() const -> std::string { return to_string(-1); }
void Stmt::validate() const {}

SuiteStmt::SuiteStmt(std::vector<StmtPtr> stmts) {
  for (auto& s : stmts) {
    flatten(s, this->stmts);
  }
}
SuiteStmt::SuiteStmt(const SuiteStmt& stmt)
    : Stmt(stmt), stmts(::Pud::clone(stmt.stmts)) {}
auto SuiteStmt::to_string(int indent) const -> std::string {
  std::string pad =
      indent >= 0 ? ("\n" + std::string(indent + INDENT_SIZE, ' ')) : " ";
  std::string s;
  for (int i = 0; i < stmts.size(); i++) {
    if (stmts[i]) {
      auto is = stmts[i]->to_string(indent >= 0 ? indent + INDENT_SIZE : -1);
      if (stmts[i]->done) {
        is.insert(find_star(is), "*");
      }
      s += (i ? pad : "") + is;
    }
  }
  return fmt::format("(suite{})", s.empty() ? s : " " + pad + s);
}
auto SuiteStmt::clone() const -> StmtPtr {
  return std::make_shared<SuiteStmt>(*this);
}
void SuiteStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }
void SuiteStmt::flatten(const StmtPtr& s, std::vector<StmtPtr>& stmts) {
  if (!s) {
    return;
  }
  if (!s->get_suite()) {
    stmts.push_back(s);
  } else {
    for (auto& ss : s->get_suite()->stmts) {
      stmts.push_back(ss);
    }
  }
}
auto SuiteStmt::last_in_block() -> StmtPtr* {
  if (stmts.empty()) {
    return nullptr;
  }
  if (auto* s = stmts.back()->get_suite()) {
    auto* l = s->last_in_block();
    if (l) {
      return l;
    }
  }
  return &(stmts.back());
}

auto BreakStmt::to_string(int /*indent*/) const -> std::string {
  return "(break)";
}
auto BreakStmt::clone() const -> StmtPtr {
  return std::make_shared<BreakStmt>(*this);
}
void BreakStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

auto ContinueStmt::to_string(int /*indent*/) const -> std::string {
  return "(continue)";
}
auto ContinueStmt::clone() const -> StmtPtr {
  return std::make_shared<ContinueStmt>(*this);
}
void ContinueStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

ExprStmt::ExprStmt(ExprPtr expr) : expr(std::move(expr)) {}
ExprStmt::ExprStmt(const ExprStmt& stmt)
    : Stmt(stmt), expr(::Pud::clone(stmt.expr)) {}
auto ExprStmt::to_string(int) const -> std::string {
  return fmt::format("(expr {})", expr->to_string());
}
auto ExprStmt::clone() const -> StmtPtr {
  return std::make_shared<ExprStmt>(*this);
}
void ExprStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

AssignStmt::AssignStmt(ExprPtr lhs, ExprPtr rhs, ExprPtr type)
    : lhs(std::move(lhs)),
      rhs(std::move(rhs)),
      type(std::move(type)),
      update(Assign) {}
AssignStmt::AssignStmt(const AssignStmt& stmt)
    : Stmt(stmt),
      lhs(::Pud::clone(stmt.lhs)),
      rhs(::Pud::clone(stmt.rhs)),
      type(::Pud::clone(stmt.type)),
      update(stmt.update) {}
auto AssignStmt::to_string(int) const -> std::string {
  return fmt::format("({} {}{}{})", update != Assign ? "update" : "assign",
                     lhs->to_string(), rhs ? " " + rhs->to_string() : "",
                     type ? fmt::format(" #:type {}", type->to_string()) : "");
}
auto AssignStmt::clone() const -> StmtPtr {
  return std::make_shared<AssignStmt>(*this);
}
void AssignStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

PrintStmt::PrintStmt(std::vector<ExprPtr> items, bool is_inline)
    : items(std::move(items)), is_inline(is_inline) {}
PrintStmt::PrintStmt(const PrintStmt& stmt)
    : Stmt(stmt), items(::Pud::clone(stmt.items)), is_inline(stmt.is_inline) {}
auto PrintStmt::to_string(int) const -> std::string {
  return fmt::format("(print {}{})", is_inline ? "#:inline " : "",
                     combine(items));
}
auto PrintStmt::clone() const -> StmtPtr {
  return std::make_shared<PrintStmt>(*this);
}
void PrintStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

ReturnStmt::ReturnStmt(ExprPtr expr) : expr(std::move(expr)) {}
ReturnStmt::ReturnStmt(const ReturnStmt& stmt)
    : Stmt(stmt), expr(::Pud::clone(stmt.expr)) {}
auto ReturnStmt::to_string(int) const -> std::string {
  return expr ? fmt::format("(return {})", expr->to_string()) : "(return)";
}
auto ReturnStmt::clone() const -> StmtPtr {
  return std::make_shared<ReturnStmt>(*this);
}
void ReturnStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

YieldStmt::YieldStmt(ExprPtr expr) : expr(std::move(expr)) {}
YieldStmt::YieldStmt(const YieldStmt& stmt)
    : Stmt(stmt), expr(::Pud::clone(stmt.expr)) {}
auto YieldStmt::to_string(int) const -> std::string {
  return expr ? fmt::format("(yield {})", expr->to_string()) : "(yield)";
}
auto YieldStmt::clone() const -> StmtPtr {
  return std::make_shared<YieldStmt>(*this);
}
void YieldStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

AssertStmt::AssertStmt(ExprPtr expr, ExprPtr message)
    : expr(std::move(expr)), message(std::move(message)) {}
AssertStmt::AssertStmt(const AssertStmt& stmt)
    : Stmt(stmt),
      expr(::Pud::clone(stmt.expr)),
      message(::Pud::clone(stmt.message)) {}
auto AssertStmt::to_string(int) const -> std::string {
  return fmt::format("(assert {}{})", expr->to_string(),
                     message ? message->to_string() : "");
}
auto AssertStmt::clone() const -> StmtPtr {
  return std::make_shared<AssertStmt>(*this);
}
void AssertStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

WhileStmt::WhileStmt(ExprPtr cond, StmtPtr suite, StmtPtr else_suite)
    : cond(std::move(cond)),
      suite(std::move(suite)),
      else_suite(std::move(else_suite)) {}
WhileStmt::WhileStmt(const WhileStmt& stmt)
    : Stmt(stmt),
      cond(::Pud::clone(stmt.cond)),
      suite(::Pud::clone(stmt.suite)),
      else_suite(::Pud::clone(stmt.else_suite)) {}
auto WhileStmt::to_string(int indent) const -> std::string {
  std::string pad =
      indent > 0 ? ("\n" + std::string(indent + INDENT_SIZE, ' ')) : " ";
  if (else_suite && else_suite->first_in_block()) {
    return fmt::format(
        "(while-else {}{}{}{}{})", cond->to_string(), pad,
        suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1), pad,
        else_suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1));
  } else {
    return fmt::format(
        "(while {}{}{})", cond->to_string(), pad,
        suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1));
  }
}
auto WhileStmt::clone() const -> StmtPtr {
  return std::make_shared<WhileStmt>(*this);
}
void WhileStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

ForStmt::ForStmt(ExprPtr var, ExprPtr iter, StmtPtr suite, StmtPtr else_suite,
                 ExprPtr decorator, std::vector<CallExpr::Arg> omp_args)
    : var(std::move(var)),
      iter(std::move(iter)),
      suite(std::move(suite)),
      else_suite(std::move(else_suite)),
      decorator(std::move(decorator)),
      omp_args(std::move(omp_args)),
      wrapped(false) {}
ForStmt::ForStmt(const ForStmt& stmt)
    : Stmt(stmt),
      var(::Pud::clone(stmt.var)),
      iter(::Pud::clone(stmt.iter)),
      suite(::Pud::clone(stmt.suite)),
      else_suite(::Pud::clone(stmt.else_suite)),
      decorator(::Pud::clone(stmt.decorator)),
      omp_args(::Pud::clone_nop(stmt.omp_args)),
      wrapped(stmt.wrapped) {}
auto ForStmt::to_string(int indent) const -> std::string {
  std::string pad =
      indent > 0 ? ("\n" + std::string(indent + INDENT_SIZE, ' ')) : " ";
  std::string attr;
  if (decorator) {
    attr += " " + decorator->to_string();
  }
  if (!attr.empty()) {
    attr = " #:attr" + attr;
  }
  if (else_suite && else_suite->first_in_block()) {
    return fmt::format(
        "(for-else {} {}{}{}{}{}{})", var->to_string(), iter->to_string(), attr,
        pad, suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1), pad,
        else_suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1));
  } else {
    return fmt::format(
        "(for {} {}{}{}{})", var->to_string(), iter->to_string(), attr, pad,
        suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1));
  }
}
auto ForStmt::clone() const -> StmtPtr {
  return std::make_shared<ForStmt>(*this);
}
void ForStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

IfStmt::IfStmt(ExprPtr cond, StmtPtr if_suite, StmtPtr else_suite)
    : cond(std::move(cond)),
      if_suite(std::move(if_suite)),
      else_suite(std::move(else_suite)) {}
IfStmt::IfStmt(const IfStmt& stmt)
    : Stmt(stmt),
      cond(::Pud::clone(stmt.cond)),
      if_suite(::Pud::clone(stmt.if_suite)),
      else_suite(::Pud::clone(stmt.else_suite)) {}
auto IfStmt::to_string(int indent) const -> std::string {
  std::string pad =
      indent > 0 ? ("\n" + std::string(indent + INDENT_SIZE, ' ')) : " ";
  return fmt::format(
      "(if {}{}{}{})", cond->to_string(), pad,
      if_suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1),
      else_suite
          ? pad + else_suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1)
          : "");
}
auto IfStmt::clone() const -> StmtPtr {
  return std::make_shared<IfStmt>(*this);
}
void IfStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

auto MatchStmt::MatchCase::clone() const -> MatchStmt::MatchCase {
  return {::Pud::clone(pattern), ::Pud::clone(guard), ::Pud::clone(suite)};
}

MatchStmt::MatchStmt(ExprPtr what, std::vector<MatchStmt::MatchCase> cases)
    : what(std::move(what)), cases(std::move(cases)) {}
MatchStmt::MatchStmt(const MatchStmt& stmt)
    : Stmt(stmt),
      what(::Pud::clone(stmt.what)),
      cases(::Pud::clone_nop(stmt.cases)) {}
auto MatchStmt::to_string(int indent) const -> std::string {
  std::string pad =
      indent > 0 ? ("\n" + std::string(indent + INDENT_SIZE, ' ')) : " ";
  std::string pad_extra = indent > 0 ? std::string(INDENT_SIZE, ' ') : "";
  std::vector<std::string> s;
  s.reserve(cases.size());
  for (const auto& c : cases) {
    s.push_back(fmt::format(
        "(case {}{}{}{})", c.pattern->to_string(),
        c.guard ? " #:guard " + c.guard->to_string() : "", pad + pad_extra,
        c.suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1 * 2)));
  }
  return fmt::format("(match {}{}{})", what->to_string(), pad, join(s, pad));
}
auto MatchStmt::clone() const -> StmtPtr {
  return std::make_shared<MatchStmt>(*this);
}
void MatchStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

ImportStmt::ImportStmt(ExprPtr from, ExprPtr what, std::vector<Param> args,
                       ExprPtr ret, std::string as, size_t dots,
                       bool is_function)
    : from(std::move(from)),
      what(std::move(what)),
      as(std::move(as)),
      dots(dots),
      args(std::move(args)),
      ret(std::move(ret)),
      is_function(is_function) {
  validate();
}
ImportStmt::ImportStmt(const ImportStmt& stmt)
    : Stmt(stmt),
      from(::Pud::clone(stmt.from)),
      what(::Pud::clone(stmt.what)),
      as(stmt.as),
      dots(stmt.dots),
      args(::Pud::clone_nop(stmt.args)),
      ret(::Pud::clone(stmt.ret)),
      is_function(stmt.is_function) {}
auto ImportStmt::to_string(int) const -> std::string {
  std::vector<std::string> va;
  va.reserve(args.size());
  for (const auto& a : args) {
    va.push_back(a.to_string());
  }
  return fmt::format("(import {}{}{}{}{}{})", from->to_string(),
                     as.empty() ? "" : fmt::format(" #:as '{}", as),
                     what ? fmt::format(" #:what {}", what->to_string()) : "",
                     dots ? fmt::format(" #:dots {}", dots) : "",
                     va.empty() ? "" : fmt::format(" #:args ({})", join(va)),
                     ret ? fmt::format(" #:ret {}", ret->to_string()) : "");
}
void ImportStmt::validate() const {
  if (from) {
    Expr* e = from.get();
    while (auto* d = e->get_dot()) {
      e = d->expr.get();
    }
    if (!from->is_id("C") && !from->is_id("python")) {
      if (!e->get_id()) {
        Err(Error::IMPORT_IDENTIFIER, e);
      }
      if (!args.empty()) {
        Err(Error::IMPORT_FN, args[0]);
      }
      if (ret) {
        Err(Error::IMPORT_FN, ret);
      }
      if (what && !what->get_id()) {
        Err(Error::IMPORT_IDENTIFIER, what);
      }
    }
    if (!is_function && !args.empty()) {
      Err(Error::IMPORT_FN, args[0]);
    }
  }
}
auto ImportStmt::clone() const -> StmtPtr {
  return std::make_shared<ImportStmt>(*this);
}
void ImportStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

auto TryStmt::Catch::clone() const -> TryStmt::Catch {
  return {var, ::Pud::clone(exc), ::Pud::clone(suite)};
}

TryStmt::TryStmt(StmtPtr suite, std::vector<Catch> catches, StmtPtr finally)
    : suite(std::move(suite)),
      catches(std::move(catches)),
      finally(std::move(finally)) {}
TryStmt::TryStmt(const TryStmt& stmt)
    : Stmt(stmt),
      suite(::Pud::clone(stmt.suite)),
      catches(::Pud::clone_nop(stmt.catches)),
      finally(::Pud::clone(stmt.finally)) {}
auto TryStmt::to_string(int indent) const -> std::string {
  std::string pad =
      indent > 0 ? ("\n" + std::string(indent + INDENT_SIZE, ' ')) : " ";
  std::string pad_extra = indent > 0 ? std::string(INDENT_SIZE, ' ') : "";
  std::vector<std::string> s;
  s.reserve(catches.size());
  for (const auto& i : catches) {
    s.push_back(fmt::format(
        "(catch {}{}{}{})",
        !i.var.empty() ? fmt::format("#:var '{}", i.var) : "",
        i.exc ? fmt::format(" #:exc {}", i.exc->to_string()) : "",
        pad + pad_extra,
        i.suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1 * 2)));
  }
  return fmt::format(
      "(try{}{}{}{}{})", pad,
      suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1), pad,
      join(s, pad),
      finally ? fmt::format(
                    "{}{}", pad,
                    finally->to_string(indent >= 0 ? indent + INDENT_SIZE : -1))
              : "");
}
auto TryStmt::clone() const -> StmtPtr {
  return std::make_shared<TryStmt>(*this);
}
void TryStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

ThrowStmt::ThrowStmt(ExprPtr expr, bool transformed)
    : expr(std::move(expr)), transformed(transformed) {}
ThrowStmt::ThrowStmt(const ThrowStmt& stmt)
    : Stmt(stmt),
      expr(::Pud::clone(stmt.expr)),
      transformed(stmt.transformed) {}
auto ThrowStmt::to_string(int) const -> std::string {
  return fmt::format("(throw{})", expr ? " " + expr->to_string() : "");
}
auto ThrowStmt::clone() const -> StmtPtr {
  return std::make_shared<ThrowStmt>(*this);
}
void ThrowStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

GlobalStmt::GlobalStmt(std::string var, bool non_local)
    : var(std::move(var)), non_local(non_local) {}
auto GlobalStmt::to_string(int) const -> std::string {
  return fmt::format("({} '{})", non_local ? "nonlocal" : "global", var);
}
auto GlobalStmt::clone() const -> StmtPtr {
  return std::make_shared<GlobalStmt>(*this);
}
void GlobalStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

const std::string Attr::LLVM = "llvm";
const std::string Attr::Python = "python";
const std::string Attr::Atomic = "atomic";
const std::string Attr::Property = "property";
const std::string Attr::StaticMethod = "staticmethod";
const std::string Attr::Attribute = "__attribute__";
const std::string Attr::Internal = "__internal__";
const std::string Attr::ForceRealize = "__force__";
const std::string Attr::RealizeWithoutSelf =
    "std.internal.attributes.realize_without_self";
const std::string Attr::HiddenFromUser = "__hidden__";
const std::string Attr::C = "C";
const std::string Attr::CVarArg = ".__vararg__";
const std::string Attr::Method = ".__method__";
const std::string Attr::Capture = ".__capture__";
const std::string Attr::HasSelf = ".__hasself__";
const std::string Attr::Extend = "extend";
const std::string Attr::Tuple = "tuple";
const std::string Attr::Test = "std.internal.attributes.test";
const std::string Attr::Overload = "overload";
const std::string Attr::Export = "std.internal.attributes.export";

Attr::Attr(const std::vector<std::string>& attrs)
    : module(), parent_class(), is_attribute(false) {
  for (const auto& a : attrs) {
    set(a);
  }
}
void Attr::set(const std::string& attr) { custom_attr.insert(attr); }
void Attr::unset(const std::string& attr) { custom_attr.erase(attr); }
auto Attr::has(const std::string& attr) const -> bool {
  return in(custom_attr, attr);
}

FunctionStmt::FunctionStmt(std::string name, ExprPtr ret,
                           std::vector<Param> args, StmtPtr suite,
                           Attr attributes, std::vector<ExprPtr> decorators)
    : name(std::move(name)),
      ret(std::move(ret)),
      args(std::move(args)),
      suite(std::move(suite)),
      attributes(std::move(attributes)),
      decorators(std::move(decorators)) {
  parse_decorators();
}
FunctionStmt::FunctionStmt(const FunctionStmt& stmt)
    : Stmt(stmt),
      name(stmt.name),
      ret(::Pud::clone(stmt.ret)),
      args(::Pud::clone_nop(stmt.args)),
      suite(::Pud::clone(stmt.suite)),
      attributes(stmt.attributes),
      decorators(::Pud::clone(stmt.decorators)) {}
auto FunctionStmt::to_string(int indent) const -> std::string {
  std::string pad =
      indent > 0 ? ("\n" + std::string(indent + INDENT_SIZE, ' ')) : " ";
  std::vector<std::string> as;
  as.reserve(args.size());
  for (auto& a : args) {
    as.push_back(a.to_string());
  }
  std::vector<std::string> dec;
  std::vector<std::string> attr;
  for (auto& a : decorators) {
    if (a) {
      dec.push_back(fmt::format("(dec {})", a->to_string()));
    }
  }
  for (auto& a : attributes.custom_attr)
    attr.push_back(fmt::format("'{}'", a));
  return fmt::format(
      "(fn '{} ({}){}{}{}{}{})", name, join(as, " "),
      ret ? " #:ret " + ret->to_string() : "",
      dec.empty() ? "" : fmt::format(" (dec {})", join(dec, " ")),
      attr.empty() ? "" : fmt::format(" (attr {})", join(attr, " ")), pad,
      suite ? suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1)
            : "(suite)");
}
void FunctionStmt::validate() const {
  if (!ret && (attributes.has(Attr::LLVM) || attributes.has(Attr::C)))
    Err(Error::FN_LLVM, get_source_info());

  std::unordered_set<std::string> seen_args;
  bool defaults_started = false;
  bool has_star_arg = false;
  bool has_kw_arg = false;
  for (size_t ia = 0; ia < args.size(); ia++) {
    auto& a = args[ia];
    auto n = a.name;
    int stars = trim_stars(n);
    if (stars == 2) {
      if (has_kw_arg)
        Err(Error::FN_MULTIPLE_ARGS, a);
      if (a.default_value)
        Err(Error::FN_DEFAULT_STARARG, a.default_value);
      if (ia != args.size() - 1)
        Err(Error::FN_LAST_KWARG, a);
      has_kw_arg = true;
    } else if (stars == 1) {
      if (has_star_arg)
        Err(Error::FN_MULTIPLE_ARGS, a);
      if (a.default_value)
        Err(Error::FN_DEFAULT_STARARG, a.default_value);
      has_star_arg = true;
    }
    if (in(seen_args, n))
      Err(Error::FN_ARG_TWICE, a, n);
    seen_args.insert(n);
    if (!a.default_value && defaults_started && !stars &&
        a.status == Param::Normal)
      Err(Error::FN_DEFAULT, a, n);
    defaults_started |= bool(a.default_value);
    if (attributes.has(Attr::C)) {
      if (a.default_value)
        Err(Error::FN_C_DEFAULT, a.default_value, n);
      if (stars != 1 && !a.type)
        Err(Error::FN_C_TYPE, a, n);
    }
  }
}
auto FunctionStmt::signature() const -> std::string {
  std::vector<std::string> s;
  for (auto& a : args)
    s.push_back(a.type ? a.type->to_string() : "-");
  return fmt::format("{}", join(s, ":"));
}
auto FunctionStmt::has_attr(const std::string& attr) const -> bool {
  return attributes.has(attr);
}
void FunctionStmt::parse_decorators() {
  std::vector<ExprPtr> new_decorators;
  for (auto& d : decorators) {
    if (d->is_id(Attr::Attribute)) {
      if (decorators.size() != 1)
        Err(Error::FN_SINGLE_DECORATOR, decorators[1], Attr::Attribute);
      attributes.is_attribute = true;
    } else if (d->is_id(Attr::LLVM)) {
      attributes.set(Attr::LLVM);
    } else if (d->is_id(Attr::Python)) {
      if (decorators.size() != 1)
        Err(Error::FN_SINGLE_DECORATOR, decorators[1], Attr::Python);
      attributes.set(Attr::Python);
    } else if (d->is_id(Attr::Internal)) {
      attributes.set(Attr::Internal);
    } else if (d->is_id(Attr::HiddenFromUser)) {
      attributes.set(Attr::HiddenFromUser);
    } else if (d->is_id(Attr::Atomic)) {
      attributes.set(Attr::Atomic);
    } else if (d->is_id(Attr::Property)) {
      attributes.set(Attr::Property);
    } else if (d->is_id(Attr::StaticMethod)) {
      attributes.set(Attr::StaticMethod);
    } else if (d->is_id(Attr::ForceRealize)) {
      attributes.set(Attr::ForceRealize);
    } else if (d->is_id(Attr::C)) {
      attributes.set(Attr::C);
    } else {
      new_decorators.emplace_back(d);
    }
  }
  if (attributes.has(Attr::C)) {
    for (auto& a : args) {
      if (a.name.size() > 1 && a.name[0] == '*' && a.name[1] != '*') {
        attributes.set(Attr::CVarArg);
      }
    }
  }
  if (!args.empty() && !args[0].type && args[0].name == "self") {
    attributes.set(Attr::HasSelf);
  }
  decorators = new_decorators;
  validate();
}
auto FunctionStmt::get_star_args() const -> size_t {
  size_t i = 0;
  while (i < args.size()) {
    if (startswith(args[i].name, "*") && !startswith(args[i].name, "**")) {
      break;
    }
    i++;
  }
  return i;
}
auto FunctionStmt::get_kw_star_args() const -> size_t {
  size_t i = 0;
  while (i < args.size()) {
    if (startswith(args[i].name, "**")) {
      break;
    }
    i++;
  }
  return i;
}
auto FunctionStmt::get_doc_str() -> std::string {
  if (auto s = suite->first_in_block()) {
    if (auto e = s->get_expr()) {
      if (auto ss = e->expr->get_string()) {
        return ss->get_value();
      }
    }
  }
  return "";
}
auto FunctionStmt::clone() const -> StmtPtr {
  return std::make_shared<FunctionStmt>(*this);
}
void FunctionStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

// Search expression tree for a identifier
class IdSearchVisitor : public CallbackASTVisitor<bool, bool> {
  std::string what;
  bool result;

 public:
  IdSearchVisitor(std::string what) : what(std::move(what)), result(false) {}
  auto transform(const std::shared_ptr<Expr>& expr) -> bool override {
    if (result) {
      return result;
    }
    IdSearchVisitor v(what);
    if (expr) {
      expr->accept(v);
    }
    return result = v.result;
  }
  auto transform(const std::shared_ptr<Stmt>& stmt) -> bool override {
    if (result) {
      return result;
    }
    IdSearchVisitor v(what);
    if (stmt) {
      stmt->accept(v);
    }
    return result = v.result;
  }
  void visit(IdExpr* expr) override {
    if (expr->value == what) {
      result = true;
    }
  }
};

auto FunctionStmt::get_non_inferrable_generics()
    -> std::unordered_set<std::string> {
  std::unordered_set<std::string> non_inferrable_generics;
  for (auto& a : args) {
    if (a.status == Param::Generic && !a.default_value) {
      bool inferrable = false;
      for (auto& b : args) {
        if (b.type && IdSearchVisitor(a.name).transform(b.type)) {
          inferrable = true;
          break;
        }
      }
      if (ret && IdSearchVisitor(a.name).transform(ret)) {
        inferrable = true;
      }
      if (!inferrable) {
        non_inferrable_generics.insert(a.name);
      }
    }
  }
  return non_inferrable_generics;
}

ClassStmt::ClassStmt(std::string name, std::vector<Param> args, StmtPtr suite,
                     std::vector<ExprPtr> decorators,
                     std::vector<ExprPtr> base_classes,
                     std::vector<ExprPtr> static_base_classes)
    : name(std::move(name)),
      args(std::move(args)),
      suite(std::move(suite)),
      decorators(std::move(decorators)),
      static_base_classes(std::move(static_base_classes)) {
  for (auto& b : base_classes) {
    if (b->get_index() && b->get_index()->expr->is_id("Static")) {
      this->static_base_classes.push_back(b->get_index()->index);
    } else {
      this->base_classes.push_back(b);
    }
  }
  parse_decorators();
}
ClassStmt::ClassStmt(std::string name, std::vector<Param> args, StmtPtr suite,
                     Attr attr)
    : name(std::move(name)),
      args(std::move(args)),
      suite(std::move(suite)),
      attributes(std::move(attr)) {
  validate();
}
ClassStmt::ClassStmt(const ClassStmt& stmt)
    : Stmt(stmt),
      name(stmt.name),
      args(::Pud::clone_nop(stmt.args)),
      suite(::Pud::clone(stmt.suite)),
      attributes(stmt.attributes),
      decorators(::Pud::clone(stmt.decorators)),
      base_classes(::Pud::clone(stmt.base_classes)),
      static_base_classes(::Pud::clone(stmt.static_base_classes)) {}
auto ClassStmt::to_string(int indent) const -> std::string {
  std::string pad =
      indent > 0 ? ("\n" + std::string(indent + INDENT_SIZE, ' ')) : " ";
  std::vector<std::string> bases;
  for (auto& b : base_classes)
    bases.push_back(b->to_string());
  for (auto& b : static_base_classes)
    bases.push_back(fmt::format("(static {})", b->to_string()));
  std::string as;
  for (int i = 0; i < args.size(); i++)
    as += (i ? pad : "") + args[i].to_string();
  std::vector<std::string> attr;
  for (auto& a : decorators)
    attr.push_back(fmt::format("(dec {})", a->to_string()));
  return fmt::format(
      "(class '{}{}{}{}{}{})", name,
      bases.empty() ? "" : fmt::format(" (bases {})", join(bases, " ")),
      attr.empty() ? "" : fmt::format(" (attr {})", join(attr, " ")),
      as.empty() ? as : pad + as, pad,
      suite ? suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1)
            : "(suite)");
}
void ClassStmt::validate() const {
  std::unordered_set<std::string> seen;
  if (attributes.has(Attr::Extend) && !args.empty())
    Err(Error::CLASS_EXTENSION, args[0]);
  if (attributes.has(Attr::Extend) &&
      !(base_classes.empty() && static_base_classes.empty()))
    Err(Error::CLASS_EXTENSION,
        base_classes.empty() ? static_base_classes[0] : base_classes[0]);
  for (auto& a : args) {
    if (!a.type && !a.default_value)
      Err(Error::CLASS_MISSING_TYPE, a, a.name);
    if (in(seen, a.name))
      Err(Error::CLASS_ARG_TWICE, a, a.name);
    seen.insert(a.name);
  }
}
auto ClassStmt::clone() const -> StmtPtr {
  return std::make_shared<ClassStmt>(*this);
}
void ClassStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }
auto ClassStmt::is_record() const -> bool { return has_attr(Attr::Tuple); }
auto ClassStmt::has_attr(const std::string& attr) const -> bool {
  return attributes.has(attr);
}
void ClassStmt::parse_decorators() {
  // @tuple(init=, repr=, eq=, order=, hash=, pickle=, container=, python=,
  // add=, internal=...)
  // @dataclass(...)
  // @extend

  std::map<std::string, bool> tuple_magics = {
      {"new", true},           {"repr", false},    {"hash", false},
      {"eq", false},           {"ne", false},      {"lt", false},
      {"le", false},           {"gt", false},      {"ge", false},
      {"pickle", true},        {"unpickle", true}, {"to_py", false},
      {"from_py", false},      {"iter", false},    {"getitem", false},
      {"len", false},          {"to_gpu", false},  {"from_gpu", false},
      {"from_gpu_new", false}, {"tuplesize", true}};

  for (auto& d : decorators) {
    if (d->is_id("deduce")) {
      attributes.custom_attr.insert("deduce");
    } else if (d->is_id("__notuple__")) {
      attributes.custom_attr.insert("__notuple__");
    } else if (auto c = d->get_call()) {
      if (c->expr->is_id(Attr::Tuple)) {
        attributes.set(Attr::Tuple);
        for (auto& m : tuple_magics)
          m.second = true;
      } else if (!c->expr->is_id("dataclass")) {
        Err(Error::CLASS_BAD_DECORATOR, c->expr);
      } else if (attributes.has(Attr::Tuple)) {
        Err(Error::CLASS_CONFLICT_DECORATOR, c, "dataclass", Attr::Tuple);
      }
      for (auto& a : c->args) {
        auto b = dynamic_cast<BoolExpr*>(a.value.get());
        if (!b)
          Err(Error::CLASS_NONSTATIC_DECORATOR, a);
        char val = char(b->value);
        if (a.name == "init") {
          tuple_magics["new"] = val;
        } else if (a.name == "repr") {
          tuple_magics["repr"] = val;
        } else if (a.name == "eq") {
          tuple_magics["eq"] = tuple_magics["ne"] = val;
        } else if (a.name == "order") {
          tuple_magics["lt"] = tuple_magics["le"] = tuple_magics["gt"] =
              tuple_magics["ge"] = val;
        } else if (a.name == "hash") {
          tuple_magics["hash"] = val;
        } else if (a.name == "pickle") {
          tuple_magics["pickle"] = tuple_magics["unpickle"] = val;
        } else if (a.name == "python") {
          tuple_magics["to_py"] = tuple_magics["from_py"] = val;
        } else if (a.name == "gpu") {
          tuple_magics["to_gpu"] = tuple_magics["from_gpu"] =
              tuple_magics["from_gpu_new"] = val;
        } else if (a.name == "container") {
          tuple_magics["iter"] = tuple_magics["getitem"] = val;
        } else {
          Err(Error::CLASS_BAD_DECORATOR_ARG, a);
        }
      }
    } else if (d->is_id(Attr::Tuple)) {
      if (attributes.has(Attr::Tuple))
        Err(Error::CLASS_MULTIPLE_DECORATORS, d, Attr::Tuple);
      attributes.set(Attr::Tuple);
      for (auto& m : tuple_magics) {
        m.second = true;
      }
    } else if (d->is_id(Attr::Extend)) {
      attributes.set(Attr::Extend);
      if (decorators.size() != 1)
        Err(Error::CLASS_SINGLE_DECORATOR, decorators[decorators[0] == d],
            Attr::Extend);
    } else if (d->is_id(Attr::Internal)) {
      attributes.set(Attr::Internal);
    } else {
      Err(Error::CLASS_BAD_DECORATOR, d);
    }
  }
  if (attributes.has("deduce"))
    tuple_magics["new"] = false;
  if (!attributes.has(Attr::Tuple)) {
    tuple_magics["init"] = tuple_magics["new"];
    tuple_magics["new"] = tuple_magics["raw"] = true;
    tuple_magics["len"] = false;
  }
  tuple_magics["dict"] = true;
  // Internal classes do not get any auto-generated members.
  attributes.magics.clear();
  if (!attributes.has(Attr::Internal)) {
    for (auto& m : tuple_magics)
      if (m.second)
        attributes.magics.insert(m.first);
  }

  validate();
}
auto ClassStmt::is_class_var(const Param& p) -> bool {
  if (!p.default_value)
    return false;
  if (!p.type)
    return true;
  if (auto i = p.type->get_index())
    return i->expr->is_id("ClassVar");
  return false;
}
auto ClassStmt::get_doc_str() -> std::string {
  if (auto s = suite->first_in_block()) {
    if (auto e = s->get_expr()) {
      if (auto ss = e->expr->get_string())
        return ss->get_value();
    }
  }
  return "";
}

YieldFromStmt::YieldFromStmt(ExprPtr expr) : expr(std::move(expr)) {}
YieldFromStmt::YieldFromStmt(const YieldFromStmt& stmt)
    : Stmt(stmt), expr(::Pud::clone(stmt.expr)) {}
auto YieldFromStmt::to_string(int) const -> std::string {
  return fmt::format("(yield-from {})", expr->to_string());
}
auto YieldFromStmt::clone() const -> StmtPtr {
  return std::make_shared<YieldFromStmt>(*this);
}
void YieldFromStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

WithStmt::WithStmt(std::vector<ExprPtr> items, std::vector<std::string> vars,
                   StmtPtr suite)
    : items(std::move(items)), vars(std::move(vars)), suite(std::move(suite)) {
  assert(this->items.size() == this->vars.size() && "vector size mismatch");
}
WithStmt::WithStmt(std::vector<std::pair<ExprPtr, ExprPtr>> item_var_pairs,
                   StmtPtr suite)
    : suite(std::move(suite)) {
  for (auto& i : item_var_pairs) {
    items.push_back(std::move(i.first));
    if (i.second) {
      if (!i.second->get_id())
        throw;
      vars.push_back(i.second->get_id()->value);
    } else {
      vars.emplace_back();
    }
  }
}
WithStmt::WithStmt(const WithStmt& stmt)
    : Stmt(stmt),
      items(::Pud::clone(stmt.items)),
      vars(stmt.vars),
      suite(::Pud::clone(stmt.suite)) {}
auto WithStmt::to_string(int indent) const -> std::string {
  std::string pad =
      indent > 0 ? ("\n" + std::string(indent + INDENT_SIZE, ' ')) : " ";
  std::vector<std::string> as;
  as.reserve(items.size());
  for (int i = 0; i < items.size(); i++) {
    as.push_back(!vars[i].empty() ? fmt::format("({} #:var '{})",
                                                items[i]->to_string(), vars[i])
                                  : items[i]->to_string());
  }
  return fmt::format("(with ({}){}{})", join(as, " "), pad,
                     suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1));
}
auto WithStmt::clone() const -> StmtPtr {
  return std::make_shared<WithStmt>(*this);
}
void WithStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

CustomStmt::CustomStmt(std::string keyword, ExprPtr expr, StmtPtr suite)
    : keyword(std::move(keyword)),
      expr(std::move(expr)),
      suite(std::move(suite)) {}
CustomStmt::CustomStmt(const CustomStmt& stmt)
    : Stmt(stmt),
      keyword(stmt.keyword),
      expr(::Pud::clone(stmt.expr)),
      suite(::Pud::clone(stmt.suite)) {}
auto CustomStmt::to_string(int indent) const -> std::string {
  std::string pad =
      indent > 0 ? ("\n" + std::string(indent + INDENT_SIZE, ' ')) : " ";
  return fmt::format(
      "(custom-{} {}{}{})", keyword,
      expr ? fmt::format(" #:expr {}", expr->to_string()) : "", pad,
      suite ? suite->to_string(indent >= 0 ? indent + INDENT_SIZE : -1) : "");
}
auto CustomStmt::clone() const -> StmtPtr {
  return std::make_shared<CustomStmt>(*this);
}
void CustomStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

AssignMemberStmt::AssignMemberStmt(ExprPtr lhs, std::string member, ExprPtr rhs)
    : lhs(std::move(lhs)), member(std::move(member)), rhs(std::move(rhs)) {}
AssignMemberStmt::AssignMemberStmt(const AssignMemberStmt& stmt)
    : Stmt(stmt),
      lhs(::Pud::clone(stmt.lhs)),
      member(stmt.member),
      rhs(::Pud::clone(stmt.rhs)) {}
auto AssignMemberStmt::to_string(int) const -> std::string {
  return fmt::format("(assign-member {} {} {})", lhs->to_string(), member,
                     rhs->to_string());
}
auto AssignMemberStmt::clone() const -> StmtPtr {
  return std::make_shared<AssignMemberStmt>(*this);
}
void AssignMemberStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

CommentStmt::CommentStmt(std::string comment) : comment(std::move(comment)) {}
auto CommentStmt::to_string(int) const -> std::string {
  return fmt::format("(comment \"{}\")", comment);
}
auto CommentStmt::clone() const -> StmtPtr {
  return std::make_shared<CommentStmt>(*this);
}
void CommentStmt::accept(ASTVisitor& visitor) { visitor.visit(this); }

}  // namespace Pud::AST