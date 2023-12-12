#ifndef PUD_FORMAT_FORMAT_H
#define PUD_FORMAT_FORMAT_H

#include <ostream>
#include <string>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/ASTVisitor.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"

namespace Pud::AST {

class FormatVisitor : public CallbackASTVisitor<std::string, std::string> {
  std::string result;
  std::string space;
  bool renderType, renderHTML;
  int indent;

  std::string header, footer, nl;
  std::string type_start, type_end;
  std::string node_start, node_end;
  std::string expr_start, expr_end;
  std::string comment_start, comment_end;
  std::string keyword_start, keyword_end;

  Cache* cache;

 private:
  template <typename T, typename... Ts>
  auto render_expr(T&& t, Ts&&... args) -> std::string {
    std::string s;
    return fmt::format("{}{}{}{}{}{}", expr_start, s, node_start,
                       fmt::format(args...), node_end, expr_end);
  }
  template <typename... Ts>
  auto render_comment(Ts&&... args) -> std::string {
    return fmt::format("{}{}{}", comment_start, fmt::format(args...),
                       comment_end);
  }
  auto pad(int indent = 0) const -> std::string;
  auto newline() const -> std::string;
  auto keyword(const std::string& s) const -> std::string;

 public:
  FormatVisitor(bool html, Cache* cache = nullptr);
  auto transform(const ExprPtr& e) -> std::string override;
  auto transform(const Expr* expr) -> std::string;
  auto transform(const StmtPtr& stmt) -> std::string override;
  auto transform(Stmt* stmt, int indent) -> std::string;

  template <typename T>
  static auto apply(const T& stmt, Cache* cache = nullptr, bool html = false,
                    bool init = false) -> std::string {
    auto t = FormatVisitor(html, cache);
    return fmt::format("{}{}{}", t.header, t.transform(stmt), t.footer);
  }

  void default_visit(Expr* e) override { error("cannot format {}", *e); }
  void default_visit(Stmt* e) override { error("cannot format {}", *e); }

 public:
  void visit(NoneExpr*) override;
  void visit(BoolExpr*) override;
  void visit(IntExpr*) override;
  void visit(FloatExpr*) override;
  void visit(StringExpr*) override;
  void visit(IdExpr*) override;
  void visit(StarExpr*) override;
  void visit(KeywordStarExpr*) override;
  void visit(TupleExpr*) override;
  void visit(ListExpr*) override;
  void visit(SetExpr*) override;
  void visit(DictExpr*) override;
  void visit(GeneratorExpr*) override;
  void visit(DictGeneratorExpr*) override;
  void visit(InstantiateExpr* expr) override;
  void visit(IfExpr*) override;
  void visit(UnaryExpr*) override;
  void visit(BinaryExpr*) override;
  void visit(PipeExpr*) override;
  void visit(IndexExpr*) override;
  void visit(CallExpr*) override;
  void visit(DotExpr*) override;
  void visit(SliceExpr*) override;
  void visit(EllipsisExpr*) override;
  void visit(LambdaExpr*) override;
  void visit(YieldExpr*) override;
  void visit(StmtExpr* expr) override;
  void visit(AssignExpr* expr) override;

  void visit(SuiteStmt*) override;
  void visit(BreakStmt*) override;
  void visit(ContinueStmt*) override;
  void visit(ExprStmt*) override;
  void visit(AssignStmt*) override;
  void visit(AssignMemberStmt*) override;
  void visit(DelStmt*) override;
  void visit(PrintStmt*) override;
  void visit(ReturnStmt*) override;
  void visit(YieldStmt*) override;
  void visit(AssertStmt*) override;
  void visit(WhileStmt*) override;
  void visit(ForStmt*) override;
  void visit(IfStmt*) override;
  void visit(MatchStmt*) override;
  void visit(ImportStmt*) override;
  void visit(TryStmt*) override;
  void visit(GlobalStmt*) override;
  void visit(ThrowStmt*) override;
  void visit(FunctionStmt*) override;
  void visit(ClassStmt*) override;
  void visit(YieldFromStmt*) override;
  void visit(WithStmt*) override;

 public:
  friend auto operator<<(std::ostream& out, const FormatVisitor& c)
      -> std::ostream& {
    return out << c.result;
  }

  using CallbackASTVisitor<std::string, std::string>::transform;
  template <typename T>
  auto transform(const std::vector<T>& ts) -> std::string {
    std::vector<std::string> r;
    for (auto& e : ts) {
      r.push_back(transform(e));
    }
    return fmt::format("{}", fmt::join(r, ", "));
  }
};

}  // namespace Pud::AST

#endif  // PUD_FORMAT_FORMAT_H