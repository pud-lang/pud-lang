#ifndef PUD_AST_AST_VISITOR_H
#define PUD_AST_AST_VISITOR_H

#include "Pud/AST/Expr.h"
#include "Pud/AST/Stmt.h"
#include "Pud/Common/Error.h"

namespace Pud::AST {

struct ASTVisitor {
 protected:
  virtual void default_visit(Expr* expr);
  virtual void default_visit(Stmt* stmt);

 public:
  virtual void visit(NoneExpr*);
  virtual void visit(BoolExpr*);
  virtual void visit(IntExpr*);
  virtual void visit(FloatExpr*);
  virtual void visit(StringExpr*);
  virtual void visit(IdExpr*);
  virtual void visit(StarExpr*);
  virtual void visit(KeywordStarExpr*);
  virtual void visit(TupleExpr*);
  virtual void visit(ListExpr*);
  virtual void visit(SetExpr*);
  virtual void visit(DictExpr*);
  virtual void visit(GeneratorExpr*);
  virtual void visit(DictGeneratorExpr*);
  virtual void visit(IfExpr*);
  virtual void visit(UnaryExpr*);
  virtual void visit(BinaryExpr*);
  virtual void visit(ChainBinaryExpr*);
  virtual void visit(PipeExpr*);
  virtual void visit(IndexExpr*);
  virtual void visit(CallExpr*);
  virtual void visit(DotExpr*);
  virtual void visit(SliceExpr*);
  virtual void visit(EllipsisExpr*);
  virtual void visit(LambdaExpr*);
  virtual void visit(YieldExpr*);
  virtual void visit(AssignExpr*);
  virtual void visit(RangeExpr*);
  virtual void visit(InstantiateExpr*);
  virtual void visit(StmtExpr*);

  virtual void visit(AssignMemberStmt*);
  virtual void visit(SuiteStmt*);
  virtual void visit(BreakStmt*);
  virtual void visit(ContinueStmt*);
  virtual void visit(ExprStmt*);
  virtual void visit(AssignStmt*);
  virtual void visit(DelStmt*);
  virtual void visit(PrintStmt*);
  virtual void visit(ReturnStmt*);
  virtual void visit(YieldStmt*);
  virtual void visit(AssertStmt*);
  virtual void visit(WhileStmt*);
  virtual void visit(ForStmt*);
  virtual void visit(IfStmt*);
  virtual void visit(MatchStmt*);
  virtual void visit(ImportStmt*);
  virtual void visit(TryStmt*);
  virtual void visit(GlobalStmt*);
  virtual void visit(ThrowStmt*);
  virtual void visit(FunctionStmt*);
  virtual void visit(ClassStmt*);
  virtual void visit(YieldFromStmt*);
  virtual void visit(WithStmt*);
  virtual void visit(CustomStmt*);
  virtual void visit(CommentStmt*);
};

// CallbackASTVisitor 是 ASTVisitor 的一个具体实现，它扩展了基类并提供
// 了将节点源位置（SourceObject）存储为状态的能力。此外，它还定义了
// transform 方法，这些方法在访问节点时执行，并返回适当的转换结果。
// 由于不同节点类型可能返回不同类型的结果，CallbackASTVisitor 是泛型的，
// 可以处理不同返回类型。用于遍历和操作抽象语法树（AST）。
template <typename TE, typename TS>
struct CallbackASTVisitor : public ASTVisitor, public SourceObject {
  // transform 方法：用于对表达式（Expr）和语句（Stmt）节点进行转换。
  // TE 和 TS 是泛型参数，分别表示转换表达式和语句后的返回类型。
  virtual auto transform(const std::shared_ptr<Expr>& expr) -> TE = 0;
  virtual auto transform(std::shared_ptr<Expr>& expr) -> TE {
    return transform(static_cast<const std::shared_ptr<Expr>&>(expr));
  }
  virtual auto transform(const std::shared_ptr<Stmt>& stmt) -> TS = 0;
  virtual auto transform(std::shared_ptr<Stmt>& stmt) -> TS {
    return transform(static_cast<const std::shared_ptr<Stmt>&>(stmt));
  }

  // transforms a vector of nodes.
  template <typename T>
  auto transform(const std::vector<T>& ts) {
    std::vector<T> r;
    for (auto& e : ts) {
      r.push_back(transform(e));
    }
    return r;
  }

  // N 方法：用于创建节点的clone实例。用于克隆或构造新的AST节点。
  template <typename Tn>
  auto N(const Tn& ptr) {
    return std::make_shared<Tn>(ptr);
  }
  template <typename Tn, typename... Ts>
  auto N(SourceInfo s, Ts&&... args) {
    auto t = std::make_shared<Tn>(std::forward<Ts>(args)...);
    t->set_source_info(s);
    return t;
  }
  template <typename Tn, typename... Ts>
  auto N(Ts&&... args) {
    auto t = std::make_shared<Tn>(std::forward<Ts>(args)...);
    t->set_source_info(get_source_info());
    return t;
  }
  template <typename Tn, typename... Ts>
  auto NT(Ts&&... args) {
    auto t = std::make_shared<Tn>(std::forward<Ts>(args)...);
    t->set_source_info(get_source_info());
    t->mark_type();
    return t;
  }

  template <typename... TArgs>
  void error(const char* format, TArgs&&... args) {
    raise_error(-1, get_source_info(), fmt::format(format, args...).c_str());
  }

  template <typename T, typename... TArgs>
  void error(const T& p, const char* format, TArgs&&... args) {
    raise_error(-1, p->get_source_info(), fmt::format(format, args...).c_str());
  }

  template <typename T, typename... TArgs>
  void internal_error(const char* format, TArgs&&... args) {
    throw ParserException(fmt::format(
        "INTERNAL: {}", fmt::format(format, args...), get_source_info()));
  }

 public:
  void visit(NoneExpr* expr) override {}
  void visit(BoolExpr* expr) override {}
  void visit(IntExpr* expr) override {}
  void visit(FloatExpr* expr) override {}
  void visit(StringExpr* expr) override {}
  void visit(IdExpr* expr) override {}
  void visit(StarExpr* expr) override { transform(expr->what); }
  void visit(KeywordStarExpr* expr) override { transform(expr->what); }
  void visit(TupleExpr* expr) override {
    for (auto& i : expr->items) {
      transform(i);
    }
  }
  void visit(ListExpr* expr) override {
    for (auto& i : expr->items) {
      transform(i);
    }
  }
  void visit(SetExpr* expr) override {
    for (auto& i : expr->items) {
      transform(i);
    }
  }
  void visit(DictExpr* expr) override {
    for (auto& i : expr->items) {
      transform(i);
    }
  }
  void visit(GeneratorExpr* expr) override {
    transform(expr->expr);
    for (auto& l : expr->loops) {
      transform(l.vars);
      transform(l.gen);
      for (auto& c : l.conds) {
        transform(c);
      }
    }
  }
  void visit(DictGeneratorExpr* expr) override {
    transform(expr->key);
    transform(expr->expr);
    for (auto& l : expr->loops) {
      transform(l.vars);
      transform(l.gen);
      for (auto& c : l.conds) {
        transform(c);
      }
    }
  }
  void visit(IfExpr* expr) override {
    transform(expr->cond);
    transform(expr->ifexpr);
    transform(expr->elsexpr);
  }
  void visit(UnaryExpr* expr) override { transform(expr->expr); }
  void visit(BinaryExpr* expr) override {
    transform(expr->lexpr);
    transform(expr->rexpr);
  }
  void visit(ChainBinaryExpr* expr) override {
    for (auto& e : expr->exprs) {
      transform(e.second);
    }
  }
  void visit(PipeExpr* expr) override {
    for (auto& e : expr->items) {
      transform(e.expr);
    }
  }
  void visit(IndexExpr* expr) override {
    transform(expr->expr);
    transform(expr->index);
  }
  void visit(CallExpr* expr) override {
    transform(expr->expr);
    for (auto& a : expr->args) {
      transform(a.value);
    }
  }
  void visit(DotExpr* expr) override { transform(expr->expr); }
  void visit(SliceExpr* expr) override {
    transform(expr->start);
    transform(expr->stop);
    transform(expr->step);
  }
  void visit(EllipsisExpr* expr) override {}
  void visit(LambdaExpr* expr) override { transform(expr->expr); }
  void visit(YieldExpr* expr) override {}
  void visit(AssignExpr* expr) override {
    transform(expr->var);
    transform(expr->expr);
  }
  void visit(RangeExpr* expr) override {
    transform(expr->start);
    transform(expr->stop);
  }
  void visit(InstantiateExpr* expr) override {
    transform(expr->type_expr);
    for (auto& e : expr->type_params) {
      transform(e);
    }
  }
  void visit(StmtExpr* expr) override {
    for (auto& s : expr->stmts) {
      transform(s);
    }
    transform(expr->expr);
  }
  void visit(SuiteStmt* stmt) override {
    for (auto& s : stmt->stmts) {
      transform(s);
    }
  }
  void visit(BreakStmt* stmt) override {}
  void visit(ContinueStmt* stmt) override {}
  void visit(ExprStmt* stmt) override { transform(stmt->expr); }
  void visit(AssignStmt* stmt) override {
    transform(stmt->lhs);
    transform(stmt->rhs);
    transform(stmt->type);
  }
  void visit(AssignMemberStmt* stmt) override {
    transform(stmt->lhs);
    transform(stmt->rhs);
  }
  void visit(DelStmt* stmt) override { transform(stmt->expr); }
  void visit(PrintStmt* stmt) override {
    for (auto& e : stmt->items) {
      transform(e);
    }
  }
  void visit(ReturnStmt* stmt) override { transform(stmt->expr); }
  void visit(YieldStmt* stmt) override { transform(stmt->expr); }
  void visit(AssertStmt* stmt) override {
    transform(stmt->expr);
    transform(stmt->message);
  }
  void visit(WhileStmt* stmt) override {
    transform(stmt->cond);
    transform(stmt->suite);
    transform(stmt->else_suite);
  }
  void visit(ForStmt* stmt) override {
    transform(stmt->var);
    transform(stmt->iter);
    transform(stmt->suite);
    transform(stmt->else_suite);
    transform(stmt->decorator);
    for (auto& a : stmt->omp_args) {
      transform(a.value);
    }
  }
  void visit(IfStmt* stmt) override {
    transform(stmt->cond);
    transform(stmt->if_suite);
    transform(stmt->else_suite);
  }
  void visit(MatchStmt* stmt) override {
    transform(stmt->what);
    for (auto& m : stmt->cases) {
      transform(m.pattern);
      transform(m.guard);
      transform(m.suite);
    }
  }
  void visit(ImportStmt* stmt) override {
    transform(stmt->from);
    transform(stmt->what);
    for (auto& a : stmt->args) {
      transform(a.type);
      transform(a.default_value);
    }
    transform(stmt->ret);
  }
  void visit(TryStmt* stmt) override {
    transform(stmt->suite);
    for (auto& a : stmt->catches) {
      transform(a.exc);
      transform(a.suite);
    }
    transform(stmt->finally);
  }
  void visit(GlobalStmt* stmt) override {}
  void visit(ThrowStmt* stmt) override { transform(stmt->expr); }
  void visit(FunctionStmt* stmt) override {
    transform(stmt->ret);
    for (auto& a : stmt->args) {
      transform(a.type);
      transform(a.default_value);
    }
    transform(stmt->suite);
    for (auto& d : stmt->decorators) {
      transform(d);
    }
  }
  void visit(ClassStmt* stmt) override {
    for (auto& a : stmt->args) {
      transform(a.type);
      transform(a.default_value);
    }
    transform(stmt->suite);
    for (auto& d : stmt->decorators) {
      transform(d);
    }
    for (auto& d : stmt->base_classes) {
      transform(d);
    }
    for (auto& d : stmt->static_base_classes) {
      transform(d);
    }
  }
  void visit(YieldFromStmt* stmt) override { transform(stmt->expr); }
  void visit(WithStmt* stmt) override {
    for (auto& a : stmt->items) {
      transform(a);
    }
    transform(stmt->suite);
  }
  void visit(CustomStmt* stmt) override {
    transform(stmt->expr);
    transform(stmt->suite);
  }
};

}  // namespace Pud::AST

#endif  // PUD_AST_AST_VISITOR_H