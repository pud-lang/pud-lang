#ifndef PUD_AST_AST_VISITOR_H
#define PUD_AST_AST_VISITOR_H

#include "Pud/AST/Expr.h"
#include "Pud/AST/Stmt.h"

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

}  // namespace Pud::AST

#endif  // PUD_AST_AST_VISITOR_H