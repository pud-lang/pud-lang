#include "Pud/AST/ASTVisitor.h"

namespace Pud::AST {

void ASTVisitor::default_visit(Expr* expr) {}
void ASTVisitor::default_visit(Stmt* stmt) {}

void ASTVisitor::visit(NoneExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(BoolExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(IntExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(FloatExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(StringExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(IdExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(StarExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(KeywordStarExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(TupleExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(ListExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(SetExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(DictExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(GeneratorExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(DictGeneratorExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(IfExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(UnaryExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(BinaryExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(ChainBinaryExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(PipeExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(IndexExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(CallExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(DotExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(SliceExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(EllipsisExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(LambdaExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(YieldExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(AssignExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(RangeExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(InstantiateExpr* expr) { default_visit(expr); }
void ASTVisitor::visit(StmtExpr* expr) { default_visit(expr); }

void ASTVisitor::visit(SuiteStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(BreakStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(ContinueStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(ExprStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(AssignStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(AssignMemberStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(DelStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(PrintStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(ReturnStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(YieldStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(AssertStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(WhileStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(ForStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(IfStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(MatchStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(ImportStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(TryStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(GlobalStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(ThrowStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(FunctionStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(ClassStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(YieldFromStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(WithStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(CustomStmt* stmt) { default_visit(stmt); }
void ASTVisitor::visit(CommentStmt* stmt) { default_visit(stmt); }

}  // namespace Pud::AST