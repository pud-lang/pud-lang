#ifndef PUD_TRANSLATE_TRANSLATE_H
#define PUD_TRANSLATE_TRANSLATE_H

#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/ASTVisitor.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/IR/IR.h"
#include "Pud/Translate/Context.h"

namespace Pud::AST {

class TranslateVisitor : public CallbackASTVisitor<IR::Value*, IR::Value*> {
  std::shared_ptr<TranslateContext> ctx;
  IR::Value* result;

 public:
  explicit TranslateVisitor(std::shared_ptr<TranslateContext> ctx);
  static auto apply(Cache* cache, const StmtPtr& stmts) -> Pud::IR::Func*;

  auto transform(const ExprPtr& expr) -> IR::Value* override;
  auto transform(const StmtPtr& stmt) -> IR::Value* override;

 private:
  void default_visit(Expr* expr) override;
  void default_visit(Stmt* expr) override;

 public:
  void visit(NoneExpr*) override;
  void visit(BoolExpr*) override;
  void visit(IntExpr*) override;
  void visit(FloatExpr*) override;
  void visit(StringExpr*) override;
  void visit(IdExpr*) override;
  void visit(IfExpr*) override;
  void visit(CallExpr*) override;
  void visit(DotExpr*) override;
  void visit(YieldExpr*) override;
  void visit(StmtExpr*) override;
  void visit(PipeExpr*) override;

  void visit(SuiteStmt*) override;
  void visit(BreakStmt*) override;
  void visit(ContinueStmt*) override;
  void visit(ExprStmt*) override;
  void visit(AssignStmt*) override;
  void visit(AssignMemberStmt*) override;
  void visit(ReturnStmt*) override;
  void visit(YieldStmt*) override;
  void visit(WhileStmt*) override;
  void visit(ForStmt*) override;
  void visit(IfStmt*) override;
  void visit(TryStmt*) override;
  void visit(ThrowStmt*) override;
  void visit(FunctionStmt*) override;
  void visit(ClassStmt*) override;
  void visit(CommentStmt*) override {}

 private:
  auto get_type(const Type::TypePtr& t) -> IR::Types::Type*;

  void transform_function_realizations(const std::string& name, bool is_llvm);
  void transform_function(Type::FuncType* type, FunctionStmt* ast,
                          IR::Func* func);
  void transform_llvm_function(Type::FuncType* type, FunctionStmt* ast,
                               IR::Func* func);

  template <typename ValueType, typename... Args>
  auto make(Args&&... args) -> ValueType* {
    auto* ret = ctx->get_module()->N<ValueType>(std::forward<Args>(args)...);
    return ret;
  }
};

}  // namespace Pud::AST

#endif  // PUD_TRANSLATE_TRANSLATE_H