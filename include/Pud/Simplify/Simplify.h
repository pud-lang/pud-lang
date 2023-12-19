#ifndef PUD_SIMPLIFY_SIMPLIFY_H
#define PUD_SIMPLIFY_SIMPLIFY_H

#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/ASTVisitor.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Context.h"

namespace Pud::AST {
// 简化过程
// 在这个阶段，以下步骤被执行：
//
// - 所有的导入都被展平，结果是一个单独的、自包含的（且相当大的）AST。
// - 所有的标识符都被规范化（不同的对象不共享相同的名称）。
// - 生成变异类（如 Tuple）。
// - 任何可以被简单表达为一组“更简单”节点类型的AST节点都被简化。
//   如果转换需要类型信息，则在类型检查期间完成。
//
// 特别注意
// 这个阶段修改了提供的AST。如果你需要保持AST的完整性，请在简化之前克隆它。

// SimplifyVisitor 实现了抽象语法树（AST）的初步简化转换过程。
// 这个类是编译器或解释器在处理代码时的一个关键组成部分，用于对代码
// 进行初步的解析和简化处理。
class SimplifyVisitor : public CallbackASTVisitor<ExprPtr, StmtPtr> {
  // 共享的简化上下文，用于存储和管理编译过程中的状态和信息。
  std::shared_ptr<SimplifyContext> ctx;
  // 包含定义语句的前导代码，这些语句在所有模块的所有访问者中共享，
  // 在简化的语句之前执行。
  std::shared_ptr<std::vector<StmtPtr>> preamble;
  // 在当前语句之前要添加的语句集合。
  std::shared_ptr<std::vector<StmtPtr>> prepend_stmts;

  // 存储每个新表达式的结果
  ExprPtr result_expr;
  // 存储每个新语句的结果
  StmtPtr result_stmt;

 public:
  // 用于应用简化转换过程到给定的AST节点。
  static auto apply(
      Cache* cache, const StmtPtr& node, const std::string& file,
      const std::unordered_map<std::string, std::string>& defines = {},
      const std::unordered_map<std::string, std::string>& early_defines = {},
      bool barebones = false) -> StmtPtr;
  static auto apply(const std::shared_ptr<SimplifyContext>& cache,
                    const StmtPtr& node, const std::string& file,
                    int at_age = -1) -> StmtPtr;

 public:
  explicit SimplifyVisitor(
      std::shared_ptr<SimplifyContext> ctx,
      std::shared_ptr<std::vector<StmtPtr>> preamble,
      const std::shared_ptr<std::vector<StmtPtr>>& stmts = nullptr);

 public:
  // 转换方法。
  auto transform(ExprPtr& expr) -> ExprPtr override;
  auto transform(const ExprPtr& expr) -> ExprPtr override {
    auto e = expr;
    return transform(e);
  }
  auto transform(ExprPtr& expr, bool allow_types) -> ExprPtr;
  auto transform(ExprPtr&& expr, bool allow_types) -> ExprPtr {
    return transform(expr, allow_types);
  }
  auto transform_type(ExprPtr& expr, bool allow_type_of = true) -> ExprPtr;
  auto transform_type(ExprPtr&& expr, bool allow_type_of = true) -> ExprPtr {
    return transform_type(expr, allow_type_of);
  }
  auto transform(StmtPtr& stmt) -> StmtPtr override;
  auto transform(const StmtPtr& stmt) -> StmtPtr override {
    auto s = stmt;
    return transform(s);
  }
  auto transform_conditional_scope(StmtPtr& stmt) -> StmtPtr;

 private:
  /* Basic type expressions (basic.cpp) */
  void visit(IntExpr*) override;
  auto transform_int(IntExpr*) -> ExprPtr;
  void visit(FloatExpr*) override;
  auto transform_float(FloatExpr*) -> ExprPtr;
  void visit(StringExpr*) override;
  auto transform_fstring(const std::string&) -> ExprPtr;

  /* Identifier access expressions (access.cpp) */
  void visit(IdExpr*) override;
  auto check_capture(const SimplifyContext::Item&) -> bool;
  void visit(DotExpr*) override;
  auto get_import(const std::vector<std::string>&)
      -> std::pair<size_t, SimplifyContext::Item>;

  /* Collection and comprehension expressions (collections.cpp) */
  void visit(TupleExpr*) override;
  void visit(ListExpr*) override;
  void visit(SetExpr*) override;
  void visit(DictExpr*) override;
  void visit(GeneratorExpr*) override;
  void visit(DictGeneratorExpr*) override;
  auto transform_generator_body(const std::vector<GeneratorBody>&, SuiteStmt*&)
      -> StmtPtr;

  /* Conditional expression and statements (cond.cpp) */
  void visit(IfExpr*) override;
  void visit(IfStmt*) override;
  void visit(MatchStmt*) override;
  auto transform_pattern(const ExprPtr&, ExprPtr, StmtPtr) -> StmtPtr;

  /* Operators (op.cpp) */
  void visit(UnaryExpr*) override;
  void visit(BinaryExpr*) override;
  void visit(ChainBinaryExpr*) override;
  void visit(IndexExpr*) override;
  void visit(InstantiateExpr*) override;

  /* Calls (call.cpp) */
  void visit(PrintStmt*) override;
  void visit(CallExpr*) override;
  auto transform_special_call(const ExprPtr&, const std::vector<CallExpr::Arg>&)
      -> ExprPtr;
  auto transform_tuple_generator(const std::vector<CallExpr::Arg>&) -> ExprPtr;
  auto transform_named_tuple(const std::vector<CallExpr::Arg>&) -> ExprPtr;
  auto transform_functools_partial(std::vector<CallExpr::Arg>) -> ExprPtr;

  /* Assignments (assign.cpp) */
  void visit(AssignExpr*) override;
  void visit(AssignStmt*) override;
  auto transform_assignment(ExprPtr, ExprPtr, ExprPtr = nullptr, bool = false)
      -> StmtPtr;
  void unpack_assignments(const ExprPtr&, ExprPtr, std::vector<StmtPtr>&);
  void visit(DelStmt*) override;

  /* Imports (import.cpp) */
  void visit(ImportStmt*) override;
  auto transform_special_import(ImportStmt*) -> StmtPtr;
  auto get_import_path(Expr*, size_t = 0) -> std::vector<std::string>;
  auto transform_c_import(const std::string&, const std::vector<Param>&,
                          const Expr*, const std::string&) -> StmtPtr;
  auto transform_cvar_import(const std::string&, const Expr*,
                             const std::string&) -> StmtPtr;
  auto transform_cdll_import(const Expr*, const std::string&,
                             const std::vector<Param>&, const Expr*,
                             const std::string&, bool) -> StmtPtr;
  auto transform_python_import(Expr*, const std::vector<Param>&, Expr*,
                               const std::string&) -> StmtPtr;
  auto transform_new_import(const ImportFile&) -> StmtPtr;

  /* Loops (loops.cpp) */
  void visit(ContinueStmt*) override;
  void visit(BreakStmt*) override;
  void visit(WhileStmt*) override;
  void visit(ForStmt*) override;
  auto transform_for_decorator(const ExprPtr&) -> ExprPtr;

  /* Errors and exceptions (error.cpp) */
  void visit(AssertStmt*) override;
  void visit(TryStmt*) override;
  void visit(ThrowStmt*) override;
  void visit(WithStmt*) override;

  /* Functions (function.cpp) */
  void visit(YieldExpr*) override;
  void visit(LambdaExpr*) override;
  void visit(GlobalStmt*) override;
  void visit(ReturnStmt*) override;
  void visit(YieldStmt*) override;
  void visit(YieldFromStmt*) override;
  void visit(FunctionStmt*) override;
  auto make_anon_fn(std::vector<StmtPtr>, const std::vector<std::string>& = {})
      -> ExprPtr;
  auto transform_python_definition(const std::string&,
                                   const std::vector<Param>&, const Expr*,
                                   Stmt*) -> StmtPtr;
  auto transform_llvm_definition(Stmt*) -> StmtPtr;
  auto get_decorator(const ExprPtr&) -> std::pair<bool, std::string>;

  /* Classes (class.cpp) */
  void visit(ClassStmt*) override;
  auto parse_base_classes(std::vector<ExprPtr>&, std::vector<Param>&,
                          const Attr&, const std::string&,
                          const ExprPtr& = nullptr) -> std::vector<ClassStmt*>;
  auto auto_deduce_members(ClassStmt*, std::vector<Param>&)
      -> std::pair<StmtPtr, FunctionStmt*>;
  auto get_class_methods(const StmtPtr& s) -> std::vector<StmtPtr>;
  void transform_nested_classes(ClassStmt*, std::vector<StmtPtr>&,
                                std::vector<StmtPtr>&, std::vector<StmtPtr>&);
  auto codegen_magic(const std::string&, const ExprPtr&,
                     const std::vector<Param>&, bool) -> StmtPtr;

  /* The rest (simplify.cpp) */
  void visit(StmtExpr*) override;
  void visit(StarExpr*) override;
  void visit(KeywordStarExpr* expr) override;
  void visit(RangeExpr*) override;
  void visit(SliceExpr*) override;
  void visit(SuiteStmt*) override;
  void visit(ExprStmt*) override;
  void visit(CustomStmt*) override;
};

}  // namespace Pud::AST

#endif  // PUD_SIMPLIFY_SIMPLIFY_H