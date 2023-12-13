#ifndef PUD_TYPECHECK_TYPECHECK_H
#define PUD_TYPECHECK_TYPECHECK_H

#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/ASTVisitor.h"
#include "Pud/Common/Common.h"
#include "Pud/Format/Format.h"
#include "Pud/TypeCheck/Context.h"

namespace Pud::AST {

// 用于推断表达式类型并执行类型引导转换的访问者类。
class TypecheckVisitor : public CallbackASTVisitor<ExprPtr, StmtPtr> {
  // 共享简化上下文的指针，用于在类型检查过程中跟踪标识符。
  std::shared_ptr<TypeContext> ctx;
  // 在当前语句之前插入的语句列表。
  std::shared_ptr<std::vector<StmtPtr>> prependStmts;

  // 用于存储每个新表达式和语句，稍后由 transform 方法返回。
  ExprPtr resultExpr;
  StmtPtr resultStmt;

 public:
  // 应用类型检查到给定的语句。
  static auto apply(Cache* cache, const StmtPtr& stmts) -> StmtPtr;

 public:
  // 接受类型上下文和要插入的语句列表。
  explicit TypecheckVisitor(
      std::shared_ptr<TypeContext> ctx,
      const std::shared_ptr<std::vector<StmtPtr>>& stmts = nullptr);

 public:
  // 转换表达式和语句。
  auto transform(ExprPtr& e) -> ExprPtr override;
  auto transform(const ExprPtr& expr) -> ExprPtr override {
    auto e = expr;
    return transform(e);
  }
  auto transform(StmtPtr& s) -> StmtPtr override;
  auto transform(const StmtPtr& stmt) -> StmtPtr override {
    auto s = stmt;
    return transform(s);
  }
  // 转换类型表达式。
  auto transform_type(ExprPtr& expr) -> ExprPtr;
  auto transform_type(const ExprPtr& expr) -> ExprPtr {
    auto e = expr;
    return transform_type(e);
  }

 private:
  // 定义对未特别处理的表达式和语句的默认行为。
  void default_visit(Expr* e) override;
  void default_visit(Stmt* s) override;

 private:
  void visit(NoneExpr*) override;
  void visit(BoolExpr*) override;
  void visit(IntExpr*) override;
  void visit(FloatExpr*) override;
  void visit(StringExpr*) override;

  /* Identifier access expressions (access.cpp) */
  void visit(IdExpr*) override;
  void visit(DotExpr*) override;
  auto transform_dot(DotExpr*, std::vector<CallExpr::Arg>* = nullptr)
      -> ExprPtr;
  auto get_class_member(DotExpr*, std::vector<CallExpr::Arg>*) -> ExprPtr;
  auto find_special_member(const std::string&) -> Pud::Type::TypePtr;
  auto get_best_overload(Expr*, std::vector<CallExpr::Arg>*)
      -> Pud::Type::FuncTypePtr;
  auto get_dispatch(const std::string&) -> Pud::Type::FuncTypePtr;

  /* Collection and comprehension expressions (collections.cpp) */
  void visit(TupleExpr*) override;
  void visit(ListExpr*) override;
  void visit(SetExpr*) override;
  void visit(DictExpr*) override;
  void visit(GeneratorExpr*) override;
  auto transform_comprehension(const std::string&, const std::string&,
                               std::vector<ExprPtr>&) -> ExprPtr;

  /* Conditional expression and statements (cond.cpp) */
  void visit(IfExpr*) override;
  void visit(IfStmt*) override;

  /* Operators (op.cpp) */
  void visit(UnaryExpr*) override;
  auto evaluate_static_unary(UnaryExpr*) -> ExprPtr;
  void visit(BinaryExpr*) override;
  auto evaluate_static_binary(BinaryExpr*) -> ExprPtr;
  auto transform_binary_simple(BinaryExpr*) -> ExprPtr;
  auto transform_binary_is(BinaryExpr*) -> ExprPtr;
  auto get_magic(const std::string&) -> std::pair<std::string, std::string>;
  auto transform_binary_inplace_magic(BinaryExpr*, bool) -> ExprPtr;
  auto transform_binary_magic(BinaryExpr*) -> ExprPtr;
  void visit(PipeExpr*) override;
  void visit(IndexExpr*) override;
  auto transform_static_tuple_index(const Pud::Type::ClassTypePtr&,
                                    const ExprPtr&, const ExprPtr&)
      -> std::pair<bool, ExprPtr>;
  auto translate_index(int64_t, int64_t, bool = false) -> int64_t;
  auto slice_adjust_indices(int64_t, int64_t*, int64_t*, int64_t) -> int64_t;
  void visit(InstantiateExpr*) override;
  void visit(SliceExpr*) override;

  /* Calls (call.cpp) */
  /// Holds partial call information for a CallExpr.
  struct PartialCallData {
    bool isPartial = false;        // true if the call is partial
    std::string var;               // set if calling a partial type itself
    std::vector<char> known = {};  // mask of known arguments
    ExprPtr args = nullptr,
            kwArgs = nullptr;  // partial *args/**kwargs expressions
  };
  void visit(StarExpr*) override;
  void visit(KeywordStarExpr*) override;
  void visit(EllipsisExpr*) override;
  void visit(CallExpr*) override;
  auto transform_call_args(std::vector<CallExpr::Arg>&) -> bool;
  auto get_callee_fn(CallExpr*, PartialCallData&)
      -> std::pair<Pud::Type::FuncTypePtr, ExprPtr>;
  auto call_reorder_arguments(Pud::Type::FuncTypePtr, CallExpr*,
                              PartialCallData&) -> ExprPtr;
  auto typecheck_call_args(const Pud::Type::FuncTypePtr&,
                           std::vector<CallExpr::Arg>&) -> bool;
  auto transform_special_call(CallExpr*) -> std::pair<bool, ExprPtr>;
  auto transform_super_f(CallExpr* expr) -> ExprPtr;
  auto transform_super() -> ExprPtr;
  auto transform_ptr(CallExpr* expr) -> ExprPtr;
  auto transform_array(CallExpr* expr) -> ExprPtr;
  auto transform_is_instance(CallExpr* expr) -> ExprPtr;
  auto transform_static_len(CallExpr* expr) -> ExprPtr;
  auto transform_has_attr(CallExpr* expr) -> ExprPtr;
  auto transform_get_attr(CallExpr* expr) -> ExprPtr;
  auto transform_set_attr(CallExpr* expr) -> ExprPtr;
  auto transform_compile_error(CallExpr* expr) -> ExprPtr;
  auto transform_tuple_fn(CallExpr* expr) -> ExprPtr;
  auto transform_type_fn(CallExpr* expr) -> ExprPtr;
  auto transform_realized_fn(CallExpr* expr) -> ExprPtr;
  auto transform_static_print_fn(CallExpr* expr) -> ExprPtr;
  auto transform_has_rtti_fn(CallExpr* expr) -> ExprPtr;
  auto transform_internal_static_fn(CallExpr* expr) -> std::pair<bool, ExprPtr>;
  auto get_super_types(const Pud::Type::ClassTypePtr& cls)
      -> std::vector<Pud::Type::ClassTypePtr>;
  void add_function_generics(const Pud::Type::FuncType* t);
  auto generate_partial_stub(const std::vector<char>& mask,
                             Pud::Type::FuncType* fn) -> std::string;

  /* Assignments (assign.cpp) */
  void visit(AssignStmt*) override;
  void transformUpdate(AssignStmt*);
  void visit(AssignMemberStmt*) override;
  auto transform_inplace_update(AssignStmt*) -> std::pair<bool, ExprPtr>;

  /* Loops (loops.cpp) */
  void visit(BreakStmt*) override;
  void visit(ContinueStmt*) override;
  void visit(WhileStmt*) override;
  void visit(ForStmt*) override;
  auto transform_heterogenous_tuple_for(ForStmt*) -> StmtPtr;
  auto transform_static_for_loop(ForStmt*) -> StmtPtr;

  /* Errors and exceptions (error.cpp) */
  void visit(TryStmt*) override;
  void visit(ThrowStmt*) override;

  /* Functions (function.cpp) */
  void visit(YieldExpr*) override;
  void visit(ReturnStmt*) override;
  void visit(YieldStmt*) override;
  void visit(FunctionStmt*) override;
  auto partialize_function(const Pud::Type::FuncTypePtr&) -> ExprPtr;
  auto get_func_type_base(size_t) -> std::shared_ptr<Pud::Type::RecordType>;

 public:
  auto make_function_type(FunctionStmt*) -> Pud::Type::FuncTypePtr;

 private:
  /* Classes (class.cpp) */
  void visit(ClassStmt*) override;
  void parse_base_classes(ClassStmt*);
  auto generate_tuple(size_t, const std::string& = TYPE_TUPLE,
                      std::vector<std::string> = {}, bool = true)
      -> std::string;

  /* The rest (typecheck.cpp) */
  void visit(SuiteStmt*) override;
  void visit(ExprStmt*) override;
  void visit(StmtExpr*) override;
  void visit(CommentStmt* stmt) override;

 private:
  /* Type inference (infer.cpp) */
  auto unify(Pud::Type::TypePtr& a, const Pud::Type::TypePtr& b)
      -> Pud::Type::TypePtr;
  auto unify(Pud::Type::TypePtr&& a, const Pud::Type::TypePtr& b)
      -> Pud::Type::TypePtr {
    auto x = a;
    return unify(x, b);
  }
  auto infer_types(StmtPtr, bool is_toplevel = false) -> StmtPtr;
  auto realize(Pud::Type::TypePtr) -> Pud::Type::TypePtr;
  auto realize_func(Pud::Type::FuncType*, bool = false) -> Pud::Type::TypePtr;
  auto realize_type(Pud::Type::ClassType*) -> Pud::Type::TypePtr;
  auto generate_special_ast(Pud::Type::FuncType*)
      -> std::shared_ptr<FunctionStmt>;
  auto get_realization_id(Pud::Type::ClassType*, Pud::Type::FuncType*)
      -> size_t;
  auto make_ir_type(Pud::Type::ClassType*) -> Pud::IR::Types::Type*;
  auto make_ir_function(
      const std::shared_ptr<Cache::Function::FunctionRealization>&)
      -> Pud::IR::Func*;

 private:
  auto find_best_method(const Pud::Type::ClassTypePtr& typ,
                        const std::string& member,
                        const std::vector<Pud::Type::TypePtr>& args)
      -> Pud::Type::FuncTypePtr;
  auto find_best_method(const Pud::Type::ClassTypePtr& typ,
                        const std::string& member,
                        const std::vector<ExprPtr>& args)
      -> Pud::Type::FuncTypePtr;
  auto find_best_method(
      const Pud::Type::ClassTypePtr& typ, const std::string& member,
      const std::vector<std::pair<std::string, Pud::Type::TypePtr>>& args)
      -> Pud::Type::FuncTypePtr;
  auto can_call(const Pud::Type::FuncTypePtr&,
                const std::vector<CallExpr::Arg>&) -> int;
  auto find_matching_methods(const Pud::Type::ClassTypePtr& typ,
                             const std::vector<Pud::Type::FuncTypePtr>& methods,
                             const std::vector<CallExpr::Arg>& args)
      -> std::vector<Pud::Type::FuncTypePtr>;
  auto wrap_expr(ExprPtr& expr, const Pud::Type::TypePtr& expected_type,
                 const Pud::Type::FuncTypePtr& callee = nullptr,
                 bool allow_unwrap = true) -> bool;
  auto cast_to_super_class(ExprPtr expr, Pud::Type::ClassTypePtr super_typ,
                           bool = false) -> ExprPtr;
  auto prepare_vtables() -> StmtPtr;

 public:
  auto is_tuple(const std::string& s) const -> bool { return s == TYPE_TUPLE; }
  auto get_class_fields(Pud::Type::ClassType*)
      -> std::vector<Cache::Class::ClassField>&;

  friend class Cache;
  friend class Pud::Type::CallableTrait;
  friend class Pud::Type::UnionType;

 private:
  auto unpack_tuple_types(ExprPtr) -> std::shared_ptr<
      std::vector<std::pair<std::string, Pud::Type::TypePtr>>>;
  auto transform_static_loop_call(
      const std::vector<std::string>&, ExprPtr,
      std::function<std::shared_ptr<SourceObject>(StmtPtr)>)
      -> std::pair<bool, std::vector<std::shared_ptr<SourceObject>>>;
};

}  // namespace Pud::AST

#endif  // PUD_TYPECHECK_TYPECHECK_H