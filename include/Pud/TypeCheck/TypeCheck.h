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

namespace Pud::Type {

// 用于推断表达式类型并执行类型引导转换的访问者类。
class TypecheckVisitor
    : public AST::CallbackASTVisitor<AST::ExprPtr, AST::StmtPtr> {
  // 共享简化上下文的指针，用于在类型检查过程中跟踪标识符。
  std::shared_ptr<TypeContext> ctx;
  // 在当前语句之前插入的语句列表。
  std::shared_ptr<std::vector<AST::StmtPtr>> prepend_stmts;

  // 用于存储每个新表达式和语句，稍后由 transform 方法返回。
  AST::ExprPtr result_expr;
  AST::StmtPtr result_stmt;

 public:
  // 应用类型检查到给定的语句。
  static auto apply(AST::Cache* cache, const AST::StmtPtr& stmts)
      -> AST::StmtPtr;

 public:
  // 接受类型上下文和要插入的语句列表。
  explicit TypecheckVisitor(
      std::shared_ptr<TypeContext> ctx,
      const std::shared_ptr<std::vector<AST::StmtPtr>>& stmts = nullptr);

 public:
  // 转换表达式和语句。
  auto transform(AST::ExprPtr& e) -> AST::ExprPtr override;
  auto transform(const AST::ExprPtr& expr) -> AST::ExprPtr override {
    auto e = expr;
    return transform(e);
  }
  auto transform(AST::StmtPtr& s) -> AST::StmtPtr override;
  auto transform(const AST::StmtPtr& stmt) -> AST::StmtPtr override {
    auto s = stmt;
    return transform(s);
  }
  // 转换类型表达式。
  auto transform_type(AST::ExprPtr& expr) -> AST::ExprPtr;
  auto transform_type(const AST::ExprPtr& expr) -> AST::ExprPtr {
    auto e = expr;
    return transform_type(e);
  }

 private:
  // 定义对未特别处理的表达式和语句的默认行为。
  void default_visit(AST::Expr* e) override;
  void default_visit(AST::Stmt* s) override;

 private:
  void visit(AST::NoneExpr*) override;
  void visit(AST::BoolExpr*) override;
  void visit(AST::IntExpr*) override;
  void visit(AST::FloatExpr*) override;
  void visit(AST::StringExpr*) override;

  /* Identifier access expressions (access.cpp) */
  void visit(AST::IdExpr*) override;
  void visit(AST::DotExpr*) override;
  auto transform_dot(AST::DotExpr*, std::vector<AST::CallExpr::Arg>* = nullptr)
      -> AST::ExprPtr;
  auto get_class_member(AST::DotExpr*, std::vector<AST::CallExpr::Arg>*)
      -> AST::ExprPtr;
  auto find_special_member(const std::string&) -> Pud::Type::TypePtr;
  auto get_best_overload(AST::Expr*, std::vector<AST::CallExpr::Arg>*)
      -> Pud::Type::FuncTypePtr;
  auto get_dispatch(const std::string&) -> Pud::Type::FuncTypePtr;

  /* Collection and comprehension expressions (collections.cpp) */
  void visit(AST::TupleExpr*) override;
  void visit(AST::ListExpr*) override;
  void visit(AST::SetExpr*) override;
  void visit(AST::DictExpr*) override;
  void visit(AST::GeneratorExpr*) override;
  auto transform_comprehension(const std::string&, const std::string&,
                               std::vector<AST::ExprPtr>&) -> AST::ExprPtr;

  /* Conditional expression and statements (cond.cpp) */
  void visit(AST::IfExpr*) override;
  void visit(AST::IfStmt*) override;

  /* Operators (op.cpp) */
  void visit(AST::UnaryExpr*) override;
  auto evaluate_static_unary(AST::UnaryExpr*) -> AST::ExprPtr;
  void visit(AST::BinaryExpr*) override;
  auto evaluate_static_binary(AST::BinaryExpr*) -> AST::ExprPtr;
  auto transform_binary_simple(AST::BinaryExpr*) -> AST::ExprPtr;
  auto transform_binary_is(AST::BinaryExpr*) -> AST::ExprPtr;
  auto get_magic(const std::string&) -> std::pair<std::string, std::string>;
  auto transform_binary_inplace_magic(AST::BinaryExpr*, bool) -> AST::ExprPtr;
  auto transform_binary_magic(AST::BinaryExpr*) -> AST::ExprPtr;
  void visit(AST::PipeExpr*) override;
  void visit(AST::IndexExpr*) override;
  auto transform_static_tuple_index(const Pud::Type::ClassTypePtr&,
                                    const AST::ExprPtr&, const AST::ExprPtr&)
      -> std::pair<bool, AST::ExprPtr>;
  auto translate_index(int64_t, int64_t, bool = false) -> int64_t;
  auto slice_adjust_indices(int64_t, int64_t*, int64_t*, int64_t) -> int64_t;
  void visit(AST::InstantiateExpr*) override;
  void visit(AST::SliceExpr*) override;

  /* Calls (call.cpp) */
  /// Holds partial call information for a CallExpr.
  struct PartialCallData {
    bool isPartial = false;        // true if the call is partial
    std::string var;               // set if calling a partial type itself
    std::vector<char> known = {};  // mask of known arguments
    AST::ExprPtr args = nullptr,
                 kwArgs = nullptr;  // partial *args/**kwargs expressions
  };
  void visit(AST::StarExpr*) override;
  void visit(AST::KeywordStarExpr*) override;
  void visit(AST::EllipsisExpr*) override;
  void visit(AST::CallExpr*) override;
  auto transform_call_args(std::vector<AST::CallExpr::Arg>&) -> bool;
  auto get_callee_fn(AST::CallExpr*, PartialCallData&)
      -> std::pair<Pud::Type::FuncTypePtr, AST::ExprPtr>;
  auto call_reorder_arguments(Pud::Type::FuncTypePtr, AST::CallExpr*,
                              PartialCallData&) -> AST::ExprPtr;
  auto typecheck_call_args(const Pud::Type::FuncTypePtr&,
                           std::vector<AST::CallExpr::Arg>&) -> bool;
  auto transform_special_call(AST::CallExpr*) -> std::pair<bool, AST::ExprPtr>;
  auto transform_super_f(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_super() -> AST::ExprPtr;
  auto transform_ptr(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_array(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_is_instance(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_static_len(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_has_attr(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_get_attr(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_set_attr(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_compile_error(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_tuple_fn(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_type_fn(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_realized_fn(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_static_print_fn(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_has_rtti_fn(AST::CallExpr* expr) -> AST::ExprPtr;
  auto transform_internal_static_fn(AST::CallExpr* expr)
      -> std::pair<bool, AST::ExprPtr>;
  auto get_super_types(const Pud::Type::ClassTypePtr& cls)
      -> std::vector<Pud::Type::ClassTypePtr>;
  void add_function_generics(const Pud::Type::FuncType* t);
  auto generate_partial_stub(const std::vector<char>& mask,
                             Pud::Type::FuncType* fn) -> std::string;

  /* Assignments (assign.cpp) */
  void visit(AST::AssignStmt*) override;
  void transformUpdate(AST::AssignStmt*);
  void visit(AST::AssignMemberStmt*) override;
  auto transform_inplace_update(AST::AssignStmt*)
      -> std::pair<bool, AST::ExprPtr>;

  /* Loops (loops.cpp) */
  void visit(AST::BreakStmt*) override;
  void visit(AST::ContinueStmt*) override;
  void visit(AST::WhileStmt*) override;
  void visit(AST::ForStmt*) override;
  auto transform_heterogenous_tuple_for(AST::ForStmt*) -> AST::StmtPtr;
  auto transform_static_for_loop(AST::ForStmt*) -> AST::StmtPtr;

  /* Errors and exceptions (error.cpp) */
  void visit(AST::TryStmt*) override;
  void visit(AST::ThrowStmt*) override;

  /* Functions (function.cpp) */
  void visit(AST::YieldExpr*) override;
  void visit(AST::ReturnStmt*) override;
  void visit(AST::YieldStmt*) override;
  void visit(AST::FunctionStmt*) override;
  auto partialize_function(const Pud::Type::FuncTypePtr&) -> AST::ExprPtr;
  auto get_func_type_base(size_t) -> std::shared_ptr<Pud::Type::RecordType>;

 public:
  auto make_function_type(AST::FunctionStmt*) -> Pud::Type::FuncTypePtr;

 private:
  /* Classes (class.cpp) */
  void visit(AST::ClassStmt*) override;
  void parse_base_classes(AST::ClassStmt*);
  auto generate_tuple(size_t, const std::string& = TYPE_TUPLE,
                      std::vector<std::string> = {}, bool = true)
      -> std::string;

  /* The rest (typecheck.cpp) */
  void visit(AST::SuiteStmt*) override;
  void visit(AST::ExprStmt*) override;
  void visit(AST::StmtExpr*) override;
  void visit(AST::CommentStmt* stmt) override;

 private:
  /* Type inference (infer.cpp) */
  auto unify(Pud::Type::TypePtr& a, const Pud::Type::TypePtr& b)
      -> Pud::Type::TypePtr;
  auto unify(Pud::Type::TypePtr&& a, const Pud::Type::TypePtr& b)
      -> Pud::Type::TypePtr {
    auto x = a;
    return unify(x, b);
  }
  auto infer_types(AST::StmtPtr, bool is_toplevel = false) -> AST::StmtPtr;
  auto realize(Pud::Type::TypePtr) -> Pud::Type::TypePtr;
  auto realize_func(Pud::Type::FuncType*, bool = false) -> Pud::Type::TypePtr;
  auto realize_type(Pud::Type::ClassType*) -> Pud::Type::TypePtr;
  auto generate_special_ast(Pud::Type::FuncType*)
      -> std::shared_ptr<AST::FunctionStmt>;
  auto get_realization_id(Pud::Type::ClassType*, Pud::Type::FuncType*)
      -> size_t;
  auto make_ir_type(Pud::Type::ClassType*) -> Pud::IR::Types::Type*;
  auto make_ir_function(
      const std::shared_ptr<AST::Cache::Function::FunctionRealization>&)
      -> Pud::IR::Func*;

 private:
  auto find_best_method(const Pud::Type::ClassTypePtr& typ,
                        const std::string& member,
                        const std::vector<Pud::Type::TypePtr>& args)
      -> Pud::Type::FuncTypePtr;
  auto find_best_method(const Pud::Type::ClassTypePtr& typ,
                        const std::string& member,
                        const std::vector<AST::ExprPtr>& args)
      -> Pud::Type::FuncTypePtr;
  auto find_best_method(
      const Pud::Type::ClassTypePtr& typ, const std::string& member,
      const std::vector<std::pair<std::string, Pud::Type::TypePtr>>& args)
      -> Pud::Type::FuncTypePtr;
  auto can_call(const Pud::Type::FuncTypePtr&,
                const std::vector<AST::CallExpr::Arg>&) -> int;
  auto find_matching_methods(const Pud::Type::ClassTypePtr& typ,
                             const std::vector<Pud::Type::FuncTypePtr>& methods,
                             const std::vector<AST::CallExpr::Arg>& args)
      -> std::vector<Pud::Type::FuncTypePtr>;
  auto wrap_expr(AST::ExprPtr& expr, const Pud::Type::TypePtr& expected_type,
                 const Pud::Type::FuncTypePtr& callee = nullptr,
                 bool allow_unwrap = true) -> bool;
  auto cast_to_super_class(AST::ExprPtr expr, Pud::Type::ClassTypePtr super_typ,
                           bool = false) -> AST::ExprPtr;
  auto prepare_vtables() -> AST::StmtPtr;

 public:
  auto is_tuple(const std::string& s) const -> bool { return s == TYPE_TUPLE; }
  auto get_class_fields(Pud::Type::ClassType*)
      -> std::vector<AST::Cache::Class::ClassField>&;

  friend class Cache;
  friend class Pud::Type::CallableTrait;
  friend class Pud::Type::UnionType;

 private:
  auto unpack_tuple_types(AST::ExprPtr) -> std::shared_ptr<
      std::vector<std::pair<std::string, Pud::Type::TypePtr>>>;
  auto transform_static_loop_call(
      const std::vector<std::string>&, AST::ExprPtr,
      std::function<std::shared_ptr<SourceObject>(AST::StmtPtr)>)
      -> std::pair<bool, std::vector<std::shared_ptr<SourceObject>>>;
};

}  // namespace Pud::Type

#endif  // PUD_TYPECHECK_TYPECHECK_H