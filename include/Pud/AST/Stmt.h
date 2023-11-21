#ifndef PUD_AST_STMT_H
#define PUD_AST_STMT_H

#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

#include "Pud/AST/Expr.h"
#include "Pud/Common/Source.h"

namespace Pud::AST {

struct ASTVisitor;
struct AssignStmt;
struct ClassStmt;
struct ExprStmt;
struct SuiteStmt;
struct FunctionStmt;

/// 用于构建抽象语法树的基类。
/// Stmt 类代表了 AST 中的一个语句节点。
struct Stmt : public Pud::SourceObject {
  using BaseType = Stmt;

  // 表示语句中的所有类型是否已经推断出来，即类型检查是否成功。
  bool done;
  // 用于优化，例如在编译器优化阶段，可以使用它来确定节点的相关性或优先级。
  int age;

  Stmt();
  Stmt(const Stmt& s) = default;
  explicit Stmt(const Pud::SourceInfo& s);

  /// 转换节点为S表达式。
  auto to_string() const -> std::string;
  virtual auto to_string(int indent) const -> std::string = 0;

  // 验证节点的有效性，如果节点不合法，则抛出ParseASTException异常。
  void validate() const;
  // 用于深拷贝节点。
  virtual auto clone() const -> std::shared_ptr<Stmt> = 0;
  // 访问者模式。
  virtual void accept(ASTVisitor&) = 0;

  // 允许将 Stmt 对象的字符串表示直接输出到 C++ 流。
  friend auto operator<<(std::ostream& out, const Stmt& stmt) -> std::ostream& {
    return out << stmt.to_string();
  }

  virtual auto get_assign() -> AssignStmt* { return nullptr; }
  virtual auto get_class() -> ClassStmt* { return nullptr; }
  virtual auto get_expr() -> ExprStmt* { return nullptr; }
  virtual auto get_suite() -> SuiteStmt* { return nullptr; }
  virtual auto get_function() -> FunctionStmt* { return nullptr; }

  // 返回一个语句块中的第一个语句。如果该语句不是一个语句块，则返回语句本身。
  virtual auto first_in_block() -> Stmt* { return this; }

  auto is_done() const -> bool { return done; }
  void set_done() { done = true; }
};

using StmtPtr = std::shared_ptr<Stmt>;

/// 表示一个语句块，即一系列顺序执行的语句。
/// 在 Python 中类似于一个代码块，比如一个函数体或循环体。
struct SuiteStmt : public Stmt {
  // 存储一系列语句的向量。
  std::vector<StmtPtr> stmts;

  using Stmt::Stmt;

  explicit SuiteStmt(std::vector<StmtPtr> stmts = {});

  template <typename... Ts>
  SuiteStmt(StmtPtr stmt, Ts... stmts)  // NOLINT(*)
      : stmts({stmt, stmts...}) {}

  SuiteStmt(const SuiteStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;

  auto get_suite() -> SuiteStmt* override { return this; }

  auto first_in_block() -> Stmt* override {
    return stmts.empty() ? nullptr : stmts[0]->first_in_block();
  }
  auto last_in_block() -> StmtPtr*;

  // 将嵌套的 SuiteStmt 对象展平。
  static void flatten(const StmtPtr& s, std::vector<StmtPtr>& stmts);
};

struct BreakStmt : public Stmt {
  BreakStmt() = default;
  BreakStmt(const BreakStmt& stmt) = default;

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

struct ContinueStmt : public Stmt {
  ContinueStmt() = default;
  ContinueStmt(const ContinueStmt& stmt) = default;

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// 表达式语句，比如：3 + foo()
struct ExprStmt : public Stmt {
  ExprPtr expr;

  explicit ExprStmt(ExprPtr expr);
  ExprStmt(const ExprStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;

  auto get_expr() -> ExprStmt* override { return this; }
};

/// 代表一个赋值语句，如 a = 5。
/// a: Optional[int] = 5
/// a, b, c = 5, *z  (左侧 a, b, c 是一个元组，右侧 5, *z 也是一个元组。
/// *z 是一个解包操作，意味着 z 中的元素将被分配给剩余的变量 b 和 c。)
struct AssignStmt : public Stmt {
  ExprPtr lhs, rhs, type;

  AssignStmt(ExprPtr lhs, ExprPtr rhs, ExprPtr type = nullptr);
  AssignStmt(const AssignStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;

  auto get_assign() -> AssignStmt* override { return this; }

  // 检查赋值是否是更新操作。
  auto is_update() const -> bool { return update != Assign; }
  auto is_atomic_update() const -> bool { return update == UpdateAtomic; }
  void set_update() { update = Update; }
  void set_atomic_update() { update = UpdateAtomic; }

 private:
  // 表示赋值是否是更新操作或原子更新。
  enum { Assign, Update, UpdateAtomic } update;
};

/// 代表一个删除语句，如 del a。
struct DelStmt : public Stmt {
  ExprPtr expr;

  explicit DelStmt(ExprPtr expr);
  DelStmt(const DelStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

struct PrintStmt : public Stmt {
  std::vector<ExprPtr> items;
  /// 表示print后面是否有悬挂逗号（dangling comma）例如: print a,
  bool is_inline;

  explicit PrintStmt(std::vector<ExprPtr> items, bool is_inline);
  PrintStmt(const PrintStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

struct ReturnStmt : public Stmt {
  // 要返回或者yield的表达式，如果是空返回，则为 nullptr。
  ExprPtr expr;

  explicit ReturnStmt(ExprPtr expr = nullptr);
  ReturnStmt(const ReturnStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// yield语句
struct YieldStmt : public Stmt {
  // 要返回或者yield的表达式，如果是空返回，则为 nullptr。
  ExprPtr expr;

  explicit YieldStmt(ExprPtr expr = nullptr);
  YieldStmt(const YieldStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// Assert语句
/// 例如：assert a
/// assert a, "Message"
struct AssertStmt : public Stmt {
  // assert后的表达式。
  ExprPtr expr;
  // 没有信息的话为nullptr
  ExprPtr message;

  explicit AssertStmt(ExprPtr expr, ExprPtr message = nullptr);
  AssertStmt(const AssertStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// 代表一个 while 循环语句 (while cond: suite; else: else_suite)。
struct WhileStmt : public Stmt {
  // 循环的条件表达式。
  ExprPtr cond;
  // 循环体内执行的语句。
  StmtPtr suite;
  // 循环结束后执行的 else 语句块（如果有）。
  StmtPtr else_suite;
  // 用于模拟 goto 语句的变量名：`while gotoVar: ...`。
  std::string goto_var = "";

  WhileStmt(ExprPtr cond, StmtPtr suite, StmtPtr else_suite = nullptr);
  WhileStmt(const WhileStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

// TODO: CallExpr
/// 代表一个 for 循环语句 (for var in iter: suite; else else_suite)。
struct ForStmt : public Stmt {
  // 迭代变量。
  ExprPtr var;
  // 被迭代的对象。
  ExprPtr iter;
  // 循环体内执行的语句。
  StmtPtr suite;
  // 循环结束后执行的 else 语句块（如果有）。
  StmtPtr else_suite;
  ExprPtr decorator;

  /// Indicates if iter was wrapped with __iter__() call.
  bool wrapped;

  ForStmt(ExprPtr var, ExprPtr iter, StmtPtr suite,
          StmtPtr else_suite = nullptr, ExprPtr decorator = nullptr);
  ForStmt(const ForStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// 代表一个 if 条件语句 (if cond: suite; (elif cond: suite)...)。
struct IfStmt : public Stmt {
  // 条件表达式。
  ExprPtr cond;
  // 满足条件时执行的语句。
  StmtPtr if_suite;
  // 不满足条件时执行的 else 语句块。
  StmtPtr else_suite;

  IfStmt(ExprPtr cond, StmtPtr if_suite, StmtPtr else_suite = nullptr);
  IfStmt(const IfStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// 代表一个模式匹配语句 (match what: (case pattern: case)...)。
/// @li match a:
///          case 1: print
///          case _: pass
struct MatchStmt : public Stmt {
  struct MatchCase {
    ExprPtr pattern;
    ExprPtr guard;
    StmtPtr suite;

    auto clone() const -> MatchCase;
  };

  // 要匹配的表达式。
  ExprPtr what;
  // 一系列匹配情况。
  std::vector<MatchCase> cases;

  MatchStmt(ExprPtr what, std::vector<MatchCase> cases);
  MatchStmt(const MatchStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// 代表一个导入语句。
/// This node describes various kinds of import statements:
///  - from from import what (as as)
///  - import what (as as)
///  - from c import what(args...) (-> ret) (as as)
///  - from .(dots...)from import what (as as)
/// @li import a
/// @li from b import a
/// @li from ...b import a as ai
/// @li from c import foo(int) -> int as bar
/// @li from python.numpy import array
/// @li from python import numpy.array(int) -> int as na
struct ImportStmt : public Stmt {
  // 导入的模块和名称。
  ExprPtr from, what;
  // 导入时使用的别名。
  std::string as;
  // 相对导入中的点的数量(e.g. dots is 3 for "from ...foo").
  size_t dots;
  /// Function argument types for C imports.
  std::vector<Param> args;
  /// Function return type for C imports.
  ExprPtr ret;
  /// Set if this is a function C import (not variable import)
  bool is_function;

  ImportStmt(ExprPtr from, ExprPtr what, std::vector<Param> args = {},
             ExprPtr ret = nullptr, std::string as = "", size_t dots = 0,
             bool is_function = true);
  ImportStmt(const ImportStmt& stmt);

  void validate() const;

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// 代表一个 try-catch 语句 (try: suite; (catch var (as exc): suite)...;
/// finally: finally)。
/// @li: try: a
///           catch e: pass
///           catch e as Exc: pass
///           catch: pass
///           finally: print
struct TryStmt : public Stmt {
  struct Catch {
    // 如果catch未命名，则为空字符串。
    std::string var;
    // 显式表示exception类型，没有则为nullptr。
    ExprPtr exc;
    StmtPtr suite;

    auto clone() const -> Catch;
  };

  // try 块中的语句。
  StmtPtr suite;
  // 一系列 catch 块。
  std::vector<Catch> catches;
  // finally 块，没有则为nullptr。
  StmtPtr finally;

  TryStmt(StmtPtr suite, std::vector<Catch> catches, StmtPtr finally = nullptr);
  TryStmt(const TryStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

// 代表一个抛出异常的语句 (raise expr)。
/// @li: raise a
struct ThrowStmt : public Stmt {
  // 抛出的异常表达式。
  ExprPtr expr;
  // 如果语句在类型检查阶段被转换，则为True
  //(避免多次设置ExcHeader)。
  bool transformed;

  explicit ThrowStmt(ExprPtr expr, bool transformed = false);
  ThrowStmt(const ThrowStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

// 代表一个声明全局变量的语句 (global var)。
/// @li: global a
struct GlobalStmt : public Stmt {
  std::string var;
  bool nonLocal;

  explicit GlobalStmt(std::string var, bool non_local = false);
  GlobalStmt(const GlobalStmt& stmt) = default;

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

struct Attr {
  // Toplevel attributes
  const static std::string LLVM;
  const static std::string Python;
  const static std::string Atomic;
  const static std::string Property;
  const static std::string StaticMethod;
  const static std::string Attribute;
  const static std::string C;
  // Internal attributes
  const static std::string Internal;
  const static std::string HiddenFromUser;
  const static std::string ForceRealize;
  const static std::string RealizeWithoutSelf;  // not internal
  // Compiler-generated attributes
  const static std::string CVarArg;
  const static std::string Method;
  const static std::string Capture;
  const static std::string HasSelf;
  // Class attributes
  const static std::string Extend;
  const static std::string Tuple;
  // Standard library attributes
  const static std::string Test;
  const static std::string Overload;
  const static std::string Export;
  // Function module
  std::string module;
  // Parent class (set for methods only)
  std::string parent_class;
  // True if a function is decorated with __attribute__
  bool is_attribute;

  std::set<std::string> magics;

  // Set of attributes
  std::set<std::string> custom_attr;

  explicit Attr(
      const std::vector<std::string>& attrs = std::vector<std::string>());
  void set(const std::string& attr);
  void unset(const std::string& attr);
  auto has(const std::string& attr) const -> bool;
};

/// Function statement (@(attributes...) def name[funcs...](args...) -> ret:
/// suite).
/// @li: @decorator
///           def foo[T=int, U: int](a, b: int = 0) -> list[T]: pass
struct FunctionStmt : public Stmt {
  std::string name;
  /// nullptr if return type is not specified.
  ExprPtr ret;
  std::vector<Param> args;
  StmtPtr suite;
  Attr attributes;
  std::vector<ExprPtr> decorators;

  FunctionStmt(std::string name, ExprPtr ret, std::vector<Param> args,
               StmtPtr suite, Attr attributes = Attr(),
               std::vector<ExprPtr> decorators = {});
  FunctionStmt(const FunctionStmt& stmt);

  void validate() const;

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;

  /// @return a function signature that consists of generics and arguments in a
  /// S-expression form.
  /// @li (T U (int 0))
  auto signature() const -> std::string;
  auto has_attr(const std::string& attr) const -> bool;
  void parseDecorators();

  auto get_star_args() const -> size_t;
  auto get_kw_star_args() const -> size_t;

  auto get_function() -> FunctionStmt* override { return this; }
  auto get_doc_str() -> std::string;
  auto get_non_inferrable_generics() -> std::unordered_set<std::string>;
};

/// Class statement (@(attributes...) class name[generics...]: args... ; suite).
/// @li: @type
///           class F[T]:
///              m: T
///              def __new__() -> F[T]: ...
struct ClassStmt : public Stmt {
  std::string name;
  std::vector<Param> args;
  StmtPtr suite;
  Attr attributes;
  std::vector<ExprPtr> decorators;
  std::vector<ExprPtr> base_classes;
  std::vector<ExprPtr> static_base_classes;

  ClassStmt(std::string name, std::vector<Param> args, StmtPtr suite,
            std::vector<ExprPtr> decorators = {},
            std::vector<ExprPtr> base_classes = {},
            std::vector<ExprPtr> static_base_classes = {});
  ClassStmt(std::string name, std::vector<Param> args, StmtPtr suite,
            Attr attr);
  ClassStmt(const ClassStmt& stmt);

  void validate() const;

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;

  /// @return true if a class is a tuple-like record (e.g. has a "@tuple"
  /// attribute)
  auto is_record() const -> bool;
  auto has_attr(const std::string& attr) const -> bool;

  auto get_class() -> ClassStmt* override { return this; }

  void parse_decorators();
  static auto is_class_var(const Param& p) -> bool;
  auto get_doc_str() -> std::string;
};

/// Yield-from statement (yield from expr).
/// @li: yield from it
struct YieldFromStmt : public Stmt {
  ExprPtr expr;

  explicit YieldFromStmt(ExprPtr expr);
  YieldFromStmt(const YieldFromStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// With statement (with (item as var)...: suite).
/// @li: with foo(), bar() as b: pass
struct WithStmt : public Stmt {
  std::vector<ExprPtr> items;
  /// empty string if a corresponding item is unnamed
  std::vector<std::string> vars;
  StmtPtr suite;

  WithStmt(std::vector<ExprPtr> items, std::vector<std::string> vars,
           StmtPtr suite);
  WithStmt(std::vector<std::pair<ExprPtr, ExprPtr>> items, StmtPtr suite);
  WithStmt(const WithStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// Custom block statement (foo: ...).
/// @li: pt_tree: pass
struct CustomStmt : public Stmt {
  std::string keyword;
  ExprPtr expr;
  StmtPtr suite;

  CustomStmt(std::string keyword, ExprPtr expr, StmtPtr suite);
  CustomStmt(const CustomStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// The following nodes are created after the simplify stage.

/// Member assignment statement (lhs.member = rhs).
/// @li: a.x = b
struct AssignMemberStmt : public Stmt {
  ExprPtr lhs;
  std::string member;
  ExprPtr rhs;

  AssignMemberStmt(ExprPtr lhs, std::string member, ExprPtr rhs);
  AssignMemberStmt(const AssignMemberStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/// Comment statement (# comment).
/// Currently used only for pretty-printing.
struct CommentStmt : public Stmt {
  std::string comment;

  explicit CommentStmt(std::string comment);
  CommentStmt(const CommentStmt& stmt) = default;

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

/*
在 Python 的 AST 中，stmt 类型代表所有可能的语句类型，例如：

- 表达式语句 (ExprStmt)：包含一个表达式。
- 赋值语句：如 a = 5。
- 控制流语句：如 if、for、while、break、continue。
- 函数定义和类定义。
- 返回语句 (return) 和 yield 语句。
- 异常处理语句：如 try、except、finally。
- 导入语句 (import) 和其它声明语句。
*/

}  // namespace Pud::AST

#endif  // PUD_AST_STMT_H