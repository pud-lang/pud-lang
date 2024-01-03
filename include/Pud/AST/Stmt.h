#ifndef PUD_AST_STMT_H
#define PUD_AST_STMT_H

#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>

#include "Pud/AST/Expr.h"
#include "Pud/Common/Common.h"

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

  // 转换节点为S表达式。
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

  // 可变参数模板构造函数，允许以列表初始化的方式传递多个语句。
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
  // 表示print后面是否有悬挂逗号（dangling comma）例如: print a,
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
/// @li while True: print
/// @li while True: break
///          else: print
struct WhileStmt : public Stmt {
  // 循环的条件表达式。
  ExprPtr cond;
  // 循环体内执行的语句。
  StmtPtr suite;
  // 循环结束后执行的 else 语句块（如果有）。
  StmtPtr else_suite;
  // 用于模拟 goto 语句的变量名：`while goto_var: ...`。
  std::string goto_var = "";

  WhileStmt(ExprPtr cond, StmtPtr suite, StmtPtr else_suite = nullptr);
  WhileStmt(const WhileStmt& stmt);

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

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
  std::vector<CallExpr::Arg> omp_args;

  // Indicates if iter was wrapped with __iter__() call.
  bool wrapped;

  ForStmt(ExprPtr var, ExprPtr iter, StmtPtr suite,
          StmtPtr else_suite = nullptr, ExprPtr decorator = nullptr,
          std::vector<CallExpr::Arg> omp_args = {});
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
  // 用于避免在多次类型检查或其他编译阶段中重复处理同一语句。
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
  bool non_local;

  explicit GlobalStmt(std::string var, bool non_local = false);
  GlobalStmt(const GlobalStmt& stmt) = default;

  auto to_string(int indent) const -> std::string override;
  using Stmt::to_string;

  auto clone() const -> StmtPtr override;

  void accept(ASTVisitor& visitor) override;
};

// 用于表示函数或方法的各种特性和属性。
struct Attr {
  // 顶层属性，用于表示该函数与特定语言或平台的兼容性或特殊处理方式。
  const static std::string LLVM;
  const static std::string Python;
  const static std::string Atomic;    // 表示函数操作是原子性的。
  const static std::string Property;  // 表示函数作为类的属性。
  const static std::string StaticMethod;  // 表示函数是一个静态方法。
  const static std::string Attribute;  // 表示该函数自身就是一个属性。
  const static std::string C;  // 表示函数与C语言或其特性相关。
  // 内部属性，编译器内部使用的标记。
  const static std::string Internal;  // 表示这是一个内部使用的函数或特性。
  const static std::string HiddenFromUser;  // 表示该特性或函数对最终用户隐藏。
  const static std::string ForceRealize;  // 表示强制执行某些编译时计算或优化。
  const static std::string
      RealizeWithoutSelf;  // 特定于类方法，表示可以在没有类实例的情况下实现。
  // 编译器生成的属性，用于编译器在处理函数时的内部逻辑。
  const static std::string CVarArg;  // 表示函数可以接受可变数量的参数。
  const static std::string Method;  // 表示这是一个类的方法。
  const static std::string Capture;  // 与闭包或函数作用域捕获相关。
  const static std::string HasSelf;  // 表示函数是一个方法，需要self或this指针。
  // 类属性，用于标记类方法的特性。
  const static std::string Extend;  // 表示方法扩展了其所属的类。
  const static std::string Tuple;   // 表示该方法与元组操作相关。
  // 标准库属性，用于特定库功能的实现。
  const static std::string Test;      // 表示这是一个测试函数。
  const static std::string Overload;  // 表示函数重载。
  const static std::string Export;  // 表示该函数或方法是可导出的。
  // 表示函数所属的模块。
  std::string module;
  // 仅在函数为方法时设置，表示其父类名称。
  std::string parent_class;
  // 表示函数是否被__attribute__装饰。
  bool is_attribute;

  //  表示一组特殊行为方法名。
  std::set<std::string> magics;

  // 存储自定义属性集合。
  std::set<std::string> custom_attr;

  explicit Attr(
      const std::vector<std::string>& attrs = std::vector<std::string>());
  void set(const std::string& attr);
  void unset(const std::string& attr);
  auto has(const std::string& attr) const -> bool;
};

/// 表示一个函数声明 (@(attributes...) def name[funcs...](args...) -> ret:
/// suite).
/// @li: @decorator
///           def foo[T=int, U: int](a, b: int = 0) -> list[T]: pass
struct FunctionStmt : public Stmt {
  // 函数的名称。
  std::string name;
  // 函数的返回类型，如果没有指定则为 nullptr。
  ExprPtr ret;
  // 一个 Param 类型的向量，表示函数的参数。
  std::vector<Param> args;
  // 函数的实现部分，可能包含多个语句。
  StmtPtr suite;
  // 函数的属性，使用 Attr 结构体表示。
  Attr attributes;
  // 函数的装饰器列表。
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

  // 返回函数签名的字符串表示。
  // @li (T U (int 0))
  auto signature() const -> std::string;
  // 检查函数是否有特定属性。
  auto has_attr(const std::string& attr) const -> bool;
  // 处理装饰器。
  void parse_decorators();

  // 获取特定类型的参数数量。
  auto get_star_args() const -> size_t;
  auto get_kw_star_args() const -> size_t;

  // 返回指向当前函数声明的指针。
  auto get_function() -> FunctionStmt* override { return this; }
  // 获取文档字符串。
  auto get_doc_str() -> std::string;
  // 获取不能自动推断的泛型参数。
  auto get_non_inferrable_generics() -> std::unordered_set<std::string>;
};

/// Class statement (@(attributes...) class name[generics...]: args... ; suite).
/// @li: @type
///           class F[T]:
///              m: T
///              def __new__() -> F[T]: ...
/// # 装饰器
/// @decorator
/// class MyClass(BaseClass, OtherBaseClass):
///     # 类变量（字段或属性）
///     attribute1: int
///     attribute2: str
///
///     # 类方法
///     def method1(self, param1: int) -> str:
///         return str(param1)
///
///     # 静态方法
///     @staticmethod
///     def static_method(param2: str) -> int:
///         return len(param2)
/// -name: 'MyClass' - 类名。
///
/// -args: 包含 attribute1 和 attribute2 的 Param 对象列表。
///  这些是类的字段或属性，每个 Param 对象包含字段名和类型（例如，int 和 str）。
///
/// -suite: 包含类体中定义的方法（例如 method1 和 static_method）的 StmtPtr
/// 类型的语句集合。
///
/// -attributes: 可以包含类的额外信息，例如是否是原子类。
///
/// -decorators: 装饰器列表。在这个例子中，包含了一个 ExprPtr
/// 类型的表达式，对应于 @decorator。
///
/// -base_classes: 基类列表。在这个例子中，BaseClass 和 OtherBaseClass
/// 被包含在这个列表中。
///
/// -static_base_classes: 静态基类列表。这通常用于特殊情况，本例中没有展示。
struct ClassStmt : public Stmt {
  // 类名的字符串表示。
  std::string name;
  // 类的参数列表。这些参数表示类的字段或属性。
  std::vector<Param> args;
  // 代表类体的 StmtPtr 类型的语句。通常包含方法定义等。
  StmtPtr suite;
  // 类的属性，Attr
  // 类型。包含类的额外信息，如是否是原子类、是否应该实现为静态方法等。
  Attr attributes;
  // 装饰器列表，每个装饰器是一个 ExprPtr 类型的表达式。
  std::vector<ExprPtr> decorators;
  // 基类列表，每个基类是一个 ExprPtr 类型的表达式。
  std::vector<ExprPtr> base_classes;
  // 静态基类列表，用于某些特殊情况。
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

  // 检查类是否是类似元组的记录（例如，具有 "@tuple" 属性）。
  auto is_record() const -> bool;
  // 检查类是否具有指定的属性。
  auto has_attr(const std::string& attr) const -> bool;

  auto get_class() -> ClassStmt* override { return this; }

  // 解析并处理类的装饰器。
  void parse_decorators();
  // 用于判断给定的 Param 是否是类变量。
  static auto is_class_var(const Param& p) -> bool;
  // 提取类的文档字符串，通常是类定义内的第一个字符串。
  auto get_doc_str() -> std::string;
};

/// 表示 Python 中的 yield from 语句，用于委托生成器 (yield from expr).
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

/// 表示 Python 中的 with 语句，用于资源管理 (with (item as var)...: suite).
/// @li: with foo(), bar() as b: pass
struct WithStmt : public Stmt {
  std::vector<ExprPtr> items;
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

/// 表示自定义的语句块 (foo: ...).
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

template <typename T>
struct fmt::formatter<
    T, std::enable_if_t<std::is_base_of<Pud::AST::Stmt, T>::value, char>>
    : fmt::ostream_formatter {};

template <typename T>
struct fmt::formatter<
    T,
    std::enable_if_t<
        std::is_convertible<T, std::shared_ptr<Pud::AST::Stmt>>::value, char>>
    : fmt::formatter<std::string_view> {
  template <typename FormatContext>
  auto format(const T& p, FormatContext& ctx) const -> decltype(ctx.out()) {
    return fmt::format_to(ctx.out(), "{}", p ? p->toString() : "<nullptr>");
  }
};

#endif  // PUD_AST_STMT_H