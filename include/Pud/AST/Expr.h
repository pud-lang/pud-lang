#ifndef PUD_AST_EXPR_H
#define PUD_AST_EXPR_H

#include <memory>
#include <variant>

#include "Pud/AST/Types.h"
#include "Pud/Common/Source.h"

namespace Pud::AST {

struct ASTVisitor;
struct BinaryExpr;
struct CallExpr;
struct DotExpr;
struct EllipsisExpr;
struct IdExpr;
struct IfExpr;
struct IndexExpr;
struct IntExpr;
struct ListExpr;
struct NoneExpr;
struct StarExpr;
struct StmtExpr;
struct StringExpr;
struct TupleExpr;
struct UnaryExpr;
struct Stmt;

struct StaticValue {
  std::variant<int64_t, std::string> value;
  enum Type : uint8_t {
    NOT_STATIC = 0,
    STRING = 1,
    INT = 2,
    NOT_SUPPORTED = 3
  } type;
  bool evaluated;

  explicit StaticValue(Type);
  explicit StaticValue(int64_t);
  explicit StaticValue(std::string);
  auto operator==(const StaticValue& s) const -> bool;
  auto to_string() const -> std::string;
  auto get_int() const -> int64_t;
  auto get_string() const -> std::string;
};

struct Expr : public SourceObject {
  using BaseType = Expr;

  // 表达式的类型。
  Pud::Type::TypePtr type;
  // 这个表达式是否表示一个类型。
  bool is_type_expr;
  // 如果表达式是静态的（在编译时可知的值），此字段存储其值。
  StaticValue static_value;
  // 表示类型推断或某种处理是否完成。
  bool done;
  // 用于存储表达式的各种标志或属性。
  int attributes;
  // 指向原始表达式的指针，用于在表达式被转换或修改后保留原始形态。
  std::shared_ptr<Expr> orig_expr;

 public:
  Expr();
  Expr(const Expr& expr) = default;

  // S-expression输出。
  virtual auto to_string() const -> std::string = 0;
  // 验证表达式的合法性。
  void validate() const;
  // 创建并返回表达式的深拷贝。
  virtual auto clone() const -> std::shared_ptr<Expr> = 0;
  // 访问者模式的访问，用于遍历或操作AST。
  virtual void accept(ASTVisitor& visitor) = 0;

  // 获取表达式的类型。
  auto get_type() const -> Pud::Type::TypePtr;
  // 设置表达式的类型。
  void set_type(Pud::Type::TypePtr type);
  // 检查表达式是否为类型表达式。
  auto is_type() const -> bool;
  // 标记表达式为类型表达式。
  void mark_type();
  // 检查表达式是否为静态表达式。
  auto is_static() const -> bool;

  friend auto operator<<(std::ostream& out, const Expr& expr) -> std::ostream& {
    return out << expr.to_string();
  }

  virtual auto is_id(const std::string& /*val*/) const -> bool { return false; }
  virtual auto get_binary() -> BinaryExpr* { return nullptr; }
  virtual auto get_call() -> CallExpr* { return nullptr; }
  virtual auto get_dot() -> DotExpr* { return nullptr; }
  virtual auto get_ellipsis() -> EllipsisExpr* { return nullptr; }
  virtual auto get_id() -> IdExpr* { return nullptr; }
  virtual auto get_if() -> IfExpr* { return nullptr; }
  virtual auto get_index() -> IndexExpr* { return nullptr; }
  virtual auto get_int() -> IntExpr* { return nullptr; }
  virtual auto get_list() -> ListExpr* { return nullptr; }
  virtual auto get_none() -> NoneExpr* { return nullptr; }
  virtual auto get_star() -> StarExpr* { return nullptr; }
  virtual auto get_stmt_expr() -> StmtExpr* { return nullptr; }
  virtual auto get_string() -> StringExpr* { return nullptr; }
  virtual auto get_tuple() -> TupleExpr* { return nullptr; }
  virtual auto get_unary() -> UnaryExpr* { return nullptr; }

  // 获取或设置表达式的属性。
  auto has_attr(int attr) const -> bool;
  void set_attr(int attr);

  // 检查或标记表达式处理是否完成。
  auto is_done() const -> bool { return done; }
  void set_done() { done = true; }

  // 获取IdExpr或实例化表达式的类型名称。
  auto get_type_name() -> std::string;

 protected:
  // 将类型信息添加到S表达式字符串中。
  auto wrap_type(const std::string& sexpr) const -> std::string;
};

using ExprPtr = std::shared_ptr<Expr>;

/// 表示函数参数的一个数据结构。
struct Param : public SourceObject {
  std::string name;
  // 参数的类型。
  ExprPtr type;
  // 参数的默认值。
  ExprPtr default_value;
  // 表示参数的状态。它可以是普通参数（Normal），
  // 泛型参数（Generic），或隐藏的泛型参数（HiddenGeneric）。
  enum {
    Normal,
    Generic,
    HiddenGeneric
  } status;  // 1 for normal generic, 2 for hidden generic

  explicit Param(std::string name = "", ExprPtr type = nullptr,
                 ExprPtr default_value = nullptr, int generic = 0);
  explicit Param(const SourceInfo& info, std::string name = "",
                 ExprPtr type = nullptr, ExprPtr default_value = nullptr,
                 int generic = 0);

  auto to_string() const -> std::string;
  auto clone() const -> Param;
};

/// None 值
/// @li None
struct NoneExpr : public Expr {
  NoneExpr() = default;
  NoneExpr(const NoneExpr& expr) = default;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_none() -> NoneExpr* override { return this; }
};

/// 表示布尔值表达式。
/// @li True
struct BoolExpr : public Expr {
  bool value;

  explicit BoolExpr(bool value);
  BoolExpr(const BoolExpr& expr) = default;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 整数值表达式。
/// @li 12
/// @li 13u
/// @li 000_010b
struct IntExpr : public Expr {
  // 整数值的经过简化的字符串表示，如"000_010b"去掉下划线。
  std::string value;
  // 整数后缀，用于表示整数的类型，如 "u" 表示无符号整数。
  std::string suffix;

  // 用于存储解析后的64位整数值。
  std::unique_ptr<int64_t> int_value;

  explicit IntExpr(int64_t int_value);
  explicit IntExpr(const std::string& value, std::string suffix = "");
  IntExpr(const IntExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_int() -> IntExpr* override { return this; }
};

/// 浮点数表达式。
/// @li 12.1
/// @li 13.15z
/// @li e-12
struct FloatExpr : public Expr {
  // 简化后的浮点数的字符串表示。
  std::string value;
  // 数字的后缀。
  std::string suffix;

  // 解析后的浮点数值。
  std::unique_ptr<double> float_value;

  explicit FloatExpr(double float_value);
  explicit FloatExpr(const std::string& value, std::string suffix = "");
  FloatExpr(const FloatExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 字符串表达式。
/// @li s'ACGT'
/// @li "fff"
struct StringExpr : public Expr {
  // 存储字符串及其前缀。
  std::vector<std::pair<std::string, std::string>> strings;

  explicit StringExpr(std::string value, std::string prefix = "");
  explicit StringExpr(std::vector<std::pair<std::string, std::string>> strings);
  StringExpr(const StringExpr& expr) = default;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_string() -> StringExpr* override { return this; }
  auto get_value() const -> std::string;
};

/// 标识符表达式，即变量名或函数名。
struct IdExpr : public Expr {
  std::string value;

  explicit IdExpr(std::string value);
  IdExpr(const IdExpr& expr) = default;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto is_id(const std::string& val) const -> bool override {
    return this->value == val;
  }
  auto get_id() -> IdExpr* override { return this; }
};

/// 星号（解包）表达式，如 *args。
/// @li *args
struct StarExpr : public Expr {
  // 要解包的对象。
  ExprPtr what;

  explicit StarExpr(ExprPtr what);
  StarExpr(const StarExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_star() -> StarExpr* override { return this; }
};

/// 代表双星号（关键字解包）表达式，如 **kwargs。
/// @li **kwargs
struct KeywordStarExpr : public Expr {
  // 要解包的对象。
  ExprPtr what;

  explicit KeywordStarExpr(ExprPtr what);
  KeywordStarExpr(const KeywordStarExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 代表元组表达式。
/// @li (1, a)
struct TupleExpr : public Expr {
  // 列表中的元素。
  std::vector<ExprPtr> items;

  explicit TupleExpr(std::vector<ExprPtr> items = {});
  TupleExpr(const TupleExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_tuple() -> TupleExpr* override { return this; }
};

/// 列表表达式。
/// @li [1, 2]
struct ListExpr : public Expr {
  std::vector<ExprPtr> items;

  explicit ListExpr(std::vector<ExprPtr> items);
  ListExpr(const ListExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_list() -> ListExpr* override { return this; }
};

/// 集合表达式。
/// @li {1, 2}
struct SetExpr : public Expr {
  std::vector<ExprPtr> items;

  explicit SetExpr(std::vector<ExprPtr> items);
  SetExpr(const SetExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 字典表达式。
/// @li {'s': 1, 't': 2}
struct DictExpr : public Expr {
  // 字典中的键值对，以 TupleExpr 形式存储。
  std::vector<ExprPtr> items;

  explicit DictExpr(std::vector<ExprPtr> items);
  DictExpr(const DictExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 一个辅助结构体，用于表示生成器或列表推导式的循环部分。
/// [for vars in gen (if conds)...].
/// @li for i in lst if a if b
struct GeneratorBody {
  // 循环变量。
  ExprPtr vars;
  // 生成器或可迭代对象。
  ExprPtr gen;
  // 可选的条件表达式列表，用于过滤生成器或迭代器的元素。
  std::vector<ExprPtr> conds;

  auto clone() const -> GeneratorBody;
};

/// 表示生成器表达式或列表/集合推导式: [(expr (loops...))].
/// @li [i for i in j]
/// @li (f + 1 for j in k if j for f in j)
struct GeneratorExpr : public Expr {
  /// 生成器的类型（普通生成器、列表推导式、集合推导式）。
  enum GeneratorKind : uint8_t { Generator, ListGenerator, SetGenerator };

  GeneratorKind kind;
  // 生成器表达式的主体。
  ExprPtr expr;
  // 包含一个或多个 GeneratorBody 对象的向量，代表生成器或推导式的循环结构。
  std::vector<GeneratorBody> loops;

  GeneratorExpr(GeneratorKind kind, ExprPtr expr,
                std::vector<GeneratorBody> loops);
  GeneratorExpr(const GeneratorExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 表示字典推导式 [{key: expr (loops...)}].
/// @li {i: j for i, j in z.items()}
struct DictGeneratorExpr : public Expr {
  // 分别表示字典键和值的表达式。
  ExprPtr key, expr;
  // 表示字典推导式中的循环结构。
  std::vector<GeneratorBody> loops;

  DictGeneratorExpr(ExprPtr key, ExprPtr expr,
                    std::vector<GeneratorBody> loops);
  DictGeneratorExpr(const DictGeneratorExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 条件表达式。 [cond if ifexpr else elsexpr].
/// @li 1 if a else 2
struct IfExpr : public Expr {
  // 分别表示条件、条件为真时的表达式和条件为假时的表达式。
  ExprPtr cond, ifexpr, elsexpr;

  IfExpr(ExprPtr cond, ExprPtr ifexpr, ExprPtr elsexpr);
  IfExpr(const IfExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_if() -> IfExpr* override { return this; }
};

/// 一元表达式 [op expr].
/// @li -56
struct UnaryExpr : public Expr {
  // 操作符，例如 "-" 或 "!"。
  std::string op;
  // 操作符作用的表达式。
  ExprPtr expr;

  UnaryExpr(std::string op, ExprPtr expr);
  UnaryExpr(const UnaryExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_unary() -> UnaryExpr* override { return this; }
};

/// 二元表达式 [lexpr op rexpr].
/// @li 1 + 2
/// @li 3 or 4
struct BinaryExpr : public Expr {
  // 操作符，例如 "+" 或 "or"。
  std::string op;
  // 分别表示左侧和右侧的表达式。
  ExprPtr lexpr, rexpr;

  // 表明这是一个原地修改表达式，例如 "a += b"。
  bool in_place;

  BinaryExpr(ExprPtr lexpr, std::string op, ExprPtr rexpr,
             bool in_place = false);
  BinaryExpr(const BinaryExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_binary() -> BinaryExpr* override { return this; }
};

/// 链式二元操作表达式。
/// @li 1 <= x <= 2
struct ChainBinaryExpr : public Expr {
  std::vector<std::pair<std::string, ExprPtr>> exprs;

  ChainBinaryExpr(std::vector<std::pair<std::string, ExprPtr>> exprs);
  ChainBinaryExpr(const ChainBinaryExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 管道操作表达式。 [(op expr)...].
/// |>: 表示将左侧表达式的输出作为右侧表达式的输入。
/// ||>: 表示对数据进行并行处理。
/// @li a |> b ||> c
struct PipeExpr : public Expr {
  struct Pipe {
    // 操作符，例如 |> 或 ||>。
    std::string op;
    // 表达式，表示管道操作的输入。
    ExprPtr expr;

    auto clone() const -> Pipe;
  };

  // 包含管道中所有操作的向量。
  std::vector<Pipe> items;
  // 存储每个管道操作输出类型的向量。
  // 对于序列 a |> b |> c，in_types[1] 将是 a |> b 的输出类型。
  std::vector<Pud::Type::TypePtr> in_types;

  explicit PipeExpr(std::vector<Pipe> items);
  PipeExpr(const PipeExpr& expr);

  void validate() const;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 索引操作表达式 (expr[index]).
/// @li a[5]
struct IndexExpr : public Expr {
  // 表示被索引的表达式和索引表达式。
  ExprPtr expr, index;

  IndexExpr(ExprPtr expr, ExprPtr index);
  IndexExpr(const IndexExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_index() -> IndexExpr* override { return this; }
};

/// 表示函数或方法调用表达式 (expr((name=value)...)).
/// @li a(1, b=2)
struct CallExpr : public Expr {
  struct Arg : public SourceObject {
    // 参数名称。
    std::string name;
    // 参数的值。
    ExprPtr value;

    auto clone() const -> Arg;

    Arg(const SourceInfo& info, const std::string& name, ExprPtr value);
    Arg(const std::string& name, ExprPtr value);
    Arg(ExprPtr value);
  };

  // 调用的函数或方法的表达式。
  ExprPtr expr;
  // 函数调用的参数列表。
  std::vector<Arg> args;
  // 指示类型检查器是否已处理并重新排序参数。
  bool ordered;

  CallExpr(ExprPtr expr, std::vector<Arg> args = {});

  CallExpr(ExprPtr expr, std::vector<ExprPtr> args);
  template <typename... Ts>
  CallExpr(ExprPtr expr, ExprPtr arg, Ts... args)
      : CallExpr(expr, std::vector<ExprPtr>{arg, args...}) {}
  CallExpr(const CallExpr& expr);

  void validate() const;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_call() -> CallExpr* override { return this; }
};

/// 表示访问对象成员的表达式 (expr.member).
/// @li a.b
struct DotExpr : public Expr {
  // 被访问对象的表达式。
  ExprPtr expr;
  // 要访问的成员名称。
  std::string member;

  DotExpr(ExprPtr expr, std::string member);
  DotExpr(const std::string& left, std::string member);
  DotExpr(const DotExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_dot() -> DotExpr* override { return this; }
};

/// 表示切片操作 (st:stop:step).
/// @li 1:10:3
/// @li s::-1
/// @li :::
struct SliceExpr : public Expr {
  // 分别代表切片的开始、结束和步长，都是表达式类型。
  // 任何一个都可以是 nullptr，表示切片操作的部分缺失。
  ExprPtr start, stop, step;

  SliceExpr(ExprPtr start, ExprPtr stop, ExprPtr step);
  SliceExpr(const SliceExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 省略号表达式
/// @li ...
struct EllipsisExpr : public Expr {
  // 省略号在不同上下文中的类型，如作为管道表达式中的一部分、部分
  // 参数或单独使用。
  enum EllipsisType : uint8_t { PIPE, PARTIAL, STANDALONE } mode;

  explicit EllipsisExpr(EllipsisType mode = STANDALONE);
  EllipsisExpr(const EllipsisExpr& expr) = default;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_ellipsis() -> EllipsisExpr* override { return this; }
};

/// 表示匿名函数 (lambda (vars)...: expr).
/// @li lambda a, b: a + b
struct LambdaExpr : public Expr {
  std::vector<std::string> vars;
  ExprPtr expr;

  LambdaExpr(std::vector<std::string> vars, ExprPtr expr);
  LambdaExpr(const LambdaExpr&);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 用于表示生成器中的 yield 操作。
/// @li (yield)
struct YieldExpr : public Expr {
  YieldExpr();
  YieldExpr(const YieldExpr& expr) = default;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 赋值表达式 (var := expr).
/// @li a := 5 + 3
struct AssignExpr : public Expr {
  ExprPtr var, expr;

  AssignExpr(ExprPtr var, ExprPtr expr);
  AssignExpr(const AssignExpr&);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// 表示范围表达式 (start ... end).
/// 通常在匹配（match-case）语句中使用。
/// @li 1 ... 2
struct RangeExpr : public Expr {
  ExprPtr start, stop;

  RangeExpr(ExprPtr start, ExprPtr stop);
  RangeExpr(const RangeExpr&);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// (stmts...; expr).
/// 在表达式中嵌入语句，用于支持更复杂的表达式。
/// @li (a = 1; b = 2; a + b)
struct StmtExpr : public Expr {
  // 一系列语句。
  std::vector<std::shared_ptr<Stmt>> stmts;
  // 主表达式，其值将作为整个 StmtExpr 的值。
  ExprPtr expr;

  StmtExpr(std::vector<std::shared_ptr<Stmt>> stmts, ExprPtr expr);
  StmtExpr(std::shared_ptr<Stmt> stmt, ExprPtr expr);
  StmtExpr(std::shared_ptr<Stmt> stmt, std::shared_ptr<Stmt> stmt2,
           ExprPtr expr);
  StmtExpr(const StmtExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_stmt_expr() -> StmtExpr* override { return this; }
};

/// Static tuple indexing expression (expr[index]).
/// @li (1, 2, 3)[2]
struct InstantiateExpr : Expr {
  // 要实例化的类型表达式。
  ExprPtr type_expr;
  // 类型参数列表。
  std::vector<ExprPtr> type_params;

  InstantiateExpr(ExprPtr type_expr, std::vector<ExprPtr> type_params);
  InstantiateExpr(ExprPtr type_expr, ExprPtr type_param);
  InstantiateExpr(const InstantiateExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

enum ExprAttr : uint8_t {
  SequenceItem,
  StarSequenceItem,
  List,
  Set,
  Dict,
  Partial,
  Dominated,
  StarArgument,
  KwStarArgument,
  OrderedCall,
  ExternVar,
  __LAST__
};

auto get_static_generic(Expr* e) -> StaticValue::Type;

}  // namespace Pud::AST

#endif  // PUD_AST_EXPR_H