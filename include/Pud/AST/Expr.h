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

struct Param : public SourceObject {
  std::string name;
  ExprPtr type;
  ExprPtr default_value;
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

/// None expression.
/// @li None
struct NoneExpr : public Expr {
  NoneExpr();
  NoneExpr(const NoneExpr& expr) = default;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_none() -> NoneExpr* override { return this; }
};

/// Bool expression (value).
/// @li True
struct BoolExpr : public Expr {
  bool value;

  explicit BoolExpr(bool value);
  BoolExpr(const BoolExpr& expr) = default;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Int expression (value.suffix).
/// @li 12
/// @li 13u
/// @li 000_010b
struct IntExpr : public Expr {
  /// Expression value is stored as a string that is parsed during the simplify
  /// stage.
  std::string value;
  /// Number suffix (e.g. "u" for "123u").
  std::string suffix;

  /// Parsed value and sign for "normal" 64-bit integers.
  std::unique_ptr<int64_t> int_value;

  explicit IntExpr(int64_t int_value);
  explicit IntExpr(const std::string& value, std::string suffix = "");
  IntExpr(const IntExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_int() -> IntExpr* override { return this; }
};

/// Float expression (value.suffix).
/// @li 12.1
/// @li 13.15z
/// @li e-12
struct FloatExpr : public Expr {
  /// Expression value is stored as a string that is parsed during the simplify
  /// stage.
  std::string value;
  /// Number suffix (e.g. "u" for "123u").
  std::string suffix;

  /// Parsed value for 64-bit floats.
  std::unique_ptr<double> float_value;

  explicit FloatExpr(double float_value);
  explicit FloatExpr(const std::string& value, std::string suffix = "");
  FloatExpr(const FloatExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// String expression (prefix"value").
/// @li s'ACGT'
/// @li "fff"
struct StringExpr : public Expr {
  // Vector of {value, prefix} strings.
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

/// Identifier expression (value).
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

/// Star (unpacking) expression (*what).
/// @li *args
struct StarExpr : public Expr {
  ExprPtr what;

  explicit StarExpr(ExprPtr what);
  StarExpr(const StarExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_star() -> StarExpr* override { return this; }
};

/// KeywordStar (unpacking) expression (**what).
/// @li **kwargs
struct KeywordStarExpr : public Expr {
  ExprPtr what;

  explicit KeywordStarExpr(ExprPtr what);
  KeywordStarExpr(const KeywordStarExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Tuple expression ((items...)).
/// @li (1, a)
struct TupleExpr : public Expr {
  std::vector<ExprPtr> items;

  explicit TupleExpr(std::vector<ExprPtr> items = {});
  TupleExpr(const TupleExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_tuple() -> TupleExpr* override { return this; }
};

/// List expression ([items...]).
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

/// Set expression ({items...}).
/// @li {1, 2}
struct SetExpr : public Expr {
  std::vector<ExprPtr> items;

  explicit SetExpr(std::vector<ExprPtr> items);
  SetExpr(const SetExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Dictionary expression ({(key: value)...}).
/// Each (key, value) pair is stored as a TupleExpr.
/// @li {'s': 1, 't': 2}
struct DictExpr : public Expr {
  std::vector<ExprPtr> items;

  explicit DictExpr(std::vector<ExprPtr> items);
  DictExpr(const DictExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Generator body node helper [for vars in gen (if conds)...].
/// @li for i in lst if a if b
struct GeneratorBody {
  ExprPtr vars;
  ExprPtr gen;
  std::vector<ExprPtr> conds;

  auto clone() const -> GeneratorBody;
};

/// Generator or comprehension expression [(expr (loops...))].
/// @li [i for i in j]
/// @li (f + 1 for j in k if j for f in j)
struct GeneratorExpr : public Expr {
  /// Generator kind: normal generator, list comprehension, set comprehension.
  enum GeneratorKind : uint8_t { Generator, ListGenerator, SetGenerator };

  GeneratorKind kind;
  ExprPtr expr;
  std::vector<GeneratorBody> loops;

  GeneratorExpr(GeneratorKind kind, ExprPtr expr,
                std::vector<GeneratorBody> loops);
  GeneratorExpr(const GeneratorExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Dictionary comprehension expression [{key: expr (loops...)}].
/// @li {i: j for i, j in z.items()}
struct DictGeneratorExpr : public Expr {
  ExprPtr key, expr;
  std::vector<GeneratorBody> loops;

  DictGeneratorExpr(ExprPtr key, ExprPtr expr,
                    std::vector<GeneratorBody> loops);
  DictGeneratorExpr(const DictGeneratorExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Conditional expression [cond if ifexpr else elsexpr].
/// @li 1 if a else 2
struct IfExpr : public Expr {
  ExprPtr cond, ifexpr, elsexpr;

  IfExpr(ExprPtr cond, ExprPtr ifexpr, ExprPtr elsexpr);
  IfExpr(const IfExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_if() -> IfExpr* override { return this; }
};

/// Unary expression [op expr].
/// @li -56
struct UnaryExpr : public Expr {
  std::string op;
  ExprPtr expr;

  UnaryExpr(std::string op, ExprPtr expr);
  UnaryExpr(const UnaryExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_unary() -> UnaryExpr* override { return this; }
};

/// Binary expression [lexpr op rexpr].
/// @li 1 + 2
/// @li 3 or 4
struct BinaryExpr : public Expr {
  std::string op;
  ExprPtr lexpr, rexpr;

  /// True if an expression modifies lhs in-place (e.g. a += b).
  bool in_place;

  BinaryExpr(ExprPtr lexpr, std::string op, ExprPtr rexpr,
             bool in_place = false);
  BinaryExpr(const BinaryExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_binary() -> BinaryExpr* override { return this; }
};

/// Chained binary expression.
/// @li 1 <= x <= 2
struct ChainBinaryExpr : public Expr {
  std::vector<std::pair<std::string, ExprPtr>> exprs;

  ChainBinaryExpr(std::vector<std::pair<std::string, ExprPtr>> exprs);
  ChainBinaryExpr(const ChainBinaryExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Pipe expression [(op expr)...].
/// op is either "" (only the first item), "|>" or "||>".
/// @li a |> b ||> c
struct PipeExpr : public Expr {
  struct Pipe {
    std::string op;
    ExprPtr expr;

    Pipe clone() const;
  };

  std::vector<Pipe> items;
  /// Output type of a "prefix" pipe ending at the index position.
  /// Example: for a |> b |> c, inTypes[1] is typeof(a |> b).
  std::vector<Pud::Type::TypePtr> inTypes;

  explicit PipeExpr(std::vector<Pipe> items);
  PipeExpr(const PipeExpr& expr);

  void validate() const;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Index expression (expr[index]).
/// @li a[5]
struct IndexExpr : public Expr {
  ExprPtr expr, index;

  IndexExpr(ExprPtr expr, ExprPtr index);
  IndexExpr(const IndexExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_index() -> IndexExpr* override { return this; }
};

/// Call expression (expr((name=value)...)).
/// @li a(1, b=2)
struct CallExpr : public Expr {
  /// Each argument can have a name (e.g. foo(1, b=5))
  struct Arg : public SourceObject {
    std::string name;
    ExprPtr value;

    auto clone() const -> Arg;

    Arg(const SourceInfo& info, const std::string& name, ExprPtr value);
    Arg(const std::string& name, ExprPtr value);
    Arg(ExprPtr value);
  };

  ExprPtr expr;
  std::vector<Arg> args;
  /// True if type-checker has processed and re-ordered args.
  bool ordered;

  CallExpr(ExprPtr expr, std::vector<Arg> args = {});
  /// Convenience constructors
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

/// Dot (access) expression (expr.member).
/// @li a.b
struct DotExpr : public Expr {
  ExprPtr expr;
  std::string member;

  DotExpr(ExprPtr expr, std::string member);
  /// Convenience constructor.
  DotExpr(const std::string& left, std::string member);
  DotExpr(const DotExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_dot() -> DotExpr* override { return this; }
};

/// Slice expression (st:stop:step).
/// @li 1:10:3
/// @li s::-1
/// @li :::
struct SliceExpr : public Expr {
  /// Any of these can be nullptr to account for partial slices.
  ExprPtr start, stop, step;

  SliceExpr(ExprPtr start, ExprPtr stop, ExprPtr step);
  SliceExpr(const SliceExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Ellipsis expression.
/// @li ...
struct EllipsisExpr : public Expr {
  /// True if this is a target partial argument within a PipeExpr.
  /// If true, this node will be handled differently during the type-checking
  /// stage.
  enum EllipsisType : uint8_t { PIPE, PARTIAL, STANDALONE } mode;

  explicit EllipsisExpr(EllipsisType mode = STANDALONE);
  EllipsisExpr(const EllipsisExpr& expr) = default;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;

  auto get_ellipsis() -> EllipsisExpr* override { return this; }
};

/// Lambda expression (lambda (vars)...: expr).
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

/// Yield (send to generator) expression.
/// @li (yield)
struct YieldExpr : public Expr {
  YieldExpr();
  YieldExpr(const YieldExpr& expr) = default;

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Assignment (walrus) expression (var := expr).
/// @li a := 5 + 3
struct AssignExpr : public Expr {
  ExprPtr var, expr;

  AssignExpr(ExprPtr var, ExprPtr expr);
  AssignExpr(const AssignExpr&);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// Range expression (start ... end).
/// Used only in match-case statements.
/// @li 1 ... 2
struct RangeExpr : public Expr {
  ExprPtr start, stop;

  RangeExpr(ExprPtr start, ExprPtr stop);
  RangeExpr(const RangeExpr&);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

/// The following nodes are created after the simplify stage.

/// Statement expression (stmts...; expr).
/// Statements are evaluated only if the expression is evaluated
/// (to support short-circuiting).
/// @li (a = 1; b = 2; a + b)
struct StmtExpr : public Expr {
  std::vector<std::shared_ptr<Stmt>> stmts;
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
  ExprPtr type_expr;
  std::vector<ExprPtr> type_params;

  InstantiateExpr(ExprPtr type_expr, std::vector<ExprPtr> type_params);
  /// Convenience constructor for a single type parameter.
  InstantiateExpr(ExprPtr type_expr, ExprPtr type_param);
  InstantiateExpr(const InstantiateExpr& expr);

  auto to_string() const -> std::string override;
  auto clone() const -> ExprPtr override;
  void accept(ASTVisitor& visitor) override;
};

enum class ExprAttr : uint8_t {
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