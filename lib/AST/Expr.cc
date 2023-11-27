#include "Pud/AST/Expr.h"

#include <memory>
#include <string>
#include <vector>

#include "Pud/AST/ASTVisitor.h"
#include "Pud/Common/Clone.h"
#include "Pud/Common/Error.h"
#include "Pud/Common/Str.h"

namespace Pud::AST {

StaticValue::StaticValue(StaticValue::Type t)
    : value(), type(t), evaluated(false) {}
StaticValue::StaticValue(int64_t i) : value(i), type(INT), evaluated(true) {}
StaticValue::StaticValue(std::string s)
    : value(std::move(s)), type(STRING), evaluated(true) {}

auto StaticValue::operator==(const StaticValue& s) const -> bool {
  if (type != s.type || s.evaluated != evaluated) {
    return false;
  }
  return !s.evaluated || value == s.value;
}

auto StaticValue::to_string() const -> std::string {
  if (type == StaticValue::NOT_STATIC) {
    return "";
  }
  if (!evaluated) {
    return type == StaticValue::STRING ? "str" : "int";
  }
  return type == StaticValue::STRING
             ? "'" + escape(std::get<std::string>(value)) + "'"
             : std::to_string(std::get<int64_t>(value));
}

auto StaticValue::get_int() const -> int64_t {
  assert(type == StaticValue::INT && "not an int");
  return std::get<int64_t>(value);
}

auto StaticValue::get_string() const -> std::string {
  assert(type == StaticValue::STRING && "not a string");
  return std::get<std::string>(value);
}

Expr::Expr()
    : type(nullptr),
      is_type_expr(false),
      static_value(StaticValue::NOT_STATIC),
      done(false),
      attributes(0),
      orig_expr(nullptr) {}

void Expr::validate() const {}
auto Expr::get_type() const -> Pud::Type::TypePtr { return type; }
void Expr::set_type(Pud::Type::TypePtr t) { this->type = std::move(t); }
auto Expr::is_type() const -> bool { return is_type_expr; }
void Expr::mark_type() { is_type_expr = true; }
auto Expr::wrap_type(const std::string& sexpr) const -> std::string {
  auto is = sexpr;
  if (done) {
    is.insert(find_star(is), "*");
  }
  auto s =
      fmt::format("({}{})", is,
                  type ? fmt::format(" #:type \"{}\"", type->to_string()) : "");
  // if (has_attr(ExprAttr::SequenceItem)) s += "%";
  return s;
}
auto Expr::is_static() const -> bool {
  return static_value.type != StaticValue::NOT_STATIC;
}
auto Expr::has_attr(int attr) const -> bool {
  return (attributes & (1 << attr));
}
void Expr::set_attr(int attr) { attributes |= (1 << attr); }
auto Expr::get_type_name() -> std::string {
  if (get_id()) {
    return get_id()->value;
  } else {
    auto* i = dynamic_cast<InstantiateExpr*>(this);
    assert(i && i->type_expr->get_id() && "bad MRO");
    return i->type_expr->get_id()->value;
  }
}

Param::Param(std::string name, ExprPtr type, ExprPtr default_value, int status)
    : name(std::move(name)),
      type(std::move(type)),
      default_value(std::move(default_value)) {
  if (status == 0 && this->type &&
      (this->type->is_id("type") || this->type->is_id(TYPE_TYPEVAR) ||
       (this->type->get_index() &&
        this->type->get_index()->expr->is_id(TYPE_TYPEVAR)) ||
       get_static_generic(this->type.get()))) {
    this->status = Generic;
  } else {
    this->status =
        (status == 0 ? Normal : (status == 1 ? Generic : HiddenGeneric));
  }
}
Param::Param(const SourceInfo& info, std::string name, ExprPtr type,
             ExprPtr default_value, int status)
    : Param(name, type, default_value, status) {
  set_source_info(info);
}
auto Param::to_string() const -> std::string {
  return fmt::format(
      "({}{}{}{})", name, type ? " #:type " + type->to_string() : "",
      default_value ? " #:default " + default_value->to_string() : "",
      status != Param::Normal ? " #:generic" : "");
}
auto Param::clone() const -> Param {
  return Param(name, ::Pud::clone(type), ::Pud::clone(default_value), status);
}

auto NoneExpr::to_string() const -> std::string { return wrap_type("none"); }
auto NoneExpr::clone() const -> ExprPtr {
  return std::make_shared<NoneExpr>(*this);
}
void NoneExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

BoolExpr::BoolExpr(bool value) : value(value) {
  static_value = StaticValue(value);
}
auto BoolExpr::to_string() const -> std::string {
  return wrap_type(fmt::format("bool {}", static_cast<int>(value)));
}
auto BoolExpr::clone() const -> ExprPtr {
  return std::make_shared<BoolExpr>(*this);
}
void BoolExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

IntExpr::IntExpr(int64_t int_value) : value(std::to_string(int_value)) {
  this->int_value = std::make_unique<int64_t>(int_value);
  static_value = StaticValue(int_value);
}
IntExpr::IntExpr(const std::string& value, std::string suffix)
    : suffix(std::move(suffix)) {
  for (auto c : value) {
    if (c != '_') {
      this->value += c;
    }
  }
  try {
    if (startswith(this->value, "0b") || startswith(this->value, "0B")) {
      int_value = std::make_unique<int64_t>(
          std::stoull(this->value.substr(2), nullptr, 2));
    } else {
      int_value =
          std::make_unique<int64_t>(std::stoull(this->value, nullptr, 0));
    }
  } catch (std::out_of_range&) {
    int_value = nullptr;
  }
}
IntExpr::IntExpr(const IntExpr& expr)
    : Expr(expr), value(expr.value), suffix(expr.suffix) {
  int_value =
      expr.int_value ? std::make_unique<int64_t>(*(expr.int_value)) : nullptr;
}
auto IntExpr::to_string() const -> std::string {
  return wrap_type(fmt::format(
      "int {}{}", value,
      suffix.empty() ? "" : fmt::format(" #:suffix \"{}\"", suffix)));
}
auto IntExpr::clone() const -> ExprPtr {
  return std::make_shared<IntExpr>(*this);
}
void IntExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

FloatExpr::FloatExpr(double float_value)
    : value(fmt::format("{:g}", float_value)) {
  this->float_value = std::make_unique<double>(float_value);
}
FloatExpr::FloatExpr(const std::string& value, std::string suffix)
    : value(value), suffix(std::move(suffix)) {
  try {
    float_value = std::make_unique<double>(std::stod(value));
  } catch (std::out_of_range&) {
    float_value = nullptr;
  }
}
FloatExpr::FloatExpr(const FloatExpr& expr)
    : Expr(expr), value(expr.value), suffix(expr.suffix) {
  float_value = expr.float_value ? std::make_unique<double>(*(expr.float_value))
                                 : nullptr;
}
auto FloatExpr::to_string() const -> std::string {
  return wrap_type(fmt::format(
      "float {}{}", value,
      suffix.empty() ? "" : fmt::format(" #:suffix \"{}\"", suffix)));
}
auto FloatExpr::clone() const -> ExprPtr {
  return std::make_shared<FloatExpr>(*this);
}
void FloatExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

StringExpr::StringExpr(std::vector<std::pair<std::string, std::string>> s)
    : strings(std::move(s)) {
  if (strings.size() == 1 && strings.back().second.empty()) {
    static_value = StaticValue(strings.back().first);
  }
}
StringExpr::StringExpr(std::string value, std::string prefix)
    : StringExpr(
          std::vector<std::pair<std::string, std::string>>{{value, prefix}}) {}
auto StringExpr::to_string() const -> std::string {
  std::vector<std::string> s;
  s.reserve(strings.size());
  for (const auto& vp : strings) {
    s.push_back(fmt::format(
        "\"{}\"{}", escape(vp.first),
        vp.second.empty() ? "" : fmt::format(" #:prefix \"{}\"", vp.second)));
  }
  return wrap_type(fmt::format("string ({})", join(s)));
}
auto StringExpr::get_value() const -> std::string {
  assert(!strings.empty() && "invalid StringExpr");
  return strings[0].first;
}
auto StringExpr::clone() const -> ExprPtr {
  return std::make_shared<StringExpr>(*this);
}
void StringExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

IdExpr::IdExpr(std::string value) : value(std::move(value)) {}
auto IdExpr::to_string() const -> std::string {
  return !type ? fmt::format("'{}", value)
               : wrap_type(fmt::format("'{}", value));
}
auto IdExpr::clone() const -> ExprPtr {
  return std::make_shared<IdExpr>(*this);
}
void IdExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

StarExpr::StarExpr(ExprPtr what) : what(std::move(what)) {}
StarExpr::StarExpr(const StarExpr& expr)
    : Expr(expr), what(::Pud::clone(expr.what)) {}
auto StarExpr::to_string() const -> std::string {
  return wrap_type(fmt::format("star {}", what->to_string()));
}
auto StarExpr::clone() const -> ExprPtr {
  return std::make_shared<StarExpr>(*this);
}
void StarExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

KeywordStarExpr::KeywordStarExpr(ExprPtr what) : what(std::move(what)) {}
KeywordStarExpr::KeywordStarExpr(const KeywordStarExpr& expr)
    : Expr(expr), what(::Pud::clone(expr.what)) {}
std::string KeywordStarExpr::to_string() const {
  return wrap_type(fmt::format("kwstar {}", what->to_string()));
}
auto KeywordStarExpr::clone() const -> ExprPtr {
  return std::make_shared<KeywordStarExpr>(*this);
}
void KeywordStarExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

TupleExpr::TupleExpr(std::vector<ExprPtr> items) : items(std::move(items)) {}
TupleExpr::TupleExpr(const TupleExpr& expr)
    : Expr(expr), items(::Pud::clone(expr.items)) {}
auto TupleExpr::to_string() const -> std::string {
  return wrap_type(fmt::format("tuple {}", combine(items)));
}
auto TupleExpr::clone() const -> ExprPtr {
  return std::make_shared<TupleExpr>(*this);
}
void TupleExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

ListExpr::ListExpr(std::vector<ExprPtr> items) : items(std::move(items)) {}
ListExpr::ListExpr(const ListExpr& expr)
    : Expr(expr), items(::Pud::clone(expr.items)) {}
auto ListExpr::to_string() const -> std::string {
  return wrap_type(!items.empty() ? fmt::format("list {}", combine(items))
                                  : "list");
}
auto ListExpr::clone() const -> ExprPtr {
  return std::make_shared<ListExpr>(*this);
}
void ListExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

SetExpr::SetExpr(std::vector<ExprPtr> items) : items(std::move(items)) {}
SetExpr::SetExpr(const SetExpr& expr)
    : Expr(expr), items(::Pud::clone(expr.items)) {}
auto SetExpr::to_string() const -> std::string {
  return wrap_type(!items.empty() ? fmt::format("set {}", combine(items))
                                  : "set");
}
auto SetExpr::clone() const -> ExprPtr {
  return std::make_shared<SetExpr>(*this);
}
void SetExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

DictExpr::DictExpr(std::vector<ExprPtr> items) : items(std::move(items)) {
  for (auto& i : items) {
    auto* t = i->get_tuple();
    assert(t && t->items.size() == 2 && "dictionary items are invalid");
  }
}
DictExpr::DictExpr(const DictExpr& expr)
    : Expr(expr), items(::Pud::clone(expr.items)) {}
auto DictExpr::to_string() const -> std::string {
  return wrap_type(!items.empty() ? fmt::format("dict {}", combine(items))
                                  : "set");
}
auto DictExpr::clone() const -> ExprPtr {
  return std::make_shared<DictExpr>(*this);
}
void DictExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

auto GeneratorBody::clone() const -> GeneratorBody {
  return {::Pud::clone(vars), ::Pud::clone(gen), ::Pud::clone(conds)};
}

GeneratorExpr::GeneratorExpr(GeneratorExpr::GeneratorKind kind, ExprPtr expr,
                             std::vector<GeneratorBody> loops)
    : kind(kind), expr(std::move(expr)), loops(std::move(loops)) {}
GeneratorExpr::GeneratorExpr(const GeneratorExpr& expr)
    : Expr(expr),
      kind(expr.kind),
      expr(::Pud::clone(expr.expr)),
      loops(::Pud::clone_nop(expr.loops)) {}
auto GeneratorExpr::to_string() const -> std::string {
  std::string prefix;
  if (kind == GeneratorKind::ListGenerator) {
    prefix = "list-";
  }
  if (kind == GeneratorKind::SetGenerator) {
    prefix = "set-";
  }
  std::string s;
  for (const auto& i : loops) {
    std::string q;
    for (const auto& k : i.conds) {
      q += fmt::format(" (if {})", k->to_string());
    }
    s += fmt::format(" (for {} {}{})", i.vars->to_string(), i.gen->to_string(),
                     q);
  }
  return wrap_type(fmt::format("{}gen {}{}", prefix, expr->to_string(), s));
}
auto GeneratorExpr::clone() const -> ExprPtr {
  return std::make_shared<GeneratorExpr>(*this);
}
void GeneratorExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

DictGeneratorExpr::DictGeneratorExpr(ExprPtr key, ExprPtr expr,
                                     std::vector<GeneratorBody> loops)
    : key(std::move(key)), expr(std::move(expr)), loops(std::move(loops)) {}
DictGeneratorExpr::DictGeneratorExpr(const DictGeneratorExpr& expr)
    : Expr(expr),
      key(::Pud::clone(expr.key)),
      expr(::Pud::clone(expr.expr)),
      loops(::Pud::clone_nop(expr.loops)) {}
auto DictGeneratorExpr::to_string() const -> std::string {
  std::string s;
  for (const auto& i : loops) {
    std::string q;
    for (const auto& k : i.conds) {
      q += fmt::format("( if {})", k->to_string());
    }
    s += fmt::format(" (for {} {}{})", i.vars->to_string(), i.gen->to_string(),
                     q);
  }
  return wrap_type(
      fmt::format("dict-gen {} {}{}", key->to_string(), expr->to_string(), s));
}
auto DictGeneratorExpr::clone() const -> ExprPtr {
  return std::make_shared<DictGeneratorExpr>(*this);
}
void DictGeneratorExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

IfExpr::IfExpr(ExprPtr cond, ExprPtr ifexpr, ExprPtr elsexpr)
    : cond(std::move(cond)),
      ifexpr(std::move(ifexpr)),
      elsexpr(std::move(elsexpr)) {}
IfExpr::IfExpr(const IfExpr& expr)
    : Expr(expr),
      cond(::Pud::clone(expr.cond)),
      ifexpr(::Pud::clone(expr.ifexpr)),
      elsexpr(::Pud::clone(expr.elsexpr)) {}
auto IfExpr::to_string() const -> std::string {
  return wrap_type(fmt::format("if-expr {} {} {}", cond->to_string(),
                               ifexpr->to_string(), elsexpr->to_string()));
}
auto IfExpr::clone() const -> ExprPtr {
  return std::make_shared<IfExpr>(*this);
}
void IfExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

UnaryExpr::UnaryExpr(std::string op, ExprPtr expr)
    : op(std::move(op)), expr(std::move(expr)) {}
UnaryExpr::UnaryExpr(const UnaryExpr& expr)
    : Expr(expr), op(expr.op), expr(::Pud::clone(expr.expr)) {}
auto UnaryExpr::to_string() const -> std::string {
  return wrap_type(fmt::format("unary \"{}\" {}", op, expr->to_string()));
}
auto UnaryExpr::clone() const -> ExprPtr {
  return std::make_shared<UnaryExpr>(*this);
}
void UnaryExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

BinaryExpr::BinaryExpr(ExprPtr lexpr, std::string op, ExprPtr rexpr,
                       bool in_place)
    : op(std::move(op)),
      lexpr(std::move(lexpr)),
      rexpr(std::move(rexpr)),
      in_place(in_place) {}
BinaryExpr::BinaryExpr(const BinaryExpr& expr)
    : Expr(expr),
      op(expr.op),
      lexpr(::Pud::clone(expr.lexpr)),
      rexpr(::Pud::clone(expr.rexpr)),
      in_place(expr.in_place) {}
auto BinaryExpr::to_string() const -> std::string {
  return wrap_type(fmt::format("binary \"{}\" {} {}{}", op, lexpr->to_string(),
                               rexpr->to_string(),
                               in_place ? " #:in-place" : ""));
}
auto BinaryExpr::clone() const -> ExprPtr {
  return std::make_shared<BinaryExpr>(*this);
}
void BinaryExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

ChainBinaryExpr::ChainBinaryExpr(
    std::vector<std::pair<std::string, ExprPtr>> exprs)
    : exprs(std::move(exprs)) {}
ChainBinaryExpr::ChainBinaryExpr(const ChainBinaryExpr& expr) : Expr(expr) {
  for (const auto& e : expr.exprs) {
    exprs.emplace_back(make_pair(e.first, ::Pud::clone(e.second)));
  }
}
auto ChainBinaryExpr::to_string() const -> std::string {
  std::vector<std::string> s;
  s.reserve(exprs.size());
  for (const auto& i : exprs) {
    s.push_back(fmt::format("({} \"{}\")", i.first, i.second->to_string()));
  }
  return wrap_type(fmt::format("chain {}", join(s, " ")));
}
auto ChainBinaryExpr::clone() const -> ExprPtr {
  return std::make_shared<ChainBinaryExpr>(*this);
}
void ChainBinaryExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

auto PipeExpr::Pipe::clone() const -> PipeExpr::Pipe {
  return {op, ::Pud::clone(expr)};
}

PipeExpr::PipeExpr(std::vector<PipeExpr::Pipe> items)
    : items(std::move(items)) {
  for (auto& i : this->items) {
    if (auto* call = i.expr->get_call()) {
      for (auto& a : call->args) {
        if (auto* el = a.value->get_ellipsis()) {
          el->mode = EllipsisExpr::PIPE;
        }
      }
    }
  }
}
PipeExpr::PipeExpr(const PipeExpr& expr)
    : Expr(expr),
      items(::Pud::clone_nop(expr.items)),
      in_types(expr.in_types) {}
void PipeExpr::validate() const {}
auto PipeExpr::to_string() const -> std::string {
  std::vector<std::string> s;
  s.reserve(items.size());
  for (const auto& i : items) {
    s.push_back(fmt::format("({} \"{}\")", i.expr->to_string(), i.op));
  }
  return wrap_type(fmt::format("pipe {}", join(s, " ")));
}
auto PipeExpr::clone() const -> ExprPtr {
  return std::make_shared<PipeExpr>(*this);
}
void PipeExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

IndexExpr::IndexExpr(ExprPtr expr, ExprPtr index)
    : expr(std::move(expr)), index(std::move(index)) {}
IndexExpr::IndexExpr(const IndexExpr& expr)
    : Expr(expr),
      expr(::Pud::clone(expr.expr)),
      index(::Pud::clone(expr.index)) {}
auto IndexExpr::to_string() const -> std::string {
  return wrap_type(
      fmt::format("index {} {}", expr->to_string(), index->to_string()));
}
auto IndexExpr::clone() const -> ExprPtr {
  return std::make_shared<IndexExpr>(*this);
}
void IndexExpr::accept(ASTVisitor& visitor) { visitor.visit(this); }

}  // namespace Pud::AST