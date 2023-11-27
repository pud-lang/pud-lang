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

}  // namespace Pud::AST