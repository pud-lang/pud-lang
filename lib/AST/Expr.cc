#include "Pud/AST/Expr.h"

#include <memory>
#include <string>
#include <vector>

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
std::string Expr::wrap_type(const std::string& sexpr) const {
  auto is = sexpr;
  if (done) {
    is.insert(findStar(is), "*");
  }
  auto s = format("({}{})", is,
                  type ? format(" #:type \"{}\"", type->toString()) : "");
  // if (hasAttr(ExprAttr::SequenceItem)) s += "%";
  return s;
}
bool Expr::isStatic() const {
  return staticValue.type != StaticValue::NOT_STATIC;
}
bool Expr::hasAttr(int attr) const { return (attributes & (1 << attr)); }
void Expr::setAttr(int attr) { attributes |= (1 << attr); }
std::string Expr::getTypeName() {
  if (getId()) {
    return getId()->value;
  } else {
    auto i = dynamic_cast<InstantiateExpr*>(this);
    seqassertn(i && i->typeExpr->getId(), "bad MRO");
    return i->typeExpr->getId()->value;
  }
}

}  // namespace Pud::AST