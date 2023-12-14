#include "Pud/Type/Static.h"

#include <memory>
#include <string>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Format/Format.h"
#include "Pud/TypeCheck/TypeCheck.h"

namespace Pud::Type {

StaticType::StaticType(AST::Cache* cache, const std::shared_ptr<AST::Expr>& e)
    : Type(cache), expr(e->clone()) {
  // 如果表达式未被静态求值或未被求值，它将调用 parse_expr 方法来分析
  // 和填充 generics。
  if (!expr->is_static() || !expr->static_value.evaluated) {
    std::unordered_set<std::string> seen;
    parse_expr(expr, seen);
  }
}

StaticType::StaticType(AST::Cache* cache,
                       std::vector<ClassType::Generic> generics,
                       const std::shared_ptr<AST::Expr>& e)
    : Type(cache), generics(std::move(generics)), expr(e->clone()) {}

StaticType::StaticType(AST::Cache* cache, int64_t i)
    : Type(cache), expr(std::make_shared<AST::IntExpr>(i)) {}

StaticType::StaticType(AST::Cache* cache, const std::string& s)
    : Type(cache), expr(std::make_shared<AST::StringExpr>(s)) {}

// 用于统一两个静态类型。
auto StaticType::unify(Type* typ, Unification* undo) -> int {
  if (auto t = typ->get_static()) {
    // 检查当前实例 (this) 或传入的类型 t 是否可以实现（can_realize()）
    // 则对其表达式进行求值。
    if (can_realize()) {
      expr->static_value = evaluate();
    }
    if (t->can_realize()) {
      t->expr->static_value = t->evaluate();
    }
    // 检查两个类型的求值结果类型是否相同。
    if (expr->static_value.type != t->expr->static_value.type) {
      return -1;
    }
    // 如果两个 StaticType 实例都已求值，则直接比较它们的值。
    if (expr->static_value.evaluated && t->expr->static_value.evaluated) {
      return expr->static_value == t->expr->static_value ? 2 : -1;
    } else if (expr->static_value.evaluated &&
               !t->expr->static_value.evaluated) {
      // 如果当前实例已求值但传入的类型未求值，尝试反向统一。
      return typ->unify(this, undo);
    }

    // 如果当前实例是一个简单的标识符表达式，并且传入类型已求值，
    // 则尝试对泛型进行统一。
    if (expr->get_id() && t->expr->static_value.evaluated) {
      return generics[0].type->unify(typ, undo);
    }

    // 如果当前实例是一个复杂表达式（如 A+1），则比较两个类型的泛型大小。
    // seqassert(!generics.empty(), "unevaluated simple expression");
    if (generics.size() != t->generics.size()) {
      return -1;
    }

    int s1 = 2;
    int s = 0;
    // 如果表达式形式不相同
    if (!(expr->get_id() && t->expr->get_id()) &&
        expr->to_string() != t->expr->to_string()) {
      return -1;
    }
    // 遍历泛型，尝试逐个统一。
    for (int i = 0; i < generics.size(); i++) {
      s = generics[i].type->unify(t->generics[i].type.get(), undo);
      if (s == -1) {
        return -1;
      }
      s1 += s;
    }
    return s1;
  } else if (auto tl = typ->get_link()) {
    return tl->unify(this, undo);
  }
  return -1;
}

auto StaticType::generalize(int at_level) -> TypePtr {
  auto e = generics;
  for (auto& t : e)
    t.type = t.type ? t.type->generalize(at_level) : nullptr;
  auto c = std::make_shared<StaticType>(cache, e, expr);
  c->set_source_info(get_source_info());
  return c;
}

auto StaticType::instantiate(int at_level, int* unbound_count,
                             std::unordered_map<int, TypePtr>* cache)
    -> TypePtr {
  auto e = generics;
  for (auto& t : e)
    t.type =
        t.type ? t.type->instantiate(at_level, unbound_count, cache) : nullptr;
  auto c = std::make_shared<StaticType>(this->cache, e, expr);
  c->set_source_info(get_source_info());
  return c;
}

auto StaticType::get_unbounds() const -> std::vector<TypePtr> {
  std::vector<TypePtr> u;
  for (auto& t : generics)
    if (t.type) {
      auto tu = t.type->get_unbounds();
      u.insert(u.begin(), tu.begin(), tu.end());
    }
  return u;
}

auto StaticType::can_realize() const -> bool {
  if (!expr->static_value.evaluated)
    for (auto& t : generics)
      if (t.type && !t.type->can_realize())
        return false;
  return true;
}

auto StaticType::is_instantiated() const -> bool {
  return expr->static_value.evaluated;
}

auto StaticType::debug_string(char mode) const -> std::string {
  if (expr->static_value.evaluated)
    return expr->static_value.to_string();
  if (mode == 2) {
    std::vector<std::string> s;
    for (auto& g : generics)
      s.push_back(g.type->debug_string(mode));
    return fmt::format("Static[{};{}]", join(s, ","), expr->to_string());
  } else {
    return fmt::format("Static[{}]", AST::FormatVisitor::apply(expr));
  }
}

auto StaticType::realized_name() const -> std::string {
  // seqassert(can_realize(), "cannot realize {}", to_string());
  std::vector<std::string> deps;
  for (auto& e : generics)
    deps.push_back(e.type->realized_name());
  if (!expr->static_value.evaluated)  // If not already evaluated, evaluate!
    const_cast<StaticType*>(this)->expr->static_value = evaluate();
  // seqassert(expr->static_value.evaluated, "static value not evaluated");
  return expr->static_value.to_string();
}

auto StaticType::evaluate() const -> AST::StaticValue {
  if (expr->static_value.evaluated) {
    return expr->static_value;
  }
  cache->type_ctx->add_block();
  for (auto& g : generics) {
    cache->type_ctx->add(TypecheckItem::Type, g.name, g.type);
  }
  auto old_changed_nodes = cache->type_ctx->changed_nodes;
  auto en = TypecheckVisitor(cache->type_ctx).transform(expr->clone());
  cache->type_ctx->changed_nodes = old_changed_nodes;
  // seqassert(en->is_static() && en->static_value.evaluated,
  //           "{} cannot be evaluated", en);
  cache->type_ctx->pop_block();
  return en->static_value;
}

void StaticType::parse_expr(const AST::ExprPtr& e,
                            std::unordered_set<std::string>& seen) {
  e->type = nullptr;
  if (auto ei = e->get_id()) {
    if (!in(seen, ei->value)) {
      auto val = cache->type_ctx->find(ei->value);
      // seqassert(val && val->type->is_static_type(), "invalid static
      // expression");
      auto gen_typ = val->type->follow();
      auto id = gen_typ->get_link() ? gen_typ->get_link()->id
                : gen_typ->get_static()->generics.empty()
                    ? 0
                    : gen_typ->get_static()->generics[0].id;
      generics.emplace_back(ClassType::Generic(
          ei->value,
          cache->type_ctx->cache->reverse_identifier_lookup[ei->value], gen_typ,
          id));
      seen.insert(ei->value);
    }
  } else if (auto eu = e->get_unary()) {
    parse_expr(eu->expr, seen);
  } else if (auto eb = e->get_binary()) {
    parse_expr(eb->lexpr, seen);
    parse_expr(eb->rexpr, seen);
  } else if (auto ef = e->get_if()) {
    parse_expr(ef->cond, seen);
    parse_expr(ef->ifexpr, seen);
    parse_expr(ef->elsexpr, seen);
  }
}

}  // namespace Pud::Type