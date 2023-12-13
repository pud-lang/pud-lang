#include "Pud/Type/Static.h"

#include <memory>
#include <string>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"

namespace Pud::Type {

StaticType::StaticType(AST::Cache* cache, const std::shared_ptr<AST::Expr>& e)
    : Type(cache), expr(e->clone()) {
  if (!expr->is_static() || !expr->static_value.evaluated) {
    std::unordered_set<std::string> seen;
    parse_expr(expr, seen);
  }
}

StaticType::StaticType(AST::Cache* cache, std::vector<ClassType::Generic> generics,
                       const std::shared_ptr<AST::Expr>& e)
    : Type(cache), generics(std::move(generics)), expr(e->clone()) {}

StaticType::StaticType(AST::Cache* cache, int64_t i)
    : Type(cache), expr(std::make_shared<AST::IntExpr>(i)) {}

StaticType::StaticType(AST::Cache* cache, const std::string& s)
    : Type(cache), expr(std::make_shared<AST::StringExpr>(s)) {}

int StaticType::unify(Type* typ, Unification* us) {
  if (auto t = typ->get_static()) {
    if (can_realize())
      expr->static_value = evaluate();
    if (t->can_realize())
      t->expr->static_value = t->evaluate();
    // Check if both types are already evaluated.
    if (expr->static_value.type != t->expr->static_value.type)
      return -1;
    if (expr->static_value.evaluated && t->expr->static_value.evaluated)
      return expr->static_value == t->expr->static_value ? 2 : -1;
    else if (expr->static_value.evaluated && !t->expr->static_value.evaluated)
      return typ->unify(this, us);

    // Right now, *this is not evaluated
    // Let us see can we unify it with other _if_ it is a simple IdExpr?
    if (expr->get_id() && t->expr->static_value.evaluated) {
      return generics[0].type->unify(typ, us);
    }

    // At this point, *this is a complex expression (e.g. A+1).
    //seqassert(!generics.empty(), "unevaluated simple expression");
    if (generics.size() != t->generics.size())
      return -1;

    int s1 = 2, s = 0;
    if (!(expr->get_id() && t->expr->get_id()) &&
        expr->to_string() != t->expr->to_string())
      return -1;
    for (int i = 0; i < generics.size(); i++) {
      if ((s = generics[i].type->unify(t->generics[i].type.get(), us)) == -1)
        return -1;
      s1 += s;
    }
    return s1;
  } else if (auto tl = typ->get_link()) {
    return tl->unify(this, us);
  }
  return -1;
}

TypePtr StaticType::generalize(int atLevel) {
  auto e = generics;
  for (auto& t : e)
    t.type = t.type ? t.type->generalize(atLevel) : nullptr;
  auto c = std::make_shared<StaticType>(cache, e, expr);
  c->set_source_info(get_source_info());
  return c;
}

TypePtr StaticType::instantiate(int atLevel, int* unboundCount,
                                std::unordered_map<int, TypePtr>* cache) {
  auto e = generics;
  for (auto& t : e)
    t.type =
        t.type ? t.type->instantiate(atLevel, unboundCount, cache) : nullptr;
  auto c = std::make_shared<StaticType>(this->cache, e, expr);
  c->set_source_info(get_source_info());
  return c;
}

std::vector<TypePtr> StaticType::get_unbounds() const {
  std::vector<TypePtr> u;
  for (auto& t : generics)
    if (t.type) {
      auto tu = t.type->get_unbounds();
      u.insert(u.begin(), tu.begin(), tu.end());
    }
  return u;
}

bool StaticType::can_realize() const {
  if (!expr->static_value.evaluated)
    for (auto& t : generics)
      if (t.type && !t.type->can_realize())
        return false;
  return true;
}

bool StaticType::isInstantiated() const { return expr->static_value.evaluated; }

std::string StaticType::debug_string(char mode) const {
  if (expr->static_value.evaluated)
    return expr->static_value.to_string();
  if (mode == 2) {
    std::vector<std::string> s;
    for (auto& g : generics)
      s.push_back(g.type->debug_string(mode));
    return fmt::format("Static[{};{}]", join(s, ","), expr->to_string());
  } else {
    return fmt::format("Static[{}]", FormatVisitor::apply(expr));
  }
}

std::string StaticType::realized_name() const {
  seqassert(can_realize(), "cannot realize {}", to_string());
  std::vector<std::string> deps;
  for (auto& e : generics)
    deps.push_back(e.type->realized_name());
  if (!expr->static_value.evaluated)  // If not already evaluated, evaluate!
    const_cast<StaticType*>(this)->expr->static_value = evaluate();
  seqassert(expr->static_value.evaluated, "static value not evaluated");
  return expr->static_value.to_string();
}

StaticValue StaticType::evaluate() const {
  if (expr->static_value.evaluated)
    return expr->static_value;
  cache->typeCtx->addBlock();
  for (auto& g : generics)
    cache->typeCtx->add(TypecheckItem::Type, g.name, g.type);
  auto oldChangedNodes = cache->typeCtx->changedNodes;
  auto en = TypecheckVisitor(cache->typeCtx).transform(expr->clone());
  cache->typeCtx->changedNodes = oldChangedNodes;
  seqassert(en->isStatic() && en->static_value.evaluated,
            "{} cannot be evaluated", en);
  cache->typeCtx->popBlock();
  return en->static_value;
}

void StaticType::parseExpr(const ExprPtr& e,
                           std::unordered_set<std::string>& seen) {
  e->type = nullptr;
  if (auto ei = e->get_id()) {
    if (!in(seen, ei->value)) {
      auto val = cache->typeCtx->find(ei->value);
      seqassert(val && val->type->isStaticType(), "invalid static expression");
      auto genTyp = val->type->follow();
      auto id = genTyp->getLink() ? genTyp->getLink()->id
                : genTyp->getStatic()->generics.empty()
                    ? 0
                    : genTyp->getStatic()->generics[0].id;
      generics.emplace_back(ClassType::Generic(
          ei->value, cache->typeCtx->cache->reverseIdentifierLookup[ei->value],
          genTyp, id));
      seen.insert(ei->value);
    }
  } else if (auto eu = e->getUnary()) {
    parseExpr(eu->expr, seen);
  } else if (auto eb = e->getBinary()) {
    parseExpr(eb->lexpr, seen);
    parseExpr(eb->rexpr, seen);
  } else if (auto ef = e->getIf()) {
    parseExpr(ef->cond, seen);
    parseExpr(ef->ifexpr, seen);
    parseExpr(ef->elsexpr, seen);
  }
}

}  // namespace Pud::Type