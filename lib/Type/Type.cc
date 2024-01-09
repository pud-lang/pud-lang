#include "Pud/Type/Type.h"

#include "Pud/TypeCheck/TypeCheck.h"

namespace Pud::Type {

void Type::Unification::undo() {
  // 撤销链接：对于所有已经链接的类型变量，将其状态
  // 重置为未绑定（Unbound），并清除关联的类型信息。
  for (size_t i = linked.size(); i-- > 0;) {
    linked[i]->kind = LinkType::Unbound;
    linked[i]->type = nullptr;
  }
  // 恢复级别：对于所有已经修改级别的类型变量，将其级别重置为原始值。
  for (size_t i = leveled.size(); i-- > 0;) {
    seqassertn(leveled[i].first->kind == LinkType::Unbound, "not unbound [{}]",
               leveled[i].first->get_source_info());
    leveled[i].first->level = leveled[i].second;
  }
  // 清除特征：对于所有已经关联特征的类型变量，将其特征重置为 nullptr。
  for (auto& t : traits) {
    t->trait = nullptr;
  }
}

Type::Type(const std::shared_ptr<Type>& typ) : cache(typ->cache) {
  set_source_info(typ->get_source_info());
}

Type::Type(AST::Cache* cache, const SourceInfo& info) : cache(cache) {
  set_source_info(info);
}

auto Type::follow() -> std::shared_ptr<Type> { return shared_from_this(); }

auto Type::get_unbounds() const -> std::vector<std::shared_ptr<Type>> {
  return {};
}

auto Type::to_string() const -> std::string { return debug_string(1); }

auto Type::pretty_string() const -> std::string { return debug_string(0); }

auto Type::is(const std::string& s) -> bool {
  return get_class() && get_class()->name == s;
}

auto Type::is_static_type() -> char {
  auto t = follow();
  if (auto s = t->get_static())
    return char(s->expr->static_value.type);
  if (auto l = t->get_link())
    return l->is_static;
  return false;
}

// 根据类型名和标志创建相应的类型实例。
auto Type::make_type(AST::Cache* cache, const std::string& name,
                     const std::string& niceName, bool is_record) -> TypePtr {
  if (name == "Union")
    return std::make_shared<UnionType>(cache);
  if (is_record)
    return std::make_shared<RecordType>(cache, name, niceName);
  return std::make_shared<ClassType>(cache, name, niceName);
}

// 创建一个新的静态类型实例。
auto Type::make_static(AST::Cache* cache, const AST::ExprPtr& expr)
    -> std::shared_ptr<StaticType> {
  return std::make_shared<StaticType>(cache, expr);
}

}  // namespace Pud::Type