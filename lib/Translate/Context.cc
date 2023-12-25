#include "Pud/Translate/Context.h"

#include <memory>
#include <vector>

#include "Pud/AST/Context.h"
#include "Pud/Common/Common.h"
#include "Pud/Translate/Translate.h"
#include "Pud/TypeCheck/Context.h"

namespace Pud::AST {

TranslateContext::TranslateContext(Cache* cache)
    : Context<TranslateItem>(""), cache(cache) {}

auto TranslateContext::find(const std::string& name) const
    -> std::shared_ptr<TranslateItem> {
  if (auto t = Context<TranslateItem>::find(name))
    return t;
  std::shared_ptr<TranslateItem> ret = nullptr;
  auto tt = cache->type_ctx->find(name);
  if (tt && tt->is_type() && tt->type->can_realize()) {
    ret = std::make_shared<TranslateItem>(TranslateItem::Type, bases[0]);
    seqassertn(
        in(cache->classes, tt->type->get_class()->name) &&
            in(cache->classes[tt->type->get_class()->name].realizations, name),
        "cannot find type realization {}", name);
    ret->handle.type =
        cache->classes[tt->type->get_class()->name].realizations[name]->ir;
  } else if (tt && tt->type->get_func() && tt->type->can_realize()) {
    ret = std::make_shared<TranslateItem>(TranslateItem::Func, bases[0]);
    seqassertn(
        in(cache->functions, tt->type->get_func()->ast->name) &&
            in(cache->functions[tt->type->get_func()->ast->name].realizations,
               name),
        "cannot find type realization {}", name);
    ret->handle.func = cache->functions[tt->type->get_func()->ast->name]
                           .realizations[name]
                           ->ir;
  }
  return ret;
}

auto TranslateContext::force_find(const std::string& name) const
    -> std::shared_ptr<TranslateItem> {
  auto i = find(name);
  seqassertn(i, "cannot find '{}'", name);
  return i;
}

auto TranslateContext::add(TranslateItem::Kind kind, const std::string& name,
                           void* type) -> std::shared_ptr<TranslateItem> {
  auto it = std::make_shared<TranslateItem>(kind, get_base());
  if (kind == TranslateItem::Var)
    it->handle.var = (IR::Var*)type;
  else if (kind == TranslateItem::Func)
    it->handle.func = (IR::Func*)type;
  else
    it->handle.type = (IR::Types::Type*)type;
  add(name, it);
  return it;
}

void TranslateContext::add_series(Pud::IR::SeriesFlow* s) {
  series.push_back(s);
}
void TranslateContext::pop_series() { series.pop_back(); }

auto TranslateContext::get_module() const -> Pud::IR::Module* {
  return dynamic_cast<Pud::IR::Module*>(bases[0]->get_module());
}
auto TranslateContext::get_base() const -> Pud::IR::BodiedFunc* {
  return bases.back();
}
auto TranslateContext::get_series() const -> Pud::IR::SeriesFlow* {
  return series.back();
}

}  // namespace Pud::AST