#include "Pud/Type/Type.h"

namespace Pud::Type {

void Type::Unification::undo() {
  for (size_t i = linked.size(); i-- > 0;) {
    linked[i]->kind = LinkType::Unbound;
    linked[i]->type = nullptr;
  }
  for (size_t i = leveled.size(); i-- > 0;) {
    seqassertn(leveled[i].first->kind == LinkType::Unbound, "not unbound [{}]",
               leveled[i].first->getSrcInfo());
    leveled[i].first->level = leveled[i].second;
  }
  for (auto& t : traits)
    t->trait = nullptr;
}
}  // namespace Pud::Type