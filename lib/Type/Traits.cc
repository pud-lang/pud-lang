#include "Pud/Type/Traits.h"

namespace Pud::Type {

Trait::Trait(const std::shared_ptr<Type>& type) : Type(type) {}

auto Trait::can_realize() const -> bool { return false; }

auto Trait::is_instantiated() const -> bool { return false; }

auto Trait::realized_name() const -> std::string { return ""; }

CallableTrait::CallableTrait(std::vector<TypePtr> args)
    : args(std::move(args)) {}

auto CallableTrait::unify(Type* typ, Unification* undo) -> int {
  
}

}  // namespace Pud::Type