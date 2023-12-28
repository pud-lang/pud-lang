#include "Pud/IR/Const.h"

namespace Pud::IR {

const char Const::NodeId = 0;

auto Const::do_replace_used_type(const std::string& name, Types::Type* new_type)
    -> int {
  if (type->get_name() == name) {
    type = new_type;
    return 1;
  }
  return 0;
}

const char TemplatedConst<std::string>::NodeId = 0;

}  // namespace Pud::IR