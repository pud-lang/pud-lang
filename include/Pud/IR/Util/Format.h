#ifndef PUD_IR_UTIL_FORMAT_H
#define PUD_IR_UTIL_FORMAT_H

#include <iostream>

#include "Pud/IR/IR.h"

namespace Pud::IR::Util {

/// Formats an IR node.
/// @param node the node
/// @return the formatted node
auto format(const Node* node) -> std::string;

/// Formats an IR node to an IO stream.
/// @param os the output stream
/// @param node the node
/// @return the resulting output stream
auto format(std::ostream& os, const Node* node) -> std::ostream&;

}  // namespace Pud::IR::Util

#endif  // PUD_IR_UTIL_FORMAT_H