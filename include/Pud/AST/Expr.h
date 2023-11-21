#ifndef PUD_AST_EXPR_H
#define PUD_AST_EXPR_H

#include <memory>

#include "Pud/Common/Source.h"

namespace Pud::AST {

struct Expr : public SourceObject {};

using ExprPtr = std::shared_ptr<Expr>;

struct Param : public SourceObject {};

}  // namespace Pud::AST

#endif  // PUD_AST_EXPR_H