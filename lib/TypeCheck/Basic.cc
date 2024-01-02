#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

namespace Pud::AST {

/// Set type to `Optional[?]`
void TypecheckVisitor::visit(NoneExpr* expr) {
  unify(expr->type, ctx->instantiate(ctx->get_type(TYPE_OPTIONAL)));
  if (realize(expr->type)) {
    // Realize the appropriate `Optional.__new__` for the translation stage
    auto cls = expr->type->get_class();
    auto f = ctx->force_find(TYPE_OPTIONAL ".__new__:0")->type;
    auto t = realize(ctx->instantiate(f, cls)->get_func());
    expr->set_done();
  }
}

/// Set type to `bool`
void TypecheckVisitor::visit(BoolExpr* expr) {
  unify(expr->type, ctx->get_type("bool"));
  expr->set_done();
}

/// Set type to `int`
void TypecheckVisitor::visit(IntExpr* expr) {
  unify(expr->type, ctx->get_type("int"));
  expr->set_done();
}

/// Set type to `float`
void TypecheckVisitor::visit(FloatExpr* expr) {
  unify(expr->type, ctx->get_type("float"));
  expr->set_done();
}

/// Set type to `str`
void TypecheckVisitor::visit(StringExpr* expr) {
  unify(expr->type, ctx->get_type("str"));
  expr->set_done();
}

}  // namespace Pud::AST