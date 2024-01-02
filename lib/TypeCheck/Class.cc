#include <string>
#include <tuple>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/TypeCheck/TypeCheck.h"

using fmt::format;

namespace Pud::AST {

/// Parse a class (type) declaration and add a (generic) type to the context.
void TypecheckVisitor::visit(ClassStmt* stmt) {
  // Extensions are not possible after the simplification
  seqassert(!stmt->attributes.has(Attr::Extend), "invalid extension '{}'",
            stmt->name);
  // Type should be constructed only once
  stmt->set_done();

  // Generate the type and add it to the context
  auto typ =
      Type::Type::make_type(ctx->cache, stmt->name, ctx->cache->rev(stmt->name),
                            stmt->is_record())
          ->get_class();
  if (stmt->is_record() && stmt->has_attr("__notuple__"))
    typ->get_record()->no_tuple = true;
  if (stmt->is_record() && startswith(stmt->name, TYPE_PARTIAL)) {
    // Special handling of partial types (e.g., `Partial.0001.foo`)
    if (auto p = in(ctx->cache->partials, stmt->name))
      typ = std::make_shared<Type::PartialType>(typ->get_record(), p->first,
                                                p->second);
  }
  typ->set_source_info(stmt->get_source_info());
  // Classes should always be visible, so add them to the toplevel
  ctx->add_toplevel(stmt->name,
                    std::make_shared<TypecheckItem>(TypecheckItem::Type, typ));

  // Handle generics
  for (const auto& a : stmt->args) {
    if (a.status != Param::Normal) {
      // Generic and static types
      auto generic = ctx->get_unbound();
      generic->is_static = get_static_generic(a.type.get());
      auto typId = generic->id;
      generic->get_link()->generic_name = ctx->cache->rev(a.name);
      if (a.default_value) {
        auto defType = transform_type(clone(a.default_value));
        if (a.status == Param::Generic) {
          generic->default_type = defType->type;
        } else {
          // Hidden generics can be outright replaced (e.g., `T=int`).
          // Unify them immediately.
          unify(defType->type, generic);
        }
      }
      if (auto ti = CAST(a.type, InstantiateExpr)) {
        // Parse TraitVar
        seqassert(ti->type_expr->is_id(TYPE_TYPEVAR),
                  "not a TypeVar instantiation");
        auto l = transform_type(ti->type_params[0])->type;
        if (l->get_link() && l->get_link()->trait)
          generic->get_link()->trait = l->get_link()->trait;
        else
          generic->get_link()->trait = std::make_shared<Type::TypeTrait>(l);
      }
      ctx->add(TypecheckItem::Type, a.name, generic);
      Type::ClassType::Generic g{a.name, ctx->cache->rev(a.name),
                                 generic->generalize(ctx->typecheck_level),
                                 typId};
      if (a.status == Param::Generic) {
        typ->generics.push_back(g);
      } else {
        typ->hidden_generics.push_back(g);
      }
    }
  }

  // Handle class members
  ctx->typecheck_level++;  // to avoid unifying generics early
  auto& fields = ctx->cache->classes[stmt->name].fields;
  for (auto ai = 0, aj = 0; ai < stmt->args.size(); ai++)
    if (stmt->args[ai].status == Param::Normal) {
      fields[aj].type = transform_type(stmt->args[ai].type)
                            ->get_type()
                            ->generalize(ctx->typecheck_level - 1);
      fields[aj].type->set_source_info(stmt->args[ai].type->get_source_info());
      if (stmt->is_record())
        typ->get_record()->args.push_back(fields[aj].type);
      aj++;
    }
  ctx->typecheck_level--;

  // Handle MRO
  for (auto& m : ctx->cache->classes[stmt->name].mro) {
    m = transform_type(m);
  }

  // Generalize generics and remove them from the context
  for (const auto& g : stmt->args)
    if (g.status != Param::Normal) {
      auto generic = ctx->force_find(g.name)->type;
      if (g.status == Param::Generic) {
        // Generalize generics. Hidden generics are linked to the class generics
        // so ignore them
        seqassert(generic && generic->get_link() &&
                      generic->get_link()->kind != Type::LinkType::Link,
                  "generic has been unified");
        generic->get_link()->kind = Type::LinkType::Generic;
      }
      ctx->remove(g.name);
    }

  // Debug information
  LOG_REALIZE("[class] {} -> {}", stmt->name, typ);
  for (auto& m : ctx->cache->classes[stmt->name].fields)
    LOG_REALIZE("       - member: {}: {}", m.name, m.type);
}

/// Generate a tuple class `Tuple[T1,...,TN]`.
/// @param len       Tuple length (`N`)
/// @param name      Tuple name. `Tuple` by default.
///                  Can be something else (e.g., `KwTuple`)
/// @param names     Member names. By default `item1`...`itemN`.
/// @param hasSuffix Set if the tuple name should have `.N` suffix.
std::string TypecheckVisitor::generate_tuple(size_t len,
                                             const std::string& name,
                                             std::vector<std::string> names,
                                             bool hasSuffix) {
  auto key = join(names, ";");
  std::string suffix;
  if (!names.empty()) {
    // Each set of names generates different tuple (i.e., `KwArgs[foo, bar]` is
    // not the same as `KwArgs[bar, baz]`). Cache the names and use an integer
    // for each name combination.
    if (!in(ctx->cache->generated_tuples, key))
      ctx->cache->generated_tuples[key] =
          int(ctx->cache->generated_tuples.size());
    suffix = format("_{}", ctx->cache->generated_tuples[key]);
  } else {
    for (size_t i = 1; i <= len; i++)
      names.push_back(format("item{}", i));
  }

  auto typeName =
      format("{}{}", name, hasSuffix ? format("{}{}", len, suffix) : "");
  if (!ctx->find(typeName)) {
    // Generate the appropriate ClassStmt
    std::vector<Param> args;
    for (size_t i = 0; i < len; i++)
      args.emplace_back(
          Param(names[i], N<IdExpr>(format("T{}", i + 1)), nullptr));
    for (size_t i = 0; i < len; i++)
      args.emplace_back(
          Param(format("T{}", i + 1), N<IdExpr>("type"), nullptr, true));
    StmtPtr stmt =
        N<ClassStmt>(ctx->cache->generate_source_info(), typeName, args,
                     nullptr, std::vector<ExprPtr>{N<IdExpr>("tuple")});

    // Add helpers for KwArgs:
    //   `def __getitem__(self, key: Static[str]): return getattr(self, key)`
    //   `def __contains__(self, key: Static[str]): return hasattr(self, key)`
    auto getItem = N<FunctionStmt>(
        "__getitem__", nullptr,
        std::vector<Param>{
            Param{"self"},
            Param{"key", N<IndexExpr>(N<IdExpr>("Static"), N<IdExpr>("str"))}},
        N<SuiteStmt>(N<ReturnStmt>(N<CallExpr>(
            N<IdExpr>("getattr"), N<IdExpr>("self"), N<IdExpr>("key")))));
    auto contains = N<FunctionStmt>(
        "__contains__", nullptr,
        std::vector<Param>{
            Param{"self"},
            Param{"key", N<IndexExpr>(N<IdExpr>("Static"), N<IdExpr>("str"))}},
        N<SuiteStmt>(N<ReturnStmt>(N<CallExpr>(
            N<IdExpr>("hasattr"), N<IdExpr>("self"), N<IdExpr>("key")))));
    auto getDef = N<FunctionStmt>(
        "get", nullptr,
        std::vector<Param>{
            Param{"self"},
            Param{"key", N<IndexExpr>(N<IdExpr>("Static"), N<IdExpr>("str"))},
            Param{"default", nullptr, N<CallExpr>(N<IdExpr>("NoneType"))}},
        N<SuiteStmt>(N<ReturnStmt>(N<CallExpr>(
            N<DotExpr>(N<IdExpr>("__internal__"), "kwargs_get"),
            N<IdExpr>("self"), N<IdExpr>("key"), N<IdExpr>("default")))));
    if (startswith(typeName, TYPE_KWTUPLE))
      stmt->get_class()->suite = N<SuiteStmt>(getItem, contains, getDef);

    // Add repr for KwArgs:
    //   `def __repr__(self): return __magic__.repr_partial(self)`
    auto repr =
        N<FunctionStmt>("__repr__", nullptr, std::vector<Param>{Param{"self"}},
                        N<SuiteStmt>(N<ReturnStmt>(N<CallExpr>(
                            N<DotExpr>(N<IdExpr>("__magic__"), "repr_partial"),
                            N<IdExpr>("self")))));
    if (startswith(typeName, TYPE_PARTIAL))
      stmt->get_class()->suite = repr;

    // Simplify in the standard library context and type check
    stmt = SimplifyVisitor::apply(ctx->cache->imports[STDLIB_IMPORT].ctx, stmt,
                                  FILE_GENERATED, 0);
    stmt = TypecheckVisitor(ctx).transform(stmt);
    prepend_stmts->push_back(stmt);
  }
  return typeName;
}

}  // namespace Pud::AST