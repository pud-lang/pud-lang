#ifndef PUD_TRANSLATE_CONTEXT_H
#define PUD_TRANSLATE_CONTEXT_H

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Pud/AST/Cache.h"
#include "Pud/AST/Context.h"
#include "Pud/Common/Common.h"
#include "Pud/IR/IR.h"
#include "Pud/IR/Types/Types.h"

namespace Pud::AST {

/**
 * IR context object description.
 * This represents an identifier that can be either a function, a class (type),
 * or a variable.
 */
struct TranslateItem {
  enum Kind { Func, Type, Var } kind;
  /// IR handle.
  union {
    Pud::IR::Var* var;
    Pud::IR::Func* func;
    Pud::IR::Types::Type* type;
  } handle;
  /// Base function pointer.
  Pud::IR::BodiedFunc* base;

  TranslateItem(Kind k, Pud::IR::BodiedFunc* base)
      : kind(k), handle{nullptr}, base(base) {}
  auto get_base() const -> const Pud::IR::BodiedFunc* { return base; }
  auto get_func() const -> Pud::IR::Func* {
    return kind == Func ? handle.func : nullptr;
  }
  auto get_type() const -> Pud::IR::Types::Type* {
    return kind == Type ? handle.type : nullptr;
  }
  auto get_var() const -> Pud::IR::Var* {
    return kind == Var ? handle.var : nullptr;
  }
};

/**
 * A variable table (context) for the IR translation stage.
 */
struct TranslateContext : public Context<TranslateItem> {
  /// A pointer to the shared cache.
  Cache* cache;
  /// Stack of function bases.
  std::vector<Pud::IR::BodiedFunc*> bases;
  /// Stack of IR series (blocks).
  std::vector<Pud::IR::SeriesFlow*> series;
  /// Stack of sequence items for attribute initialization.
  std::vector<std::vector<std::pair<ExprAttr, Pud::IR::Value*>>> seq_items;

 public:
  TranslateContext(Cache* cache);

  using Context<TranslateItem>::add;
  /// Convenience method for adding an object to the context.
  auto add(TranslateItem::Kind kind, const std::string& name, void* type)
      -> std::shared_ptr<TranslateItem>;
  auto find(const std::string& name) const
      -> std::shared_ptr<TranslateItem> override;
  auto force_find(const std::string& name) const
      -> std::shared_ptr<TranslateItem>;

  /// Convenience method for adding a series.
  void add_series(Pud::IR::SeriesFlow* s);
  void pop_series();

 public:
  auto get_module() const -> Pud::IR::Module*;
  auto get_base() const -> Pud::IR::BodiedFunc*;
  auto get_series() const -> Pud::IR::SeriesFlow*;
};

}  // namespace Pud::AST

#endif  // PUD_TRANSLATE_CONTEXT_H