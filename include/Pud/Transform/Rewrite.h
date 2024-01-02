#ifndef PUD_TRANSFORM_REWRITE_H
#define PUD_TRANSFORM_REWRITE_H

#include "Pud/IR/Util/Visitor.h"
#include "Pud/Transform/Pass.h"

namespace Pud::IR::Transform {

/// Base for rewrite rules.
class RewriteRule : public Util::Visitor {
 private:
  Value* result = nullptr;

 protected:
  void default_visit(Node*) override {}
  void set_result(Value* r) { result = r; }
  void reset_result() { set_result(nullptr); }
  auto get_result() const -> Value* { return result; }

 public:
  virtual ~RewriteRule() noexcept = default;

  /// Apply the rule.
  /// @param v the value to rewrite
  /// @return nullptr if no rewrite, the replacement otherwise
  auto apply(Value* v) -> Value* {
    v->accept(*this);
    auto* replacement = get_result();
    reset_result();
    return replacement;
  }
};

/// A collection of rewrite rules.
class Rewriter {
 private:
  std::unordered_map<std::string, std::unique_ptr<RewriteRule>> rules;
  int num_replacements = 0;

 public:
  /// Adds a given rewrite rule with the given key.
  /// @param key the rule's key
  /// @param rule the rewrite rule
  void register_rule(const std::string& key,
                     std::unique_ptr<RewriteRule> rule) {
    rules.emplace(std::make_pair(key, std::move(rule)));
  }

  /// Applies all rewrite rules to the given node, and replaces the given
  /// node with the result of the rewrites.
  /// @param v the node to rewrite
  void rewrite(Value* v) {
    Value* result = v;
    for (auto& r : rules) {
      if (auto* rep = r.second->apply(result)) {
        ++num_replacements;
        result = rep;
      }
    }
    if (v != result)
      v->replace_all(result);
  }

  /// @return the number of replacements
  auto get_num_replacements() const -> int { return num_replacements; }

  /// Sets the replacement count to zero.
  void reset() { num_replacements = 0; }
};

}  // namespace Pud::IR::Transform

#endif  // PUD_TRANSFORM_REWRITE_H