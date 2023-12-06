#ifndef PUD_IR_ATTRIBUTE_H
#define PUD_IR_ATTRIBUTE_H

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "Pud/Common/Source.h"

namespace Pud::IR {

class Func;
class Value;

namespace Util {
class CloneVisitor;
}

struct Attribute {
  virtual ~Attribute() noexcept = default;

  virtual auto needs_clone() const -> bool { return true; }

  friend auto operator<<(std::ostream& os, const Attribute& a)
      -> std::ostream& {
    return a.do_format(os);
  }

  virtual auto clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> = 0;

  virtual auto force_clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> {
    return clone(cv);
  }

 private:
  virtual auto do_format(std::ostream& os) const -> std::ostream& = 0;
};

struct SourceInfoAttribute : public Attribute {
  static const std::string AttributeName;

  Pud::SourceInfo info;

  SourceInfoAttribute() = default;

  explicit SourceInfoAttribute(Pud::SourceInfo info) : info(std::move(info)) {}

  auto clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override {
    return std::make_unique<SourceInfoAttribute>(*this);
  }

 private:
  auto do_format(std::ostream& os) const -> std::ostream& override {
    return os << info;
  }
};

struct DocstringAttribute : public Attribute {
  static const std::string AttributeName;

  std::string docstring;

  DocstringAttribute() = default;

  explicit DocstringAttribute(std::string docstring)
      : docstring(std::move(docstring)) {}

  auto clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override {
    return std::make_unique<DocstringAttribute>(*this);
  }

 private:
  auto do_format(std::ostream& os) const -> std::ostream& override {
    return os << docstring;
  }
};

struct KeyValueAttribute : public Attribute {
  static const std::string AttributeName;

  // attributes map
  std::map<std::string, std::string> attributes;

  KeyValueAttribute() = default;

  explicit KeyValueAttribute(std::map<std::string, std::string> attributes)
      : attributes(std::move(attributes)) {}

  auto has(const std::string& key) const -> bool;

  auto get(const std::string& key) const -> std::string;

  auto clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override {
    return std::make_unique<KeyValueAttribute>(*this);
  }

 private:
  auto do_format(std::ostream& os) const -> std::ostream& override;
};

struct MemberAttribute : public Attribute {
  static const std::string AttributeName;

  // member source info map
  std::map<std::string, SourceInfo> member_source_info;

  MemberAttribute() = default;

  explicit MemberAttribute(std::map<std::string, SourceInfo> member_source_info)
      : member_source_info(std::move(member_source_info)) {}

  auto clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override {
    return std::make_unique<MemberAttribute>(*this);
  }

 private:
  auto do_format(std::ostream& os) const -> std::ostream& override;
};

struct TupleLiteralAttribute : public Attribute {
  static const std::string AttributeName;

  /// values contained in tuple literal
  std::vector<Value*> elements;

  explicit TupleLiteralAttribute(std::vector<Value*> elements)
      : elements(std::move(elements)) {}

  auto clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override;
  auto force_clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override;

 private:
  auto do_format(std::ostream& os) const -> std::ostream& override;
};

/// Information about an element in a collection literal
struct LiteralElement {
  /// the element value
  Value* value;
  /// true if preceded by "*", as in "[*x]"
  bool star;
};

struct ListLiteralAttribute : public Attribute {
  static const std::string AttributeName;

  /// elements contained in list literal
  std::vector<LiteralElement> elements;

  explicit ListLiteralAttribute(std::vector<LiteralElement> elements)
      : elements(std::move(elements)) {}

  auto clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override;
  auto force_clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override;

 private:
  auto do_format(std::ostream& os) const -> std::ostream& override;
};

/// Attribute attached to IR structures corresponding to set literals
struct SetLiteralAttribute : public Attribute {
  static const std::string AttributeName;

  /// elements contained in set literal
  std::vector<LiteralElement> elements;

  explicit SetLiteralAttribute(std::vector<LiteralElement> elements)
      : elements(std::move(elements)) {}

  auto clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override;
  auto force_clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override;

 private:
  auto do_format(std::ostream& os) const -> std::ostream& override;
};

/// Attribute attached to IR structures corresponding to dict literals
struct DictLiteralAttribute : public Attribute {
  struct KeyValuePair {
    /// the key in the literal
    Value* key;
    /// the value in the literal, or null if key is being star-unpacked
    Value* value;
  };

  static const std::string AttributeName;

  /// keys and values contained in dict literal
  std::vector<KeyValuePair> elements;

  explicit DictLiteralAttribute(std::vector<KeyValuePair> elements)
      : elements(std::move(elements)) {}

  auto clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override;
  auto force_clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override;

 private:
  auto do_format(std::ostream& os) const -> std::ostream& override;
};

/// Attribute attached to IR structures corresponding to partial functions
struct PartialFunctionAttribute : public Attribute {
  static const std::string AttributeName;

  /// base name of the function being used in the partial
  std::string name;

  /// partial arguments, or null if none
  /// e.g. "f(a, ..., b)" has elements [a, null, b]
  std::vector<Value*> args;

  PartialFunctionAttribute(std::string name, std::vector<Value*> args)
      : name(std::move(name)), args(std::move(args)) {}

  auto clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override;
  auto force_clone(Util::CloneVisitor& cv) const
      -> std::unique_ptr<Attribute> override;

 private:
  auto do_format(std::ostream& os) const -> std::ostream& override;
};

}  // namespace Pud::IR

#endif  // PUD_IR_ATTRIBUTE_H