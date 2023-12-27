#include "Pud/IR/Attribute.h"

#include <fmt/ostream.h>

#include "Pud/IR/Func.h"
#include "Pud/IR/Util/Cloning.h"
#include "Pud/IR/Value.h"

namespace Pud::IR {

const std::string KeyValueAttribute::AttributeName = "kvAttribute";

auto KeyValueAttribute::has(const std::string& key) const -> bool {
  return attributes.find(key) != attributes.end();
}

auto KeyValueAttribute::get(const std::string& key) const -> std::string {
  auto it = attributes.find(key);
  return it != attributes.end() ? it->second : "";
}

auto KeyValueAttribute::do_format(std::ostream& os) const -> std::ostream& {
  std::vector<std::string> keys;
  for (auto& val : attributes)
    keys.push_back(val.second);
  fmt::print(os, FMT_STRING("{}"), fmt::join(keys.begin(), keys.end(), ","));
  return os;
}

const std::string MemberAttribute::AttributeName = "memberAttribute";

auto MemberAttribute::do_format(std::ostream& os) const -> std::ostream& {
  std::vector<std::string> strings;
  for (auto& val : member_source_info)
    strings.push_back(fmt::format(FMT_STRING("{}={}"), val.first, val.second));
  fmt::print(os, FMT_STRING("({})"),
             fmt::join(strings.begin(), strings.end(), ","));
  return os;
}

const std::string SourceInfoAttribute::AttributeName = "srcInfoAttribute";

const std::string DocstringAttribute::AttributeName = "docstringAttribute";

const std::string TupleLiteralAttribute::AttributeName =
    "tupleLiteralAttribute";

auto TupleLiteralAttribute::clone(Util::CloneVisitor& cv) const
    -> std::unique_ptr<Attribute> {
  std::vector<Value*> elements_cloned;
  for (auto* val : elements)
    elements_cloned.push_back(cv.clone(val));
  return std::make_unique<TupleLiteralAttribute>(elements_cloned);
}

auto TupleLiteralAttribute::force_clone(Util::CloneVisitor& cv) const
    -> std::unique_ptr<Attribute> {
  std::vector<Value*> elements_cloned;
  for (auto* val : elements)
    elements_cloned.push_back(cv.force_clone(val));
  return std::make_unique<TupleLiteralAttribute>(elements_cloned);
}

auto TupleLiteralAttribute::do_format(std::ostream& os) const -> std::ostream& {
  std::vector<std::string> strings;
  for (auto* val : elements)
    strings.push_back(fmt::format(FMT_STRING("{}"), *val));
  fmt::print(os, FMT_STRING("({})"),
             fmt::join(strings.begin(), strings.end(), ","));
  return os;
}

const std::string ListLiteralAttribute::AttributeName = "listLiteralAttribute";

auto ListLiteralAttribute::clone(Util::CloneVisitor& cv) const
    -> std::unique_ptr<Attribute> {
  std::vector<LiteralElement> elements_cloned;
  for (auto& e : elements)
    elements_cloned.push_back({cv.clone(e.value), e.star});
  return std::make_unique<ListLiteralAttribute>(elements_cloned);
}

auto ListLiteralAttribute::force_clone(Util::CloneVisitor& cv) const
    -> std::unique_ptr<Attribute> {
  std::vector<LiteralElement> elements_cloned;
  for (auto& e : elements)
    elements_cloned.push_back({cv.force_clone(e.value), e.star});
  return std::make_unique<ListLiteralAttribute>(elements_cloned);
}

auto ListLiteralAttribute::do_format(std::ostream& os) const -> std::ostream& {
  std::vector<std::string> strings;
  for (auto& e : elements)
    strings.push_back(
        fmt::format(FMT_STRING("{}{}"), e.star ? "*" : "", *e.value));
  fmt::print(os, FMT_STRING("[{}]"),
             fmt::join(strings.begin(), strings.end(), ","));
  return os;
}

const std::string SetLiteralAttribute::AttributeName = "setLiteralAttribute";

auto SetLiteralAttribute::clone(Util::CloneVisitor& cv) const
    -> std::unique_ptr<Attribute> {
  std::vector<LiteralElement> elements_cloned;
  for (auto& e : elements)
    elements_cloned.push_back({cv.clone(e.value), e.star});
  return std::make_unique<SetLiteralAttribute>(elements_cloned);
}

auto SetLiteralAttribute::force_clone(Util::CloneVisitor& cv) const
    -> std::unique_ptr<Attribute> {
  std::vector<LiteralElement> elements_cloned;
  for (auto& e : elements)
    elements_cloned.push_back({cv.force_clone(e.value), e.star});
  return std::make_unique<SetLiteralAttribute>(elements_cloned);
}

auto SetLiteralAttribute::do_format(std::ostream& os) const -> std::ostream& {
  std::vector<std::string> strings;
  for (auto& e : elements)
    strings.push_back(
        fmt::format(FMT_STRING("{}{}"), e.star ? "*" : "", *e.value));
  fmt::print(os, FMT_STRING("set([{}])"),
             fmt::join(strings.begin(), strings.end(), ","));
  return os;
}

const std::string DictLiteralAttribute::AttributeName = "dictLiteralAttribute";

auto DictLiteralAttribute::clone(Util::CloneVisitor& cv) const
    -> std::unique_ptr<Attribute> {
  std::vector<DictLiteralAttribute::KeyValuePair> elements_cloned;
  for (auto& val : elements)
    elements_cloned.push_back(
        {cv.clone(val.key), val.value ? cv.clone(val.value) : nullptr});
  return std::make_unique<DictLiteralAttribute>(elements_cloned);
}

auto DictLiteralAttribute::force_clone(Util::CloneVisitor& cv) const
    -> std::unique_ptr<Attribute> {
  std::vector<DictLiteralAttribute::KeyValuePair> elements_cloned;
  for (auto& val : elements)
    elements_cloned.push_back(
        {cv.force_clone(val.key),
         val.value ? cv.force_clone(val.value) : nullptr});
  return std::make_unique<DictLiteralAttribute>(elements_cloned);
}

auto DictLiteralAttribute::do_format(std::ostream& os) const -> std::ostream& {
  std::vector<std::string> strings;
  for (auto& val : elements) {
    if (val.value) {
      strings.push_back(fmt::format(FMT_STRING("{}:{}"), *val.key, *val.value));
    } else {
      strings.push_back(fmt::format(FMT_STRING("**{}"), *val.key));
    }
  }
  fmt::print(os, FMT_STRING("dict([{}])"),
             fmt::join(strings.begin(), strings.end(), ","));
  return os;
}

const std::string PartialFunctionAttribute::AttributeName =
    "partialFunctionAttribute";

auto PartialFunctionAttribute::clone(Util::CloneVisitor& cv) const
    -> std::unique_ptr<Attribute> {
  std::vector<Value*> args_cloned;
  for (auto* val : args)
    args_cloned.push_back(cv.clone(val));
  return std::make_unique<PartialFunctionAttribute>(name, args_cloned);
}

auto PartialFunctionAttribute::force_clone(Util::CloneVisitor& cv) const
    -> std::unique_ptr<Attribute> {
  std::vector<Value*> args_cloned;
  for (auto* val : args)
    args_cloned.push_back(cv.force_clone(val));
  return std::make_unique<PartialFunctionAttribute>(name, args_cloned);
}

auto PartialFunctionAttribute::do_format(std::ostream& os) const
    -> std::ostream& {
  std::vector<std::string> strings;
  for (auto* val : args)
    strings.push_back(val ? fmt::format(FMT_STRING("{}"), *val) : "...");
  fmt::print(os, FMT_STRING("{}({})"), name,
             fmt::join(strings.begin(), strings.end(), ","));
  return os;
}

}  // namespace Pud::IR