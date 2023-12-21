#include "Pud/Simplify/Context.h"

#include <map>
#include <memory>
#include <string>
#include <unordered_map>

#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"

namespace Pud::AST {

SimplifyContext::SimplifyContext(std::string filename, Cache* cache)
    : Context<SimplifyItem>(std::move(filename)),
      cache(cache),
      is_stdlib_loading(false),
      module_name{ImportFile::PACKAGE, "", ""},
      is_conditional_expr(false),
      allow_type_of(true) {
  bases.emplace_back(Base(""));
  scope.blocks.push_back(scope.counter = 0);
}

SimplifyContext::Base::Base(std::string name, Attr* attributes)
    : name(std::move(name)),
      attributes(attributes),
      deduced_members(nullptr),
      self_name(),
      captures(nullptr),
      py_captures(nullptr) {}

// 添加一个名为 name 的项（Item）到上下文中。
void SimplifyContext::add(const std::string& name,
                          const SimplifyContext::Item& var) {
  // 首先尝试找到该名称的现有项。
  auto v = find(name);
  if (v && v->no_shadow)
    Err(Error::ID_INVALID_BIND, get_source_info(), name);
  // 调用基类的添加方法将项添加到上下文中。
  Context<SimplifyItem>::add(name, var);
}

auto SimplifyContext::add_var(const std::string& name,
                              const std::string& canonical_name,
                              const SourceInfo& source_info)
    -> SimplifyContext::Item {
  // 校验 canonical_name 不为空。
  // seqassert(!canonical_name.empty(), "empty canonical name for '{}'", name);
  // 创建一个 SimplifyItem 实例，并设置其类型（变量、类型或函数）。
  auto t = std::make_shared<SimplifyItem>(SimplifyItem::Var, get_base_name(),
                                          canonical_name, get_module(),
                                          scope.blocks);
  t->set_source_info(source_info);
  // 调用 add 方法将该项加入到上下文中。
  Context<SimplifyItem>::add(name, t);
  Context<SimplifyItem>::add(canonical_name, t);
  return t;
}

auto SimplifyContext::add_type(const std::string& name,
                               const std::string& canonical_name,
                               const SourceInfo& source_info)
    -> SimplifyContext::Item {
  // seqassert(!canonical_name.empty(), "empty canonical name for '{}'", name);
  auto t = std::make_shared<SimplifyItem>(SimplifyItem::Type, get_base_name(),
                                          canonical_name, get_module(),
                                          scope.blocks);
  t->set_source_info(source_info);
  Context<SimplifyItem>::add(name, t);
  Context<SimplifyItem>::add(canonical_name, t);
  return t;
}

auto SimplifyContext::add_func(const std::string& name,
                               const std::string& canonical_name,
                               const SourceInfo& source_info)
    -> SimplifyContext::Item {
  // seqassert(!canonical_name.empty(), "empty canonical name for '{}'", name);
  auto t = std::make_shared<SimplifyItem>(SimplifyItem::Func, get_base_name(),
                                          canonical_name, get_module(),
                                          scope.blocks);
  t->set_source_info(source_info);
  Context<SimplifyItem>::add(name, t);
  Context<SimplifyItem>::add(canonical_name, t);
  return t;
}

// 添加一个总是可见的项到标准库上下文中。
auto SimplifyContext::add_always_visible(const SimplifyContext::Item& item)
    -> SimplifyContext::Item {
  // 创建一个新的 SimplifyItem 实例，复制现有项的信息。
  auto i = std::make_shared<SimplifyItem>(
      item->kind, item->base_name, item->canonical_name, item->module_name,
      std::vector<int>{0}, item->import_path);
  auto stdlib = cache->imports[STDLIB_IMPORT].ctx;
  // 如果在标准库上下文中未找到该项，则将其添加进去。
  if (!stdlib->find(i->canonical_name)) {
    stdlib->add(i->canonical_name, i);
  }
  return i;
}

// 用于在当前模块或标准库中查找一个项。
auto SimplifyContext::find(const std::string& name) const
    -> SimplifyContext::Item {
  auto t = Context<SimplifyItem>::find(name);
  if (t)
    return t;

  // 项在当前模块中找不到。则查看标准库，并且标准库项不能被支配。
  auto stdlib = cache->imports[STDLIB_IMPORT].ctx;
  if (stdlib.get() != this)
    t = stdlib->find(name);
  return t;
}

// 在 find 的基础上增加了断言，确保找到了项。
auto SimplifyContext::force_find(const std::string& name) const
    -> SimplifyContext::Item {
  auto f = find(name);
  // seqassert(f, "cannot find '{}'", name);
  return f;
}

// 用于寻找给定名称的最近的（在作用域上占主导地位的）绑定。
auto SimplifyContext::find_dominating_binding(const std::string& name)
    -> SimplifyContext::Item {
  auto it = map.find(name);
  if (it == map.end())
    return find(name);

  // 断言检查确保找到的项不为空，保证上下文的完整性。
  // seqassert(!it->second.empty(), "corrupted SimplifyContext ({})", name);

  std::string canonical_name;
  // 初始化为找到的绑定列表的开始，表示最近有效的绑定。
  auto last_good = it->second.begin();
  // 检查最近的绑定是否在当前基本名称之外。
  bool is_outside = (*last_good)->get_base_name() != get_base_name();
  // prefix 是当前作用域块的数量。
  int prefix = int(scope.blocks.size());
  // 对该名称的所有绑定进行遍历，以找到最接近当前作用域且有效的绑定。
  for (auto i = it->second.begin(); i != it->second.end(); i++) {
    // 在绑定列表中遍历，寻找主导绑定。
    // p 是当前作用域和绑定作用域之间的最小作用域层级。
    int p = std::min(prefix, int((*i)->scope.size()));
    // 循环减少 p 直到找到作用域的共同前缀。
    while (p >= 0 && (*i)->scope[p - 1] != scope.blocks[p - 1])
      p--;
    // 如果 p 小于 0 或者绑定的基本名称不同，则中断循环。
    // 到达 toplevel 则 Break
    if (p < 0)
      break;

    if (!is_outside && (*i)->get_base_name() != get_base_name())
      break;
    // 检查当前绑定是否完全支配当前作用域。
    bool complete_domination =
        (*i)->scope.size() <= scope.blocks.size() &&
        (*i)->scope.back() == scope.blocks[(*i)->scope.size() - 1];
    // 如果没有完全支配且存在更广泛的作用域覆盖，则中断循环。
    if (!complete_domination && prefix < int(scope.blocks.size()) &&
        prefix != p) {
      break;
    }
    prefix = p;
    // 更新 last_good 为当前最近的有效绑定。
    last_good = i;
    // 完全支配当前作用域
    if (complete_domination)
      break;
  }
  // 确认 last_good 是有效的。
  // seqassert(last_good != it->second.end(), "corrupted scoping ({})", name);
  // 如果 last_good 不是最初的绑定且不是变量类型，抛出错误。
  if (last_good != it->second.begin() && !(*last_good)->is_var())
    Err(Error::CLASS_INVALID_BIND, get_source_info(), name);

  bool has_used = false;
  if ((*last_good)->scope.size() == prefix) {
    // 如果找到的绑定完全匹配当前作用域，则使用其规范名称。
    canonical_name = (*last_good)->canonical_name;
  } else {
    // 否则，创建一个新的绑定来覆盖所有可能的绑定。
    canonical_name = generate_canonical_name(name);
    // 创建一个新的 SimplifyItem。
    auto item = std::make_shared<SimplifyItem>(
        (*last_good)->kind, (*last_good)->base_name, canonical_name,
        (*last_good)->module_name,
        std::vector<int>(scope.blocks.begin(), scope.blocks.begin() + prefix),
        (*last_good)->import_path);
    item->access_checked = {(*last_good)->scope};
    last_good = it->second.insert(++last_good, item);
    stack.front().push_back(name);
    // 将新创建的绑定插入到适当的作用域中。
    // 更新作用域声明，以反映新绑定的存在: `var` and `var__used__ =
    // False`
    scope.stmts[scope.blocks[prefix - 1]].push_back(
        std::make_unique<AssignStmt>(std::make_unique<IdExpr>(canonical_name),
                                     nullptr, nullptr));
    scope.stmts[scope.blocks[prefix - 1]].push_back(
        std::make_unique<AssignStmt>(std::make_unique<IdExpr>(fmt::format(
                                         "{}.__used__", canonical_name)),
                                     std::make_unique<BoolExpr>(false),
                                     nullptr));
    // 如果到达顶层作用域，则将新绑定注册为全局绑定。
    if (prefix == 1) {
      cache->add_global(canonical_name);
      cache->add_global(fmt::format("{}.__used__", canonical_name));
    }
    has_used = true;
  }
  // 遍历并移除所有不再需要的旧绑定。
  for (auto i = it->second.begin(); i != it->second.end(); i++) {
    if (i == last_good)
      break;
    if (!(*i)->can_dominate())
      continue;
    // 更新替换规则以反映新的绑定关系。
    cache->replacements[(*i)->canonical_name] = {canonical_name, has_used};
    cache->replacements[fmt::format("{}.__used__", (*i)->canonical_name)] = {
        fmt::format("{}.__used__", canonical_name), false};
    // seqassert((*i)->canonical_name != canonical_name,
    //          "invalid replacement at {}: {}", get_source_info(),
    //          canonical_name);
    auto it = std::find(stack.front().begin(), stack.front().end(), name);
    if (it != stack.front().end())
      stack.front().erase(it);
  }
  it->second.erase(it->second.begin(), last_good);
  // 最终返回找到或新创建的主导绑定。
  return it->second.front();
}

auto SimplifyContext::get_base_name() const -> std::string {
  return bases.back().name;
}

auto SimplifyContext::get_module() const -> std::string {
  std::string base = module_name.status == ImportFile::STDLIB ? "std." : "";
  base += module_name.module;
  if (auto sz = startswith(base, "__main__"))
    base = base.substr(sz);
  return base;
}

void SimplifyContext::dump() { dump(0); }

auto SimplifyContext::generate_canonical_name(const std::string& name,
                                              bool include_base,
                                              bool zero_id) const
    -> std::string {
  std::string new_name = name;
  bool already_generated = name.find('.') != std::string::npos;
  if (include_base && !already_generated) {
    std::string base = get_base_name();
    if (base.empty())
      base = get_module();
    if (base == "std.internal.core")
      base = "";
    new_name = (base.empty() ? "" : (base + ".")) + new_name;
  }
  auto num = cache->identifier_count[new_name]++;
  if (num)
    new_name = fmt::format("{}.{}", new_name, num);
  if (name != new_name && !zero_id)
    cache->identifier_count[new_name]++;
  cache->reverse_identifier_lookup[new_name] = name;
  return new_name;
}

void SimplifyContext::enter_conditional_block() {
  scope.blocks.push_back(++scope.counter);
}

void SimplifyContext::leave_conditional_block(std::vector<StmtPtr>* stmts) {
  if (stmts && in(scope.stmts, scope.blocks.back()))
    stmts->insert(stmts->begin(), scope.stmts[scope.blocks.back()].begin(),
                  scope.stmts[scope.blocks.back()].end());
  scope.blocks.pop_back();
}

auto SimplifyContext::is_global() const -> bool { return bases.size() == 1; }

auto SimplifyContext::is_conditional() const -> bool {
  return scope.blocks.size() > 1;
}

auto SimplifyContext::get_base() -> SimplifyContext::Base* {
  return bases.empty() ? nullptr : &(bases.back());
}

auto SimplifyContext::in_function() const -> bool {
  return !is_global() && !bases.back().is_type();
}

auto SimplifyContext::in_class() const -> bool {
  return !is_global() && bases.back().is_type();
}

auto SimplifyContext::is_outer(const Item& val) const -> bool {
  return get_base_name() != val->get_base_name() ||
         get_module() != val->get_module();
}

auto SimplifyContext::get_class_base() -> SimplifyContext::Base* {
  if (bases.size() >= 2 && bases[bases.size() - 2].is_type())
    return &(bases[bases.size() - 2]);
  return nullptr;
}

void SimplifyContext::dump(int pad) {
  auto ordered =
      std::map<std::string, decltype(map)::mapped_type>(map.begin(), map.end());
  LOG("location: {}", get_source_info());
  LOG("module:   {}", get_module());
  LOG("base:     {}", get_base_name());
  LOG("scope:    {}", fmt::join(scope.blocks, ","));
  for (auto& s : stack.front())
    LOG("-> {}", s);
  for (auto& i : ordered) {
    std::string s;
    bool f = true;
    for (auto& t : i.second) {
      LOG("{}{} {} {:40} {:30} {}", std::string(pad * 2, ' '),
          !f ? std::string(40, ' ') : fmt::format("{:.<40}", i.first),
          (t->is_func() ? "F"
                        : (t->is_type() ? "T" : (t->is_import() ? "I" : "V"))),
          t->canonical_name, t->get_base_name(), combine2(t->scope, ","));
      f = false;
    }
  }
}

}  // namespace Pud::AST