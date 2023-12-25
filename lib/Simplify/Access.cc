#include <string>
#include <tuple>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"

using fmt::format;

namespace Pud::AST {

// 处理标识符表达式，如变量名或函数名。
void SimplifyVisitor::visit(IdExpr* expr) {
  // 搜索当前上下文中的标识符。
  auto val = ctx->find_dominating_binding(expr->value);

  // 如果找不到标识符并且在 Python 上下文中，将其添加到
  // py_captures 集合中，并构造一个索引表达式访问它。
  if (!val && ctx->get_base()->py_captures) {
    ctx->get_base()->py_captures->insert(expr->value);
    result_expr =
        N<IndexExpr>(N<IdExpr>("__pyenv__"), N<StringExpr>(expr->value));
    return;
  } else if (!val) {
    // 如果没有找到标识符，抛出 ID_NOT_FOUND 错误。
    Err(Error::ID_NOT_FOUND, expr, expr->value);
  }

  // 如果正在访问的是外部变量，则通过 check_capture 方法进行处理。
  auto captured = check_capture(val);
  if (captured)
    val = ctx->force_find(expr->value);

  // Example:
  // x = 1
  // while True:
  //   if x > 10: break
  //   x = x + 1
  // 如果在循环中访问变量，记录它以在循环结束后更新变量的值。
  if (ctx->get_base()->get_loop()) {
    for (size_t li = ctx->get_base()->loops.size(); li-- > 0;) {
      auto& loop = ctx->get_base()->loops[li];
      bool inside = val->scope.size() >= loop.scope.size() &&
                    val->scope[loop.scope.size() - 1] == loop.scope.back();
      if (!inside)
        loop.seen_vars.insert(expr->value);
      else
        break;
    }
  }

  // 将变量名替换为其在上下文中的规范名称。
  expr->value = val->canonical_name;

  // Mark global as "seen" to prevent later creation of local variables
  // with the same name. Example:
  // x = 1
  // def foo():
  //   print(x)  # mark x as seen
  //   x = 2     # so that this is an error
  if (!val->is_generic() && ctx->is_outer(val) &&
      !in(ctx->seen_global_identifiers[ctx->get_base_name()],
          ctx->cache->rev(val->canonical_name))) {
    ctx->seen_global_identifiers[ctx->get_base_name()]
                                [ctx->cache->rev(val->canonical_name)] =
        expr->clone();
  }

  // 如果变量指向一个类型，将表达式标记为类型表达式。
  if (val->is_type())
    expr->mark_type();

  // 对于在条件块中定义的变量，添加访问检查。
  if (!val->access_checked.empty()) {
    bool checked = false;
    for (auto& a : val->access_checked) {
      if (a.size() <= ctx->scope.blocks.size() &&
          a[a.size() - 1] == ctx->scope.blocks[a.size() - 1]) {
        checked = true;
        break;
      }
    }
    if (!checked) {
      // Prepend access with __internal__.undef([var]__used__, "[var name]")
      auto check_stmt = N<ExprStmt>(N<CallExpr>(
          N<DotExpr>("__internal__", "undef"),
          N<IdExpr>(fmt::format("{}.__used__", val->canonical_name)),
          N<StringExpr>(
              ctx->cache->reverse_identifier_lookup[val->canonical_name])));
      if (!ctx->is_conditional_expr) {
        // If the expression is not conditional, we can just do the check once
        prepend_stmts->push_back(check_stmt);
        val->access_checked.push_back(ctx->scope.blocks);
      } else {
        // Otherwise, this check must be always called
        result_expr = N<StmtExpr>(check_stmt, N<IdExpr>(*expr));
      }
    }
  }
}

/// Flatten imports.
/// @example
///   `a.b.c`      -> canonical name of `c` in `a.b` if `a.b` is an import
///   `a.B.c`      -> canonical name of `c` in class `a.B`
///   `python.foo` -> internal.python._get_identifier("foo")
/// 处理属性访问表达式，如 a.b.c。
void SimplifyVisitor::visit(DotExpr* expr) {
  // 将点表达式（如 a.b.c）转换为字符串链
  // transform Dot(Dot(a, b), c...) to {a, b, c, ...}
  std::vector<std::string> chain;
  Expr* root = expr;
  for (; root->get_dot(); root = root->get_dot()->expr.get())
    chain.push_back(root->get_dot()->member);

  if (auto id = root->get_id()) {
    // Case: a.bar.baz
    chain.push_back(id->value);
    std::reverse(chain.begin(), chain.end());
    auto p = get_import(chain);

    if (!p.second) {
      // seqassert(ctx->get_base()->py_captures, "unexpected py capture");
      ctx->get_base()->py_captures->insert(chain[0]);
      result_expr =
          N<IndexExpr>(N<IdExpr>("__pyenv__"), N<StringExpr>(chain[0]));
    } else if (p.second->get_module() == "std.python") {
      result_expr = transform(
          N<CallExpr>(N<DotExpr>(N<DotExpr>(N<IdExpr>("internal"), "python"),
                                 "_get_identifier"),
                      N<StringExpr>(chain[p.first++])));
    } else if (p.second->get_module() == ctx->get_module() && p.first == 1) {
      result_expr = transform(N<IdExpr>(chain[0]), true);
    } else {
      result_expr = N<IdExpr>(p.second->canonical_name);
      if (p.second->is_type() && p.first == chain.size())
        result_expr->mark_type();
    }
    for (auto i = p.first; i < chain.size(); i++)
      result_expr = N<DotExpr>(result_expr, chain[i]);
  } else {
    // Case: a[x].foo.bar
    transform(expr->expr, true);
  }
}

// 检查是否可以捕获当前作用域外的变量。
auto SimplifyVisitor::check_capture(const SimplifyContext::Item& val) -> bool {
  if (!ctx->is_outer(val))
    return false;
  if ((val->is_type() && !val->is_generic()) || val->is_func())
    return false;

  // Ensure that outer variables can be captured (i.e., do not cross no-capture
  // boundary). Example:
  // def foo():
  //   x = 1
  //   class T:      # <- boundary (classes cannot capture locals)
  //     t: int = x  # x cannot be accessed
  //     def bar():  # <- another boundary
  //                 # (class methods cannot capture locals except class
  //                 generics)
  //       print(x)  # x cannot be accessed
  bool cross_capture_boundary = false;
  bool local_generic =
      val->is_generic() && val->get_base_name() == ctx->get_base_name();
  bool parent_class_generic =
      val->is_generic() && !ctx->get_base()->is_type() &&
      (ctx->bases.size() > 1 && ctx->bases[ctx->bases.size() - 2].is_type() &&
       ctx->bases[ctx->bases.size() - 2].name == val->get_base_name());
  auto i = ctx->bases.size();
  for (; i-- > 0;) {
    if (ctx->bases[i].name == val->get_base_name())
      break;
    if (!local_generic && !parent_class_generic && !ctx->bases[i].captures)
      cross_capture_boundary = true;
  }

  // Mark methods (class functions that access class generics)
  if (parent_class_generic)
    ctx->get_base()->attributes->set(Attr::Method);

  // Ignore generics
  if (parent_class_generic || local_generic)
    return false;

  // Case: a global variable that has not been marked with `global` statement
  if (val->is_var() && val->get_base_name().empty() && val->scope.size() == 1) {
    val->no_shadow = true;
    if (!val->is_static())
      ctx->cache->add_global(val->canonical_name);
    return false;
  }

  // Check if a real variable (not a static) is defined outside the current
  // scope
  if (cross_capture_boundary)
    Err(Error::ID_CANNOT_CAPTURE, get_source_info(),
      ctx->cache->rev(val->canonical_name));

  // Case: a nonlocal variable that has not been marked with `nonlocal`
  // statement
  //       and capturing is enabled
  auto captures = ctx->get_base()->captures;
  if (captures && !in(*captures, val->canonical_name)) {
    // Captures are transformed to function arguments; generate new name for
    // that argument
    ExprPtr typ = nullptr;
    if (val->is_type())
      typ = N<IdExpr>("type");
    if (auto st = val->is_static())
      typ = N<IndexExpr>(N<IdExpr>("Static"),
                         N<IdExpr>(st == StaticValue::INT ? "int" : "str"));
    auto [newName, _] = (*captures)[val->canonical_name] = {
        ctx->generate_canonical_name(val->canonical_name), typ};
    ctx->cache->reverse_identifier_lookup[newName] = newName;
    // Add newly generated argument to the context
    std::shared_ptr<SimplifyItem> newVal = nullptr;
    if (val->is_type())
      newVal = ctx->add_type(ctx->cache->rev(val->canonical_name), newName,
                            get_source_info());
    else
      newVal = ctx->add_var(ctx->cache->rev(val->canonical_name), newName,
                           get_source_info());
    newVal->base_name = ctx->get_base_name();
    newVal->no_shadow = true;
    newVal->scope = ctx->get_base()->scope;
    return true;
  }

  // Case: a nonlocal variable that has not been marked with `nonlocal`
  // statement
  //       and capturing is *not* enabled
  Err(Error::ID_NONLOCAL, get_source_info(), ctx->cache->rev(val->canonical_name));
  return false;
}

/// Check if a access chain (a.b.c.d...) contains an import or class prefix.
std::pair<size_t, SimplifyContext::Item> SimplifyVisitor::get_import(
    const std::vector<std::string>& chain) {
  size_t importEnd = 0;
  std::string importName;

  // Find the longest prefix that corresponds to the existing import
  // (e.g., `a.b.c.d` -> `a.b.c` if there is `import a.b.c`)
  SimplifyContext::Item val = nullptr;
  for (auto i = chain.size(); i-- > 0;) {
    val = ctx->find(join(chain, "/", 0, i + 1));
    if (val && val->is_import()) {
      importName = val->import_path, importEnd = i + 1;
      break;
    }
  }

  if (importEnd != chain.size()) {  // false when a.b.c points to import itself
    // Find the longest prefix that corresponds to the existing class
    // (e.g., `a.b.c` -> `a.b` if there is `class a: class b:`)
    std::string itemName;
    size_t itemEnd = 0;
    auto fctx = importName.empty() ? ctx : ctx->cache->imports[importName].ctx;
    for (auto i = chain.size(); i-- > importEnd;) {
      if (fctx->get_module() == "std.python" && importEnd < chain.size()) {
        // Special case: importing from Python.
        // Fake SimplifyItem that indicates std.python access
        val = std::make_shared<SimplifyItem>(
            SimplifyItem::Var, "", "", fctx->get_module(), std::vector<int>{});
        return {importEnd, val};
      } else {
        val = fctx->find(join(chain, ".", importEnd, i + 1));
        if (val &&
            (importName.empty() || val->is_type() || !val->is_conditional())) {
          itemName = val->canonical_name, itemEnd = i + 1;
          break;
        }
      }
    }
    if (itemName.empty() && importName.empty()) {
      if (ctx->get_base()->py_captures)
        return {1, nullptr};
      Err(Error::IMPORT_NO_MODULE, get_source_info(), chain[importEnd]);
    }
    if (itemName.empty())
      Err(Error::IMPORT_NO_NAME, get_source_info(), chain[importEnd],
        ctx->cache->imports[importName].module_name);
    importEnd = itemEnd;
  }
  return {importEnd, val};
}

}  // namespace Pud::AST