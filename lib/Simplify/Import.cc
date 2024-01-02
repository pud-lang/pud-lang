#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/Common/Common.h"
#include "Pud/Parse/Parser.h"
#include "Pud/Simplify/Simplify.h"

using fmt::format;

namespace Pud::AST {

/// Import and parse a new module into its own context.
/// Also handle special imports ( see @c transform_special_import ).
/// To simulate Python's dynamic import logic and import stuff only once,
/// each import statement is guarded as follows:
///   if not _import_N_done:
///     _import_N()
///     _import_N_done = True
/// See @c transformNewImport and below for more details.
void SimplifyVisitor::visit(ImportStmt* stmt) {
  // seqassert(!ctx->in_class(), "imports within a class");
  if ((result_stmt = transform_special_import(stmt)))
    return;

  // Fetch the import
  auto components = get_import_path(stmt->from.get(), stmt->dots);
  auto path = combine2(components, "/");
  auto file =
      get_import_file(ctx->cache->argv0, path, ctx->get_filename(), false,
                      ctx->cache->module0, ctx->cache->plugin_import_paths);
  if (!file) {
    std::string s(stmt->dots, '.');
    for (size_t i = 0; i < components.size(); i++)
      if (components[i] == "..") {
        continue;
      } else if (!s.empty() && s.back() != '.') {
        s += "." + components[i];
      } else {
        s += components[i];
      }
    Err(Error::IMPORT_NO_MODULE, stmt->from, s);
  }

  // If the file has not been seen before, load it into cache
  if (ctx->cache->imports.find(file->path) == ctx->cache->imports.end())
    result_stmt = transform_new_import(*file);

  const auto& import = ctx->cache->imports[file->path];
  std::string import_var = import.import_var;
  std::string import_done_var = import_var + "_done";

  // Construct `if _import_done.__invert__(): (_import(); _import_done = True)`.
  // Do not do this during the standard library loading (we assume that standard
  // library imports are "clean" and do not need guards). Note that the
  // import_var is empty if the import has been loaded during the standard
  // library loading.
  if (!ctx->is_stdlib_loading && !import_var.empty()) {
    auto u = N<AssignStmt>(N<IdExpr>(import_done_var), N<BoolExpr>(true));
    u->set_update();
    result_stmt = N<IfStmt>(
        N<CallExpr>(N<DotExpr>(import_done_var, "__invert__")),
        N<SuiteStmt>(N<ExprStmt>(N<CallExpr>(N<IdExpr>(import_var))), u));
  }

  // Import requested identifiers from the import's scope to the current scope
  if (!stmt->what) {
    // Case: import foo
    auto name = stmt->as.empty() ? path : stmt->as;
    auto var = import_var + "_var";
    // Construct `import_var = Import([module], [path])` (for printing imports
    // etc.)
    result_stmt = N<SuiteStmt>(
        result_stmt,
        transform(N<AssignStmt>(
            N<IdExpr>(var),
            N<CallExpr>(N<IdExpr>("Import"), N<StringExpr>(file->module),
                        N<StringExpr>(file->path)),
            N<IdExpr>("Import"))));
    ctx->add_var(name, var, stmt->get_source_info())->import_path = file->path;
  } else if (stmt->what->is_id("*")) {
    // Case: from foo import *
    // seqassert(stmt->as.empty(), "renamed star-import");
    // Just copy all symbols from import's context here.
    for (auto& i : *(import.ctx)) {
      if ((!startswith(i.first, "_") ||
           (ctx->is_stdlib_loading && startswith(i.first, "__")))) {
        // Ignore all identifiers that start with `_` but not those that start
        // with
        // `__` while the standard library is being loaded
        auto c = i.second.front();
        if (c->is_conditional() && i.first.find('.') == std::string::npos) {
          c = import.ctx->find_dominating_binding(i.first);
        }
        // Imports should ignore  noShadow property
        ctx->Context<SimplifyItem>::add(i.first, c);
      }
    }
  } else {
    // Case 3: from foo import bar
    auto i = stmt->what->get_id();
    // seqassert(i, "not a valid import what expression");
    auto c = import.ctx->find(i->value);
    // Make sure that we are importing an existing global symbol
    if (!c)
      Err(Error::IMPORT_NO_NAME, i, i->value, file->module);
    if (c->is_conditional())
      c = import.ctx->find_dominating_binding(i->value);
    // Imports should ignore  noShadow property
    ctx->Context<SimplifyItem>::add(stmt->as.empty() ? i->value : stmt->as, c);
  }

  if (!result_stmt) {
    result_stmt = N<SuiteStmt>();  // erase it
  }
}

/// Transform special `from C` and `from python` imports.
/// See @c transform_c_import, @c transformCDLLImport and @c
/// transformPythonImport
auto SimplifyVisitor::transform_special_import(ImportStmt* stmt) -> StmtPtr {
  if (stmt->from && stmt->from->is_id("C") && stmt->what->get_id() &&
      stmt->is_function) {
    // C function imports
    return transform_c_import(stmt->what->get_id()->value, stmt->args,
                              stmt->ret.get(), stmt->as);
  }
  if (stmt->from && stmt->from->is_id("C") && stmt->what->get_id()) {
    // C variable imports
    return transform_cvar_import(stmt->what->get_id()->value, stmt->ret.get(),
                                 stmt->as);
  } else if (stmt->from && stmt->from->is_id("C") && stmt->what->get_dot()) {
    // dylib C imports
    return transform_cdll_import(stmt->what->get_dot()->expr.get(),
                                 stmt->what->get_dot()->member, stmt->args,
                                 stmt->ret.get(), stmt->as, stmt->is_function);
  } else if (stmt->from && stmt->from->is_id("python") && stmt->what) {
    // Python imports
    return transform_python_import(stmt->what.get(), stmt->args,
                                   stmt->ret.get(), stmt->as);
  }
  return nullptr;
}

/// Transform Dot(Dot(a, b), c...) into "{a, b, c, ...}".
/// Useful for getting import paths.
auto SimplifyVisitor::get_import_path(Expr* from, size_t dots)
    -> std::vector<std::string> {
  std::vector<std::string> components;  // Path components
  if (from) {
    for (; from->get_dot(); from = from->get_dot()->expr.get())
      components.push_back(from->get_dot()->member);
    // seqassert(from->get_id(), "invalid import statement");
    components.push_back(from->get_id()->value);
  }

  // Handle dots (i.e., `..` in `from ..m import x`)
  for (size_t i = 1; i < dots; i++)
    components.emplace_back("..");
  std::reverse(components.begin(), components.end());
  return components;
}

/// Transform a C function import.
/// @example
///   `from C import foo(int) -> float as f` ->
///   ```@.c
///      def foo(a1: int) -> float:
///        pass
///      f = foo # if altName is provided```
/// No return type implies void return type. *args is treated as C VAR_ARGS.
auto SimplifyVisitor::transform_c_import(const std::string& name,
                                         const std::vector<Param>& args,
                                         const Expr* ret,
                                         const std::string& alt_name)
    -> StmtPtr {
  std::vector<Param> fn_args;
  auto attr = Attr({Attr::C});
  for (size_t ai = 0; ai < args.size(); ai++) {
    // seqassert(args[ai].name.empty(), "unexpected argument name");
    // seqassert(!args[ai].default_value, "unexpected default argument");
    // seqassert(args[ai].type, "missing type");
    if (args[ai].type->get_ellipsis() && ai + 1 == args.size()) {
      // C VAR_ARGS support
      attr.set(Attr::CVarArg);
      fn_args.emplace_back(Param{"*args", nullptr, nullptr});
    } else {
      fn_args.emplace_back(
          Param{args[ai].name.empty() ? format("a{}", ai) : args[ai].name,
                args[ai].type->clone(), nullptr});
    }
  }
  ctx->generate_canonical_name(name);  // avoid canonical_name == name
  StmtPtr f = N<FunctionStmt>(name, ret ? ret->clone() : N<IdExpr>("NoneType"),
                              fn_args, nullptr, attr);
  f = transform(f);  // Already in the preamble
  if (!alt_name.empty()) {
    auto val = ctx->force_find(name);
    ctx->add(alt_name, val);
    ctx->remove(name);
  }
  return f;
}

/// Transform a C variable import.
/// @example
///   `from C import foo: int as f` ->
///   ```f: int = "foo"```
auto SimplifyVisitor::transform_cvar_import(const std::string& name,
                                            const Expr* type,
                                            const std::string& alt_name)
    -> StmtPtr {
  auto canonical = ctx->generate_canonical_name(name);
  auto val = ctx->add_var(alt_name.empty() ? name : alt_name, canonical);
  val->no_shadow = true;
  auto s = N<AssignStmt>(N<IdExpr>(canonical), nullptr,
                         transform_type(type->clone()));
  s->lhs->set_attr(ExprAttr::ExternVar);
  return s;
}

/// Transform a dynamic C import.
/// @example
///   `from C import lib.foo(int) -> float as f` ->
///   `f = _dlsym(lib, "foo", Fn=Function[[int], float]); f`
/// No return type implies void return type.
auto SimplifyVisitor::transform_cdll_import(
    const Expr* dylib, const std::string& name, const std::vector<Param>& args,
    const Expr* ret, const std::string& alt_name, bool is_function) -> StmtPtr {
  ExprPtr type = nullptr;
  if (is_function) {
    std::vector<ExprPtr> fn_args{N<ListExpr>(std::vector<ExprPtr>{}),
                                 ret ? ret->clone() : N<IdExpr>("NoneType")};
    for (const auto& a : args) {
      // seqassert(a.name.empty(), "unexpected argument name");
      // seqassert(!a.default_value, "unexpected default argument");
      // seqassert(a.type, "missing type");
      fn_args[0]->get_list()->items.emplace_back(clone(a.type));
    }

    type = N<IndexExpr>(N<IdExpr>("Function"), N<TupleExpr>(fn_args));
  } else {
    type = ret->clone();
  }

  return transform(N<AssignStmt>(
      N<IdExpr>(alt_name.empty() ? name : alt_name),
      N<CallExpr>(N<IdExpr>("_dlsym"),
                  std::vector<CallExpr::Arg>{CallExpr::Arg(dylib->clone()),
                                             CallExpr::Arg(N<StringExpr>(name)),
                                             {"Fn", type}})));
}

/// Transform a Python module and function imports.
/// @example
///   `from python import module as f` -> `f = pyobj._import("module")`
///   `from python import lib.foo(int) -> float as f` ->
///   ```def f(a0: int) -> float:
///        f = pyobj._import("lib")._getattr("foo")
///        return float.__from_py__(f(a0))```
/// If a return type is nullptr, the function just returns f (raw pyobj).
auto SimplifyVisitor::transform_python_import(Expr* what,
                                              const std::vector<Param>& args,
                                              Expr* ret,
                                              const std::string& alt_name)
    -> StmtPtr {
  // Get a module name (e.g., os.path)
  auto components = get_import_path(what);

  if (!ret && args.empty()) {
    // Simple import: `from python import foo.bar` -> `bar =
    // pyobj._import("foo.bar")`
    return transform(N<AssignStmt>(
        N<IdExpr>(alt_name.empty() ? components.back() : alt_name),
        N<CallExpr>(N<DotExpr>("pyobj", "_import"),
                    N<StringExpr>(combine2(components, ".")))));
  }

  // Python function import:
  // `from python import foo.bar(int) -> float` ->
  // ```def bar(a1: int) -> float:
  //      f = pyobj._import("foo")._getattr("bar")
  //      return float.__from_py__(f(a1))```

  // f = pyobj._import("foo")._getattr("bar")
  auto call = N<AssignStmt>(
      N<IdExpr>("f"),
      N<CallExpr>(N<DotExpr>(N<CallExpr>(N<DotExpr>("pyobj", "_import"),
                                         N<StringExpr>(combine2(
                                             components, ".", 0,
                                             int(components.size()) - 1))),
                             "_getattr"),
                  N<StringExpr>(components.back())));
  // f(a1, ...)
  std::vector<Param> params;
  std::vector<ExprPtr> call_args;
  for (int i = 0; i < args.size(); i++) {
    params.emplace_back(Param{format("a{}", i), clone(args[i].type), nullptr});
    call_args.emplace_back(N<IdExpr>(format("a{}", i)));
  }
  // `return ret.__from_py__(f(a1, ...))`
  auto ret_type =
      (ret && !ret->get_none()) ? ret->clone() : N<IdExpr>("NoneType");
  auto ret_expr =
      N<CallExpr>(N<DotExpr>(ret_type->clone(), "__from_py__"),
                  N<DotExpr>(N<CallExpr>(N<IdExpr>("f"), call_args), "p"));
  auto ret_stmt = N<ReturnStmt>(ret_expr);
  // Create a function
  return transform(
      N<FunctionStmt>(alt_name.empty() ? components.back() : alt_name, ret_type,
                      params, N<SuiteStmt>(call, ret_stmt)));
}

/// Import a new file into its own context and wrap its top-level statements
/// into a function to support Python-like runtime import loading.
/// @example
///   ```_import_[I]_done = False
///      def _import_[I]():
///        global [imported global variables]...
///        __name__ = [I]
///        [imported top-level statements]```
auto SimplifyVisitor::transform_new_import(const ImportFile& file) -> StmtPtr {
  // Use a clean context to parse a new file
  if (ctx->cache->age)
    ctx->cache->age++;
  auto ictx = std::make_shared<SimplifyContext>(file.path, ctx->cache);
  ictx->is_stdlib_loading = ctx->is_stdlib_loading;
  ictx->module_name = file;
  auto import =
      ctx->cache->imports.insert({file.path, {file.path, ictx}}).first;
  import->second.module_name = file.module;

  // __name__ = [import name]
  StmtPtr n = N<AssignStmt>(N<IdExpr>("__name__"),
                            N<StringExpr>(ictx->module_name.module));
  if (ictx->module_name.module == "internal.core") {
    // str is not defined when loading internal.core; __name__ is not needed
    // anyway
    n = nullptr;
  }
  n = N<SuiteStmt>(n, Parse::parse_file(ctx->cache, file.path));
  n = SimplifyVisitor(ictx, preamble).transform(n);
  if (!ctx->cache->errors.empty())
    throw ParserException();
  // Add comment to the top of import for easier dump inspection
  auto comment =
      N<CommentStmt>(format("import: {} at {}", file.module, file.path));
  if (ctx->is_stdlib_loading) {
    // When loading the standard library, imports are not wrapped.
    // We assume that the standard library has no recursive imports and that all
    // statements are executed before the user-provided code.
    return N<SuiteStmt>(comment, n);
  } else {
    // Generate import identifier
    std::string import_var = import->second.import_var =
        ctx->cache->get_temporary_var(format("import_{}", file.module));
    std::string import_done_var;

    // `import_[I]_done = False` (set to True upon successful import)
    preamble->push_back(N<AssignStmt>(
        N<IdExpr>(import_done_var = import_var + "_done"), N<BoolExpr>(false)));
    ctx->cache->add_global(import_done_var);

    // Wrap all imported top-level statements into a function.
    // Make sure to register the global variables and set their assignments as
    // updates. Note: signatures/classes/functions are not wrapped
    std::vector<StmtPtr> stmts;
    auto process_toplevel_stmt = [&](const StmtPtr& s) {
      // Process toplevel statement
      if (auto a = s->get_assign()) {
        if (!a->is_update() && a->lhs->get_id()) {
          // Global `a = ...`
          auto val = ictx->force_find(a->lhs->get_id()->value);
          if (val->is_var() && val->is_global())
            ctx->cache->add_global(val->canonical_name);
        }
      }
      stmts.push_back(s);
    };
    process_toplevel_stmt(comment);
    if (auto st = n->get_suite()) {
      for (auto& ss : st->stmts)
        if (ss)
          process_toplevel_stmt(ss);
    } else {
      process_toplevel_stmt(n);
    }

    // Create import function manually with ForceRealize
    ctx->cache->functions[import_var + ":0"].ast =
        N<FunctionStmt>(import_var + ":0", nullptr, std::vector<Param>{},
                        N<SuiteStmt>(stmts), Attr({Attr::ForceRealize}));
    preamble->push_back(ctx->cache->functions[import_var + ":0"].ast->clone());
    ctx->cache->overloads[import_var].push_back(
        {import_var + ":0", ctx->cache->age});
  }
  return nullptr;
}

}  // namespace Pud::AST