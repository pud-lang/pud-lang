#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Parse/Parser.h"
#include "Pud/Simplify/Simplify.h"

namespace Pud::AST {

/// Ensure that `(yield)` is in a function.
void SimplifyVisitor::visit(YieldExpr* expr) {
  if (!ctx->in_function())
    Err(Error::FN_OUTSIDE_ERROR, expr, "yield");
}

/// Transform lambdas. Capture outer expressions.
/// @example
///   `lambda a, b: a+b+c` -> ```def fn(a, b, c):
///                                return a+b+c
///                              fn(c=c, ...)```
/// See @c make_anon_fn
void SimplifyVisitor::visit(LambdaExpr* expr) {
  result_expr = make_anon_fn(
      std::vector<StmtPtr>{N<ReturnStmt>(clone(expr->expr))}, expr->vars);
}

/// Ensure that `return` is in a function.
void SimplifyVisitor::visit(ReturnStmt* stmt) {
  if (!ctx->in_function())
    Err(Error::FN_OUTSIDE_ERROR, stmt, "return");
  transform(stmt->expr);
}

/// Ensure that `yield` is in a function.
void SimplifyVisitor::visit(YieldStmt* stmt) {
  if (!ctx->in_function())
    Err(Error::FN_OUTSIDE_ERROR, stmt, "yield");
  transform(stmt->expr);
}

/// Transform `yield from` statements.
/// @example
///   `yield from a` -> `for var in a: yield var`
void SimplifyVisitor::visit(YieldFromStmt* stmt) {
  auto var = ctx->cache->get_temporary_var("yield");
  result_stmt = transform(
      N<ForStmt>(N<IdExpr>(var), stmt->expr, N<YieldStmt>(N<IdExpr>(var))));
}

/// Process `global` statements. Remove them upon completion.
void SimplifyVisitor::visit(GlobalStmt* stmt) {
  if (!ctx->in_function())
    Err(Error::FN_OUTSIDE_ERROR, stmt, stmt->non_local ? "nonlocal" : "global");

  // Dominate the binding
  auto val = ctx->find_dominating_binding(stmt->var);
  if (!val || !val->is_var())
    Err(Error::ID_NOT_FOUND, stmt, stmt->var);
  if (val->get_base_name() == ctx->get_base_name())
    Err(Error::FN_GLOBAL_ASSIGNED, stmt, stmt->var);

  // Check global/nonlocal distinction
  if (!stmt->non_local && !val->get_base_name().empty())
    Err(Error::FN_GLOBAL_NOT_FOUND, stmt, "global", stmt->var);
  else if (stmt->non_local && val->get_base_name().empty())
    Err(Error::FN_GLOBAL_NOT_FOUND, stmt, "nonlocal", stmt->var);
  // seqassert(!val->canonical_name.empty(), "'{}' does not have a canonical
  // name",
  //           stmt->var);

  // Register as global if needed
  ctx->cache->add_global(val->canonical_name);

  val = ctx->add_var(stmt->var, val->canonical_name, stmt->get_source_info());
  val->base_name = ctx->get_base_name();
  // Globals/nonlocals cannot be shadowed in children scopes (as in Python)
  val->no_shadow = true;
  // Erase the statement
  result_stmt = N<SuiteStmt>();
}

/// Validate and transform function definitions.
/// Handle overloads, class methods, default arguments etc.
/// Also capture variables if necessary and apply decorators.
/// @example
///   ```a = 5
///      @dec
///      def foo(b):
///        return a+b
///   ``` -> ```
///      a = 5
///      def foo(b, a_cap):
///        return a_cap+b
///      foo = dec(foo(a_cap=a, ...))
///   ```
/// For Python and LLVM definition transformations, see
/// @c transform_python_definition and @c transform_llvm_definition
void SimplifyVisitor::visit(FunctionStmt* stmt) {
  if (stmt->attributes.has(Attr::Python)) {
    // Handle Python block
    result_stmt = transform_python_definition(
        stmt->name, stmt->args, stmt->ret.get(), stmt->suite->first_in_block());
    return;
  }

  // Parse attributes
  for (auto i = stmt->decorators.size(); i-- > 0;) {
    auto [isAttr, attrName] = get_decorator(stmt->decorators[i]);
    if (!attrName.empty()) {
      stmt->attributes.set(attrName);
      if (isAttr)
        stmt->decorators[i] = nullptr;  // remove it from further consideration
    }
  }

  bool is_class_member = ctx->in_class();
  bool is_enclosed_func = ctx->in_function();
  if (stmt->attributes.has(Attr::ForceRealize) &&
      (!ctx->is_global() || is_class_member))
    Err(Error::EXPECTED_TOPLEVEL, get_source_info(), "builtin function");

  // All overloads share the same canonical name except for the number at the
  // end (e.g., `foo.1:0`, `foo.1:1` etc.)
  std::string root_name;
  if (is_class_member) {
    // Case 1: method overload
    if (auto n =
            in(ctx->cache->classes[ctx->get_base()->name].methods, stmt->name))
      root_name = *n;
  } else if (stmt->attributes.has(Attr::Overload)) {
    // Case 2: function overload
    if (auto c = ctx->find(stmt->name)) {
      if (c->is_func() && c->get_module() == ctx->get_module() &&
          c->get_base_name() == ctx->get_base_name())
        root_name = c->canonical_name;
    }
  }
  if (root_name.empty())
    root_name = ctx->generate_canonical_name(stmt->name, true);
  // Append overload number to the name
  auto canonical_name =
      fmt::format("{}:{}", root_name, ctx->cache->overloads[root_name].size());
  ctx->cache->reverse_identifier_lookup[canonical_name] = stmt->name;

  // Ensure that function binding does not shadow anything.
  // Function bindings cannot be dominated either
  if (!is_class_member) {
    auto func_val = ctx->find(stmt->name);
    if (func_val && func_val->no_shadow)
      Err(Error::CLASS_INVALID_BIND, stmt, stmt->name);
    func_val = ctx->add_func(stmt->name, root_name, stmt->get_source_info());
    ctx->add_always_visible(func_val);
  }

  std::vector<Param> args;
  StmtPtr suite = nullptr;
  ExprPtr ret = nullptr;
  std::unordered_map<std::string, std::pair<std::string, ExprPtr>> captures;
  std::unordered_set<std::string> py_captures;
  {
    // Set up the base
    SimplifyContext::BaseGuard br(ctx.get(), canonical_name);
    ctx->get_base()->attributes = &(stmt->attributes);

    // Parse arguments and add them to the context
    for (auto& a : stmt->args) {
      std::string var_name = a.name;
      int stars = trim_stars(var_name);
      auto name = ctx->generate_canonical_name(var_name);

      // Mark as method if the first argument is self
      if (is_class_member && stmt->attributes.has(Attr::HasSelf) &&
          a.name == "self") {
        ctx->get_base()->self_name = name;
        stmt->attributes.set(Attr::Method);
      }

      // Handle default values
      auto default_value = a.default_value;
      if (a.type && default_value && default_value->get_none()) {
        // Special case: `arg: Callable = None` -> `arg: Callable = NoneType()`
        if (a.type->get_index() &&
            a.type->get_index()->expr->is_id(TYPE_CALLABLE))
          default_value = N<CallExpr>(N<IdExpr>("NoneType"));
        // Special case: `arg: type = None` -> `arg: type = NoneType`
        if (a.type->is_id("type") || a.type->is_id(TYPE_TYPEVAR))
          default_value = N<IdExpr>("NoneType");
      }
      /// TODO: Uncomment for Python-style defaults
      // if (default_value) {
      //   auto default_valueCanonicalName =
      //       ctx->generate_canonical_name(format("{}.{}", canonical_name,
      //       name));
      //   prepend_stmts->push_back(N<AssignStmt>(N<IdExpr>(default_valueCanonicalName),
      //     default_value));
      //   default_value = N<IdExpr>(default_valueCanonicalName);
      // }
      args.emplace_back(Param{std::string(stars, '*') + name, a.type,
                              default_value, a.status});

      // Add generics to the context
      if (a.status != Param::Normal) {
        if (auto st = get_static_generic(a.type.get())) {
          auto val = ctx->add_var(var_name, name, stmt->get_source_info());
          val->generic = true;
          val->static_type = st;
        } else {
          ctx->add_type(var_name, name, stmt->get_source_info())->generic =
              true;
        }
      }
    }

    // Parse arguments to the context. Needs to be done after adding generics
    // to support cases like `foo(a: T, T: type)`
    for (auto& a : args) {
      a.type = transform_type(a.type, false);
      a.default_value = transform(a.default_value, true);
    }
    // Add non-generic arguments to the context. Delayed to prevent cases like
    // `def foo(a, b=a)`
    for (auto& a : args) {
      if (a.status == Param::Normal) {
        std::string can_name = a.name;
        trim_stars(can_name);
        ctx->add_var(ctx->cache->rev(can_name), can_name,
                     stmt->get_source_info());
      }
    }

    // Parse the return type
    ret = transform_type(stmt->ret, false);

    // Parse function body
    if (!stmt->attributes.has(Attr::Internal) &&
        !stmt->attributes.has(Attr::C)) {
      if (stmt->attributes.has(Attr::LLVM)) {
        suite = transform_llvm_definition(stmt->suite->first_in_block());
      } else if (stmt->attributes.has(Attr::C)) {
        // Do nothing
      } else {
        if ((is_enclosed_func || stmt->attributes.has(Attr::Capture)) &&
            !is_class_member)
          ctx->get_base()->captures = &captures;
        if (stmt->attributes.has("std.internal.attributes.pycapture"))
          ctx->get_base()->py_captures = &py_captures;
        suite = SimplifyVisitor(ctx, preamble)
                    .transform_conditional_scope(stmt->suite);
      }
    }
  }
  stmt->attributes.module = fmt::format(
      "{}{}", ctx->module_name.status == ImportFile::STDLIB ? "std::" : "::",
      ctx->module_name.module);
  ctx->cache->overloads[root_name].push_back({canonical_name, ctx->cache->age});

  // Special method handling
  if (is_class_member) {
    // Set the enclosing class name
    stmt->attributes.parent_class = ctx->get_base()->name;
    // Add the method to the class' method list
    ctx->cache->classes[ctx->get_base()->name].methods[stmt->name] = root_name;
  } else {
    // Hack so that we can later use same helpers for class overloads
    ctx->cache->classes[".toplevel"].methods[stmt->name] = root_name;
  }

  // Handle captures. Add additional argument to the function for every capture.
  // Make sure to account for **kwargs if present
  std::vector<CallExpr::Arg> partial_args;
  if (!captures.empty()) {
    Param kw;
    if (!args.empty() && startswith(args.back().name, "**")) {
      kw = args.back();
      args.pop_back();
    }
    for (auto& c : captures) {
      args.emplace_back(Param{c.second.first, c.second.second, nullptr});
      partial_args.push_back(
          {c.second.first, N<IdExpr>(ctx->cache->rev(c.first))});
    }
    if (!kw.name.empty())
      args.push_back(kw);
    partial_args.emplace_back("", N<EllipsisExpr>(EllipsisExpr::PARTIAL));
  }
  // Make function AST and cache it for later realization
  auto f = N<FunctionStmt>(canonical_name, ret, args, suite, stmt->attributes);
  ctx->cache->functions[canonical_name].ast = f;
  ctx->cache->functions[canonical_name].original_ast =
      std::static_pointer_cast<FunctionStmt>(stmt->clone());
  ctx->cache->functions[canonical_name].is_toplevel =
      ctx->get_module().empty() && ctx->is_global();
  ctx->cache->functions[canonical_name].root_name = root_name;

  // Expression to be used if function binding is modified by captures or
  // decorators
  ExprPtr final_expr = nullptr;
  // If there are captures, replace `fn` with `fn(cap1=cap1, cap2=cap2, ...)`
  if (!captures.empty()) {
    final_expr = N<CallExpr>(N<IdExpr>(stmt->name), partial_args);
    // Add updated self reference in case function is recursive!
    auto pa = partial_args;
    for (auto& a : pa) {
      if (!a.name.empty())
        a.value = N<IdExpr>(a.name);
      else
        a.value = clone(a.value);
    }
    f->suite =
        N<SuiteStmt>(N<AssignStmt>(N<IdExpr>(root_name),
                                   N<CallExpr>(N<IdExpr>(root_name), pa)),
                     suite);
  }

  // Parse remaining decorators
  for (auto i = stmt->decorators.size(); i-- > 0;) {
    if (stmt->decorators[i]) {
      if (is_class_member)
        Err(Error::FN_NO_DECORATORS, stmt->decorators[i]);
      // Replace each decorator with `decorator(finalExpr)` in the reverse order
      final_expr = N<CallExpr>(stmt->decorators[i],
                               final_expr ? final_expr : N<IdExpr>(stmt->name));
    }
  }

  if (final_expr) {
    result_stmt = N<SuiteStmt>(
        f, transform(N<AssignStmt>(N<IdExpr>(stmt->name), final_expr)));
  } else {
    result_stmt = f;
  }
}

/// Make a capturing anonymous function with the provided suite and argument
/// names. The resulting function will be added before the current statement.
/// Return an expression that can call this function (an @c IdExpr or a partial
/// call).
ExprPtr SimplifyVisitor::make_anon_fn(
    std::vector<StmtPtr> suite, const std::vector<std::string>& arg_names) {
  std::vector<Param> params;
  std::string name = ctx->cache->get_temporary_var("lambda");
  params.reserve(arg_names.size());
  for (auto& s : arg_names)
    params.emplace_back(Param(s));
  auto f = transform(N<FunctionStmt>(name, nullptr, params,
                                     N<SuiteStmt>(std::move(suite)),
                                     Attr({Attr::Capture})));
  if (auto fs = f->get_suite()) {
    // seqassert(fs->stmts.size() == 2 && fs->stmts[0]->getFunction(),
    //           "invalid function transform");
    prepend_stmts->push_back(fs->stmts[0]);
    for (StmtPtr s = fs->stmts[1]; s;) {
      if (auto suite = s->get_suite()) {
        // Suites can only occur when captures are inserted for a partial call
        // argument.
        // seqassert(suite->stmts.size() == 2, "invalid function transform");
        prepend_stmts->push_back(suite->stmts[0]);
        s = suite->stmts[1];
      } else if (auto assign = s->get_assign()) {
        return assign->rhs;
      } else {
        // seqassert(false, "invalid function transform");
      }
    }
    return nullptr;  // should fail an assert before
  } else {
    prepend_stmts->push_back(f);
    return transform(N<IdExpr>(name));
  }
}

/// Transform Python code blocks.
/// @example
///   ```@python
///      def foo(x: int, y) -> int:
///        [code]
///   ``` -> ```
///      pyobj._exec("def foo(x, y): [code]")
///      from python import __main__.foo(int, _) -> int
///   ```
auto SimplifyVisitor::transform_python_definition(
    const std::string& name, const std::vector<Param>& args, const Expr* ret,
    Stmt* code_stmt) -> StmtPtr {
  // seqassert(
  //     code_stmt && code_stmt->get_expr() &&
  //     code_stmt->get_expr()->expr->get_string(), "invalid Python
  //     definition");

  auto code = code_stmt->get_expr()->expr->get_string()->get_value();
  std::vector<std::string> pyargs;
  pyargs.reserve(args.size());
  for (const auto& a : args)
    pyargs.emplace_back(a.name);
  code = fmt::format("def {}({}):\n{}\n", name, join(pyargs, ", "), code);
  return transform(N<SuiteStmt>(
      N<ExprStmt>(
          N<CallExpr>(N<DotExpr>("pyobj", "_exec"), N<StringExpr>(code))),
      N<ImportStmt>(N<IdExpr>("python"), N<DotExpr>("__main__", name),
                    clone_nop(args), ret ? ret->clone() : N<IdExpr>("pyobj"))));
}

/// Transform LLVM functions.
/// @example
///   ```@llvm
///      def foo(x: int) -> float:
///        [code]
///   ``` -> ```
///      def foo(x: int) -> float:
///        StringExpr("[code]")
///        SuiteStmt(referenced_types)
///   ```
/// As LLVM code can reference types and static expressions in `{=expr}` blocks,
/// all block expression will be stored in the `referenced_types` suite.
/// "[code]" is transformed accordingly: each `{=expr}` block will
/// be replaced with `{}` so that @c fmt::format can fill the gaps.
/// Note that any brace (`{` or `}`) that is not part of a block is
/// escaped (e.g. `{` -> `{{` and `}` -> `}}`) so that @c fmt::format can
/// process them.
auto SimplifyVisitor::transform_llvm_definition(Stmt* code_stmt) -> StmtPtr {
  // seqassert(
  //     code_stmt && code_stmt->get_expr() &&
  //     code_stmt->getExpr()->expr->getString(), "invalid LLVM definition");

  auto code = code_stmt->get_expr()->expr->get_string()->get_value();
  std::vector<StmtPtr> items;
  auto se = N<StringExpr>("");
  std::string final_code = se->get_value();
  items.push_back(N<ExprStmt>(se));

  // Parse LLVM code and look for expression blocks that start with `{=`
  int brace_count = 0;
  int brace_start = 0;
  for (int i = 0; i < code.size(); i++) {
    if (i < code.size() - 1 && code[i] == '{' && code[i + 1] == '=') {
      if (brace_start < i)
        final_code +=
            escape_fstring_braces(code, brace_start, i - brace_start) + '{';
      if (!brace_count) {
        brace_start = i + 2;
        brace_count++;
      } else {
        Err(Error::FN_BAD_LLVM, get_source_info());
      }
    } else if (brace_count && code[i] == '}') {
      brace_count--;
      std::string expr_code = code.substr(brace_start, i - brace_start);
      auto offset = get_source_info();
      offset.column += i;
      auto expr = transform(
          Parse::parse_expr(ctx->cache, expr_code, offset).first, true);
      items.push_back(N<ExprStmt>(expr));
      brace_start = i + 1;
      final_code += '}';
    }
  }
  if (brace_count)
    Err(Error::FN_BAD_LLVM, get_source_info());
  if (brace_start != code.size())
    final_code += escape_fstring_braces(code, brace_start,
                                        int(code.size()) - brace_start);
  se->strings[0].first = final_code;
  return N<SuiteStmt>(items);
}

/// Fetch a decorator canonical name. The first pair member indicates if a
/// decorator is actually an attribute (a function with `@__attribute__`).
auto SimplifyVisitor::get_decorator(const ExprPtr& e)
    -> std::pair<bool, std::string> {
  auto dt = transform(clone(e));
  auto id = dt->get_call() ? dt->get_call()->expr : dt;
  if (id && id->get_id()) {
    auto ci = ctx->find(id->get_id()->value);
    if (ci && ci->is_func()) {
      if (ctx->cache->overloads[ci->canonical_name].size() == 1) {
        return {
            ctx->cache
                ->functions[ctx->cache->overloads[ci->canonical_name][0].name]
                .ast->attributes.is_attribute,
            ci->canonical_name};
      }
    }
  }
  return {false, ""};
}

}  // namespace Pud::AST