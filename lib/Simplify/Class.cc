#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"

namespace Pud::AST {

// 用于转换类定义和类型定义。
void SimplifyVisitor::visit(ClassStmt* stmt) {
  // 获取类的名称 name。
  std::string name = stmt->name;

  // 用于存储类的规范名称。也是唯一id。
  std::string canonical_name;
  // 引用类声明中的参数列表 args_to_parse 以便后续解析。
  std::vector<Param>& args_to_parse = stmt->args;

  // 创建一个 SimplifyItem 实例 class_item，表示这个类。
  // 这个实例用于存储关于类的信息，如类型和作用域。
  auto class_item = std::make_shared<SimplifyItem>(
      SimplifyItem::Type, "", "", ctx->get_module(), ctx->scope.blocks);
  // 设置类的源信息。
  class_item->set_source_info(stmt->get_source_info());
  // 如果类不是扩展（extend）的类，则生成并设置类的规范名称。
  if (!stmt->attributes.has(Attr::Extend)) {
    class_item->canonical_name = canonical_name = ctx->generate_canonical_name(
        name, !stmt->attributes.has(Attr::Internal));
    // 如果类不是元组类型，将类添加到上下文中。
    if (!stmt->attributes.has(Attr::Tuple)) {
      ctx->add(name, class_item);
      ctx->add_always_visible(class_item);
    }
  } else {
    // 对于扩展的类，首先检查它是否在全局作用域中定义。
    if (!ctx->is_global() || ctx->is_conditional())
      Err(Error::EXPECTED_TOPLEVEL, get_source_info(), "class extension");
    auto val = ctx->find(name);
    if (!val || !val->is_type())
      Err(Error::CLASS_ID_NOT_FOUND, get_source_info(), name);
    canonical_name = val->canonical_name;
    // 查找要扩展的类的规范名称和AST。
    const auto& ast_iter = ctx->cache->classes.find(canonical_name);
    if (ast_iter == ctx->cache->classes.end()) {
      Err(Error::CLASS_ID_NOT_FOUND, get_source_info(), name);
    } else {
      args_to_parse = ast_iter->second.ast->args;
    }
  }

  std::vector<StmtPtr> cls_stmts;
  std::vector<StmtPtr> var_stmts;
  std::vector<StmtPtr> fn_stmts;
  std::vector<SimplifyContext::Item> add_later;
  try {
    // 用于管理类的作用域。
    SimplifyContext::BaseGuard br(ctx.get(), canonical_name);

    // 用于存储类参数的列表。
    std::vector<Param> args;
    // 用于自动推断初始化方法的变量。
    std::pair<StmtPtr, FunctionStmt*> auto_deduced_init{nullptr, nullptr};
    // 解析并添加类的泛型参数。
    if (stmt->attributes.has("deduce") && args.empty()) {
      // Auto-detect generics and fields
      auto_deduced_init = auto_deduce_members(stmt, args);
    } else {
      // Add all generics before parent classes, fields and methods
      for (auto& a : args_to_parse) {
        if (a.status != Param::Generic)
          continue;
        std::string gen_name, var_name;
        if (stmt->attributes.has(Attr::Extend))
          var_name = a.name, gen_name = ctx->cache->rev(a.name);
        else
          var_name = ctx->generate_canonical_name(a.name), gen_name = a.name;
        if (auto st = get_static_generic(a.type.get())) {
          auto val =
              ctx->add_var(gen_name, var_name, a.type->get_source_info());
          val->generic = true;
          val->static_type = st;
        } else {
          ctx->add_type(gen_name, var_name, a.type->get_source_info())
              ->generic = true;
        }
        args.emplace_back(var_name, transform_type(clone(a.type), false),
                          transform_type(clone(a.default_value), false),
                          a.status);
        if (!stmt->attributes.has(Attr::Extend) && a.status == Param::Normal)
          ctx->cache->classes[canonical_name].fields.push_back(
              Cache::Class::ClassField{var_name, nullptr, canonical_name});
      }
    }

    // 形成类类型节点，例如 Foo 或 Foo[T, U]。
    ExprPtr type_ast = N<IdExpr>(name),
            transformed_type_ast = NT<IdExpr>(canonical_name);
    for (auto& a : args) {
      if (a.status == Param::Generic) {
        if (!type_ast->get_index()) {
          type_ast = N<IndexExpr>(N<IdExpr>(name), N<TupleExpr>());
          transformed_type_ast = NT<InstantiateExpr>(NT<IdExpr>(canonical_name),
                                                   std::vector<ExprPtr>{});
        }
        type_ast->get_index()->index->get_tuple()->items.push_back(
            N<IdExpr>(a.name));
        CAST(transformed_type_ast, InstantiateExpr)
            ->type_params.push_back(transform(N<IdExpr>(a.name), true));
      }
    }

    // 收集静态继承的类及其字段。
    std::vector<ClassStmt*> static_base_asts, base_asts;
    if (!stmt->attributes.has(Attr::Extend)) {
      static_base_asts = parse_base_classes(stmt->static_base_classes, args,
                                        stmt->attributes, canonical_name);
      if (ctx->cache->is_jit && !stmt->base_classes.empty())
        Err(Error::CUSTOM, stmt->base_classes[0],
          "inheritance is not yet supported in JIT mode");
      parse_base_classes(stmt->base_classes, args, stmt->attributes,
                       canonical_name, transformed_type_ast);
    }

    // 转换嵌套类。
    transform_nested_classes(stmt, cls_stmts, var_stmts, fn_stmts);

    // 收集类字段并处理类变量。
    for (auto& a : args_to_parse) {
      if (a.status == Param::Normal) {
        if (!ClassStmt::is_class_var(a)) {
          args.emplace_back(a.name, transform_type(clone(a.type), false),
                            transform(clone(a.default_value), true));
          if (!stmt->attributes.has(Attr::Extend)) {
            ctx->cache->classes[canonical_name].fields.push_back(
                Cache::Class::ClassField{a.name, nullptr, canonical_name});
          }
        } else if (!stmt->attributes.has(Attr::Extend)) {
          // Handle class variables. Transform them later to allow
          // self-references
          auto name = fmt::format("{}.{}", canonical_name, a.name);
          preamble->push_back(N<AssignStmt>(N<IdExpr>(name), nullptr, nullptr));
          ctx->cache->add_global(name);
          auto assign =
              N<AssignStmt>(N<IdExpr>(name), a.default_value,
                            a.type ? a.type->get_index()->index : nullptr);
          assign->set_update();
          var_stmts.push_back(assign);
          ctx->cache->classes[canonical_name].class_vars[a.name] = name;
        }
      }
    }

    // ASTs for member arguments to be used for populating magic methods
    std::vector<Param> member_args;
    for (auto& a : args) {
      if (a.status == Param::Normal)
        member_args.push_back(a.clone());
    }

    // Parse class members (arguments) and methods
    if (!stmt->attributes.has(Attr::Extend)) {
      // Now that we are done with arguments, add record type to the context
      if (stmt->attributes.has(Attr::Tuple)) {
        // Ensure that class binding does not shadow anything.
        // Class bindings cannot be dominated either
        auto v = ctx->find(name);
        if (v && v->no_shadow)
          Err(Error::CLASS_INVALID_BIND, stmt, name);
        ctx->add(name, class_item);
        ctx->add_always_visible(class_item);
      }
      // Create a cached AST.
      stmt->attributes.module = fmt::format(
          "{}{}", ctx->module_name.status == ImportFile::STDLIB ? "std::" : "::",
          ctx->module_name.module);
      ctx->cache->classes[canonical_name].ast =
          N<ClassStmt>(canonical_name, args, N<SuiteStmt>(), stmt->attributes);
      ctx->cache->classes[canonical_name].ast->base_classes = stmt->base_classes;
      for (auto& b : static_base_asts)
        ctx->cache->classes[canonical_name].static_parent_classes.emplace_back(
            b->name);
      ctx->cache->classes[canonical_name].ast->validate();
      ctx->cache->classes[canonical_name].module = ctx->get_module();

      // Codegen default magic methods
      for (auto& m : stmt->attributes.magics) {
        fn_stmts.push_back(transform(codegen_magic(
            m, type_ast, member_args, stmt->attributes.has(Attr::Tuple))));
      }
      // Add inherited methods
      for (auto& base : static_base_asts) {
        for (auto& mm : ctx->cache->classes[base->name].methods)
          for (auto& mf : ctx->cache->overloads[mm.second]) {
            auto f = ctx->cache->functions[mf.name].ast;
            if (!f->attributes.has("autogenerated")) {
              std::string rootName;
              auto& mts = ctx->cache->classes[ctx->get_base()->name].methods;
              auto it = mts.find(ctx->cache->rev(f->name));
              if (it != mts.end())
                rootName = it->second;
              else
                rootName = ctx->generate_canonical_name(
                    ctx->cache->rev(f->name), true);
              auto newCanonicalName = fmt::format(
                  "{}:{}", rootName, ctx->cache->overloads[rootName].size());
              ctx->cache->overloads[rootName].push_back(
                  {newCanonicalName, ctx->cache->age});
              ctx->cache->reverse_identifier_lookup[newCanonicalName] =
                  ctx->cache->rev(f->name);
              auto nf = std::dynamic_pointer_cast<FunctionStmt>(f->clone());
              nf->name = newCanonicalName;
              nf->attributes.parent_class = ctx->get_base()->name;
              ctx->cache->functions[newCanonicalName].ast = nf;
              ctx->cache->classes[ctx->get_base()->name]
                  .methods[ctx->cache->rev(f->name)] = rootName;
              fn_stmts.push_back(nf);
            }
          }
      }
      // Add auto-deduced __init__ (if available)
      if (auto_deduced_init.first)
        fn_stmts.push_back(auto_deduced_init.first);
    }
    // Add class methods
    for (const auto& sp : get_class_methods(stmt->suite))
      if (sp && sp->get_function()) {
        if (sp.get() != auto_deduced_init.second)
          fn_stmts.push_back(transform(sp));
      }

    // After popping context block, record types and nested classes will
    // disappear. Store their references and re-add them to the context after
    // popping
    add_later.reserve(cls_stmts.size() + 1);
    for (auto& c : cls_stmts)
      add_later.push_back(ctx->find(c->get_class()->name));
    if (stmt->attributes.has(Attr::Tuple))
      add_later.push_back(ctx->force_find(name));

    // Mark functions as virtual:
    auto banned = std::set<std::string>{"__init__", "__new__", "__raw__",
                                        "__tuplesize__"};
    for (auto& m : ctx->cache->classes[canonical_name].methods) {
      auto method = m.first;
      for (size_t mi = 1; mi < ctx->cache->classes[canonical_name].mro.size();
           mi++) {
        // ... in the current class
        auto b = ctx->cache->classes[canonical_name].mro[mi]->get_type_name();
        if (in(ctx->cache->classes[b].methods, method) && !in(banned, method)) {
          ctx->cache->classes[canonical_name].virtuals.insert(method);
        }
      }
      for (auto& v : ctx->cache->classes[canonical_name].virtuals) {
        for (size_t mi = 1; mi < ctx->cache->classes[canonical_name].mro.size();
             mi++) {
          // ... and in parent classes
          auto b = ctx->cache->classes[canonical_name].mro[mi]->get_type_name();
          ctx->cache->classes[b].virtuals.insert(v);
        }
      }
    }
  } catch (const ParserException&) {
    if (!stmt->attributes.has(Attr::Tuple))
      ctx->remove(name);
    ctx->cache->classes.erase(name);
    throw;
  }
  for (auto& i : add_later)
    ctx->add(ctx->cache->rev(i->canonical_name), i);

  // Extensions are not needed as the cache is already populated
  if (!stmt->attributes.has(Attr::Extend)) {
    auto c = ctx->cache->classes[canonical_name].ast;
    // seqassert(c, "not a class AST for {}", canonical_name);
    cls_stmts.push_back(c);
  }

  cls_stmts.insert(cls_stmts.end(), fn_stmts.begin(), fn_stmts.end());
  for (auto& a : var_stmts) {
    // Transform class variables here to allow self-references
    if (auto assign = a->get_assign()) {
      transform(assign->rhs);
      transform_type(assign->type);
    }
    cls_stmts.push_back(a);
  }
  result_stmt = N<SuiteStmt>(cls_stmts);
}

/// Parse statically inherited classes.
/// Returns a list of their ASTs. Also updates the class fields.
/// @param args Class fields that are to be updated with base classes' fields.
/// @param type_ast Transformed AST for base class type (e.g., `A[T]`).
///                Only set when dealing with dynamic polymorphism.
std::vector<ClassStmt*> SimplifyVisitor::parse_base_classes(
    std::vector<ExprPtr>& base_classes, std::vector<Param>& args,
    const Attr& attr, const std::string& canonical_name,
    const ExprPtr& type_ast) {
  std::vector<ClassStmt*> asts;

  // MAJOR TODO: fix MRO it to work with generic classes (maybe replacements?
  // IDK...)
  std::vector<std::vector<ExprPtr>> mro{{type_ast}};
  std::vector<ExprPtr> parent_classes;
  for (auto& cls : base_classes) {
    std::string name;
    std::vector<ExprPtr> subs;

    // Get the base class and generic replacements (e.g., if there is Bar[T],
    // Bar in Foo(Bar[int]) will have `T = int`)
    transform_type(cls);
    if (auto i = cls->get_id()) {
      name = i->value;
    } else if (auto e = CAST(cls, InstantiateExpr)) {
      if (auto ei = e->type_expr->get_id()) {
        name = ei->value;
        subs = e->type_params;
      }
    }

    auto cachedCls = const_cast<Cache::Class*>(in(ctx->cache->classes, name));
    if (!cachedCls)
      Err(Error::CLASS_ID_NOT_FOUND, get_source_info(), ctx->cache->rev(name));
    asts.push_back(cachedCls->ast.get());
    parent_classes.push_back(clone(cls));
    mro.push_back(cachedCls->mro);

    // Sanity checks
    if (attr.has(Attr::Tuple) && type_ast)
      Err(Error::CLASS_NO_INHERIT, get_source_info(), "tuple");
    if (!attr.has(Attr::Tuple) && asts.back()->attributes.has(Attr::Tuple))
      Err(Error::CLASS_TUPLE_INHERIT, get_source_info());
    if (asts.back()->attributes.has(Attr::Internal))
      Err(Error::CLASS_NO_INHERIT, get_source_info(), "internal");

    // Mark parent classes as polymorphic as well.
    if (type_ast) {
      cachedCls->rtti = true;
    }

    // Add generics first
    int nGenerics = 0;
    for (auto& a : asts.back()->args)
      nGenerics += a.status == Param::Generic;
    int si = 0;
    for (auto& a : asts.back()->args) {
      if (a.status == Param::Generic) {
        if (si == subs.size())
          Err(Error::GENERICS_MISMATCH, cls, ctx->cache->rev(asts.back()->name),
            nGenerics, subs.size());
        args.emplace_back(a.name, a.type, transform_type(subs[si++], false),
                          Param::HiddenGeneric);
      } else if (a.status == Param::HiddenGeneric) {
        args.emplace_back(a);
      }
      if (a.status != Param::Normal) {
        if (auto st = get_static_generic(a.type.get())) {
          auto val = ctx->add_var(a.name, a.name, a.type->get_source_info());
          val->generic = true;
          val->static_type = st;
        } else {
          ctx->add_type(a.name, a.name, a.type->get_source_info())->generic =
              true;
        }
      }
    }
    if (si != subs.size())
      Err(Error::GENERICS_MISMATCH, cls, ctx->cache->rev(asts.back()->name),
        nGenerics, subs.size());
  }
  // Add normal fields
  for (auto& ast : asts) {
    int ai = 0;
    for (auto& a : ast->args) {
      if (a.status == Param::Normal && !ClassStmt::is_class_var(a)) {
        auto name = a.name;
        int i = 0;
        for (auto& aa : args)
          i += aa.name == a.name || startswith(aa.name, a.name + "#");
        if (i)
          name = fmt::format("{}#{}", name, i);
        // seqassert(ctx->cache->classes[ast->name].fields[ai].name == a.name,
        //           "bad class fields: {} vs {}",
        //           ctx->cache->classes[ast->name].fields[ai].name, a.name);
        args.emplace_back(name, a.type, a.default_value);
        ctx->cache->classes[canonical_name].fields.push_back(
            Cache::Class::ClassField{
                name, nullptr,
                ctx->cache->classes[ast->name].fields[ai].base_class});
        ai++;
      }
    }
  }
  if (type_ast) {
    if (!parent_classes.empty()) {
      mro.push_back(parent_classes);
      ctx->cache->classes[canonical_name].rtti = true;
    }
    ctx->cache->classes[canonical_name].mro = Cache::merge_c3(mro);
    if (ctx->cache->classes[canonical_name].mro.empty()) {
      Err(Error::CLASS_BAD_MRO, get_source_info());
    } else if (ctx->cache->classes[canonical_name].mro.size() > 1) {
      // LOG("[mro] {} -> {}", canonical_name,
      // ctx->cache->classes[canonical_name].mro);
    }
  }
  return asts;
}

/// Find the first __init__ with self parameter and use it to deduce class
/// members. Each deduced member will be treated as generic.
/// @example
///   ```@deduce
///      class Foo:
///        def __init__(self):
///          self.x, self.y = 1, 2```
///   will result in
///   ```class Foo[T1, T2]:
///        x: T1
///        y: T2```
/// @return the transformed init and the pointer to the original function.
auto SimplifyVisitor::auto_deduce_members(
    ClassStmt* stmt, std::vector<Param>& args) -> std::pair<StmtPtr, FunctionStmt*> {
  std::pair<StmtPtr, FunctionStmt*> init{nullptr, nullptr};
  for (const auto& sp : get_class_methods(stmt->suite))
    if (sp && sp->get_function()) {
      auto f = sp->get_function();
      if (f->name == "__init__" && !f->args.empty() &&
          f->args[0].name == "self") {
        // Set up deduced_members that will be populated during AssignStmt
        // evaluation
        ctx->get_base()->deduced_members =
            std::make_shared<std::vector<std::string>>();
        auto transformed = transform(sp);
        transformed->get_function()->attributes.set(Attr::RealizeWithoutSelf);
        ctx->cache->functions[transformed->get_function()->name]
            .ast->attributes.set(Attr::RealizeWithoutSelf);
        int i = 0;
        // Once done, add arguments
        for (auto& m : *(ctx->get_base()->deduced_members)) {
          auto var_name = ctx->generate_canonical_name(fmt::format("T{}", ++i));
          auto member_name = ctx->cache->rev(var_name);
          ctx->add_type(member_name, var_name, stmt->get_source_info())
              ->generic = true;
          args.emplace_back(var_name, N<IdExpr>("type"), nullptr,
                            Param::Generic);
          args.emplace_back(m, N<IdExpr>(var_name));
          ctx->cache->classes[stmt->name].fields.push_back(
              Cache::Class::ClassField{m, nullptr, stmt->name});
        }
        ctx->get_base()->deduced_members = nullptr;
        return {transformed, f};
      }
    }
  return {nullptr, nullptr};
}

/// Return a list of all statements within a given class suite.
/// Checks each suite recursively, and assumes that each statement is either
/// a function, a class or a docstring.
auto SimplifyVisitor::get_class_methods(const StmtPtr& s) -> std::vector<StmtPtr> {
  std::vector<StmtPtr> v;
  if (!s)
    return v;
  if (auto sp = s->get_suite()) {
    for (const auto& ss : sp->stmts)
      for (const auto& u : get_class_methods(ss))
        v.push_back(u);
  } else if (s->get_expr() && s->get_expr()->expr->get_string()) {
    /// Those are doc-strings, ignore them.
  } else if (!s->get_function() && !s->get_class()) {
    Err(Error::CLASS_BAD_ATTR, s);
  } else {
    v.push_back(s);
  }
  return v;
}

/// Extract nested classes and transform them before the main class.
void SimplifyVisitor::transform_nested_classes(ClassStmt* stmt,
                                             std::vector<StmtPtr>& cls_stmts,
                                             std::vector<StmtPtr>& var_stmts,
                                             std::vector<StmtPtr>& fn_stmts) {
  for (const auto& sp : get_class_methods(stmt->suite))
    if (sp && sp->get_class()) {
      auto orig_name = sp->get_class()->name;
      // If class B is nested within A, it's name is always A.B, never B itself.
      // Ensure that parent class name is appended
      auto parent_name = stmt->name;
      sp->get_class()->name = fmt::format("{}.{}", parent_name, orig_name);
      auto tsp = transform(sp);
      std::string name;
      if (tsp->get_suite()) {
        for (auto& s : tsp->get_suite()->stmts)
          if (auto c = s->get_class()) {
            cls_stmts.push_back(s);
            name = c->name;
          } else if (auto a = s->get_assign()) {
            var_stmts.push_back(s);
          } else {
            fn_stmts.push_back(s);
          }
        ctx->add(orig_name, ctx->force_find(name));
      }
    }
}

/// Generate a magic method `__op__` for each magic `op`
/// described by @param typ_expr and its arguments.
/// Currently generate:
/// @li Constructors: __new__, __init__
/// @li Utilities: __raw__, __hash__, __repr__, __tuplesize__, __add__, __mul__,
/// __len__
/// @li Iteration: __iter__, __getitem__, __len__, __contains__
/// @li Comparisons: __eq__, __ne__, __lt__, __le__, __gt__, __ge__
/// @li Pickling: __pickle__, __unpickle__
/// @li Python: __to_py__, __from_py__
/// @li GPU: __to_gpu__, __from_gpu__, __from_gpu_new__
/// TODO: move to Codon as much as possible
auto SimplifyVisitor::codegen_magic(const std::string& op,
                                      const ExprPtr& typ_expr,
                                      const std::vector<Param>& all_args,
                                      bool is_record) -> StmtPtr {
#define I(s) N<IdExpr>(s)
#define NS(x) N<DotExpr>(N<IdExpr>("__magic__"), (x))
  // seqassert(typ_expr, "typ_expr is null");
  ExprPtr ret;
  std::vector<Param> fargs;
  std::vector<StmtPtr> stmts;
  Attr attr;
  attr.set("autogenerated");

  std::vector<Param> args;
  for (auto& a : all_args)
    args.push_back(a);

  if (op == "new") {
    ret = typ_expr->clone();
    if (is_record) {
      // Tuples: def __new__() -> T (internal)
      for (auto& a : args)
        fargs.emplace_back(a.name, clone(a.type),
                           a.default_value ? clone(a.default_value)
                                          : N<CallExpr>(clone(a.type)));
      attr.set(Attr::Internal);
    } else {
      // Classes: def __new__() -> T
      stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), typ_expr->clone())));
    }
  }
  // else if (startswith(op, "new.")) {
  //   // special handle for tuple[t1, t2, ...]
  //   int sz = atoi(op.substr(4).c_str());
  //   std::vector<ExprPtr> ts;
  //   for (int i = 0; i < sz; i++) {
  //     fargs.emplace_back(format("a{}", i + 1), I(format("T{}", i + 1)));
  //     ts.emplace_back(I(format("T{}", i + 1)));
  //   }
  //   for (int i = 0; i < sz; i++) {
  //     fargs.emplace_back(format("T{}", i + 1), I("type"));
  //   }
  //   ret = N<InstantiateExpr>(I(TYPE_TUPLE), ts);
  //   ret->markType();
  //   attr.set(Attr::Internal);
  // }
  else if (op == "init") {
    // Classes: def __init__(self: T, a1: T1, ..., aN: TN) -> None:
    //            self.aI = aI ...
    ret = I("NoneType");
    fargs.emplace_back("self", typ_expr->clone());
    for (auto& a : args) {
      stmts.push_back(N<AssignStmt>(N<DotExpr>(I("self"), a.name), I(a.name)));
      fargs.emplace_back(
          a.name, clone(a.type),
          a.default_value ? clone(a.default_value) : N<CallExpr>(clone(a.type)));
    }
  } else if (op == "raw") {
    // Classes: def __raw__(self: T)
    fargs.emplace_back("self", typ_expr->clone());
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), I("self"))));
  } else if (op == "tuplesize") {
    // def __tuplesize__() -> int
    ret = I("int");
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op))));
  } else if (op == "getitem") {
    // Tuples: def __getitem__(self: T, index: int)
    fargs.emplace_back("self", typ_expr->clone());
    fargs.emplace_back("index", I("int"));
    stmts.emplace_back(
        N<ReturnStmt>(N<CallExpr>(NS(op), I("self"), I("index"))));
  } else if (op == "iter") {
    // Tuples: def __iter__(self: T)
    fargs.emplace_back("self", typ_expr->clone());
    stmts.emplace_back(N<YieldFromStmt>(N<CallExpr>(NS(op), I("self"))));
  } else if (op == "contains") {
    // Tuples: def __contains__(self: T, what) -> bool
    fargs.emplace_back("self", typ_expr->clone());
    fargs.emplace_back("what", nullptr);
    ret = I("bool");
    stmts.emplace_back(
        N<ReturnStmt>(N<CallExpr>(NS(op), I("self"), I("what"))));
  } else if (op == "eq" || op == "ne" || op == "lt" || op == "le" ||
             op == "gt" || op == "ge") {
    // def __op__(self: T, obj: T) -> bool
    fargs.emplace_back("self", typ_expr->clone());
    fargs.emplace_back("obj", typ_expr->clone());
    ret = I("bool");
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), I("self"), I("obj"))));
  } else if (op == "hash") {
    // def __hash__(self: T) -> int
    fargs.emplace_back("self", typ_expr->clone());
    ret = I("int");
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), I("self"))));
  } else if (op == "pickle") {
    // def __pickle__(self: T, dest: Ptr[byte])
    fargs.emplace_back("self", typ_expr->clone());
    fargs.emplace_back("dest", N<IndexExpr>(I("Ptr"), I("byte")));
    stmts.emplace_back(
        N<ReturnStmt>(N<CallExpr>(NS(op), I("self"), I("dest"))));
  } else if (op == "unpickle") {
    // def __unpickle__(src: Ptr[byte]) -> T
    fargs.emplace_back("src", N<IndexExpr>(I("Ptr"), I("byte")));
    ret = typ_expr->clone();
    stmts.emplace_back(
        N<ReturnStmt>(N<CallExpr>(NS(op), I("src"), typ_expr->clone())));
  } else if (op == "len") {
    // def __len__(self: T) -> int
    fargs.emplace_back("self", typ_expr->clone());
    ret = I("int");
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), I("self"))));
  } else if (op == "to_py") {
    // def __to_py__(self: T) -> Ptr[byte]
    fargs.emplace_back("self", typ_expr->clone());
    ret = N<IndexExpr>(I("Ptr"), I("byte"));
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), I("self"))));
  } else if (op == "from_py") {
    // def __from_py__(src: Ptr[byte]) -> T
    fargs.emplace_back("src", N<IndexExpr>(I("Ptr"), I("byte")));
    ret = typ_expr->clone();
    stmts.emplace_back(
        N<ReturnStmt>(N<CallExpr>(NS(op), I("src"), typ_expr->clone())));
  } else if (op == "to_gpu") {
    // def __to_gpu__(self: T, cache) -> T
    fargs.emplace_back("self", typ_expr->clone());
    fargs.emplace_back("cache");
    ret = typ_expr->clone();
    stmts.emplace_back(
        N<ReturnStmt>(N<CallExpr>(NS(op), I("self"), I("cache"))));
  } else if (op == "from_gpu") {
    // def __from_gpu__(self: T, other: T)
    fargs.emplace_back("self", typ_expr->clone());
    fargs.emplace_back("other", typ_expr->clone());
    stmts.emplace_back(
        N<ReturnStmt>(N<CallExpr>(NS(op), I("self"), I("other"))));
  } else if (op == "from_gpu_new") {
    // def __from_gpu_new__(other: T) -> T
    fargs.emplace_back("other", typ_expr->clone());
    ret = typ_expr->clone();
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), I("other"))));
  } else if (op == "repr") {
    // def __repr__(self: T) -> str
    fargs.emplace_back("self", typ_expr->clone());
    ret = I("str");
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), I("self"))));
  } else if (op == "dict") {
    // def __dict__(self: T)
    fargs.emplace_back("self", typ_expr->clone());
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), I("self"))));
  } else if (op == "add") {
    // def __add__(self, obj)
    fargs.emplace_back("self", typ_expr->clone());
    fargs.emplace_back("obj", nullptr);
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), I("self"), I("obj"))));
  } else if (op == "mul") {
    // def __mul__(self, i: Static[int])
    fargs.emplace_back("self", typ_expr->clone());
    fargs.emplace_back("i", N<IndexExpr>(I("Static"), I("int")));
    stmts.emplace_back(N<ReturnStmt>(N<CallExpr>(NS(op), I("self"), I("i"))));
  } else {
    // seqassert(false, "invalid magic {}", op);
  }
#undef I
#undef NS
  auto t = std::make_shared<FunctionStmt>(fmt::format("__{}__", op), ret, fargs,
                                          N<SuiteStmt>(stmts), attr);
  t->set_source_info(ctx->cache->generate_source_info());
  return t;
}

}  // namespace Pud::AST