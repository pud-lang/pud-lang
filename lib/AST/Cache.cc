#include "Pud/AST/Cache.h"

#include <chrono>
#include <string>
#include <vector>

#include "Pud/Common/Common.h"
#include "Pud/IR/PyExtension.h"
#include "Pud/IR/Util/IrTools.h"
#include "Pud/Parse/Parser.h"
#include "Pud/Simplify/Simplify.h"
#include "Pud/Translate/Translate.h"
#include "Pud/TypeCheck/Context.h"
#include "Pud/TypeCheck/TypeCheck.h"

namespace Pud::AST {

Cache::Cache(std::string argv0)
    : generated_source_info_count(0),
      unbound_count(256),
      var_count(0),
      age(0),
      argv0(std::move(argv0)),
      type_ctx(nullptr),
      codegen_ctx(nullptr),
      is_jit(false),
      jit_cell(0),
      python_ext(false),
      py_module(nullptr) {}

auto Cache::get_temporary_var(const std::string& prefix, char sigil)
    -> std::string {
  return fmt::format("{}{}_{}", sigil ? fmt::format("{}_", sigil) : "", prefix,
                     ++var_count);
}

auto Cache::rev(const std::string& s) -> std::string {
  auto i = reverse_identifier_lookup.find(s);
  if (i != reverse_identifier_lookup.end())
    return i->second;
  seqassertn(false, "'{}' has no non-canonical name", s);
  return "";
}

void Cache::add_global(const std::string& name, IR::Var* var) {
  if (!in(globals, name)) {
    // LOG("[global] {}", name);
    globals[name] = var;
  }
}

auto Cache::generate_source_info() -> SourceInfo {
  return {FILE_GENERATED, generated_source_info_count,
          generated_source_info_count++, 0};
}

auto Cache::get_content(const SourceInfo& info) -> std::string {
  auto i = imports.find(info.file);
  if (i == imports.end())
    return "";
  int line = info.line - 1;
  if (line < 0 || line >= i->second.content.size())
    return "";
  auto s = i->second.content[line];
  int col = info.column - 1;
  if (col < 0 || col >= s.size())
    return "";
  int len = info.length;
  return s.substr(col, len);
}

auto Cache::find_class(const std::string& name) const -> Type::ClassTypePtr {
  auto f = type_ctx->find(name);
  if (f && f->kind == TypecheckItem::Type)
    return f->type->get_class();
  return nullptr;
}

auto Cache::find_function(const std::string& name) const -> Type::FuncTypePtr {
  auto f = type_ctx->find(name);
  if (f && f->type && f->kind == TypecheckItem::Func)
    return f->type->get_func();
  f = type_ctx->find(name + ":0");
  if (f && f->type && f->kind == TypecheckItem::Func)
    return f->type->get_func();
  return nullptr;
}

auto Cache::find_method(Type::ClassType* typ, const std::string& member,
                        const std::vector<Type::TypePtr>& args)
    -> Type::FuncTypePtr {
  auto e = std::make_shared<IdExpr>(typ->name);
  e->type = typ->get_class();
  seqassertn(e->type, "not a class");
  int old_age = type_ctx->age;
  type_ctx->age = 99999;
  auto f = TypecheckVisitor(type_ctx).find_best_method(e->type->get_class(),
                                                       member, args);
  type_ctx->age = old_age;
  return f;
}

auto Cache::realize_type(Type::ClassTypePtr type,
                         const std::vector<Type::TypePtr>& generics)
    -> IR::Types::Type* {
  auto e = std::make_shared<IdExpr>(type->name);
  e->type = type;
  type = type_ctx->instantiate_generic(type, generics)->get_class();
  auto tv = TypecheckVisitor(type_ctx);
  if (auto rtv = tv.realize(type)) {
    return classes[rtv->get_class()->name]
        .realizations[rtv->get_class()->realized_type_name()]
        ->ir;
  }
  return nullptr;
}

auto Cache::realize_function(Type::FuncTypePtr type,
                             const std::vector<Type::TypePtr>& args,
                             const std::vector<Type::TypePtr>& generics,
                             const Type::ClassTypePtr& parent_class)
    -> IR::Func* {
  auto e = std::make_shared<IdExpr>(type->ast->name);
  e->type = type;
  type = type_ctx->instantiate(type, parent_class)->get_func();
  if (args.size() != type->get_arg_types().size() + 1)
    return nullptr;
  Type::Type::Unification undo;
  if (type->get_ret_type()->unify(args[0].get(), &undo) < 0) {
    undo.undo();
    return nullptr;
  }
  for (int gi = 1; gi < args.size(); gi++) {
    undo = Type::Type::Unification();
    if (type->get_arg_types()[gi - 1]->unify(args[gi].get(), &undo) < 0) {
      undo.undo();
      return nullptr;
    }
  }
  if (!generics.empty()) {
    if (generics.size() != type->func_generics.size())
      return nullptr;
    for (int gi = 0; gi < generics.size(); gi++) {
      undo = Type::Type::Unification();
      if (type->func_generics[gi].type->unify(generics[gi].get(), &undo) < 0) {
        undo.undo();
        return nullptr;
      }
    }
  }
  int old_age = type_ctx->age;
  type_ctx->age = 99999;
  auto tv = TypecheckVisitor(type_ctx);
  IR::Func* f = nullptr;
  if (auto rtv = tv.realize(type)) {
    auto pr = pending_realizations;  // copy it as it might be modified
    for (auto& fn : pr)
      TranslateVisitor(codegen_ctx).transform(functions[fn.first].ast->clone());
    f = functions[rtv->get_func()->ast->name]
            .realizations[rtv->realized_name()]
            ->ir;
  }
  type_ctx->age = old_age;
  return f;
}

auto Cache::make_tuple(const std::vector<Type::TypePtr>& types)
    -> IR::Types::Type* {
  auto tv = TypecheckVisitor(type_ctx);
  auto t = type_ctx->instantiate_tuple(types);
  return realize_type(t, types);
}

auto Cache::make_function(const std::vector<Type::TypePtr>& types)
    -> IR::Types::Type* {
  auto tv = TypecheckVisitor(type_ctx);
  seqassertn(!types.empty(), "types must have at least one argument");

  const auto& ret = types[0];
  auto arg_type = type_ctx->instantiate_tuple(
      std::vector<Type::TypePtr>(types.begin() + 1, types.end()));
  auto t = type_ctx->find("Function");
  seqassertn(t && t->type, "cannot find 'Function'");
  return realize_type(t->type->get_class(), {arg_type, ret});
}

auto Cache::make_union(const std::vector<Type::TypePtr>& types)
    -> IR::Types::Type* {
  auto tv = TypecheckVisitor(type_ctx);

  auto arg_type = type_ctx->instantiate_tuple(types);
  auto t = type_ctx->find("Union");
  seqassertn(t && t->type, "cannot find 'Union'");
  return realize_type(t->type->get_class(), {arg_type});
}

void Cache::parse_code(const std::string& code) {
  auto node = Parse::parse_code(this, "<internal>", code, /*startLine=*/0);
  auto sctx = imports[MAIN_IMPORT].ctx;
  node = AST::SimplifyVisitor::apply(sctx, node, "<internal>", 99999);
  node = AST::TypecheckVisitor::apply(this, node);
  AST::TranslateVisitor(codegen_ctx).transform(node);
}

auto Cache::merge_c3(std::vector<std::vector<ExprPtr>>& seqs)
    -> std::vector<ExprPtr> {
  // Reference: https://www.python.org/download/releases/2.3/mro/
  std::vector<ExprPtr> result;
  for (size_t i = 0;; i++) {
    bool found = false;
    ExprPtr cand = nullptr;
    for (auto& seq : seqs) {
      if (seq.empty())
        continue;
      found = true;
      bool nothead = false;
      for (auto& s : seqs)
        if (!s.empty()) {
          bool in = false;
          for (size_t j = 1; j < s.size(); j++) {
            if ((in |= (seq[0]->get_type_name() == s[j]->get_type_name())))
              break;
          }
          if (in) {
            nothead = true;
            break;
          }
        }
      if (!nothead) {
        cand = seq[0];
        break;
      }
    }
    if (!found)
      return result;
    if (!cand)
      return {};
    result.push_back(clone(cand));
    for (auto& s : seqs)
      if (!s.empty() && cand->get_type_name() == s[0]->get_type_name()) {
        s.erase(s.begin());
      }
  }
  return result;
}

/**
 * Generate Python bindings for Cython-like access.
 *
 * TODO: this function is total mess. Needs refactoring.
 */
void Cache::populate_python_module() {
  if (!python_ext)
    return;

  LOG_USER("[py] ====== module generation =======");

#define N std::make_shared

  if (!py_module)
    py_module = std::make_shared<IR::PyModule>();
  using namespace AST;

  int old_age = type_ctx->age;
  type_ctx->age = 99999;

  auto realize_ir =
      [&](const Type::FuncTypePtr& fn,
          const std::vector<Type::TypePtr>& generics = {}) -> IR::Func* {
    auto fn_type = type_ctx->instantiate(fn);
    Type::Type::Unification u;
    for (size_t i = 0; i < generics.size(); i++)
      fn_type->get_func()->func_generics[i].type->unify(generics[i].get(), &u);
    fn_type = TypecheckVisitor(type_ctx).realize(fn_type);
    if (!fn_type)
      return nullptr;

    auto pr = pending_realizations;  // copy it as it might be modified
    for (auto& fn : pr)
      TranslateVisitor(codegen_ctx).transform(functions[fn.first].ast->clone());
    return functions[fn->ast->name].realizations[fn_type->realized_name()]->ir;
  };

  const std::string py_wrap = "std.internal.python._PyWrap";
  auto clss = classes;  // needs copy as below fns can mutate this
  for (const auto& [cn, c] : clss) {
    if (c.module.empty()) {
      if (!in(c.methods, "__to_py__") || !in(c.methods, "__from_py__"))
        continue;

      LOG_USER("[py] Cythonizing {}", cn);
      IR::PyType py{rev(cn), c.ast->get_doc_str()};

      auto tc = type_ctx->force_find(cn)->type;
      if (!tc->can_realize())
        compilation_error(
            fmt::format("cannot realize '{}' for Python export", rev(cn)));
      tc = TypecheckVisitor(type_ctx).realize(tc);
      seqassertn(tc, "cannot realize '{}'", cn);

      // 1. Replace to_py / from_py with _PyWrap.wrap_to_py/from_py
      if (auto ofnn = in(c.methods, "__to_py__")) {
        auto fnn = overloads[*ofnn].begin()->name;  // default first overload!
        auto& fna = functions[fnn].ast;
        fna->get_function()->suite =
            N<ReturnStmt>(N<CallExpr>(N<IdExpr>(py_wrap + ".wrap_to_py:0"),
                                      N<IdExpr>(fna->args[0].name)));
      }
      if (auto ofnn = in(c.methods, "__from_py__")) {
        auto fnn = overloads[*ofnn].begin()->name;  // default first overload!
        auto& fna = functions[fnn].ast;
        fna->get_function()->suite = N<ReturnStmt>(
            N<CallExpr>(N<IdExpr>(py_wrap + ".wrap_from_py:0"),
                        N<IdExpr>(fna->args[0].name), N<IdExpr>(cn)));
      }
      for (auto& n : std::vector<std::string>{"__from_py__", "__to_py__"}) {
        auto fnn = overloads[*in(c.methods, n)].begin()->name;
        IR::Func* old_ir = nullptr;
        if (!functions[fnn].realizations.empty())
          old_ir = functions[fnn].realizations.begin()->second->ir;
        functions[fnn].realizations.clear();
        auto tf = TypecheckVisitor(type_ctx).realize(functions[fnn].type);
        seqassertn(tf, "cannot re-realize '{}'", fnn);
        if (old_ir) {
          std::vector<IR::Value*> args;
          for (auto it = old_ir->arg_begin(); it != old_ir->arg_end(); ++it) {
            args.push_back(module->Nr<IR::VarValue>(*it));
          }
          IR::cast<IR::BodiedFunc>(old_ir)->set_body(
              IR::Util::series(IR::Util::call(
                  functions[fnn].realizations.begin()->second->ir, args)));
        }
      }
      for (auto& [rn, r] : functions[py_wrap + ".py_type:0"].realizations) {
        if (r->type->func_generics[0].type->unify(tc.get(), nullptr) >= 0) {
          py.typePtrHook = r->ir;
          break;
        }
      }

      // 2. Handle methods
      auto methods = c.methods;
      for (const auto& [n, ofnn] : methods) {
        auto canonical_name = overloads[ofnn].back().name;
        if (overloads[ofnn].size() == 1 &&
            functions[canonical_name].ast->has_attr("autogenerated"))
          continue;
        auto fna = functions[canonical_name].ast;
        bool is_method = fna->has_attr(Attr::Method);
        bool is_property = fna->has_attr(Attr::Property);

        std::string call = py_wrap + ".wrap_multiple";
        bool is_magic = false;
        if (startswith(n, "__") && endswith(n, "__")) {
          auto m = n.substr(2, n.size() - 4);
          if (m == "new" && c.ast->has_attr(Attr::Tuple))
            m = "init";
          if (auto i = in(classes[py_wrap].methods, "wrap_magic_" + m)) {
            call = *i;
            is_magic = true;
          }
        }
        if (is_property)
          call = py_wrap + ".wrap_get";

        auto fn_name = call + ":0";
        seqassertn(in(functions, fn_name), "bad name");
        auto generics = std::vector<Type::TypePtr>{tc};
        if (is_property) {
          generics.push_back(
              std::make_shared<Type::StaticType>(this, rev(canonical_name)));
        } else if (!is_magic) {
          generics.push_back(std::make_shared<Type::StaticType>(this, n));
          generics.push_back(
              std::make_shared<Type::StaticType>(this, (int)is_method));
        }
        auto f = realize_ir(functions[fn_name].type, generics);
        if (!f)
          continue;

        LOG_USER("[py] {} -> {} ({}; {})", n, call, is_method, is_property);
        if (is_property) {
          py.getset.push_back({rev(canonical_name), "", f, nullptr});
        } else if (n == "__repr__") {
          py.repr = f;
        } else if (n == "__add__") {
          py.add = f;
        } else if (n == "__iadd__") {
          py.iadd = f;
        } else if (n == "__sub__") {
          py.sub = f;
        } else if (n == "__isub__") {
          py.isub = f;
        } else if (n == "__mul__") {
          py.mul = f;
        } else if (n == "__imul__") {
          py.imul = f;
        } else if (n == "__mod__") {
          py.mod = f;
        } else if (n == "__imod__") {
          py.imod = f;
        } else if (n == "__divmod__") {
          py.divmod = f;
        } else if (n == "__pow__") {
          py.pow = f;
        } else if (n == "__ipow__") {
          py.ipow = f;
        } else if (n == "__neg__") {
          py.neg = f;
        } else if (n == "__pos__") {
          py.pos = f;
        } else if (n == "__abs__") {
          py.abs = f;
        } else if (n == "__bool__") {
          py.bool_ = f;
        } else if (n == "__invert__") {
          py.invert = f;
        } else if (n == "__lshift__") {
          py.lshift = f;
        } else if (n == "__ilshift__") {
          py.ilshift = f;
        } else if (n == "__rshift__") {
          py.rshift = f;
        } else if (n == "__irshift__") {
          py.irshift = f;
        } else if (n == "__and__") {
          py.and_ = f;
        } else if (n == "__iand__") {
          py.iand = f;
        } else if (n == "__xor__") {
          py.xor_ = f;
        } else if (n == "__ixor__") {
          py.ixor = f;
        } else if (n == "__or__") {
          py.or_ = f;
        } else if (n == "__ior__") {
          py.ior = f;
        } else if (n == "__int__") {
          py.int_ = f;
        } else if (n == "__float__") {
          py.float_ = f;
        } else if (n == "__floordiv__") {
          py.floordiv = f;
        } else if (n == "__ifloordiv__") {
          py.ifloordiv = f;
        } else if (n == "__truediv__") {
          py.truediv = f;
        } else if (n == "__itruediv__") {
          py.itruediv = f;
        } else if (n == "__index__") {
          py.index = f;
        } else if (n == "__matmul__") {
          py.matmul = f;
        } else if (n == "__imatmul__") {
          py.imatmul = f;
        } else if (n == "__len__") {
          py.len = f;
        } else if (n == "__getitem__") {
          py.getitem = f;
        } else if (n == "__setitem__") {
          py.setitem = f;
        } else if (n == "__contains__") {
          py.contains = f;
        } else if (n == "__hash__") {
          py.hash = f;
        } else if (n == "__call__") {
          py.call = f;
        } else if (n == "__str__") {
          py.str = f;
        } else if (n == "__iter__") {
          py.iter = f;
        } else if (n == "__del__") {
          py.del = f;
        } else if (n == "__init__" ||
                   (c.ast->has_attr(Attr::Tuple) && n == "__new__")) {
          py.init = f;
        } else {
          py.methods.push_back(IR::PyFunction{
              n, fna->get_doc_str(), f,
              fna->has_attr(Attr::Method) ? IR::PyFunction::Type::METHOD
                                          : IR::PyFunction::Type::CLASS,
              // always use FASTCALL for now; works even for 0- or 1- arg
              // methods
              2});
          py.methods.back().keywords = true;
        }
      }

      for (auto& m : py.methods) {
        if (in(std::set<std::string>{"__lt__", "__le__", "__eq__", "__ne__",
                                     "__gt__", "__ge__"},
               m.name)) {
          py.cmp = realize_ir(
              type_ctx->force_find(py_wrap + ".wrap_cmp:0")->type->get_func(),
              {tc});
          break;
        }
      }

      if (c.realizations.size() != 1)
        compilation_error(
            fmt::format("cannot pythonize generic class '{}'", cn));
      auto& r = c.realizations.begin()->second;
      py.type = realize_type(r->type);
      for (auto& [mn, mt] : r->fields) {
        /// TODO: handle PyMember for tuples
        // Generate getters & setters
        auto generics = std::vector<Type::TypePtr>{
            tc, std::make_shared<Type::StaticType>(this, mn)};
        auto gf = realize_ir(functions[py_wrap + ".wrap_get:0"].type, generics);
        IR::Func* sf = nullptr;
        if (!c.ast->has_attr(Attr::Tuple))
          sf = realize_ir(functions[py_wrap + ".wrap_set:0"].type, generics);
        py.getset.push_back({mn, "", gf, sf});
        LOG_USER("[py] {}: {} . {}", "member", cn, mn);
      }
      py_module->types.push_back(py);
    }
  }

  // Handle __iternext__ wrappers
  auto cin = "_PyWrap.IterWrap";
  for (auto& [cn, cr] : classes[cin].realizations) {
    LOG_USER("[py] iterfn: {}", cn);
    IR::PyType py{cn, ""};
    auto tc = cr->type;
    for (auto& [rn, r] : functions[py_wrap + ".py_type:0"].realizations) {
      if (r->type->func_generics[0].type->unify(tc.get(), nullptr) >= 0) {
        py.typePtrHook = r->ir;
        break;
      }
    }

    auto& methods = classes[cin].methods;
    for (auto& n : std::vector<std::string>{"_iter", "_iternext"}) {
      auto fnn = overloads[methods[n]].begin()->name;
      auto& fna = functions[fnn];
      auto ft = type_ctx->instantiate(fna.type, tc->get_class());
      auto rtv = TypecheckVisitor(type_ctx).realize(ft);
      auto f = functions[rtv->get_func()->ast->name]
                   .realizations[rtv->realized_name()]
                   ->ir;
      if (n == "_iter")
        py.iter = f;
      else
        py.iternext = f;
    }
    py.type = cr->ir;
    py_module->types.push_back(py);
  }
#undef N

  auto fns = functions;  // needs copy as below fns can mutate this
  for (const auto& [fn, f] : fns) {
    if (f.is_toplevel) {
      std::string call = py_wrap + ".wrap_multiple";
      auto fn_name = call + ":0";
      seqassertn(in(functions, fn_name), "bad name");
      auto generics = std::vector<Type::TypePtr>{
          type_ctx->force_find(".toplevel")->type,
          std::make_shared<Type::StaticType>(this, rev(f.ast->name)),
          std::make_shared<Type::StaticType>(this, 0)};
      if (auto ir = realize_ir(functions[fn_name].type, generics)) {
        LOG_USER("[py] {}: {}", "toplevel", fn);
        py_module->functions.push_back(IR::PyFunction{
            rev(fn), f.ast->get_doc_str(), ir, IR::PyFunction::Type::TOPLEVEL,
            int(f.ast->args.size())});
        py_module->functions.back().keywords = true;
      }
    }
  }

  // Handle pending realizations!
  auto pr = pending_realizations;  // copy it as it might be modified
  for (auto& fn : pr)
    TranslateVisitor(codegen_ctx).transform(functions[fn.first].ast->clone());
  type_ctx->age = old_age;
}

}  // namespace Pud::AST