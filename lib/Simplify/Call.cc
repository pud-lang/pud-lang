#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/Common/Common.h"
#include "Pud/Simplify/Simplify.h"

using fmt::format;

namespace Pud::AST {

/// 转换打印语句。
/// @example
///   `print a, b` -> `print(a, b)`
///   `print a, b,` -> `print(a, b, end=' ')`
void SimplifyVisitor::visit(PrintStmt* stmt) {
  std::vector<CallExpr::Arg> args;
  // 对于打印语句中的每个项（例如 print a, b 中的 a 和 b），
  // 将它们转换（可能包括表达式的简化）并添加到参数列表 args 中。
  for (auto& i : stmt->items)
    args.emplace_back("", transform(i));
  // 如果打印语句以逗号结尾（例如 print a, b,），则在参数列表中添加一个
  // 表示结尾的参数（在 Python 中相当于 print(a, b, end=' ')）。
  if (stmt->is_inline)
    args.emplace_back("end", N<StringExpr>(" "));
  // 将整个打印语句转换为一个函数调用表达式，调用的函数是 print，参数是
  // 前面构建的 args 列表。
  result_stmt = N<ExprStmt>(N<CallExpr>(transform(N<IdExpr>("print")), args));
}

/// 转换函数调用表达式。
void SimplifyVisitor::visit(CallExpr* expr) {
  // 首先转换函数调用的主体部分（即函数名）。
  transform(expr->expr, true);
  // 尝试将当前的函数调用转换为特殊的调用形式（例如处理特定的库函数）。
  // 如果转换成功，则提前返回。
  if ((result_expr = transform_special_call(expr->expr, expr->args)))
    return;

  // 遍历函数调用的所有参数，并对它们进行转换。
  // 如果遇到省略号表达式（...），则进行特别处理。
  for (auto& i : expr->args) {
    if (auto el = i.value->get_ellipsis()) {
      if (&(i) == &(expr->args.back()) && i.name.empty())
        el->mode = EllipsisExpr::PARTIAL;
    }
    transform(i.value, true);
  }
}

/// 处理特殊的函数调用。
///   `tuple(i for i in tup)`      (tuple generators)
///   `std.collections.namedtuple` (sugar for @tuple class)
///   `std.functools.partial`      (sugar for partial calls)
auto SimplifyVisitor::transform_special_call(
    const ExprPtr& callee, const std::vector<CallExpr::Arg>& args) -> ExprPtr {
  // 特别处理 tuple 生成器表达式（例如 tuple(i for i in tup)）和
  // type 函数调用（如果不允许 type 函数，则抛出错误）。
  if (callee->is_id("tuple") && args.size() == 1 &&
      CAST(args.front().value, GeneratorExpr)) {
    // tuple(i for i in j)
    return transform_tuple_generator(args);
  } else if (callee->is_id("type") && !ctx->allow_type_of) {
    // type(i)
    Err(Error::CALL_NO_TYPE, get_source_info());
  } else if (callee->is_id("std.collections.namedtuple")) {
    // namedtuple('Foo', ['x', 'y'])
    return transform_named_tuple(args);
  } else if (callee->is_id("std.functools.partial")) {
    // partial(foo, a=5)
    return transform_functools_partial(args);
  }
  return nullptr;
}

/// 用于转换 tuple 生成器表达式。`tuple(i for i in tup)` -> GeneratorExpr
auto SimplifyVisitor::transform_tuple_generator(
    const std::vector<CallExpr::Arg>& args) -> ExprPtr {
  GeneratorExpr* g = nullptr;
  // 这部分代码首先检查传入的参数是否符合特定的模式：只有一个参数，
  // 且该参数是一个生成器表达式。
  if (args.size() != 1 || !(g = CAST(args[0].value, GeneratorExpr)) ||
      g->kind != GeneratorExpr::Generator || g->loops.size() != 1 ||
      !g->loops[0].conds.empty())
    Err(Error::CALL_TUPLE_COMPREHENSION, args[0].value);
  // 克隆生成器表达式中的变量和表达式，以便进一步转换。
  auto var = clone(g->loops[0].vars);
  auto ex = clone(g->expr);

  // 进入一个条件块，并在当前作用域中记录循环信息。
  ctx->enter_conditional_block();
  ctx->get_base()->loops.push_back({"", ctx->scope.blocks, {}});
  // 如果循环变量是一个标识符，将其添加到当前作用域，并对变量和表达式进行转换。
  if (auto i = var->get_id()) {
    ctx->add_var(i->value, ctx->generate_canonical_name(i->value),
                 var->get_source_info());
    var = transform(var);
    ex = transform(ex);
  } else {
    // 如果不是标识符，创建一个临时变量并进行相应的转换。
    std::string var_name = ctx->cache->get_temporary_var("for");
    ctx->add_var(var_name, var_name, var->get_source_info());
    var = N<IdExpr>(var_name);
    auto head = transform(N<AssignStmt>(clone(g->loops[0].vars), clone(var)));
    ex = N<StmtExpr>(head, transform(ex));
  }
  // 离开条件块，并处理循环变量的作用域。
  ctx->leave_conditional_block();
  for (auto& var : ctx->get_base()->get_loop()->seen_vars)
    ctx->find_dominating_binding(var);
  ctx->get_base()->loops.pop_back();
  // 返回一个新的生成器表达式，包含变换后的组件。
  return N<GeneratorExpr>(
      GeneratorExpr::Generator, ex,
      std::vector<GeneratorBody>{{var, transform(g->loops[0].gen), {}}});
}

/// 用于转换 namedtuple 调用。
///   `namedtuple("NT", ["a", ("b", int)])` -> ```@tuple
///                                               class NT[T1]:
///                                                 a: T1
///                                                 b: int```
auto SimplifyVisitor::transform_named_tuple(
    const std::vector<CallExpr::Arg>& args) -> ExprPtr {
  // 检查 namedtuple 调用的参数是否有效。如果无效，抛出错误。
  if (args.size() != 2 || !args[0].value->get_string() ||
      !args[1].value->get_list())
    Err(Error::CALL_NAMEDTUPLE, get_source_info());

  // 构建类的泛型参数和字段。
  // 如果字段有指定类型，则使用该类型；否则，使用泛型参数。
  std::vector<Param> generics, params;
  int ti = 1;
  for (auto& i : args[1].value->get_list()->items) {
    if (auto s = i->get_string()) {
      generics.emplace_back(
          Param{format("T{}", ti), N<IdExpr>("type"), nullptr, true});
      params.emplace_back(
          Param{s->get_value(), N<IdExpr>(format("T{}", ti++)), nullptr});
    } else if (i->get_tuple() && i->get_tuple()->items.size() == 2 &&
               i->get_tuple()->items[0]->get_string()) {
      params.emplace_back(
          Param{i->get_tuple()->items[0]->get_string()->get_value(),
                transform_type(i->get_tuple()->items[1]), nullptr});
    } else {
      Err(Error::CALL_NAMEDTUPLE, i);
    }
  }
  for (auto& g : generics)
    params.push_back(g);
  auto name = args[0].value->get_string()->get_value();
  // 构建一个类语句，并将其添加到语句列表中。
  // 这个类继承自 tuple 并包含相应的字段。最后返回这个新类的类型。
  prepend_stmts->push_back(transform(N<ClassStmt>(
      name, params, nullptr, std::vector<ExprPtr>{N<IdExpr>("tuple")})));
  return transform_type(N<IdExpr>(name));
}

/// 用于转换 functools.partial 调用。
///   `partial(foo, 1, a=2)` -> `foo(1, a=2, ...)`
auto SimplifyVisitor::transform_functools_partial(
    std::vector<CallExpr::Arg> args) -> ExprPtr {
  // 检查参数列表是否为空。如果为空，抛出错误。
  if (args.empty())
    Err(Error::CALL_PARTIAL, get_source_info());
  // 克隆并移除列表中的第一个元素（通常是函数名）。
  auto name = clone(args[0].value);
  args.erase(args.begin());
  // 在参数列表的末尾添加一个特殊的省略号表达式，表示这是一个部分应用的函数。
  args.emplace_back("", N<EllipsisExpr>(EllipsisExpr::PARTIAL));
  return transform(N<CallExpr>(name, args));
}

}  // namespace Pud::AST