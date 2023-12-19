#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/Cache.h"
#include "Pud/Common/Common.h"
#include "Pud/Parse/Parser.h"
#include "Pud/Simplify/Simplify.h"

using fmt::format;

namespace Pud::AST {

void SimplifyVisitor::visit(IntExpr* expr) {
  result_expr = transform_int(expr);
}

void SimplifyVisitor::visit(FloatExpr* expr) {
  result_expr = transform_float(expr);
}

// 处理字符串表达式，包括 f-字符串和自定义前缀字符串。
void SimplifyVisitor::visit(StringExpr* expr) {
  std::vector<ExprPtr> exprs;
  std::vector<std::string> concat;
  for (auto& p : expr->strings) {
    if (p.second == "f" || p.second == "F") {
      // 对于 f-字符串（使用 "f" 或 "F" 前缀），调用 transform_fstring
      // 进行转换。
      exprs.push_back(transform_fstring(p.first));
    } else if (!p.second.empty()) {
      // 对于具有自定义前缀的字符串，生成一个特殊的调用表达式，如
      // call `str.__prefix_[prefix]__(str, [static length of str])`
      exprs.push_back(transform(
          N<CallExpr>(N<DotExpr>("str", format("__prefix_{}__", p.second)),
                      N<StringExpr>(p.first), N<IntExpr>(p.first.size()))));
    } else {
      exprs.push_back(N<StringExpr>(p.first));
      concat.push_back(p.first);
    }
  }
  if (concat.size() == expr->strings.size()) {
    // 如果所有字符串部分都是普通字符串，则将它们简单地连接在一起。
    expr->strings = {{combine2(concat, ""), ""}};
  } else if (exprs.size() == 1) {
    result_expr = std::move(exprs[0]);
  } else {
    // 否则，如果有多个表达式，则使用 str.cat 方法将它们连接起来。
    // call `str.cat(str1, ...)`
    result_expr = transform(N<CallExpr>(N<DotExpr>("str", "cat"), exprs));
  }
}

/// 根据整数后缀解析不同的整数表示。
///   `123u`   -> `UInt[64](123)`
///   `123i56` -> `Int[56](123)`
///   `123pf`  -> `int.__suffix_pf__(123)`
ExprPtr SimplifyVisitor::transform_int(IntExpr* expr) {
  if (!expr->int_value) {
    // 检查整数是否超出范围。
    Err(Error::INT_RANGE, expr, expr->value);
  }

  // 处理固定宽度整数后缀（如 u64, i32）。
  std::unique_ptr<int16_t> suffix_value = nullptr;
  if (expr->suffix.size() > 1 &&
      (expr->suffix[0] == 'u' || expr->suffix[0] == 'i') &&
      isdigit(expr->suffix.substr(1))) {
    try {
      suffix_value =
          std::make_unique<int16_t>(std::stoi(expr->suffix.substr(1)));
    } catch (...) {
    }
    if (suffix_value && *suffix_value > MAX_INT_WIDTH) {
      suffix_value = nullptr;
    }
  }

  // 如果没有后缀，则返回普通整数表达式。
  if (expr->suffix.empty()) {
    return N<IntExpr>(*(expr->int_value));
  } else if (expr->suffix == "u") {
    // 对于带有 u 后缀的无符号整数，生成 UInt[64](value) 的调用表达式。
    // call `UInt[64](value)`
    return transform(
        N<CallExpr>(N<IndexExpr>(N<IdExpr>("UInt"), N<IntExpr>(64)),
                    N<IntExpr>(*(expr->int_value))));
  } else if (suffix_value) {
    // 对于带有 uNNN 或 iNNN 后缀的固定宽度整数，生成相应的 UInt[NNN](value)
    // 或 Int[NNN](value) 的调用表达式。
    // call `UInt[NNN](value)` or `Int[NNN](value)`
    return transform(N<CallExpr>(
        N<IndexExpr>(N<IdExpr>(expr->suffix[0] == 'u' ? "UInt" : "Int"),
                     N<IntExpr>(*suffix_value)),
        N<IntExpr>(*(expr->int_value))));
  } else {
    // 对于其他自定义后缀，生成 int.__suffix_[suffix]__(value) 的调用表达式。
    // call `int.__suffix_[suffix]__(value)`
    return transform(
        N<CallExpr>(N<DotExpr>("int", format("__suffix_{}__", expr->suffix)),
                    N<IntExpr>(*(expr->int_value))));
  }
}

/// 根据浮点数后缀解析不同的浮点数表示。
///   `123.4pf` -> `float.__suffix_pf__(123.4)`
auto SimplifyVisitor::transform_float(FloatExpr* expr) -> ExprPtr {
  if (!expr->float_value) {
    // 检查浮点数是否超出范围。
    Err(Error::FLOAT_RANGE, expr, expr->value);
  }

  if (expr->suffix.empty()) {
    // 如果没有后缀，则返回普通浮点数表达式。
    return N<FloatExpr>(*(expr->float_value));
  } else {
    // 对于带有自定义后缀的浮点数，生成 float.__suffix_[suffix]__(value) 的调用
    // 表达式。
    // call `float.__suffix_[suffix]__(value)`
    return transform(
        N<CallExpr>(N<DotExpr>("float", format("__suffix_{}__", expr->suffix)),
                    N<FloatExpr>(*(expr->float_value))));
  }
}

/// 解析 Python 风格的 f-字符串。
///   `f"foo {x+1} bar"` -> `str.cat("foo ", str(x+1), " bar")`
/// Supports "{x=}" specifier (that prints the raw expression as well):
///   `f"{x+1=}"` -> `str.cat("x+1=", str(x+1))`
auto SimplifyVisitor::transform_fstring(const std::string& value) -> ExprPtr {
  // 用于存储转换后的表达式序列。
  std::vector<ExprPtr> items;
  // 跟踪花括号 {} 的平衡。
  int brace_count = 0;
  // 记录当前正在处理的表达式或文本片段的起始位置。
  int brace_start = 0;
  // 循环遍历整个字符串 value，处理每个字符。
  for (int i = 0; i < value.size(); i++) {
    // 当遇到 { 时，开始一个新的表达式。
    if (value[i] == '{') {
      if (brace_start < i)
        items.push_back(
            N<StringExpr>(value.substr(brace_start, i - brace_start)));
      if (!brace_count)
        brace_start = i + 1;
      brace_count++;
    } else if (value[i] == '}') {  // 当遇到 } 时，结束当前的表达式。
      brace_count--;
      if (!brace_count) {
        std::string code = value.substr(brace_start, i - brace_start);
        auto offset = get_source_info();
        offset.column += i;
        // 获取表达式的代码字符串。
        if (!code.empty() && code.back() == '=') {
          // Special case: f"{x=}"
          code = code.substr(0, code.size() - 1);
          items.push_back(N<StringExpr>(fmt::format("{}=", code)));
        }
        // 解析表达式，可能包括特殊格式处理（如 __format__ 方法）。
        auto [expr, format] = Parse::parse_expr(ctx->cache, code, offset);
        if (!format.empty()) {
          items.push_back(N<CallExpr>(N<DotExpr>(expr, "__format__"),
                                      N<StringExpr>(format)));
        } else {
          // Every expression is wrapped within `str`
          items.push_back(N<CallExpr>(N<IdExpr>("str"), expr));
        }
      }
      brace_start = i + 1;
    }
  }
  // 确保所有的 { 都有匹配的 }，反之亦然。
  if (brace_count > 0)
    Err(Error::STR_FSTRING_BALANCE_EXTRA, get_source_info());
  if (brace_count < 0)
    Err(Error::STR_FSTRING_BALANCE_MISSING, get_source_info());
  // 如果字符串末尾还有未处理的文本，将其作为字符串表达式添加到 items 中。
  if (brace_start != value.size())
    items.push_back(
        N<StringExpr>(value.substr(brace_start, value.size() - brace_start)));
  // 使用 str.cat 方法将所有的 items 连接起来，生成最终的字符串表达式。
  // transform 方法被调用以进一步处理这个连接操作。
  return transform(N<CallExpr>(N<DotExpr>("str", "cat"), items));
}

}  // namespace Pud::AST
