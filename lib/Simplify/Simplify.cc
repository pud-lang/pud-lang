#include "Pud/Simplify/Simplify.h"

#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/Common/Common.h"
#include "Pud/Parse/Parser.h"
#include "Pud/Simplify/Context.h"

namespace Pud::AST {

// 简化一个AST节点，并在需要时加载标准库。
auto SimplifyVisitor::apply(
    Cache* cache, const StmtPtr& node, const std::string& file,
    const std::unordered_map<std::string, std::string>& defines,
    const std::unordered_map<std::string, std::string>& early_defines,
    bool barebones) -> StmtPtr {
  // 创建一个用于存储预处理语句的 preamble。
  auto preamble = std::make_shared<std::vector<StmtPtr>>();
  // 确认 cache 的模块已经设置。
  seqassertn(cache->module, "cache's module is not set");

#define N std::make_shared
  // 设置标准库的路径和文件名。
  if (!in(cache->imports, STDLIB_IMPORT)) {
    // Load the internal.__init__
    auto stdlib = std::make_shared<SimplifyContext>(STDLIB_IMPORT, cache);
    auto stdlib_path = get_import_file(cache->argv0, STDLIB_INTERNAL_MODULE, "",
                                       true, cache->module0);
    const std::string init_file = "__init__.pud";
    if (!stdlib_path || !endswith(stdlib_path->path, init_file))
      Err(Error::COMPILER_NO_STDLIB);

    // 如果是快速测试模式（barebones），使用简化的标准库。
    // Use __init_test__ for faster testing (e.g., #%% name,barebones)
    if (barebones) {
      stdlib_path->path = stdlib_path->path.substr(
                              0, stdlib_path->path.size() - init_file.size()) +
                          "__init_test__.pud";
    }
    stdlib->set_filename(stdlib_path->path);
    cache->imports[STDLIB_IMPORT] = {stdlib_path->path, stdlib};
    stdlib->is_stdlib_loading = true;
    stdlib->module_name = {ImportFile::STDLIB, stdlib_path->path, "__init__"};
    // Load the standard library
    stdlib->set_filename(stdlib_path->path);
    // 解析并转换标准库中的代码。
    preamble->push_back(
        SimplifyVisitor(stdlib, preamble)
            .transform(Parse::parse_code(stdlib->cache, stdlib_path->path,
                                         "from internal.core import *")));
    // 处理早期定义（early_defines）。
    for (auto& d : early_defines) {
      // Load early compile-time defines (for standard library)
      preamble->push_back(
          SimplifyVisitor(stdlib, preamble)
              .transform(N<AssignStmt>(
                  N<IdExpr>(d.first), N<IntExpr>(d.second),
                  N<IndexExpr>(N<IdExpr>("Static"), N<IdExpr>("int")))));
    }
    preamble->push_back(
        SimplifyVisitor(stdlib, preamble)
            .transform(Parse::parse_file(stdlib->cache, stdlib_path->path)));
    stdlib->is_stdlib_loading = false;

    // 整个标准库的age为0，以允许反向引用
    cache->age++;
  }

  // Set up the context and the cache
  auto ctx = std::make_shared<SimplifyContext>(file, cache);
  cache->imports[file].filename = file;
  cache->imports[file].ctx = ctx;
  cache->imports[MAIN_IMPORT] = {file, ctx};
  ctx->set_filename(file);
  ctx->module_name = {ImportFile::PACKAGE, file, MODULE_MAIN};

  // 创建一个新的 SuiteStmt 用于存储转换后的语句。
  auto suite = N<SuiteStmt>();
  suite->stmts.push_back(
      N<ClassStmt>(".toplevel", std::vector<Param>{}, nullptr,
                   std::vector<ExprPtr>{N<IdExpr>(Attr::Internal)}));
  // 添加类定义和编译时定义（defines）。
  for (auto& d : defines) {
    // Load compile-time defines (e.g., pud run -DFOO=1 ...)
    suite->stmts.push_back(
        N<AssignStmt>(N<IdExpr>(d.first), N<IntExpr>(d.second),
                      N<IndexExpr>(N<IdExpr>("Static"), N<IdExpr>("int"))));
  }
  // 设置 __name__ 变量。
  suite->stmts.push_back(
      N<AssignStmt>(N<IdExpr>("__name__"), N<StringExpr>(MODULE_MAIN)));
  // 将传入的节点添加到 suite。
  suite->stmts.push_back(node);
  // 使用 SimplifyVisitor 转换 suite。
  auto n = SimplifyVisitor(ctx, preamble).transform(suite);

  // 返回结果:
  // 创建一个新的 SuiteStmt，其中包括 preamble 和转换后的 suite。
  // 返回这个新的 SuiteStmt。
  suite = N<SuiteStmt>();
  suite->stmts.push_back(N<SuiteStmt>(*preamble));
  // Add dominated assignment declarations
  if (in(ctx->scope.stmts, ctx->scope.blocks.back()))
    suite->stmts.insert(suite->stmts.end(),
                        ctx->scope.stmts[ctx->scope.blocks.back()].begin(),
                        ctx->scope.stmts[ctx->scope.blocks.back()].end());
  suite->stmts.push_back(n);
#undef N

  if (!ctx->cache->errors.empty())
    throw ParserException();

  return suite;
}

// 用于在标准库已加载的情况下简化AST节点。
auto SimplifyVisitor::apply(const std::shared_ptr<SimplifyContext>& ctx,
                            const StmtPtr& node, const std::string& file,
                            int at_age) -> StmtPtr {
  // 创建一个 stmts 向量来存储转换后的语句。
  std::vector<StmtPtr> stmts;
  int old_age = ctx->cache->age;
  // 如果指定了 at_age，设置 cache 的年龄为 at_age。
  if (at_age != -1)
    ctx->cache->age = at_age;
  auto preamble = std::make_shared<std::vector<StmtPtr>>();
  // 使用 SimplifyVisitor 转换传入的节点 node。
  stmts.emplace_back(SimplifyVisitor(ctx, preamble).transform(node));
  if (!ctx->cache->errors.empty())
    throw ParserException();

  // 如果指定了 at_age，恢复 cache 的年龄为原始值。
  if (at_age != -1)
    ctx->cache->age = old_age;
  // 创建一个新的 SuiteStmt。
  auto suite = std::make_shared<SuiteStmt>();
  // 将 preamble 和 stmts 中的所有语句添加到这个 SuiteStmt。
  for (auto& s : *preamble)
    suite->stmts.push_back(s);
  for (auto& s : stmts)
    suite->stmts.push_back(s);
  return suite;
}

SimplifyVisitor::SimplifyVisitor(
    std::shared_ptr<SimplifyContext> ctx,
    std::shared_ptr<std::vector<StmtPtr>> preamble,
    const std::shared_ptr<std::vector<StmtPtr>>& stmts)
    : ctx(std::move(ctx)), preamble(std::move(preamble)) {
  prepend_stmts = stmts ? stmts : std::make_shared<std::vector<StmtPtr>>();
}

auto SimplifyVisitor::transform(ExprPtr& expr) -> ExprPtr {
  return transform(expr, false);
}

// 用于转换表达式类型节点，并根据 allow_types 参数确定是否允许类型。
auto SimplifyVisitor::transform(ExprPtr& expr, bool allow_types) -> ExprPtr {
  if (!expr)
    return nullptr;
  SimplifyVisitor v(ctx, preamble);
  v.prepend_stmts = prepend_stmts;
  v.set_source_info(expr->get_source_info());
  ctx->push_source_info(expr->get_source_info());
  expr->accept(v);
  ctx->pop_source_info();
  if (v.result_expr) {
    v.result_expr->attributes |= expr->attributes;
    expr = v.result_expr;
  }
  if (!allow_types && expr && expr->is_type())
    Err(Error::UNEXPECTED_TYPE, expr, "type");
  return expr;
}

/// Transform a type expression node.
/// @param allowTypeOf Set if `type()` expressions are allowed. Usually
/// disallowed in
///                    class/function definitions.
/// @throw @c ParserException if a node is not a type (use @c transform
/// instead).
auto SimplifyVisitor::transform_type(ExprPtr& expr, bool allow_type_of)
    -> ExprPtr {
  auto old_type_of = ctx->allow_type_of;
  ctx->allow_type_of = allow_type_of;
  transform(expr, true);
  if (expr && expr->get_none())
    expr->mark_type();
  ctx->allow_type_of = old_type_of;
  if (expr && !expr->is_type())
    Err(Error::EXPECTED_TYPE, expr, "type");
  return expr;
}

/// Transform a statement node.
auto SimplifyVisitor::transform(StmtPtr& stmt) -> StmtPtr {
  if (!stmt)
    return nullptr;

  SimplifyVisitor v(ctx, preamble);
  v.set_source_info(stmt->get_source_info());
  ctx->push_source_info(stmt->get_source_info());
  try {
    stmt->accept(v);
  } catch (const ParserException& e) {
    ctx->cache->errors.push_back(e);
    // throw;
  }
  ctx->pop_source_info();
  if (v.result_stmt)
    stmt = v.result_stmt;
  stmt->age = ctx->cache->age;
  if (!v.prepend_stmts->empty()) {
    // Handle prepends
    if (stmt)
      v.prepend_stmts->push_back(stmt);
    stmt = N<SuiteStmt>(*v.prepend_stmts);
    stmt->age = ctx->cache->age;
  }
  return stmt;
}

/// Transform a statement in conditional scope.
/// Because variables and forward declarations within conditional scopes can be
/// added later after the domination analysis, ensure that all such declarations
/// are prepended.
auto SimplifyVisitor::transform_conditional_scope(StmtPtr& stmt) -> StmtPtr {
  if (stmt) {
    ctx->enter_conditional_block();
    transform(stmt);
    SuiteStmt* suite = stmt->get_suite();
    if (!suite) {
      stmt = N<SuiteStmt>(stmt);
      suite = stmt->get_suite();
    }
    ctx->leave_conditional_block(&suite->stmts);
    return stmt;
  }
  return stmt = nullptr;
}

void SimplifyVisitor::visit(StmtExpr* expr) {
  for (auto& s : expr->stmts)
    transform(s);
  transform(expr->expr);
}

void SimplifyVisitor::visit(StarExpr* expr) { transform(expr->what); }

void SimplifyVisitor::visit(KeywordStarExpr* expr) { transform(expr->what); }

/// Only allowed in @c MatchStmt
void SimplifyVisitor::visit(RangeExpr* expr) {
  Err(Error::UNEXPECTED_TYPE, expr, "range");
}

/// Handled during the type checking
void SimplifyVisitor::visit(SliceExpr* expr) {
  transform(expr->start);
  transform(expr->stop);
  transform(expr->step);
}

void SimplifyVisitor::visit(SuiteStmt* stmt) {
  for (auto& s : stmt->stmts)
    transform(s);
  result_stmt = N<SuiteStmt>(stmt->stmts);  // needed for flattening
}

void SimplifyVisitor::visit(ExprStmt* stmt) { transform(stmt->expr, true); }

void SimplifyVisitor::visit(CustomStmt* stmt) {
  if (stmt->suite) {
    auto fn = ctx->cache->custom_block_stmts.find(stmt->keyword);
    // seqassert(fn != ctx->cache->custom_block_stmts.end(), "unknown keyword {}",
    //           stmt->keyword);
    result_stmt = fn->second.second(this, stmt);
  } else {
    auto fn = ctx->cache->custom_expr_stmts.find(stmt->keyword);
    // seqassert(fn != ctx->cache->custom_expr_stmts.end(), "unknown keyword {}",
    //           stmt->keyword);
    result_stmt = fn->second(this, stmt);
  }
}

}  // namespace Pud::AST