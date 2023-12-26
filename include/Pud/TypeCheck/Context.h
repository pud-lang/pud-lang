#ifndef PUD_TYPECHECK_CONTEXT_H
#define PUD_TYPECHECK_CONTEXT_H

#include <memory>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "Pud/AST/Cache.h"
#include "Pud/AST/Context.h"
#include "Pud/Common/Common.h"

namespace Pud::AST {

// 用于表示类型检查过程中的一个标识符项。
struct TypecheckItem {
  // 一个枚举类型，用来标识这个项是函数（Func）、类型（Type）还是变量（Var）。
  enum Kind { Func, Type, Var } kind;
  // 一个类型指针，存储与这个项相关的类型信息。
  Pud::Type::TypePtr type;

  TypecheckItem(Kind k, Pud::Type::TypePtr type)
      : kind(k), type(std::move(type)) {}

  auto is_type() const -> bool { return kind == Kind::Type; }
  auto is_var() const -> bool { return kind == Kind::Var; }
};

// 上下文类，用于在类型检查过程中追踪标识符。
struct TypeContext : public AST::Context<TypecheckItem> {
  // 指向共享缓存的指针，缓存在类型检查过程中共享的数据结构。
  AST::Cache* cache;

  // 定义一个函数实现的基本结构，包括函数名、类型、返回类型等。
  // 每个函数实现定义一个新的基本范围。
  // 用于正确实现封闭函数，并防止相互递归的封闭函数混乱。
  struct RealizationBase {
    // Function name
    std::string name;
    // Function type
    Pud::Type::TypePtr type;
    // The return type of currently realized function
    Pud::Type::TypePtr return_type = nullptr;
    // Typechecking iteration
    int iteration = 0;
    std::set<Pud::Type::TypePtr> pending_defaults;
  };
  std::vector<RealizationBase> realization_bases;

  // 当前类型检查级别，用于类型实例化和泛化。
  int typecheck_level;
  // 变化的节点数。
  int changed_nodes;

  // 当前解析语句的年龄。
  int age;
  // 嵌套实现的深度。
  int realization_depth;
  // 嵌套默认参数调用的集合。
  // (e.g. class A: def __init__(a: A = A())).
  std::set<std::string> default_call_depth;

  // 嵌套块的层级 (0 for toplevel)
  int block_level;
  // 如果发现早期返回，则为真。
  bool return_early;
  // 静态循环控制变量的堆栈。
  std::vector<std::string> static_loops;

 public:
  explicit TypeContext(AST::Cache* cache);

  using Context<TypecheckItem>::add;
  // 向上下文添加对象的方法。
  auto add(TypecheckItem::Kind kind, const std::string& name,
           const Pud::Type::TypePtr& type = nullptr)
      -> std::shared_ptr<TypecheckItem>;
  auto add_toplevel(const std::string& name,
                    const std::shared_ptr<TypecheckItem>& item)
      -> std::shared_ptr<TypecheckItem> {
    map[name].push_front(item);
    return item;
  }

  // 查找具有给定名称的对象。
  auto find(const std::string& name) const
      -> std::shared_ptr<TypecheckItem> override;
  auto find(const char* name) const -> std::shared_ptr<TypecheckItem> {
    return find(std::string(name));
  }

  // 假设对象存在并查找它。
  auto force_find(const std::string& name) const
      -> std::shared_ptr<TypecheckItem>;

  // 获取给定名称的类型。
  auto get_type(const std::string& name) const -> Pud::Type::TypePtr;

  // 打印当前上下文状态。
  void dump() override { dump(0); }

 public:
  // 获取当前实现深度。
  auto get_realization_depth() const -> size_t;
  // 获取当前实现的基础。
  auto get_realization_base() -> RealizationBase*;
  // 获取实现堆栈的名称 (e.g., `fn1:fn2:...`).
  auto get_realization_stack_name() const -> std::string;

 public:
  // 创建一个未绑定的类型。
  auto get_unbound(const SourceInfo& info, int level) const
      -> std::shared_ptr<Pud::Type::LinkType>;
  auto get_unbound(const SourceInfo& info) const
      -> std::shared_ptr<Pud::Type::LinkType>;
  auto get_unbound() const -> std::shared_ptr<Pud::Type::LinkType>;

  // 实例化类型。
  auto instantiate(const SourceInfo& info, const Pud::Type::TypePtr& type,
                   const Pud::Type::ClassTypePtr& generics = nullptr)
      -> Pud::Type::TypePtr;
  auto instantiate(Pud::Type::TypePtr type,
                   const Pud::Type::ClassTypePtr& generics = nullptr)
      -> Pud::Type::TypePtr {
    return instantiate(get_source_info(), type, generics);
  }

  // 实例化泛型类型。
  auto instantiate_generic(const SourceInfo& info,
                           const Pud::Type::TypePtr& root,
                           const std::vector<Pud::Type::TypePtr>& generics)
      -> Pud::Type::TypePtr;
  auto instantiate_generic(Pud::Type::TypePtr root,
                           const std::vector<Pud::Type::TypePtr>& generics)
      -> Pud::Type::TypePtr {
    return instantiate_generic(get_source_info(), root, generics);
  }

  // 实例化元组类型。
  auto instantiate_tuple(const SourceInfo& info,
                         const std::vector<Pud::Type::TypePtr>& generics)
      -> std::shared_ptr<Pud::Type::RecordType>;
  auto instantiate_tuple(const std::vector<Pud::Type::TypePtr>& generics)
      -> std::shared_ptr<Pud::Type::RecordType> {
    return instantiate_tuple(get_source_info(), generics);
  }
  auto instantiate_tuple(size_t n) -> std::shared_ptr<Pud::Type::RecordType>;
  auto generate_tuple(size_t n) -> std::string;

  // 查找类中的方法。
  auto find_method(Pud::Type::ClassType* type, const std::string& method,
                   bool hide_shadowed = true)
      -> std::vector<Pud::Type::FuncTypePtr>;
  // 查找类成员 Special cases: __elemsize__ and __atomic__.
  auto find_member(const Pud::Type::ClassTypePtr&, const std::string&) const
      -> Pud::Type::TypePtr;

  using ReorderDoneFn =
      std::function<int(int, int, const std::vector<std::vector<int>>&, bool)>;
  using ReorderErrorFn =
      std::function<int(Error, const SourceInfo&, std::string)>;

  // 重排命名参数。
  auto reorder_named_args(Pud::Type::FuncType* func,
                          const std::vector<AST::CallExpr::Arg>& args,
                          const ReorderDoneFn& on_done,
                          const ReorderErrorFn& on_error,
                          const std::vector<char>& known = std::vector<char>())
      -> int;

 private:
  void dump(int pad);
  auto debug_info() -> std::string;

 public:
  auto get_function_args(Pud::Type::TypePtr t)
      -> std::shared_ptr<std::pair<std::vector<Pud::Type::TypePtr>,
                                   std::vector<Pud::Type::TypePtr>>>;
  auto get_static_string(Pud::Type::TypePtr t) -> std::shared_ptr<std::string>;
  auto get_static_int(Pud::Type::TypePtr t) -> std::shared_ptr<int64_t>;
  auto extract_function(Pud::Type::TypePtr t) -> Pud::Type::FuncTypePtr;
};

}  // namespace Pud::Type

#endif  // PUD_TYPECHECK_CONTEXT_H