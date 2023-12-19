#ifndef PUD_SIMPLIFY_CONTEXT_H
#define PUD_SIMPLIFY_CONTEXT_H

#include <deque>
#include <memory>
#include <set>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Pud/AST/Cache.h"
#include "Pud/AST/Context.h"
#include "Pud/Common/Common.h"

namespace Pud::AST {

// 表示在代码简化过程中各种类型的标识符，如函数、类（类型）或变量。
// 这些标识符在编译器内部用于追踪和管理代码中的各种实体。
struct SimplifyItem : public SourceObject {
  // 标识符类型，可以是函数（Func）、类型（Type）或变量（Var）。
  enum Kind : uint8_t { Func, Type, Var } kind;
  // 基本名称，如 foo.bar.baz。
  // 这通常是标识符在代码中的名称，用于在代码中引用该标识符。
  std::string base_name;
  // 唯一标识符（规范名称）。
  // 这是在内部用于唯一识别标识符的名称，它可能与代码中使用的名称不同。
  std::string canonical_name;
  // 完整模块名称。表示该标识符所属的模块或命名空间。
  std::string module_name;
  // 完整作用域信息。这是一个整数向量，表示标识符所在的作用域层级。
  std::vector<int> scope;
  // 如果变量是导入变量，则为非空字符串，表示从哪里导入的路径。
  std::string import_path;
  // 标识符可以在不进行 __used__ 检查的情况下访问的作用域列表。
  std::vector<std::vector<int>> access_checked;
  // 如果设置，则表示该标识符不能被遮蔽。
  // 这通常用于全局变量，这些变量不应该被局部作用域中的同名变量覆盖。
  bool no_shadow = false;
  // 如果设置，则表示该标识符是类或函数的泛型。
  bool generic = false;
  // 如果该标识符是静态变量，则设置此项。
  char static_type = 0;
  // 如果设置，则表示标识符不应该被支配。这通常用于循环变量。
  bool avoid_domination = false;

 public:
  SimplifyItem(Kind kind, std::string base_name, std::string canonical_name,
               std::string module_name, std::vector<int> scope,
               std::string import_path = "")
      : kind(kind),
        base_name(std::move(base_name)),
        canonical_name(std::move(canonical_name)),
        module_name(std::move(module_name)),
        scope(std::move(scope)),
        import_path(std::move(import_path)) {}

  auto get_base_name() const -> std::string { return base_name; }
  auto get_module() const -> std::string { return module_name; }
  auto is_var() const -> bool { return kind == Var; }
  auto is_func() const -> bool { return kind == Func; }
  auto is_type() const -> bool { return kind == Type; }
  auto is_import() const -> bool { return !import_path.empty(); }
  auto is_global() const -> bool {
    return scope.size() == 1 && base_name.empty();
  }
  // 检查标识符是否在条件块内（即作用域数组大小大于 1）。
  // 在条件块内的标识符可能不会在运行时执行，因此需要特殊处理。
  auto is_conditional() const -> bool { return scope.size() > 1; }
  auto is_generic() const -> bool { return generic; }
  auto is_static() const -> char { return static_type; }
  // 检查标识符是否可以在其作用域内主导其他标识符。
  // 例如，在列表推导中的循环变量通常不应该被视为支配变量。
  auto can_dominate() const -> bool { return !avoid_domination; }
};

// 一个上下文类，用于在代码简化过程中追踪标识符。
// 这个类的设计和实现思想主要是为了管理和维护在代码转换、优化和简化过程
// 中的变量、函数和类型的作用域和属性。
struct SimplifyContext : public Context<SimplifyItem> {
  // 指向共享缓存的指针，用于在整个程序或模块中存储和访问共享数据。
  Cache* cache;

  // 保存当前作用域信息。在Python中，作用域规则可能需要特别处理，
  // 特别是在条件块（可能在运行时不被执行的代码块）中。
  struct {
    // 用于为每个条件块生成唯一的作用域 ID。
    int counter;
    // 当前条件块的层次结构，以向上追踪嵌套的条件块。
    std::vector<int> blocks;
    // 用于存储在条件块转换后要添加到块前面的语句列表。
    std::map<int, std::vector<StmtPtr>> stmts;
  } scope;

  // 保存当前基础信息，定义为函数或类块。
  // 它包含有关函数或类的信息，如名称、属性、捕获的标识符等。
  struct Base {
    // 函数或类的规范名称，表示该基础所属的函数或类。
    std::string name;
    // 仅对函数有效，用于跟踪函数属性（例如是否具有 @atomic 或 @test 属性）。
    Attr* attributes;
    // 仅对类有效，存储类字段的列表，按照遍历顺序排列。
    std::shared_ptr<std::vector<std::string>> deduced_members;
    // 用于推断类字段的 self 参数的规范名称（例如，self in self.foo）。
    std::string self_name;
    // 捕获的标识符映射到新的规范名称和它们的类型，表示它们是类型、静态还是变量。
    std::unordered_map<std::string, std::pair<std::string, ExprPtr>>* captures;

    // 从 Python 中提取的标识符的集合。
    std::unordered_set<std::string>* py_captures;

    // 包含有关当前作用域的信息。用于支持 Python 的变量作用域规则。
    std::vector<int> scope;

    // 这个 Loop 结构体在编译器中的作用主要是帮助管理和优化循环结构，
    // 尤其是那些包含 break 语句和 else 子句的循环。通过跟踪循环的作用域和
    // 变量行为，编译器能够更精确地优化代码并避免潜在的错误。
    struct Loop {
      // 这个成员变量用于存储与当前循环关联的 break 变量的名称。
      // 当解析包含 else 子句的循环时，编译器会创建一个 break 变量。
      // 这个变量用于跟踪循环是否因 break 语句而提前退出，这对于正确处理
      // loop-else 构造（即循环具有一个 else 子句，只有在没有执行 break
      // 时才执行）至关重要。
      // 如果循环没有 else 块，那么对应的 break_var 将会是一个空字符串。
      std::string break_var;
      // 这个向量表示当前循环的作用域层级。
      // 作用域层级是用于追踪变量、函数和类的可见性和生命周期。
      // 在嵌套循环的情况下，每个循环将有其独特的作用域层级，
      // 这有助于编译器理解变量在哪个循环中有效。
      std::vector<int> scope;
      // 这个集合存储了在循环内部“看到”但在赋值之前就被引用的变量的名称。
      // 这对于处理那些在循环中更新的变量非常重要。
      // 变量在循环中的行为可能会非常特殊。
      // 例如，一个变量可能在循环开始之前就存在，但在循环内部被修改。
      // 这个集合帮助编译器跟踪这些变量，以确保它们在循环中正确地被“支配”（
      // 即它们的行为受到循环逻辑的控制）。
      std::unordered_set<std::string> seen_vars;
    };
    // 用于管理嵌套循环的堆栈，包括每个循环的 break 变量和作用域信息。
    std::vector<Loop> loops;

   public:
    explicit Base(std::string name, Attr* attributes = nullptr);
    auto get_loop() -> Loop* {
      return loops.empty() ? nullptr : &(loops.back());
    }
    auto is_type() const -> bool { return attributes == nullptr; }
  };
  // 当前基础栈，存储所有嵌套的基础（函数或类）。
  std::vector<Base> bases;

  // 用于修改 SimplifyContext 状态的辅助结构，添加和移除 Base 实例。
  struct BaseGuard {
    // 指向 SimplifyContext 的指针，用于在构造和析构期间修改SimplifyContext
    // 的状态。在构造时，将新的 Base 实例添加到 bases 堆栈，并调用 add_block；
    // 在析构时，从 bases 堆栈中弹出 Base 实例，并调用 pop_block。
    SimplifyContext* holder;
    BaseGuard(SimplifyContext* holder, const std::string& name)
        : holder(holder) {
      holder->bases.emplace_back(Base(name));
      holder->bases.back().scope = holder->scope.blocks;
      holder->add_block();
    }
    ~BaseGuard() {
      holder->bases.pop_back();
      holder->pop_block();
    }
  };

  // 保存已见的全局标识符，以防止在后续创建具有相同名称的局部变量。
  std::unordered_map<std::string, std::unordered_map<std::string, ExprPtr>>
      seen_global_identifiers;

  // 表示当前是否正在加载标准库。
  bool is_stdlib_loading;
  // 当前模块的名称，默认为 __main__。
  ImportFile module_name;
  // 表示是否在短路表达式的依赖部分，以禁止在那里进行赋值表达式。
  bool is_conditional_expr;
  // 允许使用 type() 表达式。目前用于禁止在类和函数定义中使用 type()。
  bool allow_type_of;
  // 如果设置，表示所有赋值不应在后续被支配。
  bool avoid_domination = false;

 public:
  SimplifyContext(std::string filename, Cache* cache);

  // 向上下文中添加一个标识符（如变量、类型、函数等）。
  // name 是标识符的名称，var 是标识符的具体信息。
  void add(const std::string& name, const Item& var) override;
  // 专门用于添加变量到上下文的方法。
  // name 是变量的名称，canonical_name 是其规范名称，
  // source_info 提供源代码相关信息。
  auto add_var(const std::string& name, const std::string& canonical_name,
               const SourceInfo& source_info = SourceInfo()) -> Item;
  // 专门用于添加类型到上下文的方法。
  auto add_type(const std::string& name, const std::string& canonical_name,
                const SourceInfo& source_info = SourceInfo()) -> Item;
  // 专门用于添加函数到上下文的方法。
  auto add_func(const std::string& name, const std::string& canonical_name,
                const SourceInfo& source_info = SourceInfo()) -> Item;
  // 添加一个始终可见的项到标准库模块，确保其在所有模块中都可见。
  // 用于处理那些需要跨模块可见的标识符。
  auto add_always_visible(const Item& item) -> Item;

  // 在上下文中查找一个标识符。如果找不到，返回 nullptr。
  auto find(const std::string& name) const -> Item override;
  // 类似于 find，但如果找不到标识符，将引发断言错误。
  auto force_find(const std::string& name) const -> Item;
  // 查找上下文中的一个标识符，执行支配分析，特别是对于在条件块中
  // 定义的项（例如，Python的作用域规则）。
  auto find_dominating_binding(const std::string& name) -> Item;

  // 返回当前基础（如函数或类）的规范名称。顶层基础返回空字符串。
  auto get_base_name() const -> std::string;
  // 返回当前模块的名称。
  auto get_module() const -> std::string;
  // 将当前上下文状态的信息打印输出，用于调试。
  void dump() override;

  // 生成一个给定字符串的唯一规范名称。
  auto generate_canonical_name(const std::string& name,
                               bool include_base = false,
                               bool zero_id = false) const -> std::string;
  // 进入一个条件块（如 if 或 while 块）。
  void enter_conditional_block();
  // 离开一个条件块，并用在该块中新添加的标识符声明填充 stmts（如果设置）。
  void leave_conditional_block(std::vector<StmtPtr>* stmts = nullptr);
  // 判断当前是否处于全局作用域。
  auto is_global() const -> bool;
  // 判断当前是否处于条件块内。
  auto is_conditional() const -> bool;
  // 获取当前的Base（函数或类）。
  auto get_base() -> Base*;
  // 判断当前Base是否为函数。
  auto in_function() const -> bool;
  // 判断当前Base是否为类。
  auto in_class() const -> bool;
  // 判断一个项是否定义在当前基础或模块之外。
  auto is_outer(const Item& val) const -> bool;
  // 获取封闭类基础（如果存在）。
  auto get_class_base() -> Base*;

 private:
  // 用于打印当前上下文状态，pad 用于格式化输出。
  void dump(int pad);
};

}  // namespace Pud::AST

#endif  // PUD_SIMPLIFY_CONTEXT_H