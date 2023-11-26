#ifndef PUD_TYPE_TYPE_H
#define PUD_TYPE_TYPE_H

#include <memory>
#include <unordered_map>
#include <vector>

#include "Pud/Common/Source.h"

#define TYPE_TUPLE "Tuple"

namespace Pud::Type {

struct FuncType;
struct ClassType;
struct LinkType;
struct RecordType;
struct PartialType;
struct StaticType;
struct UnionType;

/// 定义了一个用于类型推断的复杂结构，采用了Hindley-Milner
/// Algorithm W进行类型推断。
/// 这种类型系统的设计允许编译器自动推断程序中的类型，减少了对程序员的类型标
/// 注要求，同时提高了代码的灵活性和表现力。
struct Type : public SourceObject, public std::enable_shared_from_this<Type> {
  /// 管理类型统一（unification）过程中的步骤，特别是为了能够在必要时撤销这些步骤。
  /// 由于unify()方法是破坏性的（即它会修改类型的状态），因此需要一种机制来记录这些
  /// 变化，以便以后可以撤销。
  struct Unification {
    // 撤销类型统一步骤。它利用上述列表来恢复原始的类型状态，撤销在unify()方法中所做的所有更改。
    // 由于 unify 是一个破坏性操作（destructive operation），意味着它会修改类型的状态，因此实现了
    // 一个机制来允许之后撤销（undo）这些改变。
    void undo();

    // 存储已更改的未绑定类型的列表。LinkType*指的是指向未绑定类型的指针。
    // 当类型统一时，某些未绑定的类型可能会与其他类型相关联或链接。这个列表用于记录
    // 这些更改，以便在需要时可以撤销这些链接。
    std::vector<LinkType*> linked;
    // 存储级别发生变化的未绑定类型及其原始级别的列表。每个元素是一个包含类型指针和整数级别的对。
    // 在类型统一过程中，未绑定类型的级别（level）可能会改变。这个列表记录了这些变化，以便以后可
    // 以恢复它们原来的级别。
    std::vector<std::pair<LinkType*, int>> leveled;
    // 存储了在统一过程中分配的特征（traits）。
    // 在类型统一时，可能会给某些类型分配特定的特征。这个列表用于追踪这些分配，以便在需要时撤销。
    std::vector<LinkType*> traits;
    // 存储由类型统一过程拥有的类型指针的列表。
    // 这个列表确保在类型统一过程中创建的任何新类型都不会因为内存问题而丢失或错误删除。
    std::vector<std::shared_ptr<Type>> owned_types;
  };

 public:
  // 以下三个接口定义了类型系统中的核心功能，涉及类型统一（unification）、泛化（generalization）
  // 和实例化（instantiation）。这些功能是类型推断和类型检查的基础。

  // 将给定的类型typ与当前类型统一。类型统一即确定两个类型是否可以被视为相同的过程。
  virtual auto unify(Type* typ, Unification* undo) -> int = 0;

  // 泛化所有级别低于提供级别的未绑定类型。泛化是将未绑定类型转换为泛型类型的过程。
  // 返回泛化后的类型。
  virtual auto generalize(int at_level) -> std::shared_ptr<Type> = 0;

  // 实例化所有泛型类型。这是generalize()的逆过程，它将所有泛型类型替换为新的未绑定类型。
  virtual auto instantiate(
      int at_level, int* unbound_count,
      std::unordered_map<int, std::shared_ptr<Type>>* cache)
      -> std::shared_ptr<Type> = 0;

 public:  // NOLINT(*)
  // 用于获取最终类型。它遵循所有LinkType链接，以找到链接序列的末尾。
  // 如果有一个类型链a -> b -> c -> d，follow()将返回指向d的指针。
  virtual auto follow() -> std::shared_ptr<Type>;

  // 获取内部未绑定类型的列表。
  // 在类型推断过程中，用来查找所有尚未确定具体类型的类型变量。
  virtual auto get_unbounds() const -> std::vector<std::shared_ptr<Type>>;

  // 判断一个类型是否有意义或者是否能够在给定的类型系统中被实现。例如某些类型可能在理论上
  // 定义了，但在实践中无法实现。
  virtual auto can_realize() const -> bool = 0;

  // 用于检查类型是否已经完全确定，没有任何剩余的泛型或未绑定类型。
  virtual auto is_instantiated() const -> bool = 0;

  auto to_string() const -> std::string;

  auto pretty_string() const -> std::string;

  virtual auto debug_string(char mode) const -> std::string = 0;

  virtual auto realized_name() const -> std::string = 0;

  // 避免使用dynamic_cast，没有llvm classof那种机制，一种简便的实现方式。
  virtual auto get_func() -> std::shared_ptr<FuncType> { return nullptr; }
  virtual auto get_partial() -> std::shared_ptr<PartialType> { return nullptr; }
  virtual auto get_class() -> std::shared_ptr<ClassType> { return nullptr; }
  virtual auto get_record() -> std::shared_ptr<RecordType> { return nullptr; }
  virtual auto get_link() -> std::shared_ptr<LinkType> { return nullptr; }
  virtual auto get_unbound() -> std::shared_ptr<LinkType> { return nullptr; }
  virtual auto get_static() -> std::shared_ptr<StaticType> { return nullptr; }
  virtual auto get_union() -> std::shared_ptr<UnionType> { return nullptr; }
  virtual auto get_heterogenous_tuple() -> std::shared_ptr<RecordType> {
    return nullptr;
  }

  virtual auto is(const std::string& s) -> bool;

  auto is_static_type() -> char;

 protected:
  explicit Type(const std::shared_ptr<Type>&);
  explicit Type(const SourceInfo& = SourceInfo());
};

using TypePtr = std::shared_ptr<Type>;

}  // namespace Pud::Type

#endif  // PUD_TYPE_TYPE_H