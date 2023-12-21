#ifndef PUD_AST_CACHE_H
#define PUD_AST_CACHE_H

#include <map>
#include <ostream>
#include <set>
#include <string>
#include <vector>

#include "Pud/AST/AST.h"
#include "Pud/AST/Context.h"
#include "Pud/Common/Common.h"
#include "Pud/IR/IR.h"
#include "Pud/IR/PyExtension.h"

#define FILE_GENERATED "<generated>"
#define MODULE_MAIN "__main__"
#define MAIN_IMPORT ""
#define STDLIB_IMPORT ":stdlib:"
#define STDLIB_INTERNAL_MODULE "internal"

#define TYPE_TUPLE "Tuple"
#define TYPE_KWTUPLE "KwTuple.N"
#define TYPE_TYPEVAR "TypeVar"
#define TYPE_CALLABLE "Callable"
#define TYPE_PARTIAL "Partial.N"
#define TYPE_OPTIONAL "Optional"
#define TYPE_SLICE "std.internal.types.slice.Slice"
#define FN_UNWRAP "std.internal.types.optional.unwrap"
#define VAR_ARGV "__argv__"

#define MAX_INT_WIDTH 10000
#define MAX_REALIZATION_DEPTH 200
#define MAX_STATIC_ITER 1024

namespace Pud::Type {
struct TypeContext;
}

namespace Pud::AST {

struct SimplifyContext;
class SimplifyVisitor;
struct TranslateContext;

// Cache 结构体在编译器设计中的作用是管理和维护在编译过程中的各种数据结构，
// 尤其是在不同编译阶段（如AST转换、类型检查等）之间共享的数据。
// 这种设计避免了大量全局对象的使用。
// 缓存在编译器的不同阶段需要共享的数据。
// 管理标识符、类型信息、类和函数定义、导入的模块等信息。
// 支持编译器前端的功能，如源代码解析、类型检查、IR生成。
struct Cache : public std::enable_shared_from_this<Cache> {
  // 用于管理代码中每个标识符的出现次数 (e.g. Foo -> Foo.2)
  std::unordered_map<std::string, int> identifier_count;
  // 反向查找每个标识符原始名称 (e.g. Foo -> Foo.2)
  std::unordered_map<std::string, std::string> reverse_identifier_lookup;
  // 生成唯一的源代码位置信息
  int generated_source_info_count;
  // 未绑定变量的数量
  int unbound_count;
  // 自动生成变量的数量
  int var_count;
  // 存储导入文件的数量
  int age;

  // 存储导入的模块和类的详细信息。
  struct Import {
    // 导入文件的绝对路径。
    std::string filename;
    // 导入的上下文信息，例如 SimplifyContext。
    std::shared_ptr<SimplifyContext> ctx;
    // 用于标识导入模块的唯一变量，检查是否已经加载。
    std::string import_var;
    // 文件内容，可以按行和列进行索引。
    std::vector<std::string> content;
    // 模块的相对名称，例如 foo.bar。
    std::string module_name;
  };

  // 存储可执行文件的路径
  std::string argv0;
  // 存储入口模块的路径
  std::string module0;
  // 指向IR模块的指针
  IR::Module* module = nullptr;

  // 存储导入的模块信息
  std::unordered_map<std::string, Import> imports;

  // 全局变量
  std::map<std::string, IR::Var*> globals;

  // 类
  struct Class {
    // 类的泛型（未实现）AST模板
    std::shared_ptr<ClassStmt> ast;
    // 未简化的AST
    std::shared_ptr<ClassStmt> original_ast;

    // 类方法的查找表，每个非规范名称指向相应方法的根函数名称。
    std::unordered_map<std::string, std::string> methods;

    // 存储类字段的信息
    struct ClassField {
      // 字段名称
      std::string name;
      // 字段类型
      Pud::Type::TypePtr type;
      // 基类名称
      std::string base_class;
    };

    // 类的 ClassField 实例的列表。
    std::vector<ClassField> fields;

    // 类变量的字典，名称映射到规范名称。
    std::unordered_map<std::string, std::string> class_vars;

    // 存储类实现的细节，包括实现类型、字段、IR类型指针等。
    struct ClassRealization {
      Pud::Type::ClassTypePtr type;
      std::vector<std::pair<std::string, Pud::Type::TypePtr>> fields;
      Pud::IR::Types::Type* ir = nullptr;

      struct VTable {
        // Maps {base, thunk signature} to {thunk realization, thunk ID}
        std::map<std::pair<std::string, std::string>,
                 std::pair<Pud::Type::FuncTypePtr, size_t>>
            table;
        Pud::IR::Var* ir = nullptr;
      };
      // All vtables (for each base class)
      std::unordered_map<std::string, VTable> vtables;
      // Realization ID
      size_t id = 0;
    };

    // 实现查找表，将实现类名称映射到 ClassRealization 实例。
    std::unordered_map<std::string, std::shared_ptr<ClassRealization>>
        realizations;

    // 如果类是多态的并且有运行时类型信息（RTTI），则设置为真。
    bool rtti = false;
    // 虚拟方法名称列表。
    std::unordered_set<std::string> virtuals;
    // 方法解析顺序（MRO）。
    std::vector<ExprPtr> mro;

    // 静态继承的类列表。
    std::vector<std::string> static_parent_classes;

    // Module information
    std::string module;

    Class() : ast(nullptr), original_ast(nullptr), rtti(false) {}
  };
  // 存储类的信息。
  std::unordered_map<std::string, Class> classes;
  size_t class_realization_count = 0;

  struct Function {
    // 函数的泛型（未实现）AST模板
    std::shared_ptr<FunctionStmt> ast;
    // 未简化的AST
    std::shared_ptr<FunctionStmt> original_ast;

    // 存储函数实现的细节，包括实现类型、AST、IR函数指针等。
    struct FunctionRealization {
      Pud::Type::FuncTypePtr type;
      std::shared_ptr<FunctionStmt> ast;
      IR::Func* ir;
    };
    // 实现查找表，将实现函数名称映射到 FunctionRealization 实例。
    std::unordered_map<std::string, std::shared_ptr<FunctionRealization>>
        realizations;

    // 未实现函数类型。
    Pud::Type::FuncTypePtr type;

    // Module information
    std::string root_name = "";
    bool is_toplevel = false;

    Function()
        : ast(nullptr),
          original_ast(nullptr),
          type(nullptr),
          root_name(""),
          is_toplevel(false) {}
  };
  // 存储函数的信息。
  std::unordered_map<std::string, Function> functions;

  struct Overload {
    // 重载的规范名称，例如 Foo.__init__.1。
    std::string name;
    // 重载的年龄，用于防止在代码中定义之前使用重载。
    int age;
  };
  // 函数重载的信息。
  std::unordered_map<std::string, std::vector<Overload>> overloads;

  // 用于IR API访问的上下文。
  std::shared_ptr<Type::TypeContext> type_ctx;
  std::shared_ptr<TranslateContext> codegen_ctx;
  // 用于存储即将转换为IR的函数实现和部分记录名。
  std::set<std::pair<std::string, std::string>> pending_realizations;
  std::unordered_map<std::string,
                     std::pair<Pud::Type::FuncTypePtr, std::vector<char>>>
      partials;

  // Custom operators
  std::unordered_map<
      std::string, std::pair<bool, std::function<StmtPtr(AST::SimplifyVisitor*,
                                                         AST::CustomStmt*)>>>
      custom_block_stmts;
  std::unordered_map<std::string, std::function<StmtPtr(AST::SimplifyVisitor*,
                                                        AST::CustomStmt*)>>
      custom_expr_stmts;

  // Plugin-added import paths
  std::vector<std::string> plugin_import_paths;

  // Set if the Codon is running in JIT mode.
  bool is_jit;
  int jit_cell;

  std::unordered_map<std::string, std::pair<std::string, bool>> replacements;
  std::unordered_map<std::string, int> generated_tuples;
  std::vector<ParserException> errors;

  /// Set if operates in Python compatibility mode (e.g., with Python numerics)
  bool python_compat = false;
  /// Set if operates in Python extension mode
  bool python_ext = false;

 public:
  explicit Cache(std::string argv0 = "");

  // 生成临时变量名
  auto get_temporary_var(const std::string& prefix = "", char sigil = '.')
      -> std::string;
  // 进行标识符的反向查找
  auto rev(const std::string& s) -> std::string;

  /// Generate a unique SourceInfo for internally generated AST nodes.
  auto generate_source_info() -> SourceInfo;
  /// Get file contents at the given location.
  auto get_content(const SourceInfo& info) -> std::string;
  /// Register a global identifier.
  void add_global(const std::string& name, Pud::IR::Var* var = nullptr);

  /// Realization API.

  /// Find a class with a given canonical name and return a matching Types::Type
  /// pointer or a nullptr if a class is not found. Returns an _uninstantiated_
  /// type.
  auto find_class(const std::string& name) const -> Pud::Type::ClassTypePtr;
  /// Find a function with a given canonical name and return a matching
  /// Pud::Type::Type pointer or a nullptr if a function is not found. Returns
  /// an _uninstantiated_ type.
  auto find_function(const std::string& name) const -> Pud::Type::FuncTypePtr;
  /// Find the canonical name of a class method.
  auto get_method(const Pud::Type::ClassTypePtr& typ, const std::string& member)
      -> std::string {
    if (auto m = in(classes, typ->name)) {
      if (auto t = in(m->methods, member))
        return *t;
    }
    // assert(false && "cannot find '{}' in '{}'" && member &&
    // typ->to_string());
    return "";
  }
  /// Find the class method in a given class type that best matches the given
  /// arguments. Returns an _uninstantiated_ type.
  auto find_method(Pud::Type::ClassType* typ, const std::string& member,
                   const std::vector<Pud::Type::TypePtr>& args)
      -> Pud::Type::FuncTypePtr;

  /// Given a class type and the matching generic vector, instantiate the type
  /// and realize it.
  auto realize_type(Pud::Type::ClassTypePtr type,
                    const std::vector<Pud::Type::TypePtr>& generics = {})
      -> Pud::IR::Types::Type*;
  /// Given a function type and function arguments, instantiate the type and
  /// realize it. The first argument is the function return type.
  /// You can also pass function generics if a function has one (e.g. T in def
  /// foo[T](...)). If a generic is used as an argument, it will be
  /// auto-deduced. Pass only if a generic cannot be deduced from the provided
  /// args.
  auto realize_function(Pud::Type::FuncTypePtr type,
                        const std::vector<Pud::Type::TypePtr>& args,
                        const std::vector<Pud::Type::TypePtr>& generics = {},
                        const Pud::Type::ClassTypePtr& parent_class = nullptr)
      -> Pud::IR::Func*;

  auto make_tuple(const std::vector<Pud::Type::TypePtr>& types)
      -> Pud::IR::Types::Type*;
  auto make_function(const std::vector<Pud::Type::TypePtr>& types)
      -> Pud::IR::Types::Type*;
  auto make_union(const std::vector<Pud::Type::TypePtr>& types)
      -> Pud::IR::Types::Type*;

  void parse_code(const std::string& code);

  static auto merge_c3(std::vector<std::vector<ExprPtr>>&)
      -> std::vector<ExprPtr>;

  std::shared_ptr<Pud::IR::PyModule> py_module = nullptr;
  void populate_python_module();
};

}  // namespace Pud::AST

#endif  // PUD_AST_CACHE_H