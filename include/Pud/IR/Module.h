#ifndef PUD_IR_MODULE_H
#define PUD_IR_MODULE_H

#include <iterator>
#include <memory>
#include <string>
#include <unordered_map>

#include "Pud/IR/Func.h"
#include "Pud/IR/Util/Iterators.h"
#include "Pud/IR/Value.h"
#include "Pud/IR/Var.h"

namespace Pud {

namespace AST {
struct Cache;
class TranslateVisitor;
class TypecheckVisitor;
}  // namespace AST

namespace IR {

// Module 类是一个编译器中间表示对象，表示程序的整体结构。
class Module : public AcceptorExtend<Module, Node> {
 public:
  // VOID_NAME，BOOL_NAME，等：
  // 这些是基本类型（如 void, bool, byte, int, float 等）的名称常量。
  static const std::string VOID_NAME;
  static const std::string BOOL_NAME;
  static const std::string BYTE_NAME;
  static const std::string INT_NAME;
  static const std::string FLOAT_NAME;
  static const std::string FLOAT32_NAME;
  static const std::string FLOAT16_NAME;
  static const std::string BFLOAT16_NAME;
  static const std::string FLOAT128_NAME;
  static const std::string STRING_NAME;

  // EQ_MAGIC_NAME，NE_MAGIC_NAME，等：
  // 这些常量代表各种魔术方法的名称，如等于（==）、不等于（!=）、小于（<）等。
  static const std::string EQ_MAGIC_NAME;
  static const std::string NE_MAGIC_NAME;
  static const std::string LT_MAGIC_NAME;
  static const std::string GT_MAGIC_NAME;
  static const std::string LE_MAGIC_NAME;
  static const std::string GE_MAGIC_NAME;

  static const std::string POS_MAGIC_NAME;
  static const std::string NEG_MAGIC_NAME;
  static const std::string INVERT_MAGIC_NAME;

  static const std::string ADD_MAGIC_NAME;
  static const std::string SUB_MAGIC_NAME;
  static const std::string MUL_MAGIC_NAME;
  static const std::string MATMUL_MAGIC_NAME;
  static const std::string TRUE_DIV_MAGIC_NAME;
  static const std::string FLOOR_DIV_MAGIC_NAME;
  static const std::string MOD_MAGIC_NAME;
  static const std::string POW_MAGIC_NAME;
  static const std::string LSHIFT_MAGIC_NAME;
  static const std::string RSHIFT_MAGIC_NAME;
  static const std::string AND_MAGIC_NAME;
  static const std::string OR_MAGIC_NAME;
  static const std::string XOR_MAGIC_NAME;

  static const std::string IADD_MAGIC_NAME;
  static const std::string ISUB_MAGIC_NAME;
  static const std::string IMUL_MAGIC_NAME;
  static const std::string IMATMUL_MAGIC_NAME;
  static const std::string ITRUE_DIV_MAGIC_NAME;
  static const std::string IFLOOR_DIV_MAGIC_NAME;
  static const std::string IMOD_MAGIC_NAME;
  static const std::string IPOW_MAGIC_NAME;
  static const std::string ILSHIFT_MAGIC_NAME;
  static const std::string IRSHIFT_MAGIC_NAME;
  static const std::string IAND_MAGIC_NAME;
  static const std::string IOR_MAGIC_NAME;
  static const std::string IXOR_MAGIC_NAME;

  static const std::string RADD_MAGIC_NAME;
  static const std::string RSUB_MAGIC_NAME;
  static const std::string RMUL_MAGIC_NAME;
  static const std::string RMATMUL_MAGIC_NAME;
  static const std::string RTRUE_DIV_MAGIC_NAME;
  static const std::string RFLOOR_DIV_MAGIC_NAME;
  static const std::string RMOD_MAGIC_NAME;
  static const std::string RPOW_MAGIC_NAME;
  static const std::string RLSHIFT_MAGIC_NAME;
  static const std::string RRSHIFT_MAGIC_NAME;
  static const std::string RAND_MAGIC_NAME;
  static const std::string ROR_MAGIC_NAME;
  static const std::string RXOR_MAGIC_NAME;

  static const std::string INT_MAGIC_NAME;
  static const std::string FLOAT_MAGIC_NAME;
  static const std::string BOOL_MAGIC_NAME;
  static const std::string STR_MAGIC_NAME;
  static const std::string REPR_MAGIC_NAME;

  static const std::string GETITEM_MAGIC_NAME;
  static const std::string SETITEM_MAGIC_NAME;
  static const std::string ITER_MAGIC_NAME;
  static const std::string LEN_MAGIC_NAME;

  static const std::string NEW_MAGIC_NAME;
  static const std::string INIT_MAGIC_NAME;

 private:
  // 指向程序的 "main" 函数的指针。
  std::unique_ptr<Func> main_func;
  // 指向程序的 argv 变量的指针。
  std::unique_ptr<Var> arg_var;
  // 全局变量列表。
  std::list<std::unique_ptr<Var>> vars;
  // 全局变量映射（id 到变量的迭代器）。
  std::unordered_map<id_t, std::list<std::unique_ptr<Var>>::iterator> var_map;
  // 全局值列表。
  std::list<std::unique_ptr<Value>> values;
  // 全局值映射（id 到值的迭代器）。
  std::unordered_map<id_t, std::list<std::unique_ptr<Value>>::iterator>
      value_map;
  // 全局类型列表。
  std::list<std::unique_ptr<Types::Type>> types;
  // 全局类型映射（名称到类型的迭代器）。
  std::unordered_map<std::string,
                     std::list<std::unique_ptr<Types::Type>>::iterator>
      types_map;

  // 指向 AST 缓存的指针，用于类型检查。
  AST::Cache* cache = nullptr;

 public:
  static const char NodeId;

  // 初始化一个 IR 模块。
  explicit Module(const std::string& name = "");

  virtual ~Module() noexcept = default;

  // 返回主函数。
  auto get_main_func() -> Func* { return main_func.get(); }
  auto get_main_func() const -> const Func* { return main_func.get(); }

  // 返回 argv 变量。
  auto get_arg_var() -> Var* { return arg_var.get(); }
  auto get_arg_var() const -> const Var* { return arg_var.get(); }

  auto begin() { return Util::raw_ptr_adaptor(vars.begin()); }
  auto end() { return Util::raw_ptr_adaptor(vars.end()); }
  auto begin() const { return Util::const_raw_ptr_adaptor(vars.begin()); }
  auto end() const { return Util::const_raw_ptr_adaptor(vars.end()); }

  auto front() -> Var* { return vars.front().get(); }
  auto back() -> Var* { return vars.back().get(); }
  auto front() const -> const Var* { return vars.front().get(); }
  auto back() const -> const Var* { return vars.back().get(); }

  // 按 id 获取变量
  auto get_var(id_t id) -> Var* {
    auto it = var_map.find(id);
    return it != var_map.end() ? it->second->get() : nullptr;
  }
  auto get_var(id_t id) const -> const Var* {
    auto it = var_map.find(id);
    return it != var_map.end() ? it->second->get() : nullptr;
  }
  // 移除指定的变量。
  void remove(const Var* v) {
    auto it = var_map.find(v->get_id());
    vars.erase(it->second);
    var_map.erase(it);
  }

  auto values_begin() { return Util::raw_ptr_adaptor(values.begin()); }
  auto values_end() { return Util::raw_ptr_adaptor(values.end()); }
  auto values_begin() const {
    return Util::const_raw_ptr_adaptor(values.begin());
  }
  auto values_end() const { return Util::const_raw_ptr_adaptor(values.end()); }
  auto values_front() -> Value* { return values.front().get(); }
  auto values_back() -> Value* { return values.back().get(); }
  auto values_front() const -> const Value* { return values.front().get(); }
  auto values_back() const -> const Value* { return values.back().get(); }

  auto get_value(id_t id) -> Value* {
    auto it = value_map.find(id);
    return it != value_map.end() ? it->second->get() : nullptr;
  }
  auto get_value(id_t id) const -> const Value* {
    auto it = value_map.find(id);
    return it != value_map.end() ? it->second->get() : nullptr;
  }

  void remove(const Value* v) {
    auto it = value_map.find(v->get_id());
    values.erase(it->second);
    value_map.erase(it);
  }

  auto types_begin() { return Util::raw_ptr_adaptor(types.begin()); }
  auto types_end() { return Util::raw_ptr_adaptor(types.end()); }
  auto types_begin() const {
    return Util::const_raw_ptr_adaptor(types.begin());
  }
  auto types_end() const { return Util::const_raw_ptr_adaptor(types.end()); }

  auto types_front() const -> Types::Type* { return types.front().get(); }
  auto types_back() const -> Types::Type* { return types.back().get(); }

  auto get_type(const std::string& name) -> Types::Type* {
    auto it = types_map.find(name);
    return it == types_map.end() ? nullptr : it->second->get();
  }
  auto get_type(const std::string& name) const -> Types::Type* {
    auto it = types_map.find(name);
    return it == types_map.end() ? nullptr : it->second->get();
  }

  void remove(Types::Type* t) {
    auto it = types_map.find(t->get_name());
    types.erase(it->second);
    types_map.erase(it);
  }

  // 构造并注册 IR 节点。
  template <typename DesiredType, typename... Args>
  auto N(Pud::SourceInfo s, Args&&... args) -> DesiredType* {
    auto* ret = new DesiredType(std::forward<Args>(args)...);
    ret->set_module(this);
    ret->set_source_info(s);

    store(ret);
    return ret;
  }
  template <typename DesiredType, typename... Args>
  auto N(const Pud::SourceObject* s, Args&&... args) -> DesiredType* {
    return N<DesiredType>(s->get_source_info(), std::forward<Args>(args)...);
  }
  template <typename DesiredType, typename... Args>
  auto N(const Node* s, Args&&... args) -> DesiredType* {
    return N<DesiredType>(s->get_source_info(), std::forward<Args>(args)...);
  }
  template <typename DesiredType, typename... Args>
  auto Nr(Args&&... args) -> DesiredType* {
    return N<DesiredType>(Pud::SourceInfo(), std::forward<Args>(args)...);
  }

  // 获取和设置类型检查缓存。
  auto get_cache() const -> AST::Cache* { return cache; }
  void set_cache(AST::Cache* c) { cache = c; }

  // 解析代码块。
  void parse_code(const std::string& code);

  auto get_or_realize_method(Types::Type* parent,
                             const std::string& method_name,
                             std::vector<Types::Type*> args,
                             std::vector<Types::Generic> generics = {})
      -> Func*;

  auto get_or_realize_func(const std::string& func_name,
                           std::vector<Types::Type*> args,
                           std::vector<Types::Generic> generics = {},
                           const std::string& module = "") -> Func*;

  auto get_or_realize_type(const std::string& type_name,
                           std::vector<Types::Generic> generics = {},
                           const std::string& module = "") -> Types::Type*;

  auto get_void_type() -> Types::Type*;
  auto get_bool_type() -> Types::Type*;
  auto get_byte_type() -> Types::Type*;
  auto get_int_type() -> Types::Type*;
  auto get_float_type() -> Types::Type*;
  auto get_float32_type() -> Types::Type*;
  auto get_float16_type() -> Types::Type*;
  auto get_bfloat16_type() -> Types::Type*;
  auto get_float128_type() -> Types::Type*;
  auto get_string_type() -> Types::Type*;
  auto get_pointer_type(Types::Type* base) -> Types::Type*;
  auto get_array_type(Types::Type* base) -> Types::Type*;
  auto get_generator_type(Types::Type* base) -> Types::Type*;
  auto get_optional_type(Types::Type* base) -> Types::Type*;
  auto get_func_type(Types::Type* ret_type, std::vector<Types::Type*> arg_types,
                     bool variadic = false) -> Types::Type*;
  auto get_intn_type(unsigned len, bool sign) -> Types::Type*;
  auto get_vector_type(unsigned count, Types::Type* base) -> Types::Type*;
  auto get_tuple_type(std::vector<Types::Type*> args) -> Types::Type*;
  auto get_union_type(std::vector<Types::Type*> types) -> Types::Type*;
  auto get_none_type() -> Types::Type*;

  auto get_int(int64_t v) -> Value*;
  auto get_float(double v) -> Value*;
  auto get_bool(bool v) -> Value*;
  auto get_string(std::string v) -> Value*;

  auto unsafe_get_dummy_func_type() -> Types::Type*;
  auto unsafe_get_pointer_type(Types::Type* base) -> Types::Type*;
  auto unsafe_get_array_type(Types::Type* base) -> Types::Type*;
  auto unsafe_get_generator_type(Types::Type* base) -> Types::Type*;
  auto unsafe_get_optional_type(Types::Type* base) -> Types::Type*;
  auto unsafe_get_func_type(const std::string& name, Types::Type* ret_type,
                            std::vector<Types::Type*> arg_types,
                            bool variadic = false) -> Types::Type*;
  auto unsafe_get_membered_type(const std::string& name, bool ref = false)
      -> Types::Type*;
  auto unsafe_get_intn_type(unsigned len, bool sign) -> Types::Type*;
  auto unsafe_get_vector_type(unsigned count, Types::Type* base)
      -> Types::Type*;
  auto unsafe_get_union_type(const std::vector<Types::Type*>& types)
      -> Types::Type*;

 private:
  void store(Types::Type* t) {
    types.emplace_back(t);
    types_map[t->get_name()] = std::prev(types.end());
  }
  void store(Value* v) {
    values.emplace_back(v);
    value_map[v->get_id()] = std::prev(values.end());
  }
  void store(Var* v) {
    vars.emplace_back(v);
    var_map[v->get_id()] = std::prev(vars.end());
  }
};

}  // namespace IR
}  // namespace Pud

#endif  // PUD_IR_MODULE_H