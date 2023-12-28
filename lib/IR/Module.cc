#include "Pud/IR/Module.h"

#include <algorithm>
#include <memory>

#include "Pud/AST/Cache.h"
#include "Pud/IR/Func.h"

namespace Pud::IR {

namespace {
auto translate_generics(Pud::AST::Cache* cache,
                        std::vector<Types::Generic>& generics)
    -> std::vector<Pud::Type::TypePtr> {
  std::vector<Pud::Type::TypePtr> ret;
  for (auto& g : generics) {
    seqassertn(g.is_static() || g.get_type_value(),
               "generic must be static or a type");
    ret.push_back(std::make_shared<Pud::Type::LinkType>(
        g.is_static()
            ? std::make_shared<Pud::Type::StaticType>(cache,
                                                      g.get_static_value())
            : (g.is_static_str() ? std::make_shared<Pud::Type::StaticType>(
                                       cache, g.get_static_string_value())
                                 : g.get_type_value()->get_ast_type())));
  }
  return ret;
}

auto generate_dummy_names(std::vector<Types::Type*>& types)
    -> std::vector<Pud::Type::TypePtr> {
  std::vector<Pud::Type::TypePtr> ret;
  for (auto* t : types) {
    seqassertn(t->get_ast_type(), "{} must have an ast type", *t);
    ret.emplace_back(t->get_ast_type());
  }
  return ret;
}

auto translate_args(Pud::AST::Cache* cache, std::vector<Types::Type*>& types)
    -> std::vector<Pud::Type::TypePtr> {
  std::vector<Pud::Type::TypePtr> ret = {std::make_shared<Pud::Type::LinkType>(
      cache, Pud::Type::LinkType::Kind::Unbound, 0)};
  for (auto* t : types) {
    seqassertn(t->get_ast_type(), "{} must have an ast type", *t);
    if (auto f = t->get_ast_type()->get_func()) {
      auto* irType = cast<Types::FuncType>(t);
      std::vector<char> mask(std::distance(irType->begin(), irType->end()), 0);
      ret.push_back(std::make_shared<Pud::Type::PartialType>(
          t->get_ast_type()->get_record(), f, mask));
    } else {
      ret.push_back(t->get_ast_type());
    }
  }
  return ret;
}
}  // namespace

const std::string Module::VOID_NAME = "void";
const std::string Module::BOOL_NAME = "bool";
const std::string Module::BYTE_NAME = "byte";
const std::string Module::INT_NAME = "int";
const std::string Module::FLOAT_NAME = "float";
const std::string Module::FLOAT32_NAME = "float32";
const std::string Module::FLOAT16_NAME = "float16";
const std::string Module::BFLOAT16_NAME = "bfloat16";
const std::string Module::FLOAT128_NAME = "float128";
const std::string Module::STRING_NAME = "str";

const std::string Module::EQ_MAGIC_NAME = "__eq__";
const std::string Module::NE_MAGIC_NAME = "__ne__";
const std::string Module::LT_MAGIC_NAME = "__lt__";
const std::string Module::GT_MAGIC_NAME = "__gt__";
const std::string Module::LE_MAGIC_NAME = "__le__";
const std::string Module::GE_MAGIC_NAME = "__ge__";

const std::string Module::POS_MAGIC_NAME = "__pos__";
const std::string Module::NEG_MAGIC_NAME = "__neg__";
const std::string Module::INVERT_MAGIC_NAME = "__invert__";

const std::string Module::ADD_MAGIC_NAME = "__add__";
const std::string Module::SUB_MAGIC_NAME = "__sub__";
const std::string Module::MUL_MAGIC_NAME = "__mul__";
const std::string Module::MATMUL_MAGIC_NAME = "__matmul__";
const std::string Module::TRUE_DIV_MAGIC_NAME = "__truediv__";
const std::string Module::FLOOR_DIV_MAGIC_NAME = "__floordiv__";
const std::string Module::MOD_MAGIC_NAME = "__mod__";
const std::string Module::POW_MAGIC_NAME = "__pow__";
const std::string Module::LSHIFT_MAGIC_NAME = "__lshift__";
const std::string Module::RSHIFT_MAGIC_NAME = "__rshift__";
const std::string Module::AND_MAGIC_NAME = "__and__";
const std::string Module::OR_MAGIC_NAME = "__or__";
const std::string Module::XOR_MAGIC_NAME = "__xor__";

const std::string Module::IADD_MAGIC_NAME = "__iadd__";
const std::string Module::ISUB_MAGIC_NAME = "__isub__";
const std::string Module::IMUL_MAGIC_NAME = "__imul__";
const std::string Module::IMATMUL_MAGIC_NAME = "__imatmul__";
const std::string Module::ITRUE_DIV_MAGIC_NAME = "__itruediv__";
const std::string Module::IFLOOR_DIV_MAGIC_NAME = "__ifloordiv__";
const std::string Module::IMOD_MAGIC_NAME = "__imod__";
const std::string Module::IPOW_MAGIC_NAME = "__ipow__";
const std::string Module::ILSHIFT_MAGIC_NAME = "__ilshift__";
const std::string Module::IRSHIFT_MAGIC_NAME = "__irshift__";
const std::string Module::IAND_MAGIC_NAME = "__iand__";
const std::string Module::IOR_MAGIC_NAME = "__ior__";
const std::string Module::IXOR_MAGIC_NAME = "__ixor__";

const std::string Module::RADD_MAGIC_NAME = "__radd__";
const std::string Module::RSUB_MAGIC_NAME = "__rsub__";
const std::string Module::RMUL_MAGIC_NAME = "__rmul__";
const std::string Module::RMATMUL_MAGIC_NAME = "__rmatmul__";
const std::string Module::RTRUE_DIV_MAGIC_NAME = "__rtruediv__";
const std::string Module::RFLOOR_DIV_MAGIC_NAME = "__rfloordiv__";
const std::string Module::RMOD_MAGIC_NAME = "__rmod__";
const std::string Module::RPOW_MAGIC_NAME = "__rpow__";
const std::string Module::RLSHIFT_MAGIC_NAME = "__rlshift__";
const std::string Module::RRSHIFT_MAGIC_NAME = "__rrshift__";
const std::string Module::RAND_MAGIC_NAME = "__rand__";
const std::string Module::ROR_MAGIC_NAME = "__ror__";
const std::string Module::RXOR_MAGIC_NAME = "__rxor__";

const std::string Module::INT_MAGIC_NAME = "__int__";
const std::string Module::FLOAT_MAGIC_NAME = "__float__";
const std::string Module::BOOL_MAGIC_NAME = "__bool__";
const std::string Module::STR_MAGIC_NAME = "__str__";
const std::string Module::REPR_MAGIC_NAME = "__repr__";

const std::string Module::GETITEM_MAGIC_NAME = "__getitem__";
const std::string Module::SETITEM_MAGIC_NAME = "__setitem__";
const std::string Module::ITER_MAGIC_NAME = "__iter__";
const std::string Module::LEN_MAGIC_NAME = "__len__";

const std::string Module::NEW_MAGIC_NAME = "__new__";
const std::string Module::INIT_MAGIC_NAME = "__init__";

const char Module::NodeId = 0;

Module::Module(const std::string& name) : AcceptorExtend(name) {
  main_func = std::make_unique<BodiedFunc>("main");
  main_func->realize(cast<Types::FuncType>(unsafe_get_dummy_func_type()), {});
  main_func->set_module(this);
  main_func->set_replaceable(false);
  arg_var = std::make_unique<Var>(unsafe_get_array_type(get_string_type()),
                                  /*global=*/true,
                                  /*external=*/false, ".argv");
  arg_var->set_module(this);
  arg_var->set_replaceable(false);
}

void Module::parse_code(const std::string& code) { cache->parse_code(code); }

auto Module::get_or_realize_method(Types::Type* parent,
                                   const std::string& method_name,
                                   std::vector<Types::Type*> args,
                                   std::vector<Types::Generic> generics)
    -> Func* {
  auto cls =
      std::const_pointer_cast<Type::Type>(parent->get_ast_type())->get_class();
  auto method =
      cache->find_method(cls.get(), method_name, generate_dummy_names(args));
  if (!method)
    return nullptr;
  try {
    return cache->realize_function(method, translate_args(cache, args),
                                   translate_generics(cache, generics), cls);
  } catch (const ParserException& e) {
    for (int i = 0; i < e.messages.size(); i++)
      LOG_IR("get_or_realize_method parser error at {}: {}", e.locations[i],
             e.messages[i]);
    return nullptr;
  }
}

auto Module::get_or_realize_func(const std::string& func_name,
                                 std::vector<Types::Type*> args,
                                 std::vector<Types::Generic> generics,
                                 const std::string& module) -> Func* {
  auto fq_name = module.empty()
                     ? func_name
                     : fmt::format(FMT_STRING("{}.{}"), module, func_name);
  auto func = cache->find_function(fq_name);
  if (!func)
    return nullptr;
  auto arg = translate_args(cache, args);
  auto gens = translate_generics(cache, generics);
  try {
    return cache->realize_function(func, arg, gens);
  } catch (const ParserException& e) {
    for (int i = 0; i < e.messages.size(); i++)
      LOG_IR("get_or_realize_func parser error at {}: {}", e.locations[i],
             e.messages[i]);
    return nullptr;
  }
}

auto Module::get_or_realize_type(const std::string& type_name,
                                 std::vector<Types::Generic> generics,
                                 const std::string& module) -> Types::Type* {
  auto fq_name = module.empty()
                     ? type_name
                     : fmt::format(FMT_STRING("{}.{}"), module, type_name);
  auto type = cache->find_class(fq_name);
  if (!type)
    return nullptr;
  try {
    return cache->realize_type(type, translate_generics(cache, generics));
  } catch (const ParserException& e) {
    for (int i = 0; i < e.messages.size(); i++)
      LOG_IR("get_or_realize_type parser error at {}: {}", e.locations[i],
             e.messages[i]);
    return nullptr;
  }
}

auto Module::get_void_type() -> Types::Type* {
  if (auto* r_val = get_type(VOID_NAME))
    return r_val;
  return Nr<Types::VoidType>();
}

auto Module::get_bool_type() -> Types::Type* {
  if (auto* r_val = get_type(BOOL_NAME))
    return r_val;
  return Nr<Types::BoolType>();
}

auto Module::get_byte_type() -> Types::Type* {
  if (auto* r_val = get_type(BYTE_NAME))
    return r_val;
  return Nr<Types::ByteType>();
}

auto Module::get_int_type() -> Types::Type* {
  if (auto* r_val = get_type(INT_NAME))
    return r_val;
  return Nr<Types::IntType>();
}

auto Module::get_float_type() -> Types::Type* {
  if (auto* r_val = get_type(FLOAT_NAME))
    return r_val;
  return Nr<Types::FloatType>();
}

auto Module::get_float32_type() -> Types::Type* {
  if (auto* r_val = get_type(FLOAT32_NAME))
    return r_val;
  return Nr<Types::Float32Type>();
}

auto Module::get_float16_type() -> Types::Type* {
  if (auto* r_val = get_type(FLOAT16_NAME))
    return r_val;
  return Nr<Types::Float16Type>();
}

auto Module::get_bfloat16_type() -> Types::Type* {
  if (auto* r_val = get_type(BFLOAT16_NAME))
    return r_val;
  return Nr<Types::BFloat16Type>();
}

auto Module::get_float128_type() -> Types::Type* {
  if (auto* r_val = get_type(FLOAT128_NAME))
    return r_val;
  return Nr<Types::Float128Type>();
}

auto Module::get_string_type() -> Types::Type* {
  if (auto* r_val = get_type(STRING_NAME))
    return r_val;
  return Nr<Types::RecordType>(
      STRING_NAME,
      std::vector<Types::Type*>{get_int_type(),
                                unsafe_get_pointer_type(get_byte_type())},
      std::vector<std::string>{"len", "ptr"});
}

auto Module::get_pointer_type(Types::Type* base) -> Types::Type* {
  return get_or_realize_type("Ptr", {base});
}

auto Module::get_array_type(Types::Type* base) -> Types::Type* {
  return get_or_realize_type("Array", {base});
}

auto Module::get_generator_type(Types::Type* base) -> Types::Type* {
  return get_or_realize_type("Generator", {base});
}

auto Module::get_optional_type(Types::Type* base) -> Types::Type* {
  return get_or_realize_type("Optional", {base});
}

auto Module::get_func_type(Types::Type* r_type,
                           std::vector<Types::Type*> arg_types, bool variadic)
    -> Types::Type* {
  auto args = translate_args(cache, arg_types);
  args[0] = std::make_shared<Pud::Type::LinkType>(r_type->get_ast_type());
  auto* result = cache->make_function(args);
  if (variadic) {
    // Type checker types have no concept of variadic functions, so we will
    // create a new IR type here with the same AST type.
    auto* f = cast<Types::FuncType>(result);
    result =
        unsafe_get_func_type(f->get_name() + "$variadic", f->get_return_type(),
                             std::vector<Types::Type*>(f->begin(), f->end()),
                             /*variadic=*/true);
    result->set_ast_type(f->get_ast_type());
  }
  return result;
}

auto Module::get_intn_type(unsigned int len, bool sign) -> Types::Type* {
  return get_or_realize_type(sign ? "Int" : "UInt", {len});
}

auto Module::get_vector_type(unsigned count, Types::Type* base)
    -> Types::Type* {
  return get_or_realize_type("Vec", {base, count});
}

auto Module::get_tuple_type(std::vector<Types::Type*> args) -> Types::Type* {
  std::vector<Type::TypePtr> arg_types;
  for (auto* t : args) {
    seqassertn(t->get_ast_type(), "{} must have an ast type", *t);
    arg_types.push_back(t->get_ast_type());
  }
  return cache->make_tuple(arg_types);
}

auto Module::get_union_type(std::vector<Types::Type*> types) -> Types::Type* {
  std::vector<Type::TypePtr> arg_types;
  for (auto* t : types) {
    seqassertn(t->get_ast_type(), "{} must have an ast type", *t);
    arg_types.push_back(t->get_ast_type());
  }
  return cache->make_union(arg_types);
}

auto Module::get_none_type() -> Types::Type* {
  return get_or_realize_type("NoneType");
}

auto Module::get_int(int64_t v) -> Value* {
  return Nr<IntConst>(v, get_int_type());
}

auto Module::get_float(double v) -> Value* {
  return Nr<FloatConst>(v, get_float_type());
}

auto Module::get_bool(bool v) -> Value* {
  return Nr<BoolConst>(v, get_bool_type());
}

auto Module::get_string(std::string v) -> Value* {
  return Nr<StringConst>(std::move(v), get_string_type());
}

auto Module::unsafe_get_dummy_func_type() -> Types::Type* {
  return unsafe_get_func_type("<internal_func_type>", get_void_type(), {});
}

auto Module::unsafe_get_pointer_type(Types::Type* base) -> Types::Type* {
  auto name = Types::PointerType::get_instance_name(base);
  if (auto* r_val = get_type(name))
    return r_val;
  return Nr<Types::PointerType>(base);
}

auto Module::unsafe_get_array_type(Types::Type* base) -> Types::Type* {
  auto name = fmt::format(FMT_STRING(".Array[{}]"), base->reference_string());
  if (auto* r_val = get_type(name))
    return r_val;
  std::vector<Types::Type*> members = {get_int_type(),
                                       unsafe_get_pointer_type(base)};
  std::vector<std::string> names = {"len", "ptr"};
  return Nr<Types::RecordType>(name, members, names);
}

auto Module::unsafe_get_generator_type(Types::Type* base) -> Types::Type* {
  auto name = Types::GeneratorType::get_instance_name(base);
  if (auto* r_val = get_type(name))
    return r_val;
  return Nr<Types::GeneratorType>(base);
}

auto Module::unsafe_get_optional_type(Types::Type* base) -> Types::Type* {
  auto name = Types::OptionalType::get_instance_name(base);
  if (auto* r_val = get_type(name))
    return r_val;
  return Nr<Types::OptionalType>(base);
}

auto Module::unsafe_get_func_type(const std::string& name, Types::Type* r_type,
                                  std::vector<Types::Type*> arg_types,
                                  bool variadic) -> Types::Type* {
  if (auto* r_val = get_type(name))
    return r_val;
  return Nr<Types::FuncType>(name, r_type, std::move(arg_types), variadic);
}

auto Module::unsafe_get_membered_type(const std::string& name, bool ref)
    -> Types::Type* {
  auto* r_val = get_type(name);

  if (!r_val) {
    if (ref) {
      auto content_name = name + ".contents";
      auto* record = get_type(content_name);
      if (!record) {
        record = Nr<Types::RecordType>(content_name);
      }
      r_val = Nr<Types::RefType>(name, cast<Types::RecordType>(record));
    } else {
      r_val = Nr<Types::RecordType>(name);
    }
  }

  return r_val;
}

auto Module::unsafe_get_intn_type(unsigned int len, bool sign) -> Types::Type* {
  auto name = Types::IntNType::get_instance_name(len, sign);
  if (auto* r_val = get_type(name))
    return r_val;
  return Nr<Types::IntNType>(len, sign);
}

auto Module::unsafe_get_vector_type(unsigned int count, Types::Type* base)
    -> Types::Type* {
  auto* primitive = cast<Types::PrimitiveType>(base);
  auto name = Types::VectorType::get_instance_name(count, primitive);
  if (auto* r_val = get_type(name))
    return r_val;
  seqassertn(primitive, "base type must be a primitive type");
  return Nr<Types::VectorType>(count, primitive);
}

auto Module::unsafe_get_union_type(const std::vector<Types::Type*>& types)
    -> Types::Type* {
  auto name = Types::UnionType::get_instance_name(types);
  if (auto* r_val = get_type(name))
    return r_val;
  return Nr<Types::UnionType>(types);
}

}  // namespace Pud::IR