#ifndef PUD_IR_UTIL_IR_TOOLS_H
#define PUD_IR_UTIL_IR_TOOLS_H

#include "Pud/IR/IR.h"

namespace Pud::IR::Util {

/// Checks whether a function has a given attribute.
/// @param func the function
/// @param attribute the attribute name
/// @return true if the function has the given attribute
auto has_attribute(const Func* func, const std::string& attribute) -> bool;

/// Checks whether a function comes from the standard library, and
/// optionally a specific module therein.
/// @param func the function
/// @param submodule module name (e.g. "std::bio"), or empty if
///                  no module check is required
/// @return true if the function is from the standard library in
///         the given module
auto is_stdlib_func(const Func* func, const std::string& submodule = "")
    -> bool;

/// Calls a function.
/// @param func the function
/// @param args vector of call arguments
/// @return call instruction with the given function and arguments
auto call(Func* func, const std::vector<Value*>& args) -> CallInstr*;

/// Checks if a value represents a call of a particular function.
/// @param value the value to check
/// @param name the function's (unmangled) name
/// @param inputs vector of input types
/// @param output output type, null for no check
/// @param method true to ensure this call is a method call
/// @return true if value is a call matching all parameters above
auto is_call_of(const Value* value, const std::string& name,
                const std::vector<Types::Type*>& inputs,
                Types::Type* output = nullptr, bool method = false) -> bool;

/// Checks if a value represents a call of a particular function.
/// @param value the value to check
/// @param name the function's (unmangled) name
/// @param numArgs argument count, negative for no check
/// @param output output type, null for no check
/// @param method true to ensure this call is a method call
/// @return true if value is a call matching all parameters above
auto is_call_of(const Value* value, const std::string& name, int num_args = -1,
                Types::Type* output = nullptr, bool method = false) -> bool;

/// Checks if a value represents a call to a magic method.
/// Magic method names start and end in "__" (two underscores).
/// @param value the value to check
/// @return true if value is a magic method call
auto is_magic_method_call(const Value* value) -> bool;

/// Constructs a new tuple.
/// @param args vector of tuple contents
/// @param M the module; inferred from elements if null
/// @return value represents a tuple with the given contents
auto make_tuple(const std::vector<Value*>& args, Module* M = nullptr) -> Value*;

/// Constructs and assigns a new variable.
/// @param x the value to assign to the new variable
/// @param flow series flow in which to assign the new variable
/// @param parent function to add the new variable to, or null for global
/// variable
/// @param prepend true to insert assignment at start of block
/// @return value containing the new variable
auto make_var(Value* x, SeriesFlow* flow, BodiedFunc* parent,
              bool prepend = false) -> VarValue*;

/// Dynamically allocates memory for the given type with the given
/// number of elements.
/// @param type the type
/// @param count integer value representing the number of elements
/// @return value representing a pointer to the allocated memory
auto alloc(Types::Type* type, Value* count) -> Value*;

/// Dynamically allocates memory for the given type with the given
/// number of elements.
/// @param type the type
/// @param count the number of elements
/// @return value representing a pointer to the allocated memory
auto alloc(Types::Type* type, int64_t count) -> Value*;

/// Builds a new series flow with the given contents. Returns
/// null if no contents are provided.
/// @param args contents of the series flow
/// @return new series flow
template <typename... Args>
auto series(Args... args) -> SeriesFlow* {
  std::vector<Value*> vals = {args...};
  if (vals.empty())
    return nullptr;
  auto* series = vals[0]->get_module()->Nr<SeriesFlow>();
  for (auto* val : vals) {
    series->push_back(val);
  }
  return series;
}

/// Checks whether the given value is a constant of the given
/// type. Note that standard "int" corresponds to the C type
/// "int64_t", which should be used here.
/// @param x the value to check
/// @return true if the value is constant
template <typename T>
auto is_const(const Value* x) -> bool {
  return is_a<TemplatedConst<T>>(x);
}

/// Checks whether the given value is a constant of the given
/// type, and that is has a particular value. Note that standard
/// "int" corresponds to the C type "int64_t", which should be used here.
/// @param x the value to check
/// @param value constant value to compare to
/// @return true if the value is constant with the given value
template <typename T>
auto is_const(const Value* x, const T& value) -> bool {
  if (auto* c = cast<TemplatedConst<T>>(x)) {
    return c->get_val() == value;
  }
  return false;
}

/// Returns the constant represented by a given value. Raises an assertion
/// error if the given value is not constant. Note that standard
/// "int" corresponds to the C type "int64_t", which should be used here.
/// @param x the (constant) value
/// @return the constant represented by the given value
template <typename T>
auto get_const(const Value* x) -> T {
  auto* c = cast<TemplatedConst<T>>(x);
  seqassertn(c, "{} is not a constant [{}]", *x, x->get_source_info());
  return c->get_val();
}

/// Gets a variable from a value.
/// @param x the value
/// @return the variable represented by the given value, or null if none
auto get_var(Value* x) -> Var*;

/// Gets a variable from a value.
/// @param x the value
/// @return the variable represented by the given value, or null if none
auto get_var(const Value* x) -> const Var*;

/// Gets a function from a value.
/// @param x the value
/// @return the function represented by the given value, or null if none
auto get_func(Value* x) -> Func*;

/// Gets a function from a value.
/// @param x the value
/// @return the function represented by the given value, or null if none
auto get_func(const Value* x) -> const Func*;

/// Loads value from a pointer.
/// @param ptr the pointer
/// @return the value pointed to by the argument
auto ptr_load(Value* ptr) -> Value*;

/// Stores a value into a pointer.
/// @param ptr the pointer
/// @param val the value to store
/// @return "__setitem__" call representing the store
auto ptr_store(Value* ptr, Value* val) -> Value*;

/// Gets value from a tuple at the given index.
/// @param tuple the tuple
/// @param index the 0-based index
/// @return tuple element at the given index
auto tuple_get(Value* tuple, unsigned index) -> Value*;

/// Stores value in a tuple at the given index. Since tuples are immutable,
/// a new instance is returned with the appropriate element replaced.
/// @param tuple the tuple
/// @param index the 0-based index
/// @param val the value to store
/// @return new tuple instance with the given value inserted
auto tuple_store(Value* tuple, unsigned index, Value* val) -> Value*;

/// Gets a bodied standard library function from a value.
/// @param x the value
/// @param name name of the function
/// @param submodule optional module to check
/// @return the standard library function (with the given name, from the given
/// submodule) represented by the given value, or null if none
auto get_stdlib_func(Value* x, const std::string& name,
                     const std::string& submodule = "") -> BodiedFunc*;

/// Gets a bodied standard library function from a value.
/// @param x the value
/// @param name name of the function
/// @param submodule optional module to check
/// @return the standard library function (with the given name, from the given
/// submodule) represented by the given value, or null if none
auto get_stdlib_func(const Value* x, const std::string& name,
                     const std::string& submodule = "") -> const BodiedFunc*;

/// Gets the return type of a function.
/// @param func the function
/// @return the return type of the given function
auto get_return_type(const Func* func) -> Types::Type*;

/// Sets the return type of a function. Argument types remain unchanged.
/// @param func the function
/// @param rType the new return type
void set_return_type(Func* func, Types::Type* r_type);

}  // namespace Pud::IR::Util

#endif  // PUD_IR_UTIL_IR_TOOLS_H