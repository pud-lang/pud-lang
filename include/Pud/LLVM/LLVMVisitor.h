#ifndef PUD_LLVM_LLVM_VISITOR_H
#define PUD_LLVM_LLVM_VISITOR_H

#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "Pud/Common/Common.h"
#include "Pud/DSL/Plugin.h"
#include "Pud/IR/IR.h"
#include "Pud/IR/PyExtension.h"
#include "Pud/LLVM/LLVM.h"

namespace Pud::IR {

class LLVMVisitor : public Util::ConstVisitor {
 private:
  struct CoroData {
    /// Coroutine promise (where yielded values are stored)
    llvm::Value* promise;
    /// Coroutine handle
    llvm::Value* handle;
    /// Coroutine cleanup block
    llvm::BasicBlock* cleanup;
    /// Coroutine suspend block
    llvm::BasicBlock* suspend;
    /// Coroutine exit block
    llvm::BasicBlock* exit;

    void reset() { promise = handle = cleanup = suspend = exit = nullptr; }
  };

  struct NestableData {
    int sequence_number;

    NestableData() : sequence_number(-1) {}
  };

  struct LoopData : NestableData {
    /// Block to branch to in case of "break"
    llvm::BasicBlock* break_block;
    /// Block to branch to in case of "continue"
    llvm::BasicBlock* continue_block;
    /// Loop id
    id_t loop_id;

    LoopData(llvm::BasicBlock* break_block, llvm::BasicBlock* continue_block,
             id_t loop_id)
        : NestableData(),
          break_block(break_block),
          continue_block(continue_block),
          loop_id(loop_id) {}

    void reset() { break_block = continue_block = nullptr; }
  };

  struct TryCatchData : NestableData {
    /// Possible try-catch states when reaching finally block
    enum State { NOT_THROWN = 0, THROWN, CAUGHT, RETURN, BREAK, CONTINUE };
    /// Exception block
    llvm::BasicBlock* exception_block;
    /// Exception route block
    llvm::BasicBlock* exception_route_block;
    /// Finally start block
    llvm::BasicBlock* finally_block;
    /// Try-catch catch types
    std::vector<Types::Type*> catch_types;
    /// Try-catch handlers, corresponding to catch types
    std::vector<llvm::BasicBlock*> handlers;
    /// Exception state flag (see "State")
    llvm::Value* exc_flag;
    /// Storage for caught exception
    llvm::Value* catch_store;
    /// How far to delegate up the finally chain
    llvm::Value* delegate_depth;
    /// Storage for postponed return
    llvm::Value* ret_store;
    /// Loop being manipulated
    llvm::Value* loop_sequence;

    TryCatchData()
        : NestableData(),
          exception_block(nullptr),
          exception_route_block(nullptr),
          finally_block(nullptr),
          catch_types(),
          handlers(),
          exc_flag(nullptr),
          catch_store(nullptr),
          delegate_depth(nullptr),
          ret_store(nullptr),
          loop_sequence(nullptr) {}

    void reset() {
      exception_block = exception_route_block = finally_block = nullptr;
      catch_types.clear();
      handlers.clear();
      exc_flag = catch_store = delegate_depth = loop_sequence = nullptr;
    }
  };

  struct CatchData : NestableData {
    llvm::Value* exception;
    llvm::Value* type_id;
  };

  struct DebugInfo {
    /// LLVM debug info builder
    std::unique_ptr<llvm::DIBuilder> builder;
    /// Current compilation unit
    llvm::DICompileUnit* unit;
    /// Whether we are compiling in debug mode
    bool debug;
    /// Whether we are compiling in JIT mode
    bool jit;
    /// Whether we are compiling a standalone object/executable
    bool standalone;
    /// Whether to capture writes to stdout/stderr
    bool capture;
    /// Program command-line flags
    std::string flags;

    DebugInfo()
        : builder(),
          unit(nullptr),
          debug(false),
          jit(false),
          standalone(false),
          capture(false),
          flags() {}

    auto get_file(const std::string& path) -> llvm::DIFile*;

    void reset() {
      builder = {};
      unit = nullptr;
    }
  };

  /// LLVM context used for compilation
  std::unique_ptr<llvm::LLVMContext> context;
  /// Module we are compiling
  std::unique_ptr<llvm::Module> M;
  /// LLVM IR builder used for constructing LLVM IR
  std::unique_ptr<llvm::IRBuilder<>> B;
  /// Current function we are compiling
  llvm::Function* func;
  /// Current basic block we are compiling
  llvm::BasicBlock* block;
  /// Last compiled value
  llvm::Value* value;
  /// LLVM values corresponding to IR variables
  std::unordered_map<id_t, llvm::Value*> vars;
  /// LLVM functions corresponding to IR functions
  std::unordered_map<id_t, llvm::Function*> funcs;
  /// Coroutine data, if current function is a coroutine
  CoroData coro;
  /// Loop data stack, containing break/continue blocks
  std::vector<LoopData> loops;
  /// Try-catch data stack
  std::vector<TryCatchData> trycatch;
  /// Catch-block data stack
  std::vector<CatchData> catches;
  /// Debug information
  DebugInfo db;
  /// Plugin manager
  PluginManager* plugins;

  auto get_di_type_helper(
      Types::Type* t,
      std::unordered_map<std::string, llvm::DICompositeType*>& cache)
      -> llvm::DIType*;

  /// GC allocation functions
  auto make_alloc_func(bool atomic) -> llvm::FunctionCallee;
  /// Personality function for exception handling
  auto make_personality_func() -> llvm::FunctionCallee;
  /// Exception allocation function
  auto make_exc_alloc_func() -> llvm::FunctionCallee;
  /// Exception throw function
  auto make_throw_func() -> llvm::FunctionCallee;
  /// Program termination function
  auto make_terminate_func() -> llvm::FunctionCallee;

  // Try-catch types and utilities
  auto get_type_info_type() -> llvm::StructType*;
  auto get_pad_type() -> llvm::StructType*;
  auto get_exception_type() -> llvm::StructType*;
  auto get_type_idx_var(const std::string& name) -> llvm::GlobalVariable*;
  auto get_type_idx_var(Types::Type* catch_type) -> llvm::GlobalVariable*;
  auto get_type_idx(Types::Type* catch_type = nullptr) -> int;

  // General function helpers
  auto call(llvm::FunctionCallee callee, llvm::ArrayRef<llvm::Value*> args)
      -> llvm::Value*;
  auto make_llvm_function(const Func*) -> llvm::Function*;
  void make_yield(llvm::Value* value = nullptr, bool final_yield = false);
  auto build_llvm_code_string(const LLVMFunc*) -> std::string;
  void call_stage(const PipelineFlow::Stage* stage);
  void codegen_pipeline(const std::vector<const PipelineFlow::Stage*>& stages,
                        unsigned where = 0);

  // Loop and try-catch state
  void enter_loop(LoopData data);
  void exit_loop();
  void enter_try_catch(TryCatchData data);
  void exit_try_catch();
  void enter_catch(CatchData data);
  void exit_catch();
  auto get_innermost_try_catch() -> TryCatchData*;
  auto get_innermost_try_catch_before_loop() -> TryCatchData*;

  // Shared library setup
  void setup_global_ctor_for_shared_library();

  // Python extension setup
  auto create_py_try_catch_wrapper(llvm::Function* func) -> llvm::Function*;

  // LLVM passes
  void run_llvm_pipeline();

  auto get_var(const Var* var) -> llvm::Value*;
  void insert_var(const Var* var, llvm::Value* x) {
    vars.emplace(var->get_id(), x);
  }
  auto get_func(const Func* func) -> llvm::Function*;
  void insertFunc(const Func* func, llvm::Function* x) {
    funcs.emplace(func->get_id(), x);
  }
  auto get_dummy_void_value() -> llvm::Value* {
    return llvm::ConstantTokenNone::get(*context);
  }
  auto get_di_subprogram_for_func(const Func* x) -> llvm::DISubprogram*;
  void clear_llvm_data();

 public:
  static auto get_name_for_function(const Func* x) -> std::string;
  static auto get_name_for_var(const Var* x) -> std::string;

  static auto get_debug_name_for_variable(const Var* x) -> std::string {
    std::string name = x->get_name();
    auto pos = name.find(".");
    if (pos != 0 && pos != std::string::npos) {
      return name.substr(0, pos);
    } else {
      return name;
    }
  }

  static auto get_default_source_info() -> const SourceInfo* {
    static SourceInfo default_source_info("<internal>", 0, 0, 0);
    return &default_source_info;
  }

  static auto get_source_info(const Node* x) -> const SourceInfo* {
    if (auto* src_info = x->get_attribute<SourceInfoAttribute>()) {
      return &src_info->info;
    } else {
      return get_default_source_info();
    }
  }

  /// Constructs an LLVM visitor.
  LLVMVisitor();

  /// @return true if in debug mode, false otherwise
  auto get_debug() const -> bool { return db.debug; }
  /// Sets debug status.
  /// @param d true if debug mode
  void set_debug(bool d = true) { db.debug = d; }

  /// @return true if in JIT mode, false otherwise
  auto get_jit() const -> bool { return db.jit; }
  /// Sets JIT status.
  /// @param j true if JIT mode
  void set_jit(bool j = true) { db.jit = j; }

  /// @return true if in standalone mode, false otherwise
  auto get_standalone() const -> bool { return db.standalone; }
  /// Sets standalone status.
  /// @param s true if standalone
  void set_standalone(bool s = true) { db.standalone = s; }

  /// @return true if capturing outputs, false otherwise
  auto get_capture() const -> bool { return db.capture; }
  /// Sets capture status.
  /// @param c true to capture
  void set_capture(bool c = true) { db.capture = c; }

  /// @return program flags
  auto get_flags() const -> std::string { return db.flags; }
  /// Sets program flags.
  /// @param f flags
  void set_flags(const std::string& f) { db.flags = f; }

  auto get_context() -> llvm::LLVMContext& { return *context; }
  auto get_builder() -> llvm::IRBuilder<>& { return *B; }
  auto get_module() -> llvm::Module* { return M.get(); }
  auto get_func() -> llvm::FunctionCallee { return func; }
  auto get_block() -> llvm::BasicBlock* { return block; }
  auto get_value() -> llvm::Value* { return value; }
  auto get_vars() -> std::unordered_map<id_t, llvm::Value*>& { return vars; }
  auto get_funcs() -> std::unordered_map<id_t, llvm::Function*>& {
    return funcs;
  }
  auto get_coro() -> CoroData& { return coro; }
  auto get_loops() -> std::vector<LoopData>& { return loops; }
  auto get_try_catch() -> std::vector<TryCatchData>& { return trycatch; }
  auto get_debug_info() -> DebugInfo& { return db; }

  void set_func(llvm::Function* f) { func = f; }
  void set_block(llvm::BasicBlock* b) { block = b; }
  void set_value(llvm::Value* v) { value = v; }

  /// Registers a new global variable or function with
  /// this visitor.
  /// @param var the global variable (or function) to register
  void register_global(const Var* var);

  /// Returns a new LLVM module initialized for the host
  /// architecture.
  /// @param context LLVM context used for creating module
  /// @param src source information for the new module
  /// @return a new module
  auto make_module(llvm::LLVMContext& context, const SourceInfo* src = nullptr)
      -> std::unique_ptr<llvm::Module>;

  /// Returns the current module/LLVM context and replaces them
  /// with new, fresh ones. References to variables or functions
  /// from the old module will be included as "external".
  /// @param module the IR module
  /// @param src source information for the new module
  /// @return the current module/context, replaced internally
  auto take_module(Module* module, const SourceInfo* src = nullptr)
      -> std::pair<std::unique_ptr<llvm::Module>,
                   std::unique_ptr<llvm::LLVMContext>>;

  /// Sets current debug info based on a given node.
  /// @param node the node whose debug info to use
  void set_debug_info_for_node(const Node* node);

  /// Compiles a given IR node, updating the internal
  /// LLVM value and/or function as a result.
  /// @param node the node to compile
  void process(const Node* node);

  /// Dumps the unoptimized module IR to a file.
  /// @param filename name of file to write IR to
  void dump(const std::string& filename = "_dump.ll");
  /// Writes module as native object file.
  /// @param filename the .o file to write to
  /// @param pic true to write position-independent code
  void write_to_object_file(const std::string& filename, bool pic = false);
  /// Writes module as LLVM bitcode file.
  /// @param filename the .bc file to write to
  void write_to_bitcode_file(const std::string& filename);
  /// Writes module as LLVM IR file.
  /// @param filename the .ll file to write to
  void write_to_ll_file(const std::string& filename, bool optimize = true);
  /// Writes module as native executable. Invokes an
  /// external linker to generate the final executable.
  /// @param filename the file to write to
  /// @param argv0 compiler's argv[0] used to set rpath
  /// @param library whether to make a shared library
  /// @param libs library names to link
  /// @param lflags extra flags to pass linker
  void write_to_executable(const std::string& filename,
                           const std::string& argv0, bool library = false,
                           const std::vector<std::string>& libs = {},
                           const std::string& lflags = "");
  /// Writes module as Python extension object.
  /// @param pymod extension module
  /// @param filename the file to write to
  void write_to_python_extension(const PyModule& pymod,
                                 const std::string& filename);
  /// Runs optimization passes on module and writes the result
  /// to the specified file. The output type is determined by
  /// the file extension (.ll for LLVM IR, .bc for LLVM bitcode
  /// .o or .obj for object file, other for executable).
  /// @param filename name of the file to write to
  /// @param argv0 compiler's argv[0] used to set rpath
  /// @param libs library names to link to, if creating executable
  /// @param lflags extra flags to pass linker, if creating executable
  void compile(const std::string& filename, const std::string& argv0,
               const std::vector<std::string>& libs = {},
               const std::string& lflags = "");
  /// Runs optimization passes on module and executes it.
  /// @param args vector of arguments to program
  /// @param libs vector of libraries to load
  /// @param envp program environment
  void run(const std::vector<std::string>& args = {},
           const std::vector<std::string>& libs = {},
           const char* const* envp = nullptr);

  /// Gets LLVM type from IR type
  /// @param t the IR type
  /// @return corresponding LLVM type
  auto get_llvm_type(Types::Type* t) -> llvm::Type*;
  /// Gets LLVM function type from IR function type
  /// @param t the IR type (must be FuncType)
  /// @return corresponding LLVM function type
  auto get_llvm_func_type(Types::Type* t) -> llvm::FunctionType*;
  /// Gets the LLVM debug info type from the IR type
  /// @param t the IR type
  /// @return corresponding LLVM DI type
  auto get_di_type(Types::Type* t) -> llvm::DIType*;
  /// Gets loop data for a given loop id
  /// @param loop_id the IR id of the loop
  /// @return the loop's datas
  auto get_loop_data(id_t loop_id) -> LoopData*;

  /// Sets the plugin manager
  /// @param p the plugin manager
  void set_plugin_manager(PluginManager* p) { plugins = p; }
  /// @return the plugin manager
  auto get_plugin_manager() -> PluginManager* { return plugins; }

  void visit(const Module*) override;
  void visit(const BodiedFunc*) override;
  void visit(const ExternalFunc*) override;
  void visit(const InternalFunc*) override;
  void visit(const LLVMFunc*) override;
  void visit(const Var*) override;
  void visit(const VarValue*) override;
  void visit(const PointerValue*) override;

  void visit(const IntConst*) override;
  void visit(const FloatConst*) override;
  void visit(const BoolConst*) override;
  void visit(const StringConst*) override;
  void visit(const DSL::CustomConst*) override;

  void visit(const SeriesFlow*) override;
  void visit(const IfFlow*) override;
  void visit(const WhileFlow*) override;
  void visit(const ForFlow*) override;
  void visit(const ImperativeForFlow*) override;
  void visit(const TryCatchFlow*) override;
  void visit(const PipelineFlow*) override;
  void visit(const DSL::CustomFlow*) override;

  void visit(const AssignInstr*) override;
  void visit(const ExtractInstr*) override;
  void visit(const InsertInstr*) override;
  void visit(const CallInstr*) override;
  void visit(const StackAllocInstr*) override;
  void visit(const TypePropertyInstr*) override;
  void visit(const YieldInInstr*) override;
  void visit(const TernaryInstr*) override;
  void visit(const BreakInstr*) override;
  void visit(const ContinueInstr*) override;
  void visit(const ReturnInstr*) override;
  void visit(const YieldInstr*) override;
  void visit(const ThrowInstr*) override;
  void visit(const FlowInstr*) override;
  void visit(const DSL::CustomInstr*) override;
};

}  // namespace Pud::IR

#endif  // PUD_LLVM_LLVM_VISITOR_H