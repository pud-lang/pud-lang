#ifndef PUD_IR_FLOW_H
#define PUD_IR_FLOW_H

#include <list>
#include <vector>

#include "Pud/IR/Base.h"
#include "Pud/IR/Value.h"
#include "Pud/IR/Var.h"
#include "Pud/Transform/Parallel/Schedule.h"

namespace Pud::IR {

// 所有控制流结构的基类
class Flow : public AcceptorExtend<Flow, Value> {
 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

 protected:
  // 返回流的类型。由于 Flow 是一个抽象类，这个函数将在子类中具体实现。
  auto do_get_type() const -> Types::Type* final;
};

// 表示一系列控制流程或指令的组合。
class SeriesFlow : public AcceptorExtend<SeriesFlow, Flow> {
 private:
  // 存储一系列控制流程或指令。
  std::list<Value*> series;

 public:
  static const char NodeId;

  using AcceptorExtend::AcceptorExtend;

  auto begin() { return series.begin(); }
  auto end() { return series.end(); }
  auto begin() const { return series.begin(); }
  auto end() const { return series.end(); }

  auto front() -> Value* { return series.front(); }
  auto back() -> Value* { return series.back(); }
  auto front() const -> const Value* { return series.front(); }
  auto back() const -> const Value* { return series.back(); }

  // 向序列中添加新的流程或指令。
  template <typename It>
  auto insert(It pos, Value* v) {
    return series.insert(pos, v);
  }

  void push_back(Value* f) { series.push_back(f); }

  // 从序列中移除流程或指令。
  template <typename It>
  auto erase(It pos) {
    return series.erase(pos);
  }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override {
    return std::vector<Value*>(series.begin(), series.end());
  }
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

// 表示一个 while 循环。
class WhileFlow : public AcceptorExtend<WhileFlow, Flow> {
 private:
  // 循环的条件。
  Value* cond;
  // 循环的主体。
  Value* body;

 public:
  static const char NodeId;

  WhileFlow(Value* cond, Flow* body, std::string name = "")
      : AcceptorExtend(std::move(name)), cond(cond), body(body) {}

  // 获取和设置循环条件。
  auto get_cond() -> Value* { return cond; }
  auto get_cond() const -> const Value* { return cond; }
  void set_cond(Value* c) { cond = c; }

  // 获取和设置循环主体。
  auto get_body() -> Flow* { return cast<Flow>(body); }
  auto get_body() const -> const Flow* { return cast<Flow>(body); }
  void set_body(Flow* f) { body = f; }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override {
    return {cond, body};
  }

  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

// 表示一个 for 循环。
class ForFlow : public AcceptorExtend<ForFlow, Flow> {
 private:
  // 循环的迭代器。
  Value* iter;
  // 循环的主体。
  Value* body;
  // 循环中使用的变量。
  Var* var;
  // 并行循环调度信息（可选）。
  std::unique_ptr<Transform::Parallel::OMPSched> schedule;

 public:
  static const char NodeId;

  ForFlow(Value* iter, Flow* body, Var* var,
          std::unique_ptr<Transform::Parallel::OMPSched> schedule = {},
          std::string name = "")
      : AcceptorExtend(std::move(name)),
        iter(iter),
        body(body),
        var(var),
        schedule(std::move(schedule)) {}

  auto get_iter() -> Value* { return iter; }
  auto get_iter() const -> const Value* { return iter; }
  void set_iter(Value* f) { iter = f; }

  auto get_body() -> Flow* { return cast<Flow>(body); }
  auto get_body() const -> const Flow* { return cast<Flow>(body); }
  void set_body(Flow* f) { body = f; }

  auto get_var() -> Var* { return var; }
  auto get_var() const -> const Var* { return var; }
  void set_var(Var* c) { var = c; }

  auto is_parallel() const -> bool { return bool(schedule); }

  void set_parallel(bool a = true) {
    if (a) {
      schedule = std::make_unique<Transform::Parallel::OMPSched>();
    } else {
      schedule = std::unique_ptr<Transform::Parallel::OMPSched>();
    }
  }

  auto get_schedule() -> Transform::Parallel::OMPSched* {
    return schedule.get();
  }
  auto get_schedule() const -> const Transform::Parallel::OMPSched* {
    return schedule.get();
  }
  void set_schedule(std::unique_ptr<Transform::Parallel::OMPSched> s) {
    schedule = std::move(s);
  }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;

  auto do_get_used_variables() const -> std::vector<Var*> override {
    return {var};
  }
  auto do_replace_used_variable(id_t id, Var* new_var) -> int override;
};

// 表示一个传统的 for 循环，具有起始值、步长和结束值。
class ImperativeForFlow : public AcceptorExtend<ImperativeForFlow, Flow> {
 private:
  // 起始值
  Value* start;
  // 步长
  int64_t step;
  // 结束值
  Value* end;
  // 循环的主体
  Value* body;
  // 循环中使用的变量
  Var* var;
  // 并行循环调度信息（可选）。
  std::unique_ptr<Transform::Parallel::OMPSched> schedule;

 public:
  static const char NodeId;

  ImperativeForFlow(
      Value* start, int64_t step, Value* end, Flow* body, Var* var,
      std::unique_ptr<Transform::Parallel::OMPSched> schedule = {},
      std::string name = "")
      : AcceptorExtend(std::move(name)),
        start(start),
        step(step),
        end(end),
        body(body),
        var(var),
        schedule(std::move(schedule)) {}

  auto get_start() const -> Value* { return start; }
  void set_start(Value* val) { start = val; }

  auto get_step() const -> int64_t { return step; }
  void set_step(int64_t val) { step = val; }

  auto get_end() const -> Value* { return end; }
  void set_end(Value* val) { end = val; }

  auto get_body() -> Flow* { return cast<Flow>(body); }
  auto get_body() const -> const Flow* { return cast<Flow>(body); }
  void set_body(Flow* f) { body = f; }

  auto get_var() -> Var* { return var; }
  auto get_var() const -> const Var* { return var; }
  void set_var(Var* c) { var = c; }

  auto is_parallel() const -> bool { return bool(schedule); }

  void set_parallel(bool a = true) {
    if (a) {
      schedule = std::make_unique<Transform::Parallel::OMPSched>();
    } else {
      schedule = std::unique_ptr<Transform::Parallel::OMPSched>();
    }
  }

  auto get_schedule() -> Transform::Parallel::OMPSched* {
    return schedule.get();
  }
  auto get_schedule() const -> const Transform::Parallel::OMPSched* {
    return schedule.get();
  }
  void set_schedule(std::unique_ptr<Transform::Parallel::OMPSched> s) {
    schedule = std::move(s);
  }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;

  auto do_get_used_variables() const -> std::vector<Var*> override {
    return {var};
  }
  auto do_replace_used_variable(id_t id, Var* new_var) -> int override;
};

// 表示一个 if 语句。
class IfFlow : public AcceptorExtend<IfFlow, Flow> {
 private:
  // 条件表达式。
  Value* cond;
  Value* true_branch;
  Value* false_branch;

 public:
  static const char NodeId;

  IfFlow(Value* cond, Flow* true_branch, Flow* false_branch = nullptr,
         std::string name = "")
      : AcceptorExtend(std::move(name)),
        cond(cond),
        true_branch(true_branch),
        false_branch(false_branch) {}

  auto get_true_branch() -> Flow* { return cast<Flow>(true_branch); }
  auto get_true_branch() const -> const Flow* {
    return cast<Flow>(true_branch);
  }
  void setTrueBranch(Flow* f) { true_branch = f; }

  auto get_false_branch() -> Flow* { return cast<Flow>(false_branch); }
  auto get_false_branch() const -> const Flow* {
    return cast<Flow>(false_branch);
  }
  void set_false_branch(Flow* f) { false_branch = f; }

  auto get_cond() -> Value* { return cond; }
  auto get_cond() const -> const Value* { return cond; }
  void set_cond(Value* c) { cond = c; }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

// 表示一个 try-catch 语句。
class TryCatchFlow : public AcceptorExtend<TryCatchFlow, Flow> {
 public:
  // Catch 类代表 try-catch 语句中的一个 catch 子句。
  // 每个 catch 子句都有可能有一个异常处理程序、一个可选的捕获类型
  // 和一个可选的变量。
  // 这些组件使得 Catch 类能够表示复杂的异常处理逻辑。
  class Catch {
   private:
    // 异常处理程序，实际上是一个 Flow
    // 类型的对象，代表在捕获到异常时要执行的代码。
    Value* handler;
    // 可选的，表示捕获的异常类型。如果为 nullptr，
    // 表示这是一个捕获所有类型异常的 catch 子句。
    Types::Type* type;
    // 可选的，表示与异常类型关联的变量。
    // 这个变量通常用于访问或处理捕获的异常对象。
    Var* catch_var;

   public:
    explicit Catch(Flow* handler, Types::Type* type = nullptr,
                   Var* catch_var = nullptr)
        : handler(handler), type(type), catch_var(catch_var) {}

    auto get_handler() -> Flow* { return cast<Flow>(handler); }
    auto get_handler() const -> const Flow* { return cast<Flow>(handler); }
    void set_handler(Flow* h) { handler = h; }

    auto get_type() const -> Types::Type* { return type; }
    void set_type(Types::Type* t) { type = t; }

    auto get_var() -> Var* { return catch_var; }
    auto get_var() const -> const Var* { return catch_var; }
    void set_var(Var* v) { catch_var = v; }
  };

 private:
  // catch 子句的列表。
  std::list<Catch> catches;

  // try 主体
  Value* body;
  // finally 块
  Value* finally;

 public:
  static const char NodeId;

  explicit TryCatchFlow(Flow* body, Flow* finally = nullptr,
                        std::string name = "")
      : AcceptorExtend(std::move(name)), body(body), finally(finally) {}

  auto get_body() -> Flow* { return cast<Flow>(body); }
  auto get_body() const -> const Flow* { return cast<Flow>(body); }
  void set_body(Flow* f) { body = f; }

  auto get_finally() -> Flow* { return cast<Flow>(finally); }
  auto get_finally() const -> const Flow* { return cast<Flow>(finally); }
  void set_finally(Flow* f) { finally = f; }

  auto begin() { return catches.begin(); }
  auto end() { return catches.end(); }
  auto begin() const { return catches.begin(); }
  auto end() const { return catches.end(); }

  auto front() -> auto& { return catches.front(); }
  auto back() -> auto& { return catches.back(); }
  auto front() const -> auto& { return catches.front(); }
  auto back() const -> auto& { return catches.back(); }

  template <typename It>
  auto insert(It pos, Catch v) {
    return catches.insert(pos, v);
  }

  void push_back(Catch v) { catches.push_back(v); }

  template <typename... Args>
  void emplace_back(Args&&... args) {
    catches.emplace_back(std::forward<Args>(args)...);
  }

  template <typename It>
  auto erase(It pos) {
    return catches.erase(pos);
  }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;

  auto do_get_used_types() const -> std::vector<Types::Type*> override;
  auto do_replace_used_type(const std::string& name, Types::Type* new_type)
      -> int override;

  auto do_get_used_variables() const -> std::vector<Var*> override;
  auto do_replace_used_variable(id_t id, Var* new_var) -> int override;
};

// 代表一个管道流程，这是一种在编译器中间表示中使用的控制流结构。
// PipelineFlow 类使用嵌套的 Stage 类来表示管道中的各个阶段。
class PipelineFlow : public AcceptorExtend<PipelineFlow, Flow> {
 public:
  // 代表管道中的单个阶段
  class Stage {
   private:
    // 被调用的函数或生成器。
    Value* callee;
    // 函数调用的参数，其中 nullptr 代表前一个阶段的输出。
    std::vector<Value*> args;
    // 标志该阶段是否为生成器。
    bool generator;
    // 标志该阶段是否并行执行。
    bool parallel;

   public:
    Stage(Value* callee, std::vector<Value*> args, bool generator,
          bool parallel)
        : callee(callee),
          args(std::move(args)),
          generator(generator),
          parallel(parallel) {}

    auto begin() { return args.begin(); }
    auto end() { return args.end(); }
    auto begin() const { return args.begin(); }
    auto end() const { return args.end(); }

    auto front() -> Value* { return args.front(); }
    auto back() -> Value* { return args.back(); }
    auto front() const -> const Value* { return args.front(); }
    auto back() const -> const Value* { return args.back(); }

    template <typename It>
    auto insert(It pos, Value* v) {
      return args.insert(pos, v);
    }
    void push_back(Value* v) { args.push_back(v); }

    template <typename It>
    auto erase(It pos) {
      return args.erase(pos);
    }

    void set_callee(Value* c) { callee = c; }
    auto get_callee() -> Value* { return callee; }
    auto get_callee() const -> const Value* { return callee; }

    void set_generator(bool v = true) { generator = v; }
    auto is_generator() const -> bool { return generator; }
    void set_parallel(bool v = true) { parallel = v; }
    auto is_parallel() const -> bool { return parallel; }
    auto get_output_type() const -> Types::Type*;
    auto get_output_element_type() const -> Types::Type*;

    friend class PipelineFlow;
  };

 private:
  // 存储管道中的所有阶段。
  std::list<Stage> stages;

 public:
  static const char NodeId;

  explicit PipelineFlow(std::vector<Stage> stages = {}, std::string name = "")
      : AcceptorExtend(std::move(name)), stages(stages.begin(), stages.end()) {}

  auto begin() { return stages.begin(); }
  auto end() { return stages.end(); }
  auto begin() const { return stages.begin(); }
  auto end() const { return stages.end(); }

  auto front() -> Stage& { return stages.front(); }
  auto back() -> Stage& { return stages.back(); }
  auto front() const -> const Stage& { return stages.front(); }
  auto back() const -> const Stage& { return stages.back(); }

  template <typename It>
  auto insert(It pos, Stage v) {
    return stages.insert(pos, v);
  }
  void push_back(Stage v) { stages.push_back(std::move(v)); }

  template <typename It>
  auto erase(It pos) {
    return stages.erase(pos);
  }

  template <typename... Args>
  void emplace_back(Args&&... args) {
    stages.emplace_back(std::forward<Args>(args)...);
  }

 protected:
  auto do_get_used_values() const -> std::vector<Value*> override;
  auto do_replace_used_value(id_t id, Value* new_value) -> int override;
};

}  // namespace Pud::IR

#endif  // PUD_IR_FLOW_H