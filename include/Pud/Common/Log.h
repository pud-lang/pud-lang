#ifndef PUD_COMMON_LOG_H
#define PUD_COMMON_LOG_H

#include <fmt/format.h>
#include <fmt/ostream.h>
#include <fmt/ranges.h>

#include <iostream>
#include <ostream>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wgnu-zero-variadic-macro-arguments"

#define DBG(c, ...)                                                          \
  fmt::print(Pud::getLogger().log, "{}" c "\n",                            \
             std::string(size_t(2) * size_t(Pud::getLogger().level), ' '), \
             ##__VA_ARGS__)
#define LOG(c, ...) DBG(c, ##__VA_ARGS__)
#define LOG_TIME(c, ...)                                     \
  {                                                          \
    if (Pud::getLogger().flags & Pud::Logger::FLAG_TIME) \
      DBG(c, ##__VA_ARGS__);                                 \
  }
#define LOG_REALIZE(c, ...)                                     \
  {                                                             \
    if (Pud::getLogger().flags & Pud::Logger::FLAG_REALIZE) \
      DBG(c, ##__VA_ARGS__);                                    \
  }
#define LOG_TYPECHECK(c, ...)                                     \
  {                                                               \
    if (Pud::getLogger().flags & Pud::Logger::FLAG_TYPECHECK) \
      DBG(c, ##__VA_ARGS__);                                      \
  }
#define LOG_IR(c, ...)                                     \
  {                                                        \
    if (Pud::getLogger().flags & Pud::Logger::FLAG_IR) \
      DBG(c, ##__VA_ARGS__);                               \
  }
#define LOG_USER(c, ...)                                     \
  {                                                          \
    if (Pud::getLogger().flags & Pud::Logger::FLAG_USER) \
      DBG(c, ##__VA_ARGS__);                                 \
  }

#define TIME(name) Pud::Timer __timer(name)

#ifndef NDEBUG
#define seqassertn(expr, msg, ...)                             \
  ((expr) ? (void)(0)                                          \
          : Pud::assertionFailure(#expr, __FILE__, __LINE__, \
                                    fmt::format(msg, ##__VA_ARGS__)))
#define seqassert(expr, msg, ...)          \
  ((expr) ? (void)(0)                      \
          : Pud::assertionFailure(       \
                #expr, __FILE__, __LINE__, \
                fmt::format(msg " [{}]", ##__VA_ARGS__, getSrcInfo())))
#else
#define seqassertn(expr, msg, ...) ;
#define seqassert(expr, msg, ...) ;
#endif
#pragma clang diagnostic pop

namespace Pud {

void assertionFailure(const char *expr_str, const char *file, int line,
                      const std::string &msg);

struct Logger {
  static constexpr int FLAG_TIME = (1 << 0);
  static constexpr int FLAG_REALIZE = (1 << 1);
  static constexpr int FLAG_TYPECHECK = (1 << 2);
  static constexpr int FLAG_IR = (1 << 3);
  static constexpr int FLAG_USER = (1 << 4);

  int flags;
  int level;
  std::ostream &out;
  std::ostream &err;
  std::ostream &log;

  Logger() : flags(0), level(0), out(std::cout), err(std::cerr), log(std::clog) {}

  void parse(const std::string &logs);
};

Logger &getLogger();
void pushLogger();
bool popLogger();

}


#endif  // PUD_COMMON_LOG_H