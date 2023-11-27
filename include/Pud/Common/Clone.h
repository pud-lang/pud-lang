#ifndef PUD_COMMON_CLONE_H
#define PUD_COMMON_CLONE_H

#include <memory>
#include <vector>

namespace Pud {

template <typename T>
auto clone(const std::shared_ptr<T>& t) {
  return t ? t->clone() : nullptr;
}

template <typename T>
auto clone(const std::vector<T>& t) -> std::vector<T> {
  std::vector<T> v;
  for (auto& i : t) {
    v.push_back(clone(i));
  }
  return v;
}

template <typename T>
auto clone_nop(const std::vector<T>& t) -> std::vector<T> {
  std::vector<T> v;
  for (auto& i : t) {
    v.push_back(i.clone());
  }
  return v;
}

}  // namespace Pud

#endif  // PUD_COMMON_CLONE_H