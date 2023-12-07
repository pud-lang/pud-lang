#ifndef PUD_IR_UTIL_ITERATORS_H
#define PUD_IR_UTIL_ITERATORS_H

#include <iterator>
#include <memory>
#include <type_traits>

namespace Pud::IR::Util {

// 迭代器适配器用于将函数应用到底层迭代器的结果上，
// 从而实现对迭代元素的自定义处理。
template <typename It, typename DereferenceFunc, typename MemberFunc>
struct function_iterator_adaptor {
  It internal;
  DereferenceFunc d;
  MemberFunc m;

  using iterator_category = std::input_iterator_tag;
  using value_type = std::remove_reference_t<decltype(d(*internal))>;
  using reference = void;
  using pointer = void;
  using difference_type = typename std::iterator_traits<It>::difference_type;

  function_iterator_adaptor(It internal, DereferenceFunc&& d, MemberFunc&& m)
      : internal(std::move(internal)), d(std::move(d)), m(std::move(m)) {}

  auto operator*() -> decltype(auto) { return d(*internal); }
  auto operator->() -> decltype(auto) { return m(*internal); }

  auto operator++() -> function_iterator_adaptor& {
    internal++;
    return *this;
  }
  auto operator++(int) -> function_iterator_adaptor {
    function_iterator_adaptor<It, DereferenceFunc, MemberFunc> copy(*this);
    internal++;
    return copy;
  }

  template <typename OtherIt, typename OtherDereferenceFunc,
            typename OtherMemberFunc>
  auto operator==(const function_iterator_adaptor<OtherIt, OtherDereferenceFunc,
                                                  OtherMemberFunc>& other) const
      -> bool {
    return other.internal == internal;
  }

  template <typename OtherIt, typename OtherDereferenceFunc,
            typename OtherMemberFunc>
  auto operator!=(const function_iterator_adaptor<OtherIt, OtherDereferenceFunc,
                                                  OtherMemberFunc>& other) const
      -> bool {
    return other.internal != internal;
  }
};

// 创建一个解引用适配器，将底层迭代器的值解引用。
template <typename It>
auto dereference_adaptor(It it) {
  auto f = [](const auto& v) -> auto& { return *v; };
  auto m = [](const auto& v) -> auto { return v.get(); };
  return function_iterator_adaptor<It, decltype(f), decltype(m)>(
      it, std::move(f), std::move(m));
}

// 创建一个适配器，返回底层迭代器元素的原始指针。
template <typename It>
auto raw_ptr_adaptor(It it) {
  auto f = [](auto& v) -> auto* { return v.get(); };
  auto m = [](auto& v) -> auto* { return v.get(); };
  return function_iterator_adaptor<It, decltype(f), decltype(m)>(
      it, std::move(f), std::move(m));
}

// 类似于 raw_ptr_adaptor，但返回常量指针。
template <typename It>
auto const_raw_ptr_adaptor(It it) {
  auto f = [](auto& v) -> const auto* { return v.get(); };
  auto m = [](auto& v) -> const auto* { return v.get(); };
  return function_iterator_adaptor<It, decltype(f), decltype(m)>(
      it, std::move(f), std::move(m));
}

// 用于 map 类型的迭代器，返回迭代器元素的键（key）。
template <typename It>
auto map_key_adaptor(It it) {
  auto f = [](auto& v) -> auto& { return v.first; };
  auto m = [](auto& v) -> auto& { return v.first; };
  return function_iterator_adaptor<It, decltype(f), decltype(m)>(
      it, std::move(f), std::move(m));
}

// 类似于 map_key_adaptor，但返回常量引用。
template <typename It>
auto const_map_key_adaptor(It it) {
  auto f = [](auto& v) -> const auto& { return v.first; };
  auto m = [](auto& v) -> const auto& { return v.first; };
  return function_iterator_adaptor<It, decltype(f), decltype(m)>(
      it, std::move(f), std::move(m));
}

}  // namespace Pud::IR::Util

#endif  // PUD_IR_UTIL_ITERATORS_H