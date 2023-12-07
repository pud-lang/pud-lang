#ifndef PUD_IR_UTIL_PACKS_H
#define PUD_IR_UTIL_PACKS_H

#include <vector>

namespace Pud::IR::UTIL {

// 将一组参数（通常是可变参数模板）中的特定类型的参数提取出来，
// 并将它们的地址存储在一个 std::vector 中。这是一种处理可变参
// 数模板的常用技巧，特别是在需要根据类型对参数进行分类或选择时。
//  int a = 1, b = 2;
//  double c = 3.0;
//  std::vector<int*> ints;
//
//  stripPack<int>(ints, a, b, c); // c 应该被忽略
//
//  assert(ints.size() == 2);
//  assert(*ints[0] == a);
//  assert(*ints[1] == b);

// 基本版本（两个参数）：
// 接受一个 std::vector<Desired*>& dst 和一个 Desired& first。
// 将 first 的地址添加到 dst 中。
template <typename Desired>
void strip_pack(std::vector<Desired*>& dst, Desired& first) {
  dst.push_back(&first);
}

// 递归终止版本（单参数）：
// 仅接受一个 std::vector<Desired*>& dst 参数。
// 这是递归调用的终止条件，不执行任何操作。
template <typename Desired>
void strip_pack(std::vector<Desired*>& dst) {}

// 递归版本（三个及以上参数）：
// 接受一个 std::vector<Desired*>& dst，一个 Desired& first 和一个参数包
// Args&&... args。 将 first 的地址添加到 dst 中。
// 递归地调用自身，处理参数包中的剩余参数。
template <typename Desired, typename... Args>
void strip_pack(std::vector<Desired*>& dst, Desired& first, Args&&... args) {
  dst.push_back(&first);
  strip_pack<Desired>(dst, std::forward<Args>(args)...);
}

}  // namespace Pud::IR::UTIL

#endif  // PUD_IR_UTIL_PACKS_H