/* -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- */

// Author: Kevin P. Barry [ta0kira@gmail.com]

#ifndef RETURNS_HPP_
#define RETURNS_HPP_

#include "boxed.hpp"
#include "pooled.hpp"


namespace zeolite_internal {

template<>
class PoolManager<BoxedValue> {
  using PoolEntry = PoolStorage<BoxedValue>;
  using Managed = PoolEntry::Managed;

  template<class> friend class PoolArray;
  template<class> friend struct PoolCache;

  static PoolEntry* Take(int size);
  static void Return(PoolEntry* storage, int size);

  static thread_local PoolCache<BoxedValue> cache3_;
};

}  // namespace zeolite_internal


class ReturnTuple {
 public:
  constexpr ReturnTuple() : data_() {}

  ReturnTuple(int size) : data_(size-1) {}

  template<class T, class...Ts>
  explicit ReturnTuple(T first, Ts... returns)
    : first_(std::move(first)), data_(sizeof...(Ts)) {
    data_.Init(std::move(returns)...);
  }

  ReturnTuple(ReturnTuple&&) = default;

  void TransposeFrom(ReturnTuple&& other);

  // NOTE: This will never be 0.
  int Size() const;
  BoxedValue& At(int pos);
  const BoxedValue& At(int pos) const;

 private:
  ReturnTuple(const ReturnTuple&) = delete;
  ReturnTuple& operator =(ReturnTuple&&) = delete;
  ReturnTuple& operator =(const ReturnTuple&) = delete;
  void* operator new(std::size_t size) = delete;

  BoxedValue first_;
  zeolite_internal::PoolArray<BoxedValue> data_;
};

#endif  // RETURNS_HPP_
