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

#ifndef TYPES_HPP_
#define TYPES_HPP_

#include <atomic>
#include <cstdint>
#include <functional>
#include <memory>
#include <tuple>
#include <vector>

#include "logging.hpp"
#include "pooled.hpp"


#define ALWAYS_PERMANENT(type) \
  type(const type&) = delete; \
  type(type&&) = delete; \
  type& operator =(const type&) = delete; \
  type& operator =(type&&) = delete;

using CollectionType = int;

using PrimInt = std::int64_t;
using PrimString = std::string;
using PrimChar = char;
using PrimCharBuffer = std::string;
using PrimFloat = double;

template<int S>
inline PrimString PrimString_FromLiteral(const char(&literal)[S]) {
  return PrimString(literal, literal + (S - 1));
}

template<class T>
using R = std::unique_ptr<T>;

template<class T>
inline R<T> R_get(T* val) { return R<T>(val); }

template<class T>
using S = std::shared_ptr<T>;

template<class T>
inline S<T> S_get(T* val) { return S<T>(val); }

template<class...Ts>
using T = std::tuple<Ts...>;

template<class...Ts>
T<Ts...> T_get(Ts... ts) { return std::make_tuple(ts...); }

template<class T>
using L = std::vector<T>;

template<class T, class...Ts>
inline L<T> L_get(Ts... ts) { return L<T>{ts...}; }

template<class T>
class LazyInit {
 public:
  LazyInit(const std::function<T()>& create)
    : initialized_(false), pending_(false), value_(), create_(create) {}

  const T& Get() {
    InitValue();
    return value_;
  }

  LazyInit& operator = (const T& value) {
    InitValue();
    value_ = value;
    return *this;
  }

 private:
  LazyInit(const LazyInit&) = delete;
  LazyInit(LazyInit&&) = delete;
  LazyInit& operator = (const LazyInit&) = delete;
  LazyInit& operator = (LazyInit&&) = delete;

  void InitValue() {
    if (pending_) {
      FAIL() << "Cycle in lazy initialization";
    }
    if (!initialized_) {
      pending_ = true;
      value_ = create_();
      pending_ = false;
      initialized_ = true;
    }
  }

  bool initialized_, pending_;
  T value_;
  const std::function<T()> create_;
};


class TypeCategory;
class TypeInstance;
class TypeValue;
class BoxedValue;
class WeakValue;

template<int N, class...Ts>
struct Params {
  using Type = typename Params<N-1, const S<TypeInstance>&, Ts...>::Type;
};

template<class...Ts>
struct Params<0, Ts...> {
  using Type = T<Ts...>;
};

template<int N, class...Ts>
struct ParamsKey {
  using Type = typename ParamsKey<N-1, const TypeInstance*, Ts...>::Type;
};

template<class...Ts>
struct ParamsKey<0, Ts...> {
  using Type = T<Ts...>;
};

template<int N, int K, class...Ts>
struct KeyFromParams {
  static typename ParamsKey<N>::Type Get(const typename Params<N>::Type& from, Ts... args) {
    return KeyFromParams<N, K+1, Ts..., const TypeInstance*>::Get(from, args..., std::get<K>(from).get());
  }
};

template<int N, class...Ts>
struct KeyFromParams<N, N, Ts...> {
  static typename ParamsKey<N>::Type Get(const typename Params<N>::Type&, Ts... args) {
    return typename ParamsKey<N>::Type(args...);
  }
};

template<int N>
typename ParamsKey<N>::Type GetKeyFromParams(const typename Params<N>::Type& from) {
  return KeyFromParams<N, 0>::Get(from);
}

class ValueTuple {
 public:
  virtual int Size() const = 0;
  virtual const BoxedValue& At(int pos) const = 0;
  virtual const BoxedValue& Only() const = 0;

 protected:
  ValueTuple() = default;
  virtual ~ValueTuple() = default;

 private:
  void* operator new(std::size_t size) = delete;
};


namespace zeolite_internal {

template<>
class PoolManager<BoxedValue> {
  using PoolEntry = PoolStorage<BoxedValue>;
  using Managed = PoolEntry::Managed;

  template<class> friend class PoolArray;

  static PoolEntry* Take(int size);
  static void Return(PoolEntry* storage);

  static constexpr unsigned int pool_limit_ = 256;
  static unsigned int pool4_size_;
  static PoolEntry* pool4_;
  static std::atomic_flag pool4_flag_;
};

template<>
class PoolManager<const BoxedValue*> {
  using PoolEntry = PoolStorage<const BoxedValue*>;
  using Managed = PoolEntry::Managed;

  template<class> friend class PoolArray;

  static PoolEntry* Take(int size);
  static void Return(PoolEntry* storage);

  static constexpr unsigned int pool_limit_ = 256;
  static unsigned int pool4_size_;
  static PoolEntry* pool4_;
  static std::atomic_flag pool4_flag_;
};

template<>
class PoolManager<S<TypeInstance>> {
  using PoolEntry = PoolStorage<S<TypeInstance>>;
  using Managed = PoolEntry::Managed;

  template<class> friend class PoolArray;

  static PoolEntry* Take(int size);
  static void Return(PoolEntry* storage);

  static constexpr unsigned int pool_limit_ = 256;
  static unsigned int pool4_size_;
  static PoolEntry* pool4_;
  static std::atomic_flag pool4_flag_;
};

}  // namespace zeolite_internal


class ReturnTuple : public ValueTuple {
 public:
  ReturnTuple(int size) : size_(size), data_(size_) {}

  template<class...Ts>
  explicit ReturnTuple(Ts... returns) : size_(sizeof...(Ts)), data_(size_) {
    data_.Init(std::move(returns)...);
  }

  ReturnTuple(ReturnTuple&&) = default;
  ReturnTuple& operator = (ReturnTuple&&);

  int Size() const final;
  BoxedValue& At(int pos);
  const BoxedValue& At(int pos) const final;
  const BoxedValue& Only() const final;

 private:
  ReturnTuple(const ReturnTuple&) = delete;
  ReturnTuple& operator =(const ReturnTuple&) = delete;
  void* operator new(std::size_t size) = delete;

  int size_;
  zeolite_internal::PoolArray<BoxedValue> data_;
};

class ArgTuple : public ValueTuple {
 public:
  template<class...Ts>
  explicit ArgTuple(const Ts&... args) : size_(sizeof...(Ts)), data_(size_) {
    data_.Init(&args...);
  }

  int Size() const final;
  const BoxedValue& At(int pos) const final;
  const BoxedValue& Only() const final;

 private:
  ArgTuple(const ArgTuple&) = delete;
  ArgTuple(ArgTuple&&) = delete;
  ArgTuple& operator = (const ArgTuple&) = delete;
  ArgTuple& operator = (ArgTuple&&) =  delete;
  void* operator new(std::size_t size) = delete;

  int size_;
  zeolite_internal::PoolArray<const BoxedValue*> data_;
};

class ParamTuple {
 public:
  template<class...Ts>
  explicit ParamTuple(const Ts&... params) : size_(sizeof...(Ts)), data_(size_) {
    data_.Init(std::move(params)...);
  }

  ParamTuple(ParamTuple&& other) = default;

  int Size() const;
  const S<TypeInstance>& At(int pos) const;

 private:
  ParamTuple(const ParamTuple&) = delete;
  ParamTuple& operator = (const ParamTuple&) = delete;
  ParamTuple& operator = (ParamTuple&&) = delete;
  void* operator new(std::size_t size) = delete;

  int size_;
  zeolite_internal::PoolArray<S<TypeInstance>> data_;
};

#endif  // TYPES_HPP_
