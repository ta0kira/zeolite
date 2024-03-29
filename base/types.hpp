/* -----------------------------------------------------------------------------
Copyright 2019-2023 Kevin P. Barry

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

#include <cstdint>
#include <functional>
#include <memory>
#include <set>
#include <tuple>

#include "logging.hpp"
#include "pooled.hpp"


#define ALWAYS_PERMANENT(type) \
  type(const type&) = delete; \
  type(type&&) = delete; \
  type& operator =(const type&) = delete; \
  type& operator =(type&&) = delete;

using CategoryId = std::int64_t;

using PrimBool = bool;
using PrimInt = std::int64_t;
using PrimString = std::string;
using PrimChar = char;
using PrimCharBuffer = std::string;
using PrimFloat = double;

class OpaqueObject { ~OpaqueObject() = default; };
using PrimPointer = OpaqueObject*;

class OpaqueInstance { ~OpaqueInstance() = default; };
using PrimIdentifier = OpaqueInstance*;

inline void SwapValues(PrimBool& left, PrimBool& right) {
  PrimBool temp = right;
  right = left;
  left = temp;
}

inline void SwapValues(PrimInt& left, PrimInt& right) {
  PrimInt temp = right;
  right = left;
  left = temp;
}

inline void SwapValues(PrimChar& left, PrimChar& right) {
  PrimChar temp = right;
  right = left;
  left = temp;
}

inline void SwapValues(PrimFloat& left, PrimFloat& right) {
  PrimFloat temp = right;
  right = left;
  left = temp;
}

inline void SwapValues(PrimPointer& left, PrimPointer& right) {
  PrimPointer temp = right;
  right = left;
  left = temp;
}

inline void SwapValues(PrimIdentifier& left, PrimIdentifier& right) {
  PrimIdentifier temp = right;
  right = left;
  left = temp;
}

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

template<class T>
using W = std::weak_ptr<T>;

template<class...Ts>
using T = std::tuple<Ts...>;

template<class...Ts>
T<Ts...> T_get(Ts... ts) { return std::make_tuple(ts...); }

template<class T>
using L = std::set<T>;

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

  LazyInit& operator = (T&& value) {
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

template<class T>
inline void SwapValues(LazyInit<T>& left, LazyInit<T>& right) {
  T temp = right.Get();
  right = left.Get();
  left = std::move(temp);
}

template<class T>
inline void SwapValues(T& left, LazyInit<T>& right) {
  T temp = right.Get();
  right = std::move(left);
  left = std::move(temp);
}

template<class T>
inline void SwapValues(LazyInit<T>& left, T& right) {
  T temp = std::move(right);
  right = left.Get();
  left = std::move(temp);
}


class TypeCategory;
class TypeInstance;
class TypeValue;

namespace zeolite_internal {
class BoxedValue;
class WeakValue;
}  // namespace zeolite_internal

using zeolite_internal::BoxedValue;
using zeolite_internal::WeakValue;

template<int N, class...Ts>
struct Params {
  using Type = typename Params<N-1, const S<const TypeInstance>&, Ts...>::Type;
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

#endif  // TYPES_HPP_
