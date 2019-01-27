#ifndef TYPES_HXX_
#define TYPES_HXX_

#include <memory>
#include <tuple>
#include <vector>

#include "logging.hpp"


#define ALWAYS_PERMANENT(type) \
  type(const type&) = delete; \
  type(type&&) = delete; \
  type& operator =(const type&) = delete; \
  type& operator =(type&&) = delete;

enum class SymbolScope {
  CategoryScope,
  TypeScope,
  ValueScope,
};

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

template<class T>
inline W<T> W_get(T* val) { return W<T>(val); }

template<class...Ts>
using T = std::tuple<Ts...>;

template<class...Ts>
T<Ts...> T_get(Ts... ts) { return std::make_tuple(ts...); }

template<class T>
using L = std::vector<T>;

template<class T, class...Ts>
inline L<T> L_get(Ts... ts) { return L<T>{ts...}; }


template<int K, class V, class T>
struct ExpandToTuple {
  static void Set(const V& vals, T& tuple) {
    std::get<K-1>(tuple) = vals[K-1];
    ExpandToTuple<K-1,V,T>::Set(vals, tuple);
  }
};

template<class V, class T>
struct ExpandToTuple<0,V,T> {
  static void Set(const V& vals, T& tuple) {}
};

template<class Tx, class X>
Tx V_to_T(const L<X>& vals) {
  static constexpr int tuple_size = std::tuple_size<Tx>::value;
  FAIL_IF(vals.size() != tuple_size) << "Expected " << tuple_size << " elements";
  Tx tuple;
  ExpandToTuple<tuple_size,L<X>,Tx>::Set(vals, tuple);
  return tuple;
}


template<int K, class T, class V>
struct FlattenFromTuple {
  static void Set(const T& tuple, V& vals) {
    vals.push_back(std::get<K-1>(tuple));
    FlattenFromTuple<K-1,T,V>::Set(tuple, vals);
  }
};

template<class T, class V>
struct FlattenFromTuple<0,T,V> {
  static void Set(const T& tuple, V& vals) {}
};

template<class X, class...Ts>
L<X> T_to_V(const T<Ts...>& tuple) {
  static constexpr int tuple_size = std::tuple_size<T<Ts...>>::value;
  L<X> vals;
  vals.reserve(tuple_size);
  FlattenFromTuple<tuple_size,T<Ts...>,L<X>>::Set(tuple, vals);
  return vals;
}


class TypeCategory;
class TypeInstance;
class TypeValue;

using DParams = L<TypeInstance*>;

template<int N, class...Ts>
struct Params {
  using Type = typename Params<N-1, TypeInstance*, Ts...>::Type;
};

template<class...Ts>
struct Params<0, Ts...> {
  using Type = T<Ts...>;
};

using DReturns = L<S<TypeValue>>;

template<int N, class...Ts>
struct Returns {
  using Type = typename Returns<N-1, S<TypeValue>, Ts...>::Type;
};

template<class...Ts>
struct Returns<0, Ts...> {
  using Type = T<Ts...>;
};

using DArgs = L<S<TypeValue>>;

template<int N, class...Ts>
struct Args {
  using Type = typename Args<N-1, S<TypeValue>, Ts...>::Type;
};

template<class...Ts>
struct Args<0, Ts...> {
  using Type = T<Ts...>;
};

#endif  // TYPES_HXX_
