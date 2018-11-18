#ifndef BASE_H_
#define BASE_H_

#include <memory>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

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


class LogThenCrash {
 public:
  LogThenCrash(bool fail, const std::string& condition = "");
  ~LogThenCrash();

  template<class T>
  LogThenCrash& operator << (const T& stuff) {
    if (fail_) {
      static_cast<std::ostream&>(output_) << stuff;
    }
    return *this;
  }

 private:
  const bool fail_;
  const std::string condition_;
  std::ostringstream output_;
};

#define FAIL() LogThenCrash(true)

#define FAIL_IF(p) LogThenCrash(p,#p)


template<int K, class V, class T>
struct ArgsToTuple {
  static void Set(const V& vals, T& tuple) {
    std::get<K-1>(tuple) = vals[K];
    ArgsToTuple<K-1,V,T>::Set(vals, tuple);
  }
};

template<class V, class T>
struct ArgsToTuple<0,V,T> {
  static void Set(const V& vals, T& tuple) {}
};

template<class X, class...Ts>
T<Ts...> V_to_T(const std::vector<X>& vals) {
  static constexpr int tuple_size = std::tuple_size<T<Ts...>>::value;
  FAIL_IF(vals.size() != tuple_size) << "Expected " << tuple_size << " elements";
  T<Ts...> tuple;
  ArgsToTuple<tuple_size,std::vector<X>,T<Ts...>>::Set(vals, tuple);
  return tuple;
}


template<int K, class T, class V>
struct TupleToArgs {
  static void Set(const T& tuple, V& vals) {
    vals.push_back(std::get<K-1>(tuple));
    TupleToArgs<K-1,T,V>::Set(tuple, vals);
  }
};

template<class T, class V>
struct TupleToArgs<0,T,V> {
  static void Set(const T& tuple, V& vals) {}
};

template<class X, class...Ts>
std::vector<X> T_to_V(const T<Ts...>& tuple) {
  static constexpr int tuple_size = std::tuple_size<T<Ts...>>::value;
  std::vector<X> vals;
  vals.reserve(tuple_size);
  TupleToArgs<tuple_size,T<Ts...>,std::vector<X>>::Set(tuple, vals);
  return vals;
}

#endif  // BASE_H_
