#ifndef TYPES_HPP_
#define TYPES_HPP_

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
  CATEGORY,
  TYPE,
  VALUE,
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


class TypeCategory;
class TypeInstance;
class TypeValue;

template<int N, class...Ts>
struct Params {
  using Type = typename Params<N-1, TypeInstance*, Ts...>::Type;
};

template<class...Ts>
struct Params<0, Ts...> {
  using Type = T<Ts...>;
};


class ValueTuple {
 public:
  virtual int Size() const = 0;
  virtual S<TypeValue>& At(int pos) = 0;
  virtual const S<TypeValue>& At(int pos) const = 0;
  virtual const S<TypeValue>& Only() const = 0;

 protected:
  ValueTuple() = default;
  virtual ~ValueTuple() = default;

 private:
  void* operator new(std::size_t size) = delete;
};

class ReturnTuple : public ValueTuple {
 public:
  ReturnTuple(int size) : returns_(size) {}

  template<class...Ts>
  ReturnTuple(Ts... returns) : returns_{std::move(returns)...} {}

  int Size() const final;
  S<TypeValue>& At(int pos) final;
  const S<TypeValue>& At(int pos) const final;
  const S<TypeValue>& Only() const final;

 private:
  ReturnTuple& operator =(const ReturnTuple&) = delete;

  std::vector<S<TypeValue>> returns_;
};

class ArgTuple : public ValueTuple {
 public:
  ArgTuple(ArgTuple&& other) : args_(std::move(other.args_)) {}

  template<class...Ts>
  ArgTuple(Ts... args) : args_{std::move(args)...} {}

  int Size() const final;
  S<TypeValue>& At(int pos) final;
  const S<TypeValue>& At(int pos) const final;
  const S<TypeValue>& Only() const final;

 private:
  ArgTuple(const ArgTuple&) = delete;
  ArgTuple& operator =(const ArgTuple&) = delete;

  std::vector<ReturnTuple> args_;
};

class ParamTuple {
 public:
  ParamTuple(ParamTuple&& other) : params_(std::move(other.params_)) {}

  template<class...Ts>
  ParamTuple(Ts*... args) : params_{args...} {}

  int Size() const;
  TypeInstance* At(int pos) const;

 private:
  ParamTuple(const ParamTuple&) = delete;
  ParamTuple& operator =(const ParamTuple&) = delete;
  void* operator new(std::size_t size) = delete;

  std::vector<TypeInstance*> params_;
};


#endif  // TYPES_HPP_
