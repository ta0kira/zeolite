/* -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

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
#include <tuple>
#include <vector>

#include "logging.hpp"


#define ALWAYS_PERMANENT(type) \
  type(const type&) = delete; \
  type(type&&) = delete; \
  type& operator =(const type&) = delete; \
  type& operator =(type&&) = delete;

using PrimInt = std::int64_t;
using PrimString = std::string;
using PrimChar = char;
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

  ReturnTuple(ReturnTuple&&) = default;
  ReturnTuple& operator =(ReturnTuple&&);

  template<class...Ts>
  explicit ReturnTuple(Ts... returns) : returns_{std::move(returns)...} {}

  int Size() const final;
  S<TypeValue>& At(int pos);
  const S<TypeValue>& At(int pos) const final;
  const S<TypeValue>& Only() const final;

 private:
  ReturnTuple(const ReturnTuple&) = delete;
  ReturnTuple& operator =(const ReturnTuple&) = delete;

  std::vector<S<TypeValue>> returns_;
};

class ArgTuple : public ValueTuple {
 public:
  template<class...Ts>
  explicit ArgTuple(const Ts&... args) : args_{&args...} {}

  int Size() const final;
  const S<TypeValue>& At(int pos) const final;
  const S<TypeValue>& Only() const final;

 private:
  ArgTuple(const ArgTuple&) = delete;
  ArgTuple(ArgTuple&&) = delete;
  ArgTuple& operator =(const ArgTuple&) = delete;
  ArgTuple& operator =(ArgTuple&&) =  delete;

  std::vector<const S<TypeValue>*> args_;
};

class ParamTuple {
 public:
  ParamTuple(ParamTuple&& other) : params_(std::move(other.params_)) {}

  template<class...Ts>
  explicit ParamTuple(Ts*... args) : params_{args...} {}

  int Size() const;
  TypeInstance* At(int pos) const;

 private:
  ParamTuple(const ParamTuple&) = delete;
  ParamTuple& operator =(const ParamTuple&) = delete;
  void* operator new(std::size_t size) = delete;

  std::vector<TypeInstance*> params_;
};

#endif  // TYPES_HPP_
