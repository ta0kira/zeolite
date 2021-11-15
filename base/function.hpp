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

#ifndef FUNCTION_HPP_
#define FUNCTION_HPP_

#include <ostream>

#include "types.hpp"


struct CategoryFunction {
  const int param_count;
  const int arg_count;
  const int return_count;
  const char* const category;
  const char* const function;
  const CategoryId collection;
  const int function_num;
};

struct TypeFunction {
  const int param_count;
  const int arg_count;
  const int return_count;
  const char* const category;
  const char* const function;
  const CategoryId collection;
  const int function_num;
};

struct ValueFunction {
  const int param_count;
  const int arg_count;
  const int return_count;
  const char* const category;
  const char* const function;
  const CategoryId collection;
  const int function_num;
};

inline std::ostream& operator << (std::ostream& output, const CategoryFunction& func) {
  return output << func.category << ":" << func.function;
}

inline std::ostream& operator << (std::ostream& output, const TypeFunction& func) {
  return output << func.category << "." << func.function;
}

inline std::ostream& operator << (std::ostream& output, const ValueFunction& func) {
  return output << func.category << "." << func.function;
}

struct ParamsArgs {
  virtual int NumParams() const = 0;
  virtual const S<const TypeInstance>& GetParam(int pos) const = 0;
  virtual int NumArgs() const = 0;
  virtual const BoxedValue& GetArg(int pos) const = 0;
};

template<int P, int A>
struct PassArgs : public ParamsArgs {
  template<class... Ps>
  PassArgs(const Ps&... passed) {
    Init<0, 0>(passed...);
  }

  template<int Pn, int An>
  void Init() {}

  template<int Pn, int An, class... Ps>
  void Init(const S<const TypeInstance>& param, const Ps&... passed) {
    params[Pn] = &param;
    Init<Pn+1, An>(passed...);
  }

  template<int Pn, int An, class... Ps>
  void Init(const BoxedValue& arg, const Ps&... passed) {
    args[An] = &arg;
    Init<Pn, An+1>(passed...);
  }

  int NumParams() const final { return P; }

  const S<const TypeInstance>& GetParam(int pos) const final {
    if (pos < 0 || pos >= P) {
      FAIL() << "Bad param index";
    }
    return *params[pos];
  }

  int NumArgs() const final { return A; }

  const BoxedValue& GetArg(int pos) const final {
    if (pos < 0 || pos >= A) {
      FAIL() << "Bad arg index";
    }
    return *args[pos];
  }

  const S<const TypeInstance>* params[P];
  const BoxedValue*   args[A];
};

template<>
struct PassArgs<0, 0> : public ParamsArgs {
  constexpr PassArgs() {}

  int NumParams() const final { return 0; }

  const S<const TypeInstance>& GetParam(int pos) const final {
    FAIL() << "Bad param index";
    __builtin_unreachable();
  }

  int NumArgs() const final { return 0; }

  const BoxedValue& GetArg(int pos) const final {
    FAIL() << "Bad arg index";
    __builtin_unreachable();
  }
};

template<int P>
struct PassReturns : public ParamsArgs {
  template<class... Ps>
  PassReturns(const Ps&... passed) {
    Init<0>(passed...);
  }

  template<int Pn, class... Ps>
  void Init(const S<const TypeInstance>& param, const Ps&... passed) {
    params[Pn] = &param;
    Init<Pn+1>(passed...);
  }

  template<int Pn>
  void Init(const ReturnTuple& forward) {
    returns = &forward;
  }

  int NumParams() const final { return P; }

  const S<const TypeInstance>& GetParam(int pos) const final {
    if (pos < 0 || pos >= P) {
      FAIL() << "Bad param index";
    }
    return *params[pos];
  }

  int NumArgs() const final                     { return returns->Size(); }
  const BoxedValue& GetArg(int pos) const final { return returns->At(pos); }

  const S<const TypeInstance>* params[P];
  const ReturnTuple* returns;
};

template<int P, int A, class... Ps>
struct AutoArgs;

template<int P, int A>
struct AutoArgs<P, A> {
  using Type = PassArgs<P, A>;
};

template<int P, int A, class... Ps>
struct AutoArgs<P, A, BoxedValue, Ps...> {
  using Type = typename AutoArgs<P, A+1, Ps...>::Type;
};

template<int P, class... Ps>
struct AutoCall {
  using Type = typename AutoArgs<P, 0, Ps...>::Type;
};

template<int P>
struct AutoCall<P, ReturnTuple> {
  using Type = PassReturns<P>;
};

template<int P, class... Ps>
struct AutoCall<P, S<const TypeInstance>, Ps...> {
  using Type = typename AutoCall<P+1, Ps...>::Type;
};

template<class... Ps>
struct GetCall {
  using Type = typename AutoCall<0, Ps...>::Type;
};

template<class... Ps>
typename GetCall<Ps...>::Type PassParamsArgs(const Ps&... passed) {
  return typename GetCall<Ps...>::Type(passed...);
}

#endif  // FUNCTION_HPP_
