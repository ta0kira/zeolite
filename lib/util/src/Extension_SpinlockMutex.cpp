/* -----------------------------------------------------------------------------
Copyright 2021 Kevin P. Barry

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

#include <atomic>

#include "category-source.hpp"
#include "Streamlined_SpinlockMutex.hpp"
#include "Category_Mutex.hpp"
#include "Category_SpinlockMutex.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_SpinlockMutex(S<Type_SpinlockMutex> parent);

struct ExtCategory_SpinlockMutex : public Category_SpinlockMutex {
};

struct ExtType_SpinlockMutex : public Type_SpinlockMutex {
  inline ExtType_SpinlockMutex(Category_SpinlockMutex& p, Params<0>::Type params) : Type_SpinlockMutex(p, params) {}

  ReturnTuple Call_new(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SpinlockMutex.new")
    return ReturnTuple(CreateValue_SpinlockMutex(CreateType_SpinlockMutex(Params<0>::Type())));
  }
};

struct ExtValue_SpinlockMutex : public Value_SpinlockMutex {
  inline ExtValue_SpinlockMutex(S<Type_SpinlockMutex> p) : Value_SpinlockMutex(p) {}

  ReturnTuple Call_lock(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SpinlockMutex.lock")
    while (flag.test_and_set(std::memory_order_acquire));
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_unlock(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SpinlockMutex.unlock")
    flag.clear(std::memory_order_release);
    return ReturnTuple(Var_self);
  }

  std::atomic_flag flag = ATOMIC_FLAG_INIT;
};

Category_SpinlockMutex& CreateCategory_SpinlockMutex() {
  static auto& category = *new ExtCategory_SpinlockMutex();
  return category;
}
S<Type_SpinlockMutex> CreateType_SpinlockMutex(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_SpinlockMutex(CreateCategory_SpinlockMutex(), Params<0>::Type()));
  return cached;
}
BoxedValue CreateValue_SpinlockMutex(S<Type_SpinlockMutex> parent) {
  return BoxedValue(new ExtValue_SpinlockMutex(parent));
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
