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

#include <mutex>

#include "category-source.hpp"
#include "Streamlined_MutexLock.hpp"
#include "Category_Mutex.hpp"
#include "Category_MutexLock.hpp"
#include "Category_PersistentResource.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_MutexLock(S<Type_MutexLock> parent, const ValueTuple& args);

struct ExtCategory_MutexLock : public Category_MutexLock {
};

struct ExtType_MutexLock : public Type_MutexLock {
  inline ExtType_MutexLock(Category_MutexLock& p, Params<0>::Type params) : Type_MutexLock(p, params) {}

  ReturnTuple Call_lock(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("MutexLock.lock")
    return ReturnTuple(CreateValue_MutexLock(CreateType_MutexLock(Params<0>::Type()), args));
  }
};

struct ExtValue_MutexLock : public Value_MutexLock {
  inline ExtValue_MutexLock(S<Type_MutexLock> p, const ValueTuple& args)
    : Value_MutexLock(p), mutex(args.Only()) {
    TypeValue::Call(BoxedValue::Require(mutex), Function_Mutex_lock, ParamTuple(), ArgTuple());
  }

  ReturnTuple Call_freeResource(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("MutexLock.freeResource")
    TRACE_CREATION
    if (!BoxedValue::Present(mutex)) {
      FAIL() << "MutexLock freed multiple times";
    } else {
      TypeValue::Call(mutex, Function_Mutex_unlock, ParamTuple(), ArgTuple());
      mutex = Var_empty;
    }
    return ReturnTuple();
  }

  ~ExtValue_MutexLock() {
    if (BoxedValue::Present(mutex)) {
      TRACE_CREATION
      TypeValue::Call(BoxedValue::Require(mutex), Function_Mutex_unlock, ParamTuple(), ArgTuple());
      mutex = Var_empty;
      FAIL() << "MutexLock not freed with freeResource()";
    }
  }

  // optional Mutex
  BoxedValue mutex;
  CAPTURE_CREATION("MutexLock")
};

Category_MutexLock& CreateCategory_MutexLock() {
  static auto& category = *new ExtCategory_MutexLock();
  return category;
}

S<Type_MutexLock> CreateType_MutexLock(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_MutexLock(CreateCategory_MutexLock(), Params<0>::Type()));
  return cached;
}

BoxedValue CreateValue_MutexLock(S<Type_MutexLock> parent, const ValueTuple& args) {
  return BoxedValue::New<ExtValue_MutexLock>(parent, args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
