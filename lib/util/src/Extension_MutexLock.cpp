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

BoxedValue CreateValue_MutexLock(S<const Type_MutexLock> parent, const BoxedValue& mutex);

struct ExtCategory_MutexLock : public Category_MutexLock {
};

struct ExtType_MutexLock : public Type_MutexLock {
  inline ExtType_MutexLock(Category_MutexLock& p, Params<0>::Type params) : Type_MutexLock(p, params) {}

  ReturnTuple Call_lock(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("MutexLock.lock")
    return ReturnTuple(CreateValue_MutexLock(CreateType_MutexLock(Params<0>::Type()), params_args.GetArg(0)));
  }
};

struct ExtValue_MutexLock : public Value_MutexLock {
  inline ExtValue_MutexLock(S<const Type_MutexLock> p, const BoxedValue& mutex)
    : Value_MutexLock(std::move(p)), mutex_(mutex) {
    TypeValue::Call(BoxedValue::Require(mutex_), Function_Mutex_lock, PassParamsArgs());
  }

  ReturnTuple Call_freeResource(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("MutexLock.freeResource")
    TRACE_CREATION
    if (!BoxedValue::Present(mutex_)) {
      FAIL() << "MutexLock freed multiple times";
    } else {
      TypeValue::Call(mutex_, Function_Mutex_unlock, PassParamsArgs());
      mutex_ = Var_empty;
    }
    return ReturnTuple();
  }

  ~ExtValue_MutexLock() {
    if (BoxedValue::Present(mutex_)) {
      TRACE_CREATION
      TypeValue::Call(BoxedValue::Require(mutex_), Function_Mutex_unlock, PassParamsArgs());
      mutex_ = Var_empty;
      FAIL() << "MutexLock not freed with freeResource()";
    }
  }

  // optional Mutex
  BoxedValue mutex_;
  CAPTURE_CREATION("MutexLock")
};

Category_MutexLock& CreateCategory_MutexLock() {
  static auto& category = *new ExtCategory_MutexLock();
  return category;
}

S<const Type_MutexLock> CreateType_MutexLock(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_MutexLock(CreateCategory_MutexLock(), Params<0>::Type()));
  return cached;
}

void RemoveType_MutexLock(const Params<0>::Type& params) {}

BoxedValue CreateValue_MutexLock(S<const Type_MutexLock> parent, const BoxedValue& mutex) {
  return BoxedValue::New<ExtValue_MutexLock>(std::move(parent), mutex);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
