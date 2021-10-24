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
#include "Streamlined_SimpleMutex.hpp"
#include "Category_Mutex.hpp"
#include "Category_SimpleMutex.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_SimpleMutex(S<Type_SimpleMutex> parent);

struct ExtCategory_SimpleMutex : public Category_SimpleMutex {
};

struct ExtType_SimpleMutex : public Type_SimpleMutex {
  inline ExtType_SimpleMutex(Category_SimpleMutex& p, Params<0>::Type params) : Type_SimpleMutex(p, params) {}

  ReturnTuple Call_new(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleMutex.new")
    return ReturnTuple(CreateValue_SimpleMutex(CreateType_SimpleMutex(Params<0>::Type())));
  }
};

struct ExtValue_SimpleMutex : public Value_SimpleMutex {
  inline ExtValue_SimpleMutex(S<Type_SimpleMutex> p) : Value_SimpleMutex(p) {}

  ReturnTuple Call_lock(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleMutex.lock")
    mutex.lock();
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_unlock(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleMutex.unlock")
    mutex.unlock();
    return ReturnTuple(Var_self);
  }

  std::mutex mutex;
};

Category_SimpleMutex& CreateCategory_SimpleMutex() {
  static auto& category = *new ExtCategory_SimpleMutex();
  return category;
}

S<Type_SimpleMutex> CreateType_SimpleMutex(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_SimpleMutex(CreateCategory_SimpleMutex(), Params<0>::Type()));
  return cached;
}

BoxedValue CreateValue_SimpleMutex(S<Type_SimpleMutex> parent) {
  return BoxedValue::New<ExtValue_SimpleMutex>(parent);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
