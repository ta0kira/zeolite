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

#include <errno.h>
#include <math.h>
#include <string.h>
#include <time.h>

#include <chrono>
#include <iomanip>
#include <thread>

#include "category-source.hpp"
#include "Streamlined_Realtime.hpp"
#include "Category_Float.hpp"
#include "Category_Realtime.hpp"

#ifndef SLEEP_SPINLOCK_LIMIT
#define SLEEP_SPINLOCK_LIMIT 0.001
#endif

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

struct ExtCategory_Realtime : public Category_Realtime {
};

struct ExtType_Realtime : public Type_Realtime {
  inline ExtType_Realtime(Category_Realtime& p, Params<0>::Type params) : Type_Realtime(p, params) {}

  ReturnTuple Call_sleepSeconds(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("Realtime.sleepSeconds")
    const PrimFloat Var_arg1 = (args.At(0)).AsFloat();
    if (Var_arg1 > 0) {
      std::this_thread::sleep_for(std::chrono::duration<double>(Var_arg1));
    }
    return ReturnTuple();
  }

  ReturnTuple Call_sleepSecondsPrecise(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("Realtime.sleepSecondsPrecise")
    const PrimFloat Var_arg1 = (args.At(0)).AsFloat();

    const auto spinlock_limit = std::chrono::duration<double>(SLEEP_SPINLOCK_LIMIT);

    const auto target_time =
      std::chrono::high_resolution_clock::now().time_since_epoch() +
      std::chrono::duration<double>(Var_arg1);

    while (true) {
      const auto now = std::chrono::high_resolution_clock::now().time_since_epoch();
      if (now >= target_time) break;
      const auto sleep_time = target_time - now;
      if (sleep_time <= spinlock_limit) {
        while (std::chrono::high_resolution_clock::now().time_since_epoch() < target_time);
        break;
      } else {
        std::this_thread::sleep_for(sleep_time - spinlock_limit);
      }
    }

    return ReturnTuple();
  }

  ReturnTuple Call_monoSeconds(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("Realtime.monoSeconds")
    const auto time = std::chrono::steady_clock::now().time_since_epoch();
    const PrimFloat seconds =
      (PrimFloat) std::chrono::duration_cast<std::chrono::microseconds>(time).count() /
      (PrimFloat) 1000000.0;
    return ReturnTuple(Box_Float(seconds));
  }
};

Category_Realtime& CreateCategory_Realtime() {
  static auto& category = *new ExtCategory_Realtime();
  return category;
}

S<const Type_Realtime> CreateType_Realtime(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Realtime(CreateCategory_Realtime(), Params<0>::Type()));
  return cached;
}

void RemoveType_Realtime(const Params<0>::Type& params) {}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
