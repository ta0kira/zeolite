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

#include "category-source.hpp"
#include "Streamlined_Realtime.hpp"
#include "Category_Float.hpp"
#include "Category_Realtime.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

struct ExtCategory_Realtime : public Category_Realtime {
};

struct ExtType_Realtime : public Type_Realtime {
  inline ExtType_Realtime(Category_Realtime& p, Params<0>::Type params) : Type_Realtime(p, params) {}
  ReturnTuple Call_sleepSeconds(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Realtime.sleepSeconds")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    if (Var_arg1 < 0) {
      FAIL() << "Bad wait time " << Var_arg1;
    }
    struct timespec timeout{ (int) trunc(Var_arg1), (int) (1000000000.0 * (Var_arg1-trunc(Var_arg1))) };
    struct timespec remainder;
    while (nanosleep(&timeout, &remainder) != 0) {
      if (errno == EINTR) {
        timeout = remainder;
      } else {
        FAIL() << "Error sleeping: " << strerror(errno) << " (error " << errno << ")";
      }
    }
    return ReturnTuple();
  }
};

Category_Realtime& CreateCategory_Realtime() {
  static auto& category = *new ExtCategory_Realtime();
  return category;
}
S<Type_Realtime> CreateType_Realtime(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_Realtime(CreateCategory_Realtime(), Params<0>::Type()));
  return cached;
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
