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

#include <pthread.h>
#include <string.h>

#include "category-source.hpp"
#include "Streamlined_EnumeratedWait.hpp"
#include "Streamlined_EnumeratedBarrier.hpp"
#include "Category_BarrierWait.hpp"
#include "Category_EnumeratedBarrier.hpp"
#include "Category_Int.hpp"
#include "Category_Stack.hpp"
#include "Category_Vector.hpp"

namespace {

class Barrier {
 public:
  Barrier(int count) {
    int error = pthread_barrier_init(&barrier, NULL, count);
    if (error != 0) {
      FAIL() << "Error creating barrier: " << strerror(error) << " (error " << error << ")";
    }
  }

  void Wait() {
    int error = pthread_barrier_wait(&barrier);
    if (error != 0 && error != PTHREAD_BARRIER_SERIAL_THREAD) {
      FAIL() << "Error waiting for barrier: " << strerror(error) << " (error " << error << ")";
    }
  }

  ~Barrier() {
    int error = pthread_barrier_destroy(&barrier);
    if (error != 0) {
      FAIL() << "Error cleaning up barrier: " << strerror(error) << " (error " << error << ")";
    }
  }

 private:
  pthread_barrier_t barrier;
};

}  // namespace

#ifdef ZEOLITE_PRIVATE_NAMESPACE
namespace ZEOLITE_PRIVATE_NAMESPACE {
#endif  // ZEOLITE_PRIVATE_NAMESPACE

S<TypeValue> CreateValue_EnumeratedWait(S<Type_EnumeratedWait> parent, const ParamTuple& params, S<Barrier> b);

struct ExtCategory_EnumeratedWait : public Category_EnumeratedWait {
};

struct ExtType_EnumeratedWait : public Type_EnumeratedWait {
  inline ExtType_EnumeratedWait(Category_EnumeratedWait& p, Params<0>::Type params) : Type_EnumeratedWait(p, params) {}
};

struct ExtValue_EnumeratedWait : public Value_EnumeratedWait {
  inline ExtValue_EnumeratedWait(S<Type_EnumeratedWait> p, const ParamTuple& params, S<Barrier> b)
    : Value_EnumeratedWait(p, params), barrier(b) {}

  ReturnTuple Call_wait(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("EnumeratedWait.wait")
    barrier->Wait();
    return ReturnTuple(Var_self);
  }

  const S<Barrier> barrier;
};

Category_EnumeratedWait& CreateCategory_EnumeratedWait() {
  static auto& category = *new ExtCategory_EnumeratedWait();
  return category;
}
S<Type_EnumeratedWait> CreateType_EnumeratedWait(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_EnumeratedWait(CreateCategory_EnumeratedWait(), Params<0>::Type()));
  return cached;
}
S<TypeValue> CreateValue_EnumeratedWait(S<Type_EnumeratedWait> parent, const ParamTuple& params, S<Barrier> b) {
  return S_get(new ExtValue_EnumeratedWait(parent, params, b));
}

#ifdef ZEOLITE_PRIVATE_NAMESPACE
}  // namespace ZEOLITE_PRIVATE_NAMESPACE
using namespace ZEOLITE_PRIVATE_NAMESPACE;
#endif  // ZEOLITE_PRIVATE_NAMESPACE


#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

struct ExtCategory_EnumeratedBarrier : public Category_EnumeratedBarrier {
};

struct ExtType_EnumeratedBarrier : public Type_EnumeratedBarrier {
  inline ExtType_EnumeratedBarrier(Category_EnumeratedBarrier& p, Params<0>::Type params) : Type_EnumeratedBarrier(p, params) {}

  ReturnTuple Call_new(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("EnumeratedBarrier.new")
    const PrimInt Var_arg1 = (args.At(0))->AsInt();
    if (Var_arg1 < 0) {
      FAIL() << "Invalid barrier thread count " << Var_arg1;
    }
    S<TypeValue> vector = GetCategory_Vector().Call(
      Function_Vector_create,
      ParamTuple(GetType_EnumeratedWait(Params<0>::Type())),
      ArgTuple()).Only();
    if (Var_arg1 > 0) {
      S<TypeValue> wait = CreateValue_EnumeratedWait(
        CreateType_EnumeratedWait(Params<0>::Type()), ParamTuple(), S_get(new Barrier(Var_arg1)));
      for (int i = 0; i < Var_arg1; ++i) {
        TypeValue::Call(vector, Function_Stack_push, ParamTuple(), ArgTuple(wait));
      }
    }
    return ReturnTuple(vector);
  }
};

Category_EnumeratedBarrier& CreateCategory_EnumeratedBarrier() {
  static auto& category = *new ExtCategory_EnumeratedBarrier();
  return category;
}
S<Type_EnumeratedBarrier> CreateType_EnumeratedBarrier(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_EnumeratedBarrier(CreateCategory_EnumeratedBarrier(), Params<0>::Type()));
  return cached;
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
