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
#include <vector>

#include <pthread.h>
#include <string.h>

#include "category-source.hpp"
#include "Streamlined_EnumeratedWait.hpp"
#include "Streamlined_EnumeratedBarrier.hpp"
#include "Category_BarrierWait.hpp"
#include "Category_EnumeratedBarrier.hpp"
#include "Category_Int.hpp"

namespace {

class Barrier {
 public:
  Barrier(int count) : index_usage(new std::atomic_int[count]) {
    alive.store(true);
    wait_count.store(0);
    for (int i = 0; i < count; ++i) {
      index_usage[i].store(0);
    }
    int error = pthread_barrier_init(&barrier, NULL, count);
    if (error != 0) {
      FAIL() << "Error creating barrier: " << strerror(error) << " (error " << error << ")";
    }
  }

  void Wait(int index) {
    TRACE_CREATION
    Enter(index);
    int error = pthread_barrier_wait(&barrier);
    if (error != 0 && error != PTHREAD_BARRIER_SERIAL_THREAD) {
      FAIL() << "Error waiting for barrier: " << strerror(error) << " (error " << error << ")";
    }
    Exit(index);
  }

  void Kill(int index) {
    TRACE_CREATION
    if (alive.exchange(false) && wait_count.load() > 0) {
      FAIL() << "BarrierWait at index " << index << " destroyed while one or more threads were waiting";
    }
  }

  ~Barrier() {
    TRACE_CREATION
    int error = pthread_barrier_destroy(&barrier);
    if (error != 0) {
      FAIL() << "Error cleaning up barrier: " << strerror(error) << " (error " << error << ")";
    }
  }

private:
  void Enter(int index) {
    ++wait_count;
    if (!alive.load()) {
      --wait_count;
      FAIL() << "One or more BarrierWait have been destroyed";
    }
    if (++index_usage[index] > 1) {
      Exit(index);
      FAIL() << "BarrierWait at index " << index << " is already in use";
    }
  }

  void Exit(int index) {
    --wait_count;
    --index_usage[index];
  }

  std::atomic_bool alive;
  std::atomic_int wait_count;
  const R<std::atomic_int[]> index_usage;
  pthread_barrier_t barrier;
  CAPTURE_CREATION("EnumeratedBarrier")
};

}  // namespace

#ifdef ZEOLITE_PRIVATE_NAMESPACE
namespace ZEOLITE_PRIVATE_NAMESPACE {
#endif  // ZEOLITE_PRIVATE_NAMESPACE

BoxedValue CreateValue_EnumeratedWait(
  S<const Type_EnumeratedWait> parent, S<Barrier> b, int i);

struct ExtCategory_EnumeratedWait : public Category_EnumeratedWait {
};

struct ExtType_EnumeratedWait : public Type_EnumeratedWait {
  inline ExtType_EnumeratedWait(Category_EnumeratedWait& p, Params<0>::Type params) : Type_EnumeratedWait(p, params) {}
};

struct ExtValue_EnumeratedWait : public Value_EnumeratedWait {
  inline ExtValue_EnumeratedWait(S<const Type_EnumeratedWait> p, S<Barrier> b, int i)
    : Value_EnumeratedWait(std::move(p)), barrier(b), index(i) {}

  ReturnTuple Call_wait(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("EnumeratedWait.wait")
    barrier->Wait(index);
    return ReturnTuple(VAR_SELF);
  }

  ~ExtValue_EnumeratedWait() {
    barrier->Kill(index);
  }

  const S<Barrier> barrier;
  int index;
};

Category_EnumeratedWait& CreateCategory_EnumeratedWait() {
  static auto& category = *new ExtCategory_EnumeratedWait();
  return category;
}

S<const Type_EnumeratedWait> CreateType_EnumeratedWait(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_EnumeratedWait(CreateCategory_EnumeratedWait(), Params<0>::Type()));
  return cached;
}

void RemoveType_EnumeratedWait(const Params<0>::Type& params) {}

BoxedValue CreateValue_EnumeratedWait(
  S<const Type_EnumeratedWait> parent, S<Barrier> b, int i) {
  return BoxedValue::New<ExtValue_EnumeratedWait>(std::move(parent), std::move(b), i);
}

#ifdef ZEOLITE_PRIVATE_NAMESPACE
}  // namespace ZEOLITE_PRIVATE_NAMESPACE
using namespace ZEOLITE_PRIVATE_NAMESPACE;
#endif  // ZEOLITE_PRIVATE_NAMESPACE


#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_EnumeratedBarrier(
  S<const Type_EnumeratedBarrier> parent, std::vector<BoxedValue> w);

struct ExtCategory_EnumeratedBarrier : public Category_EnumeratedBarrier {
};

struct ExtType_EnumeratedBarrier : public Type_EnumeratedBarrier {
  inline ExtType_EnumeratedBarrier(Category_EnumeratedBarrier& p, Params<0>::Type params) : Type_EnumeratedBarrier(p, params) {}

  ReturnTuple Call_new(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("EnumeratedBarrier.new")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    if (Var_arg1 < 0) {
      FAIL() << "Invalid barrier thread count " << Var_arg1;
    }
    std::vector<BoxedValue> waits;
    S<Barrier> barrier(Var_arg1? new Barrier(Var_arg1) : nullptr);
    for (int i = 0; i < Var_arg1; ++i) {
      BoxedValue wait = CreateValue_EnumeratedWait(
        CreateType_EnumeratedWait(Params<0>::Type()), barrier, i);
      waits.push_back(wait);
    }
    return ReturnTuple(CreateValue_EnumeratedBarrier(PARAM_SELF, std::move(waits)));
  }
};

struct ExtValue_EnumeratedBarrier : public Value_EnumeratedBarrier {
  inline ExtValue_EnumeratedBarrier(S<const Type_EnumeratedBarrier> p, std::vector<BoxedValue> w)
    : Value_EnumeratedBarrier(std::move(p)), waits(std::move(w)) {}

  ReturnTuple Call_readAt(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("EnumeratedBarrier.readAt")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    if (Var_arg1 < 0 || Var_arg1 >= waits.size()) {
      FAIL() << "index " << Var_arg1 << " is out of bounds";
    }
    return ReturnTuple(waits[Var_arg1]);
  }

  ReturnTuple Call_size(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("EnumeratedBarrier.size")
    return ReturnTuple(Box_Int(waits.size()));
  }

  // vector<BarrierWait>
  const std::vector<BoxedValue> waits;
};

Category_EnumeratedBarrier& CreateCategory_EnumeratedBarrier() {
  static auto& category = *new ExtCategory_EnumeratedBarrier();
  return category;
}

S<const Type_EnumeratedBarrier> CreateType_EnumeratedBarrier(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_EnumeratedBarrier(CreateCategory_EnumeratedBarrier(), Params<0>::Type()));
  return cached;
}

void RemoveType_EnumeratedBarrier(const Params<0>::Type& params) {}

BoxedValue CreateValue_EnumeratedBarrier(
  S<const Type_EnumeratedBarrier> parent, std::vector<BoxedValue> w) {
  return BoxedValue::New<ExtValue_EnumeratedBarrier>(std::move(parent), std::move(w));
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
