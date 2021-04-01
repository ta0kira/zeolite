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

#include <functional>
#include <thread>

#include "category-source.hpp"
#include "Streamlined_ProcessThread.hpp"
#include "Category_Bool.hpp"
#include "Category_Process.hpp"
#include "Category_ProcessThread.hpp"
#include "Category_Routine.hpp"
#include "Category_Thread.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

S<TypeValue> CreateValue_ProcessThread(S<Type_ProcessThread> parent, const ValueTuple& args);

struct ExtCategory_ProcessThread : public Category_ProcessThread {
};

struct ExtType_ProcessThread : public Type_ProcessThread {
  inline ExtType_ProcessThread(Category_ProcessThread& p, Params<0>::Type params) : Type_ProcessThread(p, params) {}

  ReturnTuple Call_from(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ProcessThread.from")
    return ReturnTuple(CreateValue_ProcessThread(CreateType_ProcessThread(Params<0>::Type()), args));
  }
};

struct ExtValue_ProcessThread : public Value_ProcessThread {
  inline ExtValue_ProcessThread(S<Type_ProcessThread> p, const ValueTuple& args)
    : Value_ProcessThread(p), routine(args.Only()) {}

  ReturnTuple Call_detach(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ProcessThread.detach")
    S<std::thread> temp = thread;
    thread = nullptr;
    if (!isJoinable(temp.get())) {
      FAIL() << "thread has not been started";
    } else {
      temp->detach();
    }
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_isRunning(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ProcessThread.isRunning")
    return ReturnTuple(Box_Bool(isJoinable(thread.get())));
  }

  ReturnTuple Call_join(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ProcessThread.join")
    S<std::thread> temp = thread;
    thread = nullptr;
    if (!isJoinable(temp.get())) {
      FAIL() << "thread has not been started";
    } else {
      temp->join();
    }
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_start(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ProcessThread.start")
    if (isJoinable(thread.get())) {
      FAIL() << "thread is already running";
    } else {
      // NOTE: Capture Var_self so that the thread retains a reference while
      // it's still running. This allows the caller to hold a weak reference to
      // the thread.
      thread.reset(new std::thread(
        capture_thread::ThreadCrosser::WrapCall([this,Var_self] {
          TRACE_CREATION
          TypeValue::Call(routine, Function_Routine_run, ParamTuple(), ArgTuple());
        })));
    }
    return ReturnTuple(Var_self);
  }

  inline static bool isJoinable(std::thread* thread) {
    return thread && thread->joinable();
  }

  ~ExtValue_ProcessThread() {
    S<std::thread> temp = thread;
    thread = nullptr;
    if (isJoinable(temp.get())) {
      thread->detach();
    }
  }

  const S<TypeValue> routine;
  S<std::thread> thread;
  CAPTURE_CREATION("ProcessThread")
};

Category_ProcessThread& CreateCategory_ProcessThread() {
  static auto& category = *new ExtCategory_ProcessThread();
  return category;
}
S<Type_ProcessThread> CreateType_ProcessThread(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_ProcessThread(CreateCategory_ProcessThread(), Params<0>::Type()));
  return cached;
}
S<TypeValue> CreateValue_ProcessThread(S<Type_ProcessThread> parent, const ValueTuple& args) {
  return S_get(new ExtValue_ProcessThread(parent, args));
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
