/* -----------------------------------------------------------------------------
Copyright 2021-2022 Kevin P. Barry

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

namespace {

class InheritTrace : public TraceContext {
 public:
  inline explicit InheritTrace(const TraceList& trace)
    : trace_(trace), capture_to_(this) {}

 private:
  void SetLocal(const char*) final {}

  void AppendTrace(TraceList& trace) const final{
    trace.insert(trace.end(), trace_.begin(), trace_.end());
  }

  const TraceContext* GetNext() const final { return nullptr; }

  const TraceList& trace_;
  const ScopedCapture capture_to_;
};

}  // namespace

BoxedValue CreateValue_ProcessThread(S<const Type_ProcessThread> parent, const ParamsArgs& params_args);

struct ExtCategory_ProcessThread : public Category_ProcessThread {
};

struct ExtType_ProcessThread : public Type_ProcessThread {
  inline ExtType_ProcessThread(Category_ProcessThread& p, Params<0>::Type params) : Type_ProcessThread(p, params) {}

  ReturnTuple Call_from(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("ProcessThread.from")
    return ReturnTuple(CreateValue_ProcessThread(PARAM_SELF, params_args));
  }
};

struct ExtValue_ProcessThread : public Value_ProcessThread {
  inline ExtValue_ProcessThread(S<const Type_ProcessThread> p, const ParamsArgs& params_args)
    : Value_ProcessThread(std::move(p)), routine(params_args.GetArg(0)) {}

  ReturnTuple Call_detach(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("ProcessThread.detach")
    S<std::thread> temp = thread;
    thread = nullptr;
    if (!IsJoinable(temp.get())) {
      FAIL() << "thread has not been started";
    } else {
      temp->detach();
    }
    return ReturnTuple(VAR_SELF);
  }

  ReturnTuple Call_isRunning(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("ProcessThread.isRunning")
    return ReturnTuple(Box_Bool(IsJoinable(thread.get())));
  }

  ReturnTuple Call_join(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("ProcessThread.join")
    S<std::thread> temp = thread;
    thread = nullptr;
    if (!IsJoinable(temp.get())) {
      FAIL() << "thread has not been started";
    } else {
      temp->join();
    }
    return ReturnTuple(VAR_SELF);
  }

  ReturnTuple Call_start(const ParamsArgs& params_args) final {
    const TraceList copied_trace = TraceContext::GetTrace();
    TRACE_FUNCTION("ProcessThread.start")
    if (IsJoinable(thread.get())) {
      FAIL() << "thread is already running";
    } else {
      // NOTE: Capture VAR_SELF so that the thread retains a reference while
      // it's still running. This allows the caller to hold a weak reference to
      // the thread.
      const BoxedValue self = VAR_SELF;
      thread.reset(new std::thread(
        capture_thread::ThreadCrosser::WrapCall([this,self,copied_trace] {
#ifndef DISABLE_TRACING
          InheritTrace inherit_trace(copied_trace);
#endif
          // NOTE: Don't include this in copied_trace because the latter defines
          // SetLocal as a no-op.
          TRACE_FUNCTION("ProcessThread.start")
          TRACE_CREATION
          TypeValue::Call(routine, Function_Routine_run, PassParamsArgs());
        })));
    }
    return ReturnTuple(VAR_SELF);
  }

  inline static bool IsJoinable(std::thread* thread) {
    return thread && thread->joinable();
  }

  ~ExtValue_ProcessThread() {
    S<std::thread> temp = thread;
    thread = nullptr;
    if (IsJoinable(temp.get())) {
      temp->detach();
    }
  }

  // Routine
  const BoxedValue routine;
  S<std::thread> thread;
  CAPTURE_CREATION("ProcessThread")
};

Category_ProcessThread& CreateCategory_ProcessThread() {
  static auto& category = *new ExtCategory_ProcessThread();
  return category;
}

S<const Type_ProcessThread> CreateType_ProcessThread(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_ProcessThread(CreateCategory_ProcessThread(), Params<0>::Type()));
  return cached;
}

void RemoveType_ProcessThread(const Params<0>::Type& params) {}

BoxedValue CreateValue_ProcessThread(S<const Type_ProcessThread> parent, const ParamsArgs& params_args) {
  return BoxedValue::New<ExtValue_ProcessThread>(std::move(parent), params_args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
