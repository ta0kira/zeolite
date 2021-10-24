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

#include <math.h>
#include <pthread.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>

#include "category-source.hpp"
#include "Streamlined_ThreadCondition.hpp"
#include "Category_ThreadCondition.hpp"
#include "Category_ConditionResume.hpp"
#include "Category_ConditionWait.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_ThreadCondition(S<Type_ThreadCondition> parent);

struct ExtCategory_ThreadCondition : public Category_ThreadCondition {
};

struct ExtType_ThreadCondition : public Type_ThreadCondition {
  inline ExtType_ThreadCondition(Category_ThreadCondition& p, Params<0>::Type params) : Type_ThreadCondition(p, params) {}

  ReturnTuple Call_new(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ThreadCondition.new")
    return ReturnTuple(CreateValue_ThreadCondition(shared_from_this()));
  }
};

struct ExtValue_ThreadCondition : public Value_ThreadCondition {
  inline ExtValue_ThreadCondition(S<Type_ThreadCondition> p) : Value_ThreadCondition(p) {
    pthread_mutexattr_t mutex_attr;
    pthread_mutexattr_init(&mutex_attr);
    // NOTE: Error checking is required to catch attempted waits without first
    // locking the mutex.
    pthread_mutexattr_settype(&mutex_attr, PTHREAD_MUTEX_ERRORCHECK);
    pthread_mutex_init(&mutex, &mutex_attr);
  }

  ReturnTuple Call_lock(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ThreadCondition.lock")
    int error = 0;
    if ((error = pthread_mutex_lock(&mutex)) != 0) {
      FailError("Error locking mutex", error);
    }
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_resumeAll(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ThreadCondition.resumeAll")
    int error = 0;
    if ((error = pthread_cond_broadcast(&cond)) != 0) {
      FailError("Error resuming threads", error);
    }
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_resumeOne(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ThreadCondition.resumeOne")
    int error = 0;
    if ((error = pthread_cond_signal(&cond)) != 0) {
      FailError("Error resuming thread", error);
    }
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_timedWait(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ThreadCondition.timedWait")
    int error = 0;
    const PrimFloat Var_arg1 = (args.At(0)).AsFloat();
    if (Var_arg1 < 0) {
      FAIL() << "Bad wait time " << Var_arg1;
    }
    struct timeval now;
    if (gettimeofday(&now, NULL) != 0) {
      FailError("Error getting current time", errno);
    }
    const PrimFloat abs_time = Var_arg1 + (PrimFloat) now.tv_sec + ((PrimFloat) now.tv_usec / 1000000.0);
    struct timespec timeout{ (int) trunc(abs_time), (int) (1000000000.0 * (abs_time-trunc(abs_time))) };
    error = pthread_cond_timedwait(&cond, &mutex, &timeout);
    if (error == ETIMEDOUT) {
      return ReturnTuple(Box_Bool(false));
    }
    if (error != 0) {
      FailError("Error waiting for condition", error);
    }
    return ReturnTuple(Box_Bool(true));
  }

  ReturnTuple Call_unlock(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ThreadCondition.unlock")
    int error = 0;
    if ((error = pthread_mutex_unlock(&mutex)) != 0) {
      FailError("Error unlocking mutex", error);
    }
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_wait(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("ThreadCondition.wait")
    int error = 0;
    if ((error = pthread_cond_wait(&cond, &mutex)) != 0) {
      FailError("Error waiting for condition", error);
    }
    return ReturnTuple(Var_self);
  }

  void FailError(const std::string& context, int error) const {
    TRACE_CREATION
    FAIL() << context << ": " << strerror(error) << " (error " << error << ")";
  }

  ~ExtValue_ThreadCondition() {
    int error = 0;
    // Nothing should be waiting on the condition, because then there would
    // still be a reference to this object.
    if ((error = pthread_cond_destroy(&cond)) != 0) {
      FailError("Error cleaning up condition", error);
    }
    if ((error = pthread_mutex_destroy(&mutex)) != 0) {
      FailError("Error cleaning up mutex", error);
    }
  }

  pthread_cond_t  cond  = PTHREAD_COND_INITIALIZER;
  pthread_mutex_t mutex;
  CAPTURE_CREATION("ThreadCondition")
};

Category_ThreadCondition& CreateCategory_ThreadCondition() {
  static auto& category = *new ExtCategory_ThreadCondition();
  return category;
}

S<Type_ThreadCondition> CreateType_ThreadCondition(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_ThreadCondition(CreateCategory_ThreadCondition(), Params<0>::Type()));
  return cached;
}

BoxedValue CreateValue_ThreadCondition(S<Type_ThreadCondition> parent) {
  return BoxedValue::New<ExtValue_ThreadCondition>(parent);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
