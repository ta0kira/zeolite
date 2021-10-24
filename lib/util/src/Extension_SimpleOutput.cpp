/* -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

#include <iostream>
#include <mutex>
#include <sstream>

#include "category-source.hpp"
#include "Streamlined_SimpleOutput.hpp"
#include "Category_Formatted.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

namespace {
extern const BoxedValue& Var_stdout;
extern const BoxedValue& Var_stderr;
extern const BoxedValue& Var_error;
}  // namespace

struct ExtCategory_SimpleOutput : public Category_SimpleOutput {
};

struct ExtType_SimpleOutput : public Type_SimpleOutput {
  inline ExtType_SimpleOutput(Category_SimpleOutput& p, Params<0>::Type params) : Type_SimpleOutput(p, params) {}

  ReturnTuple Call_error(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleOutput.error")
    return ReturnTuple(Var_error);
  }

  ReturnTuple Call_stderr(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleOutput.stderr")
    return ReturnTuple(Var_stderr);
  }

  ReturnTuple Call_stdout(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleOutput.stdout")
    return ReturnTuple(Var_stdout);
  }
};

namespace {
struct Writer {
  virtual void Write(const PrimString& message) = 0;
  virtual void Flush() = 0;
  virtual ~Writer() {}
};
}  // namespace

struct ExtValue_SimpleOutput : public Value_SimpleOutput {
  inline ExtValue_SimpleOutput(S<Type_SimpleOutput> p, S<Writer> w)
    : Value_SimpleOutput(p), writer(w) {}

  ReturnTuple Call_flush(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleOutput.flush")
    std::lock_guard<std::mutex> lock(mutex);
    writer->Flush();
    return ReturnTuple();
  }

  ReturnTuple Call_write(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleOutput.write")
    const BoxedValue& Var_arg1 = (args.At(0));
    std::lock_guard<std::mutex> lock(mutex);
    writer->Write(TypeValue::Call(args.At(0), Function_Formatted_formatted,
                                  ParamTuple(), ArgTuple()).Only().AsString());
    return ReturnTuple();
  }

  std::mutex mutex;
  const S<Writer> writer;
};

namespace {

class StreamWriter : public Writer {
 public:
  StreamWriter(std::ostream& o) : output(o) {}

  void Write(const PrimString& message) final {
    output << message;
  }

  void Flush() final {}

 private:
  std::ostream& output;
};

class ErrorWriter : public Writer {
 public:
  void Write(const PrimString& message) final {
    output << message;
  }

  void Flush() final {
    FAIL() << output.str();
  }

  std::ostringstream output;
};

const BoxedValue& Var_stdout = *new BoxedValue(BoxedValue::New<ExtValue_SimpleOutput>(CreateType_SimpleOutput(Params<0>::Type()), S_get(new StreamWriter(std::cout))));
const BoxedValue& Var_stderr = *new BoxedValue(BoxedValue::New<ExtValue_SimpleOutput>(CreateType_SimpleOutput(Params<0>::Type()), S_get(new StreamWriter(std::cerr))));
const BoxedValue& Var_error  = *new BoxedValue(BoxedValue::New<ExtValue_SimpleOutput>(CreateType_SimpleOutput(Params<0>::Type()), S_get(new ErrorWriter())));

}  // namespace

Category_SimpleOutput& CreateCategory_SimpleOutput() {
  static auto& category = *new ExtCategory_SimpleOutput();
  return category;
}

S<Type_SimpleOutput> CreateType_SimpleOutput(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_SimpleOutput(CreateCategory_SimpleOutput(), Params<0>::Type()));
  return cached;
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
