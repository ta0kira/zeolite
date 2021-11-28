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
#include "Streamlined_BasicOutput.hpp"
#include "Category_Formatted.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

namespace {
extern const BoxedValue Var_stdout;
extern const BoxedValue Var_stderr;
extern const BoxedValue Var_error;
}  // namespace

struct ExtCategory_BasicOutput : public Category_BasicOutput {
};

struct ExtType_BasicOutput : public Type_BasicOutput {
  inline ExtType_BasicOutput(Category_BasicOutput& p, Params<0>::Type params) : Type_BasicOutput(p, params) {}

  ReturnTuple Call_error(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("BasicOutput.error")
    return ReturnTuple(Var_error);
  }

  ReturnTuple Call_stderr(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("BasicOutput.stderr")
    return ReturnTuple(Var_stderr);
  }

  ReturnTuple Call_stdout(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("BasicOutput.stdout")
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

struct ExtValue_BasicOutput : public Value_BasicOutput {
  inline ExtValue_BasicOutput(S<const Type_BasicOutput> p, S<Writer> w)
    : Value_BasicOutput(std::move(p)), writer(w) {}

  ReturnTuple Call_flush(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("BasicOutput.flush")
    std::lock_guard<std::mutex> lock(mutex);
    writer->Flush();
    return ReturnTuple(VAR_SELF);
  }

  ReturnTuple Call_write(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("BasicOutput.write")
    const BoxedValue& Var_arg1 = (params_args.GetArg(0));
    std::lock_guard<std::mutex> lock(mutex);
    writer->Write(TypeValue::Call(params_args.GetArg(0), Function_Formatted_formatted,
                                  PassParamsArgs()).At(0).AsString());
    return ReturnTuple(VAR_SELF);
  }

  ReturnTuple Call_writeNow(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("BasicOutput.writeNow")
    (void) Call_write(params_args);
    (void) Call_flush(PassParamsArgs());
    return ReturnTuple(VAR_SELF);
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

const BoxedValue Var_stdout = BoxedValue::New<ExtValue_BasicOutput>(CreateType_BasicOutput(Params<0>::Type()), S_get(new StreamWriter(std::cout)));
const BoxedValue Var_stderr = BoxedValue::New<ExtValue_BasicOutput>(CreateType_BasicOutput(Params<0>::Type()), S_get(new StreamWriter(std::cerr)));
const BoxedValue Var_error  = BoxedValue::New<ExtValue_BasicOutput>(CreateType_BasicOutput(Params<0>::Type()), S_get(new ErrorWriter()));

}  // namespace

Category_BasicOutput& CreateCategory_BasicOutput() {
  static auto& category = *new ExtCategory_BasicOutput();
  return category;
}

S<const Type_BasicOutput> CreateType_BasicOutput(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_BasicOutput(CreateCategory_BasicOutput(), Params<0>::Type()));
  return cached;
}

void RemoveType_BasicOutput(const Params<0>::Type& params) {}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
