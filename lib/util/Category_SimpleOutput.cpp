/* -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

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

// Hand-written implementation of SimpleOutput.

#include "Category_SimpleOutput.hpp"

#include <iostream>
#include <sstream>

#include "category-source.hpp"
#include "Category_Formatted.hpp"
#include "Category_Writer.hpp"
#include "Category_BufferedWriter.hpp"


#ifdef ZEOLITE_DYNAMIC_NAMESPACE
namespace ZEOLITE_DYNAMIC_NAMESPACE {
#endif  // ZEOLITE_DYNAMIC_NAMESPACE

namespace {
const int collection = 0;
}

const void* const Functions_SimpleOutput = &collection;

const TypeFunction& Function_SimpleOutput_stdout =
  *new TypeFunction{ 0, 0, 1, "SimpleOutput", "stdout", Functions_SimpleOutput, 0 };
const TypeFunction& Function_SimpleOutput_stderr =
  *new TypeFunction{ 0, 0, 1, "SimpleOutput", "stderr", Functions_SimpleOutput, 1 };
const TypeFunction& Function_SimpleOutput_error =
  *new TypeFunction{ 0, 0, 1, "SimpleOutput", "error", Functions_SimpleOutput, 2 };

namespace {

extern const S<TypeValue>& Var_stdout;
extern const S<TypeValue>& Var_stderr;
extern const S<TypeValue>& Var_error;

struct Category_SimpleOutput : public TypeCategory {
  std::string CategoryName() const final { return "SimpleOutput"; }
};

struct Type_SimpleOutput : public TypeInstance {
  std::string CategoryName() const final { return "SimpleOutput"; }
  void BuildTypeName(std::ostream& output) const final { output << CategoryName(); }

  ReturnTuple Dispatch(const TypeFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    if (args.Size() != label.arg_count) {
      FAIL() << "Wrong number of args";
    }
    if (params.Size() != label.param_count){
      FAIL() << "Wrong number of params";
    }
    if (&label == &Function_SimpleOutput_stdout) {
      return ReturnTuple(Var_stdout);
    }
    if (&label == &Function_SimpleOutput_stderr) {
      return ReturnTuple(Var_stderr);
    }
    if (&label == &Function_SimpleOutput_error) {
      return ReturnTuple(Var_error);
    }
    return TypeInstance::Dispatch(label, params, args);
  }
};

class Value_SimpleOutput : public TypeValue {
 public:
  Value_SimpleOutput(std::ostream& output) : output_(output) {}

  std::string CategoryName() const final { return "SimpleOutput"; }

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const ValueFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    if (args.Size() != label.arg_count) {
      FAIL() << "Wrong number of args";
    }
    if (params.Size() != label.param_count){
      FAIL() << "Wrong number of params";
    }
    if (&label == &Function_Writer_write) {
      TRACE_FUNCTION("SimpleOutput.write")
      output_ << TypeValue::Call(args.At(0), Function_Formatted_formatted,
                                 ParamTuple(), ArgTuple()).Only()->AsString();
      return ReturnTuple();
    }
    if (&label == &Function_BufferedWriter_flush) {
      return ReturnTuple();
    }
    return TypeValue::Dispatch(self, label, params, args);
  }

 private:
  std::ostream& output_;
};

class Value_SimpleError : public TypeValue {
 public:
  std::string CategoryName() const final { return "SimpleError"; }

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const ValueFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    if (args.Size() != label.arg_count) {
      FAIL() << "Wrong number of args";
    }
    if (params.Size() != label.param_count){
      FAIL() << "Wrong number of params";
    }
    if (&label == &Function_Writer_write) {
      TRACE_FUNCTION("SimpleOutput.write")
      output_ << TypeValue::Call(args.At(0), Function_Formatted_formatted,
                                 ParamTuple(), ArgTuple()).Only()->AsString();
      return ReturnTuple();
    }
    if (&label == &Function_BufferedWriter_flush) {
      TRACE_FUNCTION("SimpleOutput.flush")
      FAIL() << output_.str();
      return ReturnTuple();
    }
    return TypeValue::Dispatch(self, label, params, args);
  }

 private:
  std::ostringstream output_;
};

const S<TypeValue>& Var_stdout = *new S<TypeValue>(new Value_SimpleOutput(std::cout));
const S<TypeValue>& Var_stderr = *new S<TypeValue>(new Value_SimpleOutput(std::cerr));
const S<TypeValue>& Var_error  = *new S<TypeValue>(new Value_SimpleError());

}  // namespace

TypeCategory& GetCategory_SimpleOutput() {
  static auto& category = *new Category_SimpleOutput();
  return category;
}

TypeInstance& GetType_SimpleOutput(Params<0>::Type) {
  static auto& instance = *new Type_SimpleOutput();
  return instance;
}

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
#endif  // ZEOLITE_DYNAMIC_NAMESPACE
