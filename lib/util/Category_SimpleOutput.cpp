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

#include <iostream>
#include <sstream>

#include "category-source.hpp"
#include "Category_BufferedWriter.hpp"
#include "Category_Formatted.hpp"
#include "Category_SimpleOutput.hpp"
#include "Category_String.hpp"
#include "Category_Writer.hpp"
#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE
namespace {
extern const S<TypeValue>& Var_stdout;
extern const S<TypeValue>& Var_stderr;
extern const S<TypeValue>& Var_error;
const int collection_SimpleOutput = 0;
}  // namespace
const void* const Functions_SimpleOutput = &collection_SimpleOutput;
const TypeFunction& Function_SimpleOutput_error = (*new TypeFunction{ 0, 0, 1, "SimpleOutput", "error", Functions_SimpleOutput, 0 });
const TypeFunction& Function_SimpleOutput_stderr = (*new TypeFunction{ 0, 0, 1, "SimpleOutput", "stderr", Functions_SimpleOutput, 1 });
const TypeFunction& Function_SimpleOutput_stdout = (*new TypeFunction{ 0, 0, 1, "SimpleOutput", "stdout", Functions_SimpleOutput, 2 });
namespace {
class Category_SimpleOutput;
class Type_SimpleOutput;
S<Type_SimpleOutput> CreateType_SimpleOutput(Params<0>::Type params);
class Value_SimpleOutput;
class Writer;
S<TypeValue> CreateValue_SimpleOutput(S<Writer> writer);
struct Category_SimpleOutput : public TypeCategory {
  std::string CategoryName() const final { return "SimpleOutput"; }
  Category_SimpleOutput() {
    CycleCheck<Category_SimpleOutput>::Check();
    CycleCheck<Category_SimpleOutput> marker(*this);
    TRACE_FUNCTION("SimpleOutput (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_SimpleOutput::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_SimpleOutput& CreateCategory_SimpleOutput() {
  static auto& category = *new Category_SimpleOutput();
  return category;
}
struct Type_SimpleOutput : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_SimpleOutput& parent;
  bool CanConvertFrom(const S<const TypeInstance>& from) const final {
    std::vector<S<const TypeInstance>> args;
    if (!from->TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<S<const TypeInstance>>& args) const final {
    if (&category == &GetCategory_SimpleOutput()) {
      args = std::vector<S<const TypeInstance>>{};
      return true;
    }
    if (&category == &GetCategory_BufferedWriter()) {
      args = std::vector<S<const TypeInstance>>{GetType_Formatted(T_get())};
      return true;
    }
    if (&category == &GetCategory_Writer()) {
      args = std::vector<S<const TypeInstance>>{GetType_Formatted(T_get())};
      return true;
    }
    return false;
  }
  Type_SimpleOutput(Category_SimpleOutput& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_SimpleOutput>::Check();
    CycleCheck<Type_SimpleOutput> marker(*this);
    TRACE_FUNCTION("SimpleOutput (init @type)")
  }
  ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_SimpleOutput::*)(const S<TypeInstance>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_SimpleOutput[] = {
      &Type_SimpleOutput::Call_error,
      &Type_SimpleOutput::Call_stderr,
      &Type_SimpleOutput::Call_stdout,
    };
    if (label.collection == Functions_SimpleOutput) {
      if (label.function_num < 0 || label.function_num >= sizeof Table_SimpleOutput / sizeof(CallType)) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_SimpleOutput[label.function_num])(self, params, args);
    }
    return TypeInstance::Dispatch(self, label, params, args);
  }
  ReturnTuple Call_error(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_stderr(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_stdout(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args);
};
S<Type_SimpleOutput> CreateType_SimpleOutput(Params<0>::Type params) {
  static const auto cached = S_get(new Type_SimpleOutput(CreateCategory_SimpleOutput(), Params<0>::Type()));
  return cached;
}
struct Writer {
  virtual void Write(const PrimString& message) = 0;
  virtual void Flush() = 0;
  virtual ~Writer() {}
};
struct Value_SimpleOutput : public TypeValue {
  Value_SimpleOutput(S<Writer> w) : writer_(w) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_SimpleOutput::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_BufferedWriter[] = {
      &Value_SimpleOutput::Call_flush,
    };
    static const CallType Table_Writer[] = {
      &Value_SimpleOutput::Call_write,
    };
    if (label.collection == Functions_BufferedWriter) {
      if (label.function_num < 0 || label.function_num >= sizeof Table_BufferedWriter / sizeof(CallType)) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_BufferedWriter[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_Writer) {
      if (label.function_num < 0 || label.function_num >= sizeof Table_Writer / sizeof(CallType)) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_Writer[label.function_num])(self, params, args);
    }
    return TypeValue::Dispatch(self, label, params, args);
  }
  std::string CategoryName() const final { return "SimpleOutput"; }
  ReturnTuple Call_flush(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_write(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  std::mutex mutex_;
  const S<Writer> writer_;
};
S<TypeValue> CreateValue_SimpleOutput(S<Writer> writer) {
  return S_get(new Value_SimpleOutput(writer));
}
ReturnTuple Type_SimpleOutput::Call_error(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("SimpleOutput.error")
  return ReturnTuple(Var_error);
}
ReturnTuple Type_SimpleOutput::Call_stderr(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("SimpleOutput.stderr")
      return ReturnTuple(Var_stderr);
}
ReturnTuple Type_SimpleOutput::Call_stdout(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("SimpleOutput.stdout")
  return ReturnTuple(Var_stdout);
}
ReturnTuple Value_SimpleOutput::Call_flush(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("SimpleOutput.flush");
  std::lock_guard<std::mutex> lock(mutex_);
  writer_->Flush();
  return ReturnTuple();
}
ReturnTuple Value_SimpleOutput::Call_write(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("SimpleOutput.write")
  const S<TypeValue>& Var_arg1 = (args.At(0));
  std::lock_guard<std::mutex> lock(mutex_);
  writer_->Write(TypeValue::Call(args.At(0), Function_Formatted_formatted,
                                 ParamTuple(), ArgTuple()).Only()->AsString());
  return ReturnTuple();
}
class StreamWriter : public Writer {
 public:
  StreamWriter(std::ostream& output) : output_(output) {}

  void Write(const PrimString& message) final {
    output_ << message;
  }

  void Flush() final {}

 private:
  std::ostream& output_;
};
class ErrorWriter : public Writer {
 public:
  void Write(const PrimString& message) final {
    output_ << message;
  }

  void Flush() final {
    FAIL() << output_.str();
  }

  std::ostringstream output_;
};
const S<TypeValue>& Var_stdout = CreateValue_SimpleOutput(S_get(new StreamWriter(std::cout)));
const S<TypeValue>& Var_stderr = CreateValue_SimpleOutput(S_get(new StreamWriter(std::cerr)));
const S<TypeValue>& Var_error  = CreateValue_SimpleOutput(S_get(new ErrorWriter()));
}  // namespace
TypeCategory& GetCategory_SimpleOutput() {
  return CreateCategory_SimpleOutput();
}
S<TypeInstance> GetType_SimpleOutput(Params<0>::Type params) {
  return CreateType_SimpleOutput(params);
}
#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
