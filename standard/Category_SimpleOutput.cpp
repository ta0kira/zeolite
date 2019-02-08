// Hand-written implementation of SimpleOutput.

#include "Category_SimpleOutput.hpp"

#include <iostream>
#include <sstream>

#include "category-source.hpp"
#include "Category_Writer.hpp"
#include "Category_BufferedWriter.hpp"

const Function<SymbolScope::TYPE,0,0,1>& Function_SimpleOutput_stdout =
  *new Function<SymbolScope::TYPE,0,0,1>("SimpleOutput", "stdout");
const Function<SymbolScope::TYPE,0,0,1>& Function_SimpleOutput_stderr =
  *new Function<SymbolScope::TYPE,0,0,1>("SimpleOutput", "stderr");
const Function<SymbolScope::TYPE,0,0,1>& Function_SimpleOutput_fail =
  *new Function<SymbolScope::TYPE,0,0,1>("SimpleOutput", "fail");

namespace {

extern const S<TypeValue>& Var_stdout;
extern const S<TypeValue>& Var_stderr;
extern const S<TypeValue>& Var_fail;

struct Category_SimpleOutput : public TypeCategory {
  std::string CategoryName() const final { return "SimpleOutput"; }
};

struct Type_SimpleOutput : public TypeInstance {
  std::string CategoryName() const final { return "SimpleOutput"; }

  ReturnTuple Dispatch(const DFunction<SymbolScope::TYPE>& label,
                       const ParamTuple& params, ValueTuple& args) final {
    FAIL_IF(args.Size() != label.ArgCount());
    FAIL_IF(params.Size() != label.ParamCount());
    if (&label == &Function_SimpleOutput_stdout) {
      return ReturnTuple(Var_stdout);
    }
    if (&label == &Function_SimpleOutput_stderr) {
      return ReturnTuple(Var_stderr);
    }
    if (&label == &Function_SimpleOutput_fail) {
      return ReturnTuple(Var_fail);
    }
    return TypeInstance::Dispatch(label, params, args);
  }
};

class Value_SimpleOutput : public TypeValue {
 public:
  Value_SimpleOutput(std::ostream& output) : output_(output) {}

  std::string CategoryName() const final { return "SimpleOutput"; }

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const DFunction<SymbolScope::VALUE>& label,
                       const ParamTuple& params, ValueTuple& args) final {
    FAIL_IF(args.Size() != label.ArgCount());
    FAIL_IF(params.Size() != label.ParamCount());
    if (&label == &Function_Writer_write) {
      TRACE_FUNCTION("SimpleOutput.write")
      output_ << args.At(0)->AsString();
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

class Value_Fail : public TypeValue {
 public:
  std::string CategoryName() const final { return "SimpleOutput"; }

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const DFunction<SymbolScope::VALUE>& label,
                       const ParamTuple& params, ValueTuple& args) final {
    FAIL_IF(args.Size() != label.ArgCount());
    FAIL_IF(params.Size() != label.ParamCount());
    if (&label == &Function_Writer_write) {
      TRACE_FUNCTION("SimpleOutput.write")
      output_ << args.At(0)->AsString();
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
const S<TypeValue>& Var_fail   = *new S<TypeValue>(new Value_Fail());

}  // namespace

TypeCategory& GetCategory_SimpleOutput() {
  static auto& category = *new Category_SimpleOutput();
  return category;
}

TypeInstance& GetType_SimpleOutput(Params<0>::Type) {
  static auto& instance = *new Type_SimpleOutput();
  return instance;
}
