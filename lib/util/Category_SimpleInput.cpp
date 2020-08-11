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

// TODO: Maybe use C++ instead.
#include <unistd.h>

#include "category-source.hpp"
#include "Category_SimpleInput.hpp"
#include "Category_BlockReader.hpp"


#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

namespace {
extern const S<TypeValue>& Var_stdin;

const int collection = 0;
}  // namespace

const void* const Functions_SimpleInput = &collection;
const TypeFunction& Function_SimpleInput_stdin = (*new TypeFunction{ 0, 0, 1, "SimpleInput", "stdin", Functions_SimpleInput, 0 });

namespace {
class Category_SimpleInput;
class Type_SimpleInput;
Type_SimpleInput& CreateType(Params<0>::Type params);
class Value_SimpleInput;
S<TypeValue> CreateValue(Type_SimpleInput& parent, const ParamTuple& params, const ValueTuple& args);
struct Category_SimpleInput : public TypeCategory {
  std::string CategoryName() const final { return "SimpleInput"; }
  Category_SimpleInput() {
    CycleCheck<Category_SimpleInput>::Check();
    CycleCheck<Category_SimpleInput> marker(*this);
    TRACE_FUNCTION("SimpleInput (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_SimpleInput::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_SimpleInput& CreateCategory() {
  static auto& category = *new Category_SimpleInput();
  return category;
}
struct Type_SimpleInput : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_SimpleInput& parent;
  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_SimpleInput()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    return false;
  }
  Type_SimpleInput(Category_SimpleInput& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_SimpleInput>::Check();
    CycleCheck<Type_SimpleInput> marker(*this);
    TRACE_FUNCTION("SimpleInput (init @type)")
  }
  ReturnTuple Dispatch(const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_SimpleInput::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_SimpleInput[] = {
      &Type_SimpleInput::Call_stdin,
    };
    if (label.collection == Functions_SimpleInput) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_SimpleInput[label.function_num])(params, args);
    }
    return TypeInstance::Dispatch(label, params, args);
  }
  ReturnTuple Call_stdin(const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("SimpleInput.stdin")
    return ReturnTuple(Var_stdin);
  }
};
Type_SimpleInput& CreateType(Params<0>::Type params) {
  static auto& cached = *new Type_SimpleInput(CreateCategory(), Params<0>::Type());
  return cached;
}
struct Value_SimpleInput : public TypeValue {
  Value_SimpleInput(Type_SimpleInput& p, const ParamTuple& params, const ValueTuple& args) : parent(p) {}

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const ValueFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    if (args.Size() != label.arg_count) {
      FAIL() << "Wrong number of args";
    }
    if (params.Size() != label.param_count){
      FAIL() << "Wrong number of params";
    }
    if (&label == &Function_BlockReader_readBlock) {
      TRACE_FUNCTION("SimpleInput.readBlock")
      std::lock_guard<std::mutex> lock(mutex);
      const int size = args.At(0)->AsInt();
      if (size < 0) {
        FAIL() << "Read size " << size << " is invalid";
      }
      std::string buffer(size, '\x00');
      const int read_size = read(STDIN_FILENO, &buffer[0], size);
      if (read_size < 0) {
        return ReturnTuple(Box_String(""));
      } else {
        zero_read = read_size == 0;
        return ReturnTuple(Box_String(buffer.substr(0, read_size)));
      }
    }
    if (&label == &Function_BlockReader_pastEnd) {
      TRACE_FUNCTION("SimpleInput.pastEnd")
      std::lock_guard<std::mutex> lock(mutex);
      return ReturnTuple(Box_Bool(zero_read));
    }
    return TypeValue::Dispatch(self, label, params, args);
  }

  std::string CategoryName() const final { return parent.CategoryName(); }
  bool zero_read = false;
  std::mutex mutex;
  Type_SimpleInput& parent;
};
S<TypeValue> CreateValue(Type_SimpleInput& parent, const ParamTuple& params, const ValueTuple& args) {
  return S_get(new Value_SimpleInput(parent, params, args));
}

const S<TypeValue>& Var_stdin = *new S<TypeValue>(CreateValue(CreateType(Params<0>::Type()), ParamTuple(), ArgTuple()));

}  // namespace

TypeCategory& GetCategory_SimpleInput() {
  return CreateCategory();
}
TypeInstance& GetType_SimpleInput(Params<0>::Type params) {
  return CreateType(params);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
