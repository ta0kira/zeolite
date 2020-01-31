/* -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

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

// Hand-written implementation of Argv.

#include "Category_Argv.hpp"

#include <iostream>
#include <sstream>

#include "category-source.hpp"


#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

namespace {
const int collection = 0;
}

const void* const Functions_Argv = &collection;

const TypeFunction& Function_Argv_global =
  *new TypeFunction{ 0, 0, 1, "Argv", "global", Functions_Argv, 0 };

namespace {

extern const S<TypeValue>& Var_global;

struct Category_Argv : public TypeCategory {
  std::string CategoryName() const final { return "Argv"; }
};

struct Type_Argv : public TypeInstance {
  std::string CategoryName() const final { return "Argv"; }
  void BuildTypeName(std::ostream& output) const final { output << CategoryName(); }

  ReturnTuple Dispatch(const TypeFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    if (args.Size() != label.arg_count) {
      FAIL() << "Wrong number of args";
    }
    if (params.Size() != label.param_count){
      FAIL() << "Wrong number of params";
    }
    if (&label == &Function_Argv_global) {
      return ReturnTuple(Var_global);
    }
    return TypeInstance::Dispatch(label, params, args);
  }
};

class Value_Argv : public TypeValue {
 public:
  std::string CategoryName() const final { return "Argv"; }

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const ValueFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    if (args.Size() != label.arg_count) {
      FAIL() << "Wrong number of args";
    }
    if (params.Size() != label.param_count){
      FAIL() << "Wrong number of params";
    }
    if (&label == &Function_ReadPosition_readPosition) {
      TRACE_FUNCTION("Argv.readPosition")
      return ReturnTuple(Box_String(Argv::GetArgAt(args.At(0)->AsInt())));
    }
    if (&label == &Function_ReadPosition_readSize) {
      TRACE_FUNCTION("Argv.readSize")
      return ReturnTuple(Box_Int(Argv::ArgCount()));
    }
    return TypeValue::Dispatch(self, label, params, args);
  }
};

const S<TypeValue>& Var_global = *new S<TypeValue>(new Value_Argv);

}  // namespace

TypeCategory& GetCategory_Argv() {
  static auto& category = *new Category_Argv();
  return category;
}

TypeInstance& GetType_Argv(Params<0>::Type) {
  static auto& instance = *new Type_Argv();
  return instance;
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
#endif  // ZEOLITE_PUBLIC_NAMESPACE
