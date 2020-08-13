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

#include "category-source.hpp"
#include "Category_Argv.hpp"
#include "Category_Formatted.hpp"
#include "Category_Int.hpp"
#include "Category_ReadPosition.hpp"
#include "Category_String.hpp"
#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE
namespace {
extern const S<TypeValue>& Var_global;
const int collection_Argv = 0;
}  // namespace
const void* const Functions_Argv = &collection_Argv;
const TypeFunction& Function_Argv_global = (*new TypeFunction{ 0, 0, 1, "Argv", "global", Functions_Argv, 0 });
namespace {
class Category_Argv;
class Type_Argv;
S<Type_Argv> CreateType_Argv(Params<0>::Type params);
class Value_Argv;
S<TypeValue> CreateValue_Argv(S<Type_Argv> parent, const ParamTuple& params, const ValueTuple& args);
struct Category_Argv : public TypeCategory {
  std::string CategoryName() const final { return "Argv"; }
  Category_Argv() {
    CycleCheck<Category_Argv>::Check();
    CycleCheck<Category_Argv> marker(*this);
    TRACE_FUNCTION("Argv (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_Argv::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_Argv& CreateCategory_Argv() {
  static auto& category = *new Category_Argv();
  return category;
}
struct Type_Argv : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_Argv& parent;
  bool CanConvertFrom(const S<const TypeInstance>& from) const final {
    std::vector<S<const TypeInstance>> args;
    if (!from->TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<S<const TypeInstance>>& args) const final {
    if (&category == &GetCategory_Argv()) {
      args = std::vector<S<const TypeInstance>>{};
      return true;
    }
    if (&category == &GetCategory_ReadPosition()) {
      args = std::vector<S<const TypeInstance>>{GetType_String(T_get())};
      return true;
    }
    return false;
  }
  Type_Argv(Category_Argv& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Argv>::Check();
    CycleCheck<Type_Argv> marker(*this);
    TRACE_FUNCTION("Argv (init @type)")
  }
  ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Argv::*)(const S<TypeInstance>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_Argv[] = {
      &Type_Argv::Call_global,
    };
    if (label.collection == Functions_Argv) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_Argv[label.function_num])(self, params, args);
    }
    return TypeInstance::Dispatch(self, label, params, args);
  }
  ReturnTuple Call_global(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args);
};
S<Type_Argv> CreateType_Argv(Params<0>::Type params) {
  static const auto cached = S_get(new Type_Argv(CreateCategory_Argv(), Params<0>::Type()));
  return cached;
}
struct Value_Argv : public TypeValue {
  Value_Argv(S<Type_Argv> p, const ParamTuple& params, const ValueTuple& args) : parent(p) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_Argv::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_ReadPosition[] = {
      &Value_Argv::Call_readPosition,
      &Value_Argv::Call_readSize,
      &Value_Argv::Call_subSequence,
    };
    if (label.collection == Functions_ReadPosition) {
      if (label.function_num < 0 || label.function_num >= 3) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_ReadPosition[label.function_num])(self, params, args);
    }
    return TypeValue::Dispatch(self, label, params, args);
  }
  std::string CategoryName() const final { return parent->CategoryName(); }
  ReturnTuple Call_readPosition(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_readSize(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_subSequence(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  const S<Type_Argv> parent;
};
S<TypeValue> CreateValue_Argv(S<Type_Argv> parent, const ParamTuple& params, const ValueTuple& args) {
  return S_get(new Value_Argv(parent, params, args));
}
ReturnTuple Type_Argv::Call_global(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Argv.global")
  return ReturnTuple(Var_global);
}
ReturnTuple Value_Argv::Call_readPosition(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Argv.readPosition")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  return ReturnTuple(Box_String(Argv::GetArgAt(Var_arg1)));
}
ReturnTuple Value_Argv::Call_readSize(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Argv.readSize")
  return ReturnTuple(Box_Int(Argv::ArgCount()));
}
ReturnTuple Value_Argv::Call_subSequence(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Argv.subSequence")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  const PrimInt Var_arg2 = (args.At(1))->AsInt();
  if (Var_arg1 < 0 || Var_arg1 > Argv::ArgCount()) {
    FAIL() << "Subsequence position " << Var_arg1 << " is out of bounds";
  }
  if (Var_arg2 < 0 || Var_arg1 + Var_arg2 > Argv::ArgCount()) {
    FAIL() << "Subsequence size " << Var_arg2 << " is invalid";
  }
  // TODO: This requires a general random-access data structure, or adding
  // artificial bounds on Value_Argv.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("ReadPosition.subSequence is not implemented")))
}
const S<TypeValue>& Var_global = *new S<TypeValue>(CreateValue_Argv(CreateType_Argv(Params<0>::Type()), ParamTuple(), ArgTuple()));
}  // namespace
TypeCategory& GetCategory_Argv() {
  return CreateCategory_Argv();
}
S<TypeInstance> GetType_Argv(Params<0>::Type params) {
  return CreateType_Argv(params);
}
#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
