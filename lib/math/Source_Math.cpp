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

#include <cmath>

#include "Source_Math.hpp"


#ifdef ZEOLITE_DYNAMIC_NAMESPACE
namespace ZEOLITE_DYNAMIC_NAMESPACE {
#endif  // ZEOLITE_DYNAMIC_NAMESPACE

namespace {
const int collection_Math = 0;
}  // namespace
const void* const Functions_Math = &collection_Math;
const TypeFunction& Function_Math_acos = (*new TypeFunction{ 0, 1, 1, "Math", "acos", Functions_Math, 0 });
const TypeFunction& Function_Math_acosh = (*new TypeFunction{ 0, 1, 1, "Math", "acosh", Functions_Math, 1 });
const TypeFunction& Function_Math_asin = (*new TypeFunction{ 0, 1, 1, "Math", "asin", Functions_Math, 2 });
const TypeFunction& Function_Math_asinh = (*new TypeFunction{ 0, 1, 1, "Math", "asinh", Functions_Math, 3 });
const TypeFunction& Function_Math_atan = (*new TypeFunction{ 0, 1, 1, "Math", "atan", Functions_Math, 4 });
const TypeFunction& Function_Math_atanh = (*new TypeFunction{ 0, 1, 1, "Math", "atanh", Functions_Math, 5 });
const TypeFunction& Function_Math_ceil = (*new TypeFunction{ 0, 1, 1, "Math", "ceil", Functions_Math, 6 });
const TypeFunction& Function_Math_cos = (*new TypeFunction{ 0, 1, 1, "Math", "cos", Functions_Math, 7 });
const TypeFunction& Function_Math_cosh = (*new TypeFunction{ 0, 1, 1, "Math", "cosh", Functions_Math, 8 });
const TypeFunction& Function_Math_exp = (*new TypeFunction{ 0, 1, 1, "Math", "exp", Functions_Math, 9 });
const TypeFunction& Function_Math_fabs = (*new TypeFunction{ 0, 1, 1, "Math", "fabs", Functions_Math, 10 });
const TypeFunction& Function_Math_floor = (*new TypeFunction{ 0, 1, 1, "Math", "floor", Functions_Math, 11 });
const TypeFunction& Function_Math_fmod = (*new TypeFunction{ 0, 2, 1, "Math", "fmod", Functions_Math, 12 });
const TypeFunction& Function_Math_isinf = (*new TypeFunction{ 0, 1, 1, "Math", "isinf", Functions_Math, 13 });
const TypeFunction& Function_Math_isnan = (*new TypeFunction{ 0, 1, 1, "Math", "isnan", Functions_Math, 14 });
const TypeFunction& Function_Math_log = (*new TypeFunction{ 0, 1, 1, "Math", "log", Functions_Math, 15 });
const TypeFunction& Function_Math_log10 = (*new TypeFunction{ 0, 1, 1, "Math", "log10", Functions_Math, 16 });
const TypeFunction& Function_Math_log2 = (*new TypeFunction{ 0, 1, 1, "Math", "log2", Functions_Math, 17 });
const TypeFunction& Function_Math_pow = (*new TypeFunction{ 0, 2, 1, "Math", "pow", Functions_Math, 18 });
const TypeFunction& Function_Math_round = (*new TypeFunction{ 0, 1, 1, "Math", "round", Functions_Math, 19 });
const TypeFunction& Function_Math_sin = (*new TypeFunction{ 0, 1, 1, "Math", "sin", Functions_Math, 20 });
const TypeFunction& Function_Math_sinh = (*new TypeFunction{ 0, 1, 1, "Math", "sinh", Functions_Math, 21 });
const TypeFunction& Function_Math_sqrt = (*new TypeFunction{ 0, 1, 1, "Math", "sqrt", Functions_Math, 22 });
const TypeFunction& Function_Math_tan = (*new TypeFunction{ 0, 1, 1, "Math", "tan", Functions_Math, 23 });
const TypeFunction& Function_Math_tanh = (*new TypeFunction{ 0, 1, 1, "Math", "tanh", Functions_Math, 24 });
const TypeFunction& Function_Math_trunc = (*new TypeFunction{ 0, 1, 1, "Math", "trunc", Functions_Math, 25 });

std::string Category_Math::CategoryName() const { return "Math"; }

Category_Math::Category_Math() {
  CycleCheck<Category_Math>::Check();
  CycleCheck<Category_Math> marker(*this);
  TRACE_FUNCTION("Math (init @category)")
}

ReturnTuple Category_Math::Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) {
  using CallType = ReturnTuple(Category_Math::*)(const ParamTuple&, const ValueTuple&);
  return TypeCategory::Dispatch(label, params, args);
}

Type_Math::Type_Math(Category_Math& p, Params<0>::Type params) : parent(p) {
  CycleCheck<Type_Math>::Check();
  CycleCheck<Type_Math> marker(*this);
  TRACE_FUNCTION("Math (init @type)")
}

std::string Type_Math::CategoryName() const { return parent.CategoryName(); }

void Type_Math::BuildTypeName(std::ostream& output) const {
  return TypeInstance::TypeNameFrom(output, parent);
}

bool Type_Math::CanConvertFrom(const TypeInstance& from) const {
  std::vector<S<const TypeInstance>> args;
  if (!from.TypeArgsForParent(parent, args)) return false;
  if(args.size() != 0) {
    FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
  }
  return true;
}

bool Type_Math::TypeArgsForParent(const TypeCategory& category, std::vector<S<const TypeInstance>>& args) const {
  if (&category == &GetCategory_Math()) {
    args = std::vector<S<const TypeInstance>>{};
    return true;
  }
  return false;
}

ReturnTuple Type_Math::Dispatch(const S<TypeInstance>& self, const TypeFunction& label,
                                const ParamTuple& params, const ValueTuple& args) {
  using CallType = ReturnTuple(Type_Math::*)(const S<TypeInstance>&, const ParamTuple&, const ValueTuple&);
  static const CallType Table_Math[] = {
    &Type_Math::Call_acos,
    &Type_Math::Call_acosh,
    &Type_Math::Call_asin,
    &Type_Math::Call_asinh,
    &Type_Math::Call_atan,
    &Type_Math::Call_atanh,
    &Type_Math::Call_ceil,
    &Type_Math::Call_cos,
    &Type_Math::Call_cosh,
    &Type_Math::Call_exp,
    &Type_Math::Call_fabs,
    &Type_Math::Call_floor,
    &Type_Math::Call_fmod,
    &Type_Math::Call_isinf,
    &Type_Math::Call_isnan,
    &Type_Math::Call_log,
    &Type_Math::Call_log10,
    &Type_Math::Call_log2,
    &Type_Math::Call_pow,
    &Type_Math::Call_round,
    &Type_Math::Call_sin,
    &Type_Math::Call_sinh,
    &Type_Math::Call_sqrt,
    &Type_Math::Call_tan,
    &Type_Math::Call_tanh,
    &Type_Math::Call_trunc,
  };
  if (label.collection == Functions_Math) {
    if (label.function_num < 0 || label.function_num >= 26) {
      FAIL() << "Bad function call " << label;
    }
    return (this->*Table_Math[label.function_num])(self, params, args);
  }
  return TypeInstance::Dispatch(self, label, params, args);
}

Value_Math::Value_Math(S<Type_Math> p) : parent(p) {}

ReturnTuple Value_Math::Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) {
  using CallType = ReturnTuple(Value_Math::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
  return TypeValue::Dispatch(self, label, params, args);
}

std::string Value_Math::CategoryName() const { return parent->CategoryName(); }

TypeCategory& GetCategory_Math() {
  return CreateCategory_Math();
}
S<TypeInstance> GetType_Math(Params<0>::Type params) {
  return CreateType_Math(params);
}

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE
