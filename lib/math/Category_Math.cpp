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
#include "Category_Float.hpp"
#include "Category_Formatted.hpp"
#include "Category_Math.hpp"
#include "Category_String.hpp"

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
namespace {
class Category_Math;
class Type_Math;
Type_Math& CreateType_Math(Params<0>::Type params);
class Value_Math;
S<TypeValue> CreateValue_Math(Type_Math& parent, const ParamTuple& params, const ValueTuple& args);
struct Category_Math : public TypeCategory {
  std::string CategoryName() const final { return "Math"; }
  Category_Math() {
    CycleCheck<Category_Math>::Check();
    CycleCheck<Category_Math> marker(*this);
    TRACE_FUNCTION("Math (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_Math::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_Math& CreateCategory_Math() {
  static auto& category = *new Category_Math();
  return category;
}
struct Type_Math : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_Math& parent;
  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_Math()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    return false;
  }
  Type_Math(Category_Math& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Math>::Check();
    CycleCheck<Type_Math> marker(*this);
    TRACE_FUNCTION("Math (init @type)")
  }
  ReturnTuple Dispatch(const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Math::*)(const ParamTuple&, const ValueTuple&);
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
      return (this->*Table_Math[label.function_num])(params, args);
    }
    return TypeInstance::Dispatch(label, params, args);
  }
  ReturnTuple Call_acos(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_acosh(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asin(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asinh(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_atan(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_atanh(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_ceil(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_cos(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_cosh(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_exp(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_fabs(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_floor(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_fmod(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_isinf(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_isnan(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_log(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_log10(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_log2(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_pow(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_round(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_sin(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_sinh(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_sqrt(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_tan(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_tanh(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_trunc(const ParamTuple& params, const ValueTuple& args);
};
Type_Math& CreateType_Math(Params<0>::Type params) {
  static auto& cache = *new InstanceMap<0,Type_Math>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_Math(CreateCategory_Math(), params)); }
  return *cached;
}
struct Value_Math : public TypeValue {
  Value_Math(Type_Math& p, const ParamTuple& params, const ValueTuple& args) : parent(p) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_Math::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    return TypeValue::Dispatch(self, label, params, args);
  }
  std::string CategoryName() const final { return parent.CategoryName(); }
  Type_Math& parent;
};
S<TypeValue> CreateValue_Math(Type_Math& parent, const ParamTuple& params, const ValueTuple& args) {
  return S_get(new Value_Math(parent, params, args));
}

ReturnTuple Type_Math::Call_acos(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.acos")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.acos.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.acos is not implemented")))
}

ReturnTuple Type_Math::Call_acosh(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.acosh")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.acosh.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.acosh is not implemented")))
}

ReturnTuple Type_Math::Call_asin(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.asin")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.asin.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.asin is not implemented")))
}

ReturnTuple Type_Math::Call_asinh(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.asinh")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.asinh.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.asinh is not implemented")))
}

ReturnTuple Type_Math::Call_atan(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.atan")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.atan.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.atan is not implemented")))
}

ReturnTuple Type_Math::Call_atanh(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.atanh")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.atanh.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.atanh is not implemented")))
}

ReturnTuple Type_Math::Call_ceil(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.ceil")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.ceil.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.ceil is not implemented")))
}

ReturnTuple Type_Math::Call_cos(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.cos")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.cos.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.cos is not implemented")))
}

ReturnTuple Type_Math::Call_cosh(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.cosh")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.cosh.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.cosh is not implemented")))
}

ReturnTuple Type_Math::Call_exp(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.exp")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.exp.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.exp is not implemented")))
}

ReturnTuple Type_Math::Call_fabs(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.fabs")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.fabs.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.fabs is not implemented")))
}

ReturnTuple Type_Math::Call_floor(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.floor")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.floor.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.floor is not implemented")))
}

ReturnTuple Type_Math::Call_fmod(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.fmod")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  const PrimFloat Var_arg2 = (args.At(1))->AsFloat();
  // TODO: Implement Math.fmod.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.fmod is not implemented")))
}

ReturnTuple Type_Math::Call_isinf(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.isinf")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.isinf.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.isinf is not implemented")))
}

ReturnTuple Type_Math::Call_isnan(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.isnan")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.isnan.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.isnan is not implemented")))
}

ReturnTuple Type_Math::Call_log(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.log")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.log.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.log is not implemented")))
}

ReturnTuple Type_Math::Call_log10(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.log10")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.log10.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.log10 is not implemented")))
}

ReturnTuple Type_Math::Call_log2(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.log2")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.log2.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.log2 is not implemented")))
}

ReturnTuple Type_Math::Call_pow(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.pow")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  const PrimFloat Var_arg2 = (args.At(1))->AsFloat();
  // TODO: Implement Math.pow.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.pow is not implemented")))
}

ReturnTuple Type_Math::Call_round(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.round")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.round.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.round is not implemented")))
}

ReturnTuple Type_Math::Call_sin(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.sin")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.sin.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.sin is not implemented")))
}

ReturnTuple Type_Math::Call_sinh(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.sinh")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.sinh.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.sinh is not implemented")))
}

ReturnTuple Type_Math::Call_sqrt(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.sqrt")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.sqrt.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.sqrt is not implemented")))
}

ReturnTuple Type_Math::Call_tan(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.tan")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.tan.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.tan is not implemented")))
}

ReturnTuple Type_Math::Call_tanh(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.tanh")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.tanh.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.tanh is not implemented")))
}

ReturnTuple Type_Math::Call_trunc(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Math.trunc")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  // TODO: Implement Math.trunc.
  BUILTIN_FAIL(Box_String(PrimString_FromLiteral("Math.trunc is not implemented")))
}

}  // namespace

TypeCategory& GetCategory_Math() {
  return CreateCategory_Math();
}
TypeInstance& GetType_Math(Params<0>::Type params) {
  return CreateType_Math(params);
}

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE
