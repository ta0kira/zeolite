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
#include "Category_Float.hpp"
#include "Category_String.hpp"


#ifdef ZEOLITE_DYNAMIC_NAMESPACE
namespace ZEOLITE_DYNAMIC_NAMESPACE {
#endif  // ZEOLITE_DYNAMIC_NAMESPACE

namespace {

struct Impl_Category_Math : public Category_Math {};

struct Impl_Type_Math : public Type_Math {
  Impl_Type_Math(Category_Math& p, Params<0>::Type params) : Type_Math(p, std::move(params)) {}

  ReturnTuple Call_acos(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.acos")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::acos(Var_arg1)));
  }

  ReturnTuple Call_acosh(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.acosh")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::acosh(Var_arg1)));
  }

  ReturnTuple Call_asin(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.asin")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::asin(Var_arg1)));
  }

  ReturnTuple Call_asinh(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.asinh")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::asinh(Var_arg1)));
  }

  ReturnTuple Call_atan(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.atan")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::atan(Var_arg1)));
  }

  ReturnTuple Call_atanh(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.atanh")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::atanh(Var_arg1)));
  }

  ReturnTuple Call_ceil(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.ceil")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::ceil(Var_arg1)));
  }

  ReturnTuple Call_cos(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.cos")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::cos(Var_arg1)));
  }

  ReturnTuple Call_cosh(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.cosh")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::cosh(Var_arg1)));
  }

  ReturnTuple Call_exp(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.exp")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::exp(Var_arg1)));
  }

  ReturnTuple Call_fabs(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.fabs")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::fabs(Var_arg1)));
  }

  ReturnTuple Call_floor(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.floor")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::floor(Var_arg1)));
  }

  ReturnTuple Call_fmod(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.fmod")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    const PrimFloat Var_arg2 = (args.At(1))->AsFloat();
    return ReturnTuple(Box_Float(std::fmod(Var_arg1,Var_arg2)));
  }

  ReturnTuple Call_isinf(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.isinf")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Bool(std::isinf(Var_arg1)));
  }

  ReturnTuple Call_isnan(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.isnan")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Bool(std::isnan(Var_arg1)));
  }

  ReturnTuple Call_log(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.log")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::log(Var_arg1)));
  }

  ReturnTuple Call_log10(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.log10")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::log10(Var_arg1)));
  }

  ReturnTuple Call_log2(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.log2")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::log2(Var_arg1)));
  }

  ReturnTuple Call_pow(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.pow")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    const PrimFloat Var_arg2 = (args.At(1))->AsFloat();
    return ReturnTuple(Box_Float(std::pow(Var_arg1,Var_arg2)));
  }

  ReturnTuple Call_round(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.round")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::round(Var_arg1)));
  }

  ReturnTuple Call_sin(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.sin")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::sin(Var_arg1)));
  }

  ReturnTuple Call_sinh(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.sinh")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::sinh(Var_arg1)));
  }

  ReturnTuple Call_sqrt(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.sqrt")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::sqrt(Var_arg1)));
  }

  ReturnTuple Call_tan(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.tan")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::tan(Var_arg1)));
  }

  ReturnTuple Call_tanh(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.tanh")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::tanh(Var_arg1)));
  }

  ReturnTuple Call_trunc(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Math.trunc")
    const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
    return ReturnTuple(Box_Float(std::trunc(Var_arg1)));
  }
};

struct Impl_Value_Math : public Value_Math {
  Impl_Value_Math(Type_Math& p, const ParamTuple& params, const ValueTuple& args) : Value_Math(p) {}
};

}  // namespace

Category_Math& CreateCategory_Math() {
  static auto& category = *new Impl_Category_Math();
  return category;
}

Type_Math& CreateType_Math(Params<0>::Type params) {
  static auto& cached = *new Impl_Type_Math(CreateCategory_Math(), Params<0>::Type());
  return cached;
}

S<TypeValue> CreateValue_Math(Type_Math& parent, const ParamTuple& params, const ValueTuple& args) {
  return S_get(new Impl_Value_Math(parent, params, args));
}

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE
