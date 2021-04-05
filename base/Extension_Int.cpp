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

#include <sstream>

#include "category-source.hpp"
#include "Streamlined_Int.hpp"
#include "Category_AsBool.hpp"
#include "Category_AsChar.hpp"
#include "Category_AsFloat.hpp"
#include "Category_AsInt.hpp"
#include "Category_Bool.hpp"
#include "Category_Char.hpp"
#include "Category_Default.hpp"
#include "Category_Equals.hpp"
#include "Category_Float.hpp"
#include "Category_Formatted.hpp"
#include "Category_Int.hpp"
#include "Category_LessThan.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

struct ExtCategory_Int : public Category_Int {
};

struct ExtType_Int : public Type_Int {
  inline ExtType_Int(Category_Int& p, Params<0>::Type params) : Type_Int(p, params) {}

  ReturnTuple Call_default(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Int.default")
    return ReturnTuple(Box_Int(0));
  }

  ReturnTuple Call_equals(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Int.equals")
    const PrimInt Var_arg1 = (args.At(0))->AsInt();
    const PrimInt Var_arg2 = (args.At(1))->AsInt();
    return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
  }

  ReturnTuple Call_lessThan(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Int.lessThan")
    const PrimInt Var_arg1 = (args.At(0))->AsInt();
    const PrimInt Var_arg2 = (args.At(1))->AsInt();
    return ReturnTuple(Box_Bool(Var_arg1<Var_arg2));
  }
};

struct ExtValue_Int : public Value_Int {
  inline ExtValue_Int(S<Type_Int> p, PrimInt value) : Value_Int(p), value_(value) {}

  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Int.asBool")
    return ReturnTuple(Box_Bool(value_ != 0));
  }

  ReturnTuple Call_asChar(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Int.asChar")
    return ReturnTuple(Box_Char(value_ % 0xff));
  }

  ReturnTuple Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Int.asFloat")
    return ReturnTuple(Box_Float(value_));
  }

  ReturnTuple Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Int.asInt")
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Int.formatted")
    std::ostringstream output;
    output << value_;
    return ReturnTuple(Box_String(output.str()));
  }

  PrimInt AsInt() const final { return value_; }

  const PrimInt value_;
};

Category_Int& CreateCategory_Int() {
  static auto& category = *new ExtCategory_Int();
  return category;
}
S<Type_Int> CreateType_Int(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_Int(CreateCategory_Int(), Params<0>::Type()));
  return cached;
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE

S<TypeValue> Box_Int(PrimInt value) {
  return S_get(new ExtValue_Int(CreateType_Int(Params<0>::Type()), value));
}
