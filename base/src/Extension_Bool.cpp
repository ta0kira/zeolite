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

#include "category-source.hpp"
#include "Streamlined_Bool.hpp"
#include "Category_AsBool.hpp"
#include "Category_AsFloat.hpp"
#include "Category_AsInt.hpp"
#include "Category_Bool.hpp"
#include "Category_Default.hpp"
#include "Category_Equals.hpp"
#include "Category_Float.hpp"
#include "Category_Formatted.hpp"
#include "Category_Int.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

struct ExtCategory_Bool : public Category_Bool {
};

struct ExtType_Bool : public Type_Bool {
  inline ExtType_Bool(Category_Bool& p, Params<0>::Type params) : Type_Bool(p, params) {}

  ReturnTuple Call_default(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Bool.default")
    return ReturnTuple(Box_Bool(false));
  }

  ReturnTuple Call_equals(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Bool.equals")
    const bool Var_arg1 = (args.At(0))->AsBool();
    const bool Var_arg2 = (args.At(1))->AsBool();
    return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
  }
};

struct ExtValue_Bool : public Value_Bool {
  inline ExtValue_Bool(S<Type_Bool> p, bool value) : Value_Bool(p), value_(value) {}

  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Bool.asBool")
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Bool.asFloat")
    return ReturnTuple(Box_Float(value_ ? 1.0 : 0.0));
  }

  ReturnTuple Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Bool.asInt")
    return ReturnTuple(Box_Int(value_? 1 : 0));
  }

  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Bool.formatted")
    return ReturnTuple(Box_String(value_? "true" : "false"));
  }

  bool AsBool() const final { return value_; }

  const bool value_;
};

const S<TypeValue>& Var_true = *new S<TypeValue>(new ExtValue_Bool(CreateType_Bool(Params<0>::Type()), true));
const S<TypeValue>& Var_false = *new S<TypeValue>(new ExtValue_Bool(CreateType_Bool(Params<0>::Type()), false));

Category_Bool& CreateCategory_Bool() {
  static auto& category = *new ExtCategory_Bool();
  return category;
}
S<Type_Bool> CreateType_Bool(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_Bool(CreateCategory_Bool(), Params<0>::Type()));
  return cached;
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE

S<TypeValue> Box_Bool(bool value) {
  return value? Var_true : Var_false;
}
