/* -----------------------------------------------------------------------------
Copyright 2019-2022 Kevin P. Barry

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
#include "Category_Duplicate.hpp"
#include "Category_Equals.hpp"
#include "Category_Float.hpp"
#include "Category_Formatted.hpp"
#include "Category_Hashed.hpp"
#include "Category_Int.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

struct ExtCategory_Bool : public Category_Bool {
};

struct ExtType_Bool : public Type_Bool {
  inline ExtType_Bool(Category_Bool& p, Params<0>::Type params) : Type_Bool(p, params) {}

  ReturnTuple Call_default(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Bool.default")
    return ReturnTuple(Box_Bool(false));
  }

  ReturnTuple Call_equals(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Bool.equals")
    const PrimBool Var_arg1 = (params_args.GetArg(0)).AsBool();
    const PrimBool Var_arg2 = (params_args.GetArg(1)).AsBool();
    return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
  }

  ReturnTuple Call_lessThan(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Bool.lessThan")
    const PrimBool Var_arg1 = (params_args.GetArg(0)).AsBool();
    const PrimBool Var_arg2 = (params_args.GetArg(1)).AsBool();
    return ReturnTuple(Box_Bool(!Var_arg1 && Var_arg2));
  }
};

ReturnTuple DispatchBool(PrimBool value, const ValueFunction& label,
                         const ParamsArgs& params_args) {
  switch (label.collection) {
    case CategoryId_AsBool:
      return ReturnTuple(Box_Bool(value));
    case CategoryId_AsFloat:
      return ReturnTuple(Box_Float(value ? 1.0 : 0.0));
    case CategoryId_AsInt:
      return ReturnTuple(Box_Int(value? 1 : 0));
    case CategoryId_Duplicate:
      return ReturnTuple(Box_Bool(value));
    case CategoryId_Formatted:
      return ReturnTuple(Box_String(value? "true" : "false"));
    case CategoryId_Hashed:
      return ReturnTuple(Box_Int(value ? 1000000009ULL : 1000000007ULL));
    default:
      FAIL() << "Bool does not implement " << label;
      __builtin_unreachable();
  }
}

Category_Bool& CreateCategory_Bool() {
  static auto& category = *new ExtCategory_Bool();
  return category;
}

S<const Type_Bool> CreateType_Bool(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Bool(CreateCategory_Bool(), Params<0>::Type()));
  return cached;
}

void RemoveType_Bool(const Params<0>::Type& params) {}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
