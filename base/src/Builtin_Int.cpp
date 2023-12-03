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

#include <climits>
#include <string>

#include "category-source.hpp"
#include "Streamlined_Int.hpp"
#include "Category_AsBool.hpp"
#include "Category_AsChar.hpp"
#include "Category_AsFloat.hpp"
#include "Category_AsInt.hpp"
#include "Category_Bool.hpp"
#include "Category_Char.hpp"
#include "Category_Default.hpp"
#include "Category_Duplicate.hpp"
#include "Category_Equals.hpp"
#include "Category_Float.hpp"
#include "Category_Formatted.hpp"
#include "Category_Hashed.hpp"
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

  ReturnTuple Call_maxBound(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Int.maxBound")
    return ReturnTuple(Box_Int(9223372036854775807LL));
  }

  ReturnTuple Call_minBound(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Int.minBound")
    return ReturnTuple(Box_Int(-9223372036854775807LL - 1LL));
  }

  ReturnTuple Call_default(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Int.default")
    return ReturnTuple(Box_Int(0));
  }

  ReturnTuple Call_equals(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Int.equals")
    const PrimInt Var_arg1 = (params_args.GetArg(0)).AsInt();
    const PrimInt Var_arg2 = (params_args.GetArg(1)).AsInt();
    return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
  }

  ReturnTuple Call_lessThan(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Int.lessThan")
    const PrimInt Var_arg1 = (params_args.GetArg(0)).AsInt();
    const PrimInt Var_arg2 = (params_args.GetArg(1)).AsInt();
    return ReturnTuple(Box_Bool(Var_arg1<Var_arg2));
  }
};

ReturnTuple DispatchInt(PrimInt value, const ValueFunction& label,
                        const ParamsArgs& params_args) {
  switch (label.collection) {
    case CategoryId_AsBool:
      return ReturnTuple(Box_Bool(value != 0));
    case CategoryId_AsChar:
      return ReturnTuple(Box_Char(PrimChar(((value % 256) + 256) % 256)));
    case CategoryId_AsFloat:
      return ReturnTuple(Box_Float(value));
    case CategoryId_AsInt:
      return ReturnTuple(Box_Int(value));
    case CategoryId_Duplicate:
      return ReturnTuple(Box_Int(value));
    case CategoryId_Formatted:
      return ReturnTuple(Box_String(std::to_string(value)));
    case CategoryId_Hashed:
      return ReturnTuple(Box_Int(1000000007ULL * value));
    default:
      FAIL() << "Int does not implement " << label;
      __builtin_unreachable();
  }
}

Category_Int& CreateCategory_Int() {
  static auto& category = *new ExtCategory_Int();
  return category;
}

S<const Type_Int> CreateType_Int(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Int(CreateCategory_Int(), Params<0>::Type()));
  return cached;
}

void RemoveType_Int(const Params<0>::Type& params) {}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
