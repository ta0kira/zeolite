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
#include "Streamlined_Float.hpp"
#include "Category_AsBool.hpp"
#include "Category_AsFloat.hpp"
#include "Category_AsInt.hpp"
#include "Category_Bool.hpp"
#include "Category_Default.hpp"
#include "Category_Duplicate.hpp"
#include "Category_Equals.hpp"
#include "Category_Float.hpp"
#include "Category_Formatted.hpp"
#include "Category_Int.hpp"
#include "Category_LessThan.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

struct ExtCategory_Float : public Category_Float {
};

struct ExtType_Float : public Type_Float {
  inline ExtType_Float(Category_Float& p, Params<0>::Type params) : Type_Float(p, params) {}

  ReturnTuple Call_default(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Float.default")
    return ReturnTuple(Box_Float(0.0));
  }

  ReturnTuple Call_equals(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Float.equals")
    const PrimFloat Var_arg1 = (params_args.GetArg(0)).AsFloat();
    const PrimFloat Var_arg2 = (params_args.GetArg(1)).AsFloat();
    return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
  }

  ReturnTuple Call_lessThan(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Float.lessThan")
    const PrimFloat Var_arg1 = (params_args.GetArg(0)).AsFloat();
    const PrimFloat Var_arg2 = (params_args.GetArg(1)).AsFloat();
    return ReturnTuple(Box_Bool(Var_arg1<Var_arg2));
  }
};

ReturnTuple DispatchFloat(PrimFloat value, const ValueFunction& label,
                          const ParamsArgs& params_args) {
  switch (label.collection) {
    case CategoryId_AsBool:
      return ReturnTuple(Box_Bool(value != 0.0));
    case CategoryId_AsFloat:
      return ReturnTuple(Box_Float(value));
    case CategoryId_AsInt:
      return ReturnTuple(Box_Int(value));
    case CategoryId_Duplicate:
      return ReturnTuple(Box_Float(value));
    case CategoryId_Formatted: {
      // NOTE: std::to_string does weird things with significant digits.
      std::ostringstream output;
      output << value;
      return ReturnTuple(Box_String(output.str()));
    }
    default:
      FAIL() << "Float does not implement " << label;
      __builtin_unreachable();
  }
}

Category_Float& CreateCategory_Float() {
  static auto& category = *new ExtCategory_Float();
  return category;
}

S<const Type_Float> CreateType_Float(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Float(CreateCategory_Float(), Params<0>::Type()));
  return cached;
}

void RemoveType_Float(const Params<0>::Type& params) {}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue Box_Float(PrimFloat value) {
  return BoxedValue(value);
}
