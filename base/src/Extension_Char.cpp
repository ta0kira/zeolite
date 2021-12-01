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
#include "Streamlined_Char.hpp"
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

struct ExtCategory_Char : public Category_Char {
};

struct ExtType_Char : public Type_Char {
  inline ExtType_Char(Category_Char& p, Params<0>::Type params) : Type_Char(p, params) {}

  ReturnTuple Call_maxBound(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Char.maxBound")
    return ReturnTuple(Box_Char('\xff'));
  }

  ReturnTuple Call_minBound(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Char.minBound")
    return ReturnTuple(Box_Char('\0'));
  }

  ReturnTuple Call_default(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Char.default")
    return ReturnTuple(Box_Char('\0'));
  }

  ReturnTuple Call_equals(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Char.equals")
    const PrimChar Var_arg1 = (params_args.GetArg(0)).AsChar();
    const PrimChar Var_arg2 = (params_args.GetArg(1)).AsChar();
    return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
  }

  ReturnTuple Call_lessThan(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Char.lessThan")
    const PrimChar Var_arg1 = (params_args.GetArg(0)).AsChar();
    const PrimChar Var_arg2 = (params_args.GetArg(1)).AsChar();
    return ReturnTuple(Box_Bool(Var_arg1<Var_arg2));
  }
};

ReturnTuple DispatchChar(PrimChar value, const ValueFunction& label,
                         const ParamsArgs& params_args) {
  switch (label.collection) {
    case CategoryId_AsBool:
      return ReturnTuple(Box_Bool(value != '\0'));
    case CategoryId_AsChar:
      return ReturnTuple(Box_Char(value));
    case CategoryId_AsFloat:
      return ReturnTuple(Box_Float(((int) value + 256) % 256));
    case CategoryId_AsInt:
      return ReturnTuple(Box_Int(((int) value + 256) % 256));
    case CategoryId_Duplicate:
      return ReturnTuple(Box_Char(value));
    case CategoryId_Formatted:
      return ReturnTuple(Box_String(PrimString(1, value)));
    case CategoryId_Hashed:
      return ReturnTuple(Box_Int(1000000009ULL * ((unsigned long long) value + 1000000007ULL)));
    default:
      FAIL() << "Char does not implement " << label;
      __builtin_unreachable();
  }
}

Category_Char& CreateCategory_Char() {
  static auto& category = *new ExtCategory_Char();
  return category;
}

S<const Type_Char> CreateType_Char(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Char(CreateCategory_Char(), Params<0>::Type()));
  return cached;
}

void RemoveType_Char(const Params<0>::Type& params) {}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue Box_Char(PrimChar value) {
  return BoxedValue(value);
}
