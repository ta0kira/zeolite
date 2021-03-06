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
#include "Category_Equals.hpp"
#include "Category_Float.hpp"
#include "Category_Formatted.hpp"
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

  ReturnTuple Call_maxBound(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Char.maxBound")
    return ReturnTuple(Box_Char('\xff'));
  }

  ReturnTuple Call_minBound(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Char.minBound")
    return ReturnTuple(Box_Char('\0'));
  }

  ReturnTuple Call_default(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Char.default")
    return ReturnTuple(Box_Char('\0'));
  }

  ReturnTuple Call_equals(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Char.equals")
    const PrimChar Var_arg1 = (args.At(0)).AsChar();
    const PrimChar Var_arg2 = (args.At(1)).AsChar();
    return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
  }

  ReturnTuple Call_lessThan(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Char.lessThan")
    const PrimChar Var_arg1 = (args.At(0)).AsChar();
    const PrimChar Var_arg2 = (args.At(1)).AsChar();
    return ReturnTuple(Box_Bool(Var_arg1<Var_arg2));
  }
};

ReturnTuple DispatchChar(PrimChar value, const BoxedValue& Var_self, const ValueFunction& label,
                         const ParamTuple& params, const ValueTuple& args) {
  if (&label == &Function_AsBool_asBool) {
    TRACE_FUNCTION("Char.asBool")
    return ReturnTuple(Box_Bool(value != '\0'));
  }
  if (&label == &Function_AsChar_asChar) {
    TRACE_FUNCTION("Char.asChar")
    return ReturnTuple(Var_self);
  }
  if (&label == &Function_AsFloat_asFloat) {
    TRACE_FUNCTION("Char.asFloat")
    return ReturnTuple(Box_Float(((int) value + 256) % 256));
  }
  if (&label == &Function_AsInt_asInt) {
    TRACE_FUNCTION("Char.asInt")
    return ReturnTuple(Box_Int(((int) value + 256) % 256));
  }
  if (&label == &Function_Formatted_formatted) {
    TRACE_FUNCTION("Char.formatted")
    return ReturnTuple(Box_String(PrimString(1,value)));
  }
  FAIL() << "Char does not implement " << label;
  __builtin_unreachable();
}

Category_Char& CreateCategory_Char() {
  static auto& category = *new ExtCategory_Char();
  return category;
}
S<Type_Char> CreateType_Char(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_Char(CreateCategory_Char(), Params<0>::Type()));
  return cached;
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue Box_Char(PrimChar value) {
  return BoxedValue(value);
}
