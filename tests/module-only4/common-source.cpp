/* -----------------------------------------------------------------------------
Copyright 2020-2021 Kevin P. Barry

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
#include "Streamlined_Type1.hpp"
#include "Streamlined_Type3.hpp"
#include "Category_Formatted.hpp"
#include "Category_String.hpp"
#include "Category_Type1.hpp"
#include "Category_Type2.hpp"
#include "Category_Type3.hpp"

#ifdef ZEOLITE_PRIVATE_NAMESPACE
namespace ZEOLITE_PRIVATE_NAMESPACE {
#endif  // ZEOLITE_PRIVATE_NAMESPACE

BoxedValue CreateValue_Type1(S<const Type_Type1> parent, const ValueTuple& args);

struct ExtCategory_Type1 : public Category_Type1 {
};

struct ExtType_Type1 : public Type_Type1 {
  inline ExtType_Type1(Category_Type1& p, Params<0>::Type params) : Type_Type1(p, params) {}

  ReturnTuple Call_create(const ParamTuple& params, const ValueTuple& args) const {
    TRACE_FUNCTION("Type1.create")
    return ReturnTuple(CreateValue_Type1(PARAM_SELF,
      TypeInstance::Call(GetType_Type2(Params<0>::Type()), Function_Type2_create, ParamTuple(), ArgTuple())));
  }
};

struct ExtValue_Type1 : public Value_Type1 {
  inline ExtValue_Type1(S<const Type_Type1> p, const ValueTuple& args)
    : Value_Type1(std::move(p)), value(args.At(0)) {}

  ReturnTuple Call_get(const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Type1.get")
    return ReturnTuple(TypeValue::Call(value, Function_Type2_get, ParamTuple(), ArgTuple()));
  }

  const BoxedValue value;
};

Category_Type1& CreateCategory_Type1() {
  static auto& category = *new ExtCategory_Type1();
  return category;
}

S<const Type_Type1> CreateType_Type1(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_Type1(CreateCategory_Type1(), Params<0>::Type()));
  return cached;
}

BoxedValue CreateValue_Type1(S<const Type_Type1> parent, const ValueTuple& args) {
  return BoxedValue::New<ExtValue_Type1>(std::move(parent), args);
}

#ifdef ZEOLITE_PRIVATE_NAMESPACE
}  // namespace ZEOLITE_PRIVATE_NAMESPACE
using namespace ZEOLITE_PRIVATE_NAMESPACE;
#endif  // ZEOLITE_PRIVATE_NAMESPACE


#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_Type3(S<const Type_Type3> parent, const ValueTuple& args);

struct ExtCategory_Type3 : public Category_Type3 {
};

struct ExtType_Type3 : public Type_Type3 {
  inline ExtType_Type3(Category_Type3& p, Params<0>::Type params) : Type_Type3(p, params) {}

  ReturnTuple Call_create(const ParamTuple& params, const ValueTuple& args) const {
    TRACE_FUNCTION("Type3.create")
    return ReturnTuple(CreateValue_Type3(PARAM_SELF,
      TypeInstance::Call(GetType_Type2(Params<0>::Type()), Function_Type2_create, ParamTuple(), ArgTuple())));
  }
};

struct ExtValue_Type3 : public Value_Type3 {
  inline ExtValue_Type3(S<const Type_Type3> p, const ValueTuple& args)
    : Value_Type3(std::move(p)), value(args.At(0)) {}

  ReturnTuple Call_get(const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Type3.get")
    return ReturnTuple(TypeValue::Call(value, Function_Type2_get, ParamTuple(), ArgTuple()));
  }

  const BoxedValue value;
};

Category_Type3& CreateCategory_Type3() {
  static auto& category = *new ExtCategory_Type3();
  return category;
}

S<const Type_Type3> CreateType_Type3(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_Type3(CreateCategory_Type3(), Params<0>::Type()));
  return cached;
}

BoxedValue CreateValue_Type3(S<const Type_Type3> parent, const ValueTuple& args) {
  return BoxedValue::New<ExtValue_Type3>(std::move(parent), args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
