/* -----------------------------------------------------------------------------
Copyright 2021 Kevin P. Barry

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
#include "Streamlined_Extension.hpp"
#include "Category_Default.hpp"
#include "Category_Extension.hpp"
#include "Category_Formatted.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PRIVATE_NAMESPACE
namespace ZEOLITE_PRIVATE_NAMESPACE {
#endif  // ZEOLITE_PRIVATE_NAMESPACE

BoxedValue CreateValue_Extension(S<const Type_Extension> parent, const ParamsArgs& params_args);

struct ExtCategory_Extension : public Category_Extension {
};

struct ExtType_Extension : public Type_Extension {
  inline ExtType_Extension(Category_Extension& p, Params<0>::Type params) : Type_Extension(p, params) {}

  ReturnTuple Call_default(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Extension.default")
    return ReturnTuple(CreateValue_Extension(PARAM_SELF, PassParamsArgs(Box_String("message"))));
  }

  ReturnTuple Call_execute(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Extension.execute")
    BoxedValue value = TypeInstance::Call(PARAM_SELF, Function_Default_default, PassParamsArgs()).At(0);
    return TypeValue::Call(value, Function_Formatted_formatted, PassParamsArgs());
  }
};

struct ExtValue_Extension : public Value_Extension {
  inline ExtValue_Extension(S<const Type_Extension> p, const ParamsArgs& params_args)
    : Value_Extension(std::move(p)), value_(params_args.GetArg(0).AsString()) {}

  ReturnTuple Call_formatted(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("Extension.formatted")
    return ReturnTuple(Box_String(value_));
  }

  const PrimString value_;
};

Category_Extension& CreateCategory_Extension() {
  static auto& category = *new ExtCategory_Extension();
  return category;
}

S<const Type_Extension> CreateType_Extension(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Extension(CreateCategory_Extension(), Params<0>::Type()));
  return cached;
}

void RemoveType_Extension(const Params<0>::Type& params) {}
BoxedValue CreateValue_Extension(S<const Type_Extension> parent, const ParamsArgs& params_args) {
  return BoxedValue::New<ExtValue_Extension>(std::move(parent), params_args);
}

#ifdef ZEOLITE_PRIVATE_NAMESPACE
}  // namespace ZEOLITE_PRIVATE_NAMESPACE
using namespace ZEOLITE_PRIVATE_NAMESPACE;
#endif  // ZEOLITE_PRIVATE_NAMESPACE
