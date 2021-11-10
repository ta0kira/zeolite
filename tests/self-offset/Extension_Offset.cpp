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
#include "Streamlined_Offset.hpp"
#include "Category_Offset.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

namespace {

struct Base {
  std::string message;
};

}  // namespace

BoxedValue CreateValue_Offset(S<const Type_Offset> parent, const ValueTuple& args);

struct ExtCategory_Offset : public Category_Offset {
};

struct ExtType_Offset : public Type_Offset {
  inline ExtType_Offset(Category_Offset& p, Params<0>::Type params) : Type_Offset(p, params) {}

  ReturnTuple Call_new(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("Offset.new")
    return ReturnTuple(CreateValue_Offset(PARAM_SELF, ArgTuple()));
  }
};

// Using virtual will (ideally) change the offset.
struct ExtValue_Offset : public Base, virtual public Value_Offset {
  inline ExtValue_Offset(S<const Type_Offset> p, const ValueTuple& args)
    : Value_Offset(std::move(p)) {}

  ReturnTuple Call_call(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Offset.call")
    if ((void*) this == (void*) static_cast<TypeValue*>(this)) {
      message = "same";
    } else {
      message = "different";
    }
    return ReturnTuple(VAR_SELF);
  }

  ReturnTuple Call_get(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Offset.get")
    return ReturnTuple(Box_String(message));
  }
};

Category_Offset& CreateCategory_Offset() {
  static auto& category = *new ExtCategory_Offset();
  return category;
}

S<const Type_Offset> CreateType_Offset(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Offset(CreateCategory_Offset(), Params<0>::Type()));
  return cached;
}

void RemoveType_Offset(const Params<0>::Type& params) {}

BoxedValue CreateValue_Offset(S<const Type_Offset> parent, const ValueTuple& args) {
  return BoxedValue::New<ExtValue_Offset>(std::move(parent), args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
