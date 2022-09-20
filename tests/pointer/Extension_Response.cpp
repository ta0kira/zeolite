/* -----------------------------------------------------------------------------
Copyright 2022 Kevin P. Barry

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
#include "Streamlined_Response.hpp"
#include "Category_Message.hpp"
#include "Category_Pointer.hpp"
#include "Category_Response.hpp"
#include "Category_String.hpp"

#include "call.h"

#ifdef ZEOLITE_PRIVATE_NAMESPACE
namespace ZEOLITE_PRIVATE_NAMESPACE {
#endif  // ZEOLITE_PRIVATE_NAMESPACE

BoxedValue CreateValue_Response(S<const Type_Response> parent, const ParamsArgs& params_args);

struct ExtCategory_Response : public Category_Response {
};

struct ExtType_Response : public Type_Response {
  inline ExtType_Response(Category_Response& p, Params<0>::Type params) : Type_Response(p, params) {}

  ReturnTuple Call_create(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Response.create")
    return ReturnTuple(CreateValue_Response(PARAM_SELF, params_args));
  }

  ReturnTuple Call_getData(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Response.getData")
    return ReturnTuple(Box_String(params_args.GetArg(0).AsPointer<Response>()->response_data));
  }
};

struct ExtValue_Response : public Value_Response {
  inline ExtValue_Response(S<const Type_Response> p, const ParamsArgs& params_args)
    : Value_Response(std::move(p)),
      response_{params_args.GetArg(0).AsString()} {}

  ReturnTuple Call_asMessage(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("Response.asMessage")
    return ReturnTuple(Box_Pointer<Message>(&response_));
  }

  ReturnTuple Call_asResponse(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("Response.asResponse")
    return ReturnTuple(Box_Pointer<Response>(&response_));
  }

  ReturnTuple Call_formatted(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("Response.formatted")
    return ReturnTuple(Box_String(response_.response_data));
  }

  Response response_;
};

Category_Response& CreateCategory_Response() {
  static auto& category = *new ExtCategory_Response();
  return category;
}

S<const Type_Response> CreateType_Response(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Response(CreateCategory_Response(), Params<0>::Type()));
  return cached;
}

void RemoveType_Response(const Params<0>::Type& params) {}
BoxedValue CreateValue_Response(S<const Type_Response> parent, const ParamsArgs& params_args) {
  return BoxedValue::New<ExtValue_Response>(std::move(parent), params_args);
}

#ifdef ZEOLITE_PRIVATE_NAMESPACE
}  // namespace ZEOLITE_PRIVATE_NAMESPACE
using namespace ZEOLITE_PRIVATE_NAMESPACE;
#endif  // ZEOLITE_PRIVATE_NAMESPACE
