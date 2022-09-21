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
#include "Streamlined_Request.hpp"
#include "Category_Message.hpp"
#include "Category_Pointer.hpp"
#include "Category_Request.hpp"
#include "Category_String.hpp"

#include "call.hpp"

#ifdef ZEOLITE_PRIVATE_NAMESPACE
namespace ZEOLITE_PRIVATE_NAMESPACE {
#endif  // ZEOLITE_PRIVATE_NAMESPACE

BoxedValue CreateValue_Request(S<const Type_Request> parent, const ParamsArgs& params_args);

struct ExtCategory_Request : public Category_Request {
};

struct ExtType_Request : public Type_Request {
  inline ExtType_Request(Category_Request& p, Params<0>::Type params) : Type_Request(p, params) {}

  ReturnTuple Call_create(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Request.create")
    return ReturnTuple(CreateValue_Request(PARAM_SELF, params_args));
  }

  ReturnTuple Call_getData(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Request.getData")
    return ReturnTuple(Box_String(params_args.GetArg(0).AsPointer<Request>()->request_data));
  }
};

struct ExtValue_Request : public Value_Request {
  inline ExtValue_Request(S<const Type_Request> p, const ParamsArgs& params_args)
    : Value_Request(std::move(p)),
      request_{params_args.GetArg(0).AsString()} {}

  ReturnTuple Call_asMessage(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("Request.asMessage")
    return ReturnTuple(Box_Pointer<Message>(&request_));
  }

  ReturnTuple Call_asRequest(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("Request.asRequest")
    return ReturnTuple(Box_Pointer<Request>(&request_));
  }

  ReturnTuple Call_formatted(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("Request.formatted")
    return ReturnTuple(Box_String(request_.request_data));
  }

  Request request_;
};

Category_Request& CreateCategory_Request() {
  static auto& category = *new ExtCategory_Request();
  return category;
}

S<const Type_Request> CreateType_Request(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Request(CreateCategory_Request(), Params<0>::Type()));
  return cached;
}

void RemoveType_Request(const Params<0>::Type& params) {}
BoxedValue CreateValue_Request(S<const Type_Request> parent, const ParamsArgs& params_args) {
  return BoxedValue::New<ExtValue_Request>(std::move(parent), params_args);
}

#ifdef ZEOLITE_PRIVATE_NAMESPACE
}  // namespace ZEOLITE_PRIVATE_NAMESPACE
using namespace ZEOLITE_PRIVATE_NAMESPACE;
#endif  // ZEOLITE_PRIVATE_NAMESPACE
