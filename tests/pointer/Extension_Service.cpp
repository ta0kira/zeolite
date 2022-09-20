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
#include "Streamlined_Service.hpp"
#include "Category_Request.hpp"
#include "Category_Response.hpp"
#include "Category_Service.hpp"

#include "call.h"

#ifdef ZEOLITE_PRIVATE_NAMESPACE
namespace ZEOLITE_PRIVATE_NAMESPACE {
#endif  // ZEOLITE_PRIVATE_NAMESPACE

BoxedValue CreateValue_Service(S<const Type_Service> parent, const ParamsArgs& params_args);

struct ExtCategory_Service : public Category_Service {
};

struct ExtType_Service : public Type_Service {
  inline ExtType_Service(Category_Service& p, Params<0>::Type params) : Type_Service(p, params) {}

  ReturnTuple Call_send(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Service.send")
    const Request* const request = params_args.GetArg(0).AsPointer<Request>();
    Response* const response = params_args.GetArg(1).AsPointer<Response>();
    if (!request) FAIL() << "Invalid Pointer<Request>";
    if (!response) FAIL() << "Invalid Pointer<Response>";
    response->response_data = request->request_data + " has been processed";
    return ReturnTuple();
  }

  ReturnTuple Call_send2(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Service.send2")
    const Message* const request = params_args.GetArg(0).AsPointer<Message>();
    Response* const response = params_args.GetArg(1).AsPointer<Response>();
    if (!request) FAIL() << "Invalid Pointer<Message>";
    if (!response) FAIL() << "Invalid Pointer<Response>";
    response->response_data = request->message_data + " has been processed";
    return ReturnTuple();
  }
};

struct ExtValue_Service : public Value_Service {
  inline ExtValue_Service(S<const Type_Service> p, const ParamsArgs& params_args) : Value_Service(std::move(p)) {}
};

Category_Service& CreateCategory_Service() {
  static auto& category = *new ExtCategory_Service();
  return category;
}

S<const Type_Service> CreateType_Service(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Service(CreateCategory_Service(), Params<0>::Type()));
  return cached;
}

void RemoveType_Service(const Params<0>::Type& params) {}
BoxedValue CreateValue_Service(S<const Type_Service> parent, const ParamsArgs& params_args) {
  return BoxedValue::New<ExtValue_Service>(std::move(parent), params_args);
}

#ifdef ZEOLITE_PRIVATE_NAMESPACE
}  // namespace ZEOLITE_PRIVATE_NAMESPACE
using namespace ZEOLITE_PRIVATE_NAMESPACE;
#endif  // ZEOLITE_PRIVATE_NAMESPACE
