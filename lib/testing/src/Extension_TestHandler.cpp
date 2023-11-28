/* -----------------------------------------------------------------------------
Copyright 2023 Kevin P. Barry

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

#include <cstdlib>
#include <iostream>

#include "category-source.hpp"
#include "Streamlined_TestHandler.hpp"
#include "Category_String.hpp"
#include "Category_TestHandler.hpp"

#ifdef ZEOLITE_PRIVATE_NAMESPACE
namespace ZEOLITE_PRIVATE_NAMESPACE {
#endif  // ZEOLITE_PRIVATE_NAMESPACE

struct ExtCategory_TestHandler : public Category_TestHandler {
  ReturnTuple Call_failAndExit(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("TestHandler:failAndExit")
    const BoxedValue& Var_arg1 = params_args.GetArg(0);
    std::cerr << Var_arg1.AsString();
    std::exit(1);
  }
};

struct ExtType_TestHandler : public Type_TestHandler {
  inline ExtType_TestHandler(Category_TestHandler& p, Params<0>::Type params) : Type_TestHandler(p, params) {}
};

Category_TestHandler& CreateCategory_TestHandler() {
  static auto& category = *new ExtCategory_TestHandler();
  return category;
}

S<const Type_TestHandler> CreateType_TestHandler(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_TestHandler(CreateCategory_TestHandler(), Params<0>::Type()));
  return cached;
}

void RemoveType_TestHandler(const Params<0>::Type& params) {}

#ifdef ZEOLITE_PRIVATE_NAMESPACE
}  // namespace ZEOLITE_PRIVATE_NAMESPACE
using namespace ZEOLITE_PRIVATE_NAMESPACE;
#endif  // ZEOLITE_PRIVATE_NAMESPACE
