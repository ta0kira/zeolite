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
#include "Streamlined_Destructor.hpp"
#include "Category_Destructor.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_Destructor(S<const Type_Destructor> parent, const ValueTuple& args);

struct ExtCategory_Destructor : public Category_Destructor {
};

struct ExtType_Destructor : public Type_Destructor {
  inline ExtType_Destructor(Category_Destructor& p, Params<0>::Type params) : Type_Destructor(p, params) {}
  ReturnTuple Call_new(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("Destructor.new")
    return ReturnTuple(CreateValue_Destructor(PARAM_SELF, ArgTuple()));
  }
};

struct ExtValue_Destructor : public Value_Destructor {
  inline ExtValue_Destructor(S<const Type_Destructor> p, const ValueTuple& args)
    : Value_Destructor(std::move(p)) {}

  ~ExtValue_Destructor() {
    (void) VAR_SELF;
  }
};

Category_Destructor& CreateCategory_Destructor() {
  static auto& category = *new ExtCategory_Destructor();
  return category;
}

S<const Type_Destructor> CreateType_Destructor(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Destructor(CreateCategory_Destructor(), Params<0>::Type()));
  return cached;
}

void RemoveType_Destructor(const Params<0>::Type& params) {}

BoxedValue CreateValue_Destructor(S<const Type_Destructor> parent, const ValueTuple& args) {
  return BoxedValue::New<ExtValue_Destructor>(std::move(parent), args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
