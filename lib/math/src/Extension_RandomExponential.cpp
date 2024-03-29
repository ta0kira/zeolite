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

#include <random>

#include "category-source.hpp"
#include "Streamlined_RandomExponential.hpp"
#include "Category_Float.hpp"
#include "Category_Generator.hpp"
#include "Category_RandomExponential.hpp"
#include "Category_RandomUniform.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_RandomExponential(S<const Type_RandomExponential> parent, const ParamsArgs& params_args);

struct ExtCategory_RandomExponential : public Category_RandomExponential {
};

struct ExtType_RandomExponential : public Type_RandomExponential {
  inline ExtType_RandomExponential(Category_RandomExponential& p, Params<0>::Type params) : Type_RandomExponential(p, params) {}

  ReturnTuple Call_new(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("RandomExponential.new")
    const PrimFloat Var_arg1 = (params_args.GetArg(0)).AsFloat();
    if (Var_arg1 <= 0) {
      FAIL() << "Invalid lambda " << Var_arg1;
    }
    return ReturnTuple(CreateValue_RandomExponential(PARAM_SELF, params_args));
  }
};

struct ExtValue_RandomExponential : public Value_RandomExponential {
  inline ExtValue_RandomExponential(S<const Type_RandomExponential> p, const ParamsArgs& params_args)
    : Value_RandomExponential(std::move(p)),
      generator_((int) (long) this),
      distribution_(params_args.GetArg(0).AsFloat()) {}

  ReturnTuple Call_generate(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("RandomExponential.generate")
    return ReturnTuple(Box_Float(distribution_(generator_)));
  }

  ReturnTuple Call_setSeed(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("RandomExponential.setSeed")
    distribution_.reset();
    generator_.seed(params_args.GetArg(0).AsInt());
    return ReturnTuple(VAR_SELF);
  }

  std::default_random_engine generator_;
  std::exponential_distribution<double> distribution_;
};

Category_RandomExponential& CreateCategory_RandomExponential() {
  static auto& category = *new ExtCategory_RandomExponential();
  return category;
}

S<const Type_RandomExponential> CreateType_RandomExponential(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_RandomExponential(CreateCategory_RandomExponential(), Params<0>::Type()));
  return cached;
}

void RemoveType_RandomExponential(const Params<0>::Type& params) {}

BoxedValue CreateValue_RandomExponential(S<const Type_RandomExponential> parent, const ParamsArgs& params_args) {
  return BoxedValue::New<ExtValue_RandomExponential>(std::move(parent), params_args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
