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
#include "Streamlined_RandomUniform.hpp"
#include "Category_Float.hpp"
#include "Category_Generator.hpp"
#include "Category_RandomUniform.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_RandomUniform(S<const Type_RandomUniform> parent, const ParamsArgs& params_args);

struct ExtCategory_RandomUniform : public Category_RandomUniform {
};

struct ExtType_RandomUniform : public Type_RandomUniform {
  inline ExtType_RandomUniform(Category_RandomUniform& p, Params<0>::Type params) : Type_RandomUniform(p, params) {}

  ReturnTuple Call_new(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("RandomUniform.new")
    const PrimFloat Var_arg1 = (params_args.GetArg(0)).AsFloat();
    const PrimFloat Var_arg2 = (params_args.GetArg(1)).AsFloat();
    if (Var_arg2 <= Var_arg1) {
      FAIL() << "Invalid range (" << Var_arg1 << "," << Var_arg2 << ")";
    }
    return ReturnTuple(CreateValue_RandomUniform(PARAM_SELF, params_args));
  }

  ReturnTuple Call_probability(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("RandomUniform.probability")
    return Call_new(PassParamsArgs(Box_Float(0.0), Box_Float(1.0)));
  }
};

struct ExtValue_RandomUniform : public Value_RandomUniform {
  inline ExtValue_RandomUniform(S<const Type_RandomUniform> p, const ParamsArgs& params_args)
    : Value_RandomUniform(std::move(p)),
      generator_((int) (long) this),
      distribution_(params_args.GetArg(0).AsFloat(), params_args.GetArg(1).AsFloat()) {}

  ReturnTuple Call_generate(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("RandomUniform.generate")
    return ReturnTuple(Box_Float(distribution_(generator_)));
  }

  ReturnTuple Call_setSeed(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("RandomUniform.setSeed")
    distribution_.reset();
    generator_.seed(params_args.GetArg(0).AsInt());
    return ReturnTuple(VAR_SELF);
  }

  std::default_random_engine generator_;
  std::uniform_real_distribution<double> distribution_;
};

Category_RandomUniform& CreateCategory_RandomUniform() {
  static auto& category = *new ExtCategory_RandomUniform();
  return category;
}

S<const Type_RandomUniform> CreateType_RandomUniform(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_RandomUniform(CreateCategory_RandomUniform(), Params<0>::Type()));
  return cached;
}

void RemoveType_RandomUniform(const Params<0>::Type& params) {}

BoxedValue CreateValue_RandomUniform(S<const Type_RandomUniform> parent, const ParamsArgs& params_args) {
  return BoxedValue::New<ExtValue_RandomUniform>(std::move(parent), params_args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
