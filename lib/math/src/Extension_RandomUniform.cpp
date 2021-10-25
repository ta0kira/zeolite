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

BoxedValue CreateValue_RandomUniform(S<Type_RandomUniform> parent, const ValueTuple& args);

struct ExtCategory_RandomUniform : public Category_RandomUniform {
};

struct ExtType_RandomUniform : public Type_RandomUniform {
  inline ExtType_RandomUniform(Category_RandomUniform& p, Params<0>::Type params) : Type_RandomUniform(p, params) {}

  ReturnTuple Call_new(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("RandomUniform.new")
    const PrimFloat Var_arg1 = (args.At(0)).AsFloat();
    const PrimFloat Var_arg2 = (args.At(1)).AsFloat();
    if (Var_arg2 <= Var_arg1) {
      FAIL() << "Invalid range (" << Var_arg1 << "," << Var_arg2 << ")";
    }
    return ReturnTuple(CreateValue_RandomUniform(shared_from_this(), args));
  }
};

struct ExtValue_RandomUniform : public Value_RandomUniform {
  inline ExtValue_RandomUniform(S<Type_RandomUniform> p, const ValueTuple& args)
    : Value_RandomUniform(p),
      generator_((int) (long) this),
      distribution_(args.At(0).AsFloat(), args.At(1).AsFloat()) {}

  ReturnTuple Call_generate(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("RandomUniform.generate")
    return ReturnTuple(Box_Float(distribution_(generator_)));
  }

  ReturnTuple Call_setSeed(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("RandomUniform.setSeed")
    distribution_.reset();
    generator_.seed(args.At(0).AsInt());
    return ReturnTuple(VAR_SELF);
  }

  std::default_random_engine generator_;
  std::uniform_real_distribution<double> distribution_;
};

Category_RandomUniform& CreateCategory_RandomUniform() {
  static auto& category = *new ExtCategory_RandomUniform();
  return category;
}

S<Type_RandomUniform> CreateType_RandomUniform(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_RandomUniform(CreateCategory_RandomUniform(), Params<0>::Type()));
  return cached;
}

BoxedValue CreateValue_RandomUniform(S<Type_RandomUniform> parent, const ValueTuple& args) {
  return BoxedValue::New<ExtValue_RandomUniform>(parent, args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
