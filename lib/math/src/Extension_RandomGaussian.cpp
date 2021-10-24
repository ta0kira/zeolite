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
#include "Streamlined_RandomGaussian.hpp"
#include "Category_Float.hpp"
#include "Category_Generator.hpp"
#include "Category_RandomGaussian.hpp"
#include "Category_RandomUniform.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_RandomGaussian(S<Type_RandomGaussian> parent, const ValueTuple& args);

struct ExtCategory_RandomGaussian : public Category_RandomGaussian {
};

struct ExtType_RandomGaussian : public Type_RandomGaussian {
  inline ExtType_RandomGaussian(Category_RandomGaussian& p, Params<0>::Type params) : Type_RandomGaussian(p, params) {}

  ReturnTuple Call_new(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("RandomGaussian.new")
    const PrimFloat Var_arg1 = (args.At(0)).AsFloat();
    const PrimFloat Var_arg2 = (args.At(1)).AsFloat();
    if (Var_arg2 <= 0) {
      FAIL() << "Invalid standard deviation " << Var_arg2;
    }
    return ReturnTuple(CreateValue_RandomGaussian(shared_from_this(), args));
  }
};

struct ExtValue_RandomGaussian : public Value_RandomGaussian {
  inline ExtValue_RandomGaussian(S<Type_RandomGaussian> p, const ValueTuple& args)
    : Value_RandomGaussian(p),
      generator_((int) (long) this),
      distribution_(args.At(0).AsFloat(), args.At(1).AsFloat()) {}

  ReturnTuple Call_generate(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("RandomGaussian.generate")
    return ReturnTuple(Box_Float(distribution_(generator_)));
  }

  ReturnTuple Call_setSeed(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("RandomGaussian.setSeed")
    distribution_.reset();
    generator_.seed(args.At(0).AsInt());
    return ReturnTuple();
  }

  std::default_random_engine generator_;
  std::normal_distribution<double> distribution_;
};

Category_RandomGaussian& CreateCategory_RandomGaussian() {
  static auto& category = *new ExtCategory_RandomGaussian();
  return category;
}

S<Type_RandomGaussian> CreateType_RandomGaussian(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_RandomGaussian(CreateCategory_RandomGaussian(), Params<0>::Type()));
  return cached;
}

BoxedValue CreateValue_RandomGaussian(S<Type_RandomGaussian> parent, const ValueTuple& args) {
  return BoxedValue::New<ExtValue_RandomGaussian>(parent, args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
