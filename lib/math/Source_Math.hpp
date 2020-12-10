/* -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

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

#ifndef HEADER_Source_Math
#define HEADER_Source_Math

#include <cmath>

#include "category-source.hpp"
#include "Category_Math.hpp"


#ifdef ZEOLITE_DYNAMIC_NAMESPACE
namespace ZEOLITE_DYNAMIC_NAMESPACE {
#endif  // ZEOLITE_DYNAMIC_NAMESPACE

class Category_Math : public TypeCategory {
 public:
  std::string CategoryName() const final;

  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final;

 protected:
  Category_Math();
};

class Type_Math : public TypeInstance {
 public:
  std::string CategoryName() const final;

  void BuildTypeName(std::ostream& output) const final;

  bool CanConvertFrom(const S<const TypeInstance>& from) const final;

  bool TypeArgsForParent(const TypeCategory& category, std::vector<S<const TypeInstance>>& args) const final;

  ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final;

 protected:
  Type_Math(Category_Math& p, Params<0>::Type params);

  virtual ReturnTuple Call_acos(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_acosh(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_asin(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_asinh(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_atan(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_atanh(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_ceil(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_cos(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_cosh(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_exp(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_fabs(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_floor(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_fmod(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_isinf(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_isnan(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_log(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_log10(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_log2(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_pow(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_round(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_sin(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_sinh(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_sqrt(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_tan(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_tanh(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_trunc(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_abs(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) = 0;

  Category_Math& parent;
};

class Value_Math : public TypeValue {
 public:
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final;

  std::string CategoryName() const final;

 protected:
  Value_Math(S<Type_Math> p);

  S<Type_Math> parent;
};

Category_Math& CreateCategory_Math();

S<Type_Math> CreateType_Math(Params<0>::Type params);

S<TypeValue> CreateValue_Math(Type_Math& parent, const ParamTuple& params, const ValueTuple& args);

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE

#endif  // HEADER_Source_Math
