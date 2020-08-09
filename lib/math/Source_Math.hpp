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
#include "Category_Float.hpp"
#include "Category_Formatted.hpp"
#include "Category_Math.hpp"
#include "Category_String.hpp"


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

  bool CanConvertFrom(const TypeInstance& from) const final;

  bool TypeArgsForParent(const TypeCategory& category, std::vector<const TypeInstance*>& args) const final;

  ReturnTuple Dispatch(const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final;

 protected:
  Type_Math(Category_Math& p, Params<0>::Type params);

  virtual ReturnTuple Call_acos(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_acosh(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_asin(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_asinh(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_atan(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_atanh(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_ceil(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_cos(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_cosh(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_exp(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_fabs(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_floor(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_fmod(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_isinf(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_isnan(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_log(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_log10(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_log2(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_pow(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_round(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_sin(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_sinh(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_sqrt(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_tan(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_tanh(const ParamTuple& params, const ValueTuple& args) = 0;
  virtual ReturnTuple Call_trunc(const ParamTuple& params, const ValueTuple& args) = 0;

  Category_Math& parent;
};

class Value_Math : public TypeValue {
 public:
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final;

  std::string CategoryName() const final;

 protected:
  Value_Math(Type_Math& p);

  Type_Math& parent;
};

Category_Math& CreateCategory_Math();

Type_Math& CreateType_Math(Params<0>::Type params);

S<TypeValue> CreateValue_Math(Type_Math& parent, const ParamTuple& params, const ValueTuple& args);

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE

#endif  // HEADER_Source_Math
