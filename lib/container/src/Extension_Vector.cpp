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

#include <mutex>
#include <vector>

#include "category-source.hpp"
#include "Streamlined_Vector.hpp"
#include "Category_Append.hpp"
#include "Category_Container.hpp"
#include "Category_Default.hpp"
#include "Category_Formatted.hpp"
#include "Category_ReadAt.hpp"
#include "Category_Stack.hpp"
#include "Category_String.hpp"
#include "Category_Vector.hpp"
#include "Category_WriteAt.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

using VectorType = std::vector<S<TypeValue>>;

S<TypeValue> CreateValue_Vector(S<Type_Vector> parent, const ParamTuple& params, VectorType values);

struct ExtCategory_Vector : public Category_Vector {
  ReturnTuple Call_copyFrom(const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Vector:copyFrom")
    const S<TypeInstance> Param_y = params.At(0);
    const S<TypeValue>& Var_arg1 = (args.At(0));
    VectorType values;
    const PrimInt size = TypeValue::Call(Var_arg1, Function_Container_size, ParamTuple(), ArgTuple()).Only()->AsInt();
    for (int i = 0; i < size; ++i) {
      values.push_back(TypeValue::Call(Var_arg1, Function_ReadAt_readAt, ParamTuple(), ArgTuple(Box_Int(i))).Only());
    }
    return ReturnTuple(CreateValue_Vector(CreateType_Vector(Params<1>::Type(Param_y)), ParamTuple(), values));
  }

  ReturnTuple Call_create(const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Vector:create")
    const S<TypeInstance> Param_y = params.At(0);
    return ReturnTuple(CreateValue_Vector(CreateType_Vector(Params<1>::Type(Param_y)), ParamTuple(), VectorType()));
  }

  ReturnTuple Call_createSize(const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Vector:createSize")
    const S<TypeInstance> Param_y = params.At(0);
    const PrimInt Var_arg1 = (args.At(0))->AsInt();
    VectorType values;
    for (int i = 0; i < Var_arg1; ++i) {
      values.push_back(TypeInstance::Call(Param_y, Function_Default_default, ParamTuple(), ArgTuple()).Only());
    }
    return ReturnTuple(CreateValue_Vector(CreateType_Vector(Params<1>::Type(Param_y)), ParamTuple(), values));
  }
};

struct ExtType_Vector : public Type_Vector {
  inline ExtType_Vector(Category_Vector& p, Params<1>::Type params) : Type_Vector(p, params) {}
};

struct ExtValue_Vector : public Value_Vector {
  inline ExtValue_Vector(S<Type_Vector> p, const ParamTuple& params, VectorType v)
    : Value_Vector(p, params), values(std::move(v)) {}

  ReturnTuple Call_append(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Vector.append")
    std::lock_guard<std::mutex> lock(mutex);
    const S<TypeValue>& Var_arg1 = (args.At(0));
    values.push_back(Var_arg1);
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_pop(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Vector.pop")
    std::lock_guard<std::mutex> lock(mutex);
    if (values.empty()) {
      BUILTIN_FAIL(Box_String(PrimString_FromLiteral("no elements left to pop")))
    }
    S<TypeValue> value = values.back();
    values.pop_back();
    return ReturnTuple(value);
  }

  ReturnTuple Call_push(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Vector.push")
    std::lock_guard<std::mutex> lock(mutex);
    const S<TypeValue>& Var_arg1 = (args.At(0));
    values.push_back(Var_arg1);
    return ReturnTuple(Var_self);
  }

  ReturnTuple Call_readAt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Vector.readAt")
    std::lock_guard<std::mutex> lock(mutex);
    const PrimInt Var_arg1 = (args.At(0))->AsInt();
    if (Var_arg1 < 0 || Var_arg1 >= values.size()) {
      FAIL() << "index " << Var_arg1 << " is out of bounds";
    }
    return ReturnTuple(values[Var_arg1]);
  }

  ReturnTuple Call_size(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Vector.size")
    std::lock_guard<std::mutex> lock(mutex);
    return ReturnTuple(Box_Int(values.size()));
  }

  ReturnTuple Call_writeAt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
    TRACE_FUNCTION("Vector.writeAt")
    std::lock_guard<std::mutex> lock(mutex);
    const PrimInt Var_arg1 = (args.At(0))->AsInt();
    const S<TypeValue>& Var_arg2 = (args.At(1));
    if (Var_arg1 < 0 || Var_arg1 >= values.size()) {
      FAIL() << "index " << Var_arg1 << " is out of bounds";
    }
    values[Var_arg1] = Var_arg2;
    return ReturnTuple(Var_self);
  }

  std::mutex mutex;
  VectorType values;
};

Category_Vector& CreateCategory_Vector() {
  static auto& category = *new ExtCategory_Vector();
  return category;
}
S<Type_Vector> CreateType_Vector(Params<1>::Type params) {
  static auto& cache = *new InstanceCache<1, Type_Vector>([](Params<1>::Type params) {
      return S_get(new ExtType_Vector(CreateCategory_Vector(), params));
    });
  return cache.GetOrCreate(params);
}
S<TypeValue> CreateValue_Vector(S<Type_Vector> parent, const ParamTuple& params, VectorType values) {
  return S_get(new ExtValue_Vector(parent, params, values));
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
