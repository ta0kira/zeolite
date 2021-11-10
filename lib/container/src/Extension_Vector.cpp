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

#include <vector>

#include "category-source.hpp"
#include "Streamlined_Vector.hpp"
#include "Category_Append.hpp"
#include "Category_Container.hpp"
#include "Category_Default.hpp"
#include "Category_DefaultOrder.hpp"
#include "Category_Formatted.hpp"
#include "Category_Order.hpp"
#include "Category_ReadAt.hpp"
#include "Category_Stack.hpp"
#include "Category_String.hpp"
#include "Category_Vector.hpp"
#include "Category_WriteAt.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

using VectorType = std::vector<BoxedValue>;

BoxedValue CreateValue_Vector(S<const Type_Vector> parent, VectorType values);

struct ExtCategory_Vector : public Category_Vector {
  ReturnTuple Call_copyFrom(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector:copyFrom")
    const S<const TypeInstance> Param_y = params.At(0);
    const BoxedValue& Var_arg1 = (args.At(0));
    VectorType values;
    const PrimInt size = TypeValue::Call(Var_arg1, Function_Container_size, ParamTuple(), ArgTuple()).Only().AsInt();
    for (int i = 0; i < size; ++i) {
      values.push_back(TypeValue::Call(Var_arg1, Function_ReadAt_readAt, ParamTuple(), ArgTuple(Box_Int(i))).Only());
    }
    return ReturnTuple(CreateValue_Vector(CreateType_Vector(Params<1>::Type(Param_y)), std::move(values)));
  }

  ReturnTuple Call_create(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector:create")
    const S<const TypeInstance> Param_y = params.At(0);
    return ReturnTuple(CreateValue_Vector(CreateType_Vector(Params<1>::Type(Param_y)), VectorType()));
  }

  ReturnTuple Call_createSize(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector:createSize")
    const S<const TypeInstance> Param_y = params.At(0);
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    VectorType values;
    for (int i = 0; i < Var_arg1; ++i) {
      values.push_back(TypeInstance::Call(Param_y, Function_Default_default, ParamTuple(), ArgTuple()).Only());
    }
    return ReturnTuple(CreateValue_Vector(CreateType_Vector(Params<1>::Type(Param_y)), std::move(values)));
  }
};

struct ExtType_Vector : public Type_Vector {
  inline ExtType_Vector(Category_Vector& p, Params<1>::Type params) : Type_Vector(p, params) {}
};

class VectorOrder : public TypeValue {
 public:
  VectorOrder(BoxedValue container, const VectorType& v)
    : container_(container), values_(v) {}

  std::string CategoryName() const final { return "VectorOrder"; }

  ReturnTuple Dispatch(const ValueFunction& label,
                       const ParamTuple& params,
                       const ValueTuple& args) final {
    if (&label == &Function_Order_next) {
      TRACE_FUNCTION("VectorOrder.next")
      if (index_+1 >= values_.size()) {
        return ReturnTuple(Var_empty);
      } else {
        ++index_;
        return ReturnTuple(VAR_SELF);
      }
    }
    if (&label == &Function_Order_get) {
      TRACE_FUNCTION("VectorOrder.get")
      if (index_ >= values_.size()) {
        FAIL() << "Iterated past end of Vector";
      }
      return ReturnTuple(values_[index_]);
    }
    return TypeValue::Dispatch(label, params, args);
  }

 private:
  const BoxedValue container_;
  const VectorType& values_;
  int index_ = 0;
};

struct ExtValue_Vector : public Value_Vector {
  inline ExtValue_Vector(S<const Type_Vector> p, VectorType v)
    : Value_Vector(std::move(p)), values(std::move(v)) {}

  ReturnTuple Call_append(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector.append")
    const BoxedValue& Var_arg1 = (args.At(0));
    values.push_back(Var_arg1);
    return ReturnTuple(VAR_SELF);
  }

  ReturnTuple Call_defaultOrder(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector.defaultOrder")
    if (values.empty()) {
      return ReturnTuple(Var_empty);
    } else {
      return ReturnTuple(BoxedValue::New<VectorOrder>(VAR_SELF, values));
    }
  }

  ReturnTuple Call_duplicate(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector.duplicate")
    return ReturnTuple(BoxedValue::New<ExtValue_Vector>(parent, values));
  }

  ReturnTuple Call_pop(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector.pop")
    if (values.empty()) {
      BUILTIN_FAIL(Box_String(PrimString_FromLiteral("no elements left to pop")))
    }
    BoxedValue value = values.back();
    values.pop_back();
    return ReturnTuple(value);
  }

  ReturnTuple Call_push(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector.push")
    const BoxedValue& Var_arg1 = (args.At(0));
    values.push_back(Var_arg1);
    return ReturnTuple(VAR_SELF);
  }

  ReturnTuple Call_readAt(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector.readAt")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    if (Var_arg1 < 0 || Var_arg1 >= values.size()) {
      FAIL() << "index " << Var_arg1 << " is out of bounds";
    }
    return ReturnTuple(values[Var_arg1]);
  }

  ReturnTuple Call_size(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector.size")
    return ReturnTuple(Box_Int(values.size()));
  }

  ReturnTuple Call_writeAt(const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("Vector.writeAt")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    const BoxedValue& Var_arg2 = (args.At(1));
    if (Var_arg1 < 0 || Var_arg1 >= values.size()) {
      FAIL() << "index " << Var_arg1 << " is out of bounds";
    }
    values[Var_arg1] = Var_arg2;
    return ReturnTuple(VAR_SELF);
  }

  // vector<#x>
  VectorType values;
};

Category_Vector& CreateCategory_Vector() {
  static auto& category = *new ExtCategory_Vector();
  return category;
}

static auto& Vector_instance_cache = *new InstanceCache<1, Type_Vector>([](Params<1>::Type params) {
    return S_get(new ExtType_Vector(CreateCategory_Vector(), params));
  });

S<const Type_Vector> CreateType_Vector(const Params<1>::Type& params) {
  return Vector_instance_cache.GetOrCreate(params);
}

void RemoveType_Vector(const Params<1>::Type& params) {
  Vector_instance_cache.Remove(params);
}

BoxedValue CreateValue_Vector(S<const Type_Vector> parent, VectorType values) {
  return BoxedValue::New<ExtValue_Vector>(std::move(parent), std::move(values));
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
