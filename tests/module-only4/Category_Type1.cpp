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

#include "category-source.hpp"
#include "Category_Formatted.hpp"
#include "Category_String.hpp"
#include "Category_Type1.hpp"
#include "Category_Type2.hpp"

#ifdef ZEOLITE_PRIVATE_NAMESPACE
namespace ZEOLITE_PRIVATE_NAMESPACE {
#endif  // ZEOLITE_PRIVATE_NAMESPACE

namespace {

const int collection_Type1 = 0;
}  // namespace

const void* const Functions_Type1 = &collection_Type1;
const TypeFunction& Function_Type1_create = (*new TypeFunction{ 0, 0, 1, "Type1", "create", Functions_Type1, 0 });
const ValueFunction& Function_Type1_get = (*new ValueFunction{ 0, 0, 1, "Type1", "get", Functions_Type1, 0 });

namespace {

class Category_Type1;
class Type_Type1;
Type_Type1& CreateType_Type1(Params<0>::Type params);
class Value_Type1;
S<TypeValue> CreateValue_Type1(Type_Type1& parent, const ParamTuple& params, const ValueTuple& args);

struct Category_Type1 : public TypeCategory {
  std::string CategoryName() const final { return "Type1"; }
  Category_Type1() {
    CycleCheck<Category_Type1>::Check();
    CycleCheck<Category_Type1> marker(*this);
    TRACE_FUNCTION("Type1 (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_Type1::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};

Category_Type1& CreateCategory_Type1() {
  static auto& category = *new Category_Type1();
  return category;
}

struct Type_Type1 : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_Type1& parent;
  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_Type1()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    return false;
  }
  Type_Type1(Category_Type1& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Type1>::Check();
    CycleCheck<Type_Type1> marker(*this);
    TRACE_FUNCTION("Type1 (init @type)")
  }
  ReturnTuple Dispatch(const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Type1::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Type1[] = {
      &Type_Type1::Call_create,
    };
    if (label.collection == Functions_Type1) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_Type1[label.function_num])(params, args);
    }
    return TypeInstance::Dispatch(label, params, args);
  }
  ReturnTuple Call_create(const ParamTuple& params, const ValueTuple& args);
};

Type_Type1& CreateType_Type1(Params<0>::Type params) {
  static auto& cached = *new Type_Type1(CreateCategory_Type1(), Params<0>::Type());
  return cached;
}

struct Value_Type1 : public TypeValue {
  Value_Type1(Type_Type1& p, const ParamTuple& params, const ValueTuple& args)
    : parent(p), value(args.Only()) {}

  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_Type1::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_Type1[] = {
      &Value_Type1::Call_get,
    };
    if (label.collection == Functions_Type1) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_Type1[label.function_num])(self, params, args);
    }
    return TypeValue::Dispatch(self, label, params, args);
  }
  std::string CategoryName() const final { return parent.CategoryName(); }
  ReturnTuple Call_get(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  Type_Type1& parent;
  const S<TypeValue> value;
};

S<TypeValue> CreateValue_Type1(Type_Type1& parent, const ParamTuple& params, const ValueTuple& args) {
  return S_get(new Value_Type1(parent, params, args));
}

ReturnTuple Type_Type1::Call_create(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Type1.create")
  return ReturnTuple(CreateValue_Type1(*this, ParamTuple(),
    GetType_Type2(Params<0>::Type()).Call(Function_Type2_create, ParamTuple(), ArgTuple())));
}

ReturnTuple Value_Type1::Call_get(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Type1.get")
  return ReturnTuple(TypeValue::Call(value, Function_Type2_get, ParamTuple(), ArgTuple()));
}

}  // namespace

TypeCategory& GetCategory_Type1() {
  return CreateCategory_Type1();
}

TypeInstance& GetType_Type1(Params<0>::Type params) {
  return CreateType_Type1(params);
}

#ifdef ZEOLITE_PRIVATE_NAMESPACE
}  // namespace ZEOLITE_PRIVATE_NAMESPACE
using namespace ZEOLITE_PRIVATE_NAMESPACE;
#endif  // ZEOLITE_PRIVATE_NAMESPACE
