/* -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

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

#include "Category_Float.hpp"

#include <map>
#include <sstream>

#include "category-source.hpp"
#include "Category_AsBool.hpp"
#include "Category_AsInt.hpp"
#include "Category_AsFloat.hpp"
#include "Category_Formatted.hpp"
#include "Category_Default.hpp"
#include "Category_Equals.hpp"
#include "Category_LessThan.hpp"


#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE
namespace {
const int collection_Float = 0;
}  // namespace
const void* const Functions_Float = &collection_Float;
namespace {
class Category_Float;
class Type_Float;
class Value_Float;
struct Category_Float : public TypeCategory {
  std::string CategoryName() const final { return "Float"; }
  Category_Float() {
    CycleCheck<Category_Float>::Check();
    CycleCheck<Category_Float> marker(*this);
    TRACE_FUNCTION("Float (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_Float::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_Float& CreateCategory_Float() {
  static auto& category = *new Category_Float();
  return category;
}
struct Type_Float : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_Float& parent;
  bool CanConvertFrom(const S<const TypeInstance>& from) const final {
    std::vector<S<const TypeInstance>> args;
    if (!from->TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<S<const TypeInstance>>& args) const final {
    if (&category == &GetCategory_Float()) {
      args = std::vector<S<const TypeInstance>>{};
      return true;
    }
    if (&category == &GetCategory_AsBool()) {
      args = std::vector<S<const TypeInstance>>{};
      return true;
    }
    if (&category == &GetCategory_AsInt()) {
      args = std::vector<S<const TypeInstance>>{};
      return true;
    }
    if (&category == &GetCategory_AsFloat()) {
      args = std::vector<S<const TypeInstance>>{};
      return true;
    }
    if (&category == &GetCategory_Formatted()) {
      args = std::vector<S<const TypeInstance>>{};
      return true;
    }
    return false;
  }
  Type_Float(Category_Float& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Float>::Check();
    CycleCheck<Type_Float> marker(*this);
    TRACE_FUNCTION("Float (init @type)")
  }
  ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Float::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Default[] = {
      &Type_Float::Call_default,
    };
    static const CallType Table_Equals[] = {
      &Type_Float::Call_equals,
    };
    static const CallType Table_LessThan[] = {
      &Type_Float::Call_lessThan,
    };
    static DispatchTable<CallType> all_tables[] = {
      DispatchTable<CallType>(Functions_Default,  Table_Default),
      DispatchTable<CallType>(Functions_Equals,   Table_Equals),
      DispatchTable<CallType>(Functions_LessThan, Table_LessThan),
      DispatchTable<CallType>(),
    };
    const DispatchTable<CallType>* const table = DispatchSelect(label.collection, all_tables);
    if (table) {
      if (label.function_num < 0 || label.function_num >= table->size) {
        FAIL() << "Bad function call " << label;
      } else {
        return (this->*table->table[label.function_num])(params, args);
      }
    }
    return TypeInstance::Dispatch(self, label, params, args);
  }
  ReturnTuple Call_default(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_equals(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_lessThan(const ParamTuple& params, const ValueTuple& args);
};
S<Type_Float> CreateType_Float(Params<0>::Type params) {
  static const auto cached = S_get(new Type_Float(CreateCategory_Float(), Params<0>::Type()));
  return cached;
}
struct Value_Float : public TypeValue {
  Value_Float(S<Type_Float> p, PrimFloat value) : parent(p), value_(value) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_Float::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_AsBool[] = {
      &Value_Float::Call_asBool,
    };
    static const CallType Table_AsFloat[] = {
      &Value_Float::Call_asFloat,
    };
    static const CallType Table_AsInt[] = {
      &Value_Float::Call_asInt,
    };
    static const CallType Table_Formatted[] = {
      &Value_Float::Call_formatted,
    };
    static DispatchTable<CallType> all_tables[] = {
      DispatchTable<CallType>(Functions_AsBool,    Table_AsBool),
      DispatchTable<CallType>(Functions_AsFloat,   Table_AsFloat),
      DispatchTable<CallType>(Functions_AsInt,     Table_AsInt),
      DispatchTable<CallType>(Functions_Formatted, Table_Formatted),
      DispatchTable<CallType>(),
    };
    const DispatchTable<CallType>* const table = DispatchSelect(label.collection, all_tables);
    if (table) {
      if (label.function_num < 0 || label.function_num >= table->size) {
        FAIL() << "Bad function call " << label;
      } else {
        return (this->*table->table[label.function_num])(self, params, args);
      }
    }
    return TypeValue::Dispatch(self, label, params, args);
  }
  std::string CategoryName() const final { return parent->CategoryName(); }
  PrimFloat AsFloat() const final { return value_; }
  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  const S<Type_Float> parent;
  const PrimFloat value_;
};

ReturnTuple Type_Float::Call_default(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Float.default")
  return ReturnTuple(Box_Float(0.0));
}
ReturnTuple Type_Float::Call_equals(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Float.equals")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  const PrimFloat Var_arg2 = (args.At(1))->AsFloat();
  return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
}
ReturnTuple Type_Float::Call_lessThan(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Float.lessThan")
  const PrimFloat Var_arg1 = (args.At(0))->AsFloat();
  const PrimFloat Var_arg2 = (args.At(1))->AsFloat();
  return ReturnTuple(Box_Bool(Var_arg1<Var_arg2));
}
ReturnTuple Value_Float::Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Float.asBool")
  return ReturnTuple(Box_Bool(value_ != 0.0));
}
ReturnTuple Value_Float::Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Float.asFloat")
  return ReturnTuple(Var_self);
}
ReturnTuple Value_Float::Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Float.asInt")
  return ReturnTuple(Box_Int(value_));
}
ReturnTuple Value_Float::Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Float.formatted")
  std::ostringstream output;
  output << value_;
  return ReturnTuple(Box_String(output.str()));
}
}  // namespace
TypeCategory& GetCategory_Float() {
  return CreateCategory_Float();
}
S<TypeInstance> GetType_Float(Params<0>::Type params) {
  return CreateType_Float(params);
}
#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE


S<TypeValue> Box_Float(PrimFloat value) {
  return S_get(new Value_Float(CreateType_Float(Params<0>::Type()), value));
}
