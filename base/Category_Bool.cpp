/* -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

#include "Category_Bool.hpp"

#include <map>
#include <sstream>

#include "category-source.hpp"
#include "Category_AsBool.hpp"
#include "Category_AsInt.hpp"
#include "Category_AsFloat.hpp"
#include "Category_Formatted.hpp"
#include "Category_Default.hpp"
#include "Category_Equals.hpp"


#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE
namespace {
const int collection_Bool = 0;
}  // namespace
const void* const Functions_Bool = &collection_Bool;
namespace {
class Category_Bool;
class Type_Bool;
class Value_Bool;
struct Category_Bool : public TypeCategory {
  std::string CategoryName() const final { return "Bool"; }
  Category_Bool() {
    CycleCheck<Category_Bool>::Check();
    CycleCheck<Category_Bool> marker(*this);
    TRACE_FUNCTION("Bool (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_Bool::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_Bool& CreateCategory_Bool() {
  static auto& category = *new Category_Bool();
  return category;
}
struct Type_Bool : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_Bool& parent;
  bool CanConvertFrom(const S<const TypeInstance>& from) const final {
    std::vector<S<const TypeInstance>> args;
    if (!from->TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  void Params_Bool(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_AsBool(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_AsInt(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_AsFloat(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_Formatted(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<S<const TypeInstance>>& args) const final {
    using CallType = void(Type_Bool::*)(std::vector<S<const TypeInstance>>&)const;
    static DispatchSingle<CallType> all_calls[] = {
      DispatchSingle<CallType>(&GetCategory_AsBool(),    &Type_Bool::Params_AsBool),
      DispatchSingle<CallType>(&GetCategory_AsFloat(),   &Type_Bool::Params_AsFloat),
      DispatchSingle<CallType>(&GetCategory_AsInt(),     &Type_Bool::Params_AsInt),
      DispatchSingle<CallType>(&GetCategory_Bool(),      &Type_Bool::Params_Bool),
      DispatchSingle<CallType>(&GetCategory_Formatted(), &Type_Bool::Params_Formatted),
      DispatchSingle<CallType>(),
    };
    const DispatchSingle<CallType>* const call = DispatchSelect(&category, all_calls);
    if (call) {
      (this->*call->value)(args);
      return true;
    }
    return false;
  }
  Type_Bool(Category_Bool& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Bool>::Check();
    CycleCheck<Type_Bool> marker(*this);
    TRACE_FUNCTION("Bool (init @type)")
  }
  ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Bool::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Default[] = {
      &Type_Bool::Call_default,
    };
    static const CallType Table_Equals[] = {
      &Type_Bool::Call_equals,
    };
    static DispatchTable<CallType> all_tables[] = {
      DispatchTable<CallType>(Functions_Default, Table_Default),
      DispatchTable<CallType>(Functions_Equals,  Table_Equals),
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
};
S<Type_Bool> CreateType_Bool(Params<0>::Type params) {
  static const auto cached = S_get(new Type_Bool(CreateCategory_Bool(), Params<0>::Type()));
  return cached;
}
struct Value_Bool : public TypeValue {
  Value_Bool(S<Type_Bool> p, bool value) : parent(p), value_(value) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_Bool::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_AsBool[] = {
      &Value_Bool::Call_asBool,
    };
    static const CallType Table_AsFloat[] = {
      &Value_Bool::Call_asFloat,
    };
    static const CallType Table_AsInt[] = {
      &Value_Bool::Call_asInt,
    };
    static const CallType Table_Formatted[] = {
      &Value_Bool::Call_formatted,
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
  bool AsBool() const final { return value_; }
  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  const S<Type_Bool> parent;
  const bool value_;
};

ReturnTuple Type_Bool::Call_default(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Bool.default")
  return ReturnTuple(Box_Bool(false));
}
ReturnTuple Type_Bool::Call_equals(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Bool.equals")
  const bool Var_arg1 = (args.At(0))->AsBool();
  const bool Var_arg2 = (args.At(1))->AsBool();
  return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
}
ReturnTuple Value_Bool::Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Bool.asBool")
  return ReturnTuple(Var_self);
}
ReturnTuple Value_Bool::Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Bool.asFloat")
  return ReturnTuple(Box_Float(value_ ? 1.0 : 0.0));
}
ReturnTuple Value_Bool::Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Bool.asInt")
  return ReturnTuple(Box_Int(value_? 1 : 0));
}
ReturnTuple Value_Bool::Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Bool.formatted")
  return ReturnTuple(Box_String(value_? "true" : "false"));
}
const S<TypeValue>& Var_true = *new S<TypeValue>(new Value_Bool(CreateType_Bool(Params<0>::Type()), true));
const S<TypeValue>& Var_false = *new S<TypeValue>(new Value_Bool(CreateType_Bool(Params<0>::Type()), false));
}  // namespace
TypeCategory& GetCategory_Bool() {
  return CreateCategory_Bool();
}
S<TypeInstance> GetType_Bool(Params<0>::Type params) {
  return CreateType_Bool(params);
}
#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE


S<TypeValue> Box_Bool(bool value) {
  return value? Var_true : Var_false;
}
