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

#include "Category_Int.hpp"

#include <map>
#include <sstream>

#include "category-source.hpp"
#include "Category_AsBool.hpp"
#include "Category_AsChar.hpp"
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
const int collection_Int = 0;
}  // namespace
const void* const Functions_Int = &collection_Int;
namespace {
class Category_Int;
class Type_Int;
class Value_Int;
struct Category_Int : public TypeCategory {
  std::string CategoryName() const final { return "Int"; }
  Category_Int() {
    CycleCheck<Category_Int>::Check();
    CycleCheck<Category_Int> marker(*this);
    TRACE_FUNCTION("Int (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_Int::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_Int& CreateCategory_Int() {
  static auto& category = *new Category_Int();
  return category;
}
struct Type_Int : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_Int& parent;
  bool CanConvertFrom(const S<const TypeInstance>& from) const final {
    std::vector<S<const TypeInstance>> args;
    if (!from->TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  void Params_Int(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_AsBool(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_AsChar(std::vector<S<const TypeInstance>>& args) const {
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
    using CallType = void(Type_Int::*)(std::vector<S<const TypeInstance>>&)const;
    static DispatchSingle<CallType> all_calls[] = {
      DispatchSingle<CallType>(&GetCategory_AsBool(),    &Type_Int::Params_AsBool),
      DispatchSingle<CallType>(&GetCategory_AsChar(),    &Type_Int::Params_AsChar),
      DispatchSingle<CallType>(&GetCategory_AsFloat(),   &Type_Int::Params_AsFloat),
      DispatchSingle<CallType>(&GetCategory_AsInt(),     &Type_Int::Params_AsInt),
      DispatchSingle<CallType>(&GetCategory_Formatted(), &Type_Int::Params_Formatted),
      DispatchSingle<CallType>(&GetCategory_Int(),       &Type_Int::Params_Int),
      DispatchSingle<CallType>(),
    };
    std::atomic_bool table_lock{0};
    const DispatchSingle<CallType>* const call = DispatchSelect(table_lock, &category, all_calls);
    if (call) {
      (this->*call->value)(args);
      return true;
    }
    return false;
  }
  Type_Int(Category_Int& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Int>::Check();
    CycleCheck<Type_Int> marker(*this);
    TRACE_FUNCTION("Int (init @type)")
  }
  ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Int::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Default[] = {
      &Type_Int::Call_default,
    };
    static const CallType Table_Equals[] = {
      &Type_Int::Call_equals,
    };
    static const CallType Table_LessThan[] = {
      &Type_Int::Call_lessThan,
    };
    static DispatchTable<CallType> all_tables[] = {
      DispatchTable<CallType>(Functions_Default,  Table_Default),
      DispatchTable<CallType>(Functions_Equals,   Table_Equals),
      DispatchTable<CallType>(Functions_LessThan, Table_LessThan),
      DispatchTable<CallType>(),
    };
    std::atomic_bool table_lock{0};
    const DispatchTable<CallType>* const table = DispatchSelect(table_lock, label.collection, all_tables);
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
S<Type_Int> CreateType_Int(Params<0>::Type params) {
  static const auto cached = S_get(new Type_Int(CreateCategory_Int(), Params<0>::Type()));
  return cached;
}
struct Value_Int : public TypeValue {
  Value_Int(S<Type_Int> p, PrimInt value) : parent(p), value_(value) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_Int::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_AsBool[] = {
      &Value_Int::Call_asBool,
    };
    static const CallType Table_AsChar[] = {
      &Value_Int::Call_asChar,
    };
    static const CallType Table_AsFloat[] = {
      &Value_Int::Call_asFloat,
    };
    static const CallType Table_AsInt[] = {
      &Value_Int::Call_asInt,
    };
    static const CallType Table_Formatted[] = {
      &Value_Int::Call_formatted,
    };
    static DispatchTable<CallType> all_tables[] = {
      DispatchTable<CallType>(Functions_AsBool,    Table_AsBool),
      DispatchTable<CallType>(Functions_AsChar,    Table_AsChar),
      DispatchTable<CallType>(Functions_AsFloat,   Table_AsFloat),
      DispatchTable<CallType>(Functions_AsInt,     Table_AsInt),
      DispatchTable<CallType>(Functions_Formatted, Table_Formatted),
      DispatchTable<CallType>(),
    };
    std::atomic_bool table_lock{0};
    const DispatchTable<CallType>* const table = DispatchSelect(table_lock, label.collection, all_tables);
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
  PrimInt AsInt() const final { return value_; }
  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asChar(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  const S<Type_Int> parent;
  const PrimInt value_;
};

ReturnTuple Type_Int::Call_default(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Int.default")
  return ReturnTuple(Box_Int(0));
}
ReturnTuple Type_Int::Call_equals(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Int.equals")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  const PrimInt Var_arg2 = (args.At(1))->AsInt();
  return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
}
ReturnTuple Type_Int::Call_lessThan(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Int.lessThan")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  const PrimInt Var_arg2 = (args.At(1))->AsInt();
  return ReturnTuple(Box_Bool(Var_arg1<Var_arg2));
}
ReturnTuple Value_Int::Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Int.asBool")
  return ReturnTuple(Box_Bool(value_ != 0));
}
ReturnTuple Value_Int::Call_asChar(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Int.asChar")
  return ReturnTuple(Box_Char(value_ % 0xff));
}
ReturnTuple Value_Int::Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Int.asFloat")
  return ReturnTuple(Box_Float(value_));
}
ReturnTuple Value_Int::Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Int.asInt")
  return ReturnTuple(Var_self);
}
ReturnTuple Value_Int::Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Int.formatted")
  std::ostringstream output;
  output << value_;
  return ReturnTuple(Box_String(output.str()));
}
}  // namespace
TypeCategory& GetCategory_Int() {
  return CreateCategory_Int();
}
S<TypeInstance> GetType_Int(Params<0>::Type params) {
  return CreateType_Int(params);
}
#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE


S<TypeValue> Box_Int(PrimInt value) {
  return S_get(new Value_Int(CreateType_Int(Params<0>::Type()), value));
}
