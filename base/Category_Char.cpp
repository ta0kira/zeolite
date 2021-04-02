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

#include "Category_Char.hpp"

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
const int collection_Char = 0;
}  // namespace
const void* const Functions_Char = &collection_Char;
namespace {
class Category_Char;
class Type_Char;
class Value_Char;
struct Category_Char : public TypeCategory {
  std::string CategoryName() const final { return "Char"; }
  Category_Char() {
    CycleCheck<Category_Char>::Check();
    CycleCheck<Category_Char> marker(*this);
    TRACE_FUNCTION("Char (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_Char::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_Char& CreateCategory_Char() {
  static auto& category = *new Category_Char();
  return category;
}
struct Type_Char : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_Char& parent;
  bool CanConvertFrom(const S<const TypeInstance>& from) const final {
    std::vector<S<const TypeInstance>> args;
    if (!from->TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  void Params_Char(std::vector<S<const TypeInstance>>& args) const {
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
    using CallType = void(Type_Char::*)(std::vector<S<const TypeInstance>>&)const;
    static DispatchSingle<CallType> all_calls[] = {
      DispatchSingle<CallType>(&GetCategory_AsBool(),    &Type_Char::Params_AsBool),
      DispatchSingle<CallType>(&GetCategory_AsChar(),    &Type_Char::Params_AsChar),
      DispatchSingle<CallType>(&GetCategory_AsFloat(),   &Type_Char::Params_AsFloat),
      DispatchSingle<CallType>(&GetCategory_AsInt(),     &Type_Char::Params_AsInt),
      DispatchSingle<CallType>(&GetCategory_Char(),      &Type_Char::Params_Char),
      DispatchSingle<CallType>(&GetCategory_Formatted(), &Type_Char::Params_Formatted),
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
  Type_Char(Category_Char& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Char>::Check();
    CycleCheck<Type_Char> marker(*this);
    TRACE_FUNCTION("Char (init @type)")
  }
  ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Char::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Default[] = {
      &Type_Char::Call_default,
    };
    static const CallType Table_Equals[] = {
      &Type_Char::Call_equals,
    };
    static const CallType Table_LessThan[] = {
      &Type_Char::Call_lessThan,
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
S<Type_Char> CreateType_Char(Params<0>::Type params) {
  static const auto cached = S_get(new Type_Char(CreateCategory_Char(), Params<0>::Type()));
  return cached;
}
struct Value_Char : public TypeValue {
  Value_Char(S<Type_Char> p, PrimChar value) : parent(p), value_(value) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_Char::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_AsBool[] = {
      &Value_Char::Call_asBool,
    };
    static const CallType Table_AsChar[] = {
      &Value_Char::Call_asChar,
    };
    static const CallType Table_AsFloat[] = {
      &Value_Char::Call_asFloat,
    };
    static const CallType Table_AsInt[] = {
      &Value_Char::Call_asInt,
    };
    static const CallType Table_Formatted[] = {
      &Value_Char::Call_formatted,
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
  PrimChar AsChar() const final { return value_; }
  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asChar(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  const S<Type_Char> parent;
  const PrimChar value_;
};

ReturnTuple Type_Char::Call_default(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Char.default")
  return ReturnTuple(Box_Char('\0'));
}
ReturnTuple Type_Char::Call_equals(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Char.equals")
  const PrimChar Var_arg1 = (args.At(0))->AsChar();
  const PrimChar Var_arg2 = (args.At(1))->AsChar();
      return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
}
ReturnTuple Type_Char::Call_lessThan(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Char.lessThan")
  const PrimChar Var_arg1 = (args.At(0))->AsChar();
  const PrimChar Var_arg2 = (args.At(1))->AsChar();
  return ReturnTuple(Box_Bool(Var_arg1<Var_arg2));
}
ReturnTuple Value_Char::Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Char.asBool")
  return ReturnTuple(Box_Bool(value_ != '\0'));
}
ReturnTuple Value_Char::Call_asChar(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Char.asChar")
  return ReturnTuple(Var_self);
}
ReturnTuple Value_Char::Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Char.asFloat")
  return ReturnTuple(Box_Float(value_));
}
ReturnTuple Value_Char::Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Char.asInt")
  return ReturnTuple(Box_Int(value_));
}
ReturnTuple Value_Char::Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("Char.formatted")
  std::ostringstream output;
  output << value_;
  return ReturnTuple(Box_String(output.str()));
}
}  // namespace
TypeCategory& GetCategory_Char() {
  return CreateCategory_Char();
}
S<TypeInstance> GetType_Char(Params<0>::Type params) {
  return CreateType_Char(params);
}
#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE


S<TypeValue> Box_Char(PrimChar value) {
  return S_get(new Value_Char(CreateType_Char(Params<0>::Type()), value));
}
