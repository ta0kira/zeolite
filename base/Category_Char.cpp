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

#include "Category_Char.hpp"

#include <map>
#include <sstream>

#include "category-source.hpp"
#include "Category_AsBool.hpp"
#include "Category_AsChar.hpp"
#include "Category_AsInt.hpp"
#include "Category_AsFloat.hpp"
#include "Category_Formatted.hpp"
#include "Category_Equals.hpp"
#include "Category_LessThan.hpp"


#ifdef ZEOLITE_DYNAMIC_NAMESPACE
namespace ZEOLITE_DYNAMIC_NAMESPACE {
#endif  // ZEOLITE_DYNAMIC_NAMESPACE
namespace {
const int collection_Char = 0;
}  // namespace
const void* const Functions_Char = &collection_Char;
namespace {
class Category_Char;
class Type_Char;
Type_Char& CreateType_Char(Params<0>::Type params);
class Value_Char;
S<TypeValue> CreateValue(Type_Char& parent, const ParamTuple& params, const ValueTuple& args);
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
  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_Char()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_AsBool()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_AsChar()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_AsInt()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_AsFloat()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_Formatted()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    return false;
  }
  Type_Char(Category_Char& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Char>::Check();
    CycleCheck<Type_Char> marker(*this);
    TRACE_FUNCTION("Char (init @type)")
  }
  ReturnTuple Dispatch(const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Char::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Equals[] = {
      &Type_Char::Call_equals,
    };
    static const CallType Table_LessThan[] = {
      &Type_Char::Call_lessThan,
    };
    if (label.collection == Functions_Equals) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_Equals[label.function_num])(params, args);
    }
    if (label.collection == Functions_LessThan) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_LessThan[label.function_num])(params, args);
    }
    return TypeInstance::Dispatch(label, params, args);
  }
  ReturnTuple Call_equals(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_lessThan(const ParamTuple& params, const ValueTuple& args);
};
Type_Char& CreateType_Char(Params<0>::Type params) {
  static auto& cached = *new Type_Char(CreateCategory_Char(), Params<0>::Type());
  return cached;
}
struct Value_Char : public TypeValue {
  Value_Char(Type_Char& p, PrimChar value) : parent(p), value_(value) {}
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
    if (label.collection == Functions_AsBool) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_AsBool[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_AsChar) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_AsChar[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_AsFloat) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_AsFloat[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_AsInt) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_AsInt[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_Formatted) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_Formatted[label.function_num])(self, params, args);
    }
    return TypeValue::Dispatch(self, label, params, args);
  }
  std::string CategoryName() const final { return parent.CategoryName(); }
  PrimChar AsChar() const final { return value_; }
  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asChar(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  Type_Char& parent;
  const PrimChar value_;
};
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
TypeInstance& GetType_Char(Params<0>::Type params) {
  return CreateType_Char(params);
}
#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE


S<TypeValue> Box_Char(PrimChar value) {
  return S_get(new Value_Char(CreateType_Char(Params<0>::Type()), value));
}
