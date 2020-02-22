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

#include "builtin.hpp"

#include <map>
#include <sstream>

#include "category-source.hpp"
#include "Category_AsBool.hpp"
#include "Category_AsChar.hpp"
#include "Category_AsInt.hpp"
#include "Category_AsFloat.hpp"
#include "Category_Bool.hpp"
#include "Category_Char.hpp"
#include "Category_Equals.hpp"
#include "Category_Float.hpp"
#include "Category_Formatted.hpp"
#include "Category_Int.hpp"
#include "Category_LessThan.hpp"
#include "Category_ReadPosition.hpp"
#include "Category_String.hpp"


void BuiltinFail(const S<TypeValue>& formatted) {
  FAIL() << TypeValue::Call(formatted, Function_Formatted_formatted,
                            ParamTuple(), ArgTuple()).Only()->AsString();
  __builtin_unreachable();
}

namespace {

struct OptionalEmpty : public TypeValue {
  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const ValueFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    FAIL() << "Function called on empty value";
    __builtin_unreachable();
  }

  std::string CategoryName() const final { return "empty"; }

  bool Present() const final { return false; }
};

struct Type_Intersect : public TypeInstance {
  Type_Intersect(L<TypeInstance*> params) : params_(params.begin(), params.end()) {}

  std::string CategoryName() const final { return "(intersection)"; }

  void BuildTypeName(std::ostream& output) const final {
    if (params_.empty()) {
      output << "any";
    } else {
      output << "[";
      bool first = true;
      for (const auto param : params_) {
        if (!first) output << "&";
        first = false;
        param->BuildTypeName(output);
      }
      output << "]";
    }
  }

  MergeType InstanceMergeType() const final
  { return MergeType::INTERSECT; }

  std::vector<const TypeInstance*> MergedTypes() const final
  { return params_; }

  const L<const TypeInstance*> params_;
};

struct Type_Union : public TypeInstance {
  Type_Union(L<TypeInstance*> params) : params_(params.begin(), params.end()) {}

  std::string CategoryName() const final { return "(union)"; }

  void BuildTypeName(std::ostream& output) const final {
    if (params_.empty()) {
      output << "all";
    } else {
      output << "[";
      bool first = true;
      for (const auto param : params_) {
        if (!first) output << "|";
        first = false;
        param->BuildTypeName(output);
      }
      output << "]";
    }
  }

  MergeType InstanceMergeType() const final
  { return MergeType::UNION; }

  std::vector<const TypeInstance*> MergedTypes() const final
  { return params_; }

  const L<const TypeInstance*> params_;
};

}  // namespace


TypeInstance& Merge_Intersect(L<TypeInstance*> params) {
  static auto& cache = *new std::map<L<TypeInstance*>,R<Type_Intersect>>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_Intersect(params)); }
  return *cached;
}

TypeInstance& Merge_Union(L<TypeInstance*> params) {
  static auto& cache = *new std::map<L<TypeInstance*>,R<Type_Union>>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_Union(params)); }
  return *cached;
}

TypeInstance& GetMerged_Any() {
  static auto& instance = Merge_Intersect(L_get<TypeInstance*>());
  return instance;
}

TypeInstance& GetMerged_All() {
  static auto& instance = Merge_Union(L_get<TypeInstance*>());
  return instance;
}


const S<TypeValue>& Var_empty = *new S<TypeValue>(new OptionalEmpty());


// Bool

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
namespace ZEOLITE_DYNAMIC_NAMESPACE {
#endif  // ZEOLITE_DYNAMIC_NAMESPACE
namespace {
const int collection_Bool = 0;
}  // namespace
const void* const Functions_Bool = &collection_Bool;
namespace {
class Category_Bool;
class Type_Bool;
Type_Bool& CreateType_Bool(Params<0>::Type params);
class Value_Bool;
S<TypeValue> CreateValue(Type_Bool& parent, const ParamTuple& params, const ValueTuple& args);
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
  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_Bool()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_AsBool()) {
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
  Type_Bool(Category_Bool& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Bool>::Check();
    CycleCheck<Type_Bool> marker(*this);
    TRACE_FUNCTION("Bool (init @type)")
  }
  ReturnTuple Dispatch(const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Bool::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Equals[] = {
      &Type_Bool::Call_equals,
    };
    if (label.collection == Functions_Equals) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_Equals[label.function_num])(params, args);
    }
    return TypeInstance::Dispatch(label, params, args);
  }
  ReturnTuple Call_equals(const ParamTuple& params, const ValueTuple& args);
};
Type_Bool& CreateType_Bool(Params<0>::Type params) {
  static auto& cache = *new InstanceMap<0,Type_Bool>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_Bool(CreateCategory_Bool(), params)); }
  return *cached;
}
struct Value_Bool : public TypeValue {
  Value_Bool(Type_Bool& p, bool value) : parent(p), value_(value) {}
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
    if (label.collection == Functions_AsBool) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_AsBool[label.function_num])(self, params, args);
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
  bool AsBool() const final { return value_; }
  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  Type_Bool& parent;
  const bool value_;
};
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
TypeInstance& GetType_Bool(Params<0>::Type params) {
  return CreateType_Bool(params);
}
#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE


// Char

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
  static auto& cache = *new InstanceMap<0,Type_Char>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_Char(CreateCategory_Char(), params)); }
  return *cached;
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


// Int

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
namespace ZEOLITE_DYNAMIC_NAMESPACE {
#endif  // ZEOLITE_DYNAMIC_NAMESPACE
namespace {
const int collection_Int = 0;
}  // namespace
const void* const Functions_Int = &collection_Int;
namespace {
class Category_Int;
class Type_Int;
Type_Int& CreateType_Int(Params<0>::Type params);
class Value_Int;
S<TypeValue> CreateValue(Type_Int& parent, const ParamTuple& params, const ValueTuple& args);
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
  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_Int()) {
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
  Type_Int(Category_Int& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Int>::Check();
    CycleCheck<Type_Int> marker(*this);
    TRACE_FUNCTION("Int (init @type)")
  }
  ReturnTuple Dispatch(const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Int::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Equals[] = {
      &Type_Int::Call_equals,
    };
    static const CallType Table_LessThan[] = {
      &Type_Int::Call_lessThan,
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
Type_Int& CreateType_Int(Params<0>::Type params) {
  static auto& cache = *new InstanceMap<0,Type_Int>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_Int(CreateCategory_Int(), params)); }
  return *cached;
}
struct Value_Int : public TypeValue {
  Value_Int(Type_Int& p, PrimInt value) : parent(p), value_(value) {}
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
  PrimInt AsInt() const final { return value_; }
  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asChar(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  Type_Int& parent;
  const PrimInt value_;
};
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
TypeInstance& GetType_Int(Params<0>::Type params) {
  return CreateType_Int(params);
}
#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE


// Float

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
namespace ZEOLITE_DYNAMIC_NAMESPACE {
#endif  // ZEOLITE_DYNAMIC_NAMESPACE
namespace {
const int collection_Float = 0;
}  // namespace
const void* const Functions_Float = &collection_Float;
namespace {
class Category_Float;
class Type_Float;
Type_Float& CreateType_Float(Params<0>::Type params);
class Value_Float;
S<TypeValue> CreateValue(Type_Float& parent, const ParamTuple& params, const ValueTuple& args);
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
  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_Float()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_AsBool()) {
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
  Type_Float(Category_Float& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_Float>::Check();
    CycleCheck<Type_Float> marker(*this);
    TRACE_FUNCTION("Float (init @type)")
  }
  ReturnTuple Dispatch(const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_Float::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Equals[] = {
      &Type_Float::Call_equals,
    };
    static const CallType Table_LessThan[] = {
      &Type_Float::Call_lessThan,
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
Type_Float& CreateType_Float(Params<0>::Type params) {
  static auto& cache = *new InstanceMap<0,Type_Float>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_Float(CreateCategory_Float(), params)); }
  return *cached;
}
struct Value_Float : public TypeValue {
  Value_Float(Type_Float& p, PrimFloat value) : parent(p), value_(value) {}
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
    if (label.collection == Functions_AsBool) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_AsBool[label.function_num])(self, params, args);
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
  PrimFloat AsFloat() const final { return value_; }
  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asFloat(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_asInt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  Type_Float& parent;
  const PrimFloat value_;
};
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
TypeInstance& GetType_Float(Params<0>::Type params) {
  return CreateType_Float(params);
}
#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE


// String

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
namespace ZEOLITE_DYNAMIC_NAMESPACE {
#endif  // ZEOLITE_DYNAMIC_NAMESPACE
namespace {
const int collection_String = 0;
}  // namespace
const void* const Functions_String = &collection_String;
const ValueFunction& Function_String_subSequence = (*new ValueFunction{ 0, 2, 1, "String", "subSequence", Functions_String, 0 });
namespace {
class Category_String;
class Type_String;
Type_String& CreateType_String(Params<0>::Type params);
class Value_String;
S<TypeValue> CreateValue(Type_String& parent, const ParamTuple& params, const ValueTuple& args);
struct Category_String : public TypeCategory {
  std::string CategoryName() const final { return "String"; }
  Category_String() {
    CycleCheck<Category_String>::Check();
    CycleCheck<Category_String> marker(*this);
    TRACE_FUNCTION("String (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_String::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_String& CreateCategory_String() {
  static auto& category = *new Category_String();
  return category;
}
struct Type_String : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_String& parent;
  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_String()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_AsBool()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_Formatted()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_ReadPosition()) {
      args = std::vector<const TypeInstance*>{&GetType_Char(T_get())};
      return true;
    }
    return false;
  }
  Type_String(Category_String& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_String>::Check();
    CycleCheck<Type_String> marker(*this);
    TRACE_FUNCTION("String (init @type)")
  }
  ReturnTuple Dispatch(const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_String::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Equals[] = {
      &Type_String::Call_equals,
    };
    static const CallType Table_LessThan[] = {
      &Type_String::Call_lessThan,
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
Type_String& CreateType_String(Params<0>::Type params) {
  static auto& cache = *new InstanceMap<0,Type_String>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_String(CreateCategory_String(), params)); }
  return *cached;
}
struct Value_String : public TypeValue {
  Value_String(Type_String& p, const PrimString& value) : parent(p), value_(value) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_String::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_AsBool[] = {
      &Value_String::Call_asBool,
    };
    static const CallType Table_Formatted[] = {
      &Value_String::Call_formatted,
    };
    static const CallType Table_ReadPosition[] = {
      &Value_String::Call_readPosition,
      &Value_String::Call_readSize,
      &Value_String::Call_subSequence,
    };
    static const CallType Table_String[] = {
      &Value_String::Call_subSequence,
    };
    if (label.collection == Functions_AsBool) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_AsBool[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_Formatted) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_Formatted[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_ReadPosition) {
      if (label.function_num < 0 || label.function_num >= 3) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_ReadPosition[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_String) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_String[label.function_num])(self, params, args);
    }
    return TypeValue::Dispatch(self, label, params, args);
  }
  std::string CategoryName() const final { return parent.CategoryName(); }
  const PrimString& AsString() const final { return value_; }
  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_readPosition(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_readSize(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_subSequence(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  Type_String& parent;
  const PrimString value_;
};
ReturnTuple Type_String::Call_equals(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.equals")
  const S<TypeValue>& Var_arg1 = (args.At(0));
  const S<TypeValue>& Var_arg2 = (args.At(1));
  return ReturnTuple(Box_Bool(Var_arg1->AsString()==Var_arg2->AsString()));
}
ReturnTuple Type_String::Call_lessThan(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.lessThan")
  const S<TypeValue>& Var_arg1 = (args.At(0));
  const S<TypeValue>& Var_arg2 = (args.At(1));
  return ReturnTuple(Box_Bool(Var_arg1->AsString()<Var_arg2->AsString()));
}
ReturnTuple Value_String::Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.asBool")
  return ReturnTuple(Box_Bool(value_.size() != 0));
}
ReturnTuple Value_String::Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.formatted")
  return ReturnTuple(Var_self);
}
ReturnTuple Value_String::Call_readPosition(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.readPosition")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  if (Var_arg1 < 0 || Var_arg1 >= value_.size()) {
    FAIL() << "Read position " << Var_arg1 << " is out of bounds";
  }
  return ReturnTuple(Box_Char(value_[Var_arg1]));
}
ReturnTuple Value_String::Call_readSize(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.readSize")
  return ReturnTuple(Box_Int(value_.size()));
}
ReturnTuple Value_String::Call_subSequence(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.subSequence")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  const PrimInt Var_arg2 = (args.At(1))->AsInt();
  if (Var_arg1 < 0 || Var_arg1 > value_.size()) {
    FAIL() << "Subsequence position " << Var_arg1 << " is out of bounds";
  }
  if (Var_arg2 < 0 || Var_arg1 + Var_arg2 > value_.size()) {
    FAIL() << "Subsequence size " << Var_arg2 << " is invalid";
  }
  return ReturnTuple(Box_String(value_.substr(Var_arg1,Var_arg2)));
}
}  // namespace
TypeCategory& GetCategory_String() {
  return CreateCategory_String();
}
TypeInstance& GetType_String(Params<0>::Type params) {
  return CreateType_String(params);
}
#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE


S<TypeValue> Box_Bool(bool value) {
  return value? Var_true : Var_false;
}

S<TypeValue> Box_Char(PrimChar value) {
  return S_get(new Value_Char(CreateType_Char(Params<0>::Type()), value));
}

S<TypeValue> Box_Int(PrimInt value) {
  return S_get(new Value_Int(CreateType_Int(Params<0>::Type()), value));
}

S<TypeValue> Box_Float(PrimFloat value) {
  return S_get(new Value_Float(CreateType_Float(Params<0>::Type()), value));
}

S<TypeValue> Box_String(const PrimString& value) {
  return S_get(new Value_String(CreateType_String(Params<0>::Type()), value));
}
