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

#include "Category_String.hpp"

#include <map>
#include <sstream>

#include "category-source.hpp"
#include "Category_AsBool.hpp"
#include "Category_Formatted.hpp"
#include "Category_Char.hpp"
#include "Category_Container.hpp"
#include "Category_ReadAt.hpp"
#include "Category_SubSequence.hpp"
#include "Category_Append.hpp"
#include "Category_Build.hpp"
#include "Category_Default.hpp"
#include "Category_DefaultOrder.hpp"
#include "Category_Order.hpp"
#include "Category_Equals.hpp"
#include "Category_LessThan.hpp"


#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE
namespace {
const int collection_String = 0;
}  // namespace
const void* const Functions_String = &collection_String;
const ValueFunction& Function_String_subSequence = (*new ValueFunction{ 0, 2, 1, "String", "subSequence", Functions_String, 0 });
const TypeFunction& Function_String_builder = (*new TypeFunction{ 0, 0, 1, "String", "builder", Functions_String, 0 });
namespace {
class Category_String;
class Type_String;
class Value_String;
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
  bool CanConvertFrom(const S<const TypeInstance>& from) const final {
    std::vector<S<const TypeInstance>> args;
    if (!from->TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  void Params_String(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_AsBool(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_Formatted(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_ReadAt(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{GetType_Char(T_get())};
  }
  void Params_SubSequence(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_DefaultOrder(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{GetType_Char(T_get())};
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<S<const TypeInstance>>& args) const final {
    using CallType = void(Type_String::*)(std::vector<S<const TypeInstance>>&)const;
    static DispatchSingle<CallType> all_calls[] = {
      DispatchSingle<CallType>(&GetCategory_String(),       &Type_String::Params_String),
      DispatchSingle<CallType>(&GetCategory_AsBool(),       &Type_String::Params_AsBool),
      DispatchSingle<CallType>(&GetCategory_Formatted(),    &Type_String::Params_Formatted),
      DispatchSingle<CallType>(&GetCategory_ReadAt(),       &Type_String::Params_ReadAt),
      DispatchSingle<CallType>(&GetCategory_SubSequence(),  &Type_String::Params_SubSequence),
      DispatchSingle<CallType>(&GetCategory_DefaultOrder(), &Type_String::Params_DefaultOrder),
      DispatchSingle<CallType>(),
    };
    const DispatchSingle<CallType>* const call = DispatchSelect(&category, all_calls);
    if (call) {
      (this->*call->value)(args);
      return true;
    }
    return false;
  }
  Type_String(Category_String& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_String>::Check();
    CycleCheck<Type_String> marker(*this);
    TRACE_FUNCTION("String (init @type)")
  }
  ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_String::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_Default[] = {
      &Type_String::Call_default,
    };
    static const CallType Table_Equals[] = {
      &Type_String::Call_equals,
    };
    static const CallType Table_LessThan[] = {
      &Type_String::Call_lessThan,
    };
    static const CallType Table_String[] = {
      &Type_String::Call_builder,
    };
    static DispatchTable<CallType> all_tables[] = {
      DispatchTable<CallType>(Functions_Default,  Table_Default),
      DispatchTable<CallType>(Functions_Equals,   Table_Equals),
      DispatchTable<CallType>(Functions_LessThan, Table_LessThan),
      DispatchTable<CallType>(Functions_String,   Table_String),
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
  ReturnTuple Call_builder(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_equals(const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_lessThan(const ParamTuple& params, const ValueTuple& args);
};
S<Type_String> CreateType_String(Params<0>::Type params) {
  static const auto cached = S_get(new Type_String(CreateCategory_String(), Params<0>::Type()));
  return cached;
}
struct Value_String : public TypeValue {
  Value_String(S<Type_String> p, const PrimString& value) : parent(p), value_(value) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_String::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_AsBool[] = {
      &Value_String::Call_asBool,
    };
    static const CallType Table_DefaultOrder[] = {
      &Value_String::Call_defaultOrder,
    };
    static const CallType Table_Formatted[] = {
      &Value_String::Call_formatted,
    };
    static const CallType Table_ReadAt[] = {
      &Value_String::Call_readAt,
    };
    static const CallType Table_Container[] = {
      &Value_String::Call_size,
    };
    static const CallType Table_String[] = {
      &Value_String::Call_subSequence,
    };
    static const CallType Table_SubSequence[] = {
      &Value_String::Call_subSequence,
    };
    static DispatchTable<CallType> all_tables[] = {
      DispatchTable<CallType>(Functions_AsBool,       Table_AsBool),
      DispatchTable<CallType>(Functions_Container,    Table_Container),
      DispatchTable<CallType>(Functions_DefaultOrder, Table_DefaultOrder),
      DispatchTable<CallType>(Functions_Formatted,    Table_Formatted),
      DispatchTable<CallType>(Functions_ReadAt,       Table_ReadAt),
      DispatchTable<CallType>(Functions_String,       Table_String),
      DispatchTable<CallType>(Functions_SubSequence,  Table_SubSequence),
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
  const PrimString& AsString() const final { return value_; }
  ReturnTuple Call_asBool(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_formatted(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_defaultOrder(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_readAt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_size(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_subSequence(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  const S<Type_String> parent;
  const PrimString value_;
};

class Value_StringBuilder : public TypeValue {
 public:
  std::string CategoryName() const final { return "StringBuilder"; }

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const ValueFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    if (args.Size() != label.arg_count) {
      FAIL() << "Wrong number of args";
    }
    if (params.Size() != label.param_count){
      FAIL() << "Wrong number of params";
    }
    if (&label == &Function_Append_append) {
      TRACE_FUNCTION("StringBuilder.append")
      std::lock_guard<std::mutex> lock(mutex);
      output_ << args.At(0)->AsString();
      return ReturnTuple(self);
    }
    if (&label == &Function_Build_build) {
      TRACE_FUNCTION("StringBuilder.build")
      std::lock_guard<std::mutex> lock(mutex);
      return ReturnTuple(Box_String(output_.str()));
    }
    return TypeValue::Dispatch(self, label, params, args);
  }

 private:
  std::mutex mutex;
  std::ostringstream output_;
};

struct StringOrder : public AnonymousOrder {
  StringOrder(S<TypeValue> container, const std::string& s)
    : AnonymousOrder(container, Function_Order_next, Function_Order_get), value(s) {}

  S<TypeValue> Call_next(const S<TypeValue>& self) final {
    if (index+1 >= value.size()) {
      return Var_empty;
    } else {
      ++index;
      return self;
    }
  }

  S<TypeValue> Call_get(const S<TypeValue>& self) final {
    if (index >= value.size()) FAIL() << "iterated past end of String";
    return Box_Char(value[index]);
  }

  const std::string& value;
  int index = 0;
};

ReturnTuple Type_String::Call_default(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.default")
  return ReturnTuple(Box_String(""));
}
ReturnTuple Type_String::Call_builder(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.builder")
  return ReturnTuple(S<TypeValue>(new Value_StringBuilder));
}
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
ReturnTuple Value_String::Call_defaultOrder(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.defaultOrder")
  if (value_.empty()) {
    return ReturnTuple(Var_empty);
  } else {
    return ReturnTuple(S_get(new StringOrder(Var_self, value_)));
  }
}
ReturnTuple Value_String::Call_readAt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.readAt")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  if (Var_arg1 < 0 || Var_arg1 >= value_.size()) {
    FAIL() << "Read position " << Var_arg1 << " is out of bounds";
  }
  return ReturnTuple(Box_Char(value_[Var_arg1]));
}
ReturnTuple Value_String::Call_size(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("String.size")
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
S<TypeInstance> GetType_String(Params<0>::Type params) {
  return CreateType_String(params);
}
#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE


S<TypeValue> Box_String(const PrimString& value) {
  return S_get(new Value_String(CreateType_String(Params<0>::Type()), value));
}
