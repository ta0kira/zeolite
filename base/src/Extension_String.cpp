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

#include "category-source.hpp"
#include "Streamlined_String.hpp"
#include "Category_Append.hpp"
#include "Category_AsBool.hpp"
#include "Category_Bool.hpp"
#include "Category_Build.hpp"
#include "Category_Char.hpp"
#include "Category_CharBuffer.hpp"
#include "Category_Container.hpp"
#include "Category_Default.hpp"
#include "Category_DefaultOrder.hpp"
#include "Category_Equals.hpp"
#include "Category_Formatted.hpp"
#include "Category_Int.hpp"
#include "Category_LessThan.hpp"
#include "Category_Order.hpp"
#include "Category_ReadAt.hpp"
#include "Category_String.hpp"
#include "Category_SubSequence.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

struct ExtCategory_String : public Category_String {
};

class Value_StringBuilder : public TypeValue {
 public:
  std::string CategoryName() const final { return "StringBuilder"; }

  ReturnTuple Dispatch(const BoxedValue& self,
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
      output_ << args.At(0).AsString();
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

struct ExtType_String : public Type_String {
  inline ExtType_String(Category_String& p, Params<0>::Type params) : Type_String(p, params) {}

  ReturnTuple Call_builder(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.builder")
    return ReturnTuple(BoxedValue::New<Value_StringBuilder>());
  }

  ReturnTuple Call_default(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.default")
    return ReturnTuple(Box_String(""));
  }

  ReturnTuple Call_equals(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.equals")
    const BoxedValue& Var_arg1 = (args.At(0));
    const BoxedValue& Var_arg2 = (args.At(1));
    return ReturnTuple(Box_Bool(Var_arg1.AsString()==Var_arg2.AsString()));
  }

  ReturnTuple Call_fromCharBuffer(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.fromCharBuffer")
    const BoxedValue& Var_arg1 = (args.At(0));
    return ReturnTuple(Box_String(PrimString(Var_arg1.AsCharBuffer())));
  }

  ReturnTuple Call_lessThan(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.lessThan")
    const BoxedValue& Var_arg1 = (args.At(0));
    const BoxedValue& Var_arg2 = (args.At(1));
    return ReturnTuple(Box_Bool(Var_arg1.AsString()<Var_arg2.AsString()));
  }
};

struct StringOrder : public AnonymousOrder {
  StringOrder(BoxedValue container, const std::string& s)
    : AnonymousOrder(container, Function_Order_next, Function_Order_get), value(s) {}

  BoxedValue Call_next(const BoxedValue& self) final {
    if (index+1 >= value.size()) {
      return Var_empty;
    } else {
      ++index;
      return self;
    }
  }

  BoxedValue Call_get(const BoxedValue& self) final {
    if (index >= value.size()) FAIL() << "Iterated past end of String";
    return Box_Char(value[index]);
  }

  const std::string& value;
  int index = 0;
};

struct ExtValue_String : public Value_String {
  inline ExtValue_String(S<Type_String> p, const PrimString& value) : Value_String(p), value_(value) {}

  ReturnTuple Call_asBool(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.asBool")
    return ReturnTuple(Box_Bool(value_.size() != 0));
  }

  ReturnTuple Call_defaultOrder(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.defaultOrder")
    if (value_.empty()) {
      return ReturnTuple(Var_empty);
    } else {
      return ReturnTuple(BoxedValue::New<StringOrder>(VAR_SELF, value_));
    }
  }

  ReturnTuple Call_formatted(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.formatted")
    return ReturnTuple(VAR_SELF);
  }

  ReturnTuple Call_readAt(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.readAt")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    if (Var_arg1 < 0 || Var_arg1 >= value_.size()) {
      FAIL() << "Read position " << Var_arg1 << " is out of bounds";
    }
    return ReturnTuple(Box_Char(value_[Var_arg1]));
  }

  ReturnTuple Call_size(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.size")
    return ReturnTuple(Box_Int(value_.size()));
  }

  ReturnTuple Call_subSequence(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("String.subSequence")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    const PrimInt Var_arg2 = (args.At(1)).AsInt();
    if (Var_arg1 < 0 || Var_arg1 > value_.size()) {
      FAIL() << "Subsequence position " << Var_arg1 << " is out of bounds";
    }
    if (Var_arg2 < 0 || Var_arg1 + Var_arg2 > value_.size()) {
      FAIL() << "Subsequence size " << Var_arg2 << " is invalid";
    }
    return ReturnTuple(Box_String(value_.substr(Var_arg1,Var_arg2)));
  }

  const PrimString& AsString() const final { return value_; }

  const PrimString value_;
};

Category_String& CreateCategory_String() {
  static auto& category = *new ExtCategory_String();
  return category;
}
S<Type_String> CreateType_String(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_String(CreateCategory_String(), Params<0>::Type()));
  return cached;
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue Box_String(const PrimString& value) {
  return BoxedValue::New<ExtValue_String>(CreateType_String(Params<0>::Type()), value);
}
