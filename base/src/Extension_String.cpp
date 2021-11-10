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

  ReturnTuple Dispatch(const ValueFunction& label,
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
      output_ << TypeValue::Call(args.At(0), Function_Formatted_formatted, ParamTuple(), ArgTuple()).Only().AsString();
      return ReturnTuple(VAR_SELF);
    }
    if (&label == &Function_Build_build) {
      TRACE_FUNCTION("StringBuilder.build")
      std::lock_guard<std::mutex> lock(mutex);
      return ReturnTuple(Box_String(output_.str()));
    }
    return TypeValue::Dispatch(label, params, args);
  }

 private:
  std::mutex mutex;
  std::ostringstream output_;
};

struct ExtType_String : public Type_String {
  inline ExtType_String(Category_String& p, Params<0>::Type params) : Type_String(p, params) {}

  ReturnTuple Call_builder(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("String.builder")
    return ReturnTuple(BoxedValue::New<Value_StringBuilder>());
  }

  ReturnTuple Call_default(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("String.default")
    return ReturnTuple(Box_String(""));
  }

  ReturnTuple Call_equals(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("String.equals")
    const BoxedValue& Var_arg1 = (args.At(0));
    const BoxedValue& Var_arg2 = (args.At(1));
    return ReturnTuple(Box_Bool(Var_arg1.AsString()==Var_arg2.AsString()));
  }

  ReturnTuple Call_fromCharBuffer(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("String.fromCharBuffer")
    const BoxedValue& Var_arg1 = (args.At(0));
    return ReturnTuple(Box_String(PrimString(Var_arg1.AsCharBuffer())));
  }

  ReturnTuple Call_lessThan(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("String.lessThan")
    const BoxedValue& Var_arg1 = (args.At(0));
    const BoxedValue& Var_arg2 = (args.At(1));
    return ReturnTuple(Box_Bool(Var_arg1.AsString()<Var_arg2.AsString()));
  }
};

class StringOrder : public TypeValue {
 public:
  StringOrder(BoxedValue container, const std::string& s)
    : container_(container), value_(s) {}

  std::string CategoryName() const final { return "StringOrder"; }

  ReturnTuple Dispatch(const ValueFunction& label,
                       const ParamTuple& params,
                       const ValueTuple& args) final {
    if (&label == &Function_Order_next) {
      TRACE_FUNCTION("StringOrder.next")
      if (index_+1 >= value_.size()) {
        return ReturnTuple(Var_empty);
      } else {
        ++index_;
        return ReturnTuple(VAR_SELF);
      }
    }
    if (&label == &Function_Order_get) {
      TRACE_FUNCTION("StringOrder.get")
      if (index_ >= value_.size()) {
        FAIL() << "Iterated past end of String";
      }
      return ReturnTuple(Box_Char(value_[index_]));
    }
    return TypeValue::Dispatch(label, params, args);
  }

 private:
  const BoxedValue container_;
  const std::string& value_;
  int index_ = 0;
};

struct ExtValue_String : public Value_String {
  inline ExtValue_String(S<const Type_String> p, const PrimString& value)
    : Value_String(std::move(p)), value_(value) {}

  ReturnTuple Call_asBool(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("String.asBool")
    return ReturnTuple(Box_Bool(value_.size() != 0));
  }

  ReturnTuple Call_defaultOrder(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("String.defaultOrder")
    if (value_.empty()) {
      return ReturnTuple(Var_empty);
    } else {
      return ReturnTuple(BoxedValue::New<StringOrder>(VAR_SELF, value_));
    }
  }

  ReturnTuple Call_formatted(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("String.formatted")
    return ReturnTuple(VAR_SELF);
  }

  ReturnTuple Call_readAt(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("String.readAt")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    if (Var_arg1 < 0 || Var_arg1 >= value_.size()) {
      FAIL() << "Read position " << Var_arg1 << " is out of bounds";
    }
    return ReturnTuple(Box_Char(value_[Var_arg1]));
  }

  ReturnTuple Call_size(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("String.size")
    return ReturnTuple(Box_Int(value_.size()));
  }

  ReturnTuple Call_subSequence(const ParamTuple& params, const ValueTuple& args) const final {
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

S<const Type_String> CreateType_String(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_String(CreateCategory_String(), Params<0>::Type()));
  return cached;
}

void RemoveType_String(const Params<0>::Type& params) {}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue Box_String(const PrimString& value) {
  return BoxedValue::New<ExtValue_String>(CreateType_String(Params<0>::Type()), value);
}
