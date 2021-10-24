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
#include "Streamlined_CharBuffer.hpp"
#include "Category_Char.hpp"
#include "Category_CharBuffer.hpp"
#include "Category_Container.hpp"
#include "Category_Int.hpp"
#include "Category_ReadAt.hpp"
#include "Category_WriteAt.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_CharBuffer(S<Type_CharBuffer> parent, PrimCharBuffer buffer);

struct ExtCategory_CharBuffer : public Category_CharBuffer {
};

struct ExtType_CharBuffer : public Type_CharBuffer {
  inline ExtType_CharBuffer(Category_CharBuffer& p, Params<0>::Type params) : Type_CharBuffer(p, params) {}

  ReturnTuple Call_new(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("CharBuffer.new")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    if (Var_arg1 < 0) {
      FAIL() << "Buffer size " << Var_arg1 << " is invalid";
    }
    return ReturnTuple(CreateValue_CharBuffer(CreateType_CharBuffer(Params<0>::Type()), PrimCharBuffer(Var_arg1,'\0')));
  }
};

struct ExtValue_CharBuffer : public Value_CharBuffer {
  inline ExtValue_CharBuffer(S<Type_CharBuffer> p, PrimCharBuffer value) : Value_CharBuffer(p), value_(std::move(value)) {}

  ReturnTuple Call_readAt(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("CharBuffer.readAt")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    if (Var_arg1 < 0 || Var_arg1 >= AsCharBuffer().size()) {
      FAIL() << "Read position " << Var_arg1 << " is out of bounds";
    }
    return ReturnTuple(Box_Char(AsCharBuffer()[Var_arg1]));
  }

  ReturnTuple Call_resize(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("CharBuffer.resize")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    if (Var_arg1 < 0) {
      FAIL() << "Buffer size " << Var_arg1 << " is invalid";
    } else {
      value_.resize(Var_arg1);
    }
    return ReturnTuple(VAR_SELF);
  }

  ReturnTuple Call_size(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("CharBuffer.size")
    return ReturnTuple(Box_Int(value_.size()));
  }

  ReturnTuple Call_writeAt(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("CharBuffer.writeAt")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    const PrimChar Var_arg2 = (args.At(1)).AsChar();
    if (Var_arg1 < 0 || Var_arg1 >= value_.size()) {
      FAIL() << "Write position " << Var_arg1 << " is out of bounds";
    } else {
      value_[Var_arg1] = Var_arg2;
    }
    return ReturnTuple(VAR_SELF);
  }

  PrimCharBuffer& AsCharBuffer() final { return value_; }

  PrimCharBuffer value_;
};

Category_CharBuffer& CreateCategory_CharBuffer() {
  static auto& category = *new ExtCategory_CharBuffer();
  return category;
}
S<Type_CharBuffer> CreateType_CharBuffer(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_CharBuffer(CreateCategory_CharBuffer(), Params<0>::Type()));
  return cached;
}
BoxedValue CreateValue_CharBuffer(S<Type_CharBuffer> parent, PrimCharBuffer value) {
  return BoxedValue::New<ExtValue_CharBuffer>(parent, std::move(value));
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
