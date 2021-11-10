/* -----------------------------------------------------------------------------
Copyright 2020-2021 Kevin P. Barry

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
#include "Streamlined_Argv.hpp"
#include "Category_Formatted.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

namespace {
extern const BoxedValue Var_global;
}  // namespace

struct ExtCategory_Argv : public Category_Argv {
};

struct ExtType_Argv : public Type_Argv {
  inline ExtType_Argv(Category_Argv& p, Params<0>::Type params) : Type_Argv(p, params) {}

  ReturnTuple Call_global(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("Argv.global")
    return ReturnTuple(Var_global);
  }
};

struct ExtValue_Argv : public Value_Argv {
  inline ExtValue_Argv(S<const Type_Argv> p, int st, int sz)
    : Value_Argv(std::move(p)), start(st), size(sz) {}

  ReturnTuple Call_readAt(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("Argv.readAt")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    return ReturnTuple(Box_String(Argv::GetArgAt(start + Var_arg1)));
  }

  ReturnTuple Call_size(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("Argv.size")
    return ReturnTuple(Box_Int(GetSize()));
  }

  ReturnTuple Call_subSequence(const ParamTuple& params, const ValueTuple& args) const final {
    TRACE_FUNCTION("Argv.subSequence")
    const PrimInt Var_arg1 = (args.At(0)).AsInt();
    const PrimInt Var_arg2 = (args.At(1)).AsInt();
    if (Var_arg1 < 0 || (Var_arg1 > 0 && Var_arg1 >= GetSize())) {
      FAIL() << "index " << Var_arg1 << " is out of bounds";
    }
    if (Var_arg2 < 0 || Var_arg1 + Var_arg2 > GetSize()) {
      FAIL() << "size " << Var_arg2 << " is invalid";
    }
    return ReturnTuple(BoxedValue::New<ExtValue_Argv>(parent, start + Var_arg1, Var_arg2));
  }

  inline int GetSize() const { return size < 0 ? Argv::ArgCount() : size; }

  const int start;
  const int size;
};

namespace {
const BoxedValue Var_global = BoxedValue::New<ExtValue_Argv>(CreateType_Argv(Params<0>::Type()), 0, -1);
}  // namespace

Category_Argv& CreateCategory_Argv() {
  static auto& category = *new ExtCategory_Argv();
  return category;
}

S<const Type_Argv> CreateType_Argv(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_Argv(CreateCategory_Argv(), Params<0>::Type()));
  return cached;
}

void RemoveType_Argv(const Params<0>::Type& params) {}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
