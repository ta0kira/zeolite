/* -----------------------------------------------------------------------------
Copyright 2023 Kevin P. Barry

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

#include <iomanip>
#include <sstream>

#include "category-source.hpp"
#include "Streamlined_Identifier.hpp"
#include "Category_Equals.hpp"
#include "Category_Formatted.hpp"
#include "Category_Hashed.hpp"
#include "Category_LessThan.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

struct ExtCategory_Identifier : public Category_Identifier {
};

struct ExtType_Identifier : public Type_Identifier {
  inline ExtType_Identifier(Category_Identifier& p, Params<1>::Type params) : Type_Identifier(p, params) {}

  ReturnTuple Call_equals(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Identifier.equals")
    const PrimIdentifier Var_arg1 = (params_args.GetArg(0)).AsIdentifier();
    const PrimIdentifier Var_arg2 = (params_args.GetArg(1)).AsIdentifier();
    return ReturnTuple(Box_Bool(Var_arg1==Var_arg2));
  }

  ReturnTuple Call_lessThan(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("Identifier.lessThan")
    const PrimIdentifier Var_arg1 = (params_args.GetArg(0)).AsIdentifier();
    const PrimIdentifier Var_arg2 = (params_args.GetArg(1)).AsIdentifier();
    return ReturnTuple(Box_Bool(Var_arg1<Var_arg2));
  }
};

ReturnTuple DispatchIdentifier(PrimIdentifier value, const ValueFunction& label,
                             const ParamsArgs& params_args) {
  switch (label.collection) {
    case CategoryId_Formatted: {
      std::ostringstream output;
      output << std::hex << std::setfill('0') << std::setw(16) << (unsigned long long) value;
      return ReturnTuple(Box_String(output.str()));
    }
    case CategoryId_Hashed:
      return ReturnTuple(Box_Int(1000000009ULL * ((unsigned long long) value + 1000000007ULL)));
    default:
      FAIL() << "Identifier does not implement " << label;
      __builtin_unreachable();
  }
}

Category_Identifier& CreateCategory_Identifier() {
  static auto& category = *new ExtCategory_Identifier();
  return category;
}

static auto& Identifier_instance_cache = *new InstanceCache<1, Type_Identifier>([](const Params<1>::Type& params) {
    return S_get(new ExtType_Identifier(CreateCategory_Identifier(), params));
  });

S<const Type_Identifier> CreateType_Identifier(const Params<1>::Type& params) {
  return Identifier_instance_cache.GetOrCreate(params);
}

void RemoveType_Identifier(const Params<1>::Type& params) {
  Identifier_instance_cache.Remove(params);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
