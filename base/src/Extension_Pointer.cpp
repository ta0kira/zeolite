/* -----------------------------------------------------------------------------
Copyright 2022 Kevin P. Barry

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
#include "Streamlined_Pointer.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

struct ExtCategory_Pointer : public Category_Pointer {
};

struct ExtType_Pointer : public Type_Pointer {
  inline ExtType_Pointer(Category_Pointer& p, Params<1>::Type params) : Type_Pointer(p, params) {}
};

ReturnTuple DispatchPointer(PrimPointer value, const ValueFunction& label,
                            const ParamsArgs& params_args) {
  switch (label.collection) {
    default:
      FAIL() << "Pointer does not implement " << label;
      __builtin_unreachable();
  }
}

Category_Pointer& CreateCategory_Pointer() {
  static auto& category = *new ExtCategory_Pointer();
  return category;
}

static auto& Pointer_instance_cache = *new InstanceCache<1, Type_Pointer>([](const Params<1>::Type& params) {
    return S_get(new ExtType_Pointer(CreateCategory_Pointer(), params));
  });

S<const Type_Pointer> CreateType_Pointer(const Params<1>::Type& params) {
  return Pointer_instance_cache.GetOrCreate(params);
}

void RemoveType_Pointer(const Params<1>::Type& params) {
  Pointer_instance_cache.Remove(params);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
