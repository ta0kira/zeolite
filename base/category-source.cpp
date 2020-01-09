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

#include "category-source.hpp"

#include "logging.hpp"
#include "builtin.hpp"


ReturnTuple TypeCategory::Dispatch(const DFunction<SymbolScope::CATEGORY>& label,
                                   const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  __builtin_unreachable();
}

ReturnTuple TypeInstance::Dispatch(const DFunction<SymbolScope::TYPE>& label,
                                   const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  __builtin_unreachable();
}

ReturnTuple TypeValue::Dispatch(const S<TypeValue>& self,
                                const DFunction<SymbolScope::VALUE>& label,
                                const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  __builtin_unreachable();
}

bool TypeInstance::CanConvert(const TypeInstance& x, const TypeInstance& y) {
  if (&x == &y) {
    return true;
  }
  if (x.InstanceMergeType() == MergeType::SINGLE &&
      y.InstanceMergeType() == MergeType::SINGLE) {
    return y.CanConvertFrom(x);
  }
  return ExpandCheckLeft(x,y);
}

bool TypeInstance::ExpandCheckLeft(const TypeInstance& x, const TypeInstance& y) {
  for (const TypeInstance* left : x.MergedTypes()) {
    const bool result = ExpandCheckRight(*left,y);
    switch (x.InstanceMergeType()) {
      case MergeType::SINGLE:
        return result;
      case MergeType::UNION:
        if (!result) {
          return false;
        }
        break;
      case MergeType::INTERSECT:
        if (result) {
          return true;
        }
        break;
    }
  }
  switch (x.InstanceMergeType()) {
    case MergeType::SINGLE:    return false;
    case MergeType::UNION:     return true;
    case MergeType::INTERSECT: return false;
  }
}

bool TypeInstance::ExpandCheckRight(const TypeInstance& x, const TypeInstance& y) {
  for (const TypeInstance* right : y.MergedTypes()) {
    const bool result = TypeInstance::CanConvert(x,*right);
    switch (y.InstanceMergeType()) {
      case MergeType::SINGLE:
        return result;
      case MergeType::UNION:
        if (result) {
          return true;
        }
        break;
      case MergeType::INTERSECT:
        if (!result) {
          return false;
        }
        break;
    }
  }
  switch (y.InstanceMergeType()) {
    case MergeType::SINGLE:    return false;
    case MergeType::UNION:     return false;
    case MergeType::INTERSECT: return true;
  }
}

bool TypeValue::Present(S<TypeValue> target) {
  if (target == nullptr) {
    FAIL() << "Builtin called on null value";
  }
  return target->Present();
}

S<TypeValue> TypeValue::Require(S<TypeValue> target) {
  if (target == nullptr) {
    FAIL() << "Builtin called on null value";
  }
  if (!target->Present()) {
    FAIL() << "Cannot require empty value";
  }
  return target;
}

S<TypeValue> TypeValue::Strong(W<TypeValue> target) {
  const auto strong = target.lock();
  return strong? strong : Var_empty;
}

bool TypeValue::AsBool() const {
  FAIL() << CategoryName() << " is not a Bool value";
  __builtin_unreachable();
}

PrimString TypeValue::AsString() const {
  FAIL() << CategoryName() << " is not a String value";
  __builtin_unreachable();
}

PrimChar TypeValue::AsChar() const {
  FAIL() << CategoryName() << " is not a Char value";
  __builtin_unreachable();
}

PrimInt TypeValue::AsInt() const {
  FAIL() << CategoryName() << " is not an Int value";
  __builtin_unreachable();
}

PrimFloat TypeValue::AsFloat() const {
  FAIL() << CategoryName() << " is not a Float value";
  __builtin_unreachable();
}

bool TypeValue::Present() const {
  return true;
}
