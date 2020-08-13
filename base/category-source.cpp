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
  Type_Intersect(L<S<const TypeInstance>> params) : params_(params.begin(), params.end()) {}

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

  std::vector<S<const TypeInstance>> MergedTypes() const final
  { return params_; }

  const L<S<const TypeInstance>> params_;
};

struct Type_Union : public TypeInstance {
  Type_Union(L<S<const TypeInstance>> params) : params_(params.begin(), params.end()) {}

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

  std::vector<S<const TypeInstance>> MergedTypes() const final
  { return params_; }

  const L<S<const TypeInstance>> params_;
};

}  // namespace


S<TypeInstance> Merge_Intersect(L<S<const TypeInstance>> params) {
  // Caching is more work than just creating a new instance.
  return S_get(new Type_Intersect(params));
}

S<TypeInstance> Merge_Union(L<S<const TypeInstance>> params) {
  // Caching is more work than just creating a new instance.
  return S_get(new Type_Union(params));
}

const S<TypeInstance>& GetMerged_Any() {
  static const auto instance = Merge_Intersect(L_get<S<const TypeInstance>>());
  return instance;
}

const S<TypeInstance>& GetMerged_All() {
  static const auto instance = Merge_Union(L_get<S<const TypeInstance>>());
  return instance;
}


const S<TypeValue>& Var_empty = *new S<TypeValue>(new OptionalEmpty());


ReturnTuple TypeCategory::Dispatch(const CategoryFunction& label,
                                   const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label;
  __builtin_unreachable();
}

ReturnTuple TypeInstance::Dispatch(const S<TypeInstance>& self, const TypeFunction& label,
                                   const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label;
  __builtin_unreachable();
}

ReturnTuple TypeValue::Dispatch(const S<TypeValue>& self, const ValueFunction& label,
                                const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label;
  __builtin_unreachable();
}

bool TypeInstance::CanConvert(const S<const TypeInstance>& x,
                              const S<const TypeInstance>& y) {
  // See checkGeneralType for the ordering here.
  if (x.get() == y.get()) {
    return true;
  } else if (y->InstanceMergeType() == MergeType::INTERSECT) {
    for (const auto& right : y->MergedTypes()) {
      if (!TypeInstance::CanConvert(x, right)) {
        return false;
      }
    }
    return true;
  } else if (x->InstanceMergeType() == MergeType::UNION) {
    for (const auto& left : x->MergedTypes()) {
      if (!TypeInstance::CanConvert(left, y)) {
        return false;
      }
    }
    return true;
  } else if (x->InstanceMergeType() == MergeType::INTERSECT) {
    for (const auto& left : x->MergedTypes()) {
      if (TypeInstance::CanConvert(left, y)) {
        return true;
      }
    }
    return false;
  } else if (y->InstanceMergeType() == MergeType::UNION) {
    for (const auto right : y->MergedTypes()) {
      if (TypeInstance::CanConvert(x, right)) {
        return true;
      }
    }
    return false;
  } else {
    return y->CanConvertFrom(x);
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

const PrimString& TypeValue::AsString() const {
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
