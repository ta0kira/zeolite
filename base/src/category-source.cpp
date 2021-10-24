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

#include <algorithm>

#include "logging.hpp"


namespace {

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

L<const TypeInstance*> ParamsToKey(const L<S<const TypeInstance>>& params) {
  L<const TypeInstance*> key;
  for (const auto& param : params) {
    key.push_back(param.get());
  }
  std::sort(key.begin(), key.end());
  return key;
}

}  // namespace


S<TypeInstance> Merge_Intersect(L<S<const TypeInstance>> params) {
  static auto& cache = *new std::map<L<const TypeInstance*>,S<Type_Intersect>>();
  auto& cached = cache[ParamsToKey(params)];
  S<Type_Intersect> type = cached;
  if (!type) { cached = type = S_get(new Type_Intersect(params)); }
  return type;
}

S<TypeInstance> Merge_Union(L<S<const TypeInstance>> params) {
  static auto& cache = *new std::map<L<const TypeInstance*>,S<Type_Union>>();
  auto& cached = cache[ParamsToKey(params)];
  S<Type_Union> type = cached;
  if (!type) { cached = type = S_get(new Type_Union(params)); }
  return type;
}

const S<TypeInstance>& GetMerged_Any() {
  static const auto instance = Merge_Intersect(L_get<S<const TypeInstance>>());
  return instance;
}

const S<TypeInstance>& GetMerged_All() {
  static const auto instance = Merge_Union(L_get<S<const TypeInstance>>());
  return instance;
}

const BoxedValue Var_empty;


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

ReturnTuple TypeValue::Dispatch(const ValueFunction& label,
                                const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label;
  __builtin_unreachable();
}

bool TypeInstance::CanConvert(const S<const TypeInstance>& x,
                              const S<const TypeInstance>& y) {
  // See pairMergeTree for the ordering here.
  // TODO: Consider using a cache here, since the check could be expensive.
  if (x.get() == y.get()) {
    return true;
  } else if (x->InstanceMergeType() == MergeType::INTERSECT &&
             y->InstanceMergeType() == MergeType::UNION) {
    for (const auto& left : x->MergedTypes()) {
      if (left->InstanceMergeType() == MergeType::SINGLE) {
        for (const auto& right : y->MergedTypes()) {
          if (right->InstanceMergeType() == MergeType::SINGLE) {
            if (TypeInstance::CanConvert(left, right)) {
              return true;
            }
          }
        }
      }
    }
    for (const auto& left : x->MergedTypes()) {
      if (TypeInstance::CanConvert(left, y)) {
        return true;
      }
    }
    for (const auto right : y->MergedTypes()) {
      if (TypeInstance::CanConvert(x, right)) {
        return true;
      }
    }
    return false;
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

const PrimString& TypeValue::AsString() const {
  FAIL() << CategoryName() << " is not a String value";
  __builtin_unreachable();
}

PrimCharBuffer& TypeValue::AsCharBuffer() {
  FAIL() << CategoryName() << " is not a CharBuffer value";
  __builtin_unreachable();
}

AnonymousOrder::AnonymousOrder(const BoxedValue cont,
                               const ValueFunction& func_next,
                               const ValueFunction& func_get)
  : container(cont), function_next(func_next), function_get(func_get) {}

std::string AnonymousOrder::CategoryName() const { return "AnonymousOrder"; }

ReturnTuple AnonymousOrder::Dispatch(
  const ValueFunction& label, const ParamTuple& params, const ValueTuple& args) {
  if (&label == &function_next) {
    TRACE_FUNCTION("AnonymousOrder.next")
    return ReturnTuple(Call_next(Var_self()));
  }
  if (&label == &function_get) {
    TRACE_FUNCTION("AnonymousOrder.get")
    return ReturnTuple(Call_get(Var_self()));
  }
  return TypeValue::Dispatch(label, params, args);
}
