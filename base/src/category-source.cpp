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

void Remove_Intersect(const L<S<const TypeInstance>>& params);
void Remove_Union(const L<S<const TypeInstance>>& params);

struct Type_Intersect : public TypeInstance {
  inline Type_Intersect(L<S<const TypeInstance>> params) : params_(std::move(params)) {}

  inline ~Type_Intersect() { Remove_Intersect(params_); }

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

  const L<S<const TypeInstance>>& MergedTypes() const final
  { return params_; }

  const L<S<const TypeInstance>> params_;
};

struct Type_Union : public TypeInstance {
  inline Type_Union(L<S<const TypeInstance>> params) : params_(std::move(params)) {}

  inline ~Type_Union() { Remove_Union(params_); }

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

  const L<S<const TypeInstance>>& MergedTypes() const final
  { return params_; }

  const L<S<const TypeInstance>> params_;
};

L<const TypeInstance*> ParamsToKey(const L<S<const TypeInstance>>& params) {
  L<const TypeInstance*> key;
  for (const auto& param : params) {
    key.insert(param.get());
  }
  return key;
}

template <class T>
class MetaCache {
 public:
  S<const TypeInstance> GetOrCreate(const L<S<const TypeInstance>>& params) {
    if (params.size() == 1) {
      return *params.begin();
    }
    auto key = ParamsToKey(params);
    while (lock_.test_and_set(std::memory_order_acquire));
    auto& cached = cache_[std::move(key)];
    S<T> type = cached.lock();
    if (!type) {
      cached = type = S_get(new T(params));
    }
    lock_.clear(std::memory_order_release);
    return type;
  }

  void Remove(const L<S<const TypeInstance>>& params) {
    auto key = ParamsToKey(params);
    while (lock_.test_and_set(std::memory_order_acquire));
    auto pos = cache_.find(key);
    // Skip erasing if it's a valid pointer, since that could mean that another
    // thread created a new instance while the one we're trying to remove was in
    // the process of being destructed.
    if (pos != cache_.end() && !pos->second.lock()) {
      cache_.erase(pos);
    }
    lock_.clear(std::memory_order_release);
  }

 private:
  std::atomic_flag lock_ = ATOMIC_FLAG_INIT;
  std::map<L<const TypeInstance*>, W<T>> cache_;
};

static auto& intersect_cache = *new MetaCache<Type_Intersect>;
static auto& union_cache     = *new MetaCache<Type_Union>;

void Remove_Intersect(const L<S<const TypeInstance>>& params) {
  intersect_cache.Remove(params);
}

void Remove_Union(const L<S<const TypeInstance>>& params) {
  union_cache.Remove(params);
}

}  // namespace


S<const TypeInstance> Merge_Intersect(const L<S<const TypeInstance>>& params) {
  return intersect_cache.GetOrCreate(params);
}

S<const TypeInstance> Merge_Union(const L<S<const TypeInstance>>& params) {
  return union_cache.GetOrCreate(params);
}

const S<const TypeInstance>& GetMerged_Any() {
  static const auto instance = Merge_Intersect(L_get<S<const TypeInstance>>());
  return instance;
}

const S<const TypeInstance>& GetMerged_All() {
  static const auto instance = Merge_Union(L_get<S<const TypeInstance>>());
  return instance;
}

const BoxedValue Var_empty;


ReturnTuple TypeCategory::Dispatch(const CategoryFunction& label,
                                   const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label;
  __builtin_unreachable();
}

ReturnTuple TypeInstance::Dispatch(const TypeFunction& label,
                                   const ParamTuple& params, const ValueTuple& args) const {
  FAIL() << CategoryName() << " does not implement " << label;
  __builtin_unreachable();
}

ReturnTuple TypeValue::Dispatch(const ValueFunction& label,
                                const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label;
  __builtin_unreachable();
}

// static
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
