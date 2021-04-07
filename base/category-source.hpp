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

#ifndef CATEGORY_SOURCE_HPP_
#define CATEGORY_SOURCE_HPP_

#include <iostream>  // For occasional debugging output in generated code.
#include <map>
#include <sstream>
#include <vector>

#include "cycle-check.hpp"
#include "types.hpp"
#include "function.hpp"


#define BUILTIN_FAIL(e) { \
  FAIL() << TypeValue::Call((e), Function_Formatted_formatted, \
                            ParamTuple(), ArgTuple()).Only()->AsString(); \
  __builtin_unreachable(); \
  }

#define RAW_FAIL(e) { \
  FAIL() << e; \
  __builtin_unreachable(); \
  }

extern const S<TypeValue>& Var_empty;

S<TypeValue> Box_Bool(bool value);
S<TypeValue> Box_String(const PrimString& value);
S<TypeValue> Box_Char(PrimChar value);
S<TypeValue> Box_Int(PrimInt value);
S<TypeValue> Box_Float(PrimFloat value);

S<TypeInstance> Merge_Intersect(L<S<const TypeInstance>> params);
S<TypeInstance> Merge_Union(L<S<const TypeInstance>> params);

const S<TypeInstance>& GetMerged_Any();
const S<TypeInstance>& GetMerged_All();

extern const S<TypeValue>& Var_empty;


class TypeCategory {
 public:
  inline ReturnTuple Call(const CategoryFunction& label,
                          const ParamTuple& params, const ValueTuple& args) {
    return FAIL_WHEN_NULL(Dispatch(label, params, FAIL_WHEN_NULL(args)));
  }

  virtual std::string CategoryName() const = 0;

  ALWAYS_PERMANENT(TypeCategory)
  virtual ~TypeCategory() = default;

 protected:
  TypeCategory() = default;

  virtual ReturnTuple Dispatch(const CategoryFunction& label,
                               const ParamTuple& params, const ValueTuple& args);
};

class TypeInstance {
 public:
  inline static ReturnTuple Call(const S<TypeInstance>& target,
                                 const TypeFunction& label,
                                 ParamTuple params, const ValueTuple& args) {
    if (target == nullptr) {
      FAIL() << "Function called on null value";
    }
    return FAIL_WHEN_NULL(target->Dispatch(target, label, params, FAIL_WHEN_NULL(args)));
  }

  virtual std::string CategoryName() const = 0;
  virtual void BuildTypeName(std::ostream& output) const = 0;


  static std::string TypeName(const S<const TypeInstance>& type) {
    TRACE_FUNCTION("typename")
    std::ostringstream output;
    type->BuildTypeName(output);
    return output.str();
  }

  static S<TypeValue> Reduce(const S<const TypeInstance>& from,
                             const S<const TypeInstance>& to, S<TypeValue> target) {
    TRACE_FUNCTION("reduce")
    return CanConvert(from, to)? target : Var_empty;
  }

  virtual bool TypeArgsForParent(
    const TypeCategory& category, std::vector<S<const TypeInstance>>& args) const
  { return false; }

  ALWAYS_PERMANENT(TypeInstance)
  virtual ~TypeInstance() = default;

 protected:
  TypeInstance() = default;

  virtual ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label,
                               const ParamTuple& params, const ValueTuple& args);

  virtual bool CanConvertFrom(const S<const TypeInstance>& from) const
  { return false; }

  static bool CanConvert(const S<const TypeInstance>& from, const S<const TypeInstance>& to);

  template<class...Ts>
  static void TypeNameFrom(std::ostream& output, const TypeCategory& category,
                           const Ts&... params) {
    std::vector<const TypeInstance*> params2{params.get()...};
    output << category.CategoryName();
    if (!params2.empty()) {
      output << "<";
      bool first = true;
      for (const auto param : params2) {
        if (!first) output << ",";
        first = false;
        param->BuildTypeName(output);
      }
      output << ">";
    }
  }

  enum class MergeType {
    SINGLE,
    UNION,
    INTERSECT,
  };

 private:
  virtual MergeType InstanceMergeType() const
  { return MergeType::SINGLE; }

  virtual std::vector<S<const TypeInstance>> MergedTypes() const {
    FAIL() << "Category " << CategoryName() << " is not a merged type";
    __builtin_unreachable();
  }
};

class TypeValue {
 public:
  inline static ReturnTuple Call(const S<TypeValue>& target,
                                 const ValueFunction& label,
                                 const ParamTuple& params, const ValueTuple& args) {
    if (target == nullptr) {
      FAIL() << "Function called on null value";
    }
    return FAIL_WHEN_NULL(target->Dispatch(target, label, params, FAIL_WHEN_NULL(args)));
  }

  static bool Present(S<TypeValue> target);
  static S<TypeValue> Require(S<TypeValue> target);
  static S<TypeValue> Strong(W<TypeValue> target);

  virtual bool AsBool() const;
  virtual const PrimString& AsString() const;
  virtual PrimChar AsChar() const;
  virtual PrimCharBuffer& AsCharBuffer();
  virtual PrimInt AsInt() const;
  virtual PrimFloat AsFloat() const;

  ALWAYS_PERMANENT(TypeValue)
  virtual ~TypeValue() = default;

 protected:
  TypeValue() = default;

  // NOTE: For some reason, making this private causes a segfault.
  virtual std::string CategoryName() const = 0;

  virtual bool Present() const;

  virtual ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label,
                               const ParamTuple& params, const ValueTuple& args);
};

class AnonymousOrder : public TypeValue {
 protected:
  // Passing in the function labels allows linking without depending on Order
  // when this class isn't used anywhere.
  AnonymousOrder(const S<TypeValue> cont,
                 const ValueFunction& func_next,
                 const ValueFunction& func_get);

  std::string CategoryName() const final;
  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const ValueFunction& label,
                       const ParamTuple& params,
                       const ValueTuple& args) final;

  virtual ~AnonymousOrder() = default;

 private:
  virtual S<TypeValue> Call_next(const S<TypeValue>& self) = 0;
  virtual S<TypeValue> Call_get(const S<TypeValue>& self) = 0;

  const S<TypeValue> container;
  const ValueFunction& function_next;
  const ValueFunction& function_get;
};

template <int P, class T>
class InstanceCache {
 public:
  using Creator = std::function<S<T>(typename Params<P>::Type)>;

  InstanceCache(const Creator& create) : create_(create) {}

  S<T> GetOrCreate(typename Params<P>::Type params) {
    while (lock_.test_and_set(std::memory_order_acquire));
    auto& cached = cache_[GetKeyFromParams<P>(params)];
    S<T> type = cached;
    if (!type) {
      cached = type = create_(params);
    }
    lock_.clear(std::memory_order_release);
    return type;
  }

 private:
  const Creator create_;
  std::atomic_flag lock_ = ATOMIC_FLAG_INIT;
  std::map<typename ParamsKey<P>::Type, S<T>> cache_;
};

#endif  // CATEGORY_SOURCE_HPP_
