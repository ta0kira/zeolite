/* -----------------------------------------------------------------------------
Copyright 2019-2023 Kevin P. Barry

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

#include <cstdlib>
#include <iostream>  // For occasional debugging output in generated code.
#include <map>
#include <sstream>
#include <vector>

#include "boxed.hpp"
#include "cleanup.hpp"
#include "cycle-check.hpp"
#include "function.hpp"
#include "returns.hpp"
#include "types.hpp"


#define BUILTIN_FAIL(e) { \
    FAIL() << TypeValue::Call((e), Function_Formatted_formatted, \
                              PassParamsArgs()).At(0).AsString(); \
    __builtin_unreachable(); \
  }

#define BUILTIN_EXIT(e) { \
    int code = e; \
    GlobalCleanup::Finish(code); \
    std::exit(code); \
    __builtin_unreachable(); \
  }

#define RAW_FAIL(e) { \
    FAIL() << e; \
    __builtin_unreachable(); \
  }

#define VAR_SELF TypeValue::Var_self(this)

#define PARAM_SELF Param_self()

inline BoxedValue Box_Bool(PrimBool value) {
  return BoxedValue(value);
}

inline BoxedValue Box_Char(PrimChar value) {
  return BoxedValue(value);
}

inline BoxedValue Box_Float(PrimFloat value) {
  return BoxedValue(value);
}

inline BoxedValue Box_Int(PrimInt value) {
  return BoxedValue(value);
}

template<class T1, class T2>
inline BoxedValue Box_Pointer(T2* value) {
  return BoxedValue(reinterpret_cast<PrimPointer>(static_cast<T1*>(value)));
}

inline BoxedValue Box_Identifier(PrimIdentifier value) {
  return BoxedValue(value);
}

BoxedValue Box_String(const PrimString& value);

S<const TypeInstance> Merge_Intersect(const L<S<const TypeInstance>>& params);
S<const TypeInstance> Merge_Union(const L<S<const TypeInstance>>& params);

const S<const TypeInstance>& GetMerged_Any();
const S<const TypeInstance>& GetMerged_All();

extern const BoxedValue Var_empty;


class TypeCategory {
 public:
  inline ReturnTuple Call(const CategoryFunction& label, const ParamsArgs& params_args) {
    return Dispatch(label, params_args);
  }

  virtual std::string CategoryName() const = 0;

  ALWAYS_PERMANENT(TypeCategory)
  virtual ~TypeCategory() = default;

 protected:
  TypeCategory() = default;

  virtual ReturnTuple Dispatch(const CategoryFunction& label, const ParamsArgs& params_args);
};

class TypeInstance {
 public:
  inline static ReturnTuple Call(const S<const TypeInstance>& target,
                                 const TypeFunction& label, const ParamsArgs& params_args) {
    if (target == nullptr) {
      FAIL() << "Function called on null value";
    }
    return target->Dispatch(label, params_args);
  }

  virtual std::string CategoryName() const = 0;
  virtual void BuildTypeName(std::ostream& output) const = 0;


  static std::string TypeName(const S<const TypeInstance>& type) {
    TRACE_FUNCTION("typename")
    std::ostringstream output;
    type->BuildTypeName(output);
    return output.str();
  }

  static BoxedValue Reduce(const S<const TypeInstance>& from,
                           const S<const TypeInstance>& to, BoxedValue target) {
    TRACE_FUNCTION("reduce")
    return CanConvert(from, to)? target : Var_empty;
  }

  virtual bool TypeArgsForParent(
    const CategoryId& category, std::vector<S<const TypeInstance>>& args) const
  { return false; }

  ALWAYS_PERMANENT(TypeInstance)
  virtual ~TypeInstance() = default;

 protected:
  TypeInstance() = default;

  virtual ReturnTuple Dispatch(const TypeFunction& label, const ParamsArgs& params_args) const;

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

  virtual const L<S<const TypeInstance>>& MergedTypes() const {
    FAIL() << "Category " << CategoryName() << " is not a merged type";
    __builtin_unreachable();
  }
};

#define TYPE_VALUE_CALL_UNLESS_EMPTY(expr, func, args, count) ({ \
    const BoxedValue result = expr; \
    BoxedValue::Present(result) \
        ? TypeValue::Call(result, func, args) \
        : ReturnTuple(count); \
  })

#define TYPE_VALUE_LEFT_UNLESS_EMPTY(left, right) ({ \
    const BoxedValue result = left; \
    BoxedValue::Present(result) ? result : (right); \
  })

class TypeValue {
 public:
  inline static ReturnTuple Call(const BoxedValue& target, const ValueFunction& label,
                                 const ParamsArgs& params_args) {
    return target.Dispatch(label, params_args);
  }

  virtual const PrimString& AsString() const;
  virtual PrimCharBuffer& AsCharBuffer();

  ALWAYS_PERMANENT(TypeValue)
  virtual ~TypeValue() = default;

 protected:
  friend class BoxedValue;
  friend class zeolite_internal::UnionValue;

  TypeValue() = default;

  template<class T>
  inline BoxedValue Var_self(T* must_be_this) const {
    if (must_be_this != this) {
      FAIL() << "Var_self called without passing this";
    }
    return BoxedValue::FromPointer(must_be_this);
  }

  // NOTE: For some reason, making this private causes a segfault.
  virtual std::string CategoryName() const = 0;

  virtual BoxedValue FlatCleanup() { return BoxedValue(); }

  virtual ReturnTuple Dispatch(const ValueFunction& label, const ParamsArgs& params_args);

 private:
  // Creating a BoxedValue from a TypeValue won't have the correct offset.
  inline static BoxedValue Var_self(TypeValue* invalid) {
    return BoxedValue();
  }
};

template <int P, class T>
class InstanceCache {
 public:
  using Creator = std::function<S<T>(const typename Params<P>::Type&)>;

  InstanceCache(const Creator& create) : create_(create) {}

  S<T> GetOrCreate(const typename Params<P>::Type& params) {
    while (lock_.test_and_set(std::memory_order_acquire));
    auto& cached = cache_[GetKeyFromParams<P>(params)];
    S<T> type = cached.lock();
    if (!type) {
      cached = type = create_(params);
    }
    lock_.clear(std::memory_order_release);
    return type;
  }

  void Remove(const typename Params<P>::Type& params) {
    while (lock_.test_and_set(std::memory_order_acquire));
    auto pos = cache_.find(GetKeyFromParams<P>(params));
    // Skip erasing if it's a valid pointer, since that could mean that another
    // thread created a new instance while the one we're trying to remove was in
    // the process of being destructed.
    if (pos != cache_.end() && !pos->second.lock()) {
      cache_.erase(pos);
    }
    lock_.clear(std::memory_order_release);
  }

 private:
  const Creator create_;
  std::atomic_flag lock_ = ATOMIC_FLAG_INIT;
  std::map<typename ParamsKey<P>::Type, W<T>> cache_;
};

#ifdef ZEOLITE_TESTS_ONLY__YOUR_MODULE_IS_BROKEN_IF_YOU_USE_THIS_IN_HAND_WRITTEN_CODE
BoxedValue GetCallTrace(const ValueFunction& get_func, const ValueFunction& next_func);
#endif

#endif  // CATEGORY_SOURCE_HPP_
