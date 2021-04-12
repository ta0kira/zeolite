/* -----------------------------------------------------------------------------
Copyright 2021 Kevin P. Barry

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

#ifndef BOXED_HPP_
#define BOXED_HPP_

#include <atomic>

#include "function.hpp"
#include "types.hpp"


namespace zeolite_internal {

struct UnionValue {
  // NOTE: Using enum class would break the switch/case logic.
  // NOTE: These enum values assume that dynamic allocation will never be
  // aligned to an odd address within a few bytes of ULLONG_MAX.
  enum Type : unsigned long long {
    EMPTY = ~0x00ULL,
    BOOL  = ~0x02ULL,
    CHAR  = ~0x04ULL,
    INT   = ~0x06ULL,
    FLOAT = ~0x08ULL,
  };

  struct Counters {
    std::atomic_ullong strong_;
    std::atomic_int weak_;
  };

  union {
    Counters* counters_;
    Type value_type_;
  } type_;

  union {
    TypeValue* as_pointer_;
    bool       as_bool_;
    PrimChar   as_char_;
    PrimInt    as_int_;
    PrimFloat  as_float_;
  } value_;
};

}  // namespace zeolite_internal


class BoxedValue {
 public:
  BoxedValue();

  BoxedValue(const BoxedValue&);
  BoxedValue& operator = (const BoxedValue&);
  BoxedValue(BoxedValue&&);
  BoxedValue& operator = (BoxedValue&&);

  BoxedValue(bool value);
  BoxedValue(PrimChar value);
  BoxedValue(PrimInt value);
  BoxedValue(PrimFloat value);
  BoxedValue(TypeValue* value);

  ~BoxedValue();

  bool AsBool() const;
  PrimChar AsChar() const;
  PrimInt AsInt() const;
  PrimFloat AsFloat() const;

  const PrimString& AsString() const;
  PrimCharBuffer& AsCharBuffer() const;

  static bool Present(const BoxedValue& target);
  static BoxedValue Require(const BoxedValue& target);
  static BoxedValue Strong(const WeakValue& target);

 private:
  friend class TypeValue;
  friend class WeakValue;

  inline explicit BoxedValue(std::nullptr_t) : BoxedValue() {}

  explicit BoxedValue(const WeakValue& other);

  std::string CategoryName() const;

  ReturnTuple Dispatch(const BoxedValue& self, const ValueFunction& label,
                       const ParamTuple& params, const ValueTuple& args) const;

  void Cleanup();

  zeolite_internal::UnionValue union_;
};


class WeakValue {
 public:
  WeakValue();

  WeakValue(const WeakValue&);
  WeakValue& operator = (const WeakValue&);
  WeakValue(WeakValue&&);
  WeakValue& operator = (WeakValue&&);

  WeakValue(const BoxedValue& other);
  WeakValue& operator = (const BoxedValue& other);

  ~WeakValue();

 private:
  friend class BoxedValue;

  void Cleanup();

  zeolite_internal::UnionValue union_;
};

#endif  // BOXED_HPP_
