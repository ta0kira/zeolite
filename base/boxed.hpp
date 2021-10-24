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

#include <cstddef>
#include <cstdlib>
#include <atomic>

#include "function.hpp"
#include "types.hpp"


namespace zeolite_internal {

struct UnionValue {
  enum class Type {
    EMPTY,
    BOOL,
    CHAR,
    INT,
    FLOAT,
    BOXED,
  };

  struct Pointer {
    std::atomic_ullong strong_;
    std::atomic_int weak_;
    TypeValue* object_;
  };

  Type type_;

  union {
    char*     as_bytes_;
    Pointer*  as_pointer_;
    bool      as_bool_;
    PrimChar  as_char_;
    PrimInt   as_int_;
    PrimFloat as_float_;
  } value_;
};


class BoxedValue {
 public:
  constexpr BoxedValue()
    : union_{ .type_ = UnionValue::Type::EMPTY, .value_ = { .as_pointer_ = nullptr } } {}

  BoxedValue(const BoxedValue&);
  BoxedValue& operator = (const BoxedValue&);
  BoxedValue(BoxedValue&&);
  BoxedValue& operator = (BoxedValue&&);

  BoxedValue(bool value);
  BoxedValue(PrimChar value);
  BoxedValue(PrimInt value);
  BoxedValue(PrimFloat value);

  template<class T, class... As>
  static inline BoxedValue New(const As&... args) {
    using Pointer = UnionValue::Pointer;
    BoxedValue new_value;
    new_value.union_.type_ = UnionValue::Type::BOXED;
    new_value.union_.value_.as_bytes_ = (char*) malloc(sizeof(Pointer) + sizeof(T));
    new (new_value.union_.value_.as_bytes_)
      Pointer{ {1}, {1},
               new (new_value.union_.value_.as_bytes_ + sizeof(Pointer)) T(args...) };
    return new_value;
  }

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
  friend class ::TypeValue;
  friend class WeakValue;

  // Intentionally break old calls that used new.
  inline explicit constexpr BoxedValue(void*) : BoxedValue() {}

  inline explicit constexpr BoxedValue(std::nullptr_t) : BoxedValue() {}

  explicit BoxedValue(const WeakValue& other);

  template<class T>
  static inline BoxedValue FromPointer(T* pointer) {
    BoxedValue value;
    if (pointer) {
      value.union_.type_ = UnionValue::Type::BOXED;
      value.union_.value_.as_bytes_ =
        reinterpret_cast<char*>(pointer)-sizeof(UnionValue::Pointer);
      ++value.union_.value_.as_pointer_->strong_;
      if (value.union_.value_.as_pointer_->object_ != pointer) {
        FAIL() << "Bad VAR_SELF pointer";
      }
    }
    return value;
  }

  std::string CategoryName() const;

  ReturnTuple Dispatch(
    const ValueFunction& label, const ParamTuple& params, const ValueTuple& args) const;

  void Cleanup();

  UnionValue union_;
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

  UnionValue union_;
};

}  // namespace zeolite_internal

using zeolite_internal::BoxedValue;
using zeolite_internal::WeakValue;

#endif  // BOXED_HPP_
