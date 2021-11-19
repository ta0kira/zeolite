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

#include "types.hpp"


class ParamsArgs;
class ValueFunction;

namespace zeolite_internal {

struct UnionValue {
  enum class Type : char {
    EMPTY,
    BOOL,
    CHAR,
    INT,
    FLOAT,
    BOXED,
  };

  struct Pointer {
    std::atomic_flag lock_;
    std::atomic_int strong_;
    std::atomic_int weak_;
    TypeValue* object_;
  };

  std::string CategoryName() const;

  Type type_;

  union {
    unsigned char* as_bytes_;
    Pointer*  as_pointer_;
    PrimBool  as_bool_;
    PrimChar  as_char_;
    PrimInt   as_int_;
    PrimFloat as_float_;
  } __attribute__((packed)) value_;
} __attribute__((packed));


class BoxedValue {
 public:
  constexpr BoxedValue()
    : union_{ .type_ = UnionValue::Type::EMPTY, .value_ = { .as_pointer_ = nullptr } } {}

  inline BoxedValue(const BoxedValue& other)
    : union_(other.union_) {
    switch (union_.type_) {
      case UnionValue::Type::BOXED:
        ++union_.value_.as_pointer_->strong_;
        break;
      default:
        break;
    }
  }

  inline BoxedValue& operator = (const BoxedValue& other) {
    if (&other != this) {
      Cleanup();
      union_ = other.union_;
      switch (union_.type_) {
        case UnionValue::Type::BOXED:
          ++union_.value_.as_pointer_->strong_;
          break;
        default:
          break;
      }
    }
    return *this;
  }

  inline BoxedValue(BoxedValue&& other)
    : union_(other.union_) {
    other.union_.type_  = UnionValue::Type::EMPTY;
    other.union_.value_.as_pointer_ = nullptr;
  }

  inline BoxedValue& operator = (BoxedValue&& other) {
    if (&other != this) {
      Cleanup();
      union_ = other.union_;
      other.union_.type_  = UnionValue::Type::EMPTY;
      other.union_.value_.as_pointer_ = nullptr;
    }
    return *this;
  }

  inline BoxedValue(PrimBool value)
    : union_{ .type_ = UnionValue::Type::BOOL, .value_ = { .as_bool_ = value } } {}

  inline BoxedValue(PrimChar value)
    : union_{ .type_ = UnionValue::Type::CHAR, .value_ = { .as_char_ = value } } {}

  inline BoxedValue(PrimInt value)
    : union_{ .type_ = UnionValue::Type::INT, .value_ = { .as_int_ = value } } {}

  inline BoxedValue(PrimFloat value)
    : union_{ .type_ = UnionValue::Type::FLOAT, .value_ = { .as_float_ = value } } {}

  template<class T, class... As>
  static inline BoxedValue New(const As&... args) {
    using Pointer = UnionValue::Pointer;
    BoxedValue new_value;
    new_value.union_.type_ = UnionValue::Type::BOXED;
    new_value.union_.value_.as_bytes_ = (unsigned char*) malloc(sizeof(Pointer) + sizeof(T));
    new (new_value.union_.value_.as_bytes_)
      Pointer{ ATOMIC_FLAG_INIT, {1}, {1},
               {new (new_value.union_.value_.as_bytes_ + sizeof(Pointer)) T(args...)} };
    return new_value;
  }

  inline ~BoxedValue() {
    Cleanup();
  }

  inline PrimBool AsBool() const {
    switch (union_.type_) {
      case UnionValue::Type::BOOL:
        return union_.value_.as_bool_;
      default:
        FAIL() << union_.CategoryName() << " is not a Bool value";
        __builtin_unreachable();
        break;
    }
  }

  inline PrimChar AsChar() const {
    switch (union_.type_) {
      case UnionValue::Type::CHAR:
        return union_.value_.as_char_;
      default:
        FAIL() << union_.CategoryName() << " is not a Char value";
        __builtin_unreachable();
        break;
    }
  }

  inline PrimInt AsInt() const {
    switch (union_.type_) {
      case UnionValue::Type::INT:
        return union_.value_.as_int_;
      default:
        FAIL() << union_.CategoryName() << " is not an Int value";
        __builtin_unreachable();
        break;
    }
  }

  inline PrimFloat AsFloat() const {
    switch (union_.type_) {
      case UnionValue::Type::FLOAT:
        return union_.value_.as_float_;
      default:
        FAIL() << union_.CategoryName() << " is not a Float value";
        __builtin_unreachable();
        break;
    }
  }

  inline static bool Present(const BoxedValue& target) {
    return target.union_.type_ != UnionValue::Type::EMPTY;
  }

  inline static BoxedValue Require(const BoxedValue& target) {
    switch (target.union_.type_) {
      case UnionValue::Type::EMPTY:
        FAIL() << "Cannot require empty value";
        __builtin_unreachable();
        break;
      default:
        return target;
    }
  }

  inline static BoxedValue Strong(const WeakValue& target) {
    return BoxedValue(target);
  }

  void Validate(const std::string& name) const;

  const PrimString& AsString() const;
  PrimCharBuffer& AsCharBuffer() const;

 private:
  friend class ::TypeValue;
  friend class WeakValue;

  // Intentionally break old calls that used new.
  inline explicit constexpr BoxedValue(void*) : BoxedValue() {}

  inline explicit constexpr BoxedValue(std::nullptr_t) : BoxedValue() {}

  explicit BoxedValue(const WeakValue& other);

  template<class T>
  static inline BoxedValue FromPointer(const T* pointer) {
    BoxedValue value;
    value.union_.type_ = UnionValue::Type::BOXED;
    value.union_.value_.as_bytes_ =
      reinterpret_cast<unsigned char*>(const_cast<T*>(pointer))-sizeof(UnionValue::Pointer);
    if (value.union_.value_.as_pointer_->object_ != pointer ||
        ++value.union_.value_.as_pointer_->strong_ == 1) {
      FAIL() << "Bad VAR_SELF pointer " << pointer << " in " << pointer->CategoryName();
    }
    return value;
  }

  ReturnTuple Dispatch(const ::ValueFunction& label, const ::ParamsArgs& params_args) const;

  void Cleanup();

  UnionValue union_;
} __attribute__((packed));


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

  void Validate(const std::string& name) const;

 private:
  friend class BoxedValue;

  void Cleanup();

  UnionValue union_;
} __attribute__((packed));

}  // namespace zeolite_internal

using zeolite_internal::BoxedValue;
using zeolite_internal::WeakValue;

#endif  // BOXED_HPP_
