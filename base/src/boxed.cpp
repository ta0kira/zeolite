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

#include "boxed.hpp"

#include <climits>

#include "category-source.hpp"


#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

ReturnTuple DispatchBool(bool value, const BoxedValue& Var_self, const ValueFunction& label,
                         const ParamTuple& params, const ValueTuple& args);

ReturnTuple DispatchChar(PrimChar value, const BoxedValue& Var_self, const ValueFunction& label,
                         const ParamTuple& params, const ValueTuple& args);

ReturnTuple DispatchInt(PrimInt value, const BoxedValue& Var_self, const ValueFunction& label,
                        const ParamTuple& params, const ValueTuple& args);

ReturnTuple DispatchFloat(PrimFloat value, const BoxedValue& Var_self, const ValueFunction& label,
                          const ParamTuple& params, const ValueTuple& args);

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE


using zeolite_internal::UnionValue;


BoxedValue::BoxedValue()
  : union_{ { .value_type_ = UnionValue::Type::EMPTY },
            { .as_pointer_ = nullptr } } {}

BoxedValue::BoxedValue(const BoxedValue& other)
  : union_(other.union_) {
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY:
    case UnionValue::Type::BOOL:
    case UnionValue::Type::CHAR:
    case UnionValue::Type::INT:
    case UnionValue::Type::FLOAT:
      break;
    default:
      ++union_.type_.counters_->strong_;
      break;
  }
}

BoxedValue& BoxedValue::operator = (const BoxedValue& other) {
  if (&other != this) {
    Cleanup();
    union_ = other.union_;
    switch (union_.type_.value_type_) {
      case UnionValue::Type::EMPTY:
      case UnionValue::Type::BOOL:
      case UnionValue::Type::CHAR:
      case UnionValue::Type::INT:
      case UnionValue::Type::FLOAT:
        break;
      default:
        ++union_.type_.counters_->strong_;
        break;
    }
  }
  return *this;
}

BoxedValue::BoxedValue(BoxedValue&& other)
  : union_(other.union_) {
  other.union_.type_.value_type_  = UnionValue::Type::EMPTY;
  other.union_.value_.as_pointer_ = nullptr;
}

BoxedValue& BoxedValue::operator = (BoxedValue&& other) {
  if (&other != this) {
    Cleanup();
    union_ = other.union_;
    other.union_.type_.value_type_  = UnionValue::Type::EMPTY;
    other.union_.value_.as_pointer_ = nullptr;
  }
  return *this;
}

BoxedValue::BoxedValue(const WeakValue& other)
  : union_(other.union_) {
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY:
    case UnionValue::Type::BOOL:
    case UnionValue::Type::CHAR:
    case UnionValue::Type::INT:
    case UnionValue::Type::FLOAT:
      break;
    default:
      // Using the top 24 bits here allows blocking the deletion of the pointer
      // without risking other threads using a bad pointer. This assumes that
      // each object will have fewer than 2^40 references, and that fewer than
      // 2^24 threads will be attempting to lock a weak reference for a given
      // object at any one time.
      static constexpr unsigned long long strong_lock = 0x1ULL << 40;
      if (union_.type_.counters_->strong_.fetch_add(strong_lock) % strong_lock == 0) {
        // NOTE: Subtraction of strong_lock *cannot* be optimized out!
        //
        // Unsigned overflow would still leave strong_%strong_lock == 0, but
        // there could be a race-condition between three threads:
        //
        // Thread 1: Enters *this* constructor while the pointer is still valid.
        // Thread 2: Enters BoxedValue::Cleanup and removes the last reference.
        // Thread 3: Enters *this* constructor before Thread 1 subtracts
        //           strong_lock-1, meaning strong_%strong_lock == 0.
        // Thread 1: Subtracts strong_lock-1 (+1 overall) to revive the pointer.
        //
        // This still leaves all three threads in a valid state, but with Thread
        // 3 getting empty instead of a still-valid pointer. In other words,
        // strong_%strong_lock == 0 *doesn't* guarantee that the pointer is no
        // longer valid.
        union_.type_.counters_->strong_.fetch_sub(strong_lock);
        union_.type_.value_type_  = UnionValue::Type::EMPTY;
        union_.value_.as_pointer_ = nullptr;
      } else {
        union_.type_.counters_->strong_.fetch_sub(strong_lock-1);
      }
      break;
  }
}

BoxedValue::BoxedValue(bool value)
  : union_{ { .value_type_ = UnionValue::Type::BOOL },
            { .as_bool_ = value } } {}

BoxedValue::BoxedValue(PrimChar value)
  : union_{ { .value_type_ = UnionValue::Type::CHAR },
            { .as_char_= value } } {}

BoxedValue::BoxedValue(PrimInt value)
  : union_{ { .value_type_ = UnionValue::Type::INT },
            { .as_int_= value } } {}

BoxedValue::BoxedValue(PrimFloat value)
  : union_{ { .value_type_ = UnionValue::Type::FLOAT },
            { .as_float_ = value } } {}

BoxedValue::BoxedValue(TypeValue* value)
  : union_{ { .counters_ = new UnionValue::Counters{{1},{1}} },
            { .as_pointer_ = value } } {}

BoxedValue::~BoxedValue() {
  Cleanup();
}

bool BoxedValue::AsBool() const {
  if (union_.type_.value_type_ != UnionValue::Type::BOOL) {
    FAIL() << CategoryName() << " is not a Bool value";
  }
  return union_.value_.as_bool_;
}

PrimChar BoxedValue::AsChar() const {
  if (union_.type_.value_type_ != UnionValue::Type::CHAR) {
    FAIL() << CategoryName() << " is not a Char value";
  }
  return union_.value_.as_char_;
}

PrimInt BoxedValue::AsInt() const {
  if (union_.type_.value_type_ != UnionValue::Type::INT) {
    FAIL() << CategoryName() << " is not an Int value";
  }
  return union_.value_.as_int_;
}

PrimFloat BoxedValue::AsFloat() const {
  if (union_.type_.value_type_ != UnionValue::Type::FLOAT) {
    FAIL() << CategoryName() << " is not a Float value";
  }
  return union_.value_.as_float_;
}

const PrimString& BoxedValue::AsString() const {
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY:
    case UnionValue::Type::BOOL:
    case UnionValue::Type::CHAR:
    case UnionValue::Type::INT:
    case UnionValue::Type::FLOAT:
      FAIL() << CategoryName() << " is not a String value";
      __builtin_unreachable();
      break;
    default:
      if (!union_.value_.as_pointer_) {
        FAIL() << "Function called on null pointer";
      }
      return union_.value_.as_pointer_->AsString();
  }
}

PrimCharBuffer& BoxedValue::AsCharBuffer() const {
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY:
    case UnionValue::Type::BOOL:
    case UnionValue::Type::CHAR:
    case UnionValue::Type::INT:
    case UnionValue::Type::FLOAT:
      FAIL() << CategoryName() << " is not a CharBuffer value";
      __builtin_unreachable();
      break;
    default:
      if (!union_.value_.as_pointer_) {
        FAIL() << "Function called on null pointer";
      }
      return union_.value_.as_pointer_->AsCharBuffer();
  }
}

// static
bool BoxedValue::Present(const BoxedValue& target) {
  return target.union_.type_.value_type_ != UnionValue::Type::EMPTY;
}

// static
BoxedValue BoxedValue::Require(const BoxedValue& target) {
  if (target.union_.type_.value_type_ == UnionValue::Type::EMPTY) {
    FAIL() << "Cannot require empty value";
  }
  return target;
}

// static
BoxedValue BoxedValue::Strong(const WeakValue& target) {
  return BoxedValue(target);
}

std::string BoxedValue::CategoryName() const {
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY: return "empty";
    case UnionValue::Type::BOOL:  return "Bool";
    case UnionValue::Type::CHAR:  return "Char";
    case UnionValue::Type::INT:   return "Int";
    case UnionValue::Type::FLOAT: return "Float";
    default:
      if (!union_.value_.as_pointer_) {
        FAIL() << "Function called on null pointer";
      }
      return union_.value_.as_pointer_->CategoryName();
  }
}

ReturnTuple BoxedValue::Dispatch(
  const BoxedValue& self, const ValueFunction& label,
  const ParamTuple& params, const ValueTuple& args) const {
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY:
      FAIL() << "Function called on empty value";
      __builtin_unreachable();
      break;
    case UnionValue::Type::BOOL:
      return DispatchBool(union_.value_.as_bool_, self, label, params, args);
    case UnionValue::Type::CHAR:
      return DispatchChar(union_.value_.as_char_, self, label, params, args);
    case UnionValue::Type::INT:
      return DispatchInt(union_.value_.as_int_, self, label, params, args);
    case UnionValue::Type::FLOAT:
      return DispatchFloat(union_.value_.as_float_, self, label, params, args);
    default:
      if (!union_.value_.as_pointer_) {
        FAIL() << "Function called on null pointer";
      }
      return union_.value_.as_pointer_->Dispatch(self, label, params, args);
  }
}

void BoxedValue::Cleanup() {
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY:
    case UnionValue::Type::BOOL:
    case UnionValue::Type::CHAR:
    case UnionValue::Type::INT:
    case UnionValue::Type::FLOAT:
      break;
    default:
      if (--union_.type_.counters_->strong_ == 0) {
        delete union_.value_.as_pointer_;
        if (--union_.type_.counters_->weak_ == 0) {
          delete union_.type_.counters_;
        }
      }
      break;
  }
  union_.type_.value_type_  = UnionValue::Type::EMPTY;
  union_.value_.as_pointer_ = nullptr;
}


WeakValue::WeakValue()
  : union_{ { .value_type_ = UnionValue::Type::EMPTY },
            { .as_pointer_ = nullptr } } {}

WeakValue::WeakValue(const WeakValue& other)
  : union_(other.union_) {
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY:
    case UnionValue::Type::BOOL:
    case UnionValue::Type::CHAR:
    case UnionValue::Type::INT:
    case UnionValue::Type::FLOAT:
      break;
    default:
      ++union_.type_.counters_->weak_;
      break;
  }
}

WeakValue& WeakValue::operator = (const WeakValue& other) {
  if (&other != this) {
    Cleanup();
    union_ = other.union_;
    switch (union_.type_.value_type_) {
      case UnionValue::Type::EMPTY:
      case UnionValue::Type::BOOL:
      case UnionValue::Type::CHAR:
      case UnionValue::Type::INT:
      case UnionValue::Type::FLOAT:
        break;
      default:
        ++union_.type_.counters_->weak_;
        break;
    }
  }
  return *this;
}

WeakValue::WeakValue(WeakValue&& other)
  : union_(other.union_) {
  other.union_.type_.value_type_  = UnionValue::Type::EMPTY;
  other.union_.value_.as_pointer_ = nullptr;
}

WeakValue& WeakValue::operator = (WeakValue&& other) {
  if (&other != this) {
    Cleanup();
    union_ = other.union_;
    other.union_.type_.value_type_  = UnionValue::Type::EMPTY;
    other.union_.value_.as_pointer_ = nullptr;
  }
  return *this;
}

WeakValue::WeakValue(const BoxedValue& other)
  : union_(other.union_) {
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY:
    case UnionValue::Type::BOOL:
    case UnionValue::Type::CHAR:
    case UnionValue::Type::INT:
    case UnionValue::Type::FLOAT:
      break;
    default:
      ++union_.type_.counters_->weak_;
      break;
  }
}

WeakValue& WeakValue::operator = (const BoxedValue& other) {
  Cleanup();
  union_ = other.union_;
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY:
    case UnionValue::Type::BOOL:
    case UnionValue::Type::CHAR:
    case UnionValue::Type::INT:
    case UnionValue::Type::FLOAT:
      break;
    default:
      ++union_.type_.counters_->weak_;
      break;
  }
  return *this;
}

WeakValue::~WeakValue() {
  Cleanup();
}

void WeakValue::Cleanup() {
  switch (union_.type_.value_type_) {
    case UnionValue::Type::EMPTY:
    case UnionValue::Type::BOOL:
    case UnionValue::Type::CHAR:
    case UnionValue::Type::INT:
    case UnionValue::Type::FLOAT:
      break;
    default:
      if (--union_.type_.counters_->weak_ == 0) {
        delete union_.type_.counters_;
      }
      break;
  }
  union_.type_.value_type_  = UnionValue::Type::EMPTY;
  union_.value_.as_pointer_ = nullptr;
}
