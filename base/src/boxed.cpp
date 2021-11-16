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

ReturnTuple DispatchBool(PrimBool value, const ValueFunction& label,
                         const ParamsArgs& params_args);

ReturnTuple DispatchChar(PrimChar value, const ValueFunction& label,
                         const ParamsArgs& params_args);

ReturnTuple DispatchInt(PrimInt value, const ValueFunction& label,
                        const ParamsArgs& params_args);

ReturnTuple DispatchFloat(PrimFloat value, const ValueFunction& label,
                          const ParamsArgs& params_args);

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE


namespace zeolite_internal {

namespace {

void Validate(const std::string& name, const UnionValue& the_union) {
  if (the_union.type_ == UnionValue::Type::BOXED) {
    const auto strong = the_union.value_.as_pointer_->strong_.load();
    const auto weak   = the_union.value_.as_pointer_->weak_.load();
    const TypeValue* const object = the_union.value_.as_pointer_->object_;
    const std::string type = object ? the_union.CategoryName() : "deleted value";

    if (strong == 0 && object) {
      FAIL() << "Leaked " << type << " " << name << " at "
             << ((void*) the_union.value_.as_pointer_) << " (S: "
             << std::hex << strong << " W: " << weak << ")";
    }

    if (strong != 0 && !object) {
      FAIL() << "Prematurely deleted value " << name << " at "
             << ((void*) the_union.value_.as_pointer_) << " (S: "
             << std::hex << strong << " W: " << weak << ")";
    }

    if (strong < 0 || weak < 0 || (weak == 0 && strong > 0)) {
      FAIL() << "Invalid counts for " << type << " " << name << " at "
             << ((void*) the_union.value_.as_pointer_) << " (S: "
             << std::hex << strong << " W: " << weak << ")";
    }
  }
}

}  // namespace

std::string UnionValue::CategoryName() const {
  switch (type_) {
    case UnionValue::Type::EMPTY: return "empty";
    case UnionValue::Type::BOOL:  return "Bool";
    case UnionValue::Type::CHAR:  return "Char";
    case UnionValue::Type::INT:   return "Int";
    case UnionValue::Type::FLOAT: return "Float";
    case UnionValue::Type::BOXED:
      if (!value_.as_pointer_ || !value_.as_pointer_->object_) {
        FAIL() << "Function called on null pointer";
      }
      return value_.as_pointer_->object_->CategoryName();
  }
}

BoxedValue::BoxedValue(const WeakValue& other)
  : union_(other.union_) {
  switch (union_.type_) {
    case UnionValue::Type::BOXED:
      while (union_.value_.as_pointer_->lock_.test_and_set(std::memory_order_acquire));
      if (++union_.value_.as_pointer_->strong_ == 1) {
        --union_.value_.as_pointer_->strong_;
        union_.value_.as_pointer_->lock_.clear(std::memory_order_release);
        union_.type_ = UnionValue::Type::EMPTY;
        union_.value_.as_pointer_ = nullptr;
      } else {
        union_.value_.as_pointer_->lock_.clear(std::memory_order_release);
      }
      break;
    default:
      break;
  }
}

void BoxedValue::Validate(const std::string& name) const {
  zeolite_internal::Validate(name, union_);
}

const PrimString& BoxedValue::AsString() const {
  switch (union_.type_) {
    case UnionValue::Type::BOXED:
      if (!union_.value_.as_pointer_ || !union_.value_.as_pointer_->object_) {
        FAIL() << "Function called on null pointer";
      }
      return union_.value_.as_pointer_->object_->AsString();
    default:
      FAIL() << union_.CategoryName() << " is not a String value";
      __builtin_unreachable();
      break;
  }
}

PrimCharBuffer& BoxedValue::AsCharBuffer() const {
  switch (union_.type_) {
    case UnionValue::Type::BOXED:
      if (!union_.value_.as_pointer_ || !union_.value_.as_pointer_->object_) {
        FAIL() << "Function called on null pointer";
      }
      return union_.value_.as_pointer_->object_->AsCharBuffer();
    default:
      FAIL() << union_.CategoryName() << " is not a CharBuffer value";
      __builtin_unreachable();
      break;
  }
}

ReturnTuple BoxedValue::Dispatch(
  const ValueFunction& label, const ParamsArgs& params_args) const {
  switch (union_.type_) {
    case UnionValue::Type::EMPTY:
      FAIL() << "Function called on empty value";
      __builtin_unreachable();
      break;
    case UnionValue::Type::BOOL:
      return DispatchBool(union_.value_.as_bool_, label, params_args);
    case UnionValue::Type::CHAR:
      return DispatchChar(union_.value_.as_char_, label, params_args);
    case UnionValue::Type::INT:
      return DispatchInt(union_.value_.as_int_, label, params_args);
    case UnionValue::Type::FLOAT:
      return DispatchFloat(union_.value_.as_float_, label, params_args);
    case UnionValue::Type::BOXED:
      if (!union_.value_.as_pointer_ || !union_.value_.as_pointer_->object_) {
        FAIL() << "Function called on null pointer";
      }
      return union_.value_.as_pointer_->object_->Dispatch(label, params_args);
  }
}

void BoxedValue::Cleanup() {
  switch (union_.type_) {
    case UnionValue::Type::BOXED:
      while (union_.type_ == UnionValue::Type::BOXED) {
        while (union_.value_.as_pointer_->lock_.test_and_set(std::memory_order_acquire));
        if (--union_.value_.as_pointer_->strong_ == 0) {
          TypeValue* const object = union_.value_.as_pointer_->object_;
          union_.value_.as_pointer_->object_ = nullptr;
          union_.value_.as_pointer_->lock_.clear(std::memory_order_release);
          BoxedValue next = object->FlatCleanup();
          object->~TypeValue();
          if (--union_.value_.as_pointer_->weak_ == 0) {
            // NOTE: as_bytes_ contains object => ~TypeValue() must happen first.
            free(union_.value_.as_bytes_);
          }
          union_ = next.union_;
          next.union_.type_ = UnionValue::Type::EMPTY;
          next.union_.value_.as_pointer_ = nullptr;
        } else {
          union_.value_.as_pointer_->lock_.clear(std::memory_order_release);
          break;
        }
      }
      break;
    default:
      break;
  }
  union_.type_ = UnionValue::Type::EMPTY;
  union_.value_.as_pointer_ = nullptr;
}


WeakValue::WeakValue()
  : union_{ .type_ = UnionValue::Type::EMPTY, .value_ = { .as_pointer_ = nullptr } } {}

WeakValue::WeakValue(const WeakValue& other)
  : union_(other.union_) {
  switch (union_.type_) {
    case UnionValue::Type::BOXED:
      ++union_.value_.as_pointer_->weak_;
      break;
    default:
      break;
  }
}

WeakValue& WeakValue::operator = (const WeakValue& other) {
  if (&other != this) {
    Cleanup();
    union_ = other.union_;
    switch (union_.type_) {
      case UnionValue::Type::BOXED:
        ++union_.value_.as_pointer_->weak_;
        break;
      default:
        break;
    }
  }
  return *this;
}

WeakValue::WeakValue(WeakValue&& other)
  : union_(other.union_) {
  other.union_.type_  = UnionValue::Type::EMPTY;
  other.union_.value_.as_pointer_ = nullptr;
}

WeakValue& WeakValue::operator = (WeakValue&& other) {
  if (&other != this) {
    Cleanup();
    union_ = other.union_;
    other.union_.type_  = UnionValue::Type::EMPTY;
    other.union_.value_.as_pointer_ = nullptr;
  }
  return *this;
}

WeakValue::WeakValue(const BoxedValue& other)
  : union_(other.union_) {
  switch (union_.type_) {
    case UnionValue::Type::BOXED:
      ++union_.value_.as_pointer_->weak_;
      break;
    default:
      break;
  }
}

WeakValue& WeakValue::operator = (const BoxedValue& other) {
  Cleanup();
  union_ = other.union_;
  switch (union_.type_) {
    case UnionValue::Type::BOXED:
      ++union_.value_.as_pointer_->weak_;
      break;
    default:
      break;
  }
  return *this;
}

WeakValue::~WeakValue() {
  Cleanup();
}

void WeakValue::Validate(const std::string& name) const {
  zeolite_internal::Validate(name, union_);
}

void WeakValue::Cleanup() {
  switch (union_.type_) {
    case UnionValue::Type::BOXED:
      if (--union_.value_.as_pointer_->weak_ == 0) {
        free(union_.value_.as_bytes_);
      }
      break;
    default:
      break;
  }
  union_.type_  = UnionValue::Type::EMPTY;
  union_.value_.as_pointer_ = nullptr;
}

}  // namespace zeolite_internal
