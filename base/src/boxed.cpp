#include "boxed.hpp"

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


BoxedValue::BoxedValue()
  : type_{ .value_type_ = TypeEnum::EMPTY },
    value_{ .as_pointer_ = nullptr } {}

BoxedValue::BoxedValue(const BoxedValue& other)
  : type_{ other.type_ },
    value_{ other.value_ } {
  switch (type_.value_type_) {
    case TypeEnum::EMPTY:
    case TypeEnum::BOOL:
    case TypeEnum::CHAR:
    case TypeEnum::INT:
    case TypeEnum::FLOAT:
      break;
    default:
      ++*type_.counter_;
      break;
  }
}

BoxedValue& BoxedValue::operator = (const BoxedValue& other) {
  if (&other != this) {
    Cleanup();
    type_  = other.type_;
    value_ = other.value_;
    switch (type_.value_type_) {
      case TypeEnum::EMPTY:
      case TypeEnum::BOOL:
      case TypeEnum::CHAR:
      case TypeEnum::INT:
      case TypeEnum::FLOAT:
        break;
      default:
        ++*type_.counter_;
        break;
    }
  }
  return *this;
}


BoxedValue::BoxedValue(BoxedValue&& other)
  : type_{ other.type_ },
    value_{ other.value_ } {
  other.type_.value_type_  = TypeEnum::EMPTY;
  other.value_.as_pointer_ = nullptr;
}

BoxedValue& BoxedValue::operator = (BoxedValue&& other) {
  if (&other != this) {
    Cleanup();
    type_  = other.type_;
    value_ = other.value_;
    other.type_.value_type_  = TypeEnum::EMPTY;
    other.value_.as_pointer_ = nullptr;
  }
  return *this;
}

BoxedValue::BoxedValue(const WeakValue& weak) : BoxedValue(weak.value_) {}

BoxedValue::BoxedValue(bool value)
  : type_{ .value_type_ = TypeEnum::BOOL },
    value_{ .as_bool_ = value } {}

BoxedValue::BoxedValue(PrimChar value)
  : type_{ .value_type_ = TypeEnum::CHAR },
    value_{ .as_char_= value } {}

BoxedValue::BoxedValue(PrimInt value)
  : type_{ .value_type_ = TypeEnum::INT },
    value_{ .as_int_= value } {}

BoxedValue::BoxedValue(PrimFloat value)
  : type_{ .value_type_ = TypeEnum::FLOAT },
    value_{ .as_float_ = value } {}

BoxedValue::BoxedValue(TypeValue* value)
  : type_{ .counter_ = new std::atomic_int{0} },
    value_{ .as_pointer_ = value } {
  ++*type_.counter_;
}

BoxedValue::~BoxedValue() {
  Cleanup();
}

bool BoxedValue::AsBool() const {
  if (type_.value_type_ != TypeEnum::BOOL) {
    FAIL() << CategoryName() << " is not a Bool value";
  }
  return value_.as_bool_;
}

PrimChar BoxedValue::AsChar() const {
  if (type_.value_type_ != TypeEnum::CHAR) {
    FAIL() << CategoryName() << " is not a Char value";
  }
  return value_.as_char_;
}

PrimInt BoxedValue::AsInt() const {
  if (type_.value_type_ != TypeEnum::INT) {
    FAIL() << CategoryName() << " is not an Int value";
  }
  return value_.as_int_;
}

PrimFloat BoxedValue::AsFloat() const {
  if (type_.value_type_ != TypeEnum::FLOAT) {
    FAIL() << CategoryName() << " is not a Float value";
  }
  return value_.as_float_;
}

const PrimString& BoxedValue::AsString() const {
  switch (type_.value_type_) {
    case TypeEnum::EMPTY:
    case TypeEnum::BOOL:
    case TypeEnum::CHAR:
    case TypeEnum::INT:
    case TypeEnum::FLOAT:
      FAIL() << CategoryName() << " is not a String value";
      __builtin_unreachable();
    default: return value_.as_pointer_->AsString();
  }
}

PrimCharBuffer& BoxedValue::AsCharBuffer() const {
  switch (type_.value_type_) {
    case TypeEnum::EMPTY:
    case TypeEnum::BOOL:
    case TypeEnum::CHAR:
    case TypeEnum::INT:
    case TypeEnum::FLOAT:
      FAIL() << CategoryName() << " is not a CharBuffer value";
      __builtin_unreachable();
    default: return value_.as_pointer_->AsCharBuffer();
  }
}

// static
bool BoxedValue::Present(const BoxedValue& target) {
  return target.type_.value_type_ != TypeEnum::EMPTY;
}

// static
BoxedValue BoxedValue::Require(const BoxedValue& target) {
  if (target.type_.value_type_ == TypeEnum::EMPTY) {
    FAIL() << "Cannot require empty value";
  }
  return target;
}

// static
BoxedValue BoxedValue::Strong(const WeakValue& target) {
  return BoxedValue(target);
}

std::string BoxedValue::CategoryName() const {
  switch (type_.value_type_) {
    case TypeEnum::EMPTY: return "empty";
    case TypeEnum::BOOL:  return "Bool";
    case TypeEnum::CHAR:  return "Char";
    case TypeEnum::INT:   return "Int";
    case TypeEnum::FLOAT: return "Float";
    default: return value_.as_pointer_->CategoryName();
  }
}

ReturnTuple BoxedValue::Dispatch(
  const BoxedValue& self, const ValueFunction& label,
  const ParamTuple& params, const ValueTuple& args) const {
  switch (type_.value_type_) {
    case TypeEnum::EMPTY:
      FAIL() << "Function called on empty value";
      __builtin_unreachable();
    case TypeEnum::BOOL:
      return DispatchBool(value_.as_bool_, self, label, params, args);
    case TypeEnum::CHAR:
      return DispatchChar(value_.as_char_, self, label, params, args);
    case TypeEnum::INT:
      return DispatchInt(value_.as_int_, self, label, params, args);
    case TypeEnum::FLOAT:
      return DispatchFloat(value_.as_float_, self, label, params, args);
    default:
      return value_.as_pointer_->Dispatch(self, label, params, args);
  }
}

void BoxedValue::Cleanup() {
  switch (type_.value_type_) {
    case TypeEnum::EMPTY:
    case TypeEnum::BOOL:
    case TypeEnum::CHAR:
    case TypeEnum::INT:
    case TypeEnum::FLOAT:
      break;
    default:
      if (--*type_.counter_ == 0) {
        delete type_.counter_;
        delete value_.as_pointer_;
      }
      break;
  }
  type_.value_type_  = TypeEnum::EMPTY;
  value_.as_pointer_ = nullptr;
}


WeakValue::WeakValue(BoxedValue value) : value_(value) {}

WeakValue::~WeakValue() {}
