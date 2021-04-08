#ifndef BOXED_HPP_
#define BOXED_HPP_

#include <atomic>

#include "function.hpp"
#include "types.hpp"


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

  explicit BoxedValue(const WeakValue&);

  // NOTE: Using enum class would break the switch/case logic.
  // NOTE: The enum values assume that dynamic allocation will never be aligned
  // to an odd address; especially within a few bytes of ULONG_MAX.
  enum TypeEnum : unsigned long {
    EMPTY = ~0x00ULL,
    BOOL  = ~0x02ULL,
    CHAR  = ~0x04ULL,
    INT   = ~0x06ULL,
    FLOAT = ~0x08ULL,
  };

  std::string CategoryName() const;

  ReturnTuple Dispatch(const BoxedValue& self, const ValueFunction& label,
                       const ParamTuple& params, const ValueTuple& args) const;

  void Cleanup();

  union {
    std::atomic_int* counter_;
    TypeEnum         value_type_;
  } type_;

  union {
    TypeValue* as_pointer_;
    bool       as_bool_;
    PrimChar   as_char_;
    PrimInt    as_int_;
    PrimFloat  as_float_;
    bool       as_empty_;
  } value_;
};


// TODO: This isn't actually a weak pointer.
class WeakValue {
 public:
  WeakValue() {}
  explicit WeakValue(BoxedValue value);
  ~WeakValue();

 private:
  friend class BoxedValue;

  BoxedValue value_;
};

#endif  // BOXED_HPP_
