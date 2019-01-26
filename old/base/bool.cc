#include "bool.h"

#include <string>

namespace {

class Constructor_Bool
    : public ParamInstance<0>::Type,
      public TypeInstance {
 public:
  TypeInstance& Build() final { return *this; }
  const std::string& CategoryName() const final { return name_; }
  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Bool(); }

 private:
  ~Constructor_Bool() = default;

  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_{"Bool"};
  const TypeArgs types_{this};
};

Constructor_Bool& Internal_Bool() {
  static Constructor_Bool*const constructor = new Constructor_Bool;
  return *constructor;
}


class Value_Bool : public TypeValue {
 public:
  Value_Bool(bool value) : value_(value) {}
  bool GetBool() const final { return value_; }

 private:
  const TypeInstance& InstanceType() const final { return Internal_Bool(); }

  const bool value_;
};

}  // namespace


ParamInstance<0>::Type& Category_Bool() { return Internal_Bool(); }

S<TypeValue> As_Bool(bool value) {
  static const S<TypeValue>& BOOL_TRUE = *new S<TypeValue>(S_get(new Value_Bool(true)));
  static const S<TypeValue>& BOOL_FALSE = *new S<TypeValue>(S_get(new Value_Bool(false)));
  return value? BOOL_TRUE : BOOL_FALSE;
}
