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
  const TypeCategory& CategoryType() const final { return Category_Bool; }

 private:
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_{"Bool"};
  const TypeArgs types_{this};
};

Constructor_Bool& Internal_Bool = *new Constructor_Bool;


class Value_Bool : public TypeValue {
 public:
  Value_Bool(bool value) : value_(value) {}
  const TypeInstance& InstanceType() const final { return Internal_Bool; }
  bool GetBool() const final { return value_; }

 private:
  const bool value_;
};


const S<TypeValue>& TRUE = *new S<TypeValue>(S_get(new Value_Bool(true)));
const S<TypeValue>& FALSE = *new S<TypeValue>(S_get(new Value_Bool(false)));

}  // namespace

ParamInstance<0>::Type& Category_Bool = Internal_Bool;

S<TypeValue> AsBool(bool value) {
  return value? TRUE : FALSE;
}
