#include "string.h"

namespace {

class Constructor_String
    : public ParamInstance<0>::Type,
      public TypeInstance {
 public:
  TypeInstance& Build() final { return *this; }
  const std::string& CategoryName() const final { return name_; }
  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_String(); }

 private:
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_{"String"};
  const TypeArgs types_{this};
};

Constructor_String& Internal_String() {
  static Constructor_String*const constructor = new Constructor_String;
  return *constructor;
}


class Value_String : public TypeValue {
 public:
  Value_String(const std::string& value) : value_(value) {}
  std::string GetString() const final { return value_; }

 private:
  const TypeInstance& InstanceType() const final { return Internal_String(); }

  const std::string value_;
};

}  // namespace


ParamInstance<0>::Type& Category_String() { return Internal_String(); }

S<TypeValue> As_String(const std::string& value) {
  return S_get(new Value_String(value));
}
