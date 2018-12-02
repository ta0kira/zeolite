#include "optional.h"

#include "dispatch.h"

namespace {

class Constructor_Optional;
class Instance_Optional;
class Value_Optional;


class Constructor_Optional : public ParamInstance<1>::Type {
 public:
  TypeInstance& Build(TypeInstance& x) final;
  const std::string& CategoryName() const final { return name_; }
  Instance_Optional& BuildInternal(TypeInstance& x);

 private:
  const std::string name_{"Optional"};
  InstanceCache<Instance_Optional> instance_cache_;
};

Constructor_Optional& Internal_Optional() {
  static Constructor_Optional*const constructor = new Constructor_Optional;
  return *constructor;
}


std::string ConstructOptionalName(const TypeInstance& x) {
  std::ostringstream formatted;
  formatted << "optional " << x.InstanceName();
  return formatted.str();
}


class Instance_Optional : public TypeInstance {
 public:
  Instance_Optional(TypeInstance& x)
      : x_(x),
        name_(ConstructOptionalName(x)) {}

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Optional(); }
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;
  bool IsOptional() const final { return true; }
  S<TypeValue> Create(const S<TypeValue>& value);
  S<TypeValue> Skip();

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  TypeInstance& x_;
  const std::string name_;
  const TypeArgs types_{this};
  const TypeArgs args_self_{&x_};
};


class Value_Optional : public TypeValue {
 public:
  Value_Optional(TypeInstance& parent,
                 const S<TypeValue>& value)
      : parent_(parent), value_(value) {}

  const TypeInstance& InstanceType() const final { return parent_; }
  bool IsPresent() const final { return !!value_; }

 private:
  S<TypeValue> GetNestedValue() final { return value_; }
  S<TypeValue> ConvertTo(TypeInstance&) final;

  TypeInstance& parent_;
  const S<TypeValue> value_;
};


TypeInstance& Constructor_Optional::Build(TypeInstance& x) {
  return BuildInternal(x);
}

Instance_Optional& Constructor_Optional::BuildInternal(TypeInstance& x) {
  R<Instance_Optional>& instance = instance_cache_.Create(x);
  if (!instance) {
    instance = R_get(new Instance_Optional(x));
  }
  return *instance;
}

const TypeArgs& Instance_Optional::TypeArgsForCategory(const TypeCategory& category) const {
  if (&category == &Category_Optional()) {
    return args_self_;
  }
  // Can implicitly convert from y to optional x if y -> x.
  return x_.TypeArgsForCategory(category);
}

S<TypeValue> Instance_Optional::Create(const S<TypeValue>& value) {
  S<TypeValue> converted = TypeValue::ConvertTo(value,x_);
  return S_get(new Value_Optional(*this,converted));
}

S<TypeValue> Instance_Optional::Skip() {
  return S_get(new Value_Optional(*this,nullptr));
}

bool Instance_Optional::CheckConversionFrom(const TypeInstance& instance) const {
  const TypeArgs& args = instance.TypeArgsForCategory(Category_Optional());
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return CheckConversionBetween(*SafeGet<0>(args),x_);  // covariant
}


S<TypeValue> Value_Optional::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Optional()) {
    const TypeArgs& args = instance.TypeArgsForCategory(Category_Optional());
    FAIL_IF(args.size() != 1) << "Wrong number of type args";
    return As_Optional(value_,*SafeGet<0>(args));
  }
  return TypeValue::ConvertTo(instance);
}

}  // namespace


ParamInstance<1>::Type& Category_Optional() {
  return Internal_Optional();
}

S<TypeValue> As_Optional(const S<TypeValue>& value, TypeInstance& type) {
  if (value->InstanceType().IsOptional()) {
    return value;
  } else {
    return Internal_Optional().BuildInternal(type).Create(value);
  }
}

S<TypeValue> Skip_Optional(TypeInstance& type) {
  return Internal_Optional().BuildInternal(type).Skip();
}
