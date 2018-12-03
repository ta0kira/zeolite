#include "optional.h"

#include "dispatch.h"
#include "union.h"

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
        name_(ConstructOptionalName(x)) {
    parents_.AddParent(Category_Optional(),x_);
  }

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Optional(); }
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;
  S<TypeValue> Create(const S<TypeValue>& value);
  S<TypeValue> Skip();

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  TypeInstance& x_;
  const std::string name_;
  ParentTypes parents_;
  const TypeArgs types_{this};
};


class Value_Optional : public TypeValue {
 public:
  Value_Optional(TypeInstance& parent,
                 const S<TypeValue>& value)
      : parent_(parent), value_(value) {}

  S<TypeValue> GetNestedValue() final { return value_; }
  bool IsPresent() const final { return !!value_; }

 private:
  const TypeInstance& InstanceType() const final { return parent_; }
  S<TypeValue> ConvertTo(TypeInstance&) final;

  TypeInstance& parent_;
  const S<TypeValue> value_;
};


TypeInstance& Constructor_Optional::Build(TypeInstance& x) {
  return BuildInternal(x);
}

Instance_Optional& Constructor_Optional::BuildInternal(TypeInstance& x) {
  TypeInstance* unwrapped = &x;
  // The compiler should take care of preventing optional optional x, but this
  // is mostly for redundant checking.
  while (&unwrapped->CategoryType() == &Category_Optional()) {
    unwrapped = SafeGet<0>(unwrapped->TypeArgsForCategory(Category_Optional()));
  }
  R<Instance_Optional>& instance = instance_cache_.Create(*unwrapped);
  if (!instance) {
    instance = R_get(new Instance_Optional(*unwrapped));
  }
  return *instance;
}

const TypeArgs& Instance_Optional::TypeArgsForCategory(const TypeCategory& category) const {
  if (parents_.HasParent(category)) {
    return parents_.GetParent(category);
  }
  return TypeInstance::TypeArgsForCategory(category);
}

S<TypeValue> Instance_Optional::Create(const S<TypeValue>& value) {
  S<TypeValue> converted = TypeValue::ConvertTo(value,x_);
  return S_get(new Value_Optional(*this,converted));
}

S<TypeValue> Instance_Optional::Skip() {
  return S_get(new Value_Optional(*this,nullptr));
}

bool Instance_Optional::CheckConversionFrom(const TypeInstance& instance) const {
  if (CheckConversionBetween(instance,x_)) {
    return true;
  }
  if (!instance.IsParentCategory(Category_Optional())) {
    return false;
  }
  const TypeArgs& args = instance.TypeArgsForCategory(Category_Optional());
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return CheckConversionBetween(*SafeGet<0>(args),x_);  // covariant
}


S<TypeValue> Value_Optional::ConvertTo(TypeInstance& instance) {
  if (&instance.CategoryType() == &Category_Optional()) {
    const TypeArgs& args = instance.TypeArgsForCategory(Category_Optional());
    FAIL_IF(args.size() != 1) << "Wrong number of type args";
    if (value_) {
      return As_Optional(value_,*SafeGet<0>(args));
    } else {
      return Internal_Optional().BuildInternal(*SafeGet<0>(args)).Skip();
    }
  }
  return TypeValue::ConvertTo(instance);
}

const S<TypeValue>& OPTIONAL_ALL =
    *new S<TypeValue>(Internal_Optional().BuildInternal(Union_All()).Skip());

}  // namespace


ParamInstance<1>::Type& Category_Optional() {
  return Internal_Optional();
}

S<TypeValue> Optional_Skip() {
  return OPTIONAL_ALL;
}

S<TypeValue> As_Optional(const S<TypeValue>& value, TypeInstance& type) {
  // NOTE: Assumes that value is not nullptr. The caller needs to explicitly use
  // Optional_Skip if appropriate.
  if (&value->CategoryType() == &Category_Optional()) {
    if (value->GetNestedValue()) {
      return Internal_Optional().BuildInternal(type).Create(value->GetNestedValue());
    } else {
    return Internal_Optional().BuildInternal(type).Skip();
    }
  } else {
    return Internal_Optional().BuildInternal(type).Create(value);
  }
}
