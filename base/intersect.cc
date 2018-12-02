#include "intersect.h"

#include "bool.h"
#include "dispatch.h"

namespace {

class Constructor_Intersect;
class Instance_Intersect;
class Value_Intersect;


class Constructor_Intersect : public TypeCategory {
 public:
  const std::string& CategoryName() const final { return name_; }
  Instance_Intersect& BuildInternal(const TypeArgs& types);

 private:
  const std::string name_{"Intersect"};
  InstanceCache<Instance_Intersect> instance_cache_;
};

Constructor_Intersect& Internal_Intersect() {
  static Constructor_Intersect*const constructor = new Constructor_Intersect;
  return *constructor;
}


std::string ConstructIntersectName(const TypeArgs& types) {
  if (types.empty()) {
    return "any";
  }
  std::ostringstream formatted;
  formatted << "(";
  for (int i = 0; i < types.size(); ++i) {
    if (i > 0) {
      formatted << "&";
    }
    formatted << types[i]->InstanceName();
  }
  formatted << ")";
  return formatted.str();
}


class Instance_Intersect : public TypeInstance {
 public:
  Instance_Intersect(const TypeArgs& types)
      : name_(ConstructIntersectName(types)),
        types_(types) {}

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Intersect(); }
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;
  S<TypeValue> Create(const S<TypeValue>& value);

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::UNION; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_;
};


class Value_Intersect : public TypeValue {
 public:
  Value_Intersect(TypeInstance& parent,
              const S<TypeValue>& value)
      : parent_(parent), value_(value) {}

  const TypeInstance& InstanceType() const final { return parent_; }
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const TypeArgs&,
      const FunctionArgs& args) final;
  ValueVariable* GetValueVariable(const ValueVariableId<MemberScope::VALUE>&) final;
  bool GetBool() const final;

 private:
  S<TypeValue> GetNestedValue() final { return value_; }
  S<TypeValue> ConvertTo(TypeInstance&) final;

  TypeInstance& parent_;
  const S<TypeValue> value_;
};


Instance_Intersect& Constructor_Intersect::BuildInternal(const TypeArgs& types) {
  R<Instance_Intersect>& instance = instance_cache_.Create(types);
  if (!instance) {
    instance = R_get(new Instance_Intersect(types));
  }
  return *instance;
}

const TypeArgs& Instance_Intersect::TypeArgsForCategory(const TypeCategory& category) const {
  if (&category == &Category_Intersect()) {
    return types_;
  }
  // NOTE: CheckConversionBetween is designed to allow this to skip checking
  // types_ so that we don't need to iterate.
  return TypeInstance::TypeArgsForCategory(category);
}

bool Instance_Intersect::CheckConversionFrom(const TypeInstance& instance) const {
  for (const TypeInstance* type : types_) {
    if (!TypeInstance::CheckConversionBetween(instance,*type)) {
      return false;
    }
  }
  return true;
}


FunctionReturns Value_Intersect::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return value_->CallValueFunction(id,types,args);
}

ValueVariable* Value_Intersect::GetValueVariable(
    const ValueVariableId<MemberScope::VALUE>& id) {
  return value_->GetValueVariable(id);
}

bool Value_Intersect::GetBool() const {
  return value_->GetBool();
}

S<TypeValue> Value_Intersect::ConvertTo(TypeInstance& instance) {
  return TypeValue::ConvertTo(value_,instance);
}

}  // namespace


TypeCategory& Category_Intersect() {
  return Internal_Intersect();
}

TypeInstance& Build_Intersect(const TypeArgs& types) {
  return Internal_Intersect().BuildInternal(types);
}

S<TypeValue> As_Intersect(const S<TypeValue>& value, const TypeArgs& types) {
  if (types.size() == 1) {
    return TypeValue::ConvertTo(value,*SafeGet<0>(types));
  }
  TypeInstance& intersect_type = Build_Intersect(types);
  FAIL_IF(!TypeInstance::CheckConversionBetween(value->InstanceType(),intersect_type))
      << "Cannot assign type-value " << value->InstanceType().InstanceName()
      << " to " << intersect_type.InstanceName();
  // TODO: This should probably use value->GetNestedValue() if it has one.
  return S_get(new Value_Intersect(intersect_type,value));
}
