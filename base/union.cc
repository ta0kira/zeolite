#include "union.h"

#include "bool.h"
#include "dispatch.h"
#include "intersect.h"

namespace {

class Constructor_Union;
class Instance_Union;
class Value_Union;


class Constructor_Union : public TypeCategory {
 public:
  const std::string& CategoryName() const final { return name_; }
  Instance_Union& BuildInternal(const TypeArgs& types);

 private:
  const std::string name_{"Union"};
  InstanceCache<Instance_Union> instance_cache_;
};

Constructor_Union& Internal_Union() {
  static Constructor_Union*const constructor = new Constructor_Union;
  return *constructor;
}


std::string ConstructUnionName(const TypeArgs& types) {
  if (types.empty()) {
    return "all";
  }
  std::ostringstream formatted;
  formatted << "(";
  for (int i = 0; i < types.size(); ++i) {
    if (i > 0) {
      formatted << "|";
    }
    formatted << types[i]->InstanceName();
  }
  formatted << ")";
  return formatted.str();
}


class Instance_Union : public TypeInstance {
 public:
  Instance_Union(const TypeArgs& types)
      : name_(ConstructUnionName(types)),
        types_(types) {}

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Union(); }
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;
  S<TypeValue> Create(const S<TypeValue>& value);

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::UNION; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_;
};


class Value_Union : public TypeValue {
 public:
  Value_Union(TypeInstance& parent,
              const S<TypeValue>& value)
      : parent_(parent), value_(value) {}

  const TypeInstance& InstanceType() const final { return parent_; }
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const TypeArgs&,
      const FunctionArgs& args) final;
  ValueVariable* GetValueVariable(const ValueVariableId<MemberScope::VALUE>&) final;
  S<TypeValue> GetNestedValue() final { return value_; }
  bool GetBool() const final;
  std::string GetString() const final;

 private:
  S<TypeValue> ConvertTo(TypeInstance&) final;

  TypeInstance& parent_;
  const S<TypeValue> value_;
};


Instance_Union& Constructor_Union::BuildInternal(const TypeArgs& types) {
  R<Instance_Union>& instance = instance_cache_.Create(types);
  if (!instance) {
    instance = R_get(new Instance_Union(types));
  }
  return *instance;
}

const TypeArgs& Instance_Union::TypeArgsForCategory(const TypeCategory& category) const {
  if (&category == &Category_Union()) {
    return types_;
  }
  // NOTE: CheckConversionBetween is designed to allow this to skip checking
  // types_ so that we don't need to iterate.
  return TypeInstance::TypeArgsForCategory(category);
}

bool Instance_Union::CheckConversionFrom(const TypeInstance& instance) const {
  for (const TypeInstance* type : types_) {
    if (TypeInstance::CheckConversionBetween(instance,*type)) {
      return true;
    }
  }
  return false;
}


FunctionReturns Value_Union::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return value_->CallValueFunction(id,types,args);
}

ValueVariable* Value_Union::GetValueVariable(
    const ValueVariableId<MemberScope::VALUE>& id) {
  return value_->GetValueVariable(id);
}

bool Value_Union::GetBool() const {
  return value_->GetBool();
}

std::string Value_Union::GetString() const {
  return value_->GetString();
}

S<TypeValue> Value_Union::ConvertTo(TypeInstance& instance) {
  return TypeValue::ConvertTo(value_,instance);
}

}  // namespace


TypeCategory& Category_Union() {
  return Internal_Union();
}

TypeInstance& Build_Union(const TypeArgs& types) {
  return Internal_Union().BuildInternal(types);
}

S<TypeValue> As_Union(const S<TypeValue>& value, const TypeArgs& types) {
  if (types.size() == 1) {
    return TypeValue::ConvertTo(value,*SafeGet<0>(types));
  }

  TypeInstance& union_type = Build_Union(types);
  FAIL_IF(!TypeInstance::CheckConversionBetween(value->InstanceType(),union_type))
      << "Cannot assign type-value " << value->InstanceType().InstanceName()
      << " to " << union_type.InstanceName();

#ifdef OPT_TYPE_CHECKING
  return value;
#else
  if (&value->InstanceType().CategoryType() == &Category_Intersect() ||
      &value->InstanceType().CategoryType() == &Category_Union()) {
    return S_get(new Value_Union(union_type,value->GetNestedValue()));
  } else {
    return S_get(new Value_Union(union_type,value));
  }
#endif
}
