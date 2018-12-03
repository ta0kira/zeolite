#include "intersect.h"

#include "bool.h"
#include "dispatch.h"
#include "union.h"

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
  S<TypeValue> Create(const S<TypeValue>& value);

 private:
  MergeType InstanceMergeType() const final { return MergeType::INTERSECT; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_;
};


class Value_Intersect : public TypeValue {
 public:
  Value_Intersect(TypeInstance& parent,
              const S<TypeValue>& value)
      : parent_(parent), value_(value) {
#ifdef OPT_TYPE_CHECKING
    FAIL_IF(!TypeValue::CheckConversionTo(value_,parent_))
        << "Cannot assign type-value " << value_->InstanceName()
        << " to " << parent_.InstanceName();
#endif
  }

  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const TypeArgs&,
      const FunctionArgs& args) final;
  S<TypeValue> GetNestedValue() final { return value_; }
  bool GetBool() const final;
  std::string GetString() const final;

 private:
  const TypeInstance& InstanceType() const final { return parent_; }
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

S<TypeValue> Instance_Intersect::Create(const S<TypeValue>& value) {
  if (&value->CategoryType() == &Category_Intersect() ||
      &value->CategoryType() == &Category_Union()) {
    return S_get(new Value_Intersect(*this,value->GetNestedValue()));
  } else {
    return S_get(new Value_Intersect(*this,value));
  }
}


FunctionReturns Value_Intersect::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return value_->CallValueFunction(id,types,args);
}

bool Value_Intersect::GetBool() const {
  return value_->GetBool();
}

std::string Value_Intersect::GetString() const {
  return value_->GetString();
}

S<TypeValue> Value_Intersect::ConvertTo(TypeInstance& instance) {
  return TypeValue::ConvertTo(value_,instance);
}

}  // namespace


TypeCategory& Category_Intersect() {
  return Internal_Intersect();
}

TypeInstance& Intersect_Any() {
  return Build_Intersect(TypeArgs{});
}

TypeInstance& Build_Intersect(const TypeArgs& types) {
  return Internal_Intersect().BuildInternal(types);
}

S<TypeValue> As_Intersect(const S<TypeValue>& value, const TypeArgs& types) {
#ifdef OPT_TYPE_CHECKING
  return value;
#else
  if (types.size() == 1) {
    return TypeValue::ConvertTo(value,*SafeGet<0>(types));
  } else {
    return Internal_Intersect().BuildInternal(types).Create(value);
  }
#endif
}
