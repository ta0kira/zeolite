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
  S<TypeValue> Create(const S<TypeValue>& value);

 private:
  MergeType InstanceMergeType() const final { return MergeType::UNION; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_;
};


class Value_Union : public TypeValue {
 public:
  Value_Union(TypeInstance& parent,
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
  ValueVariable* GetValueVariable(const ValueVariableId<MemberScope::VALUE>&) final;
  S<TypeValue> GetNestedValue() final { return value_; }
  bool GetBool() const final;
  std::string GetString() const final;

 private:
  const TypeInstance& InstanceType() const final { return parent_; }
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

S<TypeValue> Instance_Union::Create(const S<TypeValue>& value) {
  if (&value->CategoryType() == &Category_Union() ||
      &value->CategoryType() == &Category_Union()) {
    return S_get(new Value_Union(*this,value->GetNestedValue()));
  } else {
    return S_get(new Value_Union(*this,value));
  }
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

TypeInstance& Union_All() {
  return Build_Union(TypeArgs{});
}

TypeInstance& Build_Union(const TypeArgs& types) {
  return Internal_Union().BuildInternal(types);
}

S<TypeValue> As_Union(const S<TypeValue>& value, const TypeArgs& types) {
#ifdef OPT_TYPE_CHECKING
  return value;
#else
  if (types.size() == 1) {
    return TypeValue::ConvertTo(value,*SafeGet<0>(types));
  } else {
    return Internal_Union().BuildInternal(types).Create(value);
  }
#endif
}
