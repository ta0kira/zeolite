#include "optional.h"

#include "bool.h"
#include "dispatch.h"

namespace {

class Constructor_Optional;
class Instance_Optional;
class Value_Optional;


class Constructor_Optional : public ParamInstance<1>::Type {
 public:
  Constructor_Optional();

  TypeInstance& Build(TypeInstance& x) final;
  const std::string& CategoryName() const final { return name_; }
  Instance_Optional& BuildInternal(TypeInstance& x);

  const FunctionDispatcher<Value_Optional,MemberScope::VALUE>& value_functions;

 private:
  const std::string name_{"Optional"};
  FunctionDispatcher<Value_Optional,MemberScope::VALUE> value_functions_;
  InstanceCache<Instance_Optional> instance_cache_;
};

Constructor_Optional& Internal_Optional = *new Constructor_Optional;


class Instance_Optional : public TypeInstance {
 public:
  Instance_Optional(TypeInstance& x)
      : x_(x),
        name_(ConstructInstanceName(Category_Optional,x)) {}

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Optional; }
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
  const TypeArgs args_{&x_};
};


class Value_Optional : public TypeValue {
 public:
  Value_Optional(TypeInstance& parent,
                 const S<TypeValue>& value)
      : parent_(parent), value_(value) {}

  const TypeInstance& InstanceType() const final { return parent_; }
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) final;

  T<S<TypeValue>> Call_present(const T<>&) const;
  T<S<TypeValue>> Call_require(const T<>&) const;

 private:
  S<TypeValue> ConvertTo(TypeInstance&) final;

  TypeInstance& parent_;
  const S<TypeValue> value_;
};

}  // namespace


ParamInstance<1>::Type& Category_Optional = Internal_Optional;

const FunctionId<MemberScope::VALUE>& Function_Optional_present =
    *new FunctionId<MemberScope::VALUE>("Optional.present");
const FunctionId<MemberScope::VALUE>& Function_Optional_require =
    *new FunctionId<MemberScope::VALUE>("Optional.require");

S<TypeValue> AsOptional(const S<TypeValue>& value, TypeInstance& type) {
  if (value->InstanceType().IsOptional()) {
    return value;
  } else {
    return Internal_Optional.BuildInternal(type).Create(value);
  }
}

S<TypeValue> SkipOptional(TypeInstance& type) {
  return Internal_Optional.BuildInternal(type).Skip();
}


namespace {

Constructor_Optional::Constructor_Optional()
    : value_functions(value_functions_),
      value_functions_("Optional") {
  value_functions_
      .AddFunction(Function_Optional_present,&Value_Optional::Call_present)
      .AddFunction(Function_Optional_require,&Value_Optional::Call_require);
}

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
  if (&category == &Category_Optional) {
    return args_;
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
  const TypeArgs& args = instance.TypeArgsForCategory(Category_Optional);
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return CheckConversionBetween(*SafeGet<0>(args),x_);  // covariant
}


FunctionReturns Value_Optional::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) {
  return Internal_Optional.value_functions.Call(id,this,args);
}

T<S<TypeValue>> Value_Optional::Call_present(const T<>&) const {
  return AsBool(!!value_);
}

T<S<TypeValue>> Value_Optional::Call_require(const T<>&) const {
  if (!value_) {
    FAIL() << InstanceType().InstanceName() << " value is not present";
  }
  return value_;
}

S<TypeValue> Value_Optional::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Optional) {
    const TypeArgs& args = instance.TypeArgsForCategory(Category_Optional);
    FAIL_IF(args.size() != 1) << "Wrong number of type args";
    return AsOptional(value_,*SafeGet<0>(args));
  }
  return TypeValue::ConvertTo(instance);
}

}  // namespace
