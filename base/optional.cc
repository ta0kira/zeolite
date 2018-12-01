#include "optional.h"

#include <sstream>

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
  Instance_Optional& BuildInternal(const TypeInstance& x);

 private:
  const std::string name_{"Optional"};
  FunctionDispatcher<Value_Optional,MemberScope::VALUE> value_functions_;
  InstanceCache<Instance_Optional> instance_cache_;
};

Constructor_Optional& Internal_Optional = *new Constructor_Optional;


class Instance_Optional : public TypeInstance {
 public:
  Instance_Optional(TypeCategory& parent,
                    const FunctionDispatcher<Value_Optional,MemberScope::VALUE>& value_functions,
                    const TypeInstance& x)
      : parent_(parent),
        value_functions_(value_functions),
        x_(x),
        name_(ConstructInstanceName(parent_, x_)) {}

  const std::string& InstanceName() const final { return name_; }
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;
  bool IsOptional() const final { return true; }
  S<TypeValue> Create(const S<TypeValue>& value);
  S<TypeValue> Skip();

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  TypeCategory& parent_;
  const FunctionDispatcher<Value_Optional,MemberScope::VALUE>& value_functions_;
  const TypeInstance& x_;
  const std::string name_;
  const TypeArgs types_{this};
};


class Value_Optional : public TypeValue {
 public:
  Value_Optional(TypeInstance& parent,
                 const FunctionDispatcher<Value_Optional,MemberScope::VALUE>& value_functions,
                 const S<TypeValue>& value)
      : parent_(parent), value_functions_(value_functions), value_(value) {}

  const TypeInstance& InstanceType() const final { return parent_; }
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) final;

  T<S<TypeValue>> Call_Optional_present(const T<>&) const;
  T<S<TypeValue>> Call_Optional_require(const T<>&) const;

 private:
  TypeInstance& parent_;
  const FunctionDispatcher<Value_Optional,MemberScope::VALUE>& value_functions_;
  const S<TypeValue> value_;
};

}  // namespace


ParamInstance<1>::Type& Category_Optional = Internal_Optional;

const FunctionId<MemberScope::VALUE>& Function_Optional_present =
    *new FunctionId<MemberScope::VALUE>("Optional.present");
const FunctionId<MemberScope::VALUE>& Function_Optional_require =
    *new FunctionId<MemberScope::VALUE>("Optional.require");

S<TypeValue> AsOptional(const TypeInstance& type, const S<TypeValue>& value) {
  if (value->InstanceType().IsOptional()) {
    return value;
  } else {
    return Internal_Optional.BuildInternal(type).Create(value);
  }
}

S<TypeValue> SkipOptional(const TypeInstance& type) {
  return Internal_Optional.BuildInternal(type).Skip();
}


namespace {

Constructor_Optional::Constructor_Optional()
    : value_functions_("Optional") {
  value_functions_
      .AddFunction(Function_Optional_present,&Value_Optional::Call_Optional_present)
      .AddFunction(Function_Optional_require,&Value_Optional::Call_Optional_require);
}

TypeInstance& Constructor_Optional::Build(TypeInstance& x) {
  return BuildInternal(x);
}

Instance_Optional& Constructor_Optional::BuildInternal(const TypeInstance& x) {
  R<Instance_Optional>& instance = instance_cache_.Create(x);
  if (!instance) {
    instance = R_get(new Instance_Optional(*this,value_functions_,x));
  }
  return *instance;
}

const TypeArgs& Instance_Optional::TypeArgsForCategory(const TypeCategory& category) const {
  // Can implicitly convert from y to optional x if y -> x.
  return x_.TypeArgsForCategory(category);
}

S<TypeValue> Instance_Optional::Create(const S<TypeValue>& value) {
  S<TypeValue> converted = TypeValue::ConvertTo(value,x_);
  return S_get(new Value_Optional(*this,value_functions_,converted));
}

S<TypeValue> Instance_Optional::Skip() {
  return S_get(new Value_Optional(*this,value_functions_,nullptr));
}

bool Instance_Optional::CheckConversionFrom(const TypeInstance& type) const {
  const TypeArgs& args = type.TypeArgsForCategory(Category_Optional);
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return CheckConversionBetween(*args[0],x_);  // covariant
}


FunctionReturns Value_Optional::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) {
  return value_functions_.Call(id,this,args);
}

T<S<TypeValue>> Value_Optional::Call_Optional_present(const T<>&) const {
  // TODO: Need a boolean type.
}

T<S<TypeValue>> Value_Optional::Call_Optional_require(const T<>&) const {
  if (!value_) {
    FAIL() << InstanceType().InstanceName() << " value is not present";
  }
  return value_;
}

}  // namespace
