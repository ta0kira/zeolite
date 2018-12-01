#include "optional.h"

#include <sstream>

#include "category_base.h"

namespace {

class Constructor_Optional;
class Instance_Optional;
class Value_Optional;


const CategoryId& CATEGORY_ID = *new CategoryId("Optional");

class Constructor_Optional : public ParamInstance<1>::Type {
 public:
  Constructor_Optional();

  S<TypeInstance> BindAll(const ParamInstance<1>::Args& args) final;
  const CategoryId* CategoryType() const final { return &CATEGORY_ID; }
  S<Instance_Optional> BuildInternal(const S<TypeInstance>& x);

 private:
  FunctionDispatcher<Value_Optional,MemberScope::VALUE> value_functions_;
  InstanceCache<Instance_Optional> instance_cache_;
};

Constructor_Optional& Internal_Optional = *new Constructor_Optional;


class Instance_Optional : public TypeInstance {
 public:
  Instance_Optional(TypeCategory& parent,
                    const FunctionDispatcher<Value_Optional,MemberScope::VALUE>& value_functions,
                    const S<TypeInstance>& x)
      : parent_(parent), value_functions_(value_functions), x_(x) {}

  std::string TypeName() const final;
  const TypeArgs& TypeArgsForCategory(const CategoryId* id) const final;
  S<TypeValue> Create(const S<TypeValue>& value);
  S<TypeValue> Skip();

 private:
  bool CheckConversionFrom(const TypeInstance* type) const final;

  TypeCategory& parent_;
  const FunctionDispatcher<Value_Optional,MemberScope::VALUE>& value_functions_;
  const S<TypeInstance> x_;
};


class Value_Optional : public TypeValue {
 public:
  Value_Optional(TypeInstance& parent,
                 const FunctionDispatcher<Value_Optional,MemberScope::VALUE>& value_functions,
                 const S<TypeValue>& value)
      : parent_(parent), value_functions_(value_functions), value_(value) {}

  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) final;
  bool IsOptional() const final { return true; }

  T<S<TypeValue>> Call_Optional_present(const T<>&) const;
  T<S<TypeValue>> Call_Optional_require(const T<>&) const;

 private:
  const TypeInstance* InstanceType() const final { return &parent_; }

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

S<TypeValue> AsOptional(const S<TypeInstance>& type, const S<TypeValue>& value) {
  if (value->IsOptional()) {
    return value;
  } else {
    return Internal_Optional.BuildInternal(type)->Create(value);
  }
}

S<TypeValue> SkipOptional(const S<TypeInstance>& type) {
  return Internal_Optional.BuildInternal(type)->Skip();
}


namespace {

Constructor_Optional::Constructor_Optional()
    : value_functions_("Optional") {
  value_functions_
      .AddFunction(Function_Optional_present,&Value_Optional::Call_Optional_present)
      .AddFunction(Function_Optional_require,&Value_Optional::Call_Optional_require);
}

S<TypeInstance> Constructor_Optional::BindAll(const ParamInstance<1>::Args& args) {
  return BuildInternal(std::get<0>(args));
}

S<Instance_Optional> Constructor_Optional::BuildInternal(const S<TypeInstance>& x) {
  S<Instance_Optional>& instance = instance_cache_.Create(x);
  if (!instance) {
    instance = S_get(new Instance_Optional(*this,value_functions_,x));
  }
  return instance;
}

std::string Instance_Optional::TypeName() const {
  std::ostringstream formatted;
  formatted << parent_.CategoryType()->TypeName() << "<" << x_->TypeName() << ">";
  return formatted.str();
}

const TypeArgs& Instance_Optional::TypeArgsForCategory(const CategoryId* id) const {
  // Can implicitly convert from y to optional x if y -> x.
  return x_->TypeArgsForCategory(id);
}

S<TypeValue> Instance_Optional::Create(const S<TypeValue>& value) {
  return S_get(new Value_Optional(*this,value_functions_,value));
}

S<TypeValue> Instance_Optional::Skip() {
  return Create(nullptr);
}

bool Instance_Optional::CheckConversionFrom(const TypeInstance* type) const {
  const TypeArgs& args = type->TypeArgsForCategory(&CATEGORY_ID);
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return CheckConversionBetween(args[0],x_.get());  // covariant
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
    FAIL() << InstanceType()->TypeName() << " value is not present";
  }
  return value_;
}

}  // namespace
