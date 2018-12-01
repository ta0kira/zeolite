#include "value.h"

#include <iostream>

#include "base/dispatch.h"
#include "base/optional.h"

namespace {

class Constructor_Value;
class Instance_Value;
class Value_Value;
class Concrete_Value;


class Constructor_Value : public ParamInstance<0>::Type {
 public:
  Constructor_Value();

  TypeInstance& Build() final;
  const std::string& CategoryName() const final { return name_; }
  Instance_Value& BuildInternal();

  const FunctionDispatcher<Instance_Value,MemberScope::INSTANCE>& instance_functions;
  const FunctionDispatcher<Value_Value,MemberScope::VALUE>& value_functions;

 private:
  const std::string name_{"Value"};
  FunctionDispatcher<Instance_Value,MemberScope::INSTANCE> instance_functions_;
  FunctionDispatcher<Value_Value,MemberScope::VALUE> value_functions_;
  InstanceCache<Instance_Value> instance_cache_;
};

Constructor_Value& Internal_Value() {
  static Constructor_Value*const constructor = new Constructor_Value;
  return *constructor;
}


class Instance_Value : public TypeInstance {
 public:
  Instance_Value()
      : Type_create_r0(*this),
        name_(ConstructInstanceName(Category_Value())) {}

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Value(); }
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;
  FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>&, const FunctionArgs&) final;

  T<S<TypeValue>> Call_create(const T<>&);

  TypeInstance& Type_create_r0;

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_{this};
  const TypeArgs args_self_{};
};


class Value_Value : public TypeValue {
 public:
  Value_Value(Instance_Value& parent,
              const S<Concrete_Value>& interface)
      : parent_(parent),
        interface_(interface) {}

  const TypeInstance& InstanceType() const final { return parent_; }
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) final;

  T<> Call_print(const T<>&) const;

 private:
  S<TypeValue> ConvertTo(TypeInstance&) final;

  Instance_Value& parent_;
  const S<Concrete_Value> interface_;
};


struct Concrete_Value {
 public:
  Concrete_Value(Instance_Value& parent) : parent_(parent) {}
  T<> Call_Value_print();

 private:
  Instance_Value& parent_;
};

S<TypeValue> AsValue(const S<Concrete_Value>& value) {
  return S_get(new Value_Value(Internal_Value().BuildInternal(),value));
}


Constructor_Value::Constructor_Value()
    : instance_functions(instance_functions_),
      value_functions(value_functions_),
      instance_functions_("Value"),
      value_functions_("Value") {
  instance_functions_
      .AddFunction(Function_Value_create,&Instance_Value::Call_create);
  value_functions_
      .AddFunction(Function_Value_print,&Value_Value::Call_print);
}

TypeInstance& Constructor_Value::Build() {
  return BuildInternal();
}

Instance_Value& Constructor_Value::BuildInternal() {
  R<Instance_Value>& instance = instance_cache_.Create();
  if (!instance) {
    instance = R_get(new Instance_Value());
  }
  return *instance;
}

const TypeArgs& Instance_Value::TypeArgsForCategory(const TypeCategory& category) const {
  // TODO: Generalize this better.
  if (&category == &Category_Value()) {
    return args_self_;
  }
  return TypeInstance::TypeArgsForCategory(category);
}

FunctionReturns Instance_Value::CallInstanceFunction(
    const FunctionId<MemberScope::INSTANCE>& id, const FunctionArgs& args) {
  return Internal_Value().instance_functions.Call(id,this,args);
}

T<S<TypeValue>> Instance_Value::Call_create(const T<>&) {
  return T_get(TypeValue::ConvertTo(S_get(new Value_Value(*this,S_get(new Concrete_Value(*this)))),
               Type_create_r0));
}

bool Instance_Value::CheckConversionFrom(const TypeInstance& instance) const {
  const TypeArgs& args = instance.TypeArgsForCategory(Category_Value());
  FAIL_IF(args.size() != 0) << "Wrong number of type args";
  return true;
}


FunctionReturns Value_Value::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) {
  return Internal_Value().value_functions.Call(id,this,args);
}

T<> Value_Value::Call_print(const T<>& args) const {
  const T<> results = interface_->Call_Value_print();
  return T_get();
}

S<TypeValue> Value_Value::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Value()) {
    const TypeArgs& args = instance.TypeArgsForCategory(Category_Value());
    FAIL_IF(args.size() != 0) << "Wrong number of type args";
    return AsValue(interface_);
  }
  return TypeValue::ConvertTo(instance);
}


T<> Concrete_Value::Call_Value_print() {
  std::cout << "A Value has been printed" << std::endl;
  return T_get();
}

}  // namespace


ParamInstance<0>::Type& Category_Value() {
  return Internal_Value();
}

const FunctionId<MemberScope::INSTANCE>& Function_Value_create =
    *new FunctionId<MemberScope::INSTANCE>("Value.create");
const FunctionId<MemberScope::VALUE>& Function_Value_print =
    *new FunctionId<MemberScope::VALUE>("Value.print");
