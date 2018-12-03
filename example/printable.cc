#include "printable.h"

#include "base/dispatch.h"
#include "base/optional.h"

namespace {

class Constructor_Printable;
class Instance_Printable;
class Value_Printable;


class Constructor_Printable : public ParamInstance<0>::Type {
 public:
  Constructor_Printable();

  TypeInstance& Build() final;
  const std::string& CategoryName() const final { return name_; }
  Instance_Printable& BuildInternal();

  const FunctionDispatcher<Value_Printable,MemberScope::VALUE>& value_functions;

 private:
  const std::string name_{"Printable"};
  FunctionDispatcher<Value_Printable,MemberScope::VALUE> value_functions_;
  InstanceCache<Instance_Printable> instance_cache_;
};

Constructor_Printable& Internal_Printable() {
  static Constructor_Printable*const constructor = new Constructor_Printable;
  return *constructor;
}


class Instance_Printable : public TypeInstance {
 public:
  Instance_Printable()
      : name_(ConstructInstanceName(Category_Printable())) {
    parents_.AddParent(Category_Printable());
  }

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Printable(); }
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_{this};
  ParentTypes parents_;
};


class Value_Printable : public TypeValue {
 public:
  Value_Printable(Instance_Printable& parent,
               const S<Interface_Printable>& interface)
      : parent_(parent),
        interface_(interface) {}

  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const TypeArgs&,
      const FunctionArgs& args) final;

  ParamReturns<0>::Type Call_print(ParamTypes<0>::Type, ParamArgs<0>::Type) const;

 private:
  const TypeInstance& InstanceType() const final { return parent_; }
  S<TypeValue> ConvertTo(TypeInstance&) final;

  Instance_Printable& parent_;
  const S<Interface_Printable> interface_;
};


Constructor_Printable::Constructor_Printable()
    : value_functions(value_functions_),
      value_functions_("Printable") {
  value_functions_
      .AddFunction(Function_Printable_print,&Value_Printable::Call_print);
}

TypeInstance& Constructor_Printable::Build() {
  return BuildInternal();
}

Instance_Printable& Constructor_Printable::BuildInternal() {
  R<Instance_Printable>& instance = instance_cache_.Create();
  if (!instance) {
    instance = R_get(new Instance_Printable());
  }
  return *instance;
}

const TypeArgs& Instance_Printable::TypeArgsForCategory(const TypeCategory& category) const {
  if (parents_.HasParent(category)) {
    return parents_.GetParent(category);
  }
  return TypeInstance::TypeArgsForCategory(category);
}

bool Instance_Printable::CheckConversionFrom(const TypeInstance& instance) const {
  if (!CategoryIsParentOf(instance)) {
    return false;
  }
  const TypeArgs& args = CategoryTypeArgsFrom(instance);
  FAIL_IF(args.size() != 0) << "Wrong number of type args";
  return true;
}


FunctionReturns Value_Printable::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return Internal_Printable().value_functions.Call(id,this,types,args);
}

ParamReturns<0>::Type Value_Printable::Call_print(
    ParamTypes<0>::Type, ParamArgs<0>::Type) const {
  const T<> results = interface_->Call_Printable_print();
  return T_get();
}

S<TypeValue> Value_Printable::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Printable()) {
    const TypeArgs& args = InstanceType().CategoryTypeArgsFrom(instance);
    FAIL_IF(args.size() != 0) << "Wrong number of type args";
    return As_Printable(interface_);
  }
  return TypeValue::ConvertTo(instance);
}

}  // namespace


ParamInstance<0>::Type& Category_Printable() {
  return Internal_Printable();
}

const FunctionId<MemberScope::VALUE>& Function_Printable_print =
    *new FunctionId<MemberScope::VALUE>("Printable.print");

S<TypeValue> As_Printable(const S<Interface_Printable>& value) {
  return S_get(new Value_Printable(Internal_Printable().BuildInternal(),value));
}
