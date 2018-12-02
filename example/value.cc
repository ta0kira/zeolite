#include "value.h"

#include <iostream>

#include "base/dispatch.h"
#include "base/string.h"
#include "base/trace.h"

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
      : name_(ConstructInstanceName(Category_Value())) {}

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Value(); }
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;
  FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>&,
      const TypeArgs&,
      const FunctionArgs&) final;

  ParamReturns<1>::Type Call_create(ParamTypes<1>::Type, ParamArgs<1>::Type);

  TypeInstance& Type_Var_value(TypeInstance& x) const {
    return x;
  }

  TypeInstance& Type_create_a0(TypeInstance& x) const {
    return x;
  }

  TypeInstance& Type_create_r0() const {
    return Category_Value().Build();
  }

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_{this};
  const TypeArgs args_self_{};
  const TypeArgs args_printable_{};
};


class Value_Value : public TypeValue {
 public:
  Value_Value(Instance_Value& parent,
              const S<Concrete_Value>& interface)
      : parent_(parent),
        interface_(interface) {}

  const TypeInstance& InstanceType() const final { return parent_; }
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const TypeArgs&,
      const FunctionArgs& args) final;

  ParamReturns<0>::Type Call_print(ParamTypes<0>::Type, ParamArgs<0>::Type) const;

 private:
  S<TypeValue> ConvertTo(TypeInstance&) final;

  Instance_Value& parent_;
  const S<Concrete_Value> interface_;
};


struct Concrete_Value : virtual public Interface_Printable {
 public:
  Concrete_Value(Instance_Value& parent,
                 TypeInstance& x,
                 const S<TypeValue>& value)
      : parent_(parent),
        x_(x),
        value_(parent.Type_Var_value(x),value) {}

  T<> Call_Printable_print() final;

 private:
  TypeInstance& x_;
  Instance_Value& parent_;
  ValueVariable value_;
};

S<TypeValue> As_Value(const S<Concrete_Value>& value) {
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
      .AddFunction(Function_Printable_print,&Value_Value::Call_print);
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
  if (&category == &Category_Printable()) {
    return args_printable_;
  }
  return TypeInstance::TypeArgsForCategory(category);
}

FunctionReturns Instance_Value::CallInstanceFunction(
    const FunctionId<MemberScope::INSTANCE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return Internal_Value().instance_functions.Call(id,this,types,args);
}

/*

00: create (value) {
01:   return Value{
02:     types = <x>,
03:     value = value,
04:   };
05: }

*/

ParamReturns<1>::Type Instance_Value::Call_create(
      ParamTypes<1>::Type types, ParamArgs<1>::Type args) {
  SourceContext trace("Value.create");
  trace.SetLocal("value:0");
  S<TypeValue> value = TypeValue::ConvertTo(std::get<0>(args),Type_create_a0(*std::get<0>(types)));
  trace.SetLocal("value:1");
  return
      T_get(
          TypeValue::ConvertTo(
              S_get(
                  new Value_Value(
                      *this,
                      S_get(new Concrete_Value(*this,*std::get<0>(types),value)))),Type_create_r0()));
}

bool Instance_Value::CheckConversionFrom(const TypeInstance& instance) const {
  const TypeArgs& args = instance.TypeArgsForCategory(Category_Value());
  FAIL_IF(args.size() != 0) << "Wrong number of type args";
  return true;
}


FunctionReturns Value_Value::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return Internal_Value().value_functions.Call(id,this,types,args);
}

ParamReturns<0>::Type Value_Value::Call_print(
    ParamTypes<0>::Type types, ParamArgs<0>::Type args) const {
  const T<> results = interface_->Call_Printable_print();
  return T_get();
}

S<TypeValue> Value_Value::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Value()) {
    const TypeArgs& args = instance.TypeArgsForCategory(Category_Value());
    FAIL_IF(args.size() != 0) << "Wrong number of type args";
    return As_Value(interface_);
  }
  if (&instance.CategoryType() == &Category_Printable()) {
    return As_Printable(interface_);
  }
  return TypeValue::ConvertTo(instance);
}


/*

07: print () {
08:   optional String string = reduce<String>(value);
09:   if (present(string)) {
10:     print(require(string));  // <- fake syntax for now
11:   } else {
12:     print("(not a string)");
13:   }
14: }

*/

T<> Concrete_Value::Call_Printable_print() {
  SourceContext trace("Value.print");
  trace.SetLocal("value:8");
  S<TypeValue> string =
      TypeValue::ReduceTo(value_.GetValue(),Category_String().Build());
  trace.SetLocal("value:9");
  if (string->IsPresent()) {
    trace.SetLocal("value:10");
    std::cout << TypeValue::Require(string)->GetString() << std::endl;
  } else {
    trace.SetLocal("value:12");
    std::cout << "(not a string)" << std::endl;
  }
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
