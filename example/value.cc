#include "value.h"

#include <iostream>

#include "base/dispatch.h"
#include "base/optional.h"
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
  const GetValueDispatcher<Concrete_Value,MemberScope::VALUE>& value_variables;
  const GetTypeDispatcher<Concrete_Value,MemberScope::VALUE>& type_variables;

 private:
  ~Constructor_Value() = default;

  const std::string name_{"Value"};
  FunctionDispatcher<Instance_Value,MemberScope::INSTANCE> instance_functions_;
  FunctionDispatcher<Value_Value,MemberScope::VALUE> value_functions_;
  GetValueDispatcher<Concrete_Value,MemberScope::VALUE> value_variables_;
  GetTypeDispatcher<Concrete_Value,MemberScope::VALUE> type_variables_;
  InstanceCache<Instance_Value> instance_cache_;
};

Constructor_Value& Internal_Value() {
  static Constructor_Value*const constructor = new Constructor_Value;
  return *constructor;
}


class Instance_Value : public TypeInstance {
 public:
  Instance_Value()
      : name_(ConstructInstanceName(Category_Value())) {
    parents_
        .AddParent(Category_Value())
        .AddParent(Category_Printable());
  }

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Value(); }
  bool IsParentCategory(const TypeCategory&) const final;
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;
  FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>&,
      const TypeArgs&,
      const FunctionArgs&) final;

  ParamReturns<1>::Type Call_create(ParamTypes<1>::Type, ParamArgs<1>::Type);
  ParamReturns<0>::Type Call_view(ParamTypes<0>::Type, ParamArgs<1>::Type);

  TypeInstance& Type_Var_value(TypeInstance& x) const {
    return x;
  }

  TypeInstance& Type_create_a0(TypeInstance& x) const {
    return x;
  }

  TypeInstance& Type_create_r0() const {
    return Category_Value().Build();
  }

  TypeInstance& Type_view_a0() const {
    return Category_Value().Build();
  }

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_{this};
  ParentTypes parents_;
};


class Value_Value : public TypeValue {
 public:
  Value_Value(Instance_Value& parent,
              const S<Concrete_Value>& interface)
      : parent_(parent),
        interface_(interface) {}

  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const TypeArgs&,
      const FunctionArgs& args) final;

  ValueVariable* GetValueVariable(
      const TypeInstance&,
      const ValueVariableId<MemberScope::VALUE>&) final;

  TypeInstance* GetTypeVariable(
      const TypeInstance&,
      const TypeVariableId<MemberScope::VALUE>&) final;

  ParamReturns<0>::Type Call_print(ParamTypes<0>::Type, ParamArgs<0>::Type) const;

 private:
  const TypeInstance& InstanceType() const final { return parent_; }
  S<TypeValue> ConvertTo(TypeInstance&) final;

  Instance_Value& parent_;
  const S<Concrete_Value> interface_;
};


const ValueVariableId<MemberScope::VALUE>& Variable_Value_value =
    *new ValueVariableId<MemberScope::VALUE>("Value.value");
const TypeVariableId<MemberScope::VALUE>& Variable_Value_x =
    *new TypeVariableId<MemberScope::VALUE>("Value.x");

struct Concrete_Value : virtual public Interface_Printable {
 public:
  Concrete_Value(Instance_Value& parent,
                 TypeInstance& x,
                 const S<TypeValue>& value)
      : parent_(parent),
        x_(&x),
        value_(parent.Type_Var_value(x),value) {}

  T<> Call_Printable_print() final;

  Instance_Value& parent_;
  TypeInstance* const x_;
  ValueVariable value_;
};

S<TypeValue> As_Value(const S<Concrete_Value>& value) {
  return S_get(new Value_Value(Internal_Value().BuildInternal(),value));
}


Constructor_Value::Constructor_Value()
    : instance_functions(instance_functions_),
      value_functions(value_functions_),
      value_variables(value_variables_),
      type_variables(type_variables_),
      instance_functions_("Value"),
      value_functions_("Value"),
      value_variables_("Value"),
      type_variables_("Value") {
  instance_functions_
      .AddFunction(Function_Value_create,&Instance_Value::Call_create)
      .AddFunction(Function_Viewer_view,&Instance_Value::Call_view);
  value_functions_
      .AddFunction(Function_Printable_print,&Value_Value::Call_print);
  value_variables_
      .AddVariable(Variable_Value_value,&Concrete_Value::value_);
  type_variables_
      .AddVariable(Variable_Value_x,&Concrete_Value::x_);
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

bool Instance_Value::IsParentCategory(const TypeCategory& category) const {
  if (parents_.HasParent(category)) {
    return true;
  }
  return TypeInstance::IsParentCategory(category);
}

const TypeArgs& Instance_Value::TypeArgsForCategory(const TypeCategory& category) const {
  if (parents_.HasParent(category)) {
    return parents_.GetParent(category);
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
06:
07: view (obj) {
08:   optional String string = reduce<String>(obj.value);
09:   if (present(string)) {
10:     print("View: " + require(string));  // <- fake syntax for now
11:   }
12: }

*/

ParamReturns<1>::Type Instance_Value::Call_create(
      ParamTypes<1>::Type types, ParamArgs<1>::Type args) {
  TRACE_FUNCTION("Value.create")
  SET_CONTEXT_POINT("value:0")
  TypeInstance& x = *std::get<0>(types);
  ValueVariable value(Type_create_a0(x),std::get<0>(args));
  SET_CONTEXT_POINT("value:1")
  return
      T_get(
          TypeValue::ConvertTo(
              S_get(
                  new Value_Value(
                      *this,
                      S_get(new Concrete_Value(*this,x,value.GetValue())))),Type_create_r0()));
}

ParamReturns<0>::Type Instance_Value::Call_view(
      ParamTypes<0>::Type types, ParamArgs<1>::Type args) {
  TRACE_FUNCTION("Value.view")
  SET_CONTEXT_POINT("value:7")
  ValueVariable obj(Type_view_a0(),std::get<0>(args));
  SET_CONTEXT_POINT("value:8");
  ValueVariable string(
      Category_Optional().Build(Category_String().Build()),
      TypeValue::ReduceTo(
          obj.GetValue()
              ->GetValueVariable(*this,Variable_Value_value)
              ->GetValue(),
          // NOTE: This assumes that the compiler is going to infer that the
          // expression type is obj.x, and not whatever the runtime type of
          // obj.value happens to be.
          *TypeValue::ConvertTo(obj.GetValue(),Type_view_a0())
              ->GetTypeVariable(*this,Variable_Value_x),
          Category_String().Build()));
  SET_CONTEXT_POINT("value:9")
  if (string.GetValue()->IsPresent()) {
    SET_CONTEXT_POINT("value:10")
    std::cout << "View: " << TypeValue::Require(string.GetValue())->GetString() << std::endl;
  }
  return T_get();
}

bool Instance_Value::CheckConversionFrom(const TypeInstance& instance) const {
  if (!CategoryIsParentOf(instance)) {
    return false;
  }
  const TypeArgs& args = CategoryTypeArgsFrom(instance);
  FAIL_IF(args.size() != 0) << "Wrong number of type args";
  return true;
}


FunctionReturns Value_Value::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return Internal_Value().value_functions.Call(id,this,types,args);
}

ValueVariable* Value_Value::GetValueVariable(
    const TypeInstance& instance,
    const ValueVariableId<MemberScope::VALUE>& id) {
  if (&instance == &InstanceType()) {
    return Internal_Value().value_variables.GetVariable(id,interface_.get());
  }
  return TypeValue::GetValueVariable(instance,id);
}

TypeInstance* Value_Value::GetTypeVariable(
    const TypeInstance& instance,
    const TypeVariableId<MemberScope::VALUE>& id) {
  if (&instance == &InstanceType()) {
    return Internal_Value().type_variables.GetVariable(id,interface_.get());
  }
  return TypeValue::GetTypeVariable(instance,id);
}

ParamReturns<0>::Type Value_Value::Call_print(
    ParamTypes<0>::Type types, ParamArgs<0>::Type args) const {
  const T<> results = interface_->Call_Printable_print();
  return T_get();
}

S<TypeValue> Value_Value::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Value()) {
    const TypeArgs& args = InstanceType().CategoryTypeArgsFrom(instance);
    FAIL_IF(args.size() != 0) << "Wrong number of type args";
    return As_Value(interface_);
  }
  if (&instance.CategoryType() == &Category_Printable()) {
    return TypeValue::ConvertTo(As_Printable(interface_),instance);
  }
  return TypeValue::ConvertTo(instance);
}


/*

13: print () {
14:   optional String string = reduce<String>(value);
15:   if (present(string)) {
16:     print(require(string));  // <- fake syntax for now
17:   } else {
18:     print("(not a string)");
19:   }
10: }

*/

T<> Concrete_Value::Call_Printable_print() {
  TRACE_FUNCTION("Value.print")
  SET_CONTEXT_POINT("value:14")
  ValueVariable string(
      Category_Optional().Build(Category_String().Build()),
      TypeValue::ReduceTo(
          value_.GetValue(),
          *x_,
          Category_String().Build()));
  SET_CONTEXT_POINT("value:5")
  if (string.GetValue()->IsPresent()) {
    SET_CONTEXT_POINT("value:16")
    std::cout << TypeValue::Require(string.GetValue())->GetString() << std::endl;
  } else {
    SET_CONTEXT_POINT("value:18")
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
