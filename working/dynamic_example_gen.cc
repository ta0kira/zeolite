#include "dynamic_example_gen.h"

#include <iostream>
#include <unordered_map>

/*

interface Function<x|y> {
  call takes (x) to (y)
}

*/

const FunctionId<MemberScope::VALUE> Function_Function_call("Function.call");

class Value_Function : public TypeValue {
 public:
  const TypeInstance* InstanceType() const final;
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const FunctionArgs& args) final;

 private:
  Value_Function(Instance_Function& type,
                 const S<Interface_Function>& interface)
      : type_(type), interface_(interface) {}

  Instance_Function& type_;
  const S<Interface_Function> interface_;

  friend class Constructor_Function;
};

class Instance_Function : public TypeInstance {
 public:
  Instance_Function(Constructor_Function& parent,
                    const S<const TypeInstance>& x,
                    const S<const TypeInstance>& y)
      : parent_(parent), x_(x), y_(y) {}

  std::string TypeName() const final;

 private:
  Constructor_Function& parent_;
  const S<const TypeInstance> x_;
  const S<const TypeInstance> y_;

  friend class Value_Function;
};


Constructor_Function::Constructor_Function()
    : value_functions_(std::move(
          FunctionDispatcher<Interface_Function,MemberScope::VALUE>(CategoryType()->TypeName())
              .AddFunction(Function_Function_call, &Interface_Function::Call_Function_call))) {}

S<TypeInstance> Constructor_Function::BindAll(const ParamInstance<2>::Args& args) {
  return BindInternal(std::get<0>(args),std::get<1>(args));
}

const CategoryId* Constructor_Function::CategoryType() const {
  static const CategoryId type("Function");
  return &type;
}

S<TypeValue> Constructor_Function::CreateValue(
    const S<TypeInstance>& x,
    const S<TypeInstance>& y,
    const S<Interface_Function>& interface) {
  return S_get(new Value_Function(*BindInternal(x,y),interface));
}

S<Instance_Function> Constructor_Function::BindInternal(
    const S<TypeInstance>& x, const S<TypeInstance>& y) {
  S<Instance_Function>& instance =
      instance_cache_[InstanceCacheKey{x.get(),y.get()}];
  if (!instance) {
    instance = S_get(new Instance_Function(*this,x,y));
    std::cerr << "New: " << instance->TypeName() << std::endl;
  } else {
    std::cerr << "From cache: " << instance->TypeName() << std::endl;
  }
  return instance;
}

const S<Constructor_Function> Category_Function(new Constructor_Function);


const TypeInstance* Value_Function::InstanceType() const {
  return &type_;
}

FunctionReturns Value_Function::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const FunctionArgs& args) {
  return type_.parent_.value_functions_.Call(id,interface_.get(),args);
}


std::string Instance_Function::TypeName() const {
  std::ostringstream formatted;
  formatted << "Function<" << x_->TypeName() << "," << y_->TypeName() << ">";
  return formatted.str();
}

/*

interface Data<x> {
  set takes (x) to ()
  get takes () to (x)
}

*/

const FunctionId<MemberScope::VALUE> Function_Data_set("Data.set");
const FunctionId<MemberScope::VALUE> Function_Data_get("Data.get");

class Constructor_Data;

class Value_Data : public TypeValue {
 public:
  Value_Data(Instance_Data& type,
             const S<Interface_Data>& interface)
      : type_(type), interface_(interface) {}

  const TypeInstance* InstanceType() const final;
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const FunctionArgs& args) final;

 private:
  Instance_Data& type_;
  const S<Interface_Data> interface_;
};

class Instance_Data : public TypeInstance {
 public:
  Instance_Data(Constructor_Data& parent,
                const S<const TypeInstance>& x)
      : parent_(parent), x_(x) {}

  std::string TypeName() const final;

 private:
  Constructor_Data& parent_;
  const S<const TypeInstance> x_;

  friend class Value_Data;
};


Constructor_Data::Constructor_Data()
    : value_functions_(std::move(
          FunctionDispatcher<Interface_Data,MemberScope::VALUE>(CategoryType()->TypeName())
              .AddFunction(Function_Data_set, &Interface_Data::Call_Data_set)
              .AddFunction(Function_Data_get, &Interface_Data::Call_Data_get))) {}

S<TypeInstance> Constructor_Data::BindAll(const ParamInstance<1>::Args& args) {
  return BindInternal(std::get<0>(args));
}

const CategoryId* Constructor_Data::CategoryType() const {
  static const CategoryId type("Data");
  return &type;
}

S<TypeValue> Constructor_Data::CreateValue(
    const S<TypeInstance>& x, const S<Interface_Data>& interface) {
  return S_get(new Value_Data(*BindInternal(x),interface));
}

S<Instance_Data> Constructor_Data::BindInternal(const S<TypeInstance>& x) {
  S<Instance_Data>& instance =
      instance_cache_[InstanceCacheKey{x.get()}];
  if (!instance) {
    instance = S_get(new Instance_Data(*this,x));
    std::cerr << "New: " << instance->TypeName() << std::endl;
  } else {
    std::cerr << "From cache: " << instance->TypeName() << std::endl;
  }
  return instance;
}

const S<Constructor_Data> Category_Data(new Constructor_Data);


const TypeInstance* Value_Data::InstanceType() const {
  return &type_;
}

FunctionReturns Value_Data::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const FunctionArgs& args) {
  return type_.parent_.value_functions_.Call(id,interface_.get(),args);
}


std::string Instance_Data::TypeName() const {
  std::ostringstream formatted;
  formatted << "Data<" << x_->TypeName() << ">";
  return formatted.str();
}

/*

interface Value {
  refines Data<Value>
  in instance create takes () to (Value)
  log takes () to ()
}

*/

const FunctionId<MemberScope::INSTANCE> Function_Value_create("Value.create");
const FunctionId<MemberScope::VALUE> Function_Value_log("Value.log");

class Value_Value : public TypeValue {
 public:
  const TypeInstance* InstanceType() const final;
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const FunctionArgs& args) final;
  S<TypeValue> ConvertTo(const CategoryId* category) final;

 private:
  Value_Value(Instance_Value& type,
              const S<Interface_Value>& interface)
      : type_(type), interface_(interface) {}

  Instance_Value& type_;
  const S<Interface_Value> interface_;

  friend class Constructor_Value;
};

class Instance_Value : public TypeInstance {
 public:
  Instance_Value(Constructor_Value& parent)
      : parent_(parent),
        refines_({Category_Data->CategoryType(),{this}}) {}

  std::string TypeName() const final;
  const TypeArgs& TypeArgsForCategory(const CategoryId*) const final;
  FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>& id,
      const FunctionArgs& args) final;

 protected:
  bool CheckConversionTo(const TypeInstance*) const final;

 private:
  Constructor_Value& parent_;
  const std::unordered_map<const CategoryId*,TypeArgs&> refines_;

  friend class Value_Value;
};


Constructor_Value::Constructor_Value()
    : instance_functions_(std::move(
          FunctionDispatcher<Instance_Value,MemberScope::INSTANCE>(CategoryType()->TypeName())
              .AddFunction(Function_Value_create, &Instance_Value::create))),
      value_functions_(std::move(
          FunctionDispatcher<Interface_Value,MemberScope::VALUE>(CategoryType()->TypeName())
              .AddFunction(Function_Data_set, &Interface_Value::Call_Data_set)
              .AddFunction(Function_Data_get, &Interface_Value::Call_Data_get)
              .AddFunction(Function_Value_log, &Interface_Value::Call_Value_log))),
      only_instance_(new Instance_Value(*this)) {}

S<TypeInstance> Constructor_Value::BindAll(const ParamInstance<0>::Args& args) {
  return only_instance_;
}

const CategoryId* Constructor_Value::CategoryType() const {
  static const CategoryId type("Value");
  return &type;
}

S<TypeValue> Constructor_Value::CreateValue(const S<Interface_Value>& interface) {
  return S_get(new Value_Value(*only_instance_,interface));
}

const S<Constructor_Value> Category_Value(new Constructor_Value);


class Concrete_Value : public Interface_Value {
 public:
  Concrete_Value(Instance_Value& type) : type_(type) {}

  T<> Call_Data_set(const T<S<TypeValue>>&) final;
  T<S<TypeValue>> Call_Data_get(const T<>&) final;
  T<> Call_Value_log(const T<>&) final;

 private:
  Instance_Value& type_;
  // Corresponds to a member variable in Value.
  // TODO: There should be a variable wrapper that handles converting to/from
  // the static type used where it's defined in the code. There also needs to be
  // a non-missing variant with enforcement.
  S<TypeValue> value_;
};


const TypeInstance* Value_Value::InstanceType() const {
  return &type_;
}

FunctionReturns Value_Value::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const FunctionArgs& args) {
  return type_.parent_.value_functions_.Call(id,interface_.get(),args);
}

S<TypeValue> Value_Value::ConvertTo(const CategoryId* category) {
  // TODO: Use a map here? That might require naming each converter, though.
  // (Also, this converter is specific to the instance, not to the value.)
  if (category == Category_Data->CategoryType()) {
    // NOTE: The param passed here (x=Value) is based on inheritance used when
    // defining Value.
    return Category_Data->CreateValue(Category_Value->Build(),interface_);
  }
  return TypeValue::ConvertTo(category);
}

T<> Concrete_Value::Call_Data_set(const T<S<TypeValue>>& args) {
  std::cerr << "Call_Data_set" << std::endl;
  // Convert the arg to the declared type for Value.set. (This could be more
  // *general* than Data.set.)
  // TODO: The variable "value" should also verify that only valid values are
  // assigned, to include params if there are any.
  value_ = TypeValue::ConvertTo(std::get<0>(args),
                                Category_Value->CategoryType());
  return T_get();
}

T<S<TypeValue>> Concrete_Value::Call_Data_get(const T<>&) {
  std::cerr << "Call_Data_get" << std::endl;
  // Convert the return to the declared type for Value.get. (This could be more
  // *specific* than Data.get.)
  return T_get(TypeValue::ConvertTo(value_,
               Category_Value->CategoryType()));
}

T<> Concrete_Value::Call_Value_log(const T<>&) {
  std::cerr << "Call_Value_log" << std::endl;
  return T_get();
}


std::string Instance_Value::TypeName() const {
  return "Value";
}

const TypeArgs& Instance_Value::TypeArgsForCategory(const CategoryId*) const {
    const auto refine = refines_.find(&id);
    FAIL_IF(refine == refines_.end())
        << "Category " << id.TypeName() << " not refined by " << TypeName();
    return refine->second;
}

FunctionReturns Instance_Value::CallInstanceFunction(
    const FunctionId<MemberScope::INSTANCE>& id,
    const FunctionArgs& args) {
  return parent_.instance_functions_.Call(id,this,args);
}

bool Instance_Value::CheckConversionTo(const TypeInstance*) const {
  return false;  // TODO: Fix this.
}

TypeArgs Instance_Value::ConstructorArgs() const {
  return TypeArgs{};
}

T<S<TypeValue>> Instance_Value::create(const T<>& args) {
  return T_get(parent_.CreateValue(S_get(new Concrete_Value(*this))));
}

/*

Function<Data<x>,x>

*/

const S<ParamInstance<1>::Type> DataFunction =
    AutoCompose<2,1>(Category_Function,
                     Category_Data,
                     Select<1,0>::New());

/*

Function<Data<Value>,Value>

*/

// Using composition, e.g., within a function that creates Function<Data<x>,x>.
const S<TypeInstance> ValueDataFunction =
    AutoCompose<1,0>(DataFunction,
                     Category_Value)->Build();

// Without composition, i.e., literally Function<Data<Value>,Value>.
const S<TypeInstance> ValueDataFunction2 =
    Category_Function->Build(
      Category_Data->Build(Category_Value->Build()),
      Category_Value->Build());


int main() {
  std::cerr << ValueDataFunction->TypeName() << std::endl;
  std::cerr << ValueDataFunction2->TypeName() << std::endl;
  const S<TypeInstance> v_type = Category_Value->Build();
  S<TypeValue> v = v_type->CallInstanceFunction(Function_Value_create, FunctionArgs{})[0];
  std::cerr << v->InstanceType()->TypeName() << std::endl;
  auto v2 = v->ConvertTo(Category_Data->CategoryType());
  TypeValue::ConvertTo(v,Category_Value->CategoryType());  // Convert to same type.
  std::cerr << v2->InstanceType()->TypeName() << std::endl;
  v->CallValueFunction(Function_Data_set, FunctionArgs{v});
  v2->CallValueFunction(Function_Data_get, FunctionArgs{});
  v->CallValueFunction(Function_Value_log, FunctionArgs{});
  // Error! Even though the underlying type is Value, v2 is of type Data.
  v2->CallValueFunction(Function_Value_log, FunctionArgs{});
}
