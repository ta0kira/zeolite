#include "dynamic_example_gen.h"

#include <iostream>

/*

interface Function<x|y> {
  call takes (x) to (y)
}

*/

const FunctionId<FunctionScope::VALUE> Function_Function_call("Function.call");

class Constructor_Function;

class Instance_Function : public TypeInstance {
 public:
  Instance_Function(const Constructor_Function& parent,
                    const S<const TypeInstance>& x,
                    const S<const TypeInstance>& y)
      : parent_(parent), x_(x), y_(y) {}

  std::string TypeName() const final;
  const CategoryId* CategoryType() const final;
  TypeArgs ConstructorArgs() const final;

 private:
  const Constructor_Function& parent_;
  const S<const TypeInstance> x_;
  const S<const TypeInstance> y_;
};


S<TypeInstance> Constructor_Function::BindAll(const ParamInstance<2>::Args& args) {
  return BindInternal(std::get<0>(args),std::get<1>(args));
}

const CategoryId* Constructor_Function::CategoryType() const {
  static const CategoryId type("Function");
  return &type;
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


std::string Instance_Function::TypeName() const {
  std::ostringstream formatted;
  formatted << "Function<" << x_->TypeName() << "," << y_->TypeName() << ">";
  return formatted.str();
}

const CategoryId* Instance_Function::CategoryType() const {
  return parent_.CategoryType();
}

TypeArgs Instance_Function::ConstructorArgs() const {
  return TypeArgs{x_.get(),y_.get()};
}

/*

interface Data<x> {
}

*/

const FunctionId<FunctionScope::VALUE> Function_Data_set("Data.set");
const FunctionId<FunctionScope::VALUE> Function_Data_get("Data.get");

class Constructor_Data;

class Value_Data : public TypeValue {
 public:
  Value_Data(const Constructor_Data& parent,
             const Instance_Data& type,
             const S<Interface_Data>& interface)
      : parent_(parent), type_(type), interface_(interface) {}

  const TypeInstance* ValueType() const final;
  FunctionReturns CallValueFunction(
      const FunctionId<FunctionScope::VALUE>& id,
      const FunctionArgs& args) final;

 private:
  const Constructor_Data& parent_;
  const Instance_Data& type_;
  const S<Interface_Data> interface_;
};

class Instance_Data : public TypeInstance {
 public:
  Instance_Data(const Constructor_Data& parent,
                const S<const TypeInstance>& x)
      : parent_(parent), x_(x) {}

  std::string TypeName() const final;
  const CategoryId* CategoryType() const final;
  TypeArgs ConstructorArgs() const final;

 private:
  const Constructor_Data& parent_;
  const S<const TypeInstance> x_;
};


Constructor_Data::Constructor_Data()
    : instance_functions_(),
      value_functions_(std::move(
          FunctionRouter<Interface_Data,FunctionScope::VALUE>()
              .AddFunction(Function_Data_set, &Interface_Data::Call_Data_set)
              .AddFunction(Function_Data_get, &Interface_Data::Call_Data_get))) {}

S<TypeInstance> Constructor_Data::BindAll(const ParamInstance<1>::Args& args) {
  return BindInternal(std::get<0>(args));
}

const CategoryId* Constructor_Data::CategoryType() const {
  static const CategoryId type("Data");
  return &type;
}

S<TypeValue> Constructor_Data::Create_Value(
    const S<TypeInstance>& x, const S<Interface_Data>& interface) {
  return S_get(new Value_Data(*this,*BindInternal(x),interface));
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


const TypeInstance* Value_Data::ValueType() const {
  return &type_;
}

FunctionReturns Value_Data::CallValueFunction(
    const FunctionId<FunctionScope::VALUE>& id,
    const FunctionArgs& args) {
  return parent_.value_functions_.Call(id,interface_.get(),args);
}


std::string Instance_Data::TypeName() const {
  std::ostringstream formatted;
  formatted << "Data<" << x_->TypeName() << ">";
  return formatted.str();
}

const CategoryId* Instance_Data::CategoryType() const {
  return parent_.CategoryType();
}

TypeArgs Instance_Data::ConstructorArgs() const {
  return TypeArgs{x_.get()};
}

/*

interface Value {
  inherits Data<Value>
}

*/

const FunctionId<FunctionScope::INSTANCE> Function_Value_create("Value.create");
const FunctionId<FunctionScope::VALUE> Function_Value_set("Value.set");
const FunctionId<FunctionScope::VALUE> Function_Value_get("Value.get");

class Concrete_Value : public Interface_Value {
 public:
  Concrete_Value() {}

  T<> Call_Value_set(const T<S<TypeValue>>&) final;
  T<S<TypeValue>> Call_Value_get(const T<>&) final;

 private:
  // Corresponds to a member variable in Value.
  // TODO: There should be a variable wrapper that handles converting to/from
  // the static type used where it's defined in the code. There also needs to be
  // a non-missing variant with enforcement.
  S<TypeValue> value_;
};

class Constructor_Value;
class Instance_Value;

class Value_Value : public TypeValue {
 public:
  Value_Value(const Constructor_Value& parent,
              const TypeInstance& type,
              const S<Interface_Value>& interface)
      : parent_(parent), type_(type), interface_(interface) {}

  const TypeInstance* ValueType() const final;
  FunctionReturns CallValueFunction(
      const FunctionId<FunctionScope::VALUE>& id,
      const FunctionArgs& args) final;
  S<TypeValue> ConvertTo(const S<const TypeInstance>& type) final;

 private:
  S<TypeValue> Convert_Data(const S<const TypeInstance>&);

  const Constructor_Value& parent_;
  const TypeInstance& type_;
  const S<Interface_Value> interface_;
};

class Instance_Value : public TypeInstance {
 public:
  Instance_Value(const Constructor_Value& parent)
      : parent_(parent) {}

  std::string TypeName() const final;
  const CategoryId* CategoryType() const final;
  FunctionReturns CallInstanceFunction(
      const FunctionId<FunctionScope::INSTANCE>& id,
      const FunctionArgs& args) final;
  TypeArgs ConstructorArgs() const final;
  T<S<TypeValue>> create(const T<>& args);

 private:
  const Constructor_Value& parent_;
};


Constructor_Value::Constructor_Value()
    : instance_functions_(std::move(
          FunctionRouter<Instance_Value,FunctionScope::INSTANCE>()
              .AddFunction(Function_Value_create, &Instance_Value::create))),
      value_functions_(std::move(
          FunctionRouter<Interface_Value,FunctionScope::VALUE>()
              .AddFunction(Function_Data_set, &Interface_Value::Call_Value_set)
              .AddFunction(Function_Data_get, &Interface_Value::Call_Value_get)
              .AddFunction(Function_Value_set, &Interface_Value::Call_Value_set)
              .AddFunction(Function_Value_get, &Interface_Value::Call_Value_get))),
      only_instance_(new Instance_Value(*this)) {}

S<TypeInstance> Constructor_Value::BindAll(const ParamInstance<0>::Args& args) {
  return only_instance_;
}

const CategoryId* Constructor_Value::CategoryType() const {
  static const CategoryId type("Value");
  return &type;
}

const S<Constructor_Value> Category_Value(new Constructor_Value);


class Wrap_Value_Data : public Interface_Data {
 public:
  Wrap_Value_Data(const S<Interface_Value>& interface)
      : interface_(interface) {}

  T<> Call_Data_set(const T<S<TypeValue>>& args) final {
    std::cerr << "Call_Data_set" << std::endl;
    return interface_->Call_Value_set(args);
  }

  T<S<TypeValue>> Call_Data_get(const T<>& args) final {
    std::cerr << "Call_Data_get" << std::endl;
    return interface_->Call_Value_get(args);
  }

 private:
  const S<Interface_Value> interface_;
};


const TypeInstance* Value_Value::ValueType() const {
  return &type_;
}

FunctionReturns Value_Value::CallValueFunction(
    const FunctionId<FunctionScope::VALUE>& id,
    const FunctionArgs& args) {
  return parent_.value_functions_.Call(id,interface_.get(),args);
}

S<TypeValue> Value_Value::ConvertTo(const S<const TypeInstance>& type) {
  // TODO: This is fairly hackish. Maybe type conversion also needs routing
  // based on the base-type.
  if (type->CategoryType() == Category_Data->CategoryType()) {
    return Convert_Data(type);
  }
  return TypeValue::ConvertTo(type);
}

S<TypeValue> Value_Value::Convert_Data(const S<const TypeInstance>& type) {
  FAIL_IF(type->ConstructorArgs() != TypeArgs{&type_})
      << type->TypeName() << " parameters does not match " << type_.TypeName();
  return Category_Data->Create_Value(
      Category_Value->Build(), S_get(new Wrap_Value_Data(interface_)));
}

T<> Concrete_Value::Call_Value_set(const T<S<TypeValue>>& args) {
  std::cerr << "Call_Value_set" << std::endl;
  // TODO: There should be a way to convert the type of the arg to what the
  // function definition uses.
  value_ = TypeValue::ConvertTo(std::get<0>(args),Category_Value->Build());
  return T_get();
}

T<S<TypeValue>> Concrete_Value::Call_Value_get(const T<>&) {
  std::cerr << "Call_Value_get" << std::endl;
  return T_get(value_);
}


std::string Instance_Value::TypeName() const {
  return "Value";
}

const CategoryId* Instance_Value::CategoryType() const {
  return parent_.CategoryType();
}

FunctionReturns Instance_Value::CallInstanceFunction(
    const FunctionId<FunctionScope::INSTANCE>& id,
    const FunctionArgs& args) {
  return parent_.instance_functions_.Call(id,this,args);
}

TypeArgs Instance_Value::ConstructorArgs() const {
  return TypeArgs{};
}

T<S<TypeValue>> Instance_Value::create(const T<>& args) {
  return T_get(S_get(new Value_Value(parent_,*this,S_get(new Concrete_Value()))));
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
  std::cerr << v->ValueType()->TypeName() << std::endl;
  auto v2 = v->ConvertTo(Category_Data->Build(Category_Value->Build()));
  TypeValue::ConvertTo(v,Category_Value->Build());  // Convert to same type.
  std::cerr << v2->ValueType()->TypeName() << std::endl;
  v->CallValueFunction(Function_Data_set, FunctionArgs{v});
  v2->CallValueFunction(Function_Data_get, FunctionArgs{});
}
