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
  const TypeId* BaseType() const final;
  TypeArgs ConstructorArgs() const final;

 private:
  const Constructor_Function& parent_;
  const S<const TypeInstance> x_;
  const S<const TypeInstance> y_;
};

class Constructor_Function : public ParamInstance<2>::Type {
 public:
  S<TypeInstance> BindAll(const ParamInstance<2>::Args& args) final {
    S<TypeInstance>& instance =
        instance_cache_[InstanceCacheKey{std::get<0>(args).get(),
                                         std::get<1>(args).get()}];
    if (!instance) {
      instance = S_get(new Instance_Function(*this,
                                             std::get<0>(args),
                                             std::get<1>(args)));
      std::cerr << "New: " << instance->TypeName() << std::endl;
      return instance;
    } else {
      std::cerr << "From cache: " << instance->TypeName() << std::endl;
    }
    return instance;
  }

  const TypeId* BaseType() const final {
    static const TypeId type("Function");
    return &type;
  }

 private:
  InstanceCache instance_cache_;
};

std::string Instance_Function::TypeName() const {
  std::ostringstream formatted;
  formatted << "Function<" << x_->TypeName() << "," << y_->TypeName() << ">";
  return formatted.str();
}

const TypeId* Instance_Function::BaseType() const {
  return parent_.BaseType();
}

TypeArgs Instance_Function::ConstructorArgs() const {
  return TypeArgs{x_.get(),y_.get()};
}

const S<ParamInstance<2>::Type> Category_Function(new Constructor_Function);

/*

interface Data<x> {
}

*/

const FunctionId<FunctionScope::VALUE> Function_Data_set("Data.set");
const FunctionId<FunctionScope::VALUE> Function_Data_get("Data.get");

class Constructor_Data;

class Instance_Data : public TypeInstance {
 public:
  Instance_Data(const Constructor_Data& parent,
                const S<const TypeInstance>& x)
      : parent_(parent), x_(x) {}

  std::string TypeName() const final;
  const TypeId* BaseType() const final;
  TypeArgs ConstructorArgs() const final;

 private:
   const Constructor_Data& parent_;
  const S<const TypeInstance> x_;
};

class Constructor_Data : public ParamInstance<1>::Type {
 public:
  S<TypeInstance> BindAll(const ParamInstance<1>::Args& args) final {
    S<TypeInstance>& instance =
        instance_cache_[InstanceCacheKey{std::get<0>(args).get()}];
    if (!instance) {
      instance = S_get(new Instance_Data(*this,std::get<0>(args)));
      std::cerr << "New: " << instance->TypeName() << std::endl;
      return instance;
    } else {
      std::cerr << "From cache: " << instance->TypeName() << std::endl;
    }
    return instance;
  }

  const TypeId* BaseType() const final {
    static const TypeId type("Data");
    return &type;
  }

 private:
  InstanceCache instance_cache_;
};

std::string Instance_Data::TypeName() const {
  std::ostringstream formatted;
  formatted << "Data<" << x_->TypeName() << ">";
  return formatted.str();
}

const TypeId* Instance_Data::BaseType() const {
  return parent_.BaseType();
}

TypeArgs Instance_Data::ConstructorArgs() const {
  return TypeArgs{x_.get()};
}

const S<ParamInstance<1>::Type> Category_Data(new Constructor_Data);

/*

interface Value {
  inherits Data<Value>
}

*/

const FunctionId<FunctionScope::INSTANCE> Function_Value_create("Value.create");
const FunctionId<FunctionScope::VALUE> Function_Value_set("Value.set");
const FunctionId<FunctionScope::VALUE> Function_Value_get("Value.get");

class Constructor_Value;
class Instance_Value;

class Value_Value : public TypeValue, public Interface_Value {
 public:
  Value_Value(const Constructor_Value& parent, const Instance_Value& type)
      : parent_(parent), type_(type) {}

  const TypeInstance* ValueType() const final;
  FunctionReturns CallValueFunction(
      const FunctionId<FunctionScope::VALUE>& id,
      const FunctionArgs& args) final;
  S<TypeValue> ConvertTo(const S<const TypeInstance>& type) final;

  S<TypeValue> Convert_Data(const S<const TypeInstance>&) final;
  T<> Call_Value_set(const T<S<TypeValue>>&) final;
  T<S<TypeValue>> Call_Value_get(const T<>&) final;

 private:
  const Constructor_Value& parent_;
  const Instance_Value& type_;
};

class Instance_Value : public TypeInstance {
 public:
  Instance_Value(const Constructor_Value& parent)
      : parent_(parent) {}

  std::string TypeName() const final;
  const TypeId* BaseType() const final;
  FunctionReturns CallInstanceFunction(
      const FunctionId<FunctionScope::INSTANCE>& id,
      const FunctionArgs& args) final;
  TypeArgs ConstructorArgs() const final;
  T<S<TypeValue>> create(const T<>& args);

 private:
  const Constructor_Value& parent_;
};

class Constructor_Value : public ParamInstance<0>::Type {
 public:
  Constructor_Value() {
    instance_functions_.AddFunction(Function_Value_create, &Instance_Value::create);
    value_functions_
        .AddFunction(Function_Value_set, &Interface_Value::Call_Value_set)
        .AddFunction(Function_Value_get, &Interface_Value::Call_Value_get);
  }

  S<TypeInstance> BindAll(const ParamInstance<0>::Args& args) final {
    return only_instance_;
  }

  const TypeId* BaseType() const final {
    static const TypeId type("Value");
    return &type;
  }

 private:
  FunctionRouter<Instance_Value,FunctionScope::INSTANCE> instance_functions_;
  FunctionRouter<Interface_Value,FunctionScope::VALUE> value_functions_;
  const S<TypeInstance> only_instance_ = S_get(new Instance_Value(*this));

  friend class Instance_Value;
  friend class Value_Value;
};

const S<ParamInstance<0>::Type> Category_Value(new Constructor_Value);


// TODO: Does Data need to provide this wrapper (or an abstract interface)
// for its own conversions?
class Value_Value_Data : public TypeValue, public Interface_Data {
  public:
  Value_Value_Data(const TypeInstance& type) : type_(type) {}

  const TypeInstance* ValueType() const final {
    return &type_;
  }

  T<> Call_Data_set(const T<S<TypeValue>>&) final {
    FAIL() << "Not implemented";
    return T_get();
  }

  T<S<TypeValue>> Call_Data_get(const T<>&) final {
    FAIL() << "Not implemented";
    return T_get(S<TypeValue>());
  }

  private:
  const TypeInstance& type_;
};


const TypeInstance* Value_Value::ValueType() const {
  return &type_;
}

FunctionReturns Value_Value::CallValueFunction(
    const FunctionId<FunctionScope::VALUE>& id,
    const FunctionArgs& args) {
  return parent_.value_functions_.Call(id,this,args);
}

S<TypeValue> Value_Value::ConvertTo(const S<const TypeInstance>& type) {
  // TODO: This is fairly hackish.
  if (type->BaseType() == Category_Data->BaseType()) {
    return Convert_Data(type);
  }
  return TypeValue::ConvertTo(type);
}

S<TypeValue> Value_Value::Convert_Data(const S<const TypeInstance>& type) {
  FAIL_IF(type->ConstructorArgs() != TypeArgs{&type_})
      << type->TypeName() << " parameters does not match " << type_.TypeName();
  return S_get(new Value_Value_Data(*type));
}

T<> Value_Value::Call_Value_set(const T<S<TypeValue>>&) {
  FAIL() << "Not implemented";
  return T_get();
}

T<S<TypeValue>> Value_Value::Call_Value_get(const T<>&) {
  FAIL() << "Not implemented";
  return T_get(S<TypeValue>());
}


std::string Instance_Value::TypeName() const {
  return "Value";
}

const TypeId* Instance_Value::BaseType() const {
  return parent_.BaseType();
}

FunctionReturns Instance_Value::CallInstanceFunction(
    const FunctionId<FunctionScope::INSTANCE>& id,
    const FunctionArgs& args) {
  return parent_.instance_functions_.Call(id, this, args);
}

TypeArgs Instance_Value::ConstructorArgs() const {
  return TypeArgs{};
}

T<S<TypeValue>> Instance_Value::create(const T<>& args) {
  return T_get(S_get(new Value_Value(parent_,*this)));
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
  std::cerr << v2->ValueType()->TypeName() << std::endl;
  v2->CallValueFunction(Function_Function_call, FunctionArgs{});
}
