#include "dynamic_example_gen.h"

#include <iostream>

/*

interface Function<x|y> {
}

*/

class : public TypeId {} function_type;

class FunctionT : public ParamInstance<2>::Type {
 public:
  S<const TypeInstance> BindAll(const ParamInstance<2>::Args& args) final {
    S<const TypeInstance>& instance =
        instance_cache_[InstanceCacheKey{std::get<0>(args).get(),std::get<1>(args).get()}];
    if (!instance) {
      instance = S_get(new Instance(std::get<0>(args), std::get<1>(args)));
      std::cerr << "New: " << instance->TypeName() << std::endl;
      return instance;
    } else {
      std::cerr << "From cache: " << instance->TypeName() << std::endl;
    }
    return instance;
  }

  const TypeId* BaseType() const final {
    return &function_type;
  }

 private:
  class Instance : public TypeInstance {
   public:
    Instance(const S<const TypeInstance>& x, const S<const TypeInstance>& y) : x_(x), y_(y) {}

    std::string TypeName() const final {
      std::ostringstream formatted;
      formatted << "Function<" << x_->TypeName() << "," << y_->TypeName() << ">";
      return formatted.str();
    }

    const TypeId* BaseType() const final {
      return &function_type;
    }

    TypeArgs ConstructorArgs() const final {
      return TypeArgs{x_.get(),y_.get()};
    }

   private:
    const S<const TypeInstance> x_;
    const S<const TypeInstance> y_;
  };

  InstanceCache instance_cache_;
};

const S<ParamInstance<2>::Type> Function(new FunctionT);

/*

interface Data<x> {
}

*/

class : public TypeId {} data_type;

class DataT : public ParamInstance<1>::Type {
 public:
  S<const TypeInstance> BindAll(const ParamInstance<1>::Args& args) final {
    S<const TypeInstance>& instance =
        instance_cache_[InstanceCacheKey{std::get<0>(args).get()}];
    if (!instance) {
      instance = S_get(new Instance(std::get<0>(args)));
      std::cerr << "New: " << instance->TypeName() << std::endl;
      return instance;
    } else {
      std::cerr << "From cache: " << instance->TypeName() << std::endl;
    }
    return instance;
  }

  const TypeId* BaseType() const final {
    return &data_type;
  }

 private:
  class Instance : public TypeInstance {
   public:
    Instance(const S<const TypeInstance>& x) : x_(x) {}

    std::string TypeName() const final {
      std::ostringstream formatted;
      formatted << "Data<" << x_->TypeName() << ">";
      return formatted.str();
    }

    const TypeId* BaseType() const final {
      return &data_type;
    }

    TypeArgs ConstructorArgs() const final {
      return TypeArgs{x_.get()};
    }

   private:
    const S<const TypeInstance> x_;
  };

  InstanceCache instance_cache_;
};

const S<ParamInstance<1>::Type> Data(new DataT);

/*

interface Value {
  inherits Data<Value>
}

*/

class : public TypeId {} value_type;

class ValueT : public ParamInstance<0>::Type {
 public:
  ValueT() {
    static_router_.AddFunction(Value_create, &Instance::create);
  }

  S<const TypeInstance> BindAll(const ParamInstance<0>::Args& args) final {
    return only_instance_;
  }

  const TypeId* BaseType() const final {
    return &value_type;
  }

 private:
  class Instance : public TypeInstance {
   public:
    Instance(const ValueT& parent) : parent_(parent) {}

    std::string TypeName() const final {
      return "Value";
    }

    const TypeId* BaseType() const final {
      return parent_.BaseType();
    }

    FunctionReturns CallStaticFunction(const FunctionId& id, const FunctionArgs& args) const final {
      return parent_.static_router_.Call(id, this, args);
    }

    TypeArgs ConstructorArgs() const final {
      return TypeArgs{};
    }

    T<S<TypeValue>> create(const T<>& args) const {
      return T_get(S_get(new Value(parent_,*this)));
    }

   private:
    const ValueT& parent_;
  };

  class Value : public TypeValue {
   public:
    Value(const ValueT& parent, const Instance& type) : parent_(parent), type_(type) {}

    const TypeInstance* ValueType() const final {
      return &type_;
    }

    FunctionReturns CallInstanceFunction(const FunctionId& id, const FunctionArgs& args) final {
      return parent_.instance_router_.Call(id, this, args);
    }

    S<TypeValue> ConvertTo(const S<const TypeInstance>& type) override {
      if (type->BaseType() == Data->BaseType() && type->ConstructorArgs() == TypeArgs{&type_}) {
        // TODO: Handle conversion.
        return S_get(new Value_Data(*type));
      }
      return TypeValue::ConvertTo(type);
    }

   private:
    const ValueT& parent_;
    const Instance& type_;
  };

  // TODO: Does Data need to provide this wrapper (or an abstract interface)
  // for its own conversions?
  class Value_Data : public TypeValue {
   public:
    Value_Data(const TypeInstance& type) : type_(type) {}

    const TypeInstance* ValueType() const final {
      return &type_;
    }

   private:
    const TypeInstance& type_;
  };

  FunctionRouter<const Instance> static_router_;
  FunctionRouter<Value> instance_router_;
  const S<const TypeInstance> only_instance_ = S_get(new Instance(*this));
};

struct : FunctionId {
  std::string FunctionName() const final {
    return "Value.create";
  }
} Id_Value_create;

const FunctionId& Value_create = Id_Value_create;

const S<ParamInstance<0>::Type> Value(new ValueT);

/*

Function<Data<x>,x>

*/

const S<ParamInstance<1>::Type> DataFunction = AutoCompose<2,1>(Function, Data, Select<1,0>::New());

/*

Function<Data<Value>,Value>

*/

// Using composition, e.g., within a function that creates Function<Data<x>,x>.
const S<const TypeInstance> ValueDataFunction = DataFunction->Build(Value->Build());

// Without composition, i.e., literally Function<Data<Value>,Value>.
const S<const TypeInstance> ValueDataFunction2 = Function->Build(Data->Build(Value->Build()), Value->Build());


int main() {
  std::cerr << ValueDataFunction->TypeName() << std::endl;
  std::cerr << ValueDataFunction2->TypeName() << std::endl;
  const S<const TypeInstance> v_type = Value->Build();
  S<TypeValue> v = v_type->CallStaticFunction(Value_create, FunctionArgs{})[0];
  std::cerr << v->ValueType()->TypeName() << std::endl;
  auto v2 = v->ConvertTo(Data->Build(Value->Build()));
  std::cerr << v2->ValueType()->TypeName() << std::endl;
  // Error! Not an instance function.
  v2->CallInstanceFunction(Value_create, FunctionArgs{});
}
