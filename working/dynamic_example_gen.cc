#include <functional>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

#include "base.h"

template<class...Ts>
using T = std::tuple<Ts...>;

template<class...Ts>
T<Ts...> T_get(Ts... ts) { return std::make_tuple(ts...); }

struct TypeValue;
struct TypeInstance;

using InstanceCacheKey = std::vector<const TypeInstance*>;
using InstanceCache = std::map<InstanceCacheKey,S<const TypeInstance>>;

// TODO: Probably needs to use shared_ptr elements so that the types can be
// referenced in conversion wrappers.
using TypeArgs = std::vector<const TypeInstance*>;

using FunctionId = std::string;
using FunctionArgs = std::vector<S<TypeValue>>;
using FunctionReturns = std::vector<S<TypeValue>>;

struct TypeId {};

struct TypeInstance {
  virtual std::string TypeName() const = 0;
  virtual const TypeId* BaseType() const = 0;
  virtual TypeArgs ConstructorArgs() const = 0;

  virtual FunctionReturns CallStaticFunction(const FunctionId& id, const FunctionArgs&) const {
    FAIL() << "Static function " << id << " not supported in " << TypeName();
    return FunctionReturns();
  }

  virtual ~TypeInstance() = default;
};

struct TypeValue {
  virtual const TypeInstance* ValueType() const = 0;

  virtual S<TypeValue> ConvertTo(const S<const TypeInstance>& type) {
    FAIL() << "Cannot convert " << ValueType()->TypeName()
           << " to " << type->TypeName();
    return nullptr;
  }

  virtual FunctionReturns CallInstanceFunction(const FunctionId& id, const FunctionArgs&) {
    FAIL() << "Instance function " << id << " not supported in " << ValueType()->TypeName();
    return FunctionReturns();
  }

  virtual ~TypeValue() = default;
};

template<class...Ts>
struct TypeConstructor {
  virtual S<const TypeInstance> BindAll(const T<Ts...>&) = 0;
  virtual const TypeId* BaseType() const = 0;
  virtual S<const TypeInstance> Build(Ts... ts) {
    return BindAll(T_get(ts...));
  }

  virtual ~TypeConstructor() = default;
};


template<int N, class...Ts>
struct ParamInstance {
  using Type = typename ParamInstance<N-1, S<const TypeInstance>, Ts...>::Type;
  using Args = typename ParamInstance<N-1, S<const TypeInstance>, Ts...>::Args;
};

template<class...Ts>
struct ParamInstance<0, Ts...> {
  using Type = TypeConstructor<Ts...>;
  using Args = T<Ts...>;
};


template<int N, int K>
struct Select : public ParamInstance<N>::Type {
  S<const TypeInstance> BindAll(const typename ParamInstance<N>::Args& args) final {
    return std::get<K>(args);
  }

  const TypeId* BaseType() const final {
    static class : public TypeId {} type;
    return &type;
  }

  static S<typename ParamInstance<N>::Type> New() {
    return S_get(new Select());
  }
};

template<int N, int M, class...Cs>
class Composer : public ParamInstance<N>::Type {
 public:
  Composer(const S<typename ParamInstance<M>::Type>& outer, Cs... inner)
      : outer_(outer),
      inner_([inner...](const typename ParamInstance<N>::Args& args) ->
                 typename ParamInstance<M>::Args {
               return T_get(inner->BindAll(args)...);
             })
      {}

  const TypeId* BaseType() const final {
    return outer_->BaseType();
  }

  S<const TypeInstance> BindAll(const typename ParamInstance<N>::Args& args) final {
    return outer_->BindAll(inner_(args));
  }

 private:
  const S<typename ParamInstance<M>::Type> outer_;
  const std::function<typename ParamInstance<M>::Args(const typename ParamInstance<N>::Args&)> inner_;
};


template<int M, int N, class...Cs>
S<typename ParamInstance<N>::Type> AutoCompose(const S<typename ParamInstance<M>::Type>& outer, Cs... inner) {
  return S_get(new Composer<N, M, Cs...>(outer, inner...));
}

/*

interface Function<x|y> {
}

*/

extern const S<ParamInstance<2>::Type> Function;

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

extern const S<ParamInstance<1>::Type> Data;

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

extern const S<ParamInstance<0>::Type> Value;

class : public TypeId {} value_type;

class ValueT : public ParamInstance<0>::Type {
 public:
  S<const TypeInstance> BindAll(const ParamInstance<0>::Args& args) final {
    return only_instance_;
  }

  const TypeId* BaseType() const final {
    return &value_type;
  }

 private:
  class Instance : public TypeInstance {
   public:
    std::string TypeName() const final {
      return "Value";
    }

    const TypeId* BaseType() const final {
      return &value_type;
    }

    FunctionReturns CallStaticFunction(const FunctionId& id, const FunctionArgs& args) const final {
      if (id == "create") {
        FAIL_IF(args.size() != 0) << "Too many args passed to 'Value.create'";
        return FunctionReturns{S_get(new Value(*this))};
      }
      return TypeInstance::CallStaticFunction(id, args);
    }

    TypeArgs ConstructorArgs() const final {
      return TypeArgs{};
    }
  };

  class Value : public TypeValue {
   public:
    Value(const Instance& type) : type_(type) {}

    const TypeInstance* ValueType() const final {
      return &type_;
    }

    S<TypeValue> ConvertTo(const S<const TypeInstance>& type) override {
      if (type->BaseType() == Data->BaseType() && type->ConstructorArgs() == TypeArgs{&type_}) {
        // TODO: Handle conversion.
        return S_get(new Value_Data(*type));
      }
      return TypeValue::ConvertTo(type);
    }

   private:
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

  const S<const TypeInstance> only_instance_ = S_get(new Instance());
};

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
  S<TypeValue> v = v_type->CallStaticFunction("create", FunctionArgs{})[0];
  std::cerr << v->ValueType()->TypeName() << std::endl;
  auto v2 = v->ConvertTo(Data->Build(Value->Build()));
  std::cerr << v2->ValueType()->TypeName() << std::endl;
}
