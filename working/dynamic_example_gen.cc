#include <functional>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include <unordered_map>
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

struct TypeId {};

struct FunctionId {
  virtual std::string FunctionName() const = 0;
};

using FunctionArgs = std::vector<S<TypeValue>>;
using FunctionReturns = std::vector<S<TypeValue>>;


template<int K, class V, class T>
struct ArgsToTuple {
  static void Set(const V& vals, T& tuple) {
    std::get<K-1>(tuple) = vals[K];
    ArgsToTuple<K-1,V,T>::Set(vals, tuple);
  }
};

template<class V, class T>
struct ArgsToTuple<0,V,T> {
  static void Set(const V& vals, T& tuple) {}
};

template<class X, class...Ts>
T<Ts...> V_to_T(const std::vector<X>& vals) {
  static constexpr int tuple_size = std::tuple_size<T<Ts...>>::value;
  FAIL_IF(vals.size() != tuple_size) << "Expected " << tuple_size << " elements";
  T<Ts...> tuple;
  ArgsToTuple<tuple_size,std::vector<X>,T<Ts...>>::Set(vals, tuple);
  return tuple;
}


template<int K, class T, class V>
struct TupleToArgs {
  static void Set(const T& tuple, V& vals) {
    vals.push_back(std::get<K-1>(tuple));
    TupleToArgs<K-1,T,V>::Set(tuple, vals);
  }
};

template<class T, class V>
struct TupleToArgs<0,T,V> {
  static void Set(const T& tuple, V& vals) {}
};

template<class X, class...Ts>
std::vector<X> T_to_V(const T<Ts...>& tuple) {
  static constexpr int tuple_size = std::tuple_size<T<Ts...>>::value;
  std::vector<X> vals;
  vals.reserve(tuple_size);
  TupleToArgs<tuple_size,T<Ts...>,std::vector<X>>::Set(tuple, vals);
  return vals;
}


template<class C>
struct FunctionCaller {
  virtual FunctionReturns Call(C*, const FunctionArgs&) const = 0;
  virtual ~FunctionCaller() = default;
};

template<class C, class A, class R>
class FixedCaller : public FunctionCaller<C> {
 public:
  FixedCaller(const std::function<R(C*,const A&)>& function)
      : function_(function) {}

  FunctionReturns Call(C* object, const FunctionArgs& args) const {
    return T_to_V<S<TypeValue>>(function_(object, V_to_T<S<TypeValue>>(args)));
  }

 private:
  const std::function<R(C*,const A&)> function_;
};


template<class C>
class FunctionRouter {
 public:
  template<class A, class R>
  FunctionRouter& AddFunction(const FunctionId& id, R(C::*function)(const A&)) {
    mapped_[&id] = R_get(new FixedCaller<C,A,R>(
        [function](C* object, const A& args) {
          return (object->*function)(args);
        }));
    return *this;
  }

  FunctionReturns Call(const FunctionId& id, C* object, const FunctionArgs& args) const {
    const auto caller = mapped_.find(&id);
    FAIL_IF(caller == mapped_.end()) << "Function " << id.FunctionName() << " not supported";
    return caller->second->Call(object, args);
  }

 private:
  std::unordered_map<const FunctionId*,R<const FunctionCaller<C>>> mapped_;
};


template<class C>
class FunctionRouter<const C> {
 public:
  template<class A, class R>
  FunctionRouter& AddFunction(const FunctionId& id, R(C::*function)(const A&) const) {
    mapped_[&id] = R_get(new FixedCaller<const C,A,R>(
        [function](const C* object, const A& args) {
          return (object->*function)(args);
        }));
    return *this;
  }

  FunctionReturns Call(const FunctionId& id, const C* object, const FunctionArgs& args) const {
    const auto caller = mapped_.find(&id);
    FAIL_IF(caller == mapped_.end()) << "Function " << id.FunctionName() << " not supported";
    return caller->second->Call(object, args);
  }

 private:
  std::unordered_map<const FunctionId*,R<const FunctionCaller<const C>>> mapped_;
};


struct TypeInstance {
  virtual std::string TypeName() const = 0;
  virtual const TypeId* BaseType() const = 0;
  virtual TypeArgs ConstructorArgs() const = 0;

  virtual FunctionReturns CallStaticFunction(const FunctionId& id, const FunctionArgs&) const {
    FAIL() << "Static function " << id.FunctionName()
           << " not supported in " << TypeName();
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
    FAIL() << "Instance function " << id.FunctionName()
           << " not supported in " << ValueType()->TypeName();
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
  static const FunctionId& CREATE;

  ValueT() {
    static_router_.AddFunction(CREATE, &Instance::create);
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
} ValueT_CREATE;

const FunctionId& ValueT::CREATE = ValueT_CREATE;

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
  S<TypeValue> v = v_type->CallStaticFunction(ValueT::CREATE, FunctionArgs{})[0];
  std::cerr << v->ValueType()->TypeName() << std::endl;
  auto v2 = v->ConvertTo(Data->Build(Value->Build()));
  std::cerr << v2->ValueType()->TypeName() << std::endl;
  // Error! Not an instance function.
  v2->CallInstanceFunction(ValueT::CREATE, FunctionArgs{});
}
