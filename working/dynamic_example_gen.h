#ifndef DYNAMIC_EXAMPLE_GEN_H_
#define DYNAMIC_EXAMPLE_GEN_H_

#include <string>

#include "base.h"
#include "dynamic_base.h"
#include "dynamic_dispatch.h"

/*

interface Function<x|y> {
  call takes (x) to (y)
}

*/

class Constructor_Function;
class Instance_Function;
class Value_Function;

struct Interface_Function {
  virtual T<S<TypeValue>> Call_Function_call(const T<S<TypeValue>>&) = 0;
  virtual ~Interface_Function() = default;
};

class Constructor_Function : public ParamInstance<2>::Type {
 public:
  Constructor_Function();

  S<TypeInstance> BindAll(const ParamInstance<2>::Args& args) final;
  const CategoryId* CategoryType() const final;

  S<TypeValue> CreateValue(const S<TypeInstance>& x,
                           const S<TypeInstance>& y,
                           const S<Interface_Function>& interface);

 private:
  S<Instance_Function> BindInternal(const S<TypeInstance>& x,
                                    const S<TypeInstance>& y);

  const FunctionDispatcher<Interface_Function,FunctionScope::VALUE> value_functions_;
  InstanceCache<Instance_Function> instance_cache_;

  friend class Instance_Function;
  friend class Value_Function;
};

extern const S<Constructor_Function> Category_Function;
extern const FunctionId<FunctionScope::VALUE> Function_Function_call;

/*

interface Data<x> {
  set takes (x) to ()
  get takes () to (x)
}

*/

class Constructor_Data;
class Instance_Data;
class Value_Data;

struct Interface_Data {
  virtual T<> Call_Data_set(const T<S<TypeValue>>&) = 0;
  virtual T<S<TypeValue>> Call_Data_get(const T<>&) = 0;
  virtual ~Interface_Data() = default;
};

class Constructor_Data : public ParamInstance<1>::Type {
 public:
  Constructor_Data();

  S<TypeInstance> BindAll(const ParamInstance<1>::Args& args) final;
  const CategoryId* CategoryType() const final;

  S<TypeValue> CreateValue(const S<TypeInstance>& x,
                           const S<Interface_Data>& interface);

 private:
  S<Instance_Data> BindInternal(const S<TypeInstance>& x);

  const FunctionDispatcher<Instance_Data,FunctionScope::INSTANCE> instance_functions_;
  const FunctionDispatcher<Interface_Data,FunctionScope::VALUE> value_functions_;
  InstanceCache<Instance_Data> instance_cache_;

  friend class Instance_Data;
  friend class Value_Data;
};

extern const S<Constructor_Data> Category_Data;
extern const FunctionId<FunctionScope::VALUE> Function_Data_set;
extern const FunctionId<FunctionScope::VALUE> Function_Data_get;

/*

concrete Value {
  inherits Data<Value>
  in instance create takes () to (Value)
  log takes () to ()
}

*/

class Constructor_Value;
class Instance_Value;
class Value_Value;
class Concrete_Value;

struct Interface_Value : virtual public Interface_Data {
  using Interface_Data::Call_Data_set;
  using Interface_Data::Call_Data_get;
  virtual T<> Call_Value_log(const T<>&) = 0;
  virtual ~Interface_Value() = default;
};

class Constructor_Value : public ParamInstance<0>::Type {
 public:
  Constructor_Value();

  S<TypeInstance> BindAll(const ParamInstance<0>::Args& args) final;
  const CategoryId* CategoryType() const final;

  S<TypeValue> CreateValue(const S<Interface_Value>& interface);

 private:
  const FunctionDispatcher<Instance_Value,FunctionScope::INSTANCE> instance_functions_;
  const FunctionDispatcher<Interface_Value,FunctionScope::VALUE> value_functions_;
  const S<Instance_Value> only_instance_;

  friend class Instance_Value;
  friend class Value_Value;
  friend class Concrete_Value;
};

extern const S<Constructor_Value> Category_Value;
extern const FunctionId<FunctionScope::INSTANCE> Function_Value_create;
extern const FunctionId<FunctionScope::VALUE> Function_Value_log;

#endif  // DYNAMIC_EXAMPLE_GEN_H_
