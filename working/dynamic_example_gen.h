#ifndef DYNAMIC_EXAMPLE_GEN_H_
#define DYNAMIC_EXAMPLE_GEN_H_

#include <string>

#include "base.h"
#include "dynamic_base.h"
#include "dynamic_router.h"

/*

interface Function<x|y> {
  call takes (x) to (y)
}

*/

class Instance_Function;

struct Interface_Function {
  virtual T<S<TypeValue>> Call_Function_call(const T<S<TypeValue>>&) = 0;
  virtual ~Interface_Function() = default;
};

class Constructor_Function : public ParamInstance<2>::Type {
 public:
  S<TypeInstance> BindAll(const ParamInstance<2>::Args& args) final;
  const CategoryId* CategoryType() const final;

 private:
  S<Instance_Function> BindInternal(const S<TypeInstance>& x,
                                    const S<TypeInstance>& y);

  InstanceCache<Instance_Function> instance_cache_;
};

extern const S<Constructor_Function> Category_Function;
extern const FunctionId<FunctionScope::VALUE> Function_Function_call;

/*

interface Data<x> {
  set takes (x) to ()
  get takes () to (x)
}

*/

class Instance_Data;

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

  S<TypeValue> Create_Value(const S<TypeInstance>& x,
                            const S<Interface_Data>& interface);

 private:
  S<Instance_Data> BindInternal(const S<TypeInstance>& x);

  const FunctionRouter<Instance_Data,FunctionScope::INSTANCE> instance_functions_;
  const FunctionRouter<Interface_Data,FunctionScope::VALUE> value_functions_;
  InstanceCache<Instance_Data> instance_cache_;

  friend class Value_Data;
};

extern const S<Constructor_Data> Category_Data;
extern const FunctionId<FunctionScope::VALUE> Function_Data_set;
extern const FunctionId<FunctionScope::VALUE> Function_Data_get;

/*

concrete Value {
  inherits Data<Value>
  in instance create takes () to (Value)
}

*/

class Instance_Value;

struct Interface_Value {
  virtual T<> Call_Value_set(const T<S<TypeValue>>&) = 0;
  virtual T<S<TypeValue>> Call_Value_get(const T<>&) = 0;
  virtual ~Interface_Value() = default;
};

class Constructor_Value : public ParamInstance<0>::Type {
 public:
  Constructor_Value();
  S<TypeInstance> BindAll(const ParamInstance<0>::Args& args) final;
  const CategoryId* CategoryType() const final;

 private:
  const FunctionRouter<Instance_Value,FunctionScope::INSTANCE> instance_functions_;
  const FunctionRouter<Interface_Value,FunctionScope::VALUE> value_functions_;
  const S<Instance_Value> only_instance_;

  friend class Instance_Value;
  friend class Value_Value;
};

extern const S<Constructor_Value> Category_Value;
extern const FunctionId<FunctionScope::INSTANCE> Function_Value_create;
extern const FunctionId<FunctionScope::VALUE> Function_Value_set;
extern const FunctionId<FunctionScope::VALUE> Function_Value_get;

#endif  // DYNAMIC_EXAMPLE_GEN_H_
