#ifndef DYNAMIC_EXAMPLE_GEN_H_
#define DYNAMIC_EXAMPLE_GEN_H_

#include <string>

#include "base.h"
#include "dynamic_base.h"
#include "dynamic_router.h"

/*

interface Function<x|y> {
}

*/

extern const S<ParamInstance<2>::Type> Category_Function;

struct Interface_Function {
  virtual T<S<TypeValue>> Call_Function_call(const T<S<TypeValue>>&) = 0;
  virtual ~Interface_Function() = default;
};

extern const FunctionId<FunctionScope::VALUE> Function_Function_call;

/*

interface Data<x> {
  set takes (x) to ()
  get takes () to (x)
}

*/

extern const S<ParamInstance<1>::Type> Category_Data;

struct Interface_Data {
  virtual T<> Call_Data_set(const T<S<TypeValue>>&) = 0;
  virtual T<S<TypeValue>> Call_Data_get(const T<>&) = 0;
  virtual ~Interface_Data() = default;
};

extern const FunctionId<FunctionScope::VALUE> Function_Data_set;
extern const FunctionId<FunctionScope::VALUE> Function_Data_get;

/*

concrete Value {
  inherits Data<Value>
  static create takes () to (Value)
}

*/

extern const S<ParamInstance<0>::Type> Category_Value;

struct Interface_Value {
  virtual S<TypeValue> Convert_Data(const S<const TypeInstance>&) = 0;
  virtual T<> Call_Value_set(const T<S<TypeValue>>&) = 0;
  virtual T<S<TypeValue>> Call_Value_get(const T<>&) = 0;
  virtual ~Interface_Value() = default;
};

extern const FunctionId<FunctionScope::INSTANCE> Function_Value_create;
extern const FunctionId<FunctionScope::VALUE> Function_Value_set;
extern const FunctionId<FunctionScope::VALUE> Function_Value_get;

#endif  // DYNAMIC_EXAMPLE_GEN_H_
