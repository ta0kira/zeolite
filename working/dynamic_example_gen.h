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

class Constructor_Function;
extern const S<Constructor_Function> Category_Function;

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

class Constructor_Data;
extern const S<Constructor_Data> Category_Data;

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

class Constructor_Value;
extern const S<Constructor_Value> Category_Value;

struct Interface_Value {
  virtual T<> Call_Value_set(const T<S<TypeValue>>&) = 0;
  virtual T<S<TypeValue>> Call_Value_get(const T<>&) = 0;
  virtual ~Interface_Value() = default;
};

extern const FunctionId<FunctionScope::INSTANCE> Function_Value_create;
extern const FunctionId<FunctionScope::VALUE> Function_Value_set;
extern const FunctionId<FunctionScope::VALUE> Function_Value_get;

#endif  // DYNAMIC_EXAMPLE_GEN_H_
