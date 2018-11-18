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

extern const S<ParamInstance<2>::Type> Function;

struct Interface_Function {
  virtual T<S<TypeValue>> call(const T<S<TypeValue>>&) = 0;
  virtual ~Interface_Function() = default;
};

extern const FunctionId Function_call;

/*

interface Data<x> {
  set takes (x) to ()
  get takes () to (x)
}

*/

extern const S<ParamInstance<1>::Type> Data;

struct Interface_Data {
  virtual T<> set(const T<S<TypeValue>>&) = 0;
  virtual T<S<TypeValue>> get(const T<>&) = 0;
  virtual ~Interface_Data() = default;
};

extern const FunctionId Data_set;
extern const FunctionId Data_get;

/*

concrete Value {
  inherits Data<Value>
  static create takes () to (Value)
}

*/

extern const S<ParamInstance<0>::Type> Value;
extern const FunctionId Value_create;

struct Interface_Value : public Interface_Data {
  virtual ~Interface_Value() = default;
};

extern const FunctionId Value_set;
extern const FunctionId Value_get;

#endif  // DYNAMIC_EXAMPLE_GEN_H_
