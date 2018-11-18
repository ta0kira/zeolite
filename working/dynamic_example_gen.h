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

/*

interface Data<x> {
}

*/

extern const S<ParamInstance<1>::Type> Data;

/*

interface Value {
  inherits Data<Value>
}

*/

extern const S<ParamInstance<0>::Type> Value;
extern const FunctionId& Value_create;

#endif  // DYNAMIC_EXAMPLE_GEN_H_
