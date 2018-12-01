#ifndef VALUE_H_
#define VALUE_H_

#include "base/category.h"
#include "base/constructor.h"
#include "base/core.h"

/*

concrete Value {
  // TODO: Maybe make this take a String.
  with instance
  create takes () to (Value)

  print takes () to ()
}

*/

struct Interface_Value {
  virtual T<> Call_Value_print(const T<>&) = 0;
  virtual ~Interface_Value() = default;
};

extern ParamInstance<0>::Type& Category_Value;

extern const FunctionId<MemberScope::INSTANCE>& Function_Value_create;
extern const FunctionId<MemberScope::VALUE>& Function_Value_print;

#endif  // VALUE_H_
