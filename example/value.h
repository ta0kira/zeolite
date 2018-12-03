#ifndef VALUE_H_
#define VALUE_H_

#include "base/category.h"
#include "base/constructor.h"
#include "base/core.h"

#include "printable.h"

/*

concrete Value {
  refines Printable

  with instance
  <x>
  create takes (x) to (Value)

  with instance
  show takes (Value) to ()
}

*/

ParamInstance<0>::Type& Category_Value();

extern const FunctionId<MemberScope::INSTANCE>& Function_Value_create;
extern const FunctionId<MemberScope::INSTANCE>& Function_Value_show;

#endif  // VALUE_H_
