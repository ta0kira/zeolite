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
  create takes (String) to (Value)
}

*/

ParamInstance<0>::Type& Category_Value();

extern const FunctionId<MemberScope::INSTANCE>& Function_Value_create;

#endif  // VALUE_H_
