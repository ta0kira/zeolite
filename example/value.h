#ifndef VALUE_H_
#define VALUE_H_

#include "base/category.h"
#include "base/constructor.h"
#include "base/core.h"

#include "printable.h"
#include "viewer.h"

/*

concrete Value {
  refines Printable
  defines Viewer<Value>

  with instance
  <x>
  create takes (x) to (Value)
}

*/

ParamInstance<0>::Type& Category_Value();

extern const FunctionId<MemberScope::INSTANCE>& Function_Value_create;

#endif  // VALUE_H_
