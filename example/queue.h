#ifndef QUEUE_H_
#define QUEUE_H_

#include "base/category.h"
#include "base/constructor.h"
#include "base/core.h"

#include "reader.h"
#include "writer.h"

/*

concrete Queue<x> {
  refines Writer<x>
  refines Reader<x>

  with instance
  create takes () to (Queue<x>)
}

*/

extern ParamInstance<1>::Type& Category_Queue;

extern const FunctionId<MemberScope::INSTANCE>& Function_Queue_create;

#endif  // QUEUE_H_
