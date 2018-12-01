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

struct Interface_Queue
    : virtual public Interface_Reader,
      virtual public Interface_Writer {
  using Interface_Reader::Call_Reader_read;
  using Interface_Writer::Call_Writer_write;
  virtual ~Interface_Queue() = default;
};

extern ParamInstance<1>::Type& Category_Queue;
extern const FunctionId<MemberScope::INSTANCE>& Function_Queue_create;

#endif  // QUEUE_H_
