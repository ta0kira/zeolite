#ifndef WRITER_H_
#define WRITER_H_

#include "base/category.h"
#include "base/constructor.h"
#include "base/core.h"

/*

interface Writer<x|> {
  write takes (x) to ()
}

*/

struct Interface_Writer {
  virtual T<> Call_Writer_write(const T<S<TypeValue>>&) = 0;
  virtual ~Interface_Writer() = default;
};

extern ParamInstance<1>::Type& Category_Writer;
extern const FunctionId<MemberScope::VALUE>& Function_Writer_write;

#endif  // WRITER_H_
