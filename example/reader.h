#ifndef READER_H_
#define READER_H_

#include "base/category.h"
#include "base/constructor.h"
#include "base/core.h"

/*

interface Reader<|x> {
  read takes () to (optional x)
}

*/

struct Interface_Reader {
  virtual T<S<TypeValue>> Call_Reader_read(const T<>&) = 0;
  virtual ~Interface_Reader() = default;
};

extern ParamInstance<1>::Type& Category_Reader;
extern const FunctionId<MemberScope::VALUE>& Function_Reader_read;

#endif  // READER_H_
