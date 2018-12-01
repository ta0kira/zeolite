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
  virtual T<S<TypeValue>> Call_Reader_read() = 0;
  virtual ~Interface_Reader() = default;
};

ParamInstance<1>::Type& Category_Reader();

extern const FunctionId<MemberScope::VALUE>& Function_Reader_read;

S<TypeValue> AsReader(const S<Interface_Reader>&, TypeInstance&);

#endif  // READER_H_
