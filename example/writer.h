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
  virtual ParamReturns<0>::Type Call_Writer_write(ParamTypes<0>::Type, ParamArgs<1>::Type) = 0;
  virtual ~Interface_Writer() = default;
};

ParamInstance<1>::Type& Category_Writer();

extern const FunctionId<MemberScope::VALUE>& Function_Writer_write;

S<TypeValue> As_Writer(const S<Interface_Writer>&, TypeInstance&);

#endif  // WRITER_H_
