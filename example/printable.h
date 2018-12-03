#ifndef PRINTABLE_H_
#define PRINTABLE_H_

#include "base/category.h"
#include "base/constructor.h"
#include "base/core.h"

/*

interface Printable {
  print takes () to ()
}

*/

struct Interface_Printable {
  virtual ParamReturns<0>::Type Call_Printable_print(ParamTypes<0>::Type, ParamArgs<0>::Type) = 0;
  virtual ~Interface_Printable() = default;
};

ParamInstance<0>::Type& Category_Printable();

extern const FunctionId<MemberScope::VALUE>& Function_Printable_print;

S<TypeValue> As_Printable(const S<Interface_Printable>&);

#endif  // PRINTABLE_H_
