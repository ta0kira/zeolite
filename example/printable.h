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
  virtual T<> Call_Printable_print() = 0;
  virtual ~Interface_Printable() = default;
};

ParamInstance<0>::Type& Category_Printable();

extern const FunctionId<MemberScope::VALUE>& Function_Printable_print;

S<TypeValue> As_Printable(const S<Interface_Printable>&);

#endif  // PRINTABLE_H_
