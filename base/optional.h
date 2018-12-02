#ifndef OPTIONAL_H_
#define OPTIONAL_H_

#include "category.h"
#include "constructor.h"
#include "core.h"

ParamInstance<1>::Type& Category_Optional();

S<TypeValue> Optional_Skip();
S<TypeValue> As_Optional(const S<TypeValue>&, TypeInstance&);

#endif  // OPTIONAL_H_
