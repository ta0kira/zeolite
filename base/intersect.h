#ifndef INTERSECT_H_
#define INTERSECT_H_

#include "category.h"
#include "core.h"

TypeCategory& Category_Intersect();

TypeInstance& Intersect_Any();
TypeInstance& Build_Intersect(const TypeArgs&);
S<TypeValue> As_Intersect(const S<TypeValue>&, const TypeArgs&);

#endif  // INTERSECT_H_
