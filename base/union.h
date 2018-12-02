#ifndef UNION_H_
#define UNION_H_

#include "category.h"
#include "core.h"

TypeCategory& Category_Union();

TypeInstance& Union_All();
TypeInstance& Build_Union(const TypeArgs&);
S<TypeValue> As_Union(const S<TypeValue>&, const TypeArgs&);

#endif  // UNION_H_
