#ifndef BUILTIN_HXX_
#define BUILTIN_HXX_

#include "category-header.hpp"


TypeInstance& Merge_Intersect(L<TypeInstance*> params);
TypeInstance& Merge_Union(L<TypeInstance*> params);

TypeCategory& GetCategory_Bool();
TypeCategory& GetCategory_String();
TypeCategory& GetCategory_Int();
TypeCategory& GetCategory_Float();

TypeInstance& GetType_Bool();
TypeInstance& GetType_String();
TypeInstance& GetType_Int();
TypeInstance& GetType_Float();

extern const S<TypeValue>& Var_empty;
extern const S<TypeValue>& Var_true;
extern const S<TypeValue>& Var_false;

#endif  // BUILTIN_HXX_
