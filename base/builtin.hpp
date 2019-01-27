#ifndef BUILTIN_HPP_
#define BUILTIN_HPP_

#include "category-header.hpp"


TypeInstance& Merge_Intersect(L<TypeInstance*> params);
TypeInstance& Merge_Union(L<TypeInstance*> params);

TypeCategory& GetCategory_Bool();
TypeCategory& GetCategory_String();
TypeCategory& GetCategory_Int();
TypeCategory& GetCategory_Float();

TypeInstance& GetType_Bool(Params<0>::Type);
TypeInstance& GetType_String(Params<0>::Type);
TypeInstance& GetType_Int(Params<0>::Type);
TypeInstance& GetType_Float(Params<0>::Type);

extern const S<TypeValue>& Var_empty;
extern const S<TypeValue>& Var_true;
extern const S<TypeValue>& Var_false;

#endif  // BUILTIN_HPP_
