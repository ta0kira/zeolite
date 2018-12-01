#ifndef OPTIONAL_H_
#define OPTIONAL_H_

#include "category.h"
#include "category_base.h"
#include "constructor.h"
#include "core.h"

extern ParamInstance<1>::Type& Category_Optional;

extern const CategoryId& Id_Optional;
extern const FunctionId<MemberScope::VALUE>& Function_Optional_present;
extern const FunctionId<MemberScope::VALUE>& Function_Optional_require;

S<TypeValue> AsOptional(const TypeInstance&, const S<TypeValue>&);
S<TypeValue> SkipOptional(const TypeInstance&);

#endif  // OPTIONAL_H_
