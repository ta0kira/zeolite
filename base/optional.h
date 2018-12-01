#ifndef OPTIONAL_H_
#define OPTIONAL_H_

#include "category.h"
#include "constructor.h"
#include "core.h"

ParamInstance<1>::Type& Category_Optional();

extern const FunctionId<MemberScope::VALUE>& Function_Optional_present;
extern const FunctionId<MemberScope::VALUE>& Function_Optional_require;

S<TypeValue> AsOptional(const S<TypeValue>&, TypeInstance&);
S<TypeValue> SkipOptional(TypeInstance&);

#endif  // OPTIONAL_H_
