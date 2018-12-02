#ifndef OPTIONAL_H_
#define OPTIONAL_H_

#include "category.h"
#include "constructor.h"
#include "core.h"

ParamInstance<1>::Type& Category_Optional();

// TODO: Since we can convert x to optional x, that means x needs to support
// present and require. For example, we need to be able to call require on
// something like (x|optional y).
extern const FunctionId<MemberScope::VALUE>& Function_Optional_present;
extern const FunctionId<MemberScope::VALUE>& Function_Optional_require;

S<TypeValue> As_Optional(const S<TypeValue>&, TypeInstance&);
S<TypeValue> Skip_Optional(TypeInstance&);

#endif  // OPTIONAL_H_
