#ifndef STRING_H_
#define STRING_H_

#include <string>

#include "category.h"
#include "constructor.h"
#include "core.h"

ParamInstance<0>::Type& Category_String();

S<TypeValue> As_String(const std::string&);

#endif  // STRING_H_
