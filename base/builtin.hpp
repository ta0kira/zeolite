/* -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- */

// Author: Kevin P. Barry [ta0kira@gmail.com]

#ifndef BUILTIN_HPP_
#define BUILTIN_HPP_

#include "category-header.hpp"


void BuiltinFail(const S<TypeValue>& formatted) __attribute__ ((noreturn));

extern const void* const Functions_LessThan;
extern const Function<SymbolScope::TYPE,0,2,1>& Function_LessThan_lessThan;

extern const void* const Functions_Equals;
extern const Function<SymbolScope::TYPE,0,2,1>& Function_Equals_equals;

extern const void* const Functions_Formatted;
extern const Function<SymbolScope::VALUE,0,0,1>& Function_Formatted_formatted;

TypeInstance& Merge_Intersect(L<TypeInstance*> params);
TypeInstance& Merge_Union(L<TypeInstance*> params);

TypeInstance& GetMerged_Any();
TypeInstance& GetMerged_All();

TypeCategory& GetCategory_Bool();
TypeCategory& GetCategory_String();
TypeCategory& GetCategory_Int();
TypeCategory& GetCategory_Float();
TypeCategory& GetCategory_Formatted();
TypeCategory& GetCategory_LessThan();
TypeCategory& GetCategory_Equals();

TypeInstance& GetType_Bool(Params<0>::Type);
TypeInstance& GetType_String(Params<0>::Type);
TypeInstance& GetType_Int(Params<0>::Type);
TypeInstance& GetType_Float(Params<0>::Type);
TypeInstance& GetType_Formatted(Params<0>::Type);
TypeInstance& GetType_LessThan(Params<1>::Type params);
TypeInstance& GetType_Equals(Params<1>::Type params);

S<TypeValue> Box_Bool(bool value);
S<TypeValue> Box_String(const PrimString& value);
S<TypeValue> Box_Int(PrimInt value);
S<TypeValue> Box_Float(PrimFloat value);

extern const S<TypeValue>& Var_empty;

#endif  // BUILTIN_HPP_
