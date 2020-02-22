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

extern const void* const Functions_AsBool;
extern const ValueFunction& Function_AsBool_asBool;

extern const void* const Functions_AsChar;
extern const ValueFunction& Function_AsChar_asChar;

extern const void* const Functions_AsInt;
extern const ValueFunction& Function_AsInt_asInt;

extern const void* const Functions_AsFloat;
extern const ValueFunction& Function_AsFloat_asFloat;

extern const void* const Functions_LessThan;
extern const TypeFunction& Function_LessThan_lessThan;

extern const void* const Functions_Equals;
extern const TypeFunction& Function_Equals_equals;

extern const void* const Functions_Formatted;
extern const ValueFunction& Function_Formatted_formatted;

extern const void* const Functions_ReadPosition;
extern const ValueFunction& Function_ReadPosition_readPosition;
extern const ValueFunction& Function_ReadPosition_readSize;
extern const ValueFunction& Function_ReadPosition_subSequence;

extern const void* const Functions_String;
extern const ValueFunction& Function_String_subSequence;

TypeInstance& Merge_Intersect(L<TypeInstance*> params);
TypeInstance& Merge_Union(L<TypeInstance*> params);

TypeInstance& GetMerged_Any();
TypeInstance& GetMerged_All();

TypeCategory& GetCategory_Bool();
TypeCategory& GetCategory_Char();
TypeCategory& GetCategory_Int();
TypeCategory& GetCategory_Float();
TypeCategory& GetCategory_String();
TypeCategory& GetCategory_AsBool();
TypeCategory& GetCategory_AsChar();
TypeCategory& GetCategory_AsInt();
TypeCategory& GetCategory_AsFloat();
TypeCategory& GetCategory_AsString();
TypeCategory& GetCategory_Formatted();
TypeCategory& GetCategory_ReadPosition();
TypeCategory& GetCategory_LessThan();
TypeCategory& GetCategory_Equals();

TypeInstance& GetType_Bool(Params<0>::Type);
TypeInstance& GetType_Char(Params<0>::Type);
TypeInstance& GetType_Int(Params<0>::Type);
TypeInstance& GetType_Float(Params<0>::Type);
TypeInstance& GetType_String(Params<0>::Type);
TypeInstance& GetType_AsBool(Params<0>::Type);
TypeInstance& GetType_AsString(Params<0>::Type);
TypeInstance& GetType_AsChar(Params<0>::Type);
TypeInstance& GetType_AsInt(Params<0>::Type);
TypeInstance& GetType_AsFloat(Params<0>::Type);
TypeInstance& GetType_Formatted(Params<0>::Type);
TypeInstance& GetType_ReadPosition(Params<1>::Type);
TypeInstance& GetType_LessThan(Params<1>::Type params);
TypeInstance& GetType_Equals(Params<1>::Type params);

S<TypeValue> Box_Bool(bool value);
S<TypeValue> Box_String(const PrimString& value);
S<TypeValue> Box_Char(PrimChar value);
S<TypeValue> Box_Int(PrimInt value);
S<TypeValue> Box_Float(PrimFloat value);

extern const S<TypeValue>& Var_empty;

#endif  // BUILTIN_HPP_
