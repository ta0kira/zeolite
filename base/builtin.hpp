#ifndef BUILTIN_HPP_
#define BUILTIN_HPP_

#include "category-header.hpp"


extern const void* const Functions_LessThan;
extern const Function<SymbolScope::TYPE,0,2,1>& Function_LessThan_lessThan;

extern const void* const Functions_Equals;
extern const Function<SymbolScope::TYPE,0,2,1>& Function_Equals_equals;

extern const void* const Functions_Formatted;
extern const Function<SymbolScope::VALUE,0,0,1>& Function_Formatted_formatted;

TypeInstance& Merge_Intersect(L<TypeInstance*> params);
TypeInstance& Merge_Union(L<TypeInstance*> params);

TypeCategory& GetCategory_Bool();
TypeCategory& GetCategory_String();
TypeCategory& GetCategory_Int();
TypeCategory& GetCategory_Float();
TypeCategory& GetCategory_Formatted();

TypeInstance& GetType_Bool(Params<0>::Type);
TypeInstance& GetType_String(Params<0>::Type);
TypeInstance& GetType_Int(Params<0>::Type);
TypeInstance& GetType_Float(Params<0>::Type);
TypeInstance& GetType_Formatted(Params<0>::Type);

S<TypeValue> Box_Bool(bool value);
S<TypeValue> Box_String(std::string value);
S<TypeValue> Box_Int(int value);
S<TypeValue> Box_Float(double value);

extern const S<TypeValue>& Var_empty;
extern const S<TypeValue>& Var_true;
extern const S<TypeValue>& Var_false;

#endif  // BUILTIN_HPP_
