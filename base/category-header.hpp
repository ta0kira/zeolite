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

#ifndef CATEGORY_HEADER_HPP_
#define CATEGORY_HEADER_HPP_

#include <string>

#include "types.hpp"


class TypeCategory;
class TypeInstance;
class TypeValue;
class CategoryFunction;
class TypeFunction;
class ValueFunction;

TypeInstance& Merge_Intersect(L<TypeInstance*> params);
TypeInstance& Merge_Union(L<TypeInstance*> params);

TypeInstance& GetMerged_Any();
TypeInstance& GetMerged_All();

extern const S<TypeValue>& Var_empty;

#endif  // CATEGORY_HEADER_HPP_
