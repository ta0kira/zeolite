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

#ifndef FUNCTION_HPP_
#define FUNCTION_HPP_

#include <ostream>


#define COLLECTION_ID(vp) (int) 1000000009 * (long) (vp)

using CollectionType = int;

struct CategoryFunction {
  const int param_count;
  const int arg_count;
  const int return_count;
  const char* const category;
  const char* const function;
  const CollectionType collection;
  const int function_num;
};

struct TypeFunction {
  const int param_count;
  const int arg_count;
  const int return_count;
  const char* const category;
  const char* const function;
  const CollectionType collection;
  const int function_num;
};

struct ValueFunction {
  const int param_count;
  const int arg_count;
  const int return_count;
  const char* const category;
  const char* const function;
  const CollectionType collection;
  const int function_num;
};

inline std::ostream& operator << (std::ostream& output, const CategoryFunction& func) {
  return output << func.category << "." << func.function;
}

inline std::ostream& operator << (std::ostream& output, const TypeFunction& func) {
  return output << func.category << "." << func.function;
}

inline std::ostream& operator << (std::ostream& output, const ValueFunction& func) {
  return output << func.category << "." << func.function;
}

#endif  // FUNCTION_HPP_
