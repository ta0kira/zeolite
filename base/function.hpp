/* -----------------------------------------------------------------------------
Copyright 2019 Kevin P. Barry

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

#include "types.hpp"

#include <sstream>


template<SymbolScope S> class DFunction {
 public:
  inline DFunction(std::string category, std::string function, const void* collection, int n) :
      category_(category), function_(function), collection_(collection), n_(n) {}

  inline int Num() const { return n_; }
  inline const void* Collection() const { return collection_; }
  virtual int ParamCount() const = 0;
  virtual int ArgCount() const = 0;
  virtual int ReturnCount() const = 0;

  std::string FunctionName() const;

 private:
  ALWAYS_PERMANENT(DFunction)
  const void* const collection_;
  const int n_;
  const std::string category_;
  const std::string function_;
};


template<SymbolScope S, int P, int A, int R> class Function : public DFunction<S> {
 public:
  int ParamCount() const final { return P; }
  int ArgCount() const final { return A; }
  int ReturnCount() const final { return R; }
  using DFunction<S>::DFunction;
};

template<SymbolScope S>
std::string DFunction<S>::FunctionName() const {
  std::ostringstream output;
  switch (S) {
    case SymbolScope::CATEGORY:
      output << category_ << "$$" << function_;
      break;
    case SymbolScope::TYPE:
    case SymbolScope::VALUE:
      output << category_ << "$" << function_;
      break;
  }
  return output.str();
}

#endif  // FUNCTION_HPP_
