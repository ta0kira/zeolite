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
