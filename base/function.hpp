#ifndef FUNCTION_HPP_
#define FUNCTION_HPP_

#include "types.hpp"


template<SymbolScope S> class DFunction {};

template<>
class DFunction<SymbolScope::CategoryScope> {
 public:
  inline DFunction(std::string category, std::string function) :
      category_(category), function_(function) {}

  std::string FunctionName() const;

 private:
  ALWAYS_PERMANENT(DFunction)
  const std::string category_;
  const std::string function_;
};

template<>
class DFunction<SymbolScope::TypeScope> {
 public:
  inline DFunction(std::string category, std::string function) :
      category_(category), function_(function) {}

  std::string FunctionName() const;

 private:
  ALWAYS_PERMANENT(DFunction)
  const std::string category_;
  const std::string function_;
};

template<>
class DFunction<SymbolScope::ValueScope> {
 public:
  inline DFunction(std::string category, std::string function) :
      category_(category), function_(function) {}

  std::string FunctionName() const;

 private:
  ALWAYS_PERMANENT(DFunction)
  const std::string category_;
  const std::string function_;
};


template<SymbolScope S, int P, int A, int R> class Function {};

template<int P, int A, int R>
class Function<SymbolScope::CategoryScope,P,A,R> : public DFunction<SymbolScope::CategoryScope> {
 public:
  using DFunction::DFunction;
};

template<int P, int A, int R>
class Function<SymbolScope::TypeScope,P,A,R> : public DFunction<SymbolScope::TypeScope> {
 public:
  using DFunction::DFunction;
};

template<int P, int A, int R>
class Function<SymbolScope::ValueScope,P,A,R> : public DFunction<SymbolScope::ValueScope> {
 public:
  using DFunction::DFunction;
};

#endif  // FUNCTION_HPP_
