#ifndef FUNCTION_HPP_
#define FUNCTION_HPP_

#include "types.hpp"


template<SymbolScope S> class DFunction {};

template<>
class DFunction<SymbolScope::CATEGORY> {
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
class DFunction<SymbolScope::TYPE> {
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
class DFunction<SymbolScope::VALUE> {
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
class Function<SymbolScope::CATEGORY,P,A,R> : public DFunction<SymbolScope::CATEGORY> {
 public:
  using DFunction::DFunction;
};

template<int P, int A, int R>
class Function<SymbolScope::TYPE,P,A,R> : public DFunction<SymbolScope::TYPE> {
 public:
  using DFunction::DFunction;
};

template<int P, int A, int R>
class Function<SymbolScope::VALUE,P,A,R> : public DFunction<SymbolScope::VALUE> {
 public:
  using DFunction::DFunction;
};

#endif  // FUNCTION_HPP_
