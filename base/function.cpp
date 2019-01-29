#include "function.hpp"

#include <sstream>


std::string DFunction<SymbolScope::CATEGORY>::FunctionName() const {
  std::ostringstream output;
  output << category_ << "$$" << function_;
  return output.str();
}

std::string DFunction<SymbolScope::TYPE>::FunctionName() const {
  std::ostringstream output;
  output << category_ << "$" << function_;
  return output.str();
}

std::string DFunction<SymbolScope::VALUE>::FunctionName() const {
  std::ostringstream output;
  output << category_ << "$" << function_;
  return output.str();
}
