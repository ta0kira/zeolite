#include "function.hpp"

#include <sstream>


std::string DFunction<SymbolScope::CategoryScope>::FunctionName() const {
  std::ostringstream output;
  output << category_ << "$$" << function_;
  return output.str();
}

std::string DFunction<SymbolScope::TypeScope>::FunctionName() const {
  std::ostringstream output;
  output << category_ << "$" << function_;
  return output.str();
}

std::string DFunction<SymbolScope::ValueScope>::FunctionName() const {
  std::ostringstream output;
  output << category_ << "$" << function_;
  return output.str();
}
