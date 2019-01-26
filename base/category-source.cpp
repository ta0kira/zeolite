#include "category-source.hpp"

#include "logging.hpp"
#include "builtin.hpp"


DReturns TypeCategory::Dispatch(const DFunction<SymbolScope::CategoryScope>& label,
                                DParams params, DArgs args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return DReturns();
}

DReturns TypeInstance::Dispatch(const DFunction<SymbolScope::TypeScope>& label,
                                DParams params, DArgs args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return DReturns();
}

DReturns TypeValue::Dispatch(const S<TypeValue>& self,
                             const DFunction<SymbolScope::ValueScope>& label,
                             DParams params, DArgs args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return DReturns();
}

S<TypeValue> TypeInstance::Reduce(TypeInstance& from, TypeInstance& to, S<TypeValue> target) {
  // TODO: Implement this.
  return Var_empty;
}

bool TypeValue::AsBool() const {
  FAIL() << "Not a Bool value";
  return false;
}

std::string TypeValue::AsString() const {
  FAIL() << "Not a String value";
  return "";
}

int TypeValue::AsInt() const {
  FAIL() << "Not an Int value";
  return 0;
}

double TypeValue::AsFloat() const {
  FAIL() << "Not a Float value";
  return 0.0;
}

bool TypeValue::Present() const {
  return true;
}
