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

bool TypeInstance::CanConvert(TypeInstance& from, TypeInstance& to) {
  // TODO: Implement this.
  return false;
}

Returns<1>::Type TypeValue::Present(S<TypeValue> target) {
  return target->Present()? T_get(Var_true) : T_get(Var_false);
}

Returns<1>::Type TypeValue::Require(S<TypeValue> target) {
  if (!target->Present()) {
    FAIL() << "Cannot require empty value";
  }
  return T_get(target);
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
