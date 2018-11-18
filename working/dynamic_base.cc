#include "dynamic_base.h"

FunctionReturns TypeInstance::CallStaticFunction(const FunctionId& id, const FunctionArgs&) const {
  FAIL() << "Static function " << id.FunctionName()
          << " not supported in " << TypeName();
  return FunctionReturns();
}

S<TypeValue> TypeValue::ConvertTo(const S<const TypeInstance>& type) {
  FAIL() << "Cannot convert " << ValueType()->TypeName()
          << " to " << type->TypeName();
  return nullptr;
}

FunctionReturns TypeValue::CallInstanceFunction(const FunctionId& id, const FunctionArgs&) {
  FAIL() << "Instance function " << id.FunctionName()
          << " not supported in " << ValueType()->TypeName();
  return FunctionReturns();
}
