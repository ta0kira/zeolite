#include "dynamic_base.h"

FunctionReturns TypeInstance::CallInstanceFunction(
    const FunctionId<FunctionScope::INSTANCE>& id, const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-category " << TypeName();
  return FunctionReturns();
}

S<TypeValue> TypeValue::ConvertTo(const S<const TypeInstance>& type) {
  FAIL() << "Cannot convert " << ValueType()->TypeName()
         << " to " << type->TypeName();
  return nullptr;
}

S<TypeValue> TypeValue::ConvertTo(const S<TypeValue>& self,
                                  const S<const TypeInstance>& type) {
  if (type.get() == self->ValueType()) {
    return self;
  } else {
    return self->ConvertTo(type);
  }
}

FunctionReturns TypeValue::CallValueFunction(
    const FunctionId<FunctionScope::VALUE>& id, const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-instance " << ValueType()->TypeName();
  return FunctionReturns();
}
