#include "dynamic_base.h"

FunctionReturns TypeInstance::CallInstanceFunction(
    const FunctionId<FunctionScope::INSTANCE>& id, const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-category " << TypeName();
  return FunctionReturns();
}

S<TypeValue> TypeValue::ConvertTo(const CategoryId* category) {
  FAIL() << "Cannot convert " << InstanceType()->TypeName()
         << " to " << category->TypeName();
  return nullptr;
}

S<TypeValue> TypeValue::ConvertTo(const S<TypeValue>& self,
                                  const CategoryId* category) {
  if (category == self->InstanceType()->CategoryType()) {
    return self;
  } else {
    return self->ConvertTo(category);
  }
}

FunctionReturns TypeValue::CallValueFunction(
    const FunctionId<FunctionScope::VALUE>& id, const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-instance " << InstanceType()->TypeName();
  return FunctionReturns();
}
