#include "dynamic_base.h"

const TypeArgs& TypeInstance::TypeArgsForCategory(const CategoryId* id) const {
  FAIL() << "Category " << id->TypeName()
         << " is not a base of type-instance " << TypeName();
}

FunctionReturns TypeInstance::CallInstanceFunction(
    const FunctionId<MemberScope::INSTANCE>& id, const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-instance " << TypeName();
}

bool TypeInstance::CheckConversionBetween(const TypeInstance* from, const TypeInstance* to) {
  bool can_convert = from->MergedType() != MergeType::INTERSECT;
  for (const TypeInstance* left : from->MergedInstanceTypes()) {
    bool can_convert_to = to->MergedType() != MergeType::UNION;
    for (const TypeInstance* right : to->MergedInstanceTypes()) {
      bool can_convert_single = left->CheckConversionTo(right);
      switch (to->MergedType()) {
        case MergeType::SINGLE:
          can_convert_to = can_convert_single;
          break;
        case MergeType::UNION:
          can_convert_to |= can_convert_single;
          break;
        case MergeType::INTERSECT:
          can_convert_to &= can_convert_single;
          break;
      }
      if ((from->MergedType() == MergeType::UNION     && can_convert_to) ||
          (from->MergedType() == MergeType::INTERSECT && !can_convert_to)) {
        break;
      }
    }
    switch (from->MergedType()) {
      case MergeType::SINGLE:
        can_convert = can_convert_to;
        break;
      case MergeType::UNION:
        can_convert &= can_convert_to;
        break;
      case MergeType::INTERSECT:
        can_convert |= can_convert_to;
        break;
    }
    if ((from->MergedType() == MergeType::UNION     && !can_convert) ||
        (from->MergedType() == MergeType::INTERSECT && can_convert)) {
      break;
    }
  }
  return can_convert;
}

bool TypeInstance::CheckConversionTo(const TypeInstance*) const {
  return false;
}

MergeType TypeInstance::MergedType() const {
  return MergeType::SINGLE;
}

std::vector<const TypeInstance*> TypeInstance::MergedInstanceTypes() const {
  return std::vector<const TypeInstance*>{this};
}

FunctionReturns TypeValue::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id, const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-value " << InstanceType()->TypeName();
}

ValueVariable& TypeValue::GetValueVariable(const ValueVariableId& id) {
  FAIL() << "Member variable " << id.ValueName()
         << " not supported in type-value " << InstanceType()->TypeName();
}

S<TypeInstance> TypeValue::GetTypeVariable(const TypeVariableId& id) {
  FAIL() << "Member type " << id.TypeName()
         << " not supported in type-value " << InstanceType()->TypeName();
}

bool TypeValue::IsOptional() const {
  return false;
}

S<TypeValue> TypeValue::ConvertTo(const S<TypeValue>& self,
                                  const S<TypeInstance>& instance) {
  if (instance.get() == self->InstanceType()) {
    return self;
  } else {
    return self->ConvertTo(instance);
  }
}

S<TypeValue> TypeValue::ReduceTo(const S<TypeValue>& self,
                                 const S<TypeInstance>& instance) {
  if (instance.get() == self->InstanceType()) {
    return self;
  }
  if (TypeInstance::CheckConversionBetween(self->InstanceType(), instance.get())) {
    // TODO: Wrap with optional.
    return ConvertTo(self, instance);
  } else {
    // return `skip` (optional x)
  }
}
