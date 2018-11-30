#include "category.h"

FunctionReturns TypeCategory::CallCategoryFunction(
    const FunctionId<MemberScope::CATEGORY>& id, const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
          << " not supported in type-value " << CategoryType()->TypeName();
  return FunctionReturns();
}


const TypeArgs& TypeInstance::TypeArgsForCategory(const CategoryId* id) const {
  FAIL() << "Category " << id->TypeName()
         << " is not a base of type-instance " << TypeName();
  static const TypeArgs failed;
  return failed;
}

FunctionReturns TypeInstance::CallInstanceFunction(
    const FunctionId<MemberScope::INSTANCE>& id, const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-instance " << TypeName();
  return FunctionReturns();
}

bool TypeInstance::CheckConversionBetween(
    const TypeInstance* from, const TypeInstance* to) {
  bool can_convert = from->InstanceMergeType() != MergeType::INTERSECT;
  for (const TypeInstance* left : from->MergedInstanceTypes()) {
    bool can_convert_to = to->InstanceMergeType() != MergeType::UNION;
    for (const TypeInstance* right : to->MergedInstanceTypes()) {
      bool can_convert_single = left->CheckConversionTo(right);
      switch (to->InstanceMergeType()) {
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
      if ((from->InstanceMergeType() == MergeType::UNION     && can_convert_to) ||
          (from->InstanceMergeType() == MergeType::INTERSECT && !can_convert_to)) {
        break;
      }
    }
    switch (from->InstanceMergeType()) {
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
    if ((from->InstanceMergeType() == MergeType::UNION     && !can_convert) ||
        (from->InstanceMergeType() == MergeType::INTERSECT && can_convert)) {
      break;
    }
  }
  return can_convert;
}

bool TypeInstance::CheckConversionTo(const TypeInstance*) const {
  return false;
}

MergeType TypeInstance::InstanceMergeType() const {
  return MergeType::SINGLE;
}

std::vector<const TypeInstance*> TypeInstance::MergedInstanceTypes() const {
  return std::vector<const TypeInstance*>{this};
}


FunctionReturns TypeValue::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id, const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-value " << InstanceType()->TypeName();
  return FunctionReturns();
}

ValueVariable& TypeValue::GetValueVariable(
    const ValueVariableId<MemberScope::VALUE>& id) {
  FAIL() << "Member variable " << id.VariableName()
         << " not supported in type-value " << InstanceType()->TypeName();
  static ValueVariable failed;
  return failed;
}

S<TypeInstance> TypeValue::GetTypeVariable(
    const TypeVariableId<MemberScope::VALUE>& id) {
  FAIL() << "Member type " << id.VariableName()
         << " not supported in type-value " << InstanceType()->TypeName();
  return nullptr;
}

bool TypeValue::IsOptional() const {
  return false;
}

S<TypeValue> TypeValue::RequireValue() {
  FAIL() << "Cannot use require with type-value " << InstanceType()->TypeName();
  return nullptr;
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
    // TODO: Wrap with optional.
    return self;
  }
  if (TypeInstance::CheckConversionBetween(self->InstanceType(), instance.get())) {
    // TODO: Wrap with optional.
    return ConvertTo(self, instance);
  } else {
    // TODO: return `skip` (optional x)
    return nullptr;
  }
}
