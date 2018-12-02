#include "category.h"

#include "optional.h"

FunctionReturns TypeCategory::CallCategoryFunction(
    const FunctionId<MemberScope::CATEGORY>& id,
    const TypeArgs&,
    const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
          << " not supported in type-category " << CategoryName();
  return FunctionReturns();
}


const TypeArgs& TypeInstance::TypeArgsForCategory(const TypeCategory& category) const {
  FAIL() << "Category " << category.CategoryName()
         << " is not a base of type-instance " << InstanceName();
  static const TypeArgs failed;
  return failed;
}

FunctionReturns TypeInstance::CallInstanceFunction(
    const FunctionId<MemberScope::INSTANCE>& id,
    const TypeArgs&,
    const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-instance " << InstanceName();
  return FunctionReturns();
}

bool TypeInstance::IsOptional() const {
  return false;
}

bool TypeInstance::CheckConversionBetween(
    const TypeInstance& from, const TypeInstance& to) {
  bool can_convert = from.InstanceMergeType() != MergeType::INTERSECT;
  for (const TypeInstance* left : from.MergedInstanceTypes()) {
    bool can_convert_to = to.InstanceMergeType() != MergeType::UNION;
    for (const TypeInstance* right : to.MergedInstanceTypes()) {
      bool can_convert_single = right->CheckConversionFrom(*left);
      switch (to.InstanceMergeType()) {
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
      if ((from.InstanceMergeType() == MergeType::UNION     && can_convert_to) ||
          (from.InstanceMergeType() == MergeType::INTERSECT && !can_convert_to)) {
        break;
      }
    }
    switch (from.InstanceMergeType()) {
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
    if ((from.InstanceMergeType() == MergeType::UNION     && !can_convert) ||
        (from.InstanceMergeType() == MergeType::INTERSECT && can_convert)) {
      break;
    }
  }
  return can_convert;
}

bool TypeInstance::CheckConversionFrom(const TypeInstance&) const {
  return false;
}


FunctionReturns TypeValue::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs&,
    const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-value " << InstanceType().InstanceName();
  return FunctionReturns();
}

ValueVariable& TypeValue::GetValueVariable(
    const ValueVariableId<MemberScope::VALUE>& id) {
  FAIL() << "Member variable " << id.VariableName()
         << " not supported in type-value " << InstanceType().InstanceName();
  static ValueVariable failed;
  return failed;
}

bool TypeValue::GetBool() const {
  FAIL() << "Cannot convert type-value " << InstanceType().InstanceName() << " to Bool";
  return false;
}

S<TypeValue> TypeValue::ConvertTo(const S<TypeValue>& self,
                                  TypeInstance& instance) {
  if (&instance == &self->InstanceType()) {
    return self;
  } else {
    // TODO: We can probably disable this for non-debug builds, since the main
    // compiler should prevent bad type conversions. Right now this behavior is
    // the same as require(reduce<t>(value)), but we can optimize it.
    FAIL_IF(!TypeInstance::CheckConversionBetween(self->InstanceType(),instance))
        << "Bad conversion: " << self->InstanceType().InstanceName()
        << " -> " << instance.InstanceName();
    return self->ConvertTo(instance);
  }
}

S<TypeValue> TypeValue::ReduceTo(const S<TypeValue>& self,
                                 TypeInstance& instance) {
  if (&instance == &self->InstanceType()) {
    return As_Optional(self,instance);
  } else if (TypeInstance::CheckConversionBetween(self->InstanceType(),instance)) {
    return As_Optional(ConvertTo(self,instance),instance);
  } else {
    return Skip_Optional(instance);
  }
}

S<TypeValue> TypeValue::ConvertTo(TypeInstance& instance) {
  FAIL() << "Cannot convert " << InstanceType().InstanceName()
         << " to type " << instance.InstanceName();
  return nullptr;
}
