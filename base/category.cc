#include "category.h"

#include "intersect.h"
#include "optional.h"
#include "union.h"

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

bool TypeInstance::IsParentCategory(const TypeCategory& category) const {
  return &CategoryType() == &category;
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
    const TypeInstance& x, const TypeInstance& y) {
  bool can_convert = x.InstanceMergeType() != MergeType::INTERSECT;
  for (const TypeInstance* left : x.MergedInstanceTypes()) {
    bool can_convert_to = y.InstanceMergeType() != MergeType::UNION;
    for (const TypeInstance* right : y.MergedInstanceTypes()) {
      bool can_convert_single = false;
      switch (left->InstanceMergeType()) {
        case MergeType::SINGLE:
          can_convert_single = right->CheckConversionFrom(*left);
          break;
        case MergeType::UNION:
        case MergeType::INTERSECT:
          // Union/Intersect are implemented to not provide type params when
          // calling TypeArgsForCategory, which prevents CheckConversionFrom
          // from being useful when passing *left.
          can_convert_single = CheckConversionBetween(*left,*right);
          break;
      }
      switch (y.InstanceMergeType()) {
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
      if ((x.InstanceMergeType() == MergeType::UNION     && can_convert_to) ||
          (x.InstanceMergeType() == MergeType::INTERSECT && !can_convert_to)) {
        break;
      }
    }
    switch (x.InstanceMergeType()) {
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
    if ((x.InstanceMergeType() == MergeType::UNION     && !can_convert) ||
        (x.InstanceMergeType() == MergeType::INTERSECT && can_convert)) {
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

ValueVariable* TypeValue::GetValueVariable(
    const ValueVariableId<MemberScope::VALUE>& id) {
  FAIL() << "Member variable " << id.VariableName()
         << " not supported in type-value " << InstanceType().InstanceName();
  return nullptr;
}

bool TypeValue::IsPresent() const {
  return true;
}

bool TypeValue::GetBool() const {
  FAIL() << "Cannot convert type-value " << InstanceType().InstanceName()
         << " to primitive bool";
  return false;
}

std::string TypeValue::GetString() const {
  FAIL() << "Cannot convert type-value " << InstanceType().InstanceName()
         << " to primitive string";
  return "";
}

S<TypeValue> TypeValue::Require(const S<TypeValue>& self) {
  if (!self->IsPresent()) {
    FAIL() << self->InstanceType().InstanceName() << " value is not present";
  }
  if (&self->InstanceType().CategoryType() == &Category_Optional()) {
    return self->GetNestedValue();
  } else {
    return self;
  }
}

S<TypeValue> TypeValue::ConvertTo(const S<TypeValue>& self,
                                  TypeInstance& instance) {
  if (&instance == &self->InstanceType()) {
    return self;
  } else if (&instance.CategoryType() == &Category_Union()) {
    return As_Union(self,instance.TypeArgsForCategory(Category_Union()));
  } else if (&instance.CategoryType() == &Category_Intersect()) {
    return As_Intersect(self,instance.TypeArgsForCategory(Category_Intersect()));
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

S<TypeValue> TypeValue::GetNestedValue() {
  FAIL() << "No nested value in " << InstanceType().InstanceName();
  return nullptr;
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
