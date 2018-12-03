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


FunctionReturns TypeInstance::CallInstanceFunction(
    const FunctionId<MemberScope::INSTANCE>& id,
    const TypeArgs&,
    const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-instance " << InstanceName();
  return FunctionReturns();
}

bool TypeInstance::CheckConversionBetween(
    const TypeInstance& x, const TypeInstance& y) {
  if (&x == &y) {
    return true;
  }
  if (x.InstanceMergeType() == MergeType::SINGLE &&
      y.InstanceMergeType() == MergeType::SINGLE) {
    return y.CheckConversionFrom(x);
  }
  return ExpandCheckLeft(x,y);
}

bool TypeInstance::IsParentCategory(const TypeCategory& category) const {
  return &CategoryType() == &category;
}

const TypeArgs& TypeInstance::TypeArgsForCategory(const TypeCategory& category) const {
  FAIL() << "Category " << category.CategoryName()
         << " is not a base of type-instance " << InstanceName();
  static const TypeArgs failed;
  return failed;
}

bool TypeInstance::ExpandCheckLeft(
    const TypeInstance& x, const TypeInstance& y) {
  for (const TypeInstance* left : x.MergedInstanceTypes()) {
    const bool result = ExpandCheckRight(*left,y);
    switch (x.InstanceMergeType()) {
      case MergeType::SINGLE:
        return result;
      case MergeType::UNION:
        if (!result) {
          return false;
        }
        break;
      case MergeType::INTERSECT:
        if (result) {
          return true;
        }
        break;
    }
  }
  switch (x.InstanceMergeType()) {
    case MergeType::SINGLE:    return false;
    case MergeType::UNION:     return true;
    case MergeType::INTERSECT: return false;
  }
}

bool TypeInstance::ExpandCheckRight(
    const TypeInstance& x, const TypeInstance& y) {
  for (const TypeInstance* right : y.MergedInstanceTypes()) {
    const bool result = TypeInstance::CheckConversionBetween(x,*right);
    switch (y.InstanceMergeType()) {
      case MergeType::SINGLE:
        return result;
      case MergeType::UNION:
        if (result) {
          return true;
        }
        break;
      case MergeType::INTERSECT:
        if (!result) {
          return false;
        }
        break;
    }
  }
  switch (y.InstanceMergeType()) {
    case MergeType::SINGLE:    return false;
    case MergeType::UNION:     return false;
    case MergeType::INTERSECT: return true;
  }
}

bool TypeInstance::CheckConversionFrom(const TypeInstance&) const {
  return false;
}


FunctionReturns TypeValue::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs&,
    const FunctionArgs&) {
  FAIL() << "Function " << id.FunctionName()
         << " not supported in type-value " << InstanceName();
  return FunctionReturns();
}

ValueVariable* TypeValue::GetValueVariable(
    const TypeInstance& instance,
    const ValueVariableId<MemberScope::VALUE>& id) {
  FAIL_IF(&instance != &InstanceType())
      << "Member variables in type-value " << InstanceName()
      << " not visible from " << instance.InstanceName();
  FAIL() << "Member variable " << id.VariableName()
         << " not supported in type-value " << InstanceName();
  return nullptr;
}

TypeInstance* TypeValue::GetTypeVariable(
    const TypeInstance& instance,
    const TypeVariableId<MemberScope::VALUE>& id) {
  FAIL_IF(&instance != &InstanceType())
      << "Member types in type-value " << InstanceName()
      << " not visible from " << instance.InstanceName();
  FAIL() << "Member type " << id.VariableName()
         << " not supported in type-value " << InstanceName();
  return nullptr;
}

S<TypeValue> TypeValue::GetNestedValue() {
  FAIL() << "No nested value in " << InstanceName();
  return nullptr;
}

bool TypeValue::IsPresent() const {
  return true;
}

bool TypeValue::GetBool() const {
  FAIL() << "Cannot convert type-value " << InstanceName()
         << " to primitive bool";
  return false;
}

std::string TypeValue::GetString() const {
  FAIL() << "Cannot convert type-value " << InstanceName()
         << " to primitive string";
  return "";
}

S<TypeValue> TypeValue::Require(const S<TypeValue>& self) {
  if (!self->IsPresent()) {
    FAIL() << self->InstanceName() << " value is not present";
  }
  if (&self->CategoryType() == &Category_Optional()) {
    return self->GetNestedValue();
  } else {
    return self;
  }
}

S<TypeValue> TypeValue::ConvertTo(const S<TypeValue>& self,
                                  TypeInstance& instance) {
#ifdef OPT_TYPE_CHECKING
  // Conversions aren't actually necessary, as long as the compiler did its job
  // ensuring correct type usage. Types really only matter when calling value
  // functions. The exception is the use of reduce, which actually performs a
  // full check of the nominal type.
  return self;
#else
  if (&instance == &self->InstanceType()) {
    return self;
  }
  FAIL_IF(!CheckConversionTo(self,instance))
      << "Bad conversion from " << self->InstanceName()
      << " to " << instance.InstanceName();
  if (&instance.CategoryType() == &Category_Optional()) {
    return As_Optional(self,*SafeGet<0>(instance.CategoryTypeArgsFrom(instance)));
  } else if (&instance.CategoryType() == &Category_Union()) {
    return As_Union(self,instance.CategoryTypeArgsFrom(instance));
  } else if (&instance.CategoryType() == &Category_Intersect()) {
    return As_Intersect(self,instance.CategoryTypeArgsFrom(instance));
  } else {
    return self->ConvertTo(instance);
  }
#endif
}

S<TypeValue> TypeValue::ReduceTo(const S<TypeValue>& self,
                                 TypeInstance& nominal,
                                 TypeInstance& instance) {
  // NOTE: The runtime type of self should never be checked here, since reduce
  // is meant to operate like a compile-time operation.
  if (&nominal == &instance) {
    return As_Optional(self,instance);
  } if (TypeInstance::CheckConversionBetween(nominal,instance)) {
    return As_Optional(ConvertTo(self,instance),instance);
  } else {
    return TypeValue::ConvertTo(Optional_Skip(),Category_Optional().Build(instance));
  }
}

S<TypeValue> TypeValue::ConvertTo(TypeInstance& instance) {
  FAIL() << "Cannot convert " << InstanceName()
         << " to type " << instance.InstanceName();
  return nullptr;
}
