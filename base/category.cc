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
    if (&x == &y) {
      return true;
    }

    for (const TypeInstance* left : x.MergedInstanceTypes()) {
      switch (x.InstanceMergeType()) {
        case MergeType::SINGLE:
          return y.CheckConversionFrom(*left);
        case MergeType::UNION:
          if (!y.CheckConversionFrom(*left)) {
            return false;
          }
          break;
        case MergeType::INTERSECT:
          if (y.CheckConversionFrom(*left)) {
            return true;
          }
          break;
      }
    }

    switch (x.InstanceMergeType()) {
      case MergeType::SINGLE:    return true;
      case MergeType::UNION:     return true;
      case MergeType::INTERSECT: return false;
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
         << " not supported in type-value " << InstanceType().InstanceName();
  return FunctionReturns();
}

ValueVariable* TypeValue::GetValueVariable(
    const ValueVariableId<MemberScope::VALUE>& id) {
  FAIL() << "Member variable " << id.VariableName()
         << " not supported in type-value " << InstanceType().InstanceName();
  return nullptr;
}

S<TypeValue> TypeValue::GetNestedValue() {
  FAIL() << "No nested value in " << InstanceType().InstanceName();
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
  FAIL_IF(!TypeInstance::CheckConversionBetween(self->InstanceType(),instance))
      << "Bad conversion from " << self->InstanceType().InstanceName()
      << " to " << instance.InstanceName();
  if (&instance.CategoryType() == &Category_Optional()) {
    return As_Optional(self,*SafeGet<0>(instance.TypeArgsForCategory(Category_Optional())));
  } else if (&instance.CategoryType() == &Category_Union()) {
    return As_Union(self,instance.TypeArgsForCategory(Category_Union()));
  } else if (&instance.CategoryType() == &Category_Intersect()) {
    return As_Intersect(self,instance.TypeArgsForCategory(Category_Intersect()));
  } else {
    return self->ConvertTo(instance);
  }
#endif
}

S<TypeValue> TypeValue::ReduceTo(const S<TypeValue>& self,
                                 TypeInstance& nominal,
                                 TypeInstance& instance) {
  if (&nominal == &instance) {
    return As_Optional(self,instance);
  } else if (TypeInstance::CheckConversionBetween(nominal,instance)) {
    return As_Optional(ConvertTo(self,instance),instance);
  } else {
    return TypeValue::ConvertTo(Optional_Skip(),Category_Optional().Build(instance));
  }
}

S<TypeValue> TypeValue::ConvertTo(TypeInstance& instance) {
  FAIL() << "Cannot convert " << InstanceType().InstanceName()
         << " to type " << instance.InstanceName();
  return nullptr;
}
