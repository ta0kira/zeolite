#ifndef CATEGORY_H_
#define CATEGORY_H_

#include <vector>

#include "core.h"
#include "ids.h"

class TypeCategory;
class TypeInstance;
class TypeValue;
class ValueVariable;

using FunctionArgs = std::vector<S<TypeValue>>;
using FunctionReturns = std::vector<S<TypeValue>>;
using TypeArgs = std::vector<TypeInstance*>;


enum class MergeType {
  SINGLE,
  UNION,
  INTERSECT,
};


class TypeCategory {
 public:
  ALWAYS_PERMANENT(TypeCategory)

  virtual const std::string& CategoryName() const = 0;
  virtual FunctionReturns CallCategoryFunction(
      const FunctionId<MemberScope::CATEGORY>&,
      const TypeArgs&,
      const FunctionArgs&);

 protected:
  TypeCategory() = default;
  virtual ~TypeCategory() = default;
};


class TypeInstance {
 public:
  ALWAYS_PERMANENT(TypeInstance)

  virtual const std::string& InstanceName() const = 0;
  virtual const TypeCategory& CategoryType() const = 0;
  virtual FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>&,
      const TypeArgs&,
      const FunctionArgs&);

  inline bool CategoryIsParentOf(const TypeInstance& instance) const {
    return instance.IsParentCategory(CategoryType());
  }

  inline const TypeArgs& CategoryTypeArgsFrom(const TypeInstance& instance) const {
    return instance.TypeArgsForCategory(CategoryType());
  }

  static bool CheckConversionBetween(const TypeInstance&, const TypeInstance&);

 protected:
  TypeInstance() = default;
  virtual ~TypeInstance() = default;

  virtual bool IsParentCategory(const TypeCategory&) const;
  virtual const TypeArgs& TypeArgsForCategory(const TypeCategory&) const;
  virtual bool CheckConversionFrom(const TypeInstance&) const;
  virtual MergeType InstanceMergeType() const = 0;
  virtual const TypeArgs& MergedInstanceTypes() const = 0;

 private:
  static bool ExpandCheckLeft(const TypeInstance&, const TypeInstance&);
  static bool ExpandCheckRight(const TypeInstance&, const TypeInstance&);
};


class TypeValue {
 public:
  ALWAYS_PERMANENT(TypeValue)

  virtual ~TypeValue() = default;

  virtual FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>&,
      const TypeArgs&,
      const FunctionArgs&);
  virtual ValueVariable* GetValueVariable(
      const TypeInstance&,
      const ValueVariableId<MemberScope::VALUE>&);
  virtual TypeInstance* GetTypeVariable(
      const TypeInstance&,
      const TypeVariableId<MemberScope::VALUE>&);

  virtual S<TypeValue> GetNestedValue();
  virtual bool IsPresent() const;
  virtual bool GetBool() const;
  virtual std::string GetString() const;

  inline const TypeCategory& CategoryType() const {
    return InstanceType().CategoryType();
  }

  inline const std::string& InstanceName() const {
    return InstanceType().InstanceName();
  }

  static S<TypeValue> Require(const S<TypeValue>&);
  static S<TypeValue> ConvertTo(const S<TypeValue>&, TypeInstance&);
  static S<TypeValue> ReduceTo(const S<TypeValue>&, TypeInstance&, TypeInstance&);

 protected:
  TypeValue() = default;

  virtual const TypeInstance& InstanceType() const = 0;
  virtual S<TypeValue> ConvertTo(TypeInstance&);

  inline static bool CheckConversionTo(
      const S<TypeValue>& self, const TypeInstance& instance) {
    return TypeInstance::CheckConversionBetween(self->InstanceType(),instance);
  }
};


class ValueVariable {
 public:
  // NOTE: Do not make the instance type accessible. This forces the compiler to
  // know where to get the type from if it's needed, e.g., for reduce.
  inline ValueVariable(TypeInstance& instance, const S<TypeValue>& value)
      : instance_(instance), value_(value) {}

  ALWAYS_UNIQUE(ValueVariable)

  inline const S<TypeValue>& GetValue() const { return value_; }

  inline void SetValue(const S<TypeValue>& value) {
    value_ = TypeValue::ConvertTo(value,instance_);
  }

 private:
  TypeInstance& instance_;
  S<TypeValue> value_;
};


class ProxyVariable {
 public:
  inline ProxyVariable(TypeInstance& instance, S<TypeValue>& value)
      : instance_(instance), value_(value) {}

  ALWAYS_UNIQUE(ProxyVariable)

  inline const S<TypeValue>& GetValue() const { return value_; }

  inline void SetValue(const S<TypeValue>& value) {
    value_ = TypeValue::ConvertTo(value,instance_);
  }

 private:
  TypeInstance& instance_;
  S<TypeValue>& value_;
};

#endif  // CATEGORY_H_
