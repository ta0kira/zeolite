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
using TypeArgs = std::vector<const TypeInstance*>;

enum class MergeType {
  SINGLE,
  UNION,
  INTERSECT,
};


class TypeCategory {
 public:
  ALWAYS_UNIQUE(TypeCategory)

  virtual const CategoryId& CategoryType() const = 0;
  virtual FunctionReturns CallCategoryFunction(
      const FunctionId<MemberScope::CATEGORY>& id, const FunctionArgs&);

 protected:
  TypeCategory() = default;
  virtual ~TypeCategory() = default;
};


class TypeInstance {
 public:
  ALWAYS_UNIQUE(TypeInstance)

  virtual ~TypeInstance() = default;

  virtual std::string TypeName() const = 0;
  virtual const TypeArgs& TypeArgsForCategory(const CategoryId&) const;
  virtual FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>& id, const FunctionArgs&);

  static bool CheckConversionBetween(const TypeInstance&, const TypeInstance&);

 protected:
  TypeInstance() = default;
  virtual bool CheckConversionFrom(const TypeInstance&) const;
  virtual MergeType InstanceMergeType() const;
  virtual TypeArgs MergedInstanceTypes() const;
};


class TypeValue {
 public:
  ALWAYS_UNIQUE(TypeValue)

  virtual ~TypeValue() = default;

  virtual FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id, const FunctionArgs&);
  virtual ValueVariable& GetValueVariable(const ValueVariableId<MemberScope::VALUE>& id);

  virtual bool IsOptional() const;

  // TODO: Add accessors for primitive types.

  static S<TypeValue> ConvertTo(const S<TypeValue>& self,
                                const TypeInstance& instance);

  static S<TypeValue> ReduceTo(const S<TypeValue>& self,
                               const TypeInstance& instance);

 protected:
  TypeValue() = default;
  virtual const TypeInstance& InstanceType() const = 0;
  virtual S<TypeValue> ConvertTo(const TypeInstance& instance);
};


class ValueVariable {
 public:
  ValueVariable() = default;
  ALWAYS_UNIQUE(ValueVariable)
};

#endif  // CATEGORY_H_
