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
  virtual const CategoryId* CategoryType() const = 0;
  virtual FunctionReturns CallCategoryFunction(
      const FunctionId<MemberScope::CATEGORY>& id, const FunctionArgs&);

 protected:
  virtual ~TypeCategory() = default;
};


class TypeInstance {
 public:
  virtual ~TypeInstance() = default;

  virtual std::string TypeName() const = 0;
  virtual const TypeArgs& TypeArgsForCategory(const CategoryId*) const;
  virtual FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>& id, const FunctionArgs&);

  static bool CheckConversionBetween(const TypeInstance*, const TypeInstance*);

 protected:
  virtual bool CheckConversionFrom(const TypeInstance*) const;
  virtual MergeType InstanceMergeType() const;
  virtual TypeArgs MergedInstanceTypes() const;
};


class TypeValue {
 public:
  virtual ~TypeValue() = default;

  virtual FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id, const FunctionArgs&);
  virtual ValueVariable& GetValueVariable(const ValueVariableId<MemberScope::VALUE>& id);
  virtual S<TypeInstance> GetTypeVariable(const TypeVariableId<MemberScope::VALUE>& id);

  virtual bool IsOptional() const;

  // TODO: Add accessors for primitive types.

  static S<TypeValue> ConvertTo(const S<TypeValue>& self,
                                const S<TypeInstance>& instance);

  static S<TypeValue> ReduceTo(const S<TypeValue>& self,
                               const S<TypeInstance>& instance);

 protected:
  virtual const TypeInstance* InstanceType() const = 0;
  virtual S<TypeValue> ConvertTo(const S<TypeInstance>& instance);
};


class ValueVariable {

};

#endif  // CATEGORY_H_
