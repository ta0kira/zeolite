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

template<int I, class T>
const T& SafeGet(const std::vector<T>& values) {
  FAIL_IF(I < 0 || I >= values.size()) << "Index " << I << " out of range";
  return values[I];
}

enum class MergeType {
  SINGLE,
  UNION,
  INTERSECT,
};


class TypeCategory {
 public:
  ALWAYS_UNIQUE(TypeCategory)

  virtual const std::string& CategoryName() const = 0;
  virtual FunctionReturns CallCategoryFunction(
      const FunctionId<MemberScope::CATEGORY>&, const FunctionArgs&);

 protected:
  TypeCategory() = default;
  virtual ~TypeCategory() = default;
};


class TypeInstance {
 public:
  ALWAYS_UNIQUE(TypeInstance)

  virtual ~TypeInstance() = default;

  virtual const std::string& InstanceName() const = 0;
  virtual const TypeArgs& TypeArgsForCategory(const TypeCategory&) const;
  virtual FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>&, const FunctionArgs&);
  virtual bool IsOptional() const;

  static bool CheckConversionBetween(const TypeInstance&, const TypeInstance&);

 protected:
  TypeInstance() = default;
  virtual bool CheckConversionFrom(const TypeInstance&) const;
  virtual MergeType InstanceMergeType() const = 0;
  virtual const TypeArgs& MergedInstanceTypes() const = 0;
};


class TypeValue {
 public:
  ALWAYS_UNIQUE(TypeValue)

  virtual ~TypeValue() = default;

  virtual const TypeInstance& InstanceType() const = 0;
  virtual FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>&, const FunctionArgs&);
  virtual ValueVariable& GetValueVariable(const ValueVariableId<MemberScope::VALUE>&);

  // TODO: Add accessors for primitive types.

  bool GetBool() const;

  static S<TypeValue> ConvertTo(const S<TypeValue>&, const TypeInstance&);

  static S<TypeValue> ReduceTo(const S<TypeValue>&, const TypeInstance&);

 protected:
  TypeValue() = default;
  virtual S<TypeValue> ConvertTo(const TypeInstance&);
};


class ValueVariable {
 public:
  ValueVariable() = default;
  ALWAYS_UNIQUE(ValueVariable)
};

#endif  // CATEGORY_H_
