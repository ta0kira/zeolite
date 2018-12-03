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


template<int N, class...Ts>
struct ParamArgs {
  using Type = typename ParamArgs<N-1, S<TypeValue>, Ts...>::Type;
};

template<class...Ts>
struct ParamArgs<0, Ts...> {
  using Type = const T<Ts...>&;
};


template<int N, class...Ts>
struct ParamReturns {
  using Type = typename ParamReturns<N-1, S<TypeValue>, Ts...>::Type;
};

template<class...Ts>
struct ParamReturns<0, Ts...> {
  using Type = T<Ts...>;
};


template<int N, class...Ts>
struct ParamTypes {
  using Type = typename ParamTypes<N-1, TypeInstance*, Ts...>::Type;
};

template<class...Ts>
struct ParamTypes<0, Ts...> {
  using Type = const T<Ts...>&;
};


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
      const FunctionId<MemberScope::CATEGORY>&,
      const TypeArgs&,
      const FunctionArgs&);

 protected:
  TypeCategory() = default;
  virtual ~TypeCategory() = default;
};


class TypeInstance {
 public:
  ALWAYS_UNIQUE(TypeInstance)

  virtual ~TypeInstance() = default;

  virtual const std::string& InstanceName() const = 0;
  virtual const TypeCategory& CategoryType() const = 0;
  virtual bool IsParentCategory(const TypeCategory&) const;
  virtual const TypeArgs& TypeArgsForCategory(const TypeCategory&) const;
  virtual FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>&,
      const TypeArgs&,
      const FunctionArgs&);

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

  virtual FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>&,
      const TypeArgs&,
      const FunctionArgs&);
  virtual ValueVariable* GetValueVariable(const ValueVariableId<MemberScope::VALUE>&);

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

#endif  // CATEGORY_H_
