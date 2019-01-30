#ifndef CATEGORY_SOURCE_HPP_
#define CATEGORY_SOURCE_HPP_

#include <map>
#include <vector>

#include "types.hpp"
#include "function.hpp"
#include "builtin.hpp"
#include "dispatcher.hpp"
#include "cycle-check.hpp"


template<int I, class T>
S<TypeValue>& SafeGet(T& values) {
  S<TypeValue>& value = std::get<I>(values);
  FAIL_IF(value == nullptr) << "Value is null";
  return value;
}

template<int I, class T>
S<TypeValue> SafeGet(const T& values) {
  S<TypeValue> value = std::get<I>(values);
  FAIL_IF(value == nullptr) << "Value is null";
  return value;
}

template<int I>
W<TypeValue>& SafeGet(typename std::tuple<W<TypeValue>>& values) {
  return std::get<I>(values);
}

class TypeCategory {
 public:
  template<int P, int A, int R>
  typename Returns<R>::Type Call(const Function<SymbolScope::CATEGORY,P,A,R>& label,
                                 typename Params<P>::Type params,
                                 typename Args<A>::Type args) {
    return V_to_T<typename Returns<R>::Type>(Dispatch(
      label, T_to_V<TypeInstance*>(params), T_to_V<S<TypeValue>>(args)));
  }

  virtual std::string CategoryName() const = 0;

  ALWAYS_PERMANENT(TypeCategory)
  virtual ~TypeCategory() = default;

 protected:
  TypeCategory() = default;

  virtual DReturns Dispatch(const DFunction<SymbolScope::CATEGORY>& label,
                            DParams params, DArgs args);
};

class TypeInstance {
 public:
  template<int P, int A, int R>
  typename Returns<R>::Type Call(const Function<SymbolScope::TYPE,P,A,R>& label,
                                 typename Params<P>::Type params,
                                 typename Args<A>::Type args) {
    return V_to_T<typename Returns<R>::Type>(Dispatch(
      label, T_to_V<TypeInstance*>(params), T_to_V<S<TypeValue>>(args)));
  }

  virtual std::string CategoryName() const = 0;

  static Returns<1>::Type Reduce(TypeInstance& from, TypeInstance& to, S<TypeValue> target) {
    TRACE_FUNCTION("reduce")
    return T_get(CanConvert(from, to)? target : Var_empty);
  }

  virtual bool TypeArgsForParent(
    const TypeCategory& category, std::vector<const TypeInstance*>& args) const
  { return false; }

  ALWAYS_PERMANENT(TypeInstance)
  virtual ~TypeInstance() = default;

 protected:
  TypeInstance() = default;

  virtual DReturns Dispatch(const DFunction<SymbolScope::TYPE>& label,
                            DParams params, DArgs args);

  virtual bool CanConvertFrom(const TypeInstance& from) const
  { return false; }

  static bool CanConvert(const TypeInstance& from, const TypeInstance& to);

  enum class MergeType {
    SINGLE,
    UNION,
    INTERSECT,
  };

 private:
  virtual MergeType InstanceMergeType() const
  { return MergeType::SINGLE; }

  virtual std::vector<const TypeInstance*> MergedTypes() const
  { return std::vector<const TypeInstance*>{this}; }

  static bool ExpandCheckLeft(const TypeInstance& from, const TypeInstance& to);
  static bool ExpandCheckRight(const TypeInstance& from, const TypeInstance& to);
};

class TypeValue {
 public:
  template<int P, int A, int R>
  static typename Returns<R>::Type Call(S<TypeValue> target,
                                        const Function<SymbolScope::VALUE,P,A,R>& label,
                                        typename Params<P>::Type params,
                                        typename Args<A>::Type args) {
    FAIL_IF(target == nullptr) << "Function called on null value";
    return V_to_T<typename Returns<R>::Type>(target->Dispatch(
      target, label, T_to_V<TypeInstance*>(params), T_to_V<S<TypeValue>>(args)));
  }

  virtual std::string CategoryName() const = 0;

  static Returns<1>::Type Present(S<TypeValue> target);
  static Returns<1>::Type Require(S<TypeValue> target);
  static Returns<1>::Type Strong(W<TypeValue> target);

  virtual bool AsBool() const;
  virtual std::string AsString() const;
  virtual int AsInt() const;  // TODO: Use explict precision here.
  virtual double AsFloat() const;

  ALWAYS_PERMANENT(TypeValue)
  virtual ~TypeValue() = default;

 protected:
  TypeValue() = default;

  virtual bool Present() const;

  virtual DReturns Dispatch(const S<TypeValue>& self,
                            const DFunction<SymbolScope::VALUE>& label,
                            DParams params, DArgs args);
};

template<int P, class T>
using InstanceMap = std::map<typename Params<P>::Type, R<T>>;

#endif  // CATEGORY_SOURCE_HPP_
