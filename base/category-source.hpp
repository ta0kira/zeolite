#ifndef CATEGORY_SOURCE_HXX_
#define CATEGORY_SOURCE_HXX_

#include "types.hpp"
#include "function.hpp"
#include "dispatcher.hpp"


class TypeCategory {
 public:
  template<int P, int A, int R>
  typename Returns<R>::Type Call(const Function<SymbolScope::CategoryScope,P,A,R>& label,
                                 typename Params<P>::Type params,
                                 typename Args<A>::Type args) {
    return V_to_T(Dispatch(label, T_to_V(params), T_to_V(args)));
  }

  virtual std::string CategoryName() const = 0;

  ALWAYS_PERMANENT(TypeCategory)
  virtual ~TypeCategory() = default;

 protected:
  TypeCategory() = default;

  virtual DReturns Dispatch(const DFunction<SymbolScope::CategoryScope>& label,
                            DParams params, DArgs args);
};

class TypeInstance {
 public:
  template<int P, int A, int R>
  typename Returns<R>::Type Call(const Function<SymbolScope::TypeScope,P,A,R>& label,
                                 typename Params<P>::Type params,
                                 typename Args<A>::Type args) {
    return V_to_T(Dispatch(label, T_to_V(params), T_to_V(args)));
  }

  virtual std::string CategoryName() const = 0;

  static S<TypeValue> Reduce(TypeInstance& from, TypeInstance& to, S<TypeValue> target);

  ALWAYS_PERMANENT(TypeInstance)
  virtual ~TypeInstance() = default;

 protected:
  TypeInstance() = default;

  virtual DReturns Dispatch(const DFunction<SymbolScope::TypeScope>& label,
                            DParams params, DArgs args);
};

class TypeValue {
 public:
  template<int P, int A, int R>
  static typename Returns<R>::Type Call(S<TypeValue> target,
                                        const Function<SymbolScope::ValueScope,P,A,R>& label,
                                        typename Params<P>::Type params,
                                        typename Args<A>::Type args) {
    FAIL_IF(target == nullptr) << "Function called on missing value";
    return V_to_T(target->Dispatch(target, label, T_to_V(params), T_to_V(args)));
  }

  virtual std::string CategoryName() const = 0;

  static bool Present(S<TypeValue> target) { return target->Present(); }
  static S<TypeValue> Strong(W<TypeValue> target) { return target.lock(); }

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
                            const DFunction<SymbolScope::ValueScope>& label,
                            DParams params, DArgs args);
};

#endif  // CATEGORY_SOURCE_HXX_
