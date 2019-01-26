#ifndef DISPATCHER_HXX_
#define DISPATCHER_HXX_

#include <functional>
#include <unordered_map>

#include "types.hpp"
#include "function.hpp"


template<class C, class T, class V>
class Dispatcher {
 public:
  template<int P, int A, int R>
  Dispatcher& Register(const Function<SymbolScope::CategoryScope,P,A,R>& label,
    typename Returns<R>::Type(C::*function)(typename Params<P>::Type,
                                            typename Args<A>::Type)) {
    category_[&label] =
      [function](C& target, DParams params, DArgs args) {
        return T_to_V<S<TypeValue>>((target.*function)(
                                      V_to_T<typename Params<P>::Type>(params),
                                      V_to_T<typename Args<A>::Type>(args)));
      };
    return *this;
  }

  DReturns Dispatch(C& target,
                    const DFunction<SymbolScope::CategoryScope>& label,
                    DParams params, DArgs args) const {
    const auto caller = category_.find(&label);
    FAIL_IF(caller == category_.end())
        << target.CategoryName() << " does not implement " << label.FunctionName();
    return caller->second(target, params, args);
  }

  template<int P, int A, int R>
  Dispatcher& Register(const Function<SymbolScope::TypeScope,P,A,R>& label,
    typename Returns<R>::Type(T::*function)(typename Params<P>::Type,
                                            typename Args<A>::Type)) {
    type_[&label] =
      [function](T& target, DParams params, DArgs args) {
      return T_to_V<S<TypeValue>>((target.*function)(
                                    V_to_T<typename Params<P>::Type>(params),
                                    V_to_T<typename Args<A>::Type>(args)));
      };
    return *this;
  }

  DReturns Dispatch(T& target,
                    const DFunction<SymbolScope::TypeScope>& label,
                    DParams params, DArgs args) const {
    const auto caller = type_.find(&label);
    FAIL_IF(caller == type_.end())
        << target.CategoryName() << " does not implement " << label.FunctionName();
    return caller->second(target, params, args);
  }

  template<int P, int A, int R>
  Dispatcher& Register(const Function<SymbolScope::ValueScope,P,A,R>& label,
    typename Returns<R>::Type(V::*function)(const S<TypeValue>&,
                                            typename Params<P>::Type,
                                            typename Args<A>::Type)) {
    value_[&label] =
      [function](V& target, const S<TypeValue>& self, DParams params, DArgs args) {
        return T_to_V<S<TypeValue>>((target.*function)(
                                      self,
                                      V_to_T<typename Params<P>::Type>(params),
                                      V_to_T<typename Args<A>::Type>(args)));
      };
    return *this;
  }

  DReturns Dispatch(V& target,
                    const S<TypeValue>& self,
                    const DFunction<SymbolScope::ValueScope>& label,
                    DParams params, DArgs args) const {
    const auto caller = value_.find(&label);
    FAIL_IF(caller == value_.end())
        << target.CategoryName() << " does not implement " << label.FunctionName();
    return caller->second(target, self, params, args);
  }

 private:
  std::unordered_map<const DFunction<SymbolScope::CategoryScope>*,
                     std::function<DReturns(C&,DParams,DArgs)>> category_;
  std::unordered_map<const DFunction<SymbolScope::TypeScope>*,
                     std::function<DReturns(T&,DParams,DArgs)>> type_;
  std::unordered_map<const DFunction<SymbolScope::ValueScope>*,
                     std::function<DReturns(V&,const S<TypeValue>&,DParams,DArgs)>> value_;
};

#endif  // DISPATCHER_HXX_
