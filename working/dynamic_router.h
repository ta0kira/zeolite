#ifndef DYNAMIC_ROUTER_H_
#define DYNAMIC_ROUTER_H_

#include <functional>
#include <unordered_map>

#include "dynamic_base.h"

template<class C>
struct FunctionCaller {
  virtual FunctionReturns Call(C*, const FunctionArgs&) const = 0;
  virtual ~FunctionCaller() = default;
};

template<class C, class A, class R>
class FixedCaller : public FunctionCaller<C> {
 public:
  FixedCaller(const std::function<R(C*,const A&)>& function)
      : function_(function) {}

  FunctionReturns Call(C* object, const FunctionArgs& args) const {
    return T_to_V<S<TypeValue>>(function_(object, V_to_T<S<TypeValue>>(args)));
  }

 private:
  const std::function<R(C*,const A&)> function_;
};


template<class C>
class FunctionRouter {
 public:
  template<class A, class R>
  FunctionRouter& AddFunction(const FunctionId& id, R(C::*function)(const A&)) {
    mapped_[&id] = R_get(new FixedCaller<C,A,R>(
        [function](C* object, const A& args) {
          return (object->*function)(args);
        }));
    return *this;
  }

  FunctionReturns Call(const FunctionId& id, C* object, const FunctionArgs& args) const {
    const auto caller = mapped_.find(&id);
    FAIL_IF(caller == mapped_.end()) << "Function " << id.FunctionName() << " not supported";
    return caller->second->Call(object, args);
  }

 private:
  std::unordered_map<const FunctionId*,R<const FunctionCaller<C>>> mapped_;
};


template<class C>
class FunctionRouter<const C> {
 public:
  template<class A, class R>
  FunctionRouter& AddFunction(const FunctionId& id, R(C::*function)(const A&) const) {
    mapped_[&id] = R_get(new FixedCaller<const C,A,R>(
        [function](const C* object, const A& args) {
          return (object->*function)(args);
        }));
    return *this;
  }

  FunctionReturns Call(const FunctionId& id, const C* object, const FunctionArgs& args) const {
    const auto caller = mapped_.find(&id);
    FAIL_IF(caller == mapped_.end()) << "Function " << id.FunctionName() << " not supported";
    return caller->second->Call(object, args);
  }

 private:
  std::unordered_map<const FunctionId*,R<const FunctionCaller<const C>>> mapped_;
};

#endif  // DYNAMIC_ROUTER_H_
