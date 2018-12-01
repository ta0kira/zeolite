#ifndef DISPATCH_H_
#define DISPATCH_H_

#include <functional>
#include <unordered_map>

#include "core.h"
#include "ids.h"

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
    return T_to_V<S<TypeValue>>(function_(object, V_to_T<S<TypeValue>,A>(args)));
  }

 private:
  const std::function<R(C*,const A&)> function_;
};

template<class C, MemberScope M>
class FunctionDispatcher {
 public:
  FunctionDispatcher(const std::string& name) : name_(name) {}

  template<class A, class R, class C2>
  FunctionDispatcher& AddFunction(const FunctionId<M>& id,
                                  R(C2::*function)(const A&)) {
    mapped_[&id] = R_get(new FixedCaller<C,A,R>(
        [function](C* object, const A& args) {
          return (object->*function)(args);
        }));
    return *this;
  }

  template<class A, class R, class C2>
  FunctionDispatcher& AddFunction(const FunctionId<M>& id,
                                  R(C2::*function)(const A&) const) {
    mapped_[&id] = R_get(new FixedCaller<C,A,R>(
        [function](C* object, const A& args) {
          return (object->*function)(args);
        }));
    return *this;
  }

  FunctionReturns Call(const FunctionId<M>& id, C* object,
                       const FunctionArgs& args) const {
    const auto caller = mapped_.find(&id);
    FAIL_IF(caller == mapped_.end())
        << "Function " << id.FunctionName() << " not supported by " << name_;
    return caller->second->Call(object, args);
  }

 private:
  const std::string name_;
  std::unordered_map<const FunctionId<M>*,R<const FunctionCaller<C>>> mapped_;
};

template<class C, MemberScope M>
class GetValueDispatcher {
 public:
  GetValueDispatcher(const std::string& name) : name_(name) {}

  GetValueDispatcher& AddMember(const ValueVariableId<M>& id,
                                ValueVariable C::*member) {
    mapped_[&id] = member;
    return *this;
  }

  ValueVariable Get(const ValueVariableId<M>& id, C* object) const {
    const auto member = mapped_.find(&id);
    FAIL_IF(member == mapped_.end())
        << "Variable " << id.ValueName() << " not present in " << name_;
    return object->*member;
  }

 private:
  const std::string name_;
  std::unordered_map<const ValueVariableId<M>*,ValueVariable C::*> mapped_;
};

#endif  // DISPATCH_H_
