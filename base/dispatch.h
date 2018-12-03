#ifndef DISPATCH_H_
#define DISPATCH_H_

#include <functional>
#include <unordered_map>

#include "core.h"
#include "ids.h"

template<class C>
struct FunctionCaller {
  virtual FunctionReturns Call(C*,
                               const TypeArgs& types,
                               const FunctionArgs&) const = 0;
  virtual ~FunctionCaller() = default;
};

template<class C, class T, class A, class R>
class FixedCaller : public FunctionCaller<C> {
 public:
  FixedCaller(const std::function<R(C*,const T&,const A&)>& function)
      : function_(function) {}

  FunctionReturns Call(C* object, const TypeArgs& types, const FunctionArgs& args) const {
    return T_to_V<S<TypeValue>>(function_(object,
                                          V_to_T<TypeInstance*,T>(types),
                                          V_to_T<S<TypeValue>,A>(args)));
  }

 private:
  const std::function<R(C*,const T&,const A&)> function_;
};

template<class C, MemberScope M>
class FunctionDispatcher {
 public:
  FunctionDispatcher(const std::string& name) : name_(name) {}

  ALWAYS_UNIQUE(FunctionDispatcher)

  template<class A, class T, class R, class C2>
  FunctionDispatcher& AddFunction(const FunctionId<M>& id,
                                  R(C2::*function)(const T&,const A&)) {
    mapped_[&id] = R_get(new FixedCaller<C,T,A,R>(
        [function](C* object, const T& types, const A& args) {
          return (object->*function)(types,args);
        }));
    return *this;
  }

  template<class A, class T, class R, class C2>
  FunctionDispatcher& AddFunction(const FunctionId<M>& id,
                                  R(C2::*function)(const T&,const A&) const) {
    mapped_[&id] = R_get(new FixedCaller<C,T,A,R>(
        [function](C* object, const T& types, const A& args) {
          return (object->*function)(types,args);
        }));
    return *this;
  }

  FunctionReturns Call(const FunctionId<M>& id, C* object,
                       const TypeArgs& types,
                       const FunctionArgs& args) const {
    const auto caller = mapped_.find(&id);
    FAIL_IF(caller == mapped_.end())
        << "Function " << id.FunctionName() << " not supported by " << name_;
    return caller->second->Call(object,types,args);
  }

 private:
  const std::string name_;
  std::unordered_map<const FunctionId<M>*,R<const FunctionCaller<C>>> mapped_;
};


template<class C, MemberScope M>
class GetValueDispatcher {
 public:
  GetValueDispatcher(const std::string& name) : name_(name) {}

  ALWAYS_UNIQUE(GetValueDispatcher)

  GetValueDispatcher& AddVariable(const ValueVariableId<M>& id,
                                  ValueVariable C::*member) {
    mapped_[&id] =
        [member](C* object) {
          return &(object->*member);
        };
    return *this;
  }

  ValueVariable* GetVariable(const ValueVariableId<M>& id, C* object) const {
    const auto getter = mapped_.find(&id);
    FAIL_IF(getter == mapped_.end())
        << "Variable " << id.VariableName() << " not present in " << name_;
    return getter->second(object);
  }

 private:
  const std::string name_;
  std::unordered_map<const ValueVariableId<M>*,std::function<ValueVariable*(C*)>> mapped_;
};


template<class C, MemberScope M>
class GetTypeDispatcher {
 public:
  GetTypeDispatcher(const std::string& name) : name_(name) {}

  ALWAYS_UNIQUE(GetTypeDispatcher)

  GetTypeDispatcher& AddVariable(const TypeVariableId<M>& id,
                                 TypeInstance* const C::*member) {
    mapped_[&id] =
        [member](C* object) {
          return object->*member;
        };
    return *this;
  }

  TypeInstance* GetVariable(const TypeVariableId<M>& id, C* object) const {
    const auto getter = mapped_.find(&id);
    FAIL_IF(getter == mapped_.end())
        << "Variable " << id.VariableName() << " not present in " << name_;
    return getter->second(object);
  }

 private:
  const std::string name_;
  std::unordered_map<const TypeVariableId<M>*,std::function<TypeInstance*(C*)>> mapped_;
};

#endif  // DISPATCH_H_
