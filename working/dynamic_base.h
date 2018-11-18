#ifndef DYNAMIC_BASE_H_
#define DYNAMIC_BASE_H_

#include <map>
#include <string>

#include "base.h"

struct TypeValue;
struct TypeInstance;

using InstanceCacheKey = std::vector<TypeInstance*>;
using InstanceCache = std::map<InstanceCacheKey,S<TypeInstance>>;

// TODO: Probably needs to use shared_ptr elements so that the types can be
// referenced in conversion wrappers.
using TypeArgs = std::vector<const TypeInstance*>;

class TypeId {
 public:
  inline TypeId(const std::string& name) : name_(name) {}

  inline std::string TypeName() const {
    return name_;
  }

 private:
  const std::string name_;
};

enum class FunctionScope {
  CATEGORY,
  INSTANCE,
  VALUE,
};

template<FunctionScope>
class FunctionId {
 public:
  inline FunctionId(const std::string& name) : name_(name) {}

  inline std::string FunctionName() const {
    return name_;
  }

 private:
  const std::string name_;
};

using FunctionArgs = std::vector<S<TypeValue>>;
using FunctionReturns = std::vector<S<TypeValue>>;


template<class...Ts>
struct TypeConstructor {
  virtual S<TypeInstance> BindAll(const T<Ts...>&) = 0;
  virtual const TypeId* BaseType() const = 0;

  virtual S<TypeInstance> Build(Ts... ts) {
    return BindAll(T_get(ts...));
  }

  virtual FunctionReturns CallCategoryFunction(
      const FunctionId<FunctionScope::CATEGORY>& id, const FunctionArgs&) {
    FAIL() << "Function " << id.FunctionName()
           << " not supported in type-value " << BaseType()->TypeName();
    return FunctionReturns();
  }

  virtual ~TypeConstructor() = default;
};


struct TypeInstance {
  virtual std::string TypeName() const = 0;
  virtual const TypeId* BaseType() const = 0;
  virtual TypeArgs ConstructorArgs() const = 0;
  virtual FunctionReturns CallInstanceFunction(
      const FunctionId<FunctionScope::INSTANCE>& id, const FunctionArgs&);
  virtual ~TypeInstance() = default;
};


struct TypeValue {
  virtual const TypeInstance* ValueType() const = 0;
  virtual S<TypeValue> ConvertTo(const S<const TypeInstance>& type);
  virtual FunctionReturns CallValueFunction(
      const FunctionId<FunctionScope::VALUE>& id, const FunctionArgs&);
  virtual ~TypeValue() = default;
};



template<int N, class...Ts>
struct ParamInstance {
  using Type = typename ParamInstance<N-1, S<TypeInstance>, Ts...>::Type;
  using Args = typename ParamInstance<N-1, S<TypeInstance>, Ts...>::Args;
};

template<class...Ts>
struct ParamInstance<0, Ts...> {
  using Type = TypeConstructor<Ts...>;
  using Args = T<Ts...>;
};


template<int N, int K>
struct Select : public ParamInstance<N>::Type {
  S<TypeInstance> BindAll(const typename ParamInstance<N>::Args& args) final {
    return std::get<K>(args);
  }

  const TypeId* BaseType() const final {
    static const TypeId type("Select");
    return &type;
  }

  static S<typename ParamInstance<N>::Type> New() {
    return S_get(new Select());
  }
};

template<int N, int M, class...Cs>
class Composer : public ParamInstance<N>::Type {
 public:
  Composer(const S<typename ParamInstance<M>::Type>& outer, Cs... inner)
      : outer_(outer),
      inner_([inner...](const typename ParamInstance<N>::Args& args) ->
                 typename ParamInstance<M>::Args {
               return T_get(inner->BindAll(args)...);
             })
      {}

  const TypeId* BaseType() const final {
    return outer_->BaseType();
  }

  S<TypeInstance> BindAll(const typename ParamInstance<N>::Args& args) final {
    return outer_->BindAll(inner_(args));
  }

 private:
  const S<typename ParamInstance<M>::Type> outer_;
  const std::function<typename ParamInstance<M>::Args(const typename ParamInstance<N>::Args&)> inner_;
};


template<int M, int N, class...Cs>
S<typename ParamInstance<N>::Type> AutoCompose(const S<typename ParamInstance<M>::Type>& outer, Cs... inner) {
  return S_get(new Composer<N, M, Cs...>(outer, inner...));
}

#endif  // DYNAMIC_BASE_H_
