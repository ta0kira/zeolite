#ifndef DYNAMIC_BASE_H_
#define DYNAMIC_BASE_H_

#include <map>
#include <string>

#include "base.h"

struct TypeValue;
struct TypeInstance;

using InstanceCacheKey = std::vector<TypeInstance*>;
template<class T>
using InstanceCache = std::map<InstanceCacheKey,S<T>>;

using TypeArgs = std::vector<const TypeInstance*>;

class CategoryId {
 public:
  inline CategoryId(const std::string& name) : name_(name) {}

  inline std::string TypeName() const {
    return name_;
  }

 private:
  const std::string name_;
};

enum class MemberScope {
  CATEGORY,
  INSTANCE,
  VALUE,
};

template<MemberScope>
class FunctionId {
 public:
  inline FunctionId(const std::string& name) : name_(name) {}

  inline std::string FunctionName() const {
    return name_;
  }

 private:
  const std::string name_;
};

// NOTE: Even though variables can exist at the category/instance level, there
// should be no non-local access.
class ValueVariableId {
 public:
  inline ValueVariableId(const std::string& name) : name_(name) {}

  inline std::string ValueName() const {
    return name_;
  }

 private:
  const std::string name_;
};

// NOTE: Even though variables can exist at the category/instance level, there
// should be no non-local access.
class TypeVariableId {
 public:
  inline TypeVariableId(const std::string& name) : name_(name) {}

  inline std::string TypeName() const {
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
  virtual const CategoryId* CategoryType() const = 0;

  virtual S<TypeInstance> Build(Ts... ts) {
    return BindAll(T_get(ts...));
  }

  virtual FunctionReturns CallCategoryFunction(
      const FunctionId<MemberScope::CATEGORY>& id, const FunctionArgs&) {
    FAIL() << "Function " << id.FunctionName()
           << " not supported in type-value " << CategoryType()->TypeName();
    return FunctionReturns();
  }

  virtual ~TypeConstructor() = default;
};


enum class MergeType {
  SINGLE,
  UNION,
  INTERSECT,
};

class TypeInstance {
 public:
  virtual std::string TypeName() const = 0;
  virtual const TypeArgs& TypeArgsForCategory(const CategoryId*) const;
  virtual FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>& id, const FunctionArgs&);

  static bool CheckConversionBetween(const TypeInstance*, const TypeInstance*);

  virtual ~TypeInstance() = default;

 protected:
  virtual bool CheckConversionTo(const TypeInstance*) const;
  virtual MergeType MergedType() const;
  virtual std::vector<const TypeInstance*> MergedInstanceTypes() const;
};

using IntType = signed long long;
using FloatType = double;
using StringType = std::string;

class ValueVariable {};

class TypeValue {
 public:
  virtual FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id, const FunctionArgs&);
  virtual ValueVariable& GetValueVariable(const ValueVariableId& id);
  virtual S<TypeInstance> GetTypeVariable(const TypeVariableId& id);

  virtual bool IsOptional() const;
//   virtual IntType& AsInt();
//   virtual FloatType& AsFloat();
//   virtual StringType& AsFloat();

  static S<TypeValue> ConvertTo(const S<TypeValue>& self,
                                const S<TypeInstance>& category);

  static S<TypeValue> ReduceTo(const S<TypeValue>& self,
                               const S<TypeInstance>& category);

  virtual ~TypeValue() = default;

 protected:
  virtual const TypeInstance* InstanceType() const = 0;
  virtual S<TypeValue> ConvertTo(const S<TypeInstance>& category);
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

  const CategoryId* CategoryType() const final {
    static const CategoryId type("Select");
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

  const CategoryId* CategoryType() const final {
    return outer_->CategoryType();
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
