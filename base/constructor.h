#ifndef CONSTRUCTOR_H_
#define CONSTRUCTOR_H_

#include <map>
#include <sstream>
#include <vector>

#include "category.h"
#include "core.h"

template<class...Ts>
class TypeConstructor : public TypeCategory {
 public:
  virtual TypeInstance& Build(Ts&... ts) = 0;

 protected:
  virtual ~TypeConstructor() = default;
};

template<int N, class...Ts>
struct ParamInstance {
  using Type = typename ParamInstance<N-1, TypeInstance, Ts...>::Type;
};

template<class...Ts>
struct ParamInstance<0, Ts...> {
  using Type = TypeConstructor<Ts...>;
};


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


template<class I>
class InstanceCache {
 public:
  template<class...Ts>
  R<I>& Create(Ts&... ts) {
    return Create(TypeArgs{&ts...});
  }

  R<I>& Create(const TypeArgs& key) {
    const auto cached = cache_.find(key);
    if (cached != cache_.end()) {
      FAIL_IF(!cached->second) << "Cycle while creating type instance";
      return cached->second;
    } else {
      return cache_[key];
    }
  }

 private:
  std::map<TypeArgs,R<I>> cache_;
};


class ParentTypes {
 public:
  template<class...Ts>
  ParentTypes& AddParent(const TypeCategory& parent, Ts&... ts) {
    return AddParent(parent,TypeArgs{&ts...});
  }

  inline ParentTypes& AddParent(const TypeCategory& parent, const TypeArgs& types) {
    parents_[&parent] = types;
    return *this;
  }

  inline bool HasParent(const TypeCategory& parent) const {
    return parents_.find(&parent) != parents_.end();
  }

  inline const TypeArgs& GetParent(const TypeCategory& parent) const {
    return parents_.find(&parent)->second;
  }

 private:
  std::map<const TypeCategory*,TypeArgs> parents_;
};


template<class...Ts>
std::string ConstructInstanceName(const TypeCategory& category, const Ts&... types) {
  const std::vector<const TypeInstance*> args{&types...};
  if (args.empty()) {
    return category.CategoryName();
  }
  std::ostringstream formatted;
  formatted << category.CategoryName() << "<";
  for (int i = 0; i < args.size(); ++i) {
    if (i > 0) {
      formatted << ",";
    }
    formatted << args[i]->InstanceName();
  }
  formatted << ">";
  return formatted.str();
}

#endif  // CONSTRUCTOR_H_
