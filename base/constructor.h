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


template<class I>
class InstanceCache {
 public:
  template<class...Ts>
  R<I>& Create(const Ts&... ts) {
    return cache_[TypeArgs{&ts...}];
  }

 private:
  std::map<TypeArgs,R<I>> cache_;
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
