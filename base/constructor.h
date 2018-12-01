#ifndef CONSTRUCTOR_H_
#define CONSTRUCTOR_H_

#include "category.h"
#include "core.h"

template<class...Ts>
class TypeConstructor : public TypeCategory {
 public:
  virtual TypeInstance& BindAll(const T<Ts*...>&) = 0;

  virtual TypeInstance& Build(Ts&... ts) {
    return BindAll(T_get(&ts...));
  }

 protected:
  virtual ~TypeConstructor() = default;
};

template<int N, class...Ts>
struct ParamInstance {
  using Type = typename ParamInstance<N-1, TypeInstance, Ts...>::Type;
  using Args = typename ParamInstance<N-1, TypeInstance, Ts...>::Args;
};

template<class...Ts>
struct ParamInstance<0, Ts...> {
  using Type = TypeConstructor<Ts...>;
  using Args = const T<Ts*...>&;
};

#endif  // CONSTRUCTOR_H_
