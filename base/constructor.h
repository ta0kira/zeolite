#ifndef CONSTRUCTOR_H_
#define CONSTRUCTOR_H_

#include "category.h"
#include "core.h"

template<class...Ts>
class TypeConstructor : public TypeCategory {
 public:
  virtual S<TypeInstance> BindAll(const T<Ts...>&) = 0;

  virtual S<TypeInstance> Build(Ts... ts) {
    return BindAll(T_get(ts...));
  }

 protected:
  virtual ~TypeConstructor() = default;
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

#endif  // CONSTRUCTOR_H_
