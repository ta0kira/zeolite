#include <functional>
#include <iostream>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>

#include "base.h"

template<class...Ts>
using T = std::tuple<Ts...>;

template<class...Ts>
T<Ts...> T_get(Ts... ts) { return std::make_tuple(ts...); }


struct TypeInstance {
  virtual std::string TypeName() const = 0;
  virtual ~TypeInstance() = default;
};

template<class...Ts>
struct TypeConstructor {
  virtual S<const TypeInstance> Build(const T<Ts...>&) const = 0;

  S<const TypeInstance> AutoBuild(Ts... ts) const {
    return Build(T_get(ts...));
  }

  virtual ~TypeConstructor() = default;
};


template<int N, class...Ts>
struct ParamInstance {
  using Type = typename ParamInstance<N-1, S<const TypeInstance>, Ts...>::Type;
  using Args = typename ParamInstance<N-1, S<const TypeInstance>, Ts...>::Args;
};

template<class...Ts>
struct ParamInstance<0, Ts...> {
  using Type = TypeConstructor<Ts...>;
  using Args = T<Ts...>;
};


template<int N, int K>
struct Select : public ParamInstance<N>::Type {
  S<const TypeInstance> Build(const typename ParamInstance<N>::Args& args) const final {
    return std::get<K>(args);
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
               return T_get(inner->Build(args)...);
             })
      {}

  S<const TypeInstance> Build(const typename ParamInstance<N>::Args& args) const final {
    return outer_->Build(inner_(args));
  }

 private:
  const S<typename ParamInstance<M>::Type> outer_;
  const std::function<typename ParamInstance<M>::Args(const typename ParamInstance<N>::Args&)> inner_;
};


template<int M, int N, class...Cs>
S<typename ParamInstance<N>::Type> AutoCompose(const S<typename ParamInstance<M>::Type>& outer, Cs... inner) {
  return S_get(new Composer<N, M, Cs...>(outer, inner...));
}

/*

interface Function<x|y> {
}

*/

extern const S<ParamInstance<2>::Type> Function;

class FunctionT : public ParamInstance<2>::Type {
 public:
  S<const TypeInstance> Build(const ParamInstance<2>::Args& args) const final {
    return S_get(new Instance(std::get<0>(args), std::get<1>(args)));
  }

 private:
  class Instance : public TypeInstance {
   public:
    Instance(const S<const TypeInstance>& x, const S<const TypeInstance>& y) : x_(x), y_(y) {}

    std::string TypeName() const final {
      std::ostringstream formatted;
      formatted << "Function<" << x_->TypeName() << "," << y_->TypeName() << ">";
      return formatted.str();
    }

   private:
    const S<const TypeInstance> x_;
    const S<const TypeInstance> y_;
  };
};

const S<ParamInstance<2>::Type> Function(new FunctionT);

/*

interface Data<x> {
}

*/

extern const S<ParamInstance<1>::Type> Data;

class DataT : public ParamInstance<1>::Type {
 public:
  S<const TypeInstance> Build(const ParamInstance<1>::Args& args) const final {
    return S_get(new Instance(std::get<0>(args)));
  }

 private:
  class Instance : public TypeInstance {
   public:
    Instance(const S<const TypeInstance>& x) : x_(x) {}

    std::string TypeName() const final {
      std::ostringstream formatted;
      formatted << "Data<" << x_->TypeName() << ">";
      return formatted.str();
    }

   private:
    const S<const TypeInstance> x_;
  };
};

const S<ParamInstance<1>::Type> Data(new DataT);

/*

interface Value {
}

*/

extern const S<ParamInstance<0>::Type> Value;

class ValueT : public ParamInstance<0>::Type {
 public:
  S<const TypeInstance> Build(const ParamInstance<0>::Args& args) const final {
    return S_get(new Instance());
  }

 private:
  class Instance : public TypeInstance {
   public:
    std::string TypeName() const final {
      return "Value";
    }
  };
};

const S<ParamInstance<0>::Type> Value(new ValueT);

/*

Function<Data<x>,x>

*/

const S<ParamInstance<1>::Type> DataFunction = AutoCompose<2,1>(Function, Data, Select<1,0>::New());

/*

Function<Data<Value>,Value>

*/

// Using composition, e.g., within a function that creates Function<Data<x>,x>.
const S<const TypeInstance> ValueDataFunction = DataFunction->AutoBuild(Value->AutoBuild());

// Without composition, i.e., literally Function<Data<Value>,Value>.
const S<const TypeInstance> ValueDataFunction2 = Function->AutoBuild(Data->AutoBuild(Value->AutoBuild()), Value->AutoBuild());


int main() {
  std::cerr << ValueDataFunction->TypeName() << std::endl;
  std::cerr << ValueDataFunction2->TypeName() << std::endl;
}
