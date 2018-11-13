#include <iostream>
#include <memory>

template<class...Ts>
struct I {};

template<>
struct I<> {
  virtual ~I() = default;
};

template<class T0, class...Ts>
struct I<T0,Ts...> : virtual protected I<Ts...> {
  virtual void get(T0&) const = 0;
  virtual ~I() = default;
};

template<class T0, class T1, class...Ts>
struct I<T0,T1,Ts...> : virtual protected I<T1,Ts...> {
  using I<T1,Ts...>::get;
  virtual void get(T0&) const = 0;
  virtual ~I() = default;
};

template<class X, class...Ts>
class I_val {};

template<class X>
class I_val<X> : virtual public I<> {
 public:
  void set(X value) {
    value_ = value;
  }

 protected:
  X value_;
};

template<class X, class T0, class...Ts>
class I_val<X,T0,Ts...> : public I_val<X,Ts...>, virtual public I<T0,Ts...> {
 public:
  void get(T0& var) const final {
    std::cerr << typeid(var).name() << std::endl;
    // This should actually use a converter from X to T0.
    var = I_val<X>::value_;
  }
};

template<class X, class...Ts>
std::unique_ptr<I<Ts...>> I_get(X value) {
  auto val = std::unique_ptr<I_val<X,Ts...>>(new I_val<X,Ts...>);
  val->set(value);
  return val;
}

template<class...Ts>
class U_val {};

template<class T0, class...Ts>
class U_val<T0,Ts...> {
 public:
  void set(T0 value) {
    value_ = value;
  }

  template<class X>
  void get(X& var) const {
    std::cerr << typeid(value_).name() << " = '" << value_ << "'" << std::endl;
    // This should actually use a converter from T0 to X.
    var = value_;
  }

 private:
  T0 value_;
};

template<class T0, class T1, class...Ts>
class U_val<T0,T1,Ts...> : protected U_val<T1,Ts...> {
 public:
  using U_val<T1,Ts...>::get;
  using U_val<T1,Ts...>::set;

  void set(T0 value) {
    value_ = value;
  }

  template<class X>
  void get(X& var) const {
    // This only works if set has been called exactly once!
    if (value_) {
      std::cerr << typeid(value_).name() << " = '" << value_ << "'" << std::endl;
      // This should actually use a converter from T0 to X.
      var = value_;
    } else {
      U_val<T1,Ts...>::get(var);
    }
  }

 private:
  T0 value_;
};

template<class...Ts>
class U {
 public:
  template<class X>
  U(X value) {
    stored_.set(value);
  }

  template<class X>
  void get(X& var) const {
    stored_.get(var);
  }

 private:
  U_val<Ts...> stored_;
};

template<class X, class...Ts>
std::unique_ptr<U<Ts...>> U_get(X value) {
  return std::unique_ptr<U<Ts...>>(new U<Ts...>(value));
}


int main() {
  auto val = I_get<int,long,int,char>(10);
  int x;
  val->get(x);
  long y;
  val->get(y);
  char z;
  val->get(z);

  std::string w;
  // Error! Cannot convert *any* of the other types to string.
  // val->get(w);


  // Error! Cannot convert string to the other types.
  // auto val2 = create<std::string,long,int,char>("");

  auto val2 = U_get<int,long,int,std::string>(10);
  val2 = U_get<std::string,long,int,std::string>("");

  // Error! Cannot convert *all* of the other types to int.
  // val->get(x);

  auto val3 = U_get<int,long,int,char>(10);
  val3->get(x);
  val3 = U_get<long,long,int,char>(10L);
  val3->get(x);
  val3 = U_get<char,long,int,char>('x');
  val3->get(x);
}
