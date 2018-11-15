#ifndef BASE_H_
#define BASE_H_

#include <memory>

// TODO: Be consistent with case for base functions and template params.

template<class T>
using R = std::unique_ptr<T>;

template<class T>
inline R<T> R_get(T* val) { return R<T>(val); }

template<class T>
using S = std::shared_ptr<T>;

template<class T>
inline S<T> S_get(T* val) { return S<T>(val); }


template<class x, class y>
struct Adapter {
  static constexpr bool defined = false;
  // static y Convert(x value) { ... }
};

template<class x>
struct Adapter<x,x> {
  static constexpr bool defined = true;
  static const x& Convert(const x& value) { return value; }
};

template<class y>
struct ConvertTo {
  template<class x>
  static y From(x value) {
    return Adapter<x,y>::Convert(std::move(value));
  }
};


template<class x>
struct Missing {
  static constexpr bool defined = false;
  // static bool IsMissing(x value) { ... }
};


template<class x>
struct ReadVariable {
  virtual x get() const = 0;
};

template<class x>
struct Variable : public ReadVariable<x> {
  virtual void set(x) = 0;
  virtual ~Variable() = default;
};


template<class...Ts>
class I {};

template<bool D>
class Intersect {};

template<class T0>
class I<T0> {
 public:
  virtual ~I() = default;

  template<class X>
  void get(X& var) const {
    Intersect<Adapter<T0,X>::defined>::Convert(*this, var);
  }

 private:
  virtual void exact(T0&) const = 0;
  template<bool D> friend class Intersect;
};

template<class T0, class T1, class...Ts>
class I<T0,T1,Ts...> : virtual protected I<T1,Ts...> {
 public:
  using I<T1,Ts...>::get;
  virtual ~I() = default;

  template<class X>
  void get(X& var) const {
    Intersect<Adapter<T0,X>::defined>::Convert(*this, var);
  }

 private:
  virtual void exact(T0&) const = 0;
  template<bool D> friend class Intersect;
};

template<>
class Intersect<true> {
  template<class X, class T0, class...Ts>
  static void Convert(const I<T0,Ts...>& from, X& var) {
    T0 val0;
    from.exact(val0);
    var = ConvertTo<X>::From(val0);
  }

  template<class...Ts> friend class I;
  template<bool D> friend class Intersect;
};

template<>
struct Intersect<false> {
  template<class X, class T0, class T1, class...Ts>
  static void Convert(const I<T0,T1,Ts...>& from, X& var) {
    // NOTE: This does a cast I<T0,T1,Ts...> -> I<T1,Ts...>.
    Intersect<Adapter<T1,X>::defined>::template Convert<X,T1,Ts...>(from, var);
  }

  template<class...Ts> friend class I;
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
 private:
  void exact(T0& var) const final {
    var = ConvertTo<T0>::From(I_val<X>::value_);
  }
};

// When converting from, use Adapter.
template<class X, class...Ts>
struct Adapter<S<I<Ts...>>,X> {
  static constexpr bool defined = true;
  static X Convert(const S<I<Ts...>>& value) {
    X var;
    value->get(var);
    return var;
  }
};

// When converting to, construct a new instance.
template<class...Ts>
struct I_get {
  template<class X>
  static S<I<Ts...>> From(X value) {
    auto val = S<I_val<X,Ts...>>(new I_val<X,Ts...>);
    val->set(value);
    return val;
  }
};

template<class...Ts>
struct ConvertTo<S<I<Ts...>>> {
  template<class x>
  static S<I<Ts...>> From(const x& value) {
    return I_get<Ts...>::From(value);
  }
};


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
    var = ConvertTo<X>::From(value_);
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
    // NOTE: This only works if set has been called exactly once!
    if (value_) {
      var = ConvertTo<X>::From(value_);
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

template<class...Ts>
struct U_get {
  template<class X>
  static S<U<Ts...>> From(X value) {
    return S<U<Ts...>>(new U<Ts...>(value));
  }
};

// When converting from, use Adapter.
template<class X, class...Ts>
struct Adapter<S<U<Ts...>>,X> {
  static constexpr bool defined = true;
  static X Convert(const S<U<Ts...>>& value) {
    X var;
    value->get(var);
    return var;
  }
};

// When converting to, construct a new instance.
template<class...Ts>
struct ConvertTo<S<U<Ts...>>> {
  template<class x>
  static S<U<Ts...>> From(const x& value) {
    return U_get<Ts...>::From(value);
  }
};

#endif  // BASE_H_
