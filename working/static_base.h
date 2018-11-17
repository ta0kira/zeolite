#ifndef STATIC_BASE_H_
#define STATIC_BASE_H_

#include "base.h"

template<class X, class Y>
struct Adapter {
  static constexpr bool defined = false;
  // static Y Convert(const X& value) { ... }
};

template<class X>
struct Adapter<X,X> {
  static constexpr bool defined = true;
  static const X& Convert(X value) { return std::move(value); }
};

template<class Y>
struct ConvertTo {
  template<class X>
  static Y From(X value) {
    return Adapter<X,Y>::Convert(std::move(value));
  }
};


template<class X>
struct Missing {
  static constexpr bool defined = false;
  // static bool IsMissing(const X& value) { ... }
};


template<class X>
struct ReadVariable {
  virtual X Get() const = 0;
};

template<class X>
struct Variable : public ReadVariable<X> {
  virtual void Set(const X&) = 0;
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
  void Get(X& var) const {
    Intersect<Adapter<T0,X>::defined>::Convert(*this, var);
  }

 private:
  virtual void Exact(T0&) const = 0;
  template<bool D> friend class Intersect;
};

template<class T0, class T1, class...Ts>
class I<T0,T1,Ts...> : virtual protected I<T1,Ts...> {
 public:
  using I<T1,Ts...>::Get;
  virtual ~I() = default;

  template<class X>
  void Get(X& var) const {
    Intersect<Adapter<T0,X>::defined>::Convert(*this, var);
  }

 private:
  virtual void Exact(T0&) const = 0;
  template<bool D> friend class Intersect;
};

template<>
class Intersect<true> {
  template<class X, class T0, class...Ts>
  static void Convert(const I<T0,Ts...>& from, X& var) {
    T0 val0;
    from.Exact(val0);
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
  void Set(const X& value) {
    value_ = value;
  }

 protected:
  X value_;
};

template<class X, class T0, class...Ts>
class I_val<X,T0,Ts...> : public I_val<X,Ts...>, virtual public I<T0,Ts...> {
 private:
  void Exact(T0& var) const final {
    var = ConvertTo<T0>::From(I_val<X>::value_);
  }
};

// When converting from, use Adapter.
template<class X, class...Ts>
struct Adapter<S<I<Ts...>>,X> {
  static constexpr bool defined = true;
  static X Convert(const S<I<Ts...>>& value) {
    X var;
    value->Get(var);
    return var;
  }
};

// When converting to, construct a new instance.
template<class...Ts>
struct I_get {
  template<class X>
  static S<I<Ts...>> From(X value) {
    auto val = S<I_val<X,Ts...>>(new I_val<X,Ts...>);
    val->Set(value);
    return val;
  }
};

template<class...Ts>
struct ConvertTo<S<I<Ts...>>> {
  template<class X>
  static S<I<Ts...>> From(const X& value) {
    return I_get<Ts...>::From(value);
  }
};


template<class...Ts>
class U_val {};

template<class T0, class...Ts>
class U_val<T0,Ts...> {
 public:
  void Set(const T0& value) {
    value_ = value;
  }

  template<class X>
  void Get(X& var) const {
    var = ConvertTo<X>::From(value_);
  }

 private:
  T0 value_;
};

template<class T0, class T1, class...Ts>
class U_val<T0,T1,Ts...> : protected U_val<T1,Ts...> {
 public:
  using U_val<T1,Ts...>::Get;
  using U_val<T1,Ts...>::Set;

  void Set(const T0& value) {
    value_ = value;
  }

  template<class X>
  void Get(X& var) const {
    // NOTE: This only works if set has been called exactly once!
    if (value_) {
      var = ConvertTo<X>::From(value_);
    } else {
      U_val<T1,Ts...>::Get(var);
    }
  }

 private:
  T0 value_;
};

template<class...Ts>
class U {
 public:
  template<class X>
  U(const X& value) {
    stored_.Set(value);
  }

  template<class X>
  void Get(X& var) const {
    stored_.Get(var);
  }

 private:
  U_val<Ts...> stored_;
};

template<class...Ts>
struct U_get {
  template<class X>
  static S<U<Ts...>> From(const X& value) {
    return S<U<Ts...>>(new U<Ts...>(value));
  }
};

// When converting from, use Adapter.
template<class X, class...Ts>
struct Adapter<S<U<Ts...>>,X> {
  static constexpr bool defined = true;
  static X Convert(const S<U<Ts...>>& value) {
    X var;
    value->Get(var);
    return var;
  }
};

// When converting to, construct a new instance.
template<class...Ts>
struct ConvertTo<S<U<Ts...>>> {
  template<class X>
  static S<U<Ts...>> From(const X& value) {
    return U_get<Ts...>::From(value);
  }
};

#endif  // STATIC_BASE_H_
