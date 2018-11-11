#include <memory>

template<class x, class y>
struct Adapter {
  virtual y operator ()(x) const = 0;
  virtual ~Adapter() = default;
};

template<class x, class y>
struct Coadapter {
  virtual x operator ()(y&) const = 0;
  virtual ~Coadapter() = default;
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

// TODO: Decide when/how to share pointers to objects, rather than always
// passing ownership.

template<class T>
using R = std::unique_ptr<T>;

template<class T>
inline R<T> R_get(T* val) { return R<T>(val); }

template<class x>
struct IdAdapter : public Adapter<x,x> {
  static R<const Adapter<x,x>> create() { return R_get(new IdAdapter); }
  // Ownership is passed.
  x operator ()(x value) const final { return std::move(value); }
};

template<class x>
struct IdCoadapter : public Coadapter<x,x> {
  static R<const Coadapter<x,x>> create() { return R_get(new IdCoadapter); }
  // Ownership is not passed.
  x operator ()(x& value) const final { return value; }
};

template<class x>
class CopiedVariable : public Variable<x> {
 public:
  x get() const final { return value_; }
  void set(x value) final { value_ = value; }

 private:
  x value_;
};

/*

interface Function<x|y> {
  call takes (x) to (y)
}

*/

template<class x, class y>
class Caller_Function_call {
 public:
  virtual void set_a0(x a0) = 0;
  virtual void execute() = 0;
  virtual y get_r0() const = 0;
  virtual ~Caller_Function_call() = default;
};

template<class x, class y>
class Interface_Function {
 public:
  virtual ~Interface_Function() = default;

  template<class x2, class y2>
  R<Interface_Function<x2,y2>> Convert_Function(
    R<const Coadapter<x,x2>> conv_x,
    R<const Adapter<y,y2>> conv_y) {
    return R_get(new Adapter_Function<x2,y2>(this, std::move(conv_x), std::move(conv_y)));
  }

  y Call_Function_call(x a0) {
    const auto caller = New_Caller_Function_call();
    caller->set_a0(a0);
    caller->execute();
    return caller->get_r0();
  }

 protected:
  virtual R<Caller_Function_call<x,y>> New_Caller_Function_call() = 0;

  template<class x2, class y2>
  class Adapter_Function : public Interface_Function<x2,y2> {
   public:
    Adapter_Function(
      Interface_Function* object,
      R<const Coadapter<x,x2>> conv_x,
      R<const Adapter<y,y2>> conv_y)
        : object_(object), conv_x_(std::move(conv_x)), conv_y_(std::move(conv_y)) {}

   protected:
    R<Caller_Function_call<x2,y2>> New_Caller_Function_call() final {
      return R_get(new Adapter_Function_call<x2,y2>(
        object_->New_Caller_Function_call(), conv_x_.get(), conv_y_.get()));
    }

   private:
    Interface_Function<x,y>* const object_;
    // Converters are per free param, but only those that can vary.
    const R<const Coadapter<x,x2>> conv_x_;
    const R<const Adapter<y,y2>> conv_y_;
  };

  template<class x2, class y2>
  class Adapter_Function_call : public Caller_Function_call<x2,y2> {
   public:
    Adapter_Function_call(
      R<Caller_Function_call<x,y>> caller,
      const Coadapter<x,x2>* conv_a0,
      const Adapter<y,y2>* conv_r0)
        : caller_(std::move(caller)), conv_a0_(conv_a0), conv_r0_(conv_r0) {}

    void set_a0(x2 a0) final { caller_->set_a0((*conv_a0_)(a0)); }
    void execute() final { caller_->execute(); }
    y2 get_r0() const final { return (*conv_r0_)(caller_->get_r0()); }

   private:
    const R<Caller_Function_call<x,y>> caller_;
    // Converters are per arg and return value.
    const Coadapter<x,x2>* const conv_a0_;
    const Adapter<y,y2>* const conv_r0_;
  };
};

/*

concrete CountedId<x> {
  refines Function<x,x>
  count takes () to (Int)
}

Members:

- Int counter

Implementations:

call(val) { counter += 1; return val; }
count() { return counter; }

*/

struct Caller_CountedId_count {
  virtual void execute() = 0;
  virtual int get_r0() const = 0;
  virtual ~Caller_CountedId_count() = default;
};

template<class x>
class Interface_CountedId : public Interface_Function<x,x> {
 public:
  virtual ~Interface_CountedId() = default;

  // Concrete class with no variant params => no conversions gets generated.

  x Call_CountedId_count() {
    const auto caller = New_Caller_CountedId_count();
    caller->execute();
    return caller->get_r0();
  }

 protected:
  virtual R<Caller_CountedId_count> New_Caller_CountedId_count() = 0;
};

template<class x>
class Concrete_CountedId : public Interface_CountedId<x> {
 public:
  // (Not sure how this construction should actually happen.)
  static R<Interface_CountedId<x>> create() {
    return R_get(new Concrete_CountedId<x>);
  }

 protected:
  R<Caller_Function_call<x,x>> New_Caller_Function_call() final {
    return R_get(new Implemented_CountedId_call(this));
  }

  R<Caller_CountedId_count> New_Caller_CountedId_count() final {
    return R_get(new Implemented_CountedId_count(this));
  }

 private:
  Concrete_CountedId() = default;

  class Implemented_CountedId_call : public Caller_Function_call<x,x> {
   public:
    Implemented_CountedId_call(Concrete_CountedId* self) : self_(self) {}

    void set_a0(x a0) final {
      a0_.set(a0);
    }

    void execute() final {
      // Implementation of CountedId.call.
      self_->member_counter_.set(self_->member_counter_.get()+1);
      r0_.set(a0_.get());
    }

    x get_r0() const final {
      return r0_.get();
    }

   private:
    Concrete_CountedId* const self_;
    // TODO: We should use a special variable for when the member depends on a
    // param, e.g., if we need to reason about filters. Maybe we need some sort
    // of param wrapper that converts based on filters? Better yet, require the
    // caller to provide a converter. For example, if we have x -> Value then
    // the caller should pass something that converts their type for x to Value.
    CopiedVariable<x> a0_;
    CopiedVariable<x> r0_;
  };

  class Implemented_CountedId_count : public Caller_CountedId_count {
   public:
    Implemented_CountedId_count(Concrete_CountedId* self) : self_(self) {}

    void execute() final {
      // Implementation of CountedId.call.
      r0_.set(self_->member_counter_.get());
    }

    int get_r0() const final {
      return r0_.get();
    }

   private:
    Concrete_CountedId* const self_;
    // TODO: We really shouldn't be storing a raw type here.
    CopiedVariable<int> r0_;
  };

  // Member variable CountedId.counter.
  CopiedVariable<int> member_counter_;
};

// For use with returned objects, i.e., ownership is passed.
template<class x>
struct Adapter_CountedId_to_Function : public Adapter<R<Interface_CountedId<x>>,R<Interface_Function<x,x>>> {
  static R<const Adapter<R<Interface_CountedId<x>>,R<Interface_Function<x,x>>>> create() {
    return R_get(new Adapter_CountedId_to_Function);
  }

  virtual R<Interface_Function<x,x>> operator ()(R<Interface_CountedId<x>> object) const {
    return object;
  }
};

// For use with arg objects, i.e., ownership is not passed.
template<class x>
struct Coadapter_CountedId_to_Function : public Coadapter<R<Interface_Function<x,x>>,R<Interface_CountedId<x>>> {
  static R<const Coadapter<R<Interface_Function<x,x>>,R<Interface_CountedId<x>>>> create() {
    return R_get(new Coadapter_CountedId_to_Function);
  }

  virtual R<Interface_Function<x,x>> operator ()(R<Interface_CountedId<x>>& object) const {
    return object->Convert_Function(IdCoadapter<x>::create(), IdAdapter<x>::create());
  }
};

/*
Testing
*/

#include <iostream>
#include <sstream>
#include <string>

struct Adapter_int_to_string : public Adapter<int,std::string> {
  static R<const Adapter<int,std::string>> create() { return R_get(new Adapter_int_to_string); }

  virtual std::string operator ()(int value) const {
    std::ostringstream out;
    out << value;
    return out.str();
  }
};

struct Coadapter_int_to_long : public Coadapter<int,long> {
  static R<const Coadapter<int,long>> create() { return R_get(new Coadapter_int_to_long); }

  virtual int operator ()(long& value) const {
    return value;
  }
};

int main() {
  const R<Interface_CountedId<int>> counted =
    Concrete_CountedId<int>::create();
  const R<Interface_Function<long,std::string>> function =
    counted->Convert_Function(Coadapter_int_to_long::create(), Adapter_int_to_string::create());
  std::cerr << "Count: " << counted->Call_CountedId_count() << std::endl;
  std::cerr << "Call: " << counted->Call_Function_call(3) << std::endl;
  std::cerr << "Call: " << function->Call_Function_call(4L).append("!") << std::endl;
  std::cerr << "Count: " << counted->Call_CountedId_count() << std::endl;
}
