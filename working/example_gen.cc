#include <memory>

#include "base.h"

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
  S<Interface_Function<x2,y2>> Convert_Function() {
    return S_get(new Adapter_Function<x2,y2>(this));
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
    Adapter_Function(Interface_Function* object)
        : object_(object) {}

   protected:
    R<Caller_Function_call<x2,y2>> New_Caller_Function_call() final {
      return R_get(new Adapter_Function_call<x2,y2>(
        object_->New_Caller_Function_call()));
    }

   private:
    // TODO: This should also be shared, in case this adapter outlives the
    // original object, e.g., is used for returning a local value.
    Interface_Function<x,y>* const object_;
  };

  template<class x2, class y2>
  class Adapter_Function_call : public Caller_Function_call<x2,y2> {
   public:
    Adapter_Function_call(
      R<Caller_Function_call<x,y>> caller)
        : caller_(std::move(caller)) {}

    void set_a0(x2 a0) final {
      caller_->set_a0(ConvertTo<x>::From(a0));
    }

    void execute() final { caller_->execute(); }

    y2 get_r0() const final {
      return ConvertTo<y2>::From(caller_->get_r0());
    }

   private:
    const R<Caller_Function_call<x,y>> caller_;
  };
};

template<class x1, class y1, class x2, class y2>
struct Adapter<S<Interface_Function<x1,y1>>,S<Interface_Function<x2,y2>>> {
  static constexpr bool defined = true;
  static S<Interface_Function<x2,y2>> convert(S<Interface_Function<x1,y1>> value) {
    // Same type => convert with a wrapper.
    return value->template Convert_Function<x2,y2>();
  }
};

/*

concrete CountedId<x> {
  x allows missing
  // TODO: This is contrived, but is just for demo. Since Int is concrete, x
  // can't really be anything else.
  x allows Int
  refines Function<x,x>
  count takes () to (Int)
}

Members:

- Int counter

Implementations:

call(val) {
  if (!is_missing(val)) {  // <- x allows missing
    counter += 1;
    return val;
  } else {
    return -1;  // <- x allows Int
  }
}
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

template<class x1, class x2>
struct Adapter<S<Interface_CountedId<x1>>,S<Interface_CountedId<x2>>> {
  static constexpr bool defined = false;
  // No possible conversions => explicitly make it unimplemented.
};

template<class x1, class x2, class y2>
struct Adapter<S<Interface_CountedId<x1>>,S<Interface_Function<x2,y2>>> {
  static constexpr bool defined = true;
  static S<Interface_Function<x2,y2>> convert(S<S<Interface_CountedId<x1>>> value) {
    // Different type => cast first, then convert.
    return Adapter<S<Interface_Function<x1,x1>>,S<Interface_Function<x2,y2>>>::Convert(value);
  }
};

template<class x>
class Concrete_CountedId : public Interface_CountedId<x> {
 public:
  // (Not sure how this construction should actually happen.)
  static S<Interface_CountedId<x>> create() {
    return S_get(new Concrete_CountedId<x>());
  }

 protected:
  R<Caller_Function_call<x,x>> New_Caller_Function_call() final {
    return R_get(new Implemented_CountedId_call(this));
  }

  R<Caller_CountedId_count> New_Caller_CountedId_count() final {
    return R_get(new Implemented_CountedId_count(this));
  }

 private:
  Concrete_CountedId() {}

  class Implemented_CountedId_call : public Caller_Function_call<x,x> {
   public:
    Implemented_CountedId_call(Concrete_CountedId* self) : self_(self) {}

    void set_a0(x a0) final {
      a0_.set(a0);
    }

    void execute() final {
      // Implementation of CountedId.call.
      if (!Missing<x>::IsMissing(a0_.get())) {
        self_->member_counter_.set(self_->member_counter_.get()+1);
        r0_.set(a0_.get());
      } else {
        r0_.set(ConvertTo<x>::From(-1));
      }
    }

    x get_r0() const final {
      return r0_.get();
    }

   private:
    Concrete_CountedId* const self_;
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

/*
Testing
*/

#include <iostream>
#include <sstream>
#include <string>

template<>
struct Adapter<int,std::string> {
  static constexpr bool defined = true;
  static std::string Convert(int value) {
    std::ostringstream out;
    out << value;
    return out.str();
  }
};

template<>
struct Adapter<long,int> {
  static constexpr bool defined = true;
  static int Convert(long value) { return value; }
};

template<>
struct Missing<int> {
  static constexpr bool defined = false;
  static bool IsMissing(int value) { return value == 0; }
};

template<>
struct Missing<long> {
  static constexpr bool defined = false;
  static bool IsMissing(long value) { return value == 0L; }
};

int main() {
  const S<Interface_CountedId<int>> counted =
    Concrete_CountedId<int>::create();

  const S<Interface_Function<long,std::string>> function =
    counted->Convert_Function<long,std::string>();

  std::cerr << "Count: " << counted->Call_CountedId_count() << std::endl;
  std::cerr << "Call: " << counted->Call_Function_call(3) << std::endl;
  std::cerr << "Call: " << counted->Call_Function_call(0) << std::endl;
  std::cerr << "Call: " << function->Call_Function_call(4L).append("!") << std::endl;
  std::cerr << "Count: " << counted->Call_CountedId_count() << std::endl;
}
