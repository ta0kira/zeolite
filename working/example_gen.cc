#include <memory>

#include "base.h"

template<class X>
class CopiedVariable : public Variable<X> {
 public:
  X Get() const final { return value_; }
  void Set(const X& value) final { value_ = value; }

 private:
  X value_;
};

/*

interface Function<x|y> {
  call takes (x) to (y)
}

*/

template<class x, class y>
class Caller_Function_call {
 public:
  virtual void Set_a0(const x& a0) = 0;
  virtual void Execute() = 0;
  virtual y Get_r0() const = 0;
  virtual ~Caller_Function_call() = default;
};

template<class x, class y>
class Interface_Function {
 public:
  virtual ~Interface_Function() = default;

  // This would also include abstract functions to convert to bases of Function.

  y Call_Function_call(const x& a0) {
    const auto caller = New_Caller_Function_call();
    caller->Set_a0(a0);
    caller->Execute();
    return caller->Get_r0();
  }

 protected:
  virtual R<Caller_Function_call<x,y>> New_Caller_Function_call() = 0;

  template<class x1, class y1, class x2, class y2>
  friend class Adapter_Function;
};

template<class x1, class y1, class x2, class y2>
class Adapter_Function : public Interface_Function<x2,y2> {
 public:
  Adapter_Function(S<Interface_Function<x1,y1>> object)
      : object_(object) {}

 protected:
  R<Caller_Function_call<x2,y2>> New_Caller_Function_call() final {
    return R_get(new Adapter_Function_call(
      object_->New_Caller_Function_call()));
  }

 private:
  class Adapter_Function_call : public Caller_Function_call<x2,y2> {
   public:
    Adapter_Function_call(
      R<Caller_Function_call<x1,y1>> caller)
        : caller_(std::move(caller)) {}

    void Set_a0(const x2& a0) final {
      caller_->Set_a0(ConvertTo<x1>::From(a0));
    }

    void Execute() final { caller_->Execute(); }

    y2 Get_r0() const final {
      return ConvertTo<y2>::From(caller_->Get_r0());
    }

   private:
    const R<Caller_Function_call<x1,y1>> caller_;
  };

  const S<Interface_Function<x1,y1>> object_;
};

template<class x1, class y1, class x2, class y2>
struct Adapter<S<Interface_Function<x1,y1>>,S<Interface_Function<x2,y2>>> {
  static constexpr bool defined = true;
  static S<Interface_Function<x2,y2>> Convert(S<Interface_Function<x1,y1>> value) {
    return S_get(new Adapter_Function<x1,y1,x2,y2>(value));
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

template<class x>
struct Caller_CountedId_call {
 public:
  virtual void Set_a0(const x& a0) = 0;
  virtual void Execute() = 0;
  virtual x Get_r0() const = 0;
  virtual ~Caller_CountedId_call() = default;
};

template<class x>
struct Caller_CountedId_count {
  virtual void Execute() = 0;
  virtual int Get_r0() const = 0;
  virtual ~Caller_CountedId_count() = default;
};

template<class x>
class Interface_CountedId {
 public:
  virtual ~Interface_CountedId() = default;

  virtual S<Interface_Function<x,x>> Convert_Function() = 0;

  x Call_CountedId_call(const x& a0) {
    const auto caller = New_Caller_CountedId_call();
    caller->Set_a0(a0);
    caller->Execute();
    return caller->Get_r0();
  }

  x Call_CountedId_count() {
    const auto caller = New_Caller_CountedId_count();
    caller->Execute();
    return caller->Get_r0();
  }

 protected:
  virtual R<Caller_CountedId_call<x>> New_Caller_CountedId_call() = 0;
  virtual R<Caller_CountedId_count<x>> New_Caller_CountedId_count() = 0;
};

template<class x1, class x2>
struct Adapter<S<Interface_CountedId<x1>>,S<Interface_CountedId<x2>>> {
  static constexpr bool defined = false;
  // No possible conversions => explicitly make it unimplemented.
};

template<class x1, class x2, class y2>
struct Adapter<S<Interface_CountedId<x1>>,S<Interface_Function<x2,y2>>> {
  static constexpr bool defined = true;
  static S<Interface_Function<x2,y2>> Convert(const S<Interface_CountedId<x1>>& value) {
    return ConvertTo<S<Interface_Function<x2,y2>>>::From(value->Convert_Function());
  }
};

template<class x>
class Concrete_CountedId : public Interface_CountedId<x> {
 public:
  // (Not sure how this construction should actually happen.)
  static S<Interface_CountedId<x>> create() {
    return S_get(new Concrete_CountedId<x>());
  }

  S<Interface_Function<x,x>> Convert_Function() final {
    return S_get(new Implemented_Function(data_));
  }

 protected:
  R<Caller_CountedId_call<x>> New_Caller_CountedId_call() final {
    return R_get(new Implemented_CountedId_call(data_));
  }

  R<Caller_CountedId_count<x>> New_Caller_CountedId_count() final {
    return R_get(new Implemented_CountedId_count(data_));
  }

 private:
  Concrete_CountedId() {}

  struct Data_CountedId {
    // Member variable CountedId.counter.
    // TODO: We really shouldn't be storing a raw type here.
    CopiedVariable<int> member_counter_;
  };

  class Implemented_Function : public Interface_Function<x,x> {
   public:
    Implemented_Function(const S<Data_CountedId> data) : data_(data) {}

   protected:
    R<Caller_Function_call<x,x>> New_Caller_Function_call() final {
      return R_get(new Implemented_Function_call(data_));
    }

   private:
    const S<Data_CountedId> data_;
  };

  class Implemented_CountedId_call : public Caller_CountedId_call<x> {
   public:
    Implemented_CountedId_call(const S<Data_CountedId> data) : data_(data) {}

    void Set_a0(const x& a0) final {
      a0_.Set(a0);
    }

    void Execute() final {
      // Implementation of CountedId.call.
      if (!Missing<x>::IsMissing(a0_.Get())) {
        data_->member_counter_.Set(data_->member_counter_.Get()+1);
        r0_.Set(a0_.Get());
      } else {
        r0_.Set(ConvertTo<x>::From(-1));
      }
    }

    x Get_r0() const final {
      return r0_.Get();
    }

   private:
    const S<Data_CountedId> data_;
    CopiedVariable<x> a0_;
    CopiedVariable<x> r0_;
  };

  class Implemented_Function_call : public Caller_Function_call<x,x> {
   public:
    Implemented_Function_call(const S<Data_CountedId> data) : caller_(data) {}

    void Set_a0(const x& a0) final {
      caller_.Set_a0(ConvertTo<x>::From(a0));
    }

    void Execute() final {
      caller_.Execute();
    }

    x Get_r0() const final {
      return ConvertTo<x>::From(caller_.Get_r0());
    }

   private:
    Implemented_CountedId_call caller_;
  };

  class Implemented_CountedId_count : public Caller_CountedId_count<x> {
   public:
    Implemented_CountedId_count(const S<Data_CountedId> data) : data_(data) {}

    void Execute() final {
      // Implementation of CountedId.call.
      r0_.Set(data_->member_counter_.Get());
    }

    int Get_r0() const final {
      return r0_.Get();
    }

   private:
    const S<Data_CountedId> data_;
    // TODO: We really shouldn't be storing a raw type here.
    CopiedVariable<int> r0_;
  };

  const S<Data_CountedId> data_ = S_get(new Data_CountedId);
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
  const auto counted = Concrete_CountedId<int>::create();

  const auto function =
    ConvertTo<S<Interface_Function<S<U<long,int>>,std::string>>>::From(counted);

  std::cerr << "Count: " << counted->Call_CountedId_count() << std::endl;
  std::cerr << "Call: " << counted->Call_CountedId_call(3) << std::endl;
  std::cerr << "Call: " << counted->Call_CountedId_call(0) << std::endl;
  std::cerr << "Call: "
            << function->Call_Function_call(ConvertTo<S<U<long,int>>>::From(4L)).append("!")
            << std::endl;
  std::cerr << "Count: " << counted->Call_CountedId_count() << std::endl;
}
