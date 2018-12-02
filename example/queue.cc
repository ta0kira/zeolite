#include "queue.h"

#include <queue>

#include "base/dispatch.h"
#include "base/optional.h"

namespace {

class Constructor_Queue;
class Instance_Queue;
class Value_Queue;
class Concrete_Queue;


class Constructor_Queue : public ParamInstance<1>::Type {
 public:
  Constructor_Queue();

  TypeInstance& Build(TypeInstance& arg_x) final;
  const std::string& CategoryName() const final { return name_; }
  Instance_Queue& BuildInternal(TypeInstance& arg_x);

  const FunctionDispatcher<Instance_Queue,MemberScope::INSTANCE>& instance_functions;
  const FunctionDispatcher<Value_Queue,MemberScope::VALUE>& value_functions;

 private:
  const std::string name_{"Queue"};
  FunctionDispatcher<Instance_Queue,MemberScope::INSTANCE> instance_functions_;
  FunctionDispatcher<Value_Queue,MemberScope::VALUE> value_functions_;
  InstanceCache<Instance_Queue> instance_cache_;
};

Constructor_Queue& Internal_Queue() {
  static Constructor_Queue*const constructor = new Constructor_Queue;
  return *constructor;
}


class Instance_Queue : public TypeInstance {
 public:
  Instance_Queue(TypeInstance& arg_x)
      : param_x(arg_x),
        Type_create_r0(*this),
        Type_read_r0(Category_Optional().Build(arg_x)),
        Type_write_a0(arg_x),
        name_(ConstructInstanceName(Category_Queue(),arg_x)) {}

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Queue(); }
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;
  FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>&, const FunctionArgs&) final;

  T<S<TypeValue>> Call_create(const T<>&);

  TypeInstance& param_x;
  TypeInstance& Type_create_r0;
  TypeInstance& Type_read_r0;
  TypeInstance& Type_write_a0;

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_{this};
  const TypeArgs args_self_{&param_x};
  const TypeArgs args_reader_{&param_x};
  const TypeArgs args_writer_{&param_x};
};


class Value_Queue : public TypeValue {
 public:
  Value_Queue(Instance_Queue& parent,
              const S<Concrete_Queue>& interface)
      : parent_(parent),
        interface_(interface) {}

  const TypeInstance& InstanceType() const final { return parent_; }
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) final;

  T<S<TypeValue>> Call_read(const T<>&) const;
  T<> Call_write(const T<S<TypeValue>>&) const;

 private:
  S<TypeValue> ConvertTo(TypeInstance&) final;

  Instance_Queue& parent_;
  const S<Concrete_Queue> interface_;
};


struct Concrete_Queue
    : virtual public Interface_Reader,
      virtual public Interface_Writer {
 public:
  Concrete_Queue(Instance_Queue& parent) : parent_(parent) {}
  T<S<TypeValue>> Call_Reader_read() final;
  T<> Call_Writer_write(const S<TypeValue>&) final;

 private:
  Instance_Queue& parent_;
  std::queue<S<TypeValue>> queue_;
};

S<TypeValue> As_Queue(const S<Concrete_Queue>& value, TypeInstance& instance) {
  return S_get(new Value_Queue(Internal_Queue().BuildInternal(instance),value));
}


Constructor_Queue::Constructor_Queue()
    : instance_functions(instance_functions_),
      value_functions(value_functions_),
      instance_functions_("Queue"),
      value_functions_("Queue") {
  instance_functions_
      .AddFunction(Function_Queue_create,&Instance_Queue::Call_create);
  value_functions_
      .AddFunction(Function_Reader_read,&Value_Queue::Call_read)
      .AddFunction(Function_Writer_write,&Value_Queue::Call_write);
}

TypeInstance& Constructor_Queue::Build(TypeInstance& arg_x) {
  return BuildInternal(arg_x);
}

Instance_Queue& Constructor_Queue::BuildInternal(TypeInstance& arg_x) {
  R<Instance_Queue>& instance = instance_cache_.Create(arg_x);
  if (!instance) {
    instance = R_get(new Instance_Queue(arg_x));
  }
  return *instance;
}

const TypeArgs& Instance_Queue::TypeArgsForCategory(const TypeCategory& category) const {
  // TODO: Generalize this better.
  if (&category == &Category_Queue()) {
    return args_self_;
  }
  if (&category == &Category_Reader()) {
    return args_reader_;
  }
  if (&category == &Category_Writer()) {
    return args_writer_;
  }
  return TypeInstance::TypeArgsForCategory(category);
}

FunctionReturns Instance_Queue::CallInstanceFunction(
    const FunctionId<MemberScope::INSTANCE>& id, const FunctionArgs& args) {
  return Internal_Queue().instance_functions.Call(id,this,args);
}

T<S<TypeValue>> Instance_Queue::Call_create(const T<>&) {
  return T_get(TypeValue::ConvertTo(S_get(new Value_Queue(*this,S_get(new Concrete_Queue(*this)))),
               Type_create_r0));
}

bool Instance_Queue::CheckConversionFrom(const TypeInstance& instance) const {
  const TypeArgs& args = instance.TypeArgsForCategory(Category_Queue());
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return SafeGet<0>(args) == &param_x;  // invariant
}


FunctionReturns Value_Queue::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) {
  return Internal_Queue().value_functions.Call(id,this,args);
}

T<S<TypeValue>> Value_Queue::Call_read(const T<>& args) const {
  const T<S<TypeValue>> results = interface_->Call_Reader_read();
  return T_get(
    TypeValue::ConvertTo(std::get<0>(results),parent_.Type_read_r0));
}

T<> Value_Queue::Call_write(const T<S<TypeValue>>& args) const {
  const T<> results = interface_->Call_Writer_write(
    TypeValue::ConvertTo(std::get<0>(args),parent_.Type_write_a0));
  return T_get();
}

S<TypeValue> Value_Queue::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Queue()) {
    const TypeArgs& args = instance.TypeArgsForCategory(Category_Queue());
    FAIL_IF(args.size() != 1) << "Wrong number of type args";
    return As_Queue(interface_,*SafeGet<0>(args));
  }
  if (&instance.CategoryType() == &Category_Reader()) {
    return As_Reader(interface_,parent_.param_x);
  }
  if (&instance.CategoryType() == &Category_Writer()) {
    return As_Writer(interface_,parent_.param_x);
  }
  return TypeValue::ConvertTo(instance);
}


T<S<TypeValue>> Concrete_Queue::Call_Reader_read() {
  if (queue_.empty()) {
    return T_get(Skip_Optional(parent_.param_x));
  } else {
    const S<TypeValue> result = As_Optional(queue_.front(),parent_.param_x);
    queue_.pop();
    return T_get(result);
  }
}

T<> Concrete_Queue::Call_Writer_write(const S<TypeValue>& value) {
  queue_.push(value);
  return T_get();
}

}  // namespace


ParamInstance<1>::Type& Category_Queue() {
  return Internal_Queue();
}

const FunctionId<MemberScope::INSTANCE>& Function_Queue_create =
    *new FunctionId<MemberScope::INSTANCE>("Queue.create");
