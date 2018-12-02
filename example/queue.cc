#include "queue.h"

#include <queue>

#include "base/dispatch.h"
#include "base/optional.h"
#include "base/trace.h"

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
        name_(ConstructInstanceName(Category_Queue(),arg_x)) {
    parents_
        .AddParent(Category_Queue(),param_x)
        .AddParent(Category_Writer(),param_x)
        .AddParent(Category_Reader(),param_x);
  }

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Queue(); }
  bool IsParentCategory(const TypeCategory&) const final;
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;
  FunctionReturns CallInstanceFunction(
      const FunctionId<MemberScope::INSTANCE>&,
      const TypeArgs&,
      const FunctionArgs&) final;

  ParamReturns<1>::Type Call_create(ParamTypes<0>::Type, ParamArgs<0>::Type);

  TypeInstance& param_x;

  TypeInstance& Type_create_r0() const {
    return Category_Queue().Build(param_x);
  }

  TypeInstance& Type_read_r0() const {
    return Category_Optional().Build(param_x);
  }

  TypeInstance& Type_write_a0() const {
    return param_x;
  }

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_{this};
  ParentTypes parents_;
};


class Value_Queue : public TypeValue {
 public:
  Value_Queue(Instance_Queue& parent,
              const S<Concrete_Queue>& interface)
      : parent_(parent),
        interface_(interface) {}

  const TypeInstance& InstanceType() const final { return parent_; }
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const TypeArgs&,
      const FunctionArgs& args) final;

  ParamReturns<1>::Type Call_read(ParamTypes<0>::Type, ParamArgs<0>::Type) const;
  ParamReturns<0>::Type Call_write(ParamTypes<0>::Type, ParamArgs<1>::Type) const;

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

bool Instance_Queue::IsParentCategory(const TypeCategory& category) const {
  if (parents_.HasParent(category)) {
    return true;
  }
  return TypeInstance::IsParentCategory(category);
}

const TypeArgs& Instance_Queue::TypeArgsForCategory(const TypeCategory& category) const {
  if (parents_.HasParent(category)) {
    return parents_.GetParent(category);
  }
  return TypeInstance::TypeArgsForCategory(category);
}

FunctionReturns Instance_Queue::CallInstanceFunction(
    const FunctionId<MemberScope::INSTANCE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return Internal_Queue().instance_functions.Call(id,this,types,args);
}

ParamReturns<1>::Type Instance_Queue::Call_create(
    ParamTypes<0>::Type types, ParamArgs<0>::Type) {
  return T_get(TypeValue::ConvertTo(S_get(new Value_Queue(*this,S_get(new Concrete_Queue(*this)))),
               Type_create_r0()));
}

bool Instance_Queue::CheckConversionFrom(const TypeInstance& instance) const {
  if (!instance.IsParentCategory(Category_Queue())) {
    return false;
  }
  const TypeArgs& args = instance.TypeArgsForCategory(Category_Queue());
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return SafeGet<0>(args) == &param_x;  // invariant
}


FunctionReturns Value_Queue::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return Internal_Queue().value_functions.Call(id,this,types,args);
}

ParamReturns<1>::Type Value_Queue::Call_read(
    ParamTypes<0>::Type types, ParamArgs<0>::Type) const {
  const T<S<TypeValue>> results = interface_->Call_Reader_read();
  return T_get(
    TypeValue::ConvertTo(std::get<0>(results),parent_.Type_read_r0()));
}

ParamReturns<0>::Type Value_Queue::Call_write(
    ParamTypes<0>::Type types, ParamArgs<1>::Type args) const {
  const T<> results = interface_->Call_Writer_write(
    TypeValue::ConvertTo(std::get<0>(args),parent_.Type_write_a0()));
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
    return TypeValue::ConvertTo(As_Reader(interface_,parent_.param_x),instance);
  }
  if (&instance.CategoryType() == &Category_Writer()) {
    return TypeValue::ConvertTo(As_Writer(interface_,parent_.param_x),instance);
  }
  return TypeValue::ConvertTo(instance);
}


/*

00: read () {
01:   if (queue.empty()) {
02:     return skip;
03:   } else {
04:     return queue.pop();
05:   }
06: }
07:
08: write (value) {
09:   queue.push(value);
10: }

*/

T<S<TypeValue>> Concrete_Queue::Call_Reader_read() {
  SourceContext trace("Queue.read");
  trace.SetLocal("queue:1");
  if (queue_.empty()) {
    trace.SetLocal("queue:2");
    return T_get(TypeValue::ConvertTo(Optional_Skip(),parent_.Type_read_r0()));
  } else {
    trace.SetLocal("queue:4");
    const S<TypeValue> result = As_Optional(queue_.front(),parent_.param_x);
    queue_.pop();
    return T_get(result);
  }
}

T<> Concrete_Queue::Call_Writer_write(const S<TypeValue>& value) {
  SourceContext trace("Queue.write");
  trace.SetLocal("queue:9");
  queue_.push(value);
  return T_get();
}

}  // namespace


ParamInstance<1>::Type& Category_Queue() {
  return Internal_Queue();
}

const FunctionId<MemberScope::INSTANCE>& Function_Queue_create =
    *new FunctionId<MemberScope::INSTANCE>("Queue.create");
