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
  const ConvertToDispatcher<Value_Queue>& convert_functions;

 private:
  ~Constructor_Queue() = default;

  const std::string name_{"Queue"};
  FunctionDispatcher<Instance_Queue,MemberScope::INSTANCE> instance_functions_;
  FunctionDispatcher<Value_Queue,MemberScope::VALUE> value_functions_;
  ConvertToDispatcher<Value_Queue> convert_functions_;
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

  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const TypeArgs&,
      const FunctionArgs& args) final;

  ParamReturns<1>::Type Call_read(ParamTypes<0>::Type, ParamArgs<0>::Type) const;
  ParamReturns<0>::Type Call_write(ParamTypes<0>::Type, ParamArgs<1>::Type) const;

  S<TypeValue> Convert_Reader() const;
  S<TypeValue> Convert_Writer() const;

 private:
  const TypeInstance& InstanceType() const final { return parent_; }
  S<TypeValue> ConvertTo(TypeInstance&) final;

  Instance_Queue& parent_;
  const S<Concrete_Queue> interface_;
};


struct Concrete_Queue
    : virtual public Interface_Reader,
      virtual public Interface_Writer {
 public:
  Concrete_Queue(Instance_Queue& parent) : parent_(parent) {}
  ParamReturns<1>::Type Call_Reader_read(ParamTypes<0>::Type, ParamArgs<0>::Type) final;
  ParamReturns<0>::Type Call_Writer_write(ParamTypes<0>::Type, ParamArgs<1>::Type) final;

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
      convert_functions(convert_functions_) {
  instance_functions_
      .AddFunction(Function_Queue_create,&Instance_Queue::Call_create);
  value_functions_
      .AddFunction(Function_Reader_read,&Value_Queue::Call_read)
      .AddFunction(Function_Writer_write,&Value_Queue::Call_write);
  convert_functions_
      .AddCategory(Category_Reader(),&Value_Queue::Convert_Reader)
      .AddCategory(Category_Writer(),&Value_Queue::Convert_Writer);
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
  return Internal_Queue().instance_functions.Call(InstanceName(),id,this,types,args);
}

ParamReturns<1>::Type Instance_Queue::Call_create(
    ParamTypes<0>::Type types, ParamArgs<0>::Type) {
  ParamReturns<1>::Type returned;
  std::get<0>(returned) =
    TypeValue::ConvertTo(S_get(new Value_Queue(*this,S_get(new Concrete_Queue(*this)))),
                         Type_create_r0());
  return returned;
}

bool Instance_Queue::CheckConversionFrom(const TypeInstance& instance) const {
  if (!CategoryIsParentOf(instance)) {
    return false;
  }
  const TypeArgs& args = CategoryTypeArgsFrom(instance);
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return SafeGet<0>(args) == &param_x;  // invariant
}


FunctionReturns Value_Queue::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return Internal_Queue().value_functions.Call(InstanceName(),id,this,types,args);
}

ParamReturns<1>::Type Value_Queue::Call_read(
    ParamTypes<0>::Type types, ParamArgs<0>::Type args) const {
  return interface_->Call_Reader_read(types,args);
}

ParamReturns<0>::Type Value_Queue::Call_write(
    ParamTypes<0>::Type types, ParamArgs<1>::Type args) const {
  return interface_->Call_Writer_write(types,args);
}

S<TypeValue> Value_Queue::Convert_Reader() const {
  return As_Reader(interface_,parent_.param_x);
}

S<TypeValue> Value_Queue::Convert_Writer() const {
  return As_Writer(interface_,parent_.param_x);
}

S<TypeValue> Value_Queue::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Queue()) {
    const TypeArgs& args = InstanceType().CategoryTypeArgsFrom(instance);
    FAIL_IF(args.size() != 1) << "Wrong number of type args";
    return As_Queue(interface_,*SafeGet<0>(args));
  }
  return TypeValue::ConvertTo(
      Internal_Queue().convert_functions.ConvertTo(InstanceName(),instance.CategoryType(),this),
      instance);
}


/*

00: read () (value) {
01:   value = skip;
02:   if (!queue.empty()) {
03:     value = queue.pop();
04:   }
05: }
06:
07: write (value) {
08:   queue.push(value);
09: }

*/

ParamReturns<1>::Type Concrete_Queue::Call_Reader_read(
    ParamTypes<0>::Type types, ParamArgs<0>::Type args) {
  ParamReturns<1>::Type returned;
  TRACE_FUNCTION("Queue.read")
  SET_CONTEXT_POINT("queue:0")
  ProxyVariable value(parent_.Type_read_r0(),std::get<0>(returned));
  SET_CONTEXT_POINT("queue:1")
  value.SetValue(Optional_Skip());
  SET_CONTEXT_POINT("queue:2")
  if (!queue_.empty()) {
    SET_CONTEXT_POINT("queue:3")
    value.SetValue(TypeValue::ConvertTo(queue_.front(),parent_.Type_read_r0()));
    queue_.pop();
  }
  return returned;
}

ParamReturns<0>::Type Concrete_Queue::Call_Writer_write(
    ParamTypes<0>::Type types, ParamArgs<1>::Type args) {
  ParamReturns<0>::Type returned;
  TRACE_FUNCTION("Queue.write")
  SET_CONTEXT_POINT("queue:7")
  ValueVariable value(parent_.Type_write_a0(),std::get<0>(args));
  SET_CONTEXT_POINT("queue:8")
  queue_.push(value.GetValue());
  return returned;
}

}  // namespace


ParamInstance<1>::Type& Category_Queue() {
  return Internal_Queue();
}

const FunctionId<MemberScope::INSTANCE>& Function_Queue_create =
    *new FunctionId<MemberScope::INSTANCE>("Queue.create");
