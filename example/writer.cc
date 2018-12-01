#include "writer.h"

#include "base/dispatch.h"

namespace {

class Constructor_Writer;
class Instance_Writer;
class Value_Writer;


class Constructor_Writer : public ParamInstance<1>::Type {
 public:
  Constructor_Writer();

  TypeInstance& Build(TypeInstance& arg_x) final;
  const std::string& CategoryName() const final { return name_; }
  Instance_Writer& BuildInternal(TypeInstance& arg_x);

  const FunctionDispatcher<Value_Writer,MemberScope::VALUE>& value_functions;

 private:
  const std::string name_{"Writer"};
  FunctionDispatcher<Value_Writer,MemberScope::VALUE> value_functions_;
  InstanceCache<Instance_Writer> instance_cache_;
};

Constructor_Writer& Internal_Writer = *new Constructor_Writer;


class Instance_Writer : public TypeInstance {
 public:
  Instance_Writer(TypeInstance& arg_x)
      : param_x(arg_x),
        name_(ConstructInstanceName(Category_Writer,arg_x)) {}

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Writer; }
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;

  TypeInstance& param_x;

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_{this};
  const TypeArgs args_{&param_x};
};


class Value_Writer : public TypeValue {
 public:
  Value_Writer(Instance_Writer& parent,
               const S<Interface_Writer>& interface)
      : parent_(parent),
        interface_(interface) {}

  const TypeInstance& InstanceType() const final { return parent_; }
  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) final;

  T<> Call_write(const T<S<TypeValue>>&) const;

 private:
  S<TypeValue> ConvertTo(TypeInstance&) final;

  Instance_Writer& parent_;
  const S<Interface_Writer> interface_;
};

}  // namespace


ParamInstance<1>::Type& Category_Writer = Internal_Writer;

const FunctionId<MemberScope::VALUE>& Function_Writer_write =
    *new FunctionId<MemberScope::VALUE>("Writer.write");

S<TypeValue> AsWriter(const S<Interface_Writer>& value, TypeInstance& instance) {
  return S_get(new Value_Writer(Internal_Writer.BuildInternal(instance),value));
}


namespace {

Constructor_Writer::Constructor_Writer()
    : value_functions(value_functions_),
      value_functions_("Writer") {
  value_functions_
      .AddFunction(Function_Writer_write,&Value_Writer::Call_write);
}

TypeInstance& Constructor_Writer::Build(TypeInstance& arg_x) {
  return BuildInternal(arg_x);
}

Instance_Writer& Constructor_Writer::BuildInternal(TypeInstance& arg_x) {
  R<Instance_Writer>& instance = instance_cache_.Create(arg_x);
  if (!instance) {
    instance = R_get(new Instance_Writer(arg_x));
  }
  return *instance;
}

const TypeArgs& Instance_Writer::TypeArgsForCategory(const TypeCategory& category) const {
  // TODO: Generalize this better.
  if (&category == &Category_Writer) {
    return args_;
  }
  return TypeInstance::TypeArgsForCategory(category);
}

bool Instance_Writer::CheckConversionFrom(const TypeInstance& instance) const {
  const TypeArgs& args = instance.TypeArgsForCategory(Category_Writer);
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return CheckConversionBetween(param_x,*SafeGet<0>(args));  // contravariant
}


FunctionReturns Value_Writer::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id, const FunctionArgs& args) {
  return Internal_Writer.value_functions.Call(id,this,args);
}

T<> Value_Writer::Call_write(const T<S<TypeValue>>& args) const {
  const T<> results = interface_->Call_Writer_write(
    TypeValue::ConvertTo(std::get<0>(args),parent_.param_x));
  return T_get();
}

S<TypeValue> Value_Writer::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Writer) {
    const TypeArgs& args = instance.TypeArgsForCategory(Category_Writer);
    FAIL_IF(args.size() != 1) << "Wrong number of type args";
    return AsWriter(interface_,*SafeGet<0>(args));
  }
  return TypeValue::ConvertTo(instance);
}

}  // namespace
