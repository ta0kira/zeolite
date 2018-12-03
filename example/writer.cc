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
  ~Constructor_Writer() = default;

  const std::string name_{"Writer"};
  FunctionDispatcher<Value_Writer,MemberScope::VALUE> value_functions_;
  InstanceCache<Instance_Writer> instance_cache_;
};

Constructor_Writer& Internal_Writer() {
  static Constructor_Writer*const constructor = new Constructor_Writer;
  return *constructor;
}


class Instance_Writer : public TypeInstance {
 public:
  Instance_Writer(TypeInstance& arg_x)
      : param_x(arg_x),
        name_(ConstructInstanceName(Category_Writer(),arg_x)) {
    parents_.AddParent(Category_Writer(),param_x);
  }

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Writer(); }
  bool IsParentCategory(const TypeCategory&) const final;
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;

  TypeInstance& param_x;

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


class Value_Writer : public TypeValue {
 public:
  Value_Writer(Instance_Writer& parent,
               const S<Interface_Writer>& interface)
      : parent_(parent),
        interface_(interface) {}

  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const TypeArgs&,
      const FunctionArgs& args) final;

  ParamReturns<0>::Type Call_write(ParamTypes<0>::Type, ParamArgs<1>::Type) const;

 private:
  const TypeInstance& InstanceType() const final { return parent_; }
  S<TypeValue> ConvertTo(TypeInstance&) final;

  Instance_Writer& parent_;
  const S<Interface_Writer> interface_;
};


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

bool Instance_Writer::IsParentCategory(const TypeCategory& category) const {
  if (parents_.HasParent(category)) {
    return true;
  }
  return TypeInstance::IsParentCategory(category);
}

const TypeArgs& Instance_Writer::TypeArgsForCategory(const TypeCategory& category) const {
  if (parents_.HasParent(category)) {
    return parents_.GetParent(category);
  }
  return TypeInstance::TypeArgsForCategory(category);
}

bool Instance_Writer::CheckConversionFrom(const TypeInstance& instance) const {
  if (!CategoryIsParentOf(instance)) {
    return false;
  }
  const TypeArgs& args = CategoryTypeArgsFrom(instance);
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return CheckConversionBetween(param_x,*SafeGet<0>(args));  // contravariant
}


FunctionReturns Value_Writer::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return Internal_Writer().value_functions.Call(id,this,types,args);
}

ParamReturns<0>::Type Value_Writer::Call_write(
    ParamTypes<0>::Type types, ParamArgs<1>::Type args) const {
  const T<> results = interface_->Call_Writer_write(
    TypeValue::ConvertTo(std::get<0>(args),parent_.Type_write_a0()));
  return T_get();
}

S<TypeValue> Value_Writer::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Writer()) {
    const TypeArgs& args = InstanceType().CategoryTypeArgsFrom(instance);
    FAIL_IF(args.size() != 1) << "Wrong number of type args";
    return As_Writer(interface_,*SafeGet<0>(args));
  }
  return TypeValue::ConvertTo(instance);
}

}  // namespace


ParamInstance<1>::Type& Category_Writer() {
  return Internal_Writer();
}

const FunctionId<MemberScope::VALUE>& Function_Writer_write =
    *new FunctionId<MemberScope::VALUE>("Writer.write");

S<TypeValue> As_Writer(const S<Interface_Writer>& value, TypeInstance& instance) {
  return S_get(new Value_Writer(Internal_Writer().BuildInternal(instance),value));
}
