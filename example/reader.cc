#include "reader.h"

#include "base/dispatch.h"
#include "base/optional.h"

namespace {

class Constructor_Reader;
class Instance_Reader;
class Value_Reader;


class Constructor_Reader : public ParamInstance<1>::Type {
 public:
  Constructor_Reader();

  TypeInstance& Build(TypeInstance& arg_x) final;
  const std::string& CategoryName() const final { return name_; }
  Instance_Reader& BuildInternal(TypeInstance& arg_x);

  const FunctionDispatcher<Value_Reader,MemberScope::VALUE>& value_functions;

 private:
  const std::string name_{"Reader"};
  FunctionDispatcher<Value_Reader,MemberScope::VALUE> value_functions_;
  InstanceCache<Instance_Reader> instance_cache_;
};

Constructor_Reader& Internal_Reader() {
  static Constructor_Reader*const constructor = new Constructor_Reader;
  return *constructor;
}


class Instance_Reader : public TypeInstance {
 public:
  Instance_Reader(TypeInstance& arg_x)
      : param_x(arg_x),
        name_(ConstructInstanceName(Category_Reader(),arg_x)) {
    parents_.AddParent(Category_Reader(),param_x);
  }

  const std::string& InstanceName() const final { return name_; }
  const TypeCategory& CategoryType() const final { return Category_Reader(); }
  bool IsParentCategory(const TypeCategory&) const final;
  const TypeArgs& TypeArgsForCategory(const TypeCategory& category) const final;

  TypeInstance& param_x;

  TypeInstance& Type_read_r0() const {
    return Category_Optional().Build(param_x);
  }

 private:
  bool CheckConversionFrom(const TypeInstance& type) const final;
  MergeType InstanceMergeType() const final { return MergeType::SINGLE; }
  const TypeArgs& MergedInstanceTypes() const final { return types_; }

  const std::string name_;
  const TypeArgs types_{this};
  ParentTypes parents_;
};


class Value_Reader : public TypeValue {
 public:
  Value_Reader(Instance_Reader& parent,
               const S<Interface_Reader>& interface)
      : parent_(parent),
        interface_(interface) {}

  FunctionReturns CallValueFunction(
      const FunctionId<MemberScope::VALUE>& id,
      const TypeArgs&,
      const FunctionArgs& args) final;

  ParamReturns<1>::Type Call_read(ParamTypes<0>::Type, ParamArgs<0>::Type) const;

 private:
  const TypeInstance& InstanceType() const final { return parent_; }
  S<TypeValue> ConvertTo(TypeInstance&) final;

  Instance_Reader& parent_;
  const S<Interface_Reader> interface_;
};


Constructor_Reader::Constructor_Reader()
    : value_functions(value_functions_),
      value_functions_("Reader") {
  value_functions_
      .AddFunction(Function_Reader_read,&Value_Reader::Call_read);
}

TypeInstance& Constructor_Reader::Build(TypeInstance& arg_x) {
  return BuildInternal(arg_x);
}

Instance_Reader& Constructor_Reader::BuildInternal(TypeInstance& arg_x) {
  R<Instance_Reader>& instance = instance_cache_.Create(arg_x);
  if (!instance) {
    instance = R_get(new Instance_Reader(arg_x));
  }
  return *instance;
}

bool Instance_Reader::IsParentCategory(const TypeCategory& category) const {
  if (parents_.HasParent(category)) {
    return true;
  }
  return TypeInstance::IsParentCategory(category);
}

const TypeArgs& Instance_Reader::TypeArgsForCategory(const TypeCategory& category) const {
  if (parents_.HasParent(category)) {
    return parents_.GetParent(category);
  }
  return TypeInstance::TypeArgsForCategory(category);
}

bool Instance_Reader::CheckConversionFrom(const TypeInstance& instance) const {
  if (!CategoryIsParentOf(instance)) {
    return false;
  }
  const TypeArgs& args = CategoryTypeArgsFrom(instance);
  FAIL_IF(args.size() != 1) << "Wrong number of type args";
  return CheckConversionBetween(*SafeGet<0>(args),param_x);  // covariant
}


FunctionReturns Value_Reader::CallValueFunction(
    const FunctionId<MemberScope::VALUE>& id,
    const TypeArgs& types,
    const FunctionArgs& args) {
  return Internal_Reader().value_functions.Call(id,this,types,args);
}

ParamReturns<1>::Type Value_Reader::Call_read(
    ParamTypes<0>::Type types, ParamArgs<0>::Type args) const {
  const T<S<TypeValue>> results = interface_->Call_Reader_read();
  return T_get(
    TypeValue::ConvertTo(std::get<0>(results),parent_.Type_read_r0()));
}

S<TypeValue> Value_Reader::ConvertTo(TypeInstance& instance) {
  // TODO: Generalize this better.
  if (&instance.CategoryType() == &Category_Reader()) {
    const TypeArgs& args = InstanceType().CategoryTypeArgsFrom(instance);
    FAIL_IF(args.size() != 1) << "Wrong number of type args";
    return As_Reader(interface_,*SafeGet<0>(args));
  }
  return TypeValue::ConvertTo(instance);
}

}  // namespace


ParamInstance<1>::Type& Category_Reader() {
  return Internal_Reader();
}

const FunctionId<MemberScope::VALUE>& Function_Reader_read =
    *new FunctionId<MemberScope::VALUE>("Reader.read");

S<TypeValue> As_Reader(const S<Interface_Reader>& value, TypeInstance& instance) {
  return S_get(new Value_Reader(Internal_Reader().BuildInternal(instance),value));
}
