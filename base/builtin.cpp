#include "builtin.hpp"

#include <map>
#include <sstream>

#include "category-source.hpp"


namespace {

struct OptionalEmpty : public TypeValue {
  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const DFunction<SymbolScope::VALUE>& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    FAIL() << "Function called on empty value";
    return ReturnTuple(label.ReturnCount());
  }

  std::string CategoryName() const final { return "empty"; }

  bool Present() const final { return false; }
};

struct Type_Intersect : public TypeInstance {
  Type_Intersect(L<TypeInstance*> params) : params_(params.begin(), params.end()) {}

  std::string CategoryName() const { return "(intersection)"; }

  MergeType InstanceMergeType() const final
  { return MergeType::INTERSECT; }

  std::vector<const TypeInstance*> MergedTypes() const final
  { return params_; }

  const L<const TypeInstance*> params_;
};

struct Type_Union : public TypeInstance {
  Type_Union(L<TypeInstance*> params) : params_(params.begin(), params.end()) {}

  std::string CategoryName() const { return "(union)"; }

  MergeType InstanceMergeType() const final
  { return MergeType::UNION; }

  std::vector<const TypeInstance*> MergedTypes() const final
  { return params_; }

  const L<const TypeInstance*> params_;
};

struct Category_Bool : public TypeCategory {
  std::string CategoryName() const final { return "Bool"; }
};

struct Type_Bool : public TypeInstance {
  std::string CategoryName() const final { return "Bool"; }

  bool TypeArgsForParent(
    const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_Bool()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_Formatted()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    return false;
  }

  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(GetCategory_Bool(), args)) return false;
    FAIL_IF(args.size() != 0) << "Wrong number of args (" << args.size() << ") for " << CategoryName();
    return true;
  }
};

class Value_Bool : public TypeValue {
 public:
  Value_Bool(bool value) : value_(value) {}

  std::string CategoryName() const final { return "Bool"; }

  bool AsBool() const final { return value_; }

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const DFunction<SymbolScope::VALUE>& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    FAIL_IF(args.Size() != label.ArgCount());
    FAIL_IF(params.Size() != label.ParamCount());
    if (&label == &Function_Formatted_formatted) {
      return ReturnTuple(Box_String(self->AsBool()? "true" : "false"));
    }
    return TypeValue::Dispatch(self, label, params, args);
  }

 private:
  const bool value_;
};

struct Category_String : public TypeCategory {
  std::string CategoryName() const final { return "String"; }
};

struct Type_String : public TypeInstance {
  std::string CategoryName() const final { return "String"; }

  ReturnTuple Dispatch(const DFunction<SymbolScope::TYPE>& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    FAIL_IF(args.Size() != label.ArgCount());
    FAIL_IF(params.Size() != label.ParamCount());
    if (&label == &Function_LessThan_lessThan) {
      return ReturnTuple(Box_Bool(args.At(0)->AsString()<args.At(1)->AsString()));
    }
    if (&label == &Function_Equals_equals) {
      return ReturnTuple(Box_Bool(args.At(0)->AsString()==args.At(1)->AsString()));
    }
    return TypeInstance::Dispatch(label, params, args);
  }

  bool TypeArgsForParent(
    const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_String()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_Formatted()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    return false;
  }

  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(GetCategory_String(), args)) return false;
    FAIL_IF(args.size() != 0) << "Wrong number of args (" << args.size() << ") for " << CategoryName();
    return true;
  }
};

class Value_String : public TypeValue {
 public:
  Value_String(std::string value) : value_(value) {}

  std::string CategoryName() const final { return "String"; }

  std::string AsString() const final { return value_; }

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const DFunction<SymbolScope::VALUE>& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    FAIL_IF(args.Size() != label.ArgCount());
    FAIL_IF(params.Size() != label.ParamCount());
    if (&label == &Function_Formatted_formatted) {
      return ReturnTuple(Box_String(self->AsString()));
    }
    return TypeValue::Dispatch(self, label, params, args);
  }

 private:
  const std::string value_;
};

struct Category_Int : public TypeCategory {
  std::string CategoryName() const final { return "Int"; }
};

struct Type_Int : public TypeInstance {
  std::string CategoryName() const final { return "Int"; }

  ReturnTuple Dispatch(const DFunction<SymbolScope::TYPE>& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    FAIL_IF(args.Size() != label.ArgCount());
    FAIL_IF(params.Size() != label.ParamCount());
    if (&label == &Function_LessThan_lessThan) {
      return ReturnTuple(Box_Bool(args.At(0)->AsInt()<args.At(1)->AsInt()));
    }
    if (&label == &Function_Equals_equals) {
      return ReturnTuple(Box_Bool(args.At(0)->AsInt()==args.At(1)->AsInt()));
    }
    return TypeInstance::Dispatch(label, params, args);
  }

  bool TypeArgsForParent(
    const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_Int()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_Formatted()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    return false;
  }

  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(GetCategory_Int(), args)) return false;
    FAIL_IF(args.size() != 0) << "Wrong number of args (" << args.size() << ") for " << CategoryName();
    return true;
  }
};

class Value_Int : public TypeValue {
 public:
  Value_Int(int value) : value_(value) {}

  std::string CategoryName() const final { return "Int"; }

  int AsInt() const final { return value_; }

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const DFunction<SymbolScope::VALUE>& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    FAIL_IF(args.Size() != label.ArgCount());
    FAIL_IF(params.Size() != label.ParamCount());
    if (&label == &Function_Formatted_formatted) {
      std::ostringstream output;
      output << self->AsInt();
      return ReturnTuple(Box_String(output.str()));
    }
    return TypeValue::Dispatch(self, label, params, args);
  }

 private:
  const int value_;
};

struct Category_Float : public TypeCategory {
  std::string CategoryName() const final { return "Float"; }
};

struct Type_Float : public TypeInstance {
  std::string CategoryName() const final { return "Float"; }

  ReturnTuple Dispatch(const DFunction<SymbolScope::TYPE>& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    FAIL_IF(args.Size() != label.ArgCount());
    FAIL_IF(params.Size() != label.ParamCount());
    if (&label == &Function_LessThan_lessThan) {
      return ReturnTuple(Box_Bool(args.At(0)->AsFloat()<args.At(1)->AsFloat()));
    }
    if (&label == &Function_Equals_equals) {
      return ReturnTuple(Box_Bool(args.At(0)->AsFloat()==args.At(1)->AsFloat()));
    }
    return TypeInstance::Dispatch(label, params, args);
  }

  bool TypeArgsForParent(
    const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_Float()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_Formatted()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    return false;
  }

  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(GetCategory_Float(), args)) return false;
    FAIL_IF(args.size() != 0) << "Wrong number of args (" << args.size() << ") for " << CategoryName();
    return true;
  }
};

class Value_Float : public TypeValue {
 public:
  Value_Float(double value) : value_(value) {}

  std::string CategoryName() const final { return "Float"; }

  double AsFloat() const final { return value_; }

  ReturnTuple Dispatch(const S<TypeValue>& self,
                       const DFunction<SymbolScope::VALUE>& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    FAIL_IF(args.Size() != label.ArgCount());
    FAIL_IF(params.Size() != label.ParamCount());
    if (&label == &Function_Formatted_formatted) {
      std::ostringstream output;
      output << self->AsFloat();
      return ReturnTuple(Box_String(output.str()));
    }
    return TypeValue::Dispatch(self, label, params, args);
  }

 private:
  const double value_;
};

struct Category_Formatted : public TypeCategory {
  std::string CategoryName() const final { return "Formatted"; }
};

struct Type_Formatted : public TypeInstance {
  std::string CategoryName() const final { return "Formatted"; }

  bool TypeArgsForParent(
    const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_Formatted()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    return false;
  }

  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(GetCategory_Formatted(), args)) return false;
    FAIL_IF(args.size() != 0) << "Wrong number of args (" << args.size() << ") for " << CategoryName();
    return true;
  }
};

}  // namespace

const int Collection_LessThan = 0;
const void* const Functions_LessThan = &Collection_LessThan;
const Function<SymbolScope::TYPE,0,2,1>& Function_LessThan_lessThan =
  *new Function<SymbolScope::TYPE,0,2,1>("LessThan", "lessThan", Functions_LessThan, 0);

const int Collection_Equals = 0;
const void* const Functions_Equals = &Collection_Equals;
const Function<SymbolScope::TYPE,0,2,1>& Function_Equals_equals =
   *new Function<SymbolScope::TYPE,0,2,1>("Equals", "equals", Functions_Equals, 0);

const int Collection_Formatted = 0;
const void* const Functions_Formatted = &Collection_Formatted;
const Function<SymbolScope::VALUE,0,0,1>& Function_Formatted_formatted =
   *new Function<SymbolScope::VALUE,0,0,1>("Formatted", "formatted", Functions_Formatted, 0);

TypeInstance& Merge_Intersect(L<TypeInstance*> params) {
  static auto& cache = *new std::map<L<TypeInstance*>,R<Type_Intersect>>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_Intersect(params)); }
  return *cached;
}

TypeInstance& Merge_Union(L<TypeInstance*> params) {
  static auto& cache = *new std::map<L<TypeInstance*>,R<Type_Union>>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_Union(params)); }
  return *cached;
}

TypeCategory& GetCategory_Bool() {
  static auto& category = *new Category_Bool();
  return category;
}

TypeCategory& GetCategory_String() {
  static auto& category = *new Category_String();
  return category;
}

TypeCategory& GetCategory_Int() {
  static auto& category = *new Category_Int();
  return category;
}

TypeCategory& GetCategory_Float() {
  static auto& category = *new Category_Float();
  return category;
}

TypeCategory& GetCategory_Formatted() {
  static auto& category = *new Category_Formatted();
  return category;
}


TypeInstance& GetType_Bool(Params<0>::Type) {
  static auto& instance = *new Type_Bool();
  return instance;
}

TypeInstance& GetType_String(Params<0>::Type) {
  static auto& instance = *new Type_String();
  return instance;
}

TypeInstance& GetType_Int(Params<0>::Type) {
  static auto& instance = *new Type_Int();
  return instance;
}

TypeInstance& GetType_Float(Params<0>::Type) {
  static auto& instance = *new Type_Float();
  return instance;
}

TypeInstance& GetType_Formatted(Params<0>::Type) {
  static auto& instance = *new Type_Formatted();
  return instance;
}

S<TypeValue> Box_Bool(bool value) {
  return value? Var_true : Var_false;
}

S<TypeValue> Box_String(std::string value) {
  return S_get(new Value_String(value));
}

S<TypeValue> Box_Int(int value) {
  return S_get(new Value_Int(value));
}

S<TypeValue> Box_Float(double value) {
  return S_get(new Value_Float(value));
}


const S<TypeValue>& Var_empty = *new S<TypeValue>(new OptionalEmpty());
const S<TypeValue>& Var_true = *new S<TypeValue>(new Value_Bool(true));
const S<TypeValue>& Var_false = *new S<TypeValue>(new Value_Bool(false));
