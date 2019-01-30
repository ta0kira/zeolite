#include "builtin.hpp"

#include <map>

#include "category-source.hpp"


namespace {

struct OptionalEmpty : public TypeValue {
  DReturns Dispatch(const S<TypeValue>& self,
                    const DFunction<SymbolScope::VALUE>& label,
                    DParams params, DArgs args) final {
    FAIL() << "Function called on empty value";
    return DReturns();
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
};

class Value_Bool : public TypeValue {
 public:
  Value_Bool(bool value) : value_(value) {}

  std::string CategoryName() const final { return "Bool"; }

  virtual bool AsBool() const { return value_; }

 private:
  const bool value_;
};

struct Category_String : public TypeCategory {
  std::string CategoryName() const final { return "String"; }
};

struct Type_String : public TypeInstance {
  std::string CategoryName() const final { return "String"; }
};

class Value_String : public TypeValue {
 public:
  Value_String(std::string value) : value_(value) {}

  std::string CategoryName() const final { return "String"; }

  virtual std::string AsString() const { return value_; }

 private:
  const std::string value_;
};

struct Category_Int : public TypeCategory {
  std::string CategoryName() const final { return "Int"; }
};

struct Type_Int : public TypeInstance {
  std::string CategoryName() const final { return "Int"; }
};

class Value_Int : public TypeValue {
 public:
  Value_Int(int value) : value_(value) {}

  std::string CategoryName() const final { return "Int"; }

  virtual int AsInt() const { return value_; }

 private:
  const int value_;
};

struct Category_Float : public TypeCategory {
  std::string CategoryName() const final { return "Float"; }
};

struct Type_Float : public TypeInstance {
  std::string CategoryName() const final { return "Float"; }
};

class Value_Float : public TypeValue {
 public:
  Value_Float(double value) : value_(value) {}

  std::string CategoryName() const final { return "Float"; }

  virtual double AsFloat() const { return value_; }

 private:
  const double value_;
};

}  // namespace

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

const S<TypeValue>& Var_empty = *new S<TypeValue>(new OptionalEmpty());
const S<TypeValue>& Var_true = *new S<TypeValue>(new Value_Bool(true));
const S<TypeValue>& Var_false = *new S<TypeValue>(new Value_Bool(false));
