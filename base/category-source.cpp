#include "category-source.hpp"

#include "logging.hpp"
#include "builtin.hpp"


DReturns TypeCategory::Dispatch(const DFunction<SymbolScope::CATEGORY>& label,
                                DParams params, DArgs args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return DReturns();
}

DReturns TypeInstance::Dispatch(const DFunction<SymbolScope::TYPE>& label,
                                DParams params, DArgs args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return DReturns();
}

DReturns TypeValue::Dispatch(const S<TypeValue>& self,
                             const DFunction<SymbolScope::VALUE>& label,
                             DParams params, DArgs args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return DReturns();
}

bool TypeInstance::CanConvert(const TypeInstance& x, const TypeInstance& y) {
  if (&x == &y) {
    return true;
  }
  if (x.InstanceMergeType() == MergeType::SINGLE &&
      y.InstanceMergeType() == MergeType::SINGLE) {
    return y.CanConvertFrom(x);
  }
  return ExpandCheckLeft(x,y);
}

bool TypeInstance::ExpandCheckLeft(const TypeInstance& x, const TypeInstance& y) {
  for (const TypeInstance* left : x.MergedTypes()) {
    const bool result = ExpandCheckRight(*left,y);
    switch (x.InstanceMergeType()) {
      case MergeType::SINGLE:
        return result;
      case MergeType::UNION:
        if (!result) {
          return false;
        }
        break;
      case MergeType::INTERSECT:
        if (result) {
          return true;
        }
        break;
    }
  }
  switch (x.InstanceMergeType()) {
    case MergeType::SINGLE:    return false;
    case MergeType::UNION:     return true;
    case MergeType::INTERSECT: return false;
  }
}

bool TypeInstance::ExpandCheckRight(const TypeInstance& x, const TypeInstance& y) {
  for (const TypeInstance* right : y.MergedTypes()) {
    const bool result = TypeInstance::CanConvert(x,*right);
    switch (y.InstanceMergeType()) {
      case MergeType::SINGLE:
        return result;
      case MergeType::UNION:
        if (result) {
          return true;
        }
        break;
      case MergeType::INTERSECT:
        if (!result) {
          return false;
        }
        break;
    }
  }
  switch (y.InstanceMergeType()) {
    case MergeType::SINGLE:    return false;
    case MergeType::UNION:     return false;
    case MergeType::INTERSECT: return true;
  }
}

Returns<1>::Type TypeValue::Present(S<TypeValue> target) {
  FAIL_IF(target == nullptr) << "Builtin called on null value";
  return T_get(Box_Bool(target->Present()));
}

Returns<1>::Type TypeValue::Require(S<TypeValue> target) {
  FAIL_IF(target == nullptr) << "Builtin called on null value";
  if (!target->Present()) {
    FAIL() << "Cannot require empty value";
  }
  return T_get(target);
}

Returns<1>::Type TypeValue::Strong(W<TypeValue> target) {
  const auto strong = target.lock();
  return T_get(strong? strong : Var_empty);
}

bool TypeValue::AsBool() const {
  FAIL() << "Not a Bool value";
  return false;
}

std::string TypeValue::AsString() const {
  FAIL() << "Not a String value";
  return "";
}

int TypeValue::AsInt() const {
  FAIL() << "Not an Int value";
  return 0;
}

double TypeValue::AsFloat() const {
  FAIL() << "Not a Float value";
  return 0.0;
}

bool TypeValue::Present() const {
  return true;
}
