#include "category-source.hpp"

#include "logging.hpp"
#include "builtin.hpp"


ReturnTuple TypeCategory::Dispatch(const DFunction<SymbolScope::CATEGORY>& label,
                                   const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return ReturnTuple(label.ReturnCount());
}

ReturnTuple TypeInstance::Dispatch(const DFunction<SymbolScope::TYPE>& label,
                                   const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return ReturnTuple(label.ReturnCount());
}

ReturnTuple TypeValue::Dispatch(const S<TypeValue>& self,
                                const DFunction<SymbolScope::VALUE>& label,
                                const ParamTuple& params, const ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return ReturnTuple(label.ReturnCount());
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

bool TypeValue::Present(S<TypeValue> target) {
  FAIL_IF(target == nullptr) << "Builtin called on null value";
  return target->Present();
}

S<TypeValue> TypeValue::Require(S<TypeValue> target) {
  FAIL_IF(target == nullptr) << "Builtin called on null value";
  if (!target->Present()) {
    FAIL() << "Cannot require empty value";
  }
  return target;
}

S<TypeValue> TypeValue::Strong(W<TypeValue> target) {
  const auto strong = target.lock();
  return strong? strong : Var_empty;
}

bool TypeValue::AsBool() const {
  FAIL() << CategoryName() << " is not a Bool value";
  return false;
}

PrimString TypeValue::AsString() const {
  FAIL() << CategoryName() << " is not a String value";
  return "";
}

PrimInt TypeValue::AsInt() const {
  FAIL() << CategoryName() << " is not an Int value";
  return 0;
}

PrimFloat TypeValue::AsFloat() const {
  FAIL() << CategoryName() << " is not a Float value";
  return 0.0;
}

bool TypeValue::Present() const {
  return true;
}
