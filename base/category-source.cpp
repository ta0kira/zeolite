#include "category-source.hpp"

#include "logging.hpp"
#include "builtin.hpp"


ReturnTuple TypeCategory::Dispatch(const DFunction<SymbolScope::CATEGORY>& label,
                                   const ParamTuple& params, ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return ReturnTuple(label.ReturnCount());
}

ReturnTuple TypeInstance::Dispatch(const DFunction<SymbolScope::TYPE>& label,
                                   const ParamTuple& params, ValueTuple& args) {
  FAIL() << CategoryName() << " does not implement " << label.FunctionName();
  return ReturnTuple(label.ReturnCount());
}

ReturnTuple TypeValue::Dispatch(const S<TypeValue>& self,
                                const DFunction<SymbolScope::VALUE>& label,
                                const ParamTuple& params, ValueTuple& args) {
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

ReturnTuple TypeValue::Present(S<TypeValue> target) {
  FAIL_IF(target == nullptr) << "Builtin called on null value";
  return ReturnTuple(Box_Bool(target->Present()));
}

ReturnTuple TypeValue::Require(S<TypeValue> target) {
  FAIL_IF(target == nullptr) << "Builtin called on null value";
  if (!target->Present()) {
    FAIL() << "Cannot require empty value";
  }
  return ReturnTuple(target);
}

ReturnTuple TypeValue::Strong(W<TypeValue> target) {
  const auto strong = target.lock();
  return ReturnTuple(strong? strong : Var_empty);
}

bool TypeValue::AsBool() const {
  FAIL() << CategoryName() << " is not a Bool value";
  return false;
}

std::string TypeValue::AsString() const {
  FAIL() << CategoryName() << " is not a String value";
  return "";
}

int TypeValue::AsInt() const {
  FAIL() << CategoryName() << " is not an Int value";
  return 0;
}

double TypeValue::AsFloat() const {
  FAIL() << CategoryName() << " is not a Float value";
  return 0.0;
}

bool TypeValue::Present() const {
  return true;
}
