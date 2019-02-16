#ifndef CATEGORY_SOURCE_HPP_
#define CATEGORY_SOURCE_HPP_

#include <map>
#include <vector>

#include "types.hpp"
#include "function.hpp"
#include "builtin.hpp"
#include "cycle-check.hpp"


class TypeCategory {
 public:
  inline ReturnTuple Call(const DFunction<SymbolScope::CATEGORY>& label,
                          const ParamTuple& params, const ValueTuple& args) {
    return Dispatch(label, params, args);
  }

  virtual std::string CategoryName() const = 0;

  ALWAYS_PERMANENT(TypeCategory)
  virtual ~TypeCategory() = default;

 protected:
  TypeCategory() = default;

  virtual ReturnTuple Dispatch(const DFunction<SymbolScope::CATEGORY>& label,
                               const ParamTuple& params, const ValueTuple& args);
};

class TypeInstance {
 public:
  inline ReturnTuple Call(const DFunction<SymbolScope::TYPE>& label,
                          ParamTuple params, const ValueTuple& args) {
    return Dispatch(label, params, args);
  }

  virtual std::string CategoryName() const = 0;

  static S<TypeValue> Reduce(TypeInstance& from, TypeInstance& to, S<TypeValue> target) {
    TRACE_FUNCTION("reduce")
    return CanConvert(from, to)? target : Var_empty;
  }

  virtual bool TypeArgsForParent(
    const TypeCategory& category, std::vector<const TypeInstance*>& args) const
  { return false; }

  ALWAYS_PERMANENT(TypeInstance)
  virtual ~TypeInstance() = default;

 protected:
  TypeInstance() = default;

  virtual ReturnTuple Dispatch(const DFunction<SymbolScope::TYPE>& label,
                               const ParamTuple& params, const ValueTuple& args);

  virtual bool CanConvertFrom(const TypeInstance& from) const
  { return false; }

  static bool CanConvert(const TypeInstance& from, const TypeInstance& to);

  enum class MergeType {
    SINGLE,
    UNION,
    INTERSECT,
  };

 private:
  virtual MergeType InstanceMergeType() const
  { return MergeType::SINGLE; }

  virtual std::vector<const TypeInstance*> MergedTypes() const
  { return std::vector<const TypeInstance*>{this}; }

  static bool ExpandCheckLeft(const TypeInstance& from, const TypeInstance& to);
  static bool ExpandCheckRight(const TypeInstance& from, const TypeInstance& to);
};

class TypeValue {
 public:
  inline static ReturnTuple Call(const S<TypeValue>& target,
                                 const DFunction<SymbolScope::VALUE>& label,
                                 const ParamTuple& params, const ValueTuple& args) {
    FAIL_IF(target == nullptr);
    return target->Dispatch(target, label, params, args);
  }

  virtual std::string CategoryName() const = 0;

  static bool Present(S<TypeValue> target);
  static S<TypeValue> Require(S<TypeValue> target);
  static S<TypeValue> Strong(W<TypeValue> target);

  virtual bool AsBool() const;
  virtual std::string AsString() const;
  virtual int AsInt() const;  // TODO: Use explict precision here.
  virtual double AsFloat() const;

  ALWAYS_PERMANENT(TypeValue)
  virtual ~TypeValue() = default;

 protected:
  TypeValue() = default;

  virtual bool Present() const;

  virtual ReturnTuple Dispatch(const S<TypeValue>& self,
                               const DFunction<SymbolScope::VALUE>& label,
                               const ParamTuple& params, const ValueTuple& args);
};

template<int P, class T>
using InstanceMap = std::map<typename Params<P>::Type, R<T>>;

#endif  // CATEGORY_SOURCE_HPP_
