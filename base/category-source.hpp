/* -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- */

// Author: Kevin P. Barry [ta0kira@gmail.com]

#ifndef CATEGORY_SOURCE_HPP_
#define CATEGORY_SOURCE_HPP_

#include <map>
#include <sstream>
#include <vector>

#include "types.hpp"
#include "function.hpp"
#include "argv.hpp"
#include "cycle-check.hpp"


void BuiltinFail(const S<TypeValue>& formatted) __attribute__ ((noreturn));

extern const S<TypeValue>& Var_empty;

S<TypeValue> Box_Bool(bool value);
S<TypeValue> Box_String(const PrimString& value);
S<TypeValue> Box_Char(PrimChar value);
S<TypeValue> Box_Int(PrimInt value);
S<TypeValue> Box_Float(PrimFloat value);


class TypeCategory {
 public:
  inline ReturnTuple Call(const CategoryFunction& label,
                          const ParamTuple& params, const ValueTuple& args) {
    return Dispatch(label, params, args);
  }

  virtual std::string CategoryName() const = 0;

  ALWAYS_PERMANENT(TypeCategory)
  virtual ~TypeCategory() = default;

 protected:
  TypeCategory() = default;

  virtual ReturnTuple Dispatch(const CategoryFunction& label,
                               const ParamTuple& params, const ValueTuple& args);
};

class TypeInstance {
 public:
  inline ReturnTuple Call(const TypeFunction& label,
                          ParamTuple params, const ValueTuple& args) {
    return Dispatch(label, params, args);
  }

  virtual std::string CategoryName() const = 0;
  virtual void BuildTypeName(std::ostream& output) const = 0;


  static std::string TypeName(const TypeInstance& type) {
    std::ostringstream output;
    type.BuildTypeName(output);
    return output.str();
  }

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

  virtual ReturnTuple Dispatch(const TypeFunction& label,
                               const ParamTuple& params, const ValueTuple& args);

  virtual bool CanConvertFrom(const TypeInstance& from) const
  { return false; }

  static bool CanConvert(const TypeInstance& from, const TypeInstance& to);

  template<class...Ts>
  static void TypeNameFrom(std::ostream& output, const TypeCategory& category,
                           const Ts&... params) {
    std::vector<const TypeInstance*> params2{&params...};
    output << category.CategoryName();
    if (!params2.empty()) {
      output << "<";
      bool first = true;
      for (const auto param : params2) {
        if (!first) output << ",";
        first = false;
        param->BuildTypeName(output);
      }
      output << ">";
    }
  }

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
                                 const ValueFunction& label,
                                 const ParamTuple& params, const ValueTuple& args) {
    if (target == nullptr) {
      FAIL() << "Function called on null value";
    }
    return target->Dispatch(target, label, params, args);
  }

  virtual std::string CategoryName() const = 0;

  static bool Present(S<TypeValue> target);
  static S<TypeValue> Require(S<TypeValue> target);
  static S<TypeValue> Strong(W<TypeValue> target);

  virtual bool AsBool() const;
  virtual const PrimString& AsString() const;
  virtual PrimChar AsChar() const;
  virtual PrimInt AsInt() const;
  virtual PrimFloat AsFloat() const;

  ALWAYS_PERMANENT(TypeValue)
  virtual ~TypeValue() = default;

 protected:
  TypeValue() = default;

  virtual bool Present() const;

  virtual ReturnTuple Dispatch(const S<TypeValue>& self,
                               const ValueFunction& label,
                               const ParamTuple& params, const ValueTuple& args);
};

template<int P, class T>
using InstanceMap = std::map<typename Params<P>::Type, R<T>>;

#endif  // CATEGORY_SOURCE_HPP_
