/* -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

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

#include <fstream>

#include "category-source.hpp"
#include "Category_BlockWriter.hpp"
#include "Category_PersistentResource.hpp"
#include "Category_RawFileWriter.hpp"


#ifdef ZEOLITE_DYNAMIC_NAMESPACE
namespace ZEOLITE_DYNAMIC_NAMESPACE {
#endif  // ZEOLITE_DYNAMIC_NAMESPACE

namespace {
const int collection = 0;
}  // namespace

const void* const Functions_RawFileWriter = &collection;
const TypeFunction& Function_RawFileWriter_open = (*new TypeFunction{ 0, 1, 1, "RawFileWriter", "open", Functions_RawFileWriter, 0 });
const ValueFunction& Function_RawFileWriter_getFileError = (*new ValueFunction{ 0, 0, 1, "RawFileWriter", "getFileError", Functions_RawFileWriter, 0 });

namespace {
class Category_RawFileWriter;
class Type_RawFileWriter;
Type_RawFileWriter& CreateType(Params<0>::Type params);
class Value_RawFileWriter;
S<TypeValue> CreateValue(Type_RawFileWriter& parent, const ParamTuple& params, const ValueTuple& args);
struct Category_RawFileWriter : public TypeCategory {
  std::string CategoryName() const final { return "RawFileWriter"; }
  Category_RawFileWriter() {
    CycleCheck<Category_RawFileWriter>::Check();
    CycleCheck<Category_RawFileWriter> marker(*this);
    TRACE_FUNCTION("RawFileWriter (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_RawFileWriter::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_RawFileWriter& CreateCategory() {
  static auto& category = *new Category_RawFileWriter();
  return category;
}
struct Type_RawFileWriter : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_RawFileWriter& parent;
  bool CanConvertFrom(const TypeInstance& from) const final {
    std::vector<const TypeInstance*> args;
    if (!from.TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<const TypeInstance*>& args) const final {
    if (&category == &GetCategory_RawFileWriter()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_PersistentResource()) {
      args = std::vector<const TypeInstance*>{};
      return true;
    }
    if (&category == &GetCategory_BlockWriter()) {
      args = std::vector<const TypeInstance*>{&GetType_String(T_get())};
      return true;
    }
    return false;
  }
  Type_RawFileWriter(Category_RawFileWriter& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_RawFileWriter>::Check();
    CycleCheck<Type_RawFileWriter> marker(*this);
    TRACE_FUNCTION("RawFileWriter (init @type)")
  }
  ReturnTuple Dispatch(const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_RawFileWriter::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_RawFileWriter[] = {
      &Type_RawFileWriter::Call_open,
    };
    if (label.collection == Functions_RawFileWriter) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_RawFileWriter[label.function_num])(params, args);
    }
    return TypeInstance::Dispatch(label, params, args);
  }
  ReturnTuple Call_open(const ParamTuple& params, const ValueTuple& args);
};
Type_RawFileWriter& CreateType(Params<0>::Type params) {
  static auto& cache = *new InstanceMap<0,Type_RawFileWriter>();
  auto& cached = cache[params];
  if (!cached) { cached = R_get(new Type_RawFileWriter(CreateCategory(), params)); }
  return *cached;
}
struct Value_RawFileWriter : public TypeValue {
  Value_RawFileWriter(Type_RawFileWriter& p, const ParamTuple& params, const ValueTuple& args)
    : parent(p), file(new std::ofstream(args.At(0)->AsString(), std::ios::out | std::ios::binary | std::ios::trunc | std::ios::ate)) {}

  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_RawFileWriter::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_BlockWriter[] = {
      &Value_RawFileWriter::Call_writeBlock,
    };
    static const CallType Table_PersistentResource[] = {
      &Value_RawFileWriter::Call_freeResource,
    };
    static const CallType Table_RawFileWriter[] = {
      &Value_RawFileWriter::Call_getFileError,
    };
    if (label.collection == Functions_BlockWriter) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_BlockWriter[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_PersistentResource) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_PersistentResource[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_RawFileWriter) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_RawFileWriter[label.function_num])(self, params, args);
    }
    return TypeValue::Dispatch(self, label, params, args);
  }
  std::string CategoryName() const final { return parent.CategoryName(); }
  ReturnTuple Call_freeResource(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_getFileError(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_writeBlock(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  Type_RawFileWriter& parent;

  R<std::ostream> file;
};
S<TypeValue> CreateValue(Type_RawFileWriter& parent, const ParamTuple& params, const ValueTuple& args) {
  return S_get(new Value_RawFileWriter(parent, params, args));
}

ReturnTuple Type_RawFileWriter::Call_open(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("RawFileWriter.create")
  return ReturnTuple(CreateValue(*this, ParamTuple(), args));
}
ReturnTuple Value_RawFileWriter::Call_freeResource(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("RawFileWriter.freeResource")
  if (file) {
    file = nullptr;
  }
  return ReturnTuple();
}
ReturnTuple Value_RawFileWriter::Call_getFileError(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("RawFileWriter.getFileError")
  if (!file) {
    return ReturnTuple(Box_String(PrimString_FromLiteral("file has already been closed")));
  }
  if (file->rdstate() & std::ios::badbit) {
    return ReturnTuple(Box_String(PrimString_FromLiteral("file could not be written or opened")));
  }
  if (file->rdstate() & std::ios::failbit) {
    return ReturnTuple(Box_String(PrimString_FromLiteral("file could not be written or opened")));
  }
  return ReturnTuple(Var_empty);
}
ReturnTuple Value_RawFileWriter::Call_writeBlock(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("RawFileWriter.writeBlock")
  const PrimString& Var_arg1 = args.At(0)->AsString();
  int write_size = 0;
  if (file) {
    file->write(&Var_arg1[0], Var_arg1.size());
    file->flush();
    write_size = file->fail()? 0 : Var_arg1.size();
  }
  return ReturnTuple(Box_Int(write_size));
}

}  // namespace

TypeCategory& GetCategory_RawFileWriter() {
  return CreateCategory();
}
TypeInstance& GetType_RawFileWriter(Params<0>::Type params) {
  return CreateType(params);
}

#ifdef ZEOLITE_DYNAMIC_NAMESPACE
}  // namespace ZEOLITE_DYNAMIC_NAMESPACE
using namespace ZEOLITE_DYNAMIC_NAMESPACE;
#endif  // ZEOLITE_DYNAMIC_NAMESPACE
