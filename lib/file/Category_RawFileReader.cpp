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
#include "Category_BlockReader.hpp"
#include "Category_Bool.hpp"
#include "Category_Formatted.hpp"
#include "Category_Int.hpp"
#include "Category_PersistentResource.hpp"
#include "Category_RawFileReader.hpp"
#include "Category_String.hpp"
#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE
namespace {
const int collection_RawFileReader = 0;
}  // namespace
const void* const Functions_RawFileReader = &collection_RawFileReader;
const TypeFunction& Function_RawFileReader_open = (*new TypeFunction{ 0, 1, 1, "RawFileReader", "open", Functions_RawFileReader, 0 });
const ValueFunction& Function_RawFileReader_getFileError = (*new ValueFunction{ 0, 0, 1, "RawFileReader", "getFileError", Functions_RawFileReader, 0 });
namespace {
class Category_RawFileReader;
class Type_RawFileReader;
S<Type_RawFileReader> CreateType_RawFileReader(Params<0>::Type params);
class Value_RawFileReader;
S<TypeValue> CreateValue_RawFileReader(S<Type_RawFileReader> parent, const ParamTuple& params, const ValueTuple& args);
struct Category_RawFileReader : public TypeCategory {
  std::string CategoryName() const final { return "RawFileReader"; }
  Category_RawFileReader() {
    CycleCheck<Category_RawFileReader>::Check();
    CycleCheck<Category_RawFileReader> marker(*this);
    TRACE_FUNCTION("RawFileReader (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_RawFileReader::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_RawFileReader& CreateCategory_RawFileReader() {
  static auto& category = *new Category_RawFileReader();
  return category;
}
struct Type_RawFileReader : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_RawFileReader& parent;
  bool CanConvertFrom(const S<const TypeInstance>& from) const final {
    std::vector<S<const TypeInstance>> args;
    if (!from->TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<S<const TypeInstance>>& args) const final {
    if (&category == &GetCategory_RawFileReader()) {
      args = std::vector<S<const TypeInstance>>{};
      return true;
    }
    if (&category == &GetCategory_PersistentResource()) {
      args = std::vector<S<const TypeInstance>>{};
      return true;
    }
    if (&category == &GetCategory_BlockReader()) {
      args = std::vector<S<const TypeInstance>>{GetType_String(T_get())};
      return true;
    }
    return false;
  }
  Type_RawFileReader(Category_RawFileReader& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_RawFileReader>::Check();
    CycleCheck<Type_RawFileReader> marker(*this);
    TRACE_FUNCTION("RawFileReader (init @type)")
  }
  ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_RawFileReader::*)(const S<TypeInstance>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_RawFileReader[] = {
      &Type_RawFileReader::Call_open,
    };
    if (label.collection == Functions_RawFileReader) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_RawFileReader[label.function_num])(self, params, args);
    }
    return TypeInstance::Dispatch(self, label, params, args);
  }
  ReturnTuple Call_open(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args);
};
S<Type_RawFileReader> CreateType_RawFileReader(Params<0>::Type params) {
  static const auto cached = S_get(new Type_RawFileReader(CreateCategory_RawFileReader(), Params<0>::Type()));
  return cached;
}
struct Value_RawFileReader : public TypeValue {
  Value_RawFileReader(S<Type_RawFileReader> p, const ParamTuple& params, const ValueTuple& args)
    : parent(p), filename(args.At(0)->AsString()),
      file(new std::ifstream(filename, std::ios::in | std::ios::binary)) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_RawFileReader::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_BlockReader[] = {
      &Value_RawFileReader::Call_pastEnd,
      &Value_RawFileReader::Call_readBlock,
    };
    static const CallType Table_PersistentResource[] = {
      &Value_RawFileReader::Call_freeResource,
    };
    static const CallType Table_RawFileReader[] = {
      &Value_RawFileReader::Call_getFileError,
    };
    if (label.collection == Functions_BlockReader) {
      if (label.function_num < 0 || label.function_num >= 2) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_BlockReader[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_PersistentResource) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_PersistentResource[label.function_num])(self, params, args);
    }
    if (label.collection == Functions_RawFileReader) {
      if (label.function_num < 0 || label.function_num >= 1) {
        FAIL() << "Bad function call " << label;
      }
      return (this->*Table_RawFileReader[label.function_num])(self, params, args);
    }
    return TypeValue::Dispatch(self, label, params, args);
  }
  std::string CategoryName() const final { return parent->CategoryName(); }
  ReturnTuple Call_freeResource(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_getFileError(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_pastEnd(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_readBlock(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  const S<Type_RawFileReader> parent;
  std::mutex mutex;
  const std::string filename;
  R<std::istream> file;
  CAPTURE_CREATION
};
S<TypeValue> CreateValue_RawFileReader(S<Type_RawFileReader> parent, const ParamTuple& params, const ValueTuple& args) {
  return S_get(new Value_RawFileReader(parent, params, args));
}
ReturnTuple Type_RawFileReader::Call_open(const S<TypeInstance>& self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("RawFileReader.open")
  return ReturnTuple(CreateValue_RawFileReader(CreateType_RawFileReader(Params<0>::Type()), ParamTuple(), args));
}
ReturnTuple Value_RawFileReader::Call_freeResource(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("RawFileReader.freeResource")
  std::lock_guard<std::mutex> lock(mutex);
  if (file) {
    file = nullptr;
  }
  return ReturnTuple();
}
ReturnTuple Value_RawFileReader::Call_getFileError(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("RawFileReader.getFileError")
  std::lock_guard<std::mutex> lock(mutex);
  if (!file) {
    return ReturnTuple(Box_String(PrimString_FromLiteral("file has already been closed")));
  }
  if (file->rdstate() & std::ios::badbit) {
    return ReturnTuple(Box_String(PrimString_FromLiteral("file could not be read or opened")));
  }
  if (file->rdstate() & std::ios::failbit) {
    return ReturnTuple(Box_String(PrimString_FromLiteral("file could not be read or opened")));
  }
  return ReturnTuple(Var_empty);
}
ReturnTuple Value_RawFileReader::Call_pastEnd(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("RawFileReader.pastEnd")
  std::lock_guard<std::mutex> lock(mutex);
  return ReturnTuple(Box_Bool(!file || file->fail() || file->eof()));
}
ReturnTuple Value_RawFileReader::Call_readBlock(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("RawFileReader.readBlock")
  TRACE_CREATION
  std::lock_guard<std::mutex> lock(mutex);
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  if (Var_arg1 < 0) {
    FAIL() << "Read size " << Var_arg1 << " is invalid";
  }
  if (!file || file->rdstate() != std::ios::goodbit) {
    FAIL() << "Error reading file \"" << filename << "\"";
  }
  std::string buffer(Var_arg1, '\x00');
  int read_size = 0;
  if (file) {
    const bool eof = file->eof();
    if (!eof && !file->fail()) {
      file->read(&buffer[0], Var_arg1);
      read_size = file->gcount();
      if (file->fail()) {
        // Clear an EOF-related error, since we can't tell if it's because the
        // file ended or because of a real error.
        file->clear();
        file->setstate(std::ios::eofbit);
      }
    }
  }
  return ReturnTuple(Box_String(buffer.substr(0, read_size)));
}
}  // namespace
TypeCategory& GetCategory_RawFileReader() {
  return CreateCategory_RawFileReader();
}
S<TypeInstance> GetType_RawFileReader(Params<0>::Type params) {
  return CreateType_RawFileReader(params);
}
#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
