/* -----------------------------------------------------------------------------
Copyright 2020-2021 Kevin P. Barry
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
#include <mutex>

#include "category-source.hpp"
#include "Streamlined_RawFileWriter.hpp"
#include "Category_Formatted.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_RawFileWriter(S<const Type_RawFileWriter> parent, const ParamsArgs& params_args);

struct ExtCategory_RawFileWriter : public Category_RawFileWriter {
};

struct ExtType_RawFileWriter : public Type_RawFileWriter {
  inline ExtType_RawFileWriter(Category_RawFileWriter& p, Params<0>::Type params) : Type_RawFileWriter(p, params) {}

  ReturnTuple Call_open(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("RawFileWriter.open")
    return ReturnTuple(CreateValue_RawFileWriter(PARAM_SELF, params_args));
  }
};

struct ExtValue_RawFileWriter : public Value_RawFileWriter {
  inline ExtValue_RawFileWriter(S<const Type_RawFileWriter> p, const ParamsArgs& params_args)
    : Value_RawFileWriter(std::move(p)),
      filename(params_args.GetArg(0).AsString()),
      file(new std::ofstream(filename, std::ios::out | std::ios::binary | std::ios::trunc | std::ios::ate)) {}

  ReturnTuple Call_freeResource(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("RawFileWriter.freeResource")
    std::lock_guard<std::mutex> lock(mutex);
    if (file) {
      file = nullptr;
    }
    return ReturnTuple();
  }

  ReturnTuple Call_getFileError(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("RawFileWriter.getFileError")
    std::lock_guard<std::mutex> lock(mutex);
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

  ReturnTuple Call_writeBlock(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("RawFileWriter.writeBlock")
    TRACE_CREATION
    std::lock_guard<std::mutex> lock(mutex);
    const PrimString& Var_arg1 = params_args.GetArg(0).AsString();
    if (!file || file->rdstate() != std::ios::goodbit) {
      FAIL() << "Error writing file \"" << filename << "\"";
    }
    int write_size = 0;
    if (file) {
      file->write(&Var_arg1[0], Var_arg1.size());
      file->flush();
      write_size = file->fail()? 0 : Var_arg1.size();
    }
    return ReturnTuple(Box_Int(write_size));
  }

  std::mutex mutex;
  const std::string filename;
  R<std::ostream> file;
  CAPTURE_CREATION("RawFileWriter")
};

Category_RawFileWriter& CreateCategory_RawFileWriter() {
  static auto& category = *new ExtCategory_RawFileWriter();
  return category;
}

S<const Type_RawFileWriter> CreateType_RawFileWriter(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_RawFileWriter(CreateCategory_RawFileWriter(), Params<0>::Type()));
  return cached;
}

void RemoveType_RawFileWriter(const Params<0>::Type& params) {}

BoxedValue CreateValue_RawFileWriter(S<const Type_RawFileWriter> parent, const ParamsArgs& params_args) {
  return BoxedValue::New<ExtValue_RawFileWriter>(std::move(parent), params_args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
