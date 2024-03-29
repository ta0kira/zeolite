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
#include "Streamlined_RawFileReader.hpp"
#include "Category_Formatted.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

BoxedValue CreateValue_RawFileReader(S<const Type_RawFileReader> parent, const ParamsArgs& params_args);

struct ExtCategory_RawFileReader : public Category_RawFileReader {
};

struct ExtType_RawFileReader : public Type_RawFileReader {
  inline ExtType_RawFileReader(Category_RawFileReader& p, Params<0>::Type params) : Type_RawFileReader(p, params) {}

  ReturnTuple Call_open(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("RawFileReader.open")
    return ReturnTuple(CreateValue_RawFileReader(PARAM_SELF, params_args));
  }
};

struct ExtValue_RawFileReader : public Value_RawFileReader {
  inline ExtValue_RawFileReader(S<const Type_RawFileReader> p, const ParamsArgs& params_args)
    : Value_RawFileReader(std::move(p)),
      filename(params_args.GetArg(0).AsString()),
      file(new std::ifstream(filename, std::ios::in | std::ios::binary)) {}

  ReturnTuple Call_freeResource(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("RawFileReader.freeResource")
    std::lock_guard<std::mutex> lock(mutex);
    if (file) {
      file = nullptr;
    }
    return ReturnTuple();
  }

  ReturnTuple Call_getFileError(const ParamsArgs& params_args) final {
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

  ReturnTuple Call_pastEnd(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("RawFileReader.pastEnd")
    std::lock_guard<std::mutex> lock(mutex);
    return ReturnTuple(Box_Bool(!file || file->fail() || file->eof()));
  }

  ReturnTuple Call_readBlock(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("RawFileReader.readBlock")
    TRACE_CREATION
    std::lock_guard<std::mutex> lock(mutex);
    const PrimInt Var_arg1 = (params_args.GetArg(0)).AsInt();
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

  std::mutex mutex;
  const std::string filename;
  R<std::istream> file;
  CAPTURE_CREATION("RawFileReader")
};

Category_RawFileReader& CreateCategory_RawFileReader() {
  static auto& category = *new ExtCategory_RawFileReader();
  return category;
}

S<const Type_RawFileReader> CreateType_RawFileReader(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_RawFileReader(CreateCategory_RawFileReader(), Params<0>::Type()));
  return cached;
}

void RemoveType_RawFileReader(const Params<0>::Type& params) {}

BoxedValue CreateValue_RawFileReader(S<const Type_RawFileReader> parent, const ParamsArgs& params_args) {
  return BoxedValue::New<ExtValue_RawFileReader>(std::move(parent), params_args);
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
