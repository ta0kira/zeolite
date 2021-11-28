/* -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

// TODO: Maybe use C++ instead.
#include <unistd.h>

#include "category-source.hpp"
#include "Streamlined_BasicInput.hpp"
#include "Category_Formatted.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

namespace {
extern const BoxedValue Var_stdin;
}  // namespace

struct ExtCategory_BasicInput : public Category_BasicInput {
};

struct ExtType_BasicInput : public Type_BasicInput {
  inline ExtType_BasicInput(Category_BasicInput& p, Params<0>::Type params) : Type_BasicInput(p, params) {}

  ReturnTuple Call_stdin(const ParamsArgs& params_args) const final {
    TRACE_FUNCTION("BasicInput.stdin")
    return ReturnTuple(Var_stdin);
  }
};

struct ExtValue_BasicInput : public Value_BasicInput {
  inline ExtValue_BasicInput(S<const Type_BasicInput> p)
    : Value_BasicInput(std::move(p)) {}

  ReturnTuple Call_pastEnd(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("BasicInput.pastEnd")
    std::lock_guard<std::mutex> lock(mutex);
    return ReturnTuple(Box_Bool(zero_read));
  }

  ReturnTuple Call_readBlock(const ParamsArgs& params_args) final {
    TRACE_FUNCTION("BasicInput.readBlock")
    std::lock_guard<std::mutex> lock(mutex);
    const int size = params_args.GetArg(0).AsInt();
    if (size < 0) {
      FAIL() << "Read size " << size << " is invalid";
    }
    std::string buffer(size, '\x00');
    const int read_size = read(STDIN_FILENO, &buffer[0], size);
    if (read_size < 0) {
      return ReturnTuple(Box_String(""));
    } else {
      zero_read = read_size == 0;
      return ReturnTuple(Box_String(buffer.substr(0, read_size)));
    }
  }

  bool zero_read = false;
  std::mutex mutex;
};

namespace {
const BoxedValue Var_stdin = BoxedValue::New<ExtValue_BasicInput>(CreateType_BasicInput(Params<0>::Type()));
}  // namespace

Category_BasicInput& CreateCategory_BasicInput() {
  static auto& category = *new ExtCategory_BasicInput();
  return category;
}

S<const Type_BasicInput> CreateType_BasicInput(const Params<0>::Type& params) {
  static const auto cached = S_get(new ExtType_BasicInput(CreateCategory_BasicInput(), Params<0>::Type()));
  return cached;
}

void RemoveType_BasicInput(const Params<0>::Type& params) {}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
