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
#include "Streamlined_SimpleInput.hpp"
#include "Category_Formatted.hpp"
#include "Category_String.hpp"

#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE

namespace {
extern const BoxedValue& Var_stdin;
}  // namespace

struct ExtCategory_SimpleInput : public Category_SimpleInput {
};

struct ExtType_SimpleInput : public Type_SimpleInput {
  inline ExtType_SimpleInput(Category_SimpleInput& p, Params<0>::Type params) : Type_SimpleInput(p, params) {}

  ReturnTuple Call_stdin(const S<TypeInstance>& Param_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleInput.stdin")
    return ReturnTuple(Var_stdin);
  }
};

struct ExtValue_SimpleInput : public Value_SimpleInput {
  inline ExtValue_SimpleInput(S<Type_SimpleInput> p, const ValueTuple& args) : Value_SimpleInput(p) {}

  ReturnTuple Call_pastEnd(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleInput.pastEnd")
    std::lock_guard<std::mutex> lock(mutex);
    return ReturnTuple(Box_Bool(zero_read));
  }

  ReturnTuple Call_readBlock(const BoxedValue& Var_self, const ParamTuple& params, const ValueTuple& args) final {
    TRACE_FUNCTION("SimpleInput.readBlock")
    std::lock_guard<std::mutex> lock(mutex);
    const int size = args.At(0).AsInt();
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
const BoxedValue& Var_stdin = *new BoxedValue(new ExtValue_SimpleInput(CreateType_SimpleInput(Params<0>::Type()), ArgTuple()));
}  // namespace

Category_SimpleInput& CreateCategory_SimpleInput() {
  static auto& category = *new ExtCategory_SimpleInput();
  return category;
}
S<Type_SimpleInput> CreateType_SimpleInput(Params<0>::Type params) {
  static const auto cached = S_get(new ExtType_SimpleInput(CreateCategory_SimpleInput(), Params<0>::Type()));
  return cached;
}

#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
