/* -----------------------------------------------------------------------------
Copyright 2019 Kevin P. Barry

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

#include "category-source.hpp"

#include "Category_Runner.hpp"
#include "Category_Test.hpp"

int main() {
  SetSignalHandler();
  TRACE_FUNCTION("main")
  // GetType_Test defines Function_Runner_run in test case.
  GetType_Test(T_get()).Call(Function_Runner_run, ParamTuple(), ArgTuple());
}
