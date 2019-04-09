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

#ifndef CYCLE_CHECK_HPP_
#define CYCLE_CHECK_HPP_

#include "thread-capture.h"

#include "logging.hpp"


// Type must have a const function CategoryName().
template<class Type>
class CycleCheck : public capture_thread::ThreadCapture<CycleCheck<Type>> {
 public:
  CycleCheck(const Type& type) : type_(type), capture_to_(this) {}

  static void Check() {
    auto current = capture_thread::ThreadCapture<CycleCheck<Type>>::GetCurrent();
    if (current) {
      FAIL() << "Detected cycle in " << current->type_.CategoryName() << " init";
    }
  }

 private:
  const Type& type_;
  const typename capture_thread::ThreadCapture<CycleCheck<Type>>::ScopedCapture capture_to_;
};

#endif  // CYCLE_CHECK_HPP_
