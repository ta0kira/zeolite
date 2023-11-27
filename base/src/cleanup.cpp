/* -----------------------------------------------------------------------------
Copyright 2023 Kevin P. Barry

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

#include "cleanup.hpp"

#include "category-source.hpp"


// static
void GlobalCleanup::Finish(int code) {
  GlobalCleanup* current = GetCurrent();
  while (current) {
    current->Cleanup(code);
    current = current->GetNext();
  }
}

WrapTypeCall::WrapTypeCall(S<const TypeInstance> type, const TypeFunction* start, const TypeFunction* finish)
  : type_(std::move(type)), finish_(finish), cross_and_capture_to_(this) {
  if (start) {
    (void) TypeInstance::Call(type_, *start, PassParamsArgs());
  }
}

WrapTypeCall::~WrapTypeCall() {
  Cleanup(0);
}

GlobalCleanup* WrapTypeCall::GetNext() const {
  return cross_and_capture_to_.Previous();
}

void WrapTypeCall::Cleanup(int code) {
  if (finish_) {
    (void) TypeInstance::Call(type_, *finish_, PassParamsArgs());
    finish_ = nullptr;
  }
}
