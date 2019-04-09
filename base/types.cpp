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

#include "types.hpp"

#include "logging.hpp"


int ArgTuple::Size() const {
  return args_.size();
}

const S<TypeValue>& ArgTuple::At(int pos) const {
  if (pos < 0 || pos >= args_.size()) {
    FAIL() << "Bad ArgTuple index";
  }
  return *args_[pos];
}

const S<TypeValue>& ArgTuple::Only() const {
  if (args_.size() != 1) {
    FAIL() << "Bad ArgTuple index";
  }
  return *args_[0];
}

int ReturnTuple::Size() const {
  return returns_.size();
}

S<TypeValue>& ReturnTuple::At(int pos) {
  if (pos < 0 || pos >= returns_.size()) {
    FAIL() << "Bad ReturnTuple index";
  }
  return returns_[pos];
}

const S<TypeValue>& ReturnTuple::At(int pos) const {
  if (pos < 0 || pos >= returns_.size()) {
    FAIL() << "Bad ReturnTuple index";
  }
  return returns_[pos];
}

const S<TypeValue>& ReturnTuple::Only() const {
  if (returns_.size() != 1) {
    FAIL() << "Bad ReturnTuple index";
  }
  return returns_[0];
}

int ParamTuple::Size() const {
  return params_.size();
}

TypeInstance* ParamTuple::At(int pos) const {
  if (pos < 0 || pos >= params_.size()) {
    FAIL() << "Bad ParamTuple index";
  }
  return params_[pos];
}
