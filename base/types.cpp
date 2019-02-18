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
