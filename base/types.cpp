#include "types.hpp"

#include "logging.hpp"


int ArgTuple::Size() const {
  return args_.size();
}

S<TypeValue>& ArgTuple::At(int pos) {
  FAIL_IF(pos < 0 || pos >= args_.size());
  FAIL_IF(args_[pos].Size() != 1);
  return args_[pos].At(0);
}

const S<TypeValue>& ArgTuple::At(int pos) const {
  FAIL_IF(pos < 0 || pos >= args_.size());
  FAIL_IF(args_[pos].Size() != 1);
  return args_[pos].Only();
}

const S<TypeValue>& ArgTuple::Only() const {
  FAIL_IF(args_.size() != 1);
  FAIL_IF(args_[0].Size() != 1);
  return args_[0].Only();
}

int ReturnTuple::Size() const {
  return returns_.size();
}

S<TypeValue>& ReturnTuple::At(int pos) {
  FAIL_IF(pos < 0 || pos >= returns_.size());
  return returns_[pos];
}

const S<TypeValue>& ReturnTuple::At(int pos) const {
  FAIL_IF(pos < 0 || pos >= returns_.size());
  return returns_[pos];
}

const S<TypeValue>& ReturnTuple::Only() const {
  FAIL_IF(returns_.size() != 1);
  return returns_[0];
}

int ParamTuple::Size() const {
  return params_.size();
}

TypeInstance* ParamTuple::At(int pos) const {
  FAIL_IF(pos < 0 || pos >= params_.size());
  return params_[pos];
}
