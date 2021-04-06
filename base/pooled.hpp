/* -----------------------------------------------------------------------------
Copyright 2021 Kevin P. Barry

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

#ifndef POOLED_HPP_
#define POOLED_HPP_


template<class T>
class PoolStorage {
 public:
  inline T* data() const {
    return (T*) (this + 1);
  }

  const int size = 0;
  PoolStorage* next = nullptr;

private:
  template<class> friend class PoolManager;

  inline PoolStorage(int s, PoolStorage* n) : size(s), next(n) {}

  inline ~PoolStorage() {}

  PoolStorage(const PoolStorage&) = delete;
  PoolStorage& operator = (const PoolStorage&) = delete;
  PoolStorage(PoolStorage&&) = delete;
  PoolStorage& operator = (PoolStorage&&) = delete;
};

template<class T>
struct PoolManager {};

template<class T>
class PoolArray {
  friend class ArgTuple;
  friend class ReturnTuple;
  friend class ParamTuple;

  constexpr PoolArray() : array_(nullptr) {}

  PoolArray(int size) : array_(PoolManager<T>::Take(size)) {}

  PoolArray(PoolArray&& other) : array_(other.array_) {
    other.array_ = nullptr;
  }

  PoolArray& operator = (PoolArray&& other) {
    array_ = other.array_;
    other.array_ = nullptr;
    return *this;
  }

  const T& operator [] (int i) const {
    if (!array_ || i < 0 || i >= array_->size) {
      FAIL() << "Bad array index " << i;
    }
    return array_->data()[i];
  }

  T& operator [] (int i) {
    if (!array_ || i < 0 || i >= array_->size) {
      FAIL() << "Bad array index " << i;
    }
    return array_->data()[i];
  }

  template<class...Ts>
  inline void Init(Ts... data) {
    if (sizeof...(Ts) > array_->size) {
      FAIL() << "Too many init values " << sizeof...(Ts);
    }
    InitRec(0, std::move(data)...);
  }

  template<class...Ts>
  inline void InitRec(int i, T arg, Ts... data) {
    array_->data()[i] = std::move(arg);
    InitRec(i+1, std::move(data)...);
  }

  inline void InitRec(int i) {}

  ~PoolArray() {
    PoolManager<T>::Return(array_);
  }

  PoolArray(const PoolArray&) = delete;
  PoolArray& operator = (const PoolArray&) = delete;

  PoolStorage<T>* array_;
};

#endif  // POOLED_HPP_
