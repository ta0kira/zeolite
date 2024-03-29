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


class ReturnTuple;

namespace zeolite_internal {

template<class T>
class PoolStorage {
 public:
  using Managed = T;

  inline T* data() const {
    return (T*) (this + 1);
  }

  const int size = 0;
  PoolStorage* next = nullptr;

private:
  template<class> friend class PoolCache;
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
  friend class ::ReturnTuple;

  constexpr PoolArray() : size_(0), array_(nullptr) {}

  PoolArray(int size) : size_(size), array_(PoolManager<T>::Take(size_)) {}

  PoolArray(PoolArray&& other) : size_(other.size_), array_(other.array_) {
    other.size_  = 0;
    other.array_ = nullptr;
  }

  PoolArray& operator = (PoolArray&& other) {
    if (&other != this) {
      PoolManager<T>::Return(array_, size_);
      size_  = other.size_;
      array_ = other.array_;
      other.size_  = 0;
      other.array_ = nullptr;
    }
    return *this;
  }

  inline int Size() const {
    return size_;
  }

  const T& operator [] (int i) const {
    return array_->data()[i];
  }

  T& operator [] (int i) {
    return array_->data()[i];
  }

  template<class...Ts>
  inline void Init(Ts... data) {
    InitRec(0, std::move(data)...);
  }

  template<class...Ts>
  inline void InitRec(int i, T arg, Ts... data) {
    array_->data()[i] = std::move(arg);
    InitRec(i+1, std::move(data)...);
  }

  inline void InitRec(int i) {}

  ~PoolArray() {
    PoolManager<T>::Return(array_, size_);
  }

  PoolArray(const PoolArray&) = delete;
  PoolArray& operator = (const PoolArray&) = delete;

  int size_;
  PoolStorage<T>* array_;
};

template<class T>
class PoolCache {
 public:
  PoolCache(int max) : max_(max) {}

  ~PoolCache() {
    while (pool_) {
      --size_;
      auto* current = pool_;
      pool_ = pool_->next;
      current->~PoolStorage<T>();
      delete[] (unsigned char*) current;
    }
  }

 private:
  template<class> friend class PoolManager;

  inline PoolStorage<T>* Take() {
    PoolStorage<T>* const storage = pool_;
    if (storage == nullptr) {
      return nullptr;
    } else {
      --size_;
      pool_ = storage->next;
      storage->next = nullptr;
      return storage;
    }
  }

  inline bool Return(PoolStorage<T>* storage) {
    PoolStorage<T>* const head = pool_;
    if (size_ < max_) {
      ++size_;
      storage->next = head;
      pool_ = storage;
      return true;
    } else {
      return false;
    }
  }

  PoolStorage<T>* pool_;
  int size_ = 0;
  const int max_;
};

}  // namespace zeolite_internal

#endif  // POOLED_HPP_
