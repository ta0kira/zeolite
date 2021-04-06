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


enum class PoolStorageType {
  EMPTY,
  DYNAMIC,
  POOLED,
};

template<class T>
class PoolStorage {
 public:
  static PoolStorage* New(int size) {
    if (size < 1) return nullptr;
    PoolStorage* const storage = new (new unsigned char[sizeof(PoolStorage<T>) + size*sizeof(T)]) PoolStorage(size, PoolStorageType::DYNAMIC, nullptr);
    new (storage->data()) T[size];
    return storage;
  }

  static void Delete(PoolStorage* storage) {
    if (storage) {
      for (int i = 0; i < storage->size; ++i) {
        storage->data()[i].~T();
      }
      switch (storage->type) {
        case PoolStorageType::EMPTY:
          break;
        case PoolStorageType::DYNAMIC:
          delete[] (unsigned char*) storage;
          break;
        case PoolStorageType::POOLED:
          FAIL() << "pool-managed storage not implemented";
          break;
      }
    }
  }

  T* data() const {
    return (T*) (this + 1);
  }

  constexpr PoolStorage() : size(0), type(PoolStorageType::EMPTY), next(nullptr) {}

  PoolStorage(int s, PoolStorageType t, PoolStorage<T>* n) : size(s), type(t), next(n) {}

  const int size = 0;
  const PoolStorageType type = PoolStorageType::EMPTY;
  PoolStorage<T>* next = nullptr;
};

template<class T>
class PoolArray {
 public:
  constexpr PoolArray() : array_(nullptr) {}

  PoolArray(int size) : array_(PoolStorage<T>::New(size)) {}

  PoolArray(PoolArray&& other) : array_(other.array_) {
    other.array_ = nullptr;
  }

  PoolArray& operator = (PoolArray&& other) {
    array_ = other.array_;
    other.array_ = nullptr;
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
  void Init(Ts... data) {
    T copy[] = { std::move(data)... };
    for (int i = 0; i < sizeof...(data); ++i) {
      (*this)[i] = std::move(copy[i]);
    }
  }

  ~PoolArray() {
    PoolStorage<T>::Delete(array_);
  }

 private:
  PoolArray(const PoolArray&) = delete;
  PoolArray& operator =(const PoolArray&) = delete;

  PoolStorage<T>* array_;
};

#endif  // POOLED_HPP_
