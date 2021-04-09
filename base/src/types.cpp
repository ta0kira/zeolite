/* -----------------------------------------------------------------------------
Copyright 2019,2021 Kevin P. Barry

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

#include <atomic>

#include "boxed.hpp"
#include "logging.hpp"


int ArgTuple::Size() const {
  return size_;
}

const BoxedValue& ArgTuple::At(int pos) const {
  if (pos < 0 || pos >= size_) {
    FAIL() << "Bad ArgTuple index";
  }
  return *data_[pos];
}

const BoxedValue& ArgTuple::Only() const {
  if (size_ != 1) {
    FAIL() << "Bad ArgTuple index";
  }
  return *data_[0];
}

ReturnTuple& ReturnTuple::operator = (ReturnTuple&& other) {
  if (Size() != other.Size()) {
    FAIL() << "ReturnTuple size mismatch in assignment: " << Size()
           << " (expected) " << other.Size() << " (actual)";
  }
  for (int i = 0; i < Size(); ++i) {
    At(i) = std::move(other.At(i));
  }
  return *this;
}

int ReturnTuple::Size() const {
  return size_;
}

BoxedValue& ReturnTuple::At(int pos) {
  if (pos < 0 || pos >= size_) {
    FAIL() << "Bad ReturnTuple index";
  }
  return data_[pos];
}

const BoxedValue& ReturnTuple::At(int pos) const {
  if (pos < 0 || pos >= size_) {
    FAIL() << "Bad ReturnTuple index";
  }
  return data_[pos];
}

const BoxedValue& ReturnTuple::Only() const {
  if (size_ != 1) {
    FAIL() << "Bad ReturnTuple index";
  }
  return data_[0];
}

int ParamTuple::Size() const {
  return size_;
}

const S<TypeInstance>& ParamTuple::At(int pos) const {
  if (pos < 0 || pos >= size_) {
    FAIL() << "Bad ParamTuple index";
  }
  return data_[pos];
}


namespace {

template<class P>
static inline P* PoolTakeCommon(std::atomic_flag& flag,
                                P*& pool, unsigned int& size) {
  while (flag.test_and_set(std::memory_order_acquire));
  P* const storage = pool;
  if (storage == nullptr) {
    flag.clear(std::memory_order_release);
    return nullptr;
  } else {
    --size;
    pool = storage->next;
    flag.clear(std::memory_order_release);
    storage->next = nullptr;
    new (storage->data()) typename P::Managed[storage->size];
    return storage;
  }
}

template<class P>
static inline bool PoolReturnCommon(P* storage, std::atomic_flag& flag,
                                    P*& pool, unsigned int& size,
                                    unsigned int limit) {
  while (flag.test_and_set(std::memory_order_acquire));
  P* const head = pool;
  if (size < limit) {
    ++size;
    storage->next = head;
    pool = storage;
    flag.clear(std::memory_order_release);
    return true;
  } else {
    flag.clear(std::memory_order_release);
    return false;
  }
}

}  // namespace


namespace zeolite_internal {

unsigned int PoolManager<BoxedValue>::pool4_size_ = 0;
typename PoolManager<BoxedValue>::PoolEntry* PoolManager<BoxedValue>::pool4_{nullptr};
std::atomic_flag PoolManager<BoxedValue>::pool4_flag_ = ATOMIC_FLAG_INIT;

// static
typename PoolManager<BoxedValue>::PoolEntry* PoolManager<BoxedValue>::Take(int size) {
  if (size == 0) return nullptr;
  if (size < 4) {
    size = 4;
  }
  PoolEntry* storage = nullptr;
  if (size == 4 && (storage = PoolTakeCommon(pool4_flag_, pool4_, pool4_size_))) {
    return storage;
  }
  storage = new (new unsigned char[sizeof(PoolEntry) + size*sizeof(Managed)]) PoolEntry(size, nullptr);
  new (storage->data()) Managed[size];
  return storage;
}

// static
void PoolManager<BoxedValue>::Return(PoolEntry* storage) {
  if (!storage) return;
  for (int i = 0; i < storage->size; ++i) {
    storage->data()[i].~Managed();
  }
  if (storage->size == 4 && PoolReturnCommon(storage, pool4_flag_, pool4_, pool4_size_, pool_limit_)) {
    return;
  }
  storage->~PoolEntry();
  delete[] (unsigned char*) storage;
}


unsigned int PoolManager<const BoxedValue*>::pool4_size_ = 0;
typename PoolManager<const BoxedValue*>::PoolEntry* PoolManager<const BoxedValue*>::pool4_{nullptr};
std::atomic_flag PoolManager<const BoxedValue*>::pool4_flag_ = ATOMIC_FLAG_INIT;

// static
typename PoolManager<const BoxedValue*>::PoolEntry* PoolManager<const BoxedValue*>::Take(int size) {
  if (size == 0) return nullptr;
  if (size < 4) {
    size = 4;
  }
  PoolEntry* storage = nullptr;
  if (size == 4 && (storage = PoolTakeCommon(pool4_flag_, pool4_, pool4_size_))) {
    return storage;
  }
  storage = new (new unsigned char[sizeof(PoolEntry) + size*sizeof(Managed)]) PoolEntry(size, nullptr);
  new (storage->data()) Managed[size];
  return storage;
}

// static
void PoolManager<const BoxedValue*>::Return(PoolEntry* storage) {
  if (!storage) return;
  for (int i = 0; i < storage->size; ++i) {
    storage->data()[i].~Managed();
  }
  if (storage->size == 4 && PoolReturnCommon(storage, pool4_flag_, pool4_, pool4_size_, pool_limit_)) {
    return;
  }
  storage->~PoolEntry();
  delete[] (unsigned char*) storage;
}


unsigned int PoolManager<S<TypeInstance>>::pool4_size_ = 0;
typename PoolManager<S<TypeInstance>>::PoolEntry* PoolManager<S<TypeInstance>>::pool4_{nullptr};
std::atomic_flag PoolManager<S<TypeInstance>>::pool4_flag_ = ATOMIC_FLAG_INIT;

// static
typename PoolManager<S<TypeInstance>>::PoolEntry* PoolManager<S<TypeInstance>>::Take(int size) {
  if (size == 0) return nullptr;
  if (size < 4) {
    size = 4;
  }
  PoolEntry* storage = nullptr;
  if (size == 4 && (storage = PoolTakeCommon(pool4_flag_, pool4_, pool4_size_))) {
    return storage;
  }
  storage = new (new unsigned char[sizeof(PoolEntry) + size*sizeof(Managed)]) PoolEntry(size, nullptr);
  new (storage->data()) Managed[size];
  return storage;
}

// static
void PoolManager<S<TypeInstance>>::Return(PoolEntry* storage) {
  if (!storage) return;
  for (int i = 0; i < storage->size; ++i) {
    storage->data()[i].~Managed();
  }
  if (storage->size == 4 && PoolReturnCommon(storage, pool4_flag_, pool4_, pool4_size_, pool_limit_)) {
    return;
  }
  storage->~PoolEntry();
  delete[] (unsigned char*) storage;
}

}  // namespace zeolite_internal
