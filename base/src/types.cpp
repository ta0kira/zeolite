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

#include "logging.hpp"


int ArgTuple::Size() const {
  return size_;
}

const S<TypeValue>& ArgTuple::At(int pos) const {
  if (pos < 0 || pos >= size_) {
    FAIL() << "Bad ArgTuple index";
  }
  return *data_[pos];
}

const S<TypeValue>& ArgTuple::Only() const {
  if (size_ != 1) {
    FAIL() << "Bad ArgTuple index";
  }
  return *data_[0];
}

int ReturnTuple::Size() const {
  return size_;
}

S<TypeValue>& ReturnTuple::At(int pos) {
  if (pos < 0 || pos >= size_) {
    FAIL() << "Bad ReturnTuple index";
  }
  return data_[pos];
}

const S<TypeValue>& ReturnTuple::At(int pos) const {
  if (pos < 0 || pos >= size_) {
    FAIL() << "Bad ReturnTuple index";
  }
  return data_[pos];
}

const S<TypeValue>& ReturnTuple::Only() const {
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


template<class T>
class PoolCleanup {
 public:
  constexpr PoolCleanup() {}

  ~PoolCleanup() {
    PoolManager<T>::Purge();
  }

 private:
  PoolCleanup(const PoolCleanup&) = delete;
  PoolCleanup operator = (const PoolCleanup&) = delete;
  PoolCleanup(PoolCleanup&&) = delete;
  PoolCleanup operator = (PoolCleanup&&) = delete;
};


static void* const pool_busy_flag_ = (void*) ~0x00;


static unsigned int return_tuple_pool4_size_ = 0;
static std::atomic<PoolStorage<S<TypeValue>>*> return_tuple_pool4_{nullptr};

// static
PoolStorage<S<TypeValue>>* PoolManager<S<TypeValue>>::Take(int size) {
  if (size == 0) return nullptr;
  if (size < 4) {
    size = 4;
  }
  if (size == 4) {
    PoolEntry* storage = nullptr;
    while ((storage = return_tuple_pool4_.exchange((PoolEntry*) pool_busy_flag_)) == pool_busy_flag_);
    if (storage == nullptr) {
      return_tuple_pool4_.exchange(nullptr);
    } else {
      --return_tuple_pool4_size_;
      return_tuple_pool4_.exchange(storage->next);
      storage->next = nullptr;
      new (storage->data()) Managed[storage->size];
      return storage;
    }
  }
  PoolEntry* const storage = new (new unsigned char[sizeof(PoolEntry) + size*sizeof(Managed)]) PoolEntry(size, nullptr);
  new (storage->data()) Managed[size];
  return storage;
}

// static
void PoolManager<S<TypeValue>>::Return(PoolEntry* storage) {
  if (!storage) return;
  for (int i = 0; i < storage->size; ++i) {
    storage->data()[i].~Managed();
  }
  if (storage->size == 4) {
    PoolEntry* head = nullptr;
    while ((head = return_tuple_pool4_.exchange((PoolEntry*) pool_busy_flag_)) == pool_busy_flag_);
    if (return_tuple_pool4_size_ < pool_limit_) {
      ++return_tuple_pool4_size_;
      storage->next = head;
      return_tuple_pool4_.exchange(storage);
      return;
    } else {
      return_tuple_pool4_.exchange(head);
    }
  }
  storage->~PoolEntry();
  delete[] (unsigned char*) storage;
}


static unsigned int arg_tuple_pool4_size_ = 0;
static std::atomic<PoolStorage<const S<TypeValue>*>*> arg_tuple_pool4_{nullptr};

// static
PoolStorage<const S<TypeValue>*>* PoolManager<const S<TypeValue>*>::Take(int size) {
  if (size == 0) return nullptr;
  if (size < 4) {
    size = 4;
  }
  if (size == 4) {
    PoolEntry* storage = nullptr;
    while ((storage = arg_tuple_pool4_.exchange((PoolEntry*) pool_busy_flag_)) == pool_busy_flag_);
    if (storage == nullptr) {
      arg_tuple_pool4_.exchange(nullptr);
    } else {
      --arg_tuple_pool4_size_;
      arg_tuple_pool4_.exchange(storage->next);
      storage->next = nullptr;
      new (storage->data()) Managed[storage->size];
      return storage;
    }
  }
  PoolEntry* const storage = new (new unsigned char[sizeof(PoolEntry) + size*sizeof(Managed)]) PoolEntry(size, nullptr);
  new (storage->data()) Managed[size];
  return storage;
}

// static
void PoolManager<const S<TypeValue>*>::Return(PoolEntry* storage) {
  if (!storage) return;
  for (int i = 0; i < storage->size; ++i) {
    storage->data()[i].~Managed();
  }
  if (storage->size == 4) {
    PoolEntry* head = nullptr;
    while ((head = arg_tuple_pool4_.exchange((PoolEntry*) pool_busy_flag_)) == pool_busy_flag_);
    if (arg_tuple_pool4_size_ < pool_limit_) {
      ++arg_tuple_pool4_size_;
      storage->next = head;
      arg_tuple_pool4_.exchange(storage);
      return;
    } else {
      arg_tuple_pool4_.exchange(head);
    }
  }
  storage->~PoolEntry();
  delete[] (unsigned char*) storage;
}


static unsigned int param_tuple_pool4_size_ = 0;
static std::atomic<PoolStorage<S<TypeInstance>>*> param_tuple_pool4_{nullptr};

// static
PoolStorage<S<TypeInstance>>* PoolManager<S<TypeInstance>>::Take(int size) {
  if (size == 0) return nullptr;
  if (size < 4) {
    size = 4;
  }
  if (size == 4) {
    PoolEntry* storage = nullptr;
    while ((storage = param_tuple_pool4_.exchange((PoolEntry*) pool_busy_flag_)) == pool_busy_flag_);
    if (storage == nullptr) {
      param_tuple_pool4_.exchange(nullptr);
    } else {
      --param_tuple_pool4_size_;
      param_tuple_pool4_.exchange(storage->next);
      storage->next = nullptr;
      new (storage->data()) Managed[storage->size];
      return storage;
    }
  }
  PoolEntry* const storage = new (new unsigned char[sizeof(PoolEntry) + size*sizeof(Managed)]) PoolEntry(size, nullptr);
  new (storage->data()) Managed[size];
  return storage;
}

// static
void PoolManager<S<TypeInstance>>::Return(PoolEntry* storage) {
  if (!storage) return;
  for (int i = 0; i < storage->size; ++i) {
    storage->data()[i].~Managed();
  }
  if (storage->size == 4) {
    PoolEntry* head = nullptr;
    while ((head = param_tuple_pool4_.exchange((PoolEntry*) pool_busy_flag_)) == pool_busy_flag_);
    if (param_tuple_pool4_size_ < pool_limit_) {
      ++param_tuple_pool4_size_;
      storage->next = head;
      param_tuple_pool4_.exchange(storage);
      return;
    } else {
      param_tuple_pool4_.exchange(head);
    }
  }
  storage->~PoolEntry();
  delete[] (unsigned char*) storage;
}
