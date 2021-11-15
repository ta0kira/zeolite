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


void ReturnTuple::TransposeFrom(ReturnTuple&& other) {
  if (Size() != other.Size()) {
    FAIL() << "ReturnTuple size mismatch in assignment: " << Size()
           << " (expected) " << other.Size() << " (actual)";
  }
  for (int i = 0; i < Size(); ++i) {
    At(i) = std::move(other.At(i));
  }
}

int ReturnTuple::Size() const {
  return data_.Size();
}

BoxedValue& ReturnTuple::At(int pos) {
  if (pos < 0 || pos >= data_.Size()) {
    FAIL() << "Bad return index";
  }
  return data_[pos];
}

const BoxedValue& ReturnTuple::At(int pos) const {
  if (pos < 0 || pos >= data_.Size()) {
    FAIL() << "Bad return index";
  }
  return data_[pos];
}


namespace zeolite_internal {

thread_local PoolCache<BoxedValue> PoolManager<BoxedValue>::cache4_(256);

// static
typename PoolManager<BoxedValue>::PoolEntry* PoolManager<BoxedValue>::Take(int orig_size) {
  int size = orig_size;
  if (size == 0) return nullptr;
  if (size < 4) {
    size = 4;
  }
  PoolEntry* storage = nullptr;
  if (size == 4 && (storage = cache4_.Take())) {
  } else {
    storage = new (new unsigned char[sizeof(PoolEntry) + size*sizeof(Managed)]) PoolEntry(size, nullptr);
  }
  new (storage->data()) Managed[orig_size];
  return storage;
}

// static
void PoolManager<BoxedValue>::Return(PoolEntry* storage, int orig_size) {
  if (!storage) return;
  for (int i = 0; i < orig_size; ++i) {
    storage->data()[i].~Managed();
  }
  if (storage->size == 4 && cache4_.Return(storage)) {
    return;
  }
  storage->~PoolEntry();
  delete[] (unsigned char*) storage;
}

}  // namespace zeolite_internal
