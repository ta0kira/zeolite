/* -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

#ifndef INTERNAL_HPP_
#define INTERNAL_HPP_

#include <algorithm>
#include <atomic>

#include "category-source.hpp"


#define COLLECTION_ID(vp) (int) 1000000009 * (long) (vp)

template<class F>
struct DispatchTable {
  constexpr DispatchTable() : key(), table(nullptr), size(0) {}

  template<int S>
  DispatchTable(CollectionType k, const F(&t)[S]) : key(k), table(t), size(S) {}

  inline bool operator < (const DispatchTable<F>& other) const { return key < other.key; }

  CollectionType key;
  const F* table;
  int size;
};

template<class F>
struct DispatchSingle {
  constexpr DispatchSingle() : key(nullptr), value() {}

  DispatchSingle(const void* k, const F v) : key(k), value(v) {}

  inline bool operator < (const DispatchSingle<F>& other) const { return key < other.key; }

  const void* key;
  F value;
};

struct StaticSort {
  template<class T, int S>
  StaticSort(T(&table)[S]) {
    std::sort(table, table+S);
  }
};

template<class T>
struct LoopLimit {};

template<class F>
struct LoopLimit<DispatchTable<F>> {
  // See function-calls.0rt for tests.
  static constexpr int max = 15;
};

template<class F>
struct LoopLimit<DispatchSingle<F>> {
  // See function-calls.0rt for tests.
  static constexpr int max = 15;
};

template<bool L, class T, int S>
struct DispatchChoice {};

template<class T>
struct DispatchChoice<true, T, 0> {
  template<class K>
  static const T* Select(K key, T* table) {
    return nullptr;
  }
};
template<class T, int S>
struct DispatchChoice<true, T, S> {
  template<class K>
  static const T* Select(K key, T* table) {
    if (table->key == key) {
      return table;
    }
    return DispatchChoice<true, T, S-1>::Select(key, table+1);
  }
};

template<class T, int S>
struct DispatchChoice<false, T, S> {
  template<class K>
  static const T* Select(K key, T* table) {
    int i = 0, j = S, k;
    while ((k = (i+j)/2) > i) {
      if (table[k].key < key) {
        i = k;
      } else if (table[k].key > key) {
        j = k;
      } else {
        return &table[k];
      }
    }
    if (table[k].key == key) {
      return &table[k];
    } else {
      return nullptr;
    }
  }
};

template<class K, class T, int S>
const T* DispatchSelect(K key, T(&table)[S]) {
  return DispatchChoice<(S <= LoopLimit<T>::max), T, S>::Select(key, table);
}

#endif  // INTERNAL_HPP_
