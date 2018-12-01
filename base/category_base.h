#ifndef CATEGORY_BASE_H_
#define CATEGORY_BASE_H_

#include <map>
#include <vector>

#include "category.h"
#include "core.h"
#include "dispatch.h"
#include "ids.h"

template<class T>
class InstanceCache {
 public:
  template<class...Ts>
  S<T>& Create(Ts... ts) {
    return cache_[TypeArgs{ts.get()...}];
  }

 private:
  std::map<TypeArgs,S<T>> cache_;
};

#endif  // CATEGORY_BASE_H_
