#ifndef CATEGORY_BASE_H_
#define CATEGORY_BASE_H_

#include <functional>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>

#include "category.h"
#include "core.h"
#include "dispatch.h"
#include "ids.h"
#include "optional.h"

template<class I>
class InstanceCache {
 public:
  template<class...Ts>
  R<I>& Create(const Ts&... ts) {
    return cache_[TypeArgs{&ts...}];
  }

 private:
  std::map<TypeArgs,R<I>> cache_;
};

#endif  // CATEGORY_BASE_H_
