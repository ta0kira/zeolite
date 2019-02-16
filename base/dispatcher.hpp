#ifndef DISPATCHER_HPP_
#define DISPATCHER_HPP_

#include <functional>
#include <unordered_map>

#include "types.hpp"
#include "function.hpp"


template<class C>
class CategoryDispatcher {
 public:
  using CallType = ReturnTuple(C::*)(const ParamTuple&, const ValueTuple&);

  CategoryDispatcher() = default;
  CategoryDispatcher(CategoryDispatcher&& other) : map_(std::move(other.map_)) {}

  CategoryDispatcher& Register(const DFunction<SymbolScope::CATEGORY>& label,
                               CallType function) {
    map_[&label] = function;
    return *this;
  }

  ReturnTuple Dispatch(C& target,
                       const DFunction<SymbolScope::CATEGORY>& label,
                       const ParamTuple& params, const ValueTuple& args) const {
    const auto caller = map_.find(&label);
    FAIL_IF(caller == map_.end())
        << target.CategoryName() << " does not implement " << label.FunctionName();
    return (target.*caller->second)(params, args);
  }

 private:
  CategoryDispatcher(const CategoryDispatcher&) = delete;
  CategoryDispatcher& operator =(const CategoryDispatcher&) = delete;
  void* operator new(std::size_t size) = delete;

  std::unordered_map<const DFunction<SymbolScope::CATEGORY>*,CallType> map_;
};


template<class T>
class TypeDispatcher {
 public:
  using CallType = ReturnTuple(T::*)(const ParamTuple&, const ValueTuple&);

  TypeDispatcher() = default;
  TypeDispatcher(TypeDispatcher&& other) : map_(std::move(other.map_)) {}

  TypeDispatcher& Register(const DFunction<SymbolScope::TYPE>& label,
                           CallType function) {
    map_[&label] = function;
    return *this;
  }

  ReturnTuple Dispatch(T& target,
                       const DFunction<SymbolScope::TYPE>& label,
                       const ParamTuple& params, const ValueTuple& args) const {
    const auto caller = map_.find(&label);
    FAIL_IF(caller == map_.end())
        << target.CategoryName() << " does not implement " << label.FunctionName();
    return (target.*caller->second)(params, args);
  }

 private:
  TypeDispatcher(const TypeDispatcher&) = delete;
  TypeDispatcher& operator =(const TypeDispatcher&) = delete;
  void* operator new(std::size_t size) = delete;

  std::unordered_map<const DFunction<SymbolScope::TYPE>*,CallType> map_;
};


template<class V>
class ValueDispatcher {
 public:
  using CallType = ReturnTuple(V::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);

  ValueDispatcher() = default;
  ValueDispatcher(ValueDispatcher&& other) : map_(std::move(other.map_)) {}

  ValueDispatcher& Register(const DFunction<SymbolScope::VALUE>& label,
                            CallType function) {
    map_[&label] = function;
    return *this;
  }

  ReturnTuple Dispatch(V& target,
                       const S<TypeValue>& self,
                       const DFunction<SymbolScope::VALUE>& label,
                       const ParamTuple& params, const ValueTuple& args) const {
    const auto caller = map_.find(&label);
    FAIL_IF(caller == map_.end())
        << target.CategoryName() << " does not implement " << label.FunctionName();
    return (target.*caller->second)(self, params, args);
  }

 private:
  ValueDispatcher(const ValueDispatcher&) = delete;
  ValueDispatcher& operator =(const ValueDispatcher&) = delete;
  void* operator new(std::size_t size) = delete;

  std::unordered_map<const DFunction<SymbolScope::VALUE>*,CallType> map_;
};

#endif  // DISPATCHER_HPP_
