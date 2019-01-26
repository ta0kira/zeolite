#ifndef IDS_H_
#define IDS_H_

#include <string>

#include "core.h"

enum class MemberScope {
  CATEGORY,
  INSTANCE,
  VALUE,
};

// TODO: Update this to specify type/arg/return counts and add a wrapper around
// the function callers to enforce those counts. (Should also enforce them when
// functions are registered with FunctionDispatcher.)
template<MemberScope>
class FunctionId {
 public:
  inline FunctionId(const std::string& name) : name_(name) {}
  ALWAYS_PERMANENT(FunctionId)
  inline std::string FunctionName() const { return name_; }

 private:
  ~FunctionId() = default;
  const std::string name_;
};

template<MemberScope>
class ValueVariableId {
 public:
  inline ValueVariableId(const std::string& name) : name_(name) {}
  ALWAYS_PERMANENT(ValueVariableId)
  inline std::string VariableName() const { return name_; }

 private:
  ~ValueVariableId() = default;
  const std::string name_;
};

template<MemberScope>
class TypeVariableId {
 public:
  inline TypeVariableId(const std::string& name) : name_(name) {}
  ALWAYS_PERMANENT(TypeVariableId)
  inline std::string VariableName() const { return name_; }

 private:
  ~TypeVariableId() = default;
  const std::string name_;
};

#endif  // IDS_H_
