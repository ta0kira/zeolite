#ifndef IDS_H_
#define IDS_H_

#include <string>

enum class MemberScope {
  CATEGORY,
  INSTANCE,
  VALUE,
};

template<MemberScope>
class FunctionId {
 public:
  inline FunctionId(const std::string& name) : name_(name) {}
  inline std::string FunctionName() const { return name_; }

 private:
  const std::string name_;
};

// NOTE: Even though variables can exist at the category/instance level,
// categories and instances are singletons and therefore won't access variables
// from another object of the same type.
template<MemberScope>
class ValueVariableId {
 public:
  inline ValueVariableId(const std::string& name) : name_(name) {}
  inline std::string VariableName() const { return name_; }

 private:
  const std::string name_;
};

#endif  // IDS_H_
