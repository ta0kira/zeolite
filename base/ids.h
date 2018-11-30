#ifndef IDS_H_
#define IDS_H_

#include <string>

enum class MemberScope {
  CATEGORY,
  INSTANCE,
  VALUE,
};

class CategoryId {
 public:
  inline CategoryId(const std::string& name) : name_(name) {}
  inline std::string TypeName() const { return name_; }

 private:
  const std::string name_;
};

template<MemberScope>
class FunctionId {
 public:
  inline FunctionId(const std::string& name) : name_(name) {}
  inline std::string FunctionName() const { return name_; }

 private:
  const std::string name_;
};

// NOTE: Even though variables can exist at the category/instance level, there
// should be no non-local access.
template<MemberScope>
class ValueVariableId {
 public:
  inline ValueVariableId(const std::string& name) : name_(name) {}
  inline std::string VariableName() const { return name_; }

 private:
  const std::string name_;
};

// NOTE: Even though variables can exist at the category/instance level, there
// should be no non-local access.
template<MemberScope>
class TypeVariableId {
 public:
  inline TypeVariableId(const std::string& name) : name_(name) {}
  inline std::string VariableName() const { return name_; }

 private:
  const std::string name_;
};

#endif  // IDS_H_
