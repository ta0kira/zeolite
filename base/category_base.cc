#include "category_base.h"

// CategoryToArgs::CategoryToArgs(const std::string& name) :
//     name_(name) {
//   AddArgs(Id_Optional,this);
// }
//
// const TypeArgs& CategoryToArgs::GetArgs(const CategoryId* id) const {
//   const auto args = all_args_.find(&id);
//   FAIL_IF(args == all_args_.end())
//       << "Category " << id->TypeName() << " not supported by " << name_;
//   return args->second;
// }
