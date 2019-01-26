#include "builtin.hpp"

#include "category-source.hpp"


namespace {

class OptionalEmpty : public TypeValue {
 protected:
  DReturns Dispatch(S<TypeValue> self,
                    const DFunction<SymbolScope::ValueScope>& label,
                    DParams params, DArgs args) final {
    FAIL() << "Function called on an empty value";
    return DReturns();
  }

  bool Present() const final { return false; }
};

}  // namespace


const S<TypeValue>& Var_empty = *new OptionalEmpty();
// TODO
// const S<TypeValue>& Var_true;
// const S<TypeValue>& Var_false;
