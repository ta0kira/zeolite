#include "category-source.hpp"

#include "Category_Runner.hpp"
#include "Category_Test.hpp"

int main() {
  SetSignalHandler();
  TRACE_FUNCTION("main")
  // GetType_Test defines Function_Runner_run in test case.
  GetType_Test(T_get()).Call(Function_Runner_run, ParamTuple(), ArgTuple());
}
