// TODO: Update the compiler to create main.

#include "category-source.hpp"

#include "Category_Runner.hpp"
#include "Category_Demo.hpp"

int main() {
  SetSignalHandler();
  TRACE_FUNCTION("main")
  GetType_Demo(T_get()).Call(Function_Runner_run, ParamTuple(), ArgTuple());
}
