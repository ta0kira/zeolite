// TODO: Update the compiler to create main.

#include "category-source.hpp"

#include "Category_Runner.hpp"
#include "Category_Main.hpp"

int main() {
  SetSignalHandler();
  TRACE_FUNCTION("main")
  GetType_Main(T_get()).Call(Function_Runner_run, T_get(), T_get());
}
