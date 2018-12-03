#include <iostream>

#include "base/bool.h"
#include "base/core.h"
#include "base/optional.h"
#include "base/string.h"
#include "base/trace.h"

#include "queue.h"
#include "printable.h"
#include "reader.h"
#include "writer.h"
#include "value.h"

/*

00: Queue<Value> queue = Queue<Value>.create();
01:
02: queue.write(Value.create<String>("one"));
03: queue.write(Value.create<String>("two"));
04: queue.write(Value.create<String>("three"));
05: queue.write(Value.create<Bool>(false));
06:
07: Reader<Value> reader = queue;
08: while (true) {  // <- fake syntax for now
09:   optional Value value = reader.read();
10:   if (present(value)) {
11:     require(value).print();
12:   } else {
13:     break;  // <- fake syntax for now
14:   }
15:   Value.show(require(value));
16: }

*/

int main() {
  SetSignalHandler();
  SourceContext trace("Example.execute");

  trace.SetLocal("main:0");
  S<TypeValue> queue =
      SafeGet<0>(Category_Queue()
          .Build(Category_Value().Build())
          .CallInstanceFunction(Function_Queue_create,TypeArgs{},FunctionArgs{}));

  trace.SetLocal("main:2");
  queue->CallValueFunction(
      Function_Writer_write,
      TypeArgs{},
      FunctionArgs{
          SafeGet<0>(
              Category_Value()
                  .Build()
                  .CallInstanceFunction(
                        Function_Value_create,
                        TypeArgs{&Category_String().Build()},
                        FunctionArgs{As_String("one")}))});

  trace.SetLocal("main:3");
  queue->CallValueFunction(
      Function_Writer_write,
      TypeArgs{},
      FunctionArgs{
          SafeGet<0>(
              Category_Value()
                  .Build()
                  .CallInstanceFunction(
                        Function_Value_create,
                        TypeArgs{&Category_String().Build()},
                        FunctionArgs{As_String("two")}))});

  trace.SetLocal("main:4");
  queue->CallValueFunction(
      Function_Writer_write,
      TypeArgs{},
      FunctionArgs{
          SafeGet<0>(
              Category_Value()
                  .Build()
                  .CallInstanceFunction(
                        Function_Value_create,
                        TypeArgs{&Category_String().Build()},
                        FunctionArgs{As_String("three")}))});

  trace.SetLocal("main:5");
  queue->CallValueFunction(
      Function_Writer_write,
      TypeArgs{},
      FunctionArgs{
          SafeGet<0>(
              Category_Value()
                  .Build()
                  .CallInstanceFunction(
                        Function_Value_create,
                        TypeArgs{&Category_Bool().Build()},
                        FunctionArgs{As_Bool(false)}))});

  trace.SetLocal("main:7");
  ValueVariable reader(Category_Reader().Build(Category_Value().Build()),queue);

  while (true) {
    trace.SetLocal("main:9");
    ValueVariable value(
        Category_Value().Build(),
        SafeGet<0>(reader.GetValue()->CallValueFunction(Function_Reader_read,TypeArgs{},FunctionArgs{})));
    trace.SetLocal("main:10");
    if (value.GetValue()->IsPresent()) {
      trace.SetLocal("main:11");
      TypeValue::Require(value.GetValue())
          ->CallValueFunction(Function_Printable_print,TypeArgs{},FunctionArgs{});
    } else {
      trace.SetLocal("main:13");
      break;
    }
    trace.SetLocal("main:15");
    Category_Value().Build().CallInstanceFunction(
        Function_Value_show,TypeArgs{},FunctionArgs{TypeValue::Require(value.GetValue())});
  }
}
