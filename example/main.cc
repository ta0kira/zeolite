#include <iostream>

#include "base/core.h"
#include "base/optional.h"
#include "base/trace.h"

#include "queue.h"
#include "printable.h"
#include "reader.h"
#include "writer.h"
#include "value.h"

/*

00: Queue<Value> queue = Queue<Value>.create();
01:
02: for (i = 0:5) {  // <- fake syntax for now
03:   Value value = Value.create();
04:   queue.write(value);
05: }
06:
07: Reader<Printable> reader = queue;
08: while (true) {  // <- fake syntax for now
09:   Value value = reader.read();
10:   if (present(value)) {
11:     require(value).print();
12:   } else {
13:     break;  // <- fake syntax for now
14:   }
15: }

*/

int main() {
  SourceContext trace("Example.execute");

  trace.SetLocal("main:0");
  S<TypeValue> queue =
      SafeGet<0>(Category_Queue()
          .Build(Category_Value().Build())
          .CallInstanceFunction(Function_Queue_create,TypeArgs{},FunctionArgs{}));

  for (int i = 0; i < 5; ++i) {
    trace.SetLocal("main:3");
    S<TypeValue> value =
        SafeGet<0>(Category_Value()
            .Build()
            .CallInstanceFunction(Function_Value_create,TypeArgs{},FunctionArgs{}));
    trace.SetLocal("main:4");
    queue->CallValueFunction(Function_Writer_write,TypeArgs{},FunctionArgs{value});
  }

  trace.SetLocal("main:7");
  S<TypeValue> reader =
      TypeValue::ConvertTo(queue,Category_Reader().Build(Category_Printable().Build()));

  while (true) {
    trace.SetLocal("main:9");
    S<TypeValue> value =
        SafeGet<0>(reader->CallValueFunction(Function_Reader_read,TypeArgs{},FunctionArgs{}));
    trace.SetLocal("main:10");
    if (SafeGet<0>(value->CallValueFunction(Function_Optional_present,TypeArgs{},FunctionArgs{}))->GetBool()) {
      trace.SetLocal("main:11");
      SafeGet<0>(value
          ->CallValueFunction(Function_Optional_require,TypeArgs{},FunctionArgs{}))
          ->CallValueFunction(Function_Printable_print,TypeArgs{},FunctionArgs{});
    } else {
      trace.SetLocal("main:13");
      break;
    }
  }
}
