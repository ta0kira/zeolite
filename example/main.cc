#include "base/core.h"
#include "base/optional.h"

#include "queue.h"
#include "reader.h"
#include "writer.h"
#include "value.h"

/*

Queue<Value> queue = Queue<Value>.create();

for (i = 0:5) {
  Value value = Value.create();
  queue.write(value);
}

Reader<Value> reader = queue;
while (true) {
  Value value = reader.read();
  if (present(value)) {
    value.print();
  } else {
    break;
  }
}

*/

int main() {
  S<TypeValue> queue =
      SafeGet<0>(Category_Queue
          .Build(Category_Value.Build())
          .CallInstanceFunction(Function_Queue_create,FunctionArgs{}));

  for (int i = 0; i < 5; ++i) {
    S<TypeValue> value =
        SafeGet<0>(Category_Value
            .Build()
            .CallInstanceFunction(Function_Value_create,FunctionArgs{}));
    queue->CallValueFunction(Function_Writer_write,FunctionArgs{value});
  }

  S<TypeValue> reader =
      TypeValue::ConvertTo(queue,Category_Reader.Build(Category_Value.Build()));

  while (true) {
    S<TypeValue> value =
        SafeGet<0>(reader->CallValueFunction(Function_Reader_read,FunctionArgs{}));
    if (SafeGet<0>(value->CallValueFunction(Function_Optional_present,FunctionArgs{}))->GetBool()) {
      SafeGet<0>(value
          ->CallValueFunction(Function_Optional_require,FunctionArgs{}))
          ->CallValueFunction(Function_Value_print,FunctionArgs{});
    } else {
      break;
    }
  }
}
