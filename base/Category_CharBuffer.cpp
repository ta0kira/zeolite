/* -----------------------------------------------------------------------------
Copyright 2021 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- */

// Author: Kevin P. Barry [ta0kira@gmail.com]

#include "Category_CharBuffer.hpp"

#include <map>
#include <sstream>

#include "category-source.hpp"
#include "Category_Char.hpp"
#include "Category_Container.hpp"
#include "Category_ReadAt.hpp"
#include "Category_WriteAt.hpp"


#ifdef ZEOLITE_PUBLIC_NAMESPACE
namespace ZEOLITE_PUBLIC_NAMESPACE {
#endif  // ZEOLITE_PUBLIC_NAMESPACE
namespace {
const int collection_CharBuffer = 0;
}  // namespace
const void* const Functions_CharBuffer = &collection_CharBuffer;
const ValueFunction& Function_CharBuffer_resize = (*new ValueFunction{ 0, 1, 1, "CharBuffer", "resize", Functions_CharBuffer, 0 });
const TypeFunction& Function_CharBuffer_new = (*new TypeFunction{ 0, 1, 1, "CharBuffer", "new", Functions_CharBuffer, 0 });
namespace {
class Category_CharBuffer;
class Type_CharBuffer;
class Value_CharBuffer;
S<TypeValue> Box_CharBuffer(const PrimCharBuffer& value);
struct Category_CharBuffer : public TypeCategory {
  std::string CategoryName() const final { return "CharBuffer"; }
  Category_CharBuffer() {
    CycleCheck<Category_CharBuffer>::Check();
    CycleCheck<Category_CharBuffer> marker(*this);
    TRACE_FUNCTION("CharBuffer (init @category)")
  }
  ReturnTuple Dispatch(const CategoryFunction& label, const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Category_CharBuffer::*)(const ParamTuple&, const ValueTuple&);
    return TypeCategory::Dispatch(label, params, args);
  }
};
Category_CharBuffer& CreateCategory_CharBuffer() {
  static auto& category = *new Category_CharBuffer();
  return category;
}
struct Type_CharBuffer : public TypeInstance {
  std::string CategoryName() const final { return parent.CategoryName(); }
  void BuildTypeName(std::ostream& output) const final {
    return TypeInstance::TypeNameFrom(output, parent);
  }
  Category_CharBuffer& parent;
  bool CanConvertFrom(const S<const TypeInstance>& from) const final {
    std::vector<S<const TypeInstance>> args;
    if (!from->TypeArgsForParent(parent, args)) return false;
    if(args.size() != 0) {
      FAIL() << "Wrong number of args (" << args.size() << ")  for " << CategoryName();
    }
    return true;
  }
  void Params_CharBuffer(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_Container(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{};
  }
  void Params_ReadAt(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{GetType_Char(T_get())};
  }
  void Params_WriteAt(std::vector<S<const TypeInstance>>& args) const {
    args = std::vector<S<const TypeInstance>>{GetType_Char(T_get())};
  }
  bool TypeArgsForParent(const TypeCategory& category, std::vector<S<const TypeInstance>>& args) const final {
    using CallType = void(Type_CharBuffer::*)(std::vector<S<const TypeInstance>>&)const;
    static DispatchSingle<CallType> all_calls[] = {
      DispatchSingle<CallType>(&GetCategory_CharBuffer(), &Type_CharBuffer::Params_CharBuffer),
      DispatchSingle<CallType>(&GetCategory_Container(),  &Type_CharBuffer::Params_Container),
      DispatchSingle<CallType>(&GetCategory_ReadAt(),     &Type_CharBuffer::Params_ReadAt),
      DispatchSingle<CallType>(&GetCategory_WriteAt(),    &Type_CharBuffer::Params_WriteAt),
      DispatchSingle<CallType>(),
    };
    std::atomic_bool table_lock{0};
    const DispatchSingle<CallType>* const call = DispatchSelect(table_lock, &category, all_calls);
    if (call) {
      (this->*call->value)(args);
      return true;
    }
    return false;
  }
  Type_CharBuffer(Category_CharBuffer& p, Params<0>::Type params) : parent(p) {
    CycleCheck<Type_CharBuffer>::Check();
    CycleCheck<Type_CharBuffer> marker(*this);
    TRACE_FUNCTION("CharBuffer (init @type)")
  }
  ReturnTuple Dispatch(const S<TypeInstance>& self, const TypeFunction& label,
                       const ParamTuple& params, const ValueTuple& args) final {
    using CallType = ReturnTuple(Type_CharBuffer::*)(const ParamTuple&, const ValueTuple&);
    static const CallType Table_CharBuffer[] = {
      &Type_CharBuffer::Call_new,
    };
    static DispatchTable<CallType> all_tables[] = {
      DispatchTable<CallType>(Functions_CharBuffer, Table_CharBuffer),
      DispatchTable<CallType>(),
    };
    std::atomic_bool table_lock{0};
    const DispatchTable<CallType>* const table = DispatchSelect(table_lock, label.collection, all_tables);
    if (table) {
      if (label.function_num < 0 || label.function_num >= table->size) {
        FAIL() << "Bad function call " << label;
      } else {
        return (this->*table->table[label.function_num])(params, args);
      }
    }
    return TypeInstance::Dispatch(self, label, params, args);
  }
  ReturnTuple Call_new(const ParamTuple& params, const ValueTuple& args);
};
S<Type_CharBuffer> CreateType_CharBuffer(Params<0>::Type params) {
  static const auto cached = S_get(new Type_CharBuffer(CreateCategory_CharBuffer(), Params<0>::Type()));
  return cached;
}
struct Value_CharBuffer : public TypeValue {
  Value_CharBuffer(S<Type_CharBuffer> p, const PrimCharBuffer& value) : parent(p), value_(value) {}
  ReturnTuple Dispatch(const S<TypeValue>& self, const ValueFunction& label, const ParamTuple& params,const ValueTuple& args) final {
    using CallType = ReturnTuple(Value_CharBuffer::*)(const S<TypeValue>&, const ParamTuple&, const ValueTuple&);
    static const CallType Table_CharBuffer[] = {
      &Value_CharBuffer::Call_resize,
    };
    static const CallType Table_Container[] = {
      &Value_CharBuffer::Call_size,
    };
    static const CallType Table_ReadAt[] = {
      &Value_CharBuffer::Call_readAt,
    };
    static const CallType Table_WriteAt[] = {
      &Value_CharBuffer::Call_writeAt,
    };
    static DispatchTable<CallType> all_tables[] = {
      DispatchTable<CallType>(Functions_CharBuffer, Table_CharBuffer),
      DispatchTable<CallType>(Functions_Container,  Table_Container),
      DispatchTable<CallType>(Functions_ReadAt,     Table_ReadAt),
      DispatchTable<CallType>(Functions_WriteAt,    Table_WriteAt),
      DispatchTable<CallType>(),
    };
    std::atomic_bool table_lock{0};
    const DispatchTable<CallType>* const table = DispatchSelect(table_lock, label.collection, all_tables);
    if (table) {
      if (label.function_num < 0 || label.function_num >= table->size) {
        FAIL() << "Bad function call " << label;
      } else {
        return (this->*table->table[label.function_num])(self, params, args);
      }
    }
    return TypeValue::Dispatch(self, label, params, args);
  }
  std::string CategoryName() const final { return parent->CategoryName(); }
  PrimCharBuffer& AsCharBuffer() final { return value_; }
  ReturnTuple Call_resize(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_readAt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_size(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  ReturnTuple Call_writeAt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args);
  const S<Type_CharBuffer> parent;
  PrimCharBuffer value_;
};
ReturnTuple Type_CharBuffer::Call_new(const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("CharBuffer.new")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  if (Var_arg1 < 0) {
    FAIL() << "Buffer size " << Var_arg1 << " is invalid";
  }
  return ReturnTuple(Box_CharBuffer(PrimCharBuffer(Var_arg1,'\0')));
}
ReturnTuple Value_CharBuffer::Call_resize(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("CharBuffer.resize")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  if (Var_arg1 < 0) {
    FAIL() << "Buffer size " << Var_arg1 << " is invalid";
  } else {
    value_.resize(Var_arg1);
  }
  return ReturnTuple(Var_self);
}
ReturnTuple Value_CharBuffer::Call_readAt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("CharBuffer.readAt")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  if (Var_arg1 < 0 || Var_arg1 >= AsCharBuffer().size()) {
    FAIL() << "Read position " << Var_arg1 << " is out of bounds";
  }
  return ReturnTuple(Box_Char(AsCharBuffer()[Var_arg1]));
}
ReturnTuple Value_CharBuffer::Call_size(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("CharBuffer.size")
  return ReturnTuple(Box_Int(value_.size()));
}
ReturnTuple Value_CharBuffer::Call_writeAt(const S<TypeValue>& Var_self, const ParamTuple& params, const ValueTuple& args) {
  TRACE_FUNCTION("CharBuffer.writeAt")
  const PrimInt Var_arg1 = (args.At(0))->AsInt();
  const PrimChar Var_arg2 = (args.At(1))->AsChar();
  if (Var_arg1 < 0 || Var_arg1 >= value_.size()) {
    FAIL() << "Write position " << Var_arg1 << " is out of bounds";
  } else {
    value_[Var_arg1] = Var_arg2;
  }
  return ReturnTuple(Var_self);
}
S<TypeValue> Box_CharBuffer(const PrimCharBuffer& value) {
  return S_get(new Value_CharBuffer(CreateType_CharBuffer(Params<0>::Type()), value));
}
}  // namespace
TypeCategory& GetCategory_CharBuffer() {
  return CreateCategory_CharBuffer();
}
S<TypeInstance> GetType_CharBuffer(Params<0>::Type params) {
  return CreateType_CharBuffer(params);
}
#ifdef ZEOLITE_PUBLIC_NAMESPACE
}  // namespace ZEOLITE_PUBLIC_NAMESPACE
using namespace ZEOLITE_PUBLIC_NAMESPACE;
#endif  // ZEOLITE_PUBLIC_NAMESPACE
