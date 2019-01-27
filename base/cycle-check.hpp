#ifndef CYCLE_CHECK_HPP_
#define CYCLE_CHECK_HPP_

#include "thread-capture.h"

#include "logging.hpp"


// Type must have a const function CategoryName().
template<class Type>
class CycleCheck : public capture_thread::ThreadCapture<CycleCheck<Type>> {
 public:
  CycleCheck(const Type& type) : type_(type), capture_to_(this) {}

  static void Check() {
    auto current = capture_thread::ThreadCapture<CycleCheck<Type>>::GetCurrent();
    if (current) {
      FAIL() << "Detected cycle in " << current->type_.CategoryName() << " init";
    }
  }

 private:
  const Type& type_;
  const typename capture_thread::ThreadCapture<CycleCheck<Type>>::ScopedCapture capture_to_;
};

#endif  // CYCLE_CHECK_HPP_
