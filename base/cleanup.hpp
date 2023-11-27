/* -----------------------------------------------------------------------------
Copyright 2023 Kevin P. Barry

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

#ifndef CLEANUP_HPP_
#define CLEANUP_HPP_

#include "thread-capture.h"
#include "category-header.hpp"


class GlobalCleanup : public capture_thread::ThreadCapture<GlobalCleanup> {
 public:
  // Executes cleanup routines in reverse order.
  static void Finish(int code);

 protected:
  // NOTE: Each destructor should call Cleanup().
  virtual ~GlobalCleanup() = default;
  virtual GlobalCleanup* GetNext() const = 0;
  virtual void Cleanup(int code) = 0;
};

// Executes type functions upon construction and destruction.
class WrapTypeCall : public GlobalCleanup {
 public:
  // Both start and finish take no args. finish is only called once, whether
  // it's via Finish() or the destructor.
  WrapTypeCall(S<const TypeInstance> type, const TypeFunction* start, const TypeFunction* finish);
  ~WrapTypeCall();

 protected:
  GlobalCleanup* GetNext() const override;
  void Cleanup(int code) override;

 private:
  const S<const TypeInstance> type_;
  const TypeFunction* finish_;
  const AutoThreadCrosser cross_and_capture_to_;
};

#endif  // CLEANUP_HPP_
