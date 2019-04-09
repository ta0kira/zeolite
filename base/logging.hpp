/* -----------------------------------------------------------------------------
Copyright 2019 Kevin P. Barry

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

#ifndef LOGGING_HPP_
#define LOGGING_HPP_

#include <list>
#include <sstream>
#include <string>

#include "thread-capture.h"


#define FAIL() LogThenCrash(true)

void SetSignalHandler();

class LogThenCrash {
 public:
  LogThenCrash(bool fail, const std::string& condition = "");
  LogThenCrash(bool fail, int signal);
  ~LogThenCrash();

  template<class T>
  LogThenCrash& operator << (const T& stuff) {
    if (fail_) {
      static_cast<std::ostream&>(output_) << stuff;
    }
    return *this;
  }

 private:
  LogThenCrash(const LogThenCrash&) = delete;
  LogThenCrash(LogThenCrash&&) = delete;
  LogThenCrash& operator =(const LogThenCrash&) = delete;
  LogThenCrash& operator =(LogThenCrash&&) = delete;
  void* operator new(std::size_t size) = delete;

  const bool fail_;
  const int signal_;
  const std::string condition_;
  std::ostringstream output_;
};


#ifndef DISABLE_TRACING

  #define TRACE_FUNCTION(name) \
    SourceContext source_context(name);

  #define SET_CONTEXT_POINT(point) \
    source_context.SetLocal(point);

  #define PRED_CONTEXT_POINT(point) \
    source_context.SetLocal(point),

#else

  #define TRACE_FUNCTION(name)

  #define SET_CONTEXT_POINT(point)

  #define PRED_CONTEXT_POINT(point)

#endif

class TraceContext : public capture_thread::ThreadCapture<TraceContext> {
 public:
  static std::list<std::string> GetTrace();

 protected:
  virtual void AppendTrace(std::list<std::string>& trace) const = 0;
  virtual const TraceContext* GetNext() const = 0;
  virtual ~TraceContext() = default;
};

class SourceContext : public TraceContext {
 public:
  template<int S>
  inline SourceContext(const char(&name)[S])
    : at_(nullptr), name_(name), cross_and_capture_to_(this) {}

  template<int S>
  inline void SetLocal(const char(&at)[S]) {
    at_ = at;
  }

 private:
  void AppendTrace(std::list<std::string>& trace) const final;
  const TraceContext* GetNext() const final;

  const char* at_;
  const char* const name_;
  const AutoThreadCrosser cross_and_capture_to_;
};

#endif  // LOGGING_HPP_
