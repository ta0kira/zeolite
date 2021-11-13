/* -----------------------------------------------------------------------------
Copyright 2019-2021 Kevin P. Barry

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

#include <functional>
#include <fstream>
#include <list>
#include <memory>
#include <mutex>
#include <sstream>
#include <string>
#include <vector>

#include "thread-capture.h"


#define FAIL() LogThenCrash(true)

void SetSignalHandler();

using TraceList = std::list<std::function<std::string()>>;

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

  static void PrintTrace(const TraceList&);

  const bool fail_;
  const int signal_;
  const std::string condition_;
  std::ostringstream output_;
};


#ifndef DISABLE_TRACING

  #define TRACE_FUNCTION(name) \
    SourceContext source_context(name);

  #define TRACE_CLEANUP \
    CleanupContext cleanup_context;

  #define SET_CONTEXT_POINT(point) \
    TraceContext::SetContext(point);

  #define PRED_CONTEXT_POINT(point) \
    TraceContext::SetContext(point),

  #define CAPTURE_CREATION(name) \
    CreationTrace creation_context_ = name;

  #define TRACE_CREATION \
    TraceCreation trace_creation(creation_context_);

#else

  #define TRACE_FUNCTION(name)

  #define TRACE_CLEANUP

  #define SET_CONTEXT_POINT(point)

  #define PRED_CONTEXT_POINT(point)

  #define CAPTURE_CREATION(name)

  #define TRACE_CREATION

#endif


class TraceContext : public capture_thread::ThreadCapture<TraceContext> {
 public:
  static TraceList GetTrace();

  template<int S>
  static inline void SetContext(const char(&at)[S]) {
    if (GetCurrent()) {
      GetCurrent()->SetLocal(at);
    }
  }

 protected:
  virtual ~TraceContext() = default;

  static inline const char* GetName() {
    return GetCurrent() ? GetCurrent()->Name() : nullptr;
  }

 private:
  virtual const char* Name() const { return nullptr; }
  virtual void SetLocal(const char*) = 0;
  virtual void AppendTrace(TraceList& trace) const = 0;
  virtual const TraceContext* GetNext() const = 0;
};

class SourceContext : public TraceContext {
 public:
  template<int S>
  inline SourceContext(const char(&name)[S])
    : at_(nullptr), name_(name), capture_to_(this) {}

 private:
  const char* Name() const final { return name_; }
  void SetLocal(const char*) final;
  void AppendTrace(TraceList& trace) const final;
  const TraceContext* GetNext() const final;

  const char* at_;
  const char* const name_;
  const ScopedCapture capture_to_;
};

class CleanupContext : public TraceContext {
 public:
  inline CleanupContext()
    : at_(nullptr), capture_to_(this) {}

 private:
  void SetLocal(const char*) final;
  void AppendTrace(TraceList& trace) const final;
  const TraceContext* GetNext() const final;

  const char* at_;
  const ScopedCapture capture_to_;
};

class Argv : public capture_thread::ThreadCapture<Argv> {
 public:
  static int ArgCount();
  static const std::string& GetArgAt(int pos);

 protected:
  virtual ~Argv() = default;

 private:
  virtual const std::vector<std::string>& GetArgs() const = 0;
};

class ProgramArgv : public Argv {
 public:
  inline ProgramArgv(int argc, const char** argv)
    : argv_(argv, argv + argc), cross_and_capture_to_(this) {}

 private:
  const std::vector<std::string>& GetArgs() const final;

  const std::vector<std::string> argv_;
  const AutoThreadCrosser cross_and_capture_to_;
};

class CreationTrace {
 public:
  // NOTE: Using a template with [] here breaks inline member initialization
  // when using CAPTURE_CREATION in g++.
  inline CreationTrace(const char* type)
    : type_(type), trace_(TraceContext::GetTrace()) {}

  inline std::string GetType() const {
    return type_;
  }

  inline const TraceList& GetTrace() const {
    return trace_;
  }

 private:
  const char* const type_;
  const TraceList trace_;
};

class TraceCreation : public capture_thread::ThreadCapture<TraceCreation> {
 public:
  inline TraceCreation(const CreationTrace& trace)
    : trace_(trace), capture_to_(this) {}

  static inline std::string GetType() {
    if (GetCurrent()) {
      return GetCurrent()->trace_.GetType();
    } else {
      return std::string();
    }
  }

  static inline TraceList GetTrace() {
    if (GetCurrent()) {
      return GetCurrent()->trace_.GetTrace();
    } else {
      return TraceList();
    }
  }

 private:
  const CreationTrace& trace_;
  const ScopedCapture capture_to_;
};

class LogCalls : public capture_thread::ThreadCapture<LogCalls> {
 public:
  static inline void MaybeLogCall(const char* name, const char* at) {
    if (GetCurrent()) {
      GetCurrent()->LogCall(name, at);
    }
  }

 protected:
  virtual ~LogCalls() = default;

 private:
  virtual void LogCall(const char* name, const char* at) = 0;
};

class LogCallsToFile : public LogCalls {
 public:
  LogCallsToFile(std::string filename);

 private:
  void LogCall(const char* name, const char* at) final;

  std::mutex mutex_;
  const unsigned int unique_id_;
  const std::string& filename_;
  const std::unique_ptr<std::fstream> log_file_;
  const AutoThreadCrosser cross_and_capture_to_;
};

#endif  // LOGGING_HPP_
