/* -----------------------------------------------------------------------------
Copyright 2019-2022 Kevin P. Barry

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

#include "logging.hpp"

#include <atomic>
#include <chrono>
#include <csignal>
#include <iomanip>
#include <iostream>


LogThenCrash::LogThenCrash(bool fail, const std::string& condition)
    : fail_(fail), signal_(SIGTERM), condition_(condition) {}

LogThenCrash::LogThenCrash(bool fail, int signal)
    : fail_(fail), signal_(signal) {}

LogThenCrash::~LogThenCrash() {
  static std::atomic_int waiting_count{0};
  static std::atomic_flag print_lock = ATOMIC_FLAG_INIT;
  if (fail_) {
    std::signal(signal_, SIG_DFL);
    ++waiting_count;
    while (print_lock.test_and_set(std::memory_order_acquire));
    --waiting_count;
    if (Argv::ArgCount() > 0) {
      std::cerr << Argv::GetArgAt(0) << ": ";
    }
    std::cerr << "Failed condition";
    if (!condition_.empty()) {
      std::cerr << " '" << condition_ << "'";
    }
    std::cerr << ": " << output_.str() << std::endl;
    PrintTrace(TraceContext::GetTrace());
    const TraceList creation_trace = TraceCreation::GetTrace();
    if (!creation_trace.empty()) {
      std::cerr << "Original " << TraceCreation::GetType() << " value creation:" << std::endl;
      PrintTrace(creation_trace);
    }
    if (!waiting_count.load()) {
      std::raise(signal_);
    }
    print_lock.clear(std::memory_order_release);
  }
}

// static
void LogThenCrash::PrintTrace(const TraceList &call_trace) {
  for (const auto& trace : call_trace) {
    const std::string message = trace();
    if (!message.empty()) {
      std::cerr << "  " << message << std::endl;
    }
  }
}

// TODO: Should only be available used if POSIX is defined.
void TraceOnSignal(int signal) {
  LogThenCrash(true,signal) << "Recieved signal " << signal;
}

// TODO: Should only be available used if POSIX is defined.
void SetSignalHandler() {
#ifdef SIGINT
  std::signal(SIGINT, &TraceOnSignal);
#endif
#ifdef SIGILL
  std::signal(SIGILL, &TraceOnSignal);
#endif
#ifdef SIGABRT
  std::signal(SIGABRT, &TraceOnSignal);
#endif
#ifdef SIGFPE
  std::signal(SIGFPE, &TraceOnSignal);
#endif
#ifdef SIGQUIT
  std::signal(SIGQUIT, &TraceOnSignal);
#endif
#ifdef SIGSEGV
  std::signal(SIGSEGV, &TraceOnSignal);
#endif
#ifdef SIGPIPE
  std::signal(SIGPIPE, &TraceOnSignal);
#endif
#ifdef SIGALRM
  std::signal(SIGALRM, &TraceOnSignal);
#endif
#ifdef SIGTERM
  std::signal(SIGTERM, &TraceOnSignal);
#endif
#ifdef SIGUSR1
  std::signal(SIGUSR1, &TraceOnSignal);
#endif
#ifdef SIGUSR2
  std::signal(SIGUSR2, &TraceOnSignal);
#endif
}


TraceList TraceContext::GetTrace() {
  TraceList trace;
  const TraceContext* current = GetCurrent();
  while (current) {
    current->AppendTrace(trace);
    current = current->GetNext();
  }
  return trace;
}


void SourceContext::SetLocal(const char* at) {
  at_ = at;
  LogCalls::MaybeLogCall(name_, at_);
}

void SourceContext::AppendTrace(TraceList& trace) const {
  const char* const name = name_;
  const char* const at   = at_;
  trace.push_back([name,at]() {
      std::ostringstream output;
      if (at == nullptr || at[0] == 0x00) {
        output << "From " << name;
      } else {
        output << "From " << name << " at " << at;
      }
      return output.str();
    });
}

const TraceContext* SourceContext::GetNext() const {
  return capture_to_.Previous();
}


void CleanupContext::SetLocal(const char* at) {
  at_ = at;
  LogCalls::MaybeLogCall(GetName(), at_);
}

void CleanupContext::AppendTrace(TraceList& trace) const {
  const char* const at = at_;
  trace.push_back([at]() {
      std::ostringstream output;
      if (at == nullptr || at[0] == 0x00) {
        output << "In cleanup block";
      } else {
        output << "In cleanup block at " << at;
      }
      return output.str();
    });
}

const TraceContext* CleanupContext::GetNext() const {
  return capture_to_.Previous();
}

int Argv::ArgCount() {
  if (GetCurrent()) {
    return GetCurrent()->GetArgs().size();
  } else {
    return 0;
  }
}

const std::string& Argv::GetArgAt(int pos) {
  if (pos < 0 || pos >= ArgCount()) {
    FAIL() << "Argv index " << pos << " is out of bounds";
    __builtin_unreachable();
  } else {
    return GetCurrent()->GetArgs()[pos];
  }
}

const std::vector<std::string>& ProgramArgv::GetArgs() const {
  return argv_;
}

namespace {

std::unique_ptr<std::fstream> TryOpenOutputLog(const std::string& filename) {
  if (filename.empty()) return nullptr;
  std::unique_ptr<std::fstream> file(new std::fstream(
    filename, std::ios::in | std::ios::out | std::ios::ate | std::ios::app));
  if (*file) return file;
  // Maybe the file isn't seekable.
  file.reset(new std::fstream(filename, std::ios::out));
  if (*file) return file;
  FAIL() << "Failed to open call log " << filename << " for writing";
  __builtin_unreachable();
}

}  // namespace

unsigned int UniqueId() {
  const auto time = std::chrono::steady_clock::now().time_since_epoch();
  return (1000000009 * std::chrono::duration_cast<std::chrono::microseconds>(time).count());
}

static bool enable_coverage = true;

// static
void LogCalls::DisableCallLogging() {
  enable_coverage = false;
}

LogCallsToFile::LogCallsToFile(std::string filename)
  : unique_id_(UniqueId()),
    filename_(std::move(filename)),
    log_file_(TryOpenOutputLog(filename_)),
    cross_and_capture_to_(this) {}

void LogCallsToFile::LogCall(const char* name, const char* at) {
  if (log_file_ && enable_coverage) {
    std::lock_guard<std::mutex> lock(mutex_);
    const auto time = std::chrono::steady_clock::now().time_since_epoch();
    *log_file_ << std::chrono::duration_cast<std::chrono::microseconds>(time).count() << ","
               << unique_id_ << ","
               << "\"" << (name ? name : "") << "\"" << ","
               << "\"" << at << "\"" << std::endl;
  }
}
