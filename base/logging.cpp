/* -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

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

#include <cassert>
#include <csignal>
#include <iostream>


LogThenCrash::LogThenCrash(bool fail, const std::string& condition)
    : fail_(fail), signal_(SIGABRT), condition_(condition) {}

LogThenCrash::LogThenCrash(bool fail, int signal)
    : fail_(fail), signal_(signal) {}

LogThenCrash::~LogThenCrash() {
  if (fail_) {
    std::signal(signal_, SIG_DFL);
    if (Argv::ArgCount() > 0) {
      std::cerr << Argv::GetArgAt(0) << ": ";
    }
    std::cerr << "Failed condition";
    if (!condition_.empty()) {
      std::cerr << " '" << condition_ << "'";
    }
    std::cerr << ": " << output_.str() << std::endl;
    const TraceList call_trace = TraceContext::GetTrace();
    for (const auto& trace : call_trace) {
      if (!trace.empty()) {
        std::cerr << "  " << trace << std::endl;
      }
    }
    const TraceList creation_trace = TraceCreation::GetTrace();
    if (!creation_trace.empty()) {
      std::cerr << TraceCreation::GetType() << " value originally created at:" << std::endl;
      for (const auto& trace : creation_trace) {
        if (!trace.empty()) {
          std::cerr << "  " << trace << std::endl;
        }
      }
    }
    std::raise(signal_);
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
}

void SourceContext::AppendTrace(TraceList& trace) const {
  std::ostringstream output;
  if (at_ == nullptr || at_[0] == 0x00) {
    output << "From " << name_;
  } else {
    output << "From " << name_ << " at " << at_;
  }
  trace.push_back(output.str());
}

const TraceContext* SourceContext::GetNext() const {
  return capture_to_.Previous();
}


void CleanupContext::SetLocal(const char* at) {
  at_ = at;
}

void CleanupContext::AppendTrace(TraceList& trace) const {
  std::ostringstream output;
  if (at_ == nullptr || at_[0] == 0x00) {
    output << "In cleanup block";
  } else {
    output << "In cleanup block at " << at_;
  }
  trace.push_back(output.str());
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
