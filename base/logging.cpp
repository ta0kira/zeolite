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
    std::cerr << "Failed condition";
    if (!condition_.empty()) {
      std::cerr << " '" << condition_ << "'";
    }
    std::cerr << ": " << output_.str() << std::endl;
    for (const auto& trace : TraceContext::GetTrace()) {
      if (!trace.empty()) {
        std::cerr << "  " << trace << std::endl;
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


std::list<std::string> TraceContext::GetTrace() {
  std::list<std::string> trace;
  const TraceContext* current = GetCurrent();
  while (current) {
    current->AppendTrace(trace);
    current = current->GetNext();
  }
  return trace;
}


void SourceContext::AppendTrace(std::list<std::string>& trace) const {
  std::ostringstream output;
  if (at_ == nullptr) {
    output << "From " << name_;
  } else {
    output << "From " << name_ << " at " << at_;
  }
  trace.push_back(output.str());
}

const TraceContext* SourceContext::GetNext() const {
  return cross_and_capture_to_.Previous();
}
