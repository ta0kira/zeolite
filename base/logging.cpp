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
    std::signal(signal_, SIG_DFL);
    std::raise(signal_);
  }
}

// TODO: Should only be available used if POSIX is defined.
void TraceOnSignal(int signal) {
  LogThenCrash(true,signal) << "Recieved signal " << signal;
}

// TODO: Should only be available used if POSIX is defined.
void SetSignalHandler() {
  std::signal(SIGINT, &TraceOnSignal);
  std::signal(SIGQUIT, &TraceOnSignal);
  std::signal(SIGILL, &TraceOnSignal);
  std::signal(SIGABRT, &TraceOnSignal);
  std::signal(SIGFPE, &TraceOnSignal);
  std::signal(SIGQUIT, &TraceOnSignal);
  std::signal(SIGSEGV, &TraceOnSignal);
  std::signal(SIGPIPE, &TraceOnSignal);
  std::signal(SIGALRM, &TraceOnSignal);
  std::signal(SIGTERM, &TraceOnSignal);
  std::signal(SIGUSR1, &TraceOnSignal);
  std::signal(SIGUSR2, &TraceOnSignal);
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


SourceContext::SourceContext(std::string name)
    : name_(std::move(name)), cross_and_capture_to_(this) {}

void SourceContext::SetLocal(const std::string& at) {
  at_ = at;
}

void SourceContext::AppendTrace(std::list<std::string>& trace) const {
  std::ostringstream output;
  if (at_.empty()) {
    output << "From " << name_;
  } else {
    output << "From " << name_ << " at " << at_;
  }
  trace.push_back(output.str());
}

const TraceContext* SourceContext::GetNext() const {
  return cross_and_capture_to_.Previous();
}
