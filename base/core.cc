#include "core.h"

#include <csignal>
#include <cstdlib>
#include <iostream>

#include "trace.h"

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
