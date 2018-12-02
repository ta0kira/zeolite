#include "core.h"

#include <csignal>
#include <iostream>

#include "trace.h"

LogThenCrash::LogThenCrash(bool fail, const std::string& condition)
    : fail_(fail), condition_(condition) {}

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
    // Not abort(), since that unsets the signal handler first.
    std::raise(SIGABRT);
  }
}
