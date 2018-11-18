#include "base.h"

#include <csignal>
#include <iostream>

LogThenCrash::LogThenCrash(bool fail, const std::string& condition)
    : fail_(fail), condition_(condition) {}

LogThenCrash::~LogThenCrash() {
  if (fail_) {
    std::cerr << "Failed condition";
    if (!condition_.empty()) {
      std::cerr << " '" << condition_ << "'";
    }
    std::cerr << ": " << output_.str() << std::endl;
    // Not abort(), since that unsets the signal handler first.
    std::raise(SIGABRT);
  }
}
