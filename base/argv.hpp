/* -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

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

#ifndef ARGV_HPP_
#define ARGV_HPP_

#include <string>
#include <vector>

#include "thread-capture.h"


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
    : argv_(argv, argv + argc), capture_to_(this) {}

 private:
  const std::vector<std::string>& GetArgs() const final;

  const std::vector<std::string> argv_;
  const ScopedCapture capture_to_;
};

#endif  // ARGV_HPP_
