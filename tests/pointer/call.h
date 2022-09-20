/* -----------------------------------------------------------------------------
Copyright 2022 Kevin P. Barry

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

#ifndef CALL_H_
#define CALL_H_

#include <string>

struct Message {
  std::string message_data;
};

struct OtherBase {
  std::string other_base_data;
};

// NOTE: The inheritance of Message here doesn't need to match the inheritance
// in call.0rp because inheritance doesn't matter for Pointer.

// The virtual base class is meant to change the offset.
struct Request : public OtherBase, virtual public Message {
  explicit inline Request(std::string data)
    : Message{ "Request" },
      request_data(std::move(data)) {}

  std::string request_data;
};

// The virtual base class is meant to change the offset.
struct Response : public OtherBase, virtual public Message {
  explicit inline Response(std::string data)
    : Message{ "Response" },
      response_data(std::move(data)) {}

  std::string response_data;
};

#endif  // CALL_H_
