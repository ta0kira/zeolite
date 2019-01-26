#include "trace.h"

#include <cassert>
#include <sstream>

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
