#ifndef TRACE_H_
#define TRACE_H_

#include <list>
#include <string>

#include "capture-thread/include/thread-capture.h"

#ifndef DISABLE_TRACING

  #define TRACE_FUNCTION(name) \
    SourceContext source_context(name);

  #define SET_CONTEXT_POINT(point) \
    source_context.SetLocal(point);

#else

  #define TRACE_FUNCTION(name)

  #define SET_CONTEXT_POINT(point)

#endif

class TraceContext : public capture_thread::ThreadCapture<TraceContext> {
 public:
  static std::list<std::string> GetTrace();

 protected:
  virtual void AppendTrace(std::list<std::string>& trace) const = 0;
  virtual const TraceContext* GetNext() const = 0;
  virtual ~TraceContext() = default;
};

class SourceContext : public TraceContext {
 public:
  explicit SourceContext(std::string name);
  void SetLocal(const std::string& at);

 private:
  void AppendTrace(std::list<std::string>& trace) const final;
  const TraceContext* GetNext() const final;

  std::string at_;
  const std::string name_;
  const AutoThreadCrosser cross_and_capture_to_;
};

#endif  // TRACE_H_
