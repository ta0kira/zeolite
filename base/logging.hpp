#ifndef LOGGING_HPP_
#define LOGGING_HPP_

#include <list>
#include <sstream>
#include <string>

#include "thread-capture.h"


#define FAIL() LogThenCrash(true)

#define FAIL_IF(p) LogThenCrash(p,#p)

void SetSignalHandler();

class LogThenCrash {
 public:
  LogThenCrash(bool fail, const std::string& condition = "");
  LogThenCrash(bool fail, int signal);
  ~LogThenCrash();

  template<class T>
  LogThenCrash& operator << (const T& stuff) {
    if (fail_) {
      static_cast<std::ostream&>(output_) << stuff;
    }
    return *this;
  }

 private:
  const bool fail_;
  const int signal_;
  const std::string condition_;
  std::ostringstream output_;
};


#ifndef DISABLE_TRACING

  #define TRACE_FUNCTION(name) \
    SourceContext source_context(name);

  #define SET_CONTEXT_POINT(point) \
    source_context.SetLocal(point);

  #define PRED_CONTEXT_POINT(point) \
    source_context.SetLocal(point),

#else

  #define TRACE_FUNCTION(name)

  #define SET_CONTEXT_POINT(point)

  #define PRED_CONTEXT_POINT(point)

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

#endif  // LOGGING_HPP_
