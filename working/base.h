#ifndef BASE_H_
#define BASE_H_

#include <csignal>
#include <memory>
#include <sstream>

template<class T>
using R = std::unique_ptr<T>;

template<class T>
inline R<T> R_get(T* val) { return R<T>(val); }

template<class T>
using S = std::shared_ptr<T>;

template<class T>
inline S<T> S_get(T* val) { return S<T>(val); }

class LogThenCrash {
 public:
  LogThenCrash(bool fail, const std::string& condition = "")
      : fail_(fail), condition_(condition) {}

  ~LogThenCrash() {
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

  template<class T>
  LogThenCrash& operator << (const T& stuff) {
    if (fail_) {
      static_cast<std::ostream&>(output_) << stuff;
    }
    return *this;
  }

 private:
  const bool fail_;
  const std::string condition_;
  std::ostringstream output_;
};

#define FAIL() LogThenCrash(true)

#define FAIL_IF(p) LogThenCrash(p,#p)

#endif  // BASE_H_
