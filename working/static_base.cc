#include <iostream>

#include "static_base.h"

template<>
struct Adapter<int,char> {
  static constexpr bool defined = true;
  static char Convert(int value) { return value; }
};

template<>
struct Adapter<int,long> {
  static constexpr bool defined = true;
  static long Convert(int value) { return value; }
};

template<>
struct Adapter<int,double> {
  static constexpr bool defined = true;
  static double Convert(int value) { return value; }
};

template<>
struct Adapter<char,int> {
  static constexpr bool defined = true;
  static int Convert(char value) { return value; }
};

template<>
struct Adapter<char,long> {
  static constexpr bool defined = true;
  static long Convert(char value) { return value; }
};

template<>
struct Adapter<long,char> {
  static constexpr bool defined = true;
  static char Convert(long value) { return value; }
};

template<>
struct Adapter<long,int> {
  static constexpr bool defined = true;
  static int Convert(long value) { return value; }
};


int main() {
  auto val = ConvertTo<S<I<long,int,char>>>::From(10);
  int x = ConvertTo<int>::From(val);
  long y = ConvertTo<long>::From(val);
  char z = ConvertTo<char>::From(val);
  double w = ConvertTo<double>::From(val);

  // Error! Cannot convert *any* of the other types to string.
  // ConvertTo<std::string>::From(val);

  // Error! Cannot convert string to the other types.
  // ConvertTo<S<I<long,int,std::string>>>::From(std::string(""));

  auto val2 = ConvertTo<S<U<long,int,std::string>>>::From(10);
  val2 = ConvertTo<S<U<long,int,std::string>>>::From(std::string(""));

  // Error! Cannot convert *all* of the other types to int.
  // ConvertTo<int>::From(val2);

  auto val3 = ConvertTo<S<U<long,int,char>>>::From(10);
  x = ConvertTo<int>::From(val3);
  val3 = ConvertTo<S<U<long,int,char>>>::From(10L);
  x = ConvertTo<int>::From(val3);
  val3 = ConvertTo<S<U<long,int,char>>>::From('x');
  x = ConvertTo<int>::From(val3);

  // Error! int does not match any types in the empty set.
  // ConvertTo<S<U<>>>::From(10);

  auto val4 = ConvertTo<S<I<>>>::From(10);

  // Error! val4 can contain *any* type, and thus cannot convert to int.
  // ConvertTo<int>::From(val4);

  auto val5 = ConvertTo<S<I<long,int,char>>>::From(10);
  x = ConvertTo<int>::From(val5);
  y = ConvertTo<long>::From(val5);
  z = ConvertTo<char>::From(val5);
  w = ConvertTo<double>::From(val5);
}
