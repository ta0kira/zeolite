#include <iostream>

#include "base.h"

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
struct Adapter<char,int> {
  static constexpr bool defined = true;
  static int Convert(char value) { return value; }
};

template<>
struct Adapter<long,int> {
  static constexpr bool defined = true;
  static int Convert(long value) { return value; }
};


int main() {
  auto val = I_get<long,int,char>::From(10);
  int x;
  val->get(x);
  long y;
  val->get(y);
  char z;
  val->get(z);

  std::string w;
  // Error! Cannot convert *any* of the other types to string.
  // val->get(w);


  // Error! Cannot convert string to the other types.
  // auto val2 = U_get<long,int,char>::From("");

  auto val2 = U_get<long,int,std::string>::From(10);
  val2 = U_get<long,int,std::string>::From("");

  // Error! Cannot convert *all* of the other types to int.
  // val->get(x);

  auto val3 = U_get<long,int,char>::From(10);
  val3->get(x);
  val3 = U_get<long,int,char>::From(10L);
  val3->get(x);
  val3 = U_get<long,int,char>::From('x');
  val3->get(x);

  // Error! int does not match any types in the empty set.
  // auto val4 = U_get<>::From(10);

  auto val4 = I_get<>::From(10);

  // Error! val4 can contain *any* type, and thus cannot convert to int.
  // val4->get(x);
}
