
#ifndef UTILS_H
#define UTILS_H

#include <string>
#include <vector>
#include <stdexcept>
#include <sstream>

using namespace std;

namespace pisym {

template <typename T>
  string vector_to_string(const vector<T> & vec,
                          const string& delim=string{","},
                          const string& open=string{"["},
                          const string& close=string{"]"}) {
  stringstream buf;

  buf << open;

  bool start = true;

  for(auto & elem : vec) {
    if(start) {
      start = false;
    } else {
      buf << delim;
    }

    buf << elem;
  }

  buf << close;


  return buf.str();
 }


 class invariant_error : public std::logic_error {
 public:
   explicit invariant_error(const string& what_arg) : logic_error(what_arg) {}
   explicit invariant_error(const char* what_arg) : logic_error(what_arg) {}
 };

 
}


#endif
