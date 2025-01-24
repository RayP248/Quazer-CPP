#ifndef GLOBAL_H
#define GLOBAL_H

#include <string>
#include <vector>
#include "error/error.h"

namespace global
{
  extern std::string currfile;
  extern std::vector<error::Error> errors;
}
#endif // GLOBAL_H
