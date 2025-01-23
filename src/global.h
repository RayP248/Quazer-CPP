#ifndef GLOBAL_H
#define GLOBAL_H

#include <string>
#include <vector>
#include "error/error.h"

namespace global
{
  extern std::string currfile;
  extern std::vector<error::Error> errors;

  // TODO: Add global variables or configurations needed for new functionality.
  // Example:
  // extern bool enableVerboseLogging;
  // extern int maxIterations;
}
#endif // GLOBAL_H
