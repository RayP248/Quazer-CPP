#ifndef ERROR_H
#define ERROR_H

#include <string>

namespace error
{
  enum ErrorCode
  {
    LEXER_ERROR,
    PARSER_ERROR,
    SEMANTIC_ERROR,
    RUNTIME_ERROR,
  };

  enum ErrorImportance
  {
    MINIMAL,
    LOW,
    MEDIUM,
    MODERATE,
    HIGH,
    CRITICAL
  };

  class Error
  {
  private:
    ErrorCode code;
    std::string message;
    int linestart;
    int lineend;
    int columnstart;
    int columnend;
    std::string origin;

  public:
    ErrorImportance importance;
    Error(ErrorCode code, std::string message, int linestart, int lineend, int columnstart, int columnend, std::string origin, ErrorImportance importance);
    void print(bool not_first, bool not_last);

  private:
    void highlightLine(const std::string &line);
    void highlightErrorLocation(int columnstart, int columnend);
  };
}

#endif // ERROR_H
