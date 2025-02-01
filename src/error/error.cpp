#include "../global.h"
#include "error.h"
#include <string>
#include <vector>
#include <unordered_map>
#include <iostream>
#include <fstream>
#include <regex>

void setColor(int textColor)
{
  std::cout << "\033[" << textColor << "m";
}

void setColorRGB(int r, int g, int b)
{
  std::cout << "\033[38;2;" << r << ";" << g << ";" << b << "m";
}

void resetColor() { std::cout << "\033[0m"; }

void displayImportance(error::ErrorImportance importance)
{
  switch (importance)
  {
  case error::ErrorImportance::MINIMAL:
    setColor(32); // green
    std::cout << "|MIN| ";
    break;
  case error::ErrorImportance::LOW:
    setColor(33); // yellow
    std::cout << "|LOW| ";
    break;
  case error::ErrorImportance::MEDIUM:
    setColor(34); // dark blue
    std::cout << "|MED| ";
    break;
  case error::ErrorImportance::MODERATE:
    setColor(35); // purple
    std::cout << "|MOD| ";
    break;
  case error::ErrorImportance::HIGH:
    setColor(31); // red
    std::cout << "|HIG| ";
    break;
  case error::ErrorImportance::CRITICAL:
    setColorRGB(255, 45, 0); //! bright red
    std::cout << "|CRI| ";
    break;
  }
}

namespace error
{
  Error::Error(ErrorCode code, std::string message, int linestart, int lineend, int columnstart, int columnend, std::string origin, ErrorImportance importance)
      : code(code), message(message), linestart(linestart), lineend(lineend), columnstart(columnstart), columnend(columnend), origin(origin), importance(importance)
  {
    global::errors.push_back(*this);
  }

  void Error::print(bool first, bool last)
  {
    std::string finalLine = lineend == -1 ? std::to_string(linestart) + "+" : linestart == lineend ? std::to_string(linestart)
                                                                                                   : std::to_string(linestart) + "-" + std::to_string(lineend);
    std::string finalCol = columnend == -1 ? std::to_string(columnstart) + "+" : std::to_string(columnstart + 1) + "-" + std::to_string(columnend + 1);

    if (first)
      std::cout << "  -----------------------------------------\n";
    std::cout
        << " | quazer # ";
    displayImportance(importance);
    setColor(31); // red
    std::cout << "Error";
    setColor(34); // blue
    std::cout << " [" << global::currfile << "]: ";
    resetColor();
    std::cout << message << (finalLine.length() > 1 ? " on lines " + finalLine : " on line " + finalLine) << " through columns " << finalCol << '\n'
              << " |    - from " << origin << '\n';

    // new snippet display
    {
      std::ifstream file(global::currfile);
      if (file.is_open())
      {
        std::vector<std::string> lines;
        std::string rawline;
        while (std::getline(file, rawline))
        {
          lines.push_back(rawline);
        }

        int snippetStart = linestart > 5 ? linestart - 5 : 1;
        for (int i = snippetStart - 1; i < linestart; i++)
        {
          std::cout << " |    " << (i + 1) << " | ";
          highlightLine(lines[i]);
          std::cout << "\n";
        }
        highlightErrorLocation(columnstart, columnend);
      }
      std::cout << '\n'
                << " " << (last ? " " : "|") << "-----------------------------------------\n";
    }
  }

  void Error::highlightErrorLocation(int columnstart, int columnend)
  {
    std::cout << " |         ";
    for (int i = 0; i < columnstart; ++i)
    {
      std::cout << " ";
    }
    setColor(31); // red color
    if (columnend == -1)
    {
      std::cout << "^";
      for (int i = columnstart + 1; i < columnstart + 2; ++i)
      {
        std::cout << "~";
      }
    }
    else
    {
      for (int i = columnstart + 1; i <= columnend; ++i)
      {
        std::cout << "^";
      }
    }
    resetColor();
  }

  void Error::highlightLine(const std::string &line)
  {
    // Updated regex to separate punctuation from alphanumerics
    static const std::regex tokenRegex(R"(\w+|[^\w\s]+|\s+)");
    static const std::regex stringRegex(R"(".*"$)");
    // Keywords excluding these new types
    static const std::regex keywordRegex(R"(\b(public|const|let|if|else|for|while|return|class|struct|fn|void)\b)");
    // Updated type regex
    static const std::regex typeRegex(R"(\b(number|string|array)\b)");
    static const std::regex numberRegex(R"(^\d+(\.\d+)?$)");
    static const std::regex functionRegex(R"([A-Za-z_]\w*(?=\())");
    static const std::regex punctuationRegex(R"([\(\)\{\}\[\]])");

    std::sregex_iterator begin(line.begin(), line.end(), tokenRegex), end;
    for (auto it = begin; it != end; ++it)
    {
      std::string token = it->str();
      if (std::all_of(token.begin(), token.end(), isspace))
      {
        std::cout << token;
        continue;
      }

      // Check if next non-whitespace char is '(' (function check)
      size_t nextPos = it->position() + token.size();
      while (nextPos < line.size() && std::isspace((unsigned char)line[nextPos]))
        nextPos++;
      bool looksLikeFunction = (nextPos < line.size() && line[nextPos] == '(');

      // Apply highlighting
      if (looksLikeFunction)
      {
        // set function color to #DCDCAA -> (220, 220, 170)
        setColorRGB(220, 220, 170);
      }
      else if (std::regex_match(token, stringRegex))
      {
        setColor(93); // orange
      }
      else if (std::regex_match(token, keywordRegex))
      {
        setColor(34); // dark blue
      }
      else if (std::regex_match(token, typeRegex))
      {
        // #4EC9B0 -> (78, 201, 176)
        setColorRGB(80, 200, 120);
      }
      else if (std::regex_match(token, numberRegex))
      {
        setColor(32); // green
      }
      else if (std::regex_match(token, punctuationRegex))
      {
        // Ensure it's not part of an ANSI escape sequence
        bool isAnsiEscape = false;
        if (it->position() > 0 && line[it->position() - 1] == '\033')
        {
          isAnsiEscape = true;
        }
        size_t nextPos = it->position() + token.size();
        if (nextPos < line.size() && (line[nextPos] == 'm' || line.substr(nextPos, 5) == "38;2;"))
        {
          isAnsiEscape = true;
        }
        if (!isAnsiEscape)
        {
          setColor(35); // purple
        }
      }
      else if (std::isalpha(token[0]) || token[0] == '_')
      {
        setColor(36); // light blue
      }

      std::cout << token;
      resetColor();
    }
  }

  void display_all_errors(bool terminate)
  {
    if (global::errors.size() > 0)
    {
      std::cout << global::errors.size() << " errors found:\n";
      for (auto &err : global::errors)
      {
        err.print(&global::errors.front() == &err, &global::errors.back() == &err);
      }
      exit(1);
    }
  }
}
