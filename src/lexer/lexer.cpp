#include "lexer.h"
#include "../error/error.h"
#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <unordered_map>

namespace lexer {
  std::unordered_map<std::string, lexer::TokenKind> ops = {
    {"+", lexer::TokenKind::PLUS},
    {"-", lexer::TokenKind::MINUS},
    {"*", lexer::TokenKind::STAR},
    {"/", lexer::TokenKind::SLASH},
    {"%", lexer::TokenKind::PERCENT},
    {"(", lexer::TokenKind::OPEN_PAREN},
    {")", lexer::TokenKind::CLOSE_PAREN},
    {"{", lexer::TokenKind::OPEN_CURLY},
    {"}", lexer::TokenKind::CLOSE_CURLY},
    {"[", lexer::TokenKind::OPEN_BRACE},
    {"]", lexer::TokenKind::CLOSE_BRACE},
    {".", lexer::TokenKind::DOT},
    {"..", lexer::TokenKind::DOT_DOT},
    {":", lexer::TokenKind::COLON},
    {";", lexer::TokenKind::SEMICOLON},
    {",", lexer::TokenKind::COMMA},
    {"=", lexer::TokenKind::ASSIGNMENT},
    {"+=", lexer::TokenKind::PLUS_ASSIGNMENT},
    {"-=", lexer::TokenKind::MINUS_ASSIGNMENT},
    {"*=", lexer::TokenKind::STAR_ASSIGNMENT},
    {"/=", lexer::TokenKind::SLASH_ASSIGNMENT},
    {"%=", lexer::TokenKind::PERCENT_ASSIGNMENT},
    {"==", lexer::TokenKind::EQUAL},
    {"!", lexer::TokenKind::NOT},
    {"!=", lexer::TokenKind::NOT_EQUAL},
    {">", lexer::TokenKind::GREATER},
    {">=", lexer::TokenKind::GREATER_EQUAL},
    {"<", lexer::TokenKind::LESS},
    {"<=", lexer::TokenKind::LESS_EQUAL},
    {"&&", lexer::TokenKind::AND},
    {"||", lexer::TokenKind::OR},
    {"->", lexer::TokenKind::TYPE_ARROW},
    {":>", lexer::TokenKind::LAMBDA_ARROW},
    {"'", lexer::TokenKind::SINGLE_QUOTE},
    {"\"", lexer::TokenKind::DOUBLE_QUOTE},
    {"--", lexer::TokenKind::DASH_DASH},
    {"\\", lexer::TokenKind::BACK_SLASH},
    {"`", lexer::TokenKind::BACK_TICK},
  };

  std::unordered_map<std::string, lexer::TokenKind> reserved_keywords = {
    {"package", lexer::TokenKind::PACKAGE},
    {"const", lexer::TokenKind::CONST},
    {"let", lexer::TokenKind::LET},
    {"struct", lexer::TokenKind::STRUCT},
    {"fn", lexer::TokenKind::FN},
    {"return", lexer::TokenKind::RETURN},
    {"if", lexer::TokenKind::IF},
    {"else", lexer::TokenKind::ELSE},
    {"for", lexer::TokenKind::FOR},
    {"then", lexer::TokenKind::THEN},
    {"class", lexer::TokenKind::CLASS},
    {"constructor", lexer::TokenKind::CONSTRUCTOR},
    {"operator", lexer::TokenKind::OPERATOR},
    {"create", lexer::TokenKind::CREATE},
    {"for", lexer::TokenKind::FOR},
    {"extend", lexer::TokenKind::EXTEND},
    {"prop", lexer::TokenKind::PROP},
    {"end", lexer::TokenKind::END},
  };

  void Token::debug() const {
    std::cout << token_kind_to_string(kind) << " (" << value << ") "
              << linestart << "-" << lineend << " : "
              << columnstart << "-" << columnend << '\n'; // commented out
  }

  std::string token_kind_to_string(TokenKind kind)
  {
    switch (kind) {
    case TokenKind::NUMBER:
      return "NUMBER";
    case TokenKind::IDENTIFIER:
      return "IDENTIFIER";
    case TokenKind::STRING:
      return "STRING";
    case TokenKind::PLUS:
      return "PLUS";
    case TokenKind::MINUS:
      return "MINUS";
    case TokenKind::STAR:
      return "STAR";
    case TokenKind::SLASH:
      return "SLASH";
    case TokenKind::PERCENT:
      return "PERCENT";
    case TokenKind::TYPE_ARROW:
      return "TYPE_ARROW";
    case TokenKind::LAMBDA_ARROW:
      return "LAMBDA_ARROW";
    case TokenKind::DOT:
      return "DOT";
    case TokenKind::DOT_DOT:
      return "DOT_DOT";
    case TokenKind::EQUAL:
      return "EQUAL";
    case TokenKind::NOT:
      return "NOT";
    case TokenKind::NOT_EQUAL:
      return "NOT_EQUAL";
    case TokenKind::GREATER:
      return "GREATER";
    case TokenKind::GREATER_EQUAL:
      return "GREATER_EQUAL";
    case TokenKind::LESS:
      return "LESS";
    case TokenKind::LESS_EQUAL:
      return "LESS_EQUAL";
    case TokenKind::AND:
      return "AND";
    case TokenKind::OR:
      return "OR";
    case TokenKind::ASSIGNMENT:
      return "ASSIGNMENT";
    case TokenKind::PLUS_ASSIGNMENT:
      return "PLUS_ASSIGNMENT";
    case TokenKind::MINUS_ASSIGNMENT:
      return "MINUS_ASSIGNMENT";
    case TokenKind::STAR_ASSIGNMENT:
      return "STAR_ASSIGNMENT";
    case TokenKind::SLASH_ASSIGNMENT:
      return "SLASH_ASSIGNMENT";
    case TokenKind::PERCENT_ASSIGNMENT:
      return "PERCENT_ASSIGNMENT";
    case TokenKind::OPEN_BRACE:
      return "OPEN_BRACE";
    case TokenKind::CLOSE_BRACE:
      return "CLOSE_BRACE";
    case TokenKind::OPEN_CURLY:
      return "OPEN_CURLY";
    case TokenKind::CLOSE_CURLY:
      return "CLOSE_CURLY";
    case TokenKind::OPEN_PAREN:
      return "OPEN_PAREN";
    case TokenKind::CLOSE_PAREN:
      return "CLOSE_PAREN";
    case TokenKind::COLON:
      return "COLON";
    case TokenKind::SEMICOLON:
      return "SEMICOLON";
    case TokenKind::COMMA:
      return "COMMA";
    case TokenKind::SINGLE_QUOTE:
      return "SINGLE_QUOTE";
    case TokenKind::DOUBLE_QUOTE:
      return "DOUBLE_QUOTE";
    case TokenKind::DASH_DASH:
      return "DASH_DASH";
    case TokenKind::BACK_SLASH:
      return "BACK_SLASH";
    case TokenKind::BACK_TICK:
      return "BACK_TICK";
    case TokenKind::PACKAGE:
      return "PACKAGE";
    case TokenKind::CONST:
      return "CONST";
    case TokenKind::LET:
      return "LET";
    case TokenKind::STRUCT:
      return "STRUCT";
    case TokenKind::FN:
      return "FN";
    case TokenKind::RETURN:
      return "RETURN";
    case TokenKind::IF:
      return "IF";
    case TokenKind::ELSE:
      return "ELSE";
    case TokenKind::FOR:
      return "FOR";
    case TokenKind::THEN:
      return "THEN";
    case TokenKind::CLASS:
      return "CLASS";
    case TokenKind::CONSTRUCTOR:
      return "CONSTRUCTOR";
    case TokenKind::OPERATOR:
      return "OPERATOR";
    case TokenKind::CREATE:
      return "CREATE";
    case TokenKind::EXTEND:
      return "EXTEND";
    case TokenKind::PROP:
      return "PROP";
    case TokenKind::END:
      return "END";
    case TokenKind::REGEXP:
      return "REGEXP";
    case TokenKind::EOF_:
      return "EOF";
    default:
      return "UNKNOWN";
    }
  }

  lexer::Token new_token(lexer::TokenKind kind, std::string value, int linestart, int lineend, int columnstart, int columnend)
  {
    lexer::Token token;
    token.kind = kind;
    token.value = std::move(value);
    token.linestart = linestart;
    token.lineend = lineend;
    token.columnstart = columnstart;
    token.columnend = columnend;
    if (token.value.empty()) {
      token.columnend = -1;
    }
    return token;
  }

  std::vector<lexer::Token> tokenize(std::string input) {
    // Strip UTF-8 BOM if present
    if (input.size() >= 3 &&
        (unsigned char)input[0] == 0xEF &&
        (unsigned char)input[1] == 0xBB &&
        (unsigned char)input[2] == 0xBF)
    {
      input.erase(0, 3);
    }

    // Check for any non-ASCII punctuation just after stripping BOM
    int tcol = 1;
    int tline = 1;
    for (char c : input)
    {
      if (c == '\n')
      {
        tline++;
        tcol = 0;
      }
      else
      {
        tcol++;
      }
      if (static_cast<unsigned char>(c) > 127)
      {
        new error::Error(error::ErrorCode::LEXER_ERROR, "Non-ASCII character found: code " + std::to_string((unsigned char)c), tline, tline, tcol, tcol + 1, "lexer.cpp : tokenize : for loop : if statement", error::ErrorImportance::CRITICAL);
      }
    }

    std::vector<lexer::Token> tokens;
    tokens.reserve(100); // Reserve an estimated capacity for tokens
    std::string::const_iterator it = input.begin();
    std::string::const_iterator end = input.end();
    int col = 1;
    int line = 1;

    auto handle_grouping = [&](char op)
    {
      tokens.push_back(new_token(ops[std::string(1, op)], std::string(1, op), line, line, col, col + 1));
      col++;
      ++it;
    };

    auto handle_operator = [&](const std::string &op)
    {
      tokens.push_back(new_token(ops[op], op, line, line, col, col + op.size()));
      col += op.size();
      it += op.size();
    };

    while (it != end) {
      // Skip comment lines starting with `--`
      if (std::distance(it, end) > 1 && *it == '-' && *(it + 1) == '-') {
        // Skip until newline or end
        while (it != end && *it != '\n') {
          ++it;
          ++col;
        }
        continue;
      }

      switch (*it) {
        case '\n':
        case '\r':
          line++;
          col = 0;
          ++it;
          break;
        case ' ':
        case '\t':
          col++;
          ++it;
          break;
        default:
        {
          if (std::isdigit(*it)) {
            std::string num;
            while (it != end && (std::isdigit(*it) || *it == '.' || *it == '_')) {
              // Break numeric parse if next char(s) form an operator like ".."
              if (*it == '.' && (it + 1) != end && *(it + 1) == '.') {
                break;
              }
              num.push_back(*it);
              ++it; col++;
            }
            tokens.push_back(new_token(lexer::TokenKind::NUMBER, std::move(num), line, line, col - num.size(), col));
          } else if (std::isalpha(*it)) {
            std::string value;
            while (it != end && (std::isalpha(*it) || std::isdigit(*it) || *it == '_')) {
              value.push_back(*it);
              ++it;
              col++;
            }
            // For case-insensitive matching of reserved keywords
            std::string lower;
            lower.reserve(value.size());
            for (char c : value) {
              lower.push_back(std::tolower(static_cast<unsigned char>(c)));
            }
            if (reserved_keywords.find(lower) != reserved_keywords.end()) {
              tokens.push_back(new_token(reserved_keywords[lower],
                                         std::move(value), line, line, col - value.size(), col));
            }
            else
            {
              tokens.push_back(new_token(lexer::TokenKind::IDENTIFIER,
                                         std::move(value), line, line, col - value.size(), col));
            }
          }
          else if (
              (it + 1 != end && ops.find(std::string(1, *it) + *(it + 1)) != ops.end()) || ops.find(std::string(1, *it)) != ops.end())
          {
            if (ops.find(std::string(1, *it) + *(it + 1)) != ops.end())
            {
              handle_operator(std::string(1, *it) + *(it + 1));
            }
            else
            {
              handle_operator(std::string(1, *it));
            }
          }
          else
          {
            error::Error(error::ErrorCode::LEXER_ERROR, "Unknown token: '" + std::string(1, *it) + "'",
                         line, -1, col, -1,
                         "lexer.cpp : switch statement : default case : else",
                         error::ErrorImportance::CRITICAL);
            ++it;
            ++col;
          }
          break;
        }
      }
    }
    tokens.push_back(new_token(lexer::TokenKind::EOF_, "", line, line, col, col));
    return tokens;
  }
}
