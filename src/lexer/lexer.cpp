#include "lexer.h"
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
    std::cout << tokenKindToString(kind) << " (" << value << ") "
              << linestart << "-" << lineend << " : "
              << columnstart << "-" << columnend << '\n';
  }

  std::string tokenKindToString(TokenKind kind) {
    switch (kind) {
      case NUMBER:
        return "NUMBER";
      case IDENTIFIER:
        return "IDENTIFIER";
      case STRING:
        return "STRING";
      case PLUS:
        return "PLUS";
      case MINUS:
        return "MINUS";
      case STAR:
        return "STAR";
      case SLASH:
        return "SLASH";
      case PERCENT:
        return "PERCENT";
      case TYPE_ARROW:
        return "TYPE_ARROW";
      case LAMBDA_ARROW:
        return "LAMBDA_ARROW";
      case DOT:
        return "DOT";
      case DOT_DOT:
        return "DOT_DOT";
      case EQUAL:
        return "EQUAL";
      case NOT:
        return "NOT";
      case NOT_EQUAL:
        return "NOT_EQUAL";
      case GREATER:
        return "GREATER";
      case GREATER_EQUAL:
        return "GREATER_EQUAL";
      case LESS:
        return "LESS";
      case LESS_EQUAL:
        return "LESS_EQUAL";
      case AND:
        return "AND";
      case OR:
        return "OR";
      case ASSIGNMENT:
        return "ASSIGNMENT";
      case PLUS_ASSIGNMENT:
        return "PLUS_ASSIGNMENT";
      case MINUS_ASSIGNMENT:
        return "MINUS_ASSIGNMENT";
      case STAR_ASSIGNMENT:
        return "STAR_ASSIGNMENT";
      case SLASH_ASSIGNMENT:
        return "SLASH_ASSIGNMENT";
      case PERCENT_ASSIGNMENT:
        return "PERCENT_ASSIGNMENT";
      case OPEN_BRACE:
        return "OPEN_BRACE";
      case CLOSE_BRACE:
        return "CLOSE_BRACE";
      case OPEN_CURLY:
        return "OPEN_CURLY";
      case CLOSE_CURLY:
        return "CLOSE_CURLY";
      case OPEN_PAREN:
        return "OPEN_PAREN";
      case CLOSE_PAREN:
        return "CLOSE_PAREN";
      case COLON:
        return "COLON";
      case SEMICOLON:
        return "SEMICOLON";
      case COMMA:
        return "COMMA";
      case SINGLE_QUOTE:
        return "SINGLE_QUOTE";
      case DOUBLE_QUOTE:
        return "DOUBLE_QUOTE";
      case DASH_DASH:
        return "DASH_DASH";
      case BACK_SLASH:
        return "BACK_SLASH";
      case BACK_TICK:
        return "BACK_TICK";
      case PACKAGE:
        return "PACKAGE";
      case CONST:
        return "CONST";
      case LET:
        return "LET";
      case STRUCT:
        return "STRUCT";
      case FN:
        return "FN";
      case RETURN:
        return "RETURN";
      case IF:
        return "IF";
      case ELSE:
        return "ELSE";
      case FOR:
        return "FOR";
      case THEN:
        return "THEN";
      case CLASS:
        return "CLASS";
      case CONSTRUCTOR:
        return "CONSTRUCTOR";
      case OPERATOR:
        return "OPERATOR";
      case CREATE:
        return "CREATE";
      case EXTEND:
        return "EXTEND";
      case PROP:
        return "PROP";
      case END:
        return "END";
      case REGEXP:
        return "REGEXP";
      case EOF_:
        return "EOF";
      default:
        throw std::runtime_error("Unknown token kind");
    }
  }

  lexer::Token newToken(lexer::TokenKind kind, std::string value, int linestart, int lineend, int columnstart, int columnend) {
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
    std::vector<lexer::Token> tokens;
    tokens.reserve(100); // Reserve an estimated capacity for tokens
    std::string::const_iterator it = input.begin();
    std::string::const_iterator end = input.end();
    int col = 1;
    int line = 1;

    auto handleGrouping = [&](char op) {
      tokens.push_back(newToken(ops[std::string(1, op)], std::string(1, op), line, line, col, col + 1));
      col++;
      ++it;
    };

    auto handleOperator = [&](std::string op) {
      if (std::distance(it, end) > 1) {
        std::string twoCharOp(it, it + 2);
        if (ops.find(twoCharOp) != ops.end()) {
          tokens.push_back(newToken(ops[twoCharOp], std::move(twoCharOp), line, line, col, col + 2));
          col += 2;
          it += 2;
          return;
        }
      }
      std::string oneCharOp(1, *it);
      if (ops.find(oneCharOp) != ops.end()) {
        if(oneCharOp == "\'" || oneCharOp == "\"") {
          std::string value;
          ++it;
          col++;
          while (it != end && *it != oneCharOp[0]) {
            value.push_back(*it);
            ++it;
            col++;
          }
          tokens.push_back(newToken(lexer::TokenKind::STRING, std::move(value), line, line, col - value.size(), col));
          ++it;
          col++;
          return;
        }
        tokens.push_back(newToken(ops[oneCharOp], std::move(oneCharOp), line, line, col, col + 1));
        col++;
        ++it;
        return;
      }
      throw std::runtime_error("Unknown operator: " + op);
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
        case '(':
        case ')':
        case '{':
        case '}':
        case '[':
        case ']':
          handleGrouping(*it);
          break;
        //TODO: Add support for ALL multi AND single character operators by adding their cases here.
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
        case '.':
        case ':':
        case ';':
        case ',':
        case '=':
        case '!':
        case '>':
        case '<':
        case '&':
        case '|':
        case '\'':
        case '"':
        case '\\':
        case '`':
          handleOperator(std::string(1, *it));
          break;
        default:
          // Check first if the next one or two chars form an operator
          {
            // 1) First check two-char operator
            if (std::distance(it, end) > 1) {
              std::string twoCharOp(it, it + 2);
              if (ops.find(twoCharOp) != ops.end()) {
                tokens.push_back(newToken(ops[twoCharOp], twoCharOp, line, line, col, col + 2));
                it += 2; col += 2;
                continue;
              }
            }
            // 2) Then check single-char operator
            std::string oneCharOp(1, *it);
            if (ops.find(oneCharOp) != ops.end()) {
              // Handle string literals if ' or "
              if (oneCharOp == "'" || oneCharOp == "\"") {
                std::string value;
                ++it;
                col++;
                while (it != end && *it != oneCharOp[0]) {
                  value.push_back(*it);
                  ++it;
                  col++;
                }
                tokens.push_back(newToken(lexer::TokenKind::STRING, std::move(value), line, line, col - value.size(), col));
                ++it;
                col++;
              } else {
                tokens.push_back(newToken(ops[oneCharOp], oneCharOp, line, line, col, col + 1));
                ++it; col++;
              }
              continue;
            }
          }

          // 3) If not operator, proceed with numeric or identifier parse
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
            tokens.push_back(newToken(lexer::TokenKind::NUMBER, std::move(num), line, line, col - num.size(), col));
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
              tokens.push_back(newToken(reserved_keywords[lower],
                                        std::move(value), line, line, col - value.size(), col));
            } else {
              tokens.push_back(newToken(lexer::TokenKind::IDENTIFIER,
                                        std::move(value), line, line, col - value.size(), col));
            }
          } else {
            throw std::runtime_error("Unknown token: " + std::string(1, *it));
          }
          break;
      }
    }
    tokens.push_back(newToken(lexer::TokenKind::EOF_, "", line, line, col, col));
    return tokens;
  }
}
