#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <unordered_map>
#include <vector>

namespace lexer {
  enum TokenKind {
    //Literals
    NUMBER,
    IDENTIFIER,
    STRING,

    //Operators
    PLUS, // +
    MINUS, // -
    STAR, // *
    SLASH, // /
    PERCENT, // %
    TYPE_ARROW, // ->
    LAMBDA_ARROW, // :>
    DOT, // .
    DOT_DOT, // ..

    //Logical
    EQUAL, // ==
    NOT, // !
    NOT_EQUAL, // !=
    GREATER, // >
    GREATER_EQUAL, // >=
    LESS, // <
    LESS_EQUAL, // <=
    AND, // &&
    OR, // ||

    //Assignment
    ASSIGNMENT, // =
    PLUS_ASSIGNMENT, // +=
    MINUS_ASSIGNMENT, // -=
    STAR_ASSIGNMENT, // *=
    SLASH_ASSIGNMENT, // /=
    PERCENT_ASSIGNMENT, // %=

    //Grouping
    OPEN_PAREN, // (
    CLOSE_PAREN, // )
    OPEN_BRACE, // [
    CLOSE_BRACE, // ]
    OPEN_CURLY, // {
    CLOSE_CURLY, // }

    //Symbols
    COLON, // :
    SEMICOLON, // ;
    COMMA, // ,
    SINGLE_QUOTE, // '
    DOUBLE_QUOTE, // "
    DASH_DASH, // --
    BACK_SLASH, // \ //
    BACK_TICK, // `

    //Keywords
    PACKAGE, // package
    CONST, // const
    LET, // let
    STRUCT, // struct
    FN, // fn
    RETURN, // return
    IF, // if
    ELSE, // else
    THEN, // then
    CLASS, // class
    CONSTRUCTOR, // constructor
    OPERATOR, // operator
    CREATE, // create
    FOR, // for
    EXTEND, // extend
    PROP, // prop
    END, // end

    //Misc
    REGEXP, // / /
    EOF_, // End of file
  };

  struct Token {
    TokenKind kind;
    std::string value;
    int linestart;
    int lineend;
    int columnstart;
    int columnend;
    void debug() const;
  };

  extern std::unordered_map<std::string, lexer::TokenKind> ops;
  extern std::unordered_map<std::string, lexer::TokenKind> reserved_keywords;
  lexer::Token newToken(lexer::TokenKind kind, std::string value, int linestart, int lineend, int columnstart, int columnend);
  std::vector<lexer::Token> tokenize(std::string input);
  std::string tokenKindToString(TokenKind kind);
}

#endif // LEXER_H
