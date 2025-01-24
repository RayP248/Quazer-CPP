#ifndef PARSER_H
#define PARSER_H

#include <vector>
#include <unordered_map>
#include "../lexer/lexer.h"
#include "../ast/ast.h"

namespace parser
{
  //*--------------
  //*    PARSER
  //*--------------
  struct parser_;
  /*{
    std::vector<lexer::Token> tokens;
    size_t pos;
    lexer::Token current_token();
    lexer::TokenKind current_kind();
    lexer::Token advance();
    bool has_tokens();
    lexer::Token expect_error(lexer::TokenKind expected_kind, std::string error_message, std::string origin);
    lexer::Token expect(lexer::TokenKind expected_kind, std::string origin);
  };*/

  parser::parser_ create_parser_(const std::vector<lexer::Token> &tokens);
  ast::Program parse(const std::vector<lexer::Token> &tokens);

  //*---------------
  //*    LOOKUPS
  //*---------------
  enum BindingPower
  {
    DEFAULT,
    COMMA,
    ASSIGNMENT,
    LOGICAL,
    RELATIONAL,
    ADDITIVE,
    MULTIPLICATIVE,
    UNARY,
    CALL,
    MEMBER,
    PRIMARY
  };
  typedef ast::Statement *statement_handler(parser::parser_ &parser);
  typedef ast::Expression *nud_handler(parser::parser_ &parser);
  typedef ast::Expression *led_handler(parser::parser_ &parser, ast::Expression *left, parser::BindingPower binding_power);

  typedef std::unordered_map<lexer::TokenKind, statement_handler *> statement_lookup;
  typedef std::unordered_map<lexer::TokenKind, nud_handler *> nud_lookup;
  typedef std::unordered_map<lexer::TokenKind, led_handler *> led_lookup;
  typedef std::unordered_map<lexer::TokenKind, BindingPower *> binding_power_lookup;

  extern statement_lookup statement_lu;
  extern nud_lookup nud_lu;
  extern led_lookup led_lu;
  extern binding_power_lookup binding_power_lu;

  void led(lexer::TokenKind kind, BindingPower binding_power, led_handler *handler);
  void nud(lexer::TokenKind kind, nud_handler *handler);
  void statement(lexer::TokenKind kind, statement_handler *handler);

  void create_token_lookups();

  //*--------------------
  //*    TYPE LOOKUPS
  //*--------------------
  // TODO: Implement when needed

  //*------------------
  //*    STATEMENTS
  //*------------------
  ast::Statement *parse_statement(parser::parser_ &parser);

  //*-------------------
  //*    EXPRESSIONS
  //*-------------------
  ast::Expression *parse_expression(parser::parser_ &parser, BindingPower binding_power);
  ast::Expression *parse_primary_expression(parser::parser_ &parser);
  std::unordered_map<std::string, ast::Expression *> parse_properties(parser::parser_ &parser, lexer::Token &closecurly);
  ast::Expression *parse_binary_expression(parser::parser_ &parser, ast::Expression *left, parser::BindingPower binding_power);
}

#endif // PARSER_H
