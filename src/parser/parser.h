#ifndef PARSER_H
#define PARSER_H

#include <vector>
#include <unordered_map>
#include "../lexer/lexer.h"
#include "../ast/ast.h"
#include "../interpreter/interpreter.h"
#include <iostream> // Add for debugging

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

  //*---------------
  //*    LOOKUPS
  //*---------------
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
  typedef ast::Type *type_nud_handler(parser::parser_ &parser);
  typedef ast::Type *type_led_handler(parser::parser_ &parser, ast::Type *left, parser::BindingPower binding_power);

  typedef std::unordered_map<lexer::TokenKind, type_nud_handler *> type_nud_lookup;
  typedef std::unordered_map<lexer::TokenKind, type_led_handler *> type_led_lookup;
  typedef std::unordered_map<lexer::TokenKind, BindingPower *> type_binding_power_lookup;

  extern type_nud_lookup type_nud_lu;
  extern type_led_lookup type_led_lu;
  extern type_binding_power_lookup type_binding_power_lu;

  void type_led(lexer::TokenKind kind, BindingPower binding_power, type_led_handler *handler);
  void type_nud(lexer::TokenKind kind, type_nud_handler *handler);

  void create_token_type_lookups();

  ast::Type *parse_symbol_type(parser::parser_ &parser);
  ast::Type parse_type(parser::parser_ &parser, BindingPower binding_power);
  ast::Type *parse_array_type(parser_ &parser);

  //*------------------
  //*    STATEMENTS
  //*------------------
  ast::Statement *parse_statement(parser::parser_ &parser);
  ast::Statement *parse_variable_declaration_statement(parser_ &parser);
  ast::Statement *parse_function_declaration_statement(parser_ &parser);
  ast::Statement *parse_block_statement(parser_ &parser);
  ast::Statement *parse_return_statement(parser_ &parser);
  ast::Statement *parse_if_statement(parser_ &parser);
  ast::Statement *parse_for_loop_statement(parser_ &parser);

  //*-------------------
  //*    EXPRESSIONS
  //*-------------------
  ast::Expression *parse_expression(parser::parser_ &parser, BindingPower binding_power);
  ast::Expression *parse_primary_expression(parser::parser_ &parser);
  ast::Expression *parse_binary_expression(parser::parser_ &parser, ast::Expression *left, parser::BindingPower binding_power);
  ast::Expression *parse_call_expression(parser::parser_ &parser, ast::Expression *left, parser::BindingPower binding_power);
  ast::Expression *parse_assignment_expression(parser::parser_ &parser, ast::Expression *left, parser::BindingPower binding_power);
  ast::Expression *parse_variable_declaration_expression(parser_ &parser);
  ast::Expression *parse_object_expression(parser::parser_ &parser);
  ast::Expression *parse_member_expression(parser::parser_ &parser, ast::Expression *left, BindingPower binding_power);

  //*---------------
  //*    HELPERS
  //*---------------
  std::vector<ast::Expression *> parse_args(parser_ &parser);
  std::vector<ast::ParameterExpression *> parse_parameters(parser_ &parser);
  std::vector<std::pair<ast::Expression *, ast::Expression *>> parse_properties(parser_ &parser);
  ast::Expression *runtime_to_ast(interpreter::runtime_value *value);
}

#endif // PARSER_H
