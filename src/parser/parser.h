#ifndef PARSER_H
#define PARSER_H

#include <vector>
#include <string>
#include <iostream>
#include <memory>
#include <unordered_map>
#include "../lexer/lexer.h"
#include "../ast/ast.h"
#include "../error/error.h"

namespace parser
{

  class Parser
  {
  public:
    Parser(const std::vector<lexer::Token> &tokens);
    std::unique_ptr<ast::Program> parse();

  private:
    const std::vector<lexer::Token> &tokens;
    size_t current = 0;

    std::unique_ptr<ast::Statement> parse_statement();
    std::unique_ptr<ast::Expression> parse_expression(int rbp = 0);
    std::unique_ptr<ast::Expression> parse_primary_expression();
    int get_binding_power(const lexer::TokenKind &kind);
    bool match(lexer::TokenKind kind);
    const lexer::Token &peek() const;
    lexer::Token advance();

    // TODO: Declare new parsing functions for additional language constructs.
    // Example:
    // std::unique_ptr<ast::Statement> parse_if_statement();
    // std::unique_ptr<ast::Statement> parse_for_statement();
    // std::unique_ptr<ast::Statement> parse_function_declaration();
    // std::unique_ptr<ast::Statement> parse_while_statement();
  };

} // namespace parser

#endif // PARSER_H
