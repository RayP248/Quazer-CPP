#include "parser.h"
#include "../ast/ast.h"
#include "../global.h"
#include "../error/error.h"
#include <stdexcept>

namespace parser
{

  Parser::Parser(const std::vector<lexer::Token> &tokens) : tokens(tokens), current(0) {}

  std::unique_ptr<ast::Program> Parser::parse()
  {
    auto program = std::make_unique<ast::Program>();

    while (current < tokens.size() && tokens[current].kind != lexer::TokenKind::EOF_)
    {
      auto stmt = parse_statement();
      if (stmt)
      {
        program->body.push_back(stmt.release());
      }
    }

    return program;
  }

  std::unique_ptr<ast::Statement> Parser::parse_statement()
  {
    // TODO: Add handling for new statement types.
    // Example:
    // if (match(TokenKind::IF)) {
    //   return parse_if_statement();
    // }
    // else if (match(TokenKind::FOR)) {
    //   return parse_for_statement();
    // }
    // else if (match(TokenKind::FUNCTION)) {
    //   return parse_function_declaration();
    // }
    // else {
    //   // ...existing expression parsing...
    // }

    auto expr = parse_expression();
    if (!expr)
      return nullptr;

    auto exprStmt = std::make_unique<ast::ExpressionStatement>();
    exprStmt->expression = expr.release();

    // Set position information based on the expression
    exprStmt->linestart = exprStmt->expression->linestart;
    exprStmt->lineend = exprStmt->expression->lineend;
    exprStmt->columnstart = exprStmt->expression->columnstart;
    exprStmt->columnend = exprStmt->expression->columnend;

    return exprStmt;
  }

  // TODO: Implement parsing functions for new statements.
  // Example:
  // std::unique_ptr<ast::Statement> Parser::parse_if_statement() {
  //   // Parsing logic for 'if' statements
  // }
  //
  // std::unique_ptr<ast::Statement> Parser::parse_for_statement() {
  //   // Parsing logic for 'for' loops
  // }
  //
  // std::unique_ptr<ast::Statement> Parser::parse_function_declaration() {
  //   // Parsing logic for function declarations
  // }

  std::unique_ptr<ast::Expression> Parser::parse_expression(int rbp)
  {
    auto expr = parse_primary_expression();
    if (!expr)
      return nullptr;

    while (rbp < get_binding_power(peek().kind))
    {
      auto opToken = advance();
      int opBP = get_binding_power(opToken.kind);

      // TODO: Extend binding power and expression parsing for new operators if added.
      // Example:
      // if (opToken.kind == TokenKind::NEW_OPERATOR) {
      //   // Handle new operator precedence and associativity
      // }

      auto right = parse_expression(opBP);
      if (!right)
        return expr;

      auto binExpr = std::make_unique<ast::BinaryExpression>();
      binExpr->left = expr.release();
      binExpr->op = opToken;
      binExpr->right = right.release();

      // Set position from left's start to right's end
      binExpr->linestart = binExpr->left->linestart;
      binExpr->lineend = binExpr->right->lineend;
      binExpr->columnstart = binExpr->left->columnstart;
      binExpr->columnend = binExpr->right->columnend;

      expr = std::move(binExpr);
    }

    // Return the constructed expression.
    return expr;
  }

  // TODO: Implement helper functions needed for new functionality.
  // Example:
  // std::unique_ptr<ast::Statement> Parser::parse_while_statement() {
  //   // Parsing logic for 'while' loops
  // }

  std::unique_ptr<ast::Expression> Parser::parse_primary_expression()
  {
    const lexer::Token &tok = peek();

    switch (tok.kind)
    {
    case lexer::TokenKind::NUMBER:
    {
      advance();
      auto numExpr = std::make_unique<ast::NumberExpression>();
      numExpr->value = std::stod(tok.value);

      // Set position information from the token
      numExpr->linestart = tok.linestart;
      numExpr->lineend = tok.lineend;
      numExpr->columnstart = tok.columnstart;
      numExpr->columnend = tok.columnend;

      return numExpr;
    }
    // Handle other primary expressions like IDENTIFIER, STRING, etc.
    default:
      global::errors.emplace_back(
          error::ErrorCode::PARSER_ERROR,
          "Unexpected token: " + tok.value,
          tok.linestart, tok.lineend,
          tok.columnstart, tok.columnend,
          "parser.cpp : parse_primary_expression",
          error::ErrorImportance::CRITICAL);
      return nullptr;
    }
  }

  int Parser::get_binding_power(const lexer::TokenKind &kind)
  {
    switch (kind)
    {
    case lexer::TokenKind::PLUS:
    case lexer::TokenKind::MINUS:
      return 10;
    case lexer::TokenKind::STAR:
    case lexer::TokenKind::SLASH:
      return 20;
    default:
      return 0;
    }
  }

  bool Parser::match(lexer::TokenKind kind)
  {
    if (peek().kind == kind)
    {
      advance();
      return true;
    }
    return false;
  }

  const lexer::Token &Parser::peek() const
  {
    if (current < tokens.size())
    {
      return tokens[current];
    }
    static lexer::Token eofToken = lexer::newToken(lexer::TokenKind::EOF_, "", 0, 0, 0, 0);
    return eofToken;
  }

  lexer::Token Parser::advance()
  {
    if (current < tokens.size())
    {
      return tokens[current++];
    }
    return lexer::newToken(lexer::TokenKind::EOF_, "", 0, 0, 0, 0);
  }

} // namespace parser
