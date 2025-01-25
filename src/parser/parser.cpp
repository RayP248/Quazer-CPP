#include "parser.h"
#include "../error/error.h"

namespace parser
{
  //*--------------
  //*    PARSER
  //*--------------
  struct parser_
  {
    std::vector<lexer::Token> tokens;
    size_t pos;

    parser_(const std::vector<lexer::Token> &t) : tokens(t), pos(0) {}

    bool has_tokens() const
    {
      return pos < tokens.size();
    }

    lexer::Token advance()
    {
      if (has_tokens())
      {
        lexer::Token token = current_token();
        pos++;
        return token;
      }
      error::Error(error::ErrorCode::PARSER_ERROR, "Unexpected end of input", 0, 0, 0, 0, "parser.cpp : advance : if statement", error::ErrorImportance::CRITICAL);
      return lexer::Token();
    }

    lexer::Token current_token()
    {
      return tokens[pos];
    }
    lexer::TokenKind current_kind()
    {
      return current_token().kind;
    }
    lexer::Token expect_error(lexer::TokenKind expected_kind, std::string error_message, std::string origin)
    {
      lexer::Token token = current_token();
      lexer::TokenKind kind = token.kind;

      if (kind != expected_kind)
      {
        if (error_message != "")
        {
          error::Error err(
              error::ErrorCode::PARSER_ERROR,
              error_message,
              token.linestart,
              token.lineend,
              token.columnstart,
              token.columnend,
              origin,
              error::ErrorImportance::HIGH);
        }
        else
        {
          error::Error err(
              error::ErrorCode::PARSER_ERROR,
              "Expected '" + token.value + ", got " + lexer::token_kind_to_string(kind),
              token.linestart,
              token.lineend,
              token.columnstart,
              token.columnend,
              origin,
              error::ErrorImportance::HIGH);
        }
      }
      advance();
      return token;
    }
    lexer::Token expect(lexer::TokenKind expected_kind, std::string origin)
    {
      return expect_error(expected_kind, "", origin);
    }
  };

  parser_ create_parser_(const std::vector<lexer::Token> &tokens)
  {
    create_token_lookups();
    return parser_(tokens);
  }

  ast::Program parse(const std::vector<lexer::Token> &tokens)
  {
    // DEBUG**std::cerr << "[parse] Starting parse with " << tokens.size() << " tokens\n";
    parser_ p = create_parser_(tokens);
    ast::Program *program = new ast::Program();

    while (p.has_tokens())
    {
      if (p.current_kind() == lexer::TokenKind::EOF_)
        break;

      // std::cerr << "[parse] Parsing statement at position " << p.pos << "\n";
      auto stmt = parse_statement(p);
      if (stmt)
        program->body.push_back(stmt);
      else
        std::cerr << "[parse] Received null statement\n";
    }

    // DEBUG**std::cerr << "[parse] Finished creating Program node with " << program.body.size() << " statements\n";
    program->linestart = tokens.front().linestart;
    program->lineend = tokens.back().lineend;
    program->columnstart = tokens.front().columnstart;
    program->columnend = tokens.back().columnend;
    program->kind = ast::StatementKind::PROGRAM;
    return *program;
  }

  //*---------------
  //*    LOOKUPS
  //*---------------
  statement_lookup statement_lu;
  nud_lookup nud_lu;
  led_lookup led_lu;
  binding_power_lookup binding_power_lu;

  void led(lexer::TokenKind kind, BindingPower binding_power, led_handler *handler)
  {
    led_lu[kind] = handler;
    binding_power_lu[kind] = new BindingPower(binding_power);
  }

  void nud(lexer::TokenKind kind, nud_handler *handler)
  {
    nud_lu[kind] = handler;
  }

  void statement(lexer::TokenKind kind, statement_handler *handler)
  {
    statement_lu[kind] = handler;
    binding_power_lu[kind] = new BindingPower(DEFAULT);
  }

  void create_token_lookups()
  {
    // Logical
    led(lexer::TokenKind::AND, LOGICAL, parse_binary_expression);
    led(lexer::TokenKind::OR, LOGICAL, parse_binary_expression);

    // RELATIONAL
    led(lexer::TokenKind::LESS, BindingPower::RELATIONAL, parse_binary_expression);
    led(lexer::TokenKind::LESS_EQUAL, BindingPower::RELATIONAL, parse_binary_expression);
    led(lexer::TokenKind::GREATER, BindingPower::RELATIONAL, parse_binary_expression);
    led(lexer::TokenKind::GREATER_EQUAL, BindingPower::RELATIONAL, parse_binary_expression); // Fixed function name
    led(lexer::TokenKind::EQUAL, BindingPower::RELATIONAL, parse_binary_expression);
    led(lexer::TokenKind::NOT_EQUAL, BindingPower::RELATIONAL, parse_binary_expression);

    // ADDITIVE & Multiplicative
    led(lexer::TokenKind::PLUS, BindingPower::ADDITIVE, parse_binary_expression);
    led(lexer::TokenKind::MINUS, BindingPower::ADDITIVE, parse_binary_expression);
    led(lexer::TokenKind::STAR, BindingPower::MULTIPLICATIVE, parse_binary_expression);
    led(lexer::TokenKind::SLASH, BindingPower::MULTIPLICATIVE, parse_binary_expression);
    led(lexer::TokenKind::PERCENT, BindingPower::MULTIPLICATIVE, parse_binary_expression);

    // Literals & Symbols
    nud(lexer::TokenKind::NUMBER, parse_primary_expression);
    nud(lexer::TokenKind::STRING, parse_primary_expression);
    nud(lexer::TokenKind::IDENTIFIER, parse_primary_expression);

    // Parentheses
    nud(lexer::TokenKind::OPEN_PAREN, [](parser_ &parser)
        {
      lexer::Token open_paren = parser.expect(lexer::TokenKind::OPEN_PAREN, "parser.cpp : parse_primary_expression : Expected '('");
      ast::Expression *expr = parse_expression(parser, DEFAULT);
      lexer::Token close_paren = parser.expect(lexer::TokenKind::CLOSE_PAREN, "parser.cpp : parse_primary_expression : Expected ')'");
      return expr; });

    // Assign a default or primary binding power for these tokens
    binding_power_lu[lexer::TokenKind::NUMBER] = new BindingPower(PRIMARY);
    binding_power_lu[lexer::TokenKind::STRING] = new BindingPower(PRIMARY);
    binding_power_lu[lexer::TokenKind::IDENTIFIER] = new BindingPower(PRIMARY);
    binding_power_lu[lexer::TokenKind::OPEN_PAREN] = new BindingPower(PRIMARY);

    // Add EOF to avoid uninitialized lookups
    // Use BindingPower::DEFAULT or the lowest power
    binding_power_lu[lexer::TokenKind::EOF_] = new BindingPower(DEFAULT);
  }

  //*--------------------
  //*    TYPE LOOKUPS
  //*--------------------
  // TODO: Implement when needed

  //*------------------
  //*    STATEMENTS
  //*------------------
  ast::Statement *parse_statement(parser_ &parser)
  {
    // DEBUG**std::cerr << "[parse_statement] Current token kind: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
    auto statement_handler = statement_lu[parser.current_kind()];

    if (statement_handler != nullptr)
    {
      return statement_handler(parser); // Return pointer directly
    }

    ast::Expression *expression = parse_expression(parser, DEFAULT);
    parser.expect(lexer::TokenKind::SEMICOLON, "parser.cpp : parse_statement() : Expected ';' after expression");

    // DEBUG**std::cerr << "[parse_statement] Creating ExpressionStatement\n";
    ast::ExpressionStatement *exprstmt = new ast::ExpressionStatement();
    exprstmt->expression = expression;
    exprstmt->linestart = expression->linestart;
    exprstmt->lineend = expression->lineend;
    exprstmt->columnstart = expression->columnstart;
    exprstmt->columnend = expression->columnend;
    exprstmt->kind = ast::StatementKind::EXPRESSION_STATEMENT;
    return exprstmt;
  }

  //*-------------------
  //*    EXPRESSIONS
  //*-------------------
  ast::Expression *parse_expression(parser::parser_ &parser, BindingPower binding_power)
  {
    // DEBUG**std::cerr << "[parse_expression] Start with token kind: " << lexer::token_kind_to_string(parser.current_kind())
    // DEBUG**<< ", binding_power: " << binding_power << "\n";

    // If current token is not found in the binding_power_lu, break
    if (!binding_power_lu.count(parser.current_kind()))
    {
      std::cerr << "[parse_expression] No binding power found for token kind: "
                << lexer::token_kind_to_string(parser.current_kind())
                << ". Exiting.\n";
      return nullptr;
    }

    lexer::TokenKind token_kind = parser.current_kind();
    auto nud_handler = nud_lu[token_kind];

    if (nud_handler == nullptr)
    {
      error::Error err(
          error::ErrorCode::PARSER_ERROR,
          "No NUD function found for token of kind '" + lexer::token_kind_to_string(token_kind) + "'",
          parser.current_token().linestart,
          parser.current_token().lineend,
          parser.current_token().columnstart,
          parser.current_token().columnend,
          "parser.cpp : parse_expression()",
          error::ErrorImportance::HIGH);
      return nullptr; // Avoid calling a null pointer
    }

    ast::Expression *left = nud_handler(parser);

    while (binding_power_lu.count(parser.current_kind()) &&
           *binding_power_lu[parser.current_kind()] > binding_power)
    {
      // DEBUG**std::cerr << "[parse_expression] In while loop, token kind: " << lexer::token_kind_to_string(parser.current_kind())
      // DEBUG**<< ", next binding_power: " << *binding_power_lu[parser.current_kind()] << "\n";
      token_kind = parser.current_kind();
      auto led_handler = led_lu[token_kind];
      if (led_handler == nullptr)
      {
        error::Error err(
            error::ErrorCode::PARSER_ERROR,
            "No LED function found for token of kind '" + lexer::token_kind_to_string(token_kind) + "'",
            parser.current_token().linestart,
            parser.current_token().lineend,
            parser.current_token().columnstart,
            parser.current_token().columnend,
            "parser.cpp : parse_expression() : while loop",
            error::ErrorImportance::HIGH);
        return left; // Just return what we have
      }

      left = led_handler(parser, left, *binding_power_lu[parser.current_kind()]);
    }

    // DEBUG**std::cerr << "[parse_expression] Returning expression of kind: " << (left ? left->kind : "null expression") << "\n";
    return left;
  }
  ast::Expression *parse_primary_expression(parser::parser_ &parser)
  {
    // DEBUG**std::cerr << "[parse_primary_expression] Current token kind: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
    switch (parser.current_kind())
    {
    case lexer::TokenKind::NUMBER:
    {
      auto token = parser.current_token();
      ast::NumberExpression *literal = new ast::NumberExpression(std::stod(token.value));
      // DEBUG**std::cout << "[parse_primary_expression] Creating NumberExpression with value: " << parser.current_token().value << "\n";
      literal->linestart = parser.current_token().linestart;
      literal->lineend = parser.current_token().lineend;
      literal->columnstart = parser.current_token().columnstart;
      literal->columnend = parser.current_token().columnend;
      literal->kind = ast::StatementKind::NUMBER_EXPRESSION;
      parser.advance();
      return literal;
    }
    case lexer::TokenKind::STRING:
    {
      ast::StringExpression *literal = new ast::StringExpression();
      literal->value = parser.current_token().value;
      literal->linestart = parser.current_token().linestart;
      literal->lineend = parser.current_token().lineend;
      literal->columnstart = parser.current_token().columnstart;
      literal->columnend = parser.current_token().columnend;
      literal->kind = ast::StatementKind::STRING_EXPRESSION;
      parser.advance();
      return literal;
    }
    case lexer::TokenKind::IDENTIFIER:
    {
      auto token = parser.current_token();
      ast::SymbolExpression *identifier = new ast::SymbolExpression(token.value);
      identifier->linestart = parser.current_token().linestart;
      identifier->lineend = parser.current_token().lineend;
      identifier->columnstart = parser.current_token().columnstart;
      identifier->columnend = parser.current_token().columnend;
      identifier->kind = ast::StatementKind::SYMBOL_EXPRESSION;
      parser.advance();
      return identifier;
    }
    default:
    {
      // DEBUG**std::cerr << "[parse_primary_expression] Returning null expression from default case\n";
      error::Error err(
          error::ErrorCode::PARSER_ERROR,
          "Cannot create primary expression from token of kind '" + lexer::token_kind_to_string(parser.current_kind()) + "'",
          parser.current_token().linestart,
          parser.current_token().lineend,
          parser.current_token().columnstart,
          parser.current_token().columnend,
          "parser.cpp : parse_primary_expression() : switch : default case",
          error::ErrorImportance::HIGH);
      return nullptr;
    }
    }
  }
  ast::Expression *parse_binary_expression(parser::parser_ &parser, ast::Expression *left, BindingPower binding_power)
  {
    // DEBUG**std::cerr << "[parse_binary_expression] Left expression kind: " << (left ? left->kind : "null")
    // DEBUG**<< ", operator token: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
    lexer::Token op = parser.advance();
    ast::Expression *right = parse_expression(parser, binding_power);

    ast::BinaryExpression *binary_expr = new ast::BinaryExpression();
    binary_expr->left = left;
    binary_expr->op = op;
    binary_expr->right = right;
    binary_expr->linestart = left->linestart;
    binary_expr->lineend = right->lineend;
    binary_expr->columnstart = left->columnstart;
    binary_expr->columnend = right->columnend;
    binary_expr->kind = ast::StatementKind::BINARY_EXPRESSION;

    // DEBUG**std::cerr << "[parse_binary_expression] Returning BinaryExpression\n";
    return binary_expr;
  }
}
