#include "parser.h"
#include "../error/error.h"
#include <memory>
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
              "Expected '" + lexer::token_kind_to_symbol(expected_kind) + "', got " + "'" + token.value + "' (" + lexer::token_kind_to_symbol(kind) + ")",
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
    create_token_type_lookups();
    parser::parser_ p(tokens);
    p.tokens = tokens;
    p.pos = 0;
    return p;
  }

  ast::Program parse(const std::vector<lexer::Token> &tokens)
  {
    // [DEBUG**] std::cerr << "[parse] Starting parse with " << tokens.size() << " tokens\n";
    parser_ p = create_parser_(tokens);
    ast::Program *program = new ast::Program();

    while (p.has_tokens())
    {
      if (p.current_kind() == lexer::TokenKind::EOF_)
        break;

      std::cerr << "[parse] Parsing statement at position " << p.pos << "\n";
      ast::Statement *stmt = nullptr;
      try
      {
        stmt = parse_statement(p);
      }
      catch (const std::exception &err)
      {
        std::cerr << "[parse] Error: " << err.what() << "\n";
        break;
      }
      std::cerr << "[parse] Statement parsed\n";
      if (stmt)
        program->body.push_back(stmt);
      else
        std::cerr << "[parse] Received null statement\n";
    }

    // [DEBUG**] std::cerr << "[parse] Finished creating Program node with " << program->body.size() << " statements\n";
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
    binding_power_lu[kind] = new BindingPower(PRIMARY);
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

    // Relational
    led(lexer::TokenKind::LESS, BindingPower::RELATIONAL, parse_binary_expression);
    led(lexer::TokenKind::LESS_EQUAL, BindingPower::RELATIONAL, parse_binary_expression);
    led(lexer::TokenKind::GREATER, BindingPower::RELATIONAL, parse_binary_expression);
    led(lexer::TokenKind::GREATER_EQUAL, BindingPower::RELATIONAL, parse_binary_expression);
    led(lexer::TokenKind::EQUAL, BindingPower::RELATIONAL, parse_binary_expression);
    led(lexer::TokenKind::NOT_EQUAL, BindingPower::RELATIONAL, parse_binary_expression);

    // Additive & Multiplicative
    led(lexer::TokenKind::PLUS, BindingPower::ADDITIVE, parse_binary_expression);
    led(lexer::TokenKind::MINUS, BindingPower::ADDITIVE, parse_binary_expression);
    led(lexer::TokenKind::STAR, BindingPower::MULTIPLICATIVE, parse_binary_expression);
    led(lexer::TokenKind::SLASH, BindingPower::MULTIPLICATIVE, parse_binary_expression);
    led(lexer::TokenKind::PERCENT, BindingPower::MULTIPLICATIVE, parse_binary_expression);

    // Assignment
    led(lexer::TokenKind::ASSIGNMENT, BindingPower::ASSIGNMENT, parse_assignment_expression);
    led(lexer::TokenKind::PLUS_ASSIGNMENT, BindingPower::ASSIGNMENT, parse_assignment_expression);
    led(lexer::TokenKind::MINUS_ASSIGNMENT, BindingPower::ASSIGNMENT, parse_assignment_expression);
    led(lexer::TokenKind::STAR_ASSIGNMENT, BindingPower::ASSIGNMENT, parse_assignment_expression);
    led(lexer::TokenKind::SLASH_ASSIGNMENT, BindingPower::ASSIGNMENT, parse_assignment_expression);
    led(lexer::TokenKind::PERCENT_ASSIGNMENT, BindingPower::ASSIGNMENT, parse_assignment_expression);
    led(lexer::TokenKind::PLUS_PLUS, BindingPower::ASSIGNMENT, parse_assignment_expression);
    led(lexer::TokenKind::MINUS_MINUS, BindingPower::ASSIGNMENT, parse_assignment_expression);

    // Member
    led(lexer::TokenKind::DOT, MEMBER, parse_member_expression);
    led(lexer::TokenKind::OPEN_SQUARE, MEMBER, parse_member_expression);

    // Literals & Symbols
    nud(lexer::TokenKind::NUMBER, parse_primary_expression);
    nud(lexer::TokenKind::STRING, parse_primary_expression);
    nud(lexer::TokenKind::IDENTIFIER, parse_primary_expression);
    nud(lexer::TokenKind::OPEN_SQUARE, parse_primary_expression);
    nud(lexer::TokenKind::OPEN_CURLY, parse_object_expression);
    led(lexer::TokenKind::IDENTIFIER, DEFAULT, parse_binary_expression);

    // Parentheses
    nud(lexer::TokenKind::OPEN_PAREN, [](parser_ &parser)
        {
      lexer::Token open_paren = parser.expect(lexer::TokenKind::OPEN_PAREN, "parser.cpp : parse_primary_expression : Expected '('");
      ast::Expression *expr = parse_expression(parser, DEFAULT);
      lexer::Token close_paren = parser.expect(lexer::TokenKind::CLOSE_PAREN, "parser.cpp : parse_primary_expression : Expected ')'");
      return expr; });
    led(lexer::TokenKind::OPEN_PAREN, CALL, parse_call_expression);

    // Statement Expressions
    nud(lexer::TokenKind::LET, parse_variable_declaration_expression);
    nud(lexer::TokenKind::CONST, parse_variable_declaration_expression);

    // Statements
    statement(lexer::TokenKind::LET, parse_variable_declaration_statement);
    statement(lexer::TokenKind::CONST, parse_variable_declaration_statement);
    statement(lexer::TokenKind::PUBLIC, parse_variable_declaration_statement);
    statement(lexer::TokenKind::FN, parse_function_declaration_statement);
    statement(lexer::TokenKind::RETURN, parse_return_statement);
    statement(lexer::TokenKind::IF, parse_if_statement);
    statement(lexer::TokenKind::FOR, parse_for_loop_statement);

    binding_power_lu[lexer::TokenKind::EOF_] = new BindingPower(DEFAULT);

    // So the parser doesn't complain when it sees '{' after an expression:
    led(lexer::TokenKind::OPEN_CURLY, BindingPower::DEFAULT, [](parser_ &parser, ast::Expression *left, BindingPower)
        {
      // Ignore '{' as an infix operator; return left to avoid error.
      return left ? left : new ast::DummyExpression(); });
  }

  //*--------------------
  //*    TYPE LOOKUPS
  //*--------------------
  type_nud_lookup type_nud_lu;
  type_led_lookup type_led_lu;
  type_binding_power_lookup type_binding_power_lu;

  void type_led(lexer::TokenKind kind, BindingPower binding_power, type_led_handler *handler)
  {
    type_led_lu[kind] = handler;
    type_binding_power_lu[kind] = new BindingPower(binding_power);
  }

  void type_nud(lexer::TokenKind kind, type_nud_handler *handler)
  {
    type_nud_lu[kind] = handler;
    type_binding_power_lu[kind] = new BindingPower(PRIMARY);
  }

  void create_token_type_lookups()
  {
    type_nud(lexer::TokenKind::IDENTIFIER, parse_symbol_type);
  }

  ast::Type *parse_symbol_type(parser_ &parser)
  {
    lexer::Token token = parser.expect(lexer::TokenKind::IDENTIFIER, "parser.cpp : parse_symbol_type() : Expected type name");
    ast::Type *type = new ast::Type();
    type->name = token.value;
    return type;
  }

  ast::Type parse_type(parser_ &parser, BindingPower binding_power)
  {
    lexer::TokenKind token_kind = parser.current_kind();
    auto nud_handler = type_nud_lu[token_kind];
    if (nud_handler == nullptr)
    {
      error::Error err(
          error::ErrorCode::PARSER_ERROR,
          "No type_NUD function found for token of kind '" + lexer::token_kind_to_string(token_kind) + "'",
          parser.current_token().linestart,
          parser.current_token().lineend,
          parser.current_token().columnstart,
          parser.current_token().columnend,
          "parser.cpp : parse_type()",
          error::ErrorImportance::HIGH);
      return ast::Type();
    }
    auto left = nud_handler(parser);
    while (type_binding_power_lu.count(parser.current_kind()) > binding_power)
    {
      token_kind = parser.current_kind();
      auto led_handler = type_led_lu[token_kind];
      if (led_handler == nullptr)
      {
        error::Error err(
            error::ErrorCode::PARSER_ERROR,
            "No type_LED function found for token of kind '" + lexer::token_kind_to_string(token_kind) + "'",
            parser.current_token().linestart,
            parser.current_token().lineend,
            parser.current_token().columnstart,
            parser.current_token().columnend,
            "parser.cpp : parse_type() : while loop",
            error::ErrorImportance::HIGH);
        return *left;
      }
      left = led_handler(parser, left, *type_binding_power_lu[parser.current_kind()]);
    }

    return *left;
  }

  //*------------------
  //*    STATEMENTS
  //*------------------
  ast::Statement *parse_statement(parser_ &parser)
  {
    // [DEBUG**] std::cerr << "[parse_statement] Current token kind: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
    auto statement_handler = statement_lu[parser.current_kind()];
    // [DEBUG**] std::cerr << "[parse_statement] Statement handler is " << (statement_handler ? "available" : "null") << "\n";

    if (statement_handler != nullptr)
    {
      // [DEBUG**] std::cerr << "[parse_statement] Found statement handler for token kind: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
      return statement_handler(parser); // Return pointer directly
    }

    // [DEBUG**] std::cerr << "[parse_statement] No statement handler found for token kind: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";

    ast::Expression *expression = parse_expression(parser, DEFAULT);
    // [DEBUG**] std::cerr << "[parse_statement] Expression parsed\n";
    parser.expect(lexer::TokenKind::SEMICOLON, "parser.cpp : parse_statement() : Expected ';' after expression");
    // [DEBUG**] std::cerr << "[parse_statement] Semicolon parsed\n";

    // [DEBUG**] std::cerr << "[parse_statement] Creating ExpressionStatement\n";
    ast::ExpressionStatement *exprstmt = new ast::ExpressionStatement();
    // [DEBUG**] std::cerr << "[parse_statement] ExpressionStatement created\n";
    exprstmt->expression = expression;
    // [DEBUG**] std::cerr << "[parse_statement] Expression assigned to ExpressionStatement\n";
    exprstmt->linestart = expression->linestart;
    // [DEBUG**] std::cerr << "[parse_statement] Linestart assigned to ExpressionStatement\n";
    exprstmt->lineend = expression->lineend;
    // [DEBUG**] std::cerr << "[parse_statement] Lineend assigned to ExpressionStatement\n";
    exprstmt->columnstart = expression->columnstart;
    // [DEBUG**] std::cerr << "[parse_statement] Columnstart assigned to ExpressionStatement\n";
    exprstmt->columnend = expression->columnend;
    // [DEBUG**] std::cerr << "[parse_statement] Columnend assigned to ExpressionStatement\n";
    exprstmt->kind = ast::StatementKind::EXPRESSION_STATEMENT;
    // [DEBUG**] std::cerr << "[parse_statement] Statement kind set to ExpressionStatement\n";
    return exprstmt;
  }
  ast::Statement *parse_variable_declaration_statement(parser_ &parser)
  {
    ast::VariableDeclarationStatement *var_decl = new ast::VariableDeclarationStatement();
    lexer::Token keyword = parser.current_token();
    ast::Type type;
    std::vector<ast::Expression *> fields;
    ast::Expression *value = nullptr;
    bool is_const = false;
    bool is_public = false;

    if (keyword.kind == lexer::TokenKind::CONST)
    {
      is_const = true;
      parser.advance();
    }
    else if (keyword.kind == lexer::TokenKind::PUBLIC)
    {
      is_public = true;
      parser.advance();
      if (parser.current_kind() == lexer::TokenKind::CONST)
      {
        is_const = true;
        parser.advance();
      }
      else
      {
        parser.expect(lexer::TokenKind::LET, "parser.cpp : parse_variable_declaration_statement() : Expected 'let' after 'public'");
      }
    }
    else
    {
      parser.advance();
    }

    lexer::Token identifier = parser.expect(lexer::TokenKind::IDENTIFIER, "parser.cpp : parse_variable_declaration_statement() : Expected identifier after 'let' or 'const'");

    if (parser.current_kind() == lexer::TokenKind::COLON)
    {
      parser.advance();
      type = parse_type(parser, DEFAULT);
    }

    if (parser.current_kind() == lexer::TokenKind::ASSIGNMENT)
    {
      parser.advance();
      value = parse_expression(parser, ASSIGNMENT);
    }
    else
    {
      // Removed the redundant check that threw "Expected ';', got..."
      // when we already confirm with parser.expect below.
    }
    lexer::Token semicolon = parser.expect(
        lexer::TokenKind::SEMICOLON,
        "parser.cpp : parse_variable_declaration_statement() : Expected ';' after variable declaration");

    if (is_const && value == nullptr)
    {
      error::Error err(
          error::ErrorCode::PARSER_ERROR,
          "Constant ('const') variable declaration requires an assignment.",
          keyword.linestart,
          keyword.lineend,
          keyword.columnstart,
          keyword.columnend,
          "parser.cpp : parse_variable_declaration_statement()",
          error::ErrorImportance::HIGH);
      return new ast::DummyExpression();
    }

    var_decl->kind = ast::StatementKind::VARIABLE_DECLARATION_STATEMENT;
    var_decl->name = identifier.value;
    var_decl->value = value;
    var_decl->type = type;
    var_decl->is_const = is_const;
    var_decl->is_public = is_public;
    var_decl->linestart = keyword.linestart;
    var_decl->lineend = semicolon.lineend;
    var_decl->columnstart = keyword.columnstart;
    var_decl->columnend = semicolon.columnend;

    return var_decl;
  }
  ast::Statement *parse_function_declaration_statement(parser_ &parser)
  {
    ast::FunctionDeclarationStatement *fn_decl = new ast::FunctionDeclarationStatement();
    // [DEBUG**] std::cout << "  [parse_function_declaration_statement] current token: " << parser.current_token().value << std::endl;
    lexer::Token keyword = parser.expect(lexer::TokenKind::FN, "parser.cpp : parse_function_declaration_statement() : Expected 'fn' keyword");
    // [DEBUG**] std::cout << "  [parse_function_declaration_statement] current token: " << parser.current_token().value << std::endl;
    lexer::Token identifier = parser.expect(lexer::TokenKind::IDENTIFIER, "parser.cpp : parse_function_declaration_statement() : Expected function name after 'fn' keyword");

    // [DEBUG**] std::cout << "  [parse_function_declaration_statement] current token: " << parser.current_token().value << std::endl;
    parser.expect(lexer::TokenKind::OPEN_PAREN, "parser.cpp : parse_function_declaration_statement() : Expected '(' after function name");
    // [DEBUG**] std::cout << "  [parse_function_declaration_statement] current token: " << parser.current_token().value << std::endl;
    std::vector<ast::ParameterExpression *> parameters = parse_parameters(parser);
    parser.expect(lexer::TokenKind::CLOSE_PAREN, "parser.cpp : parse_function_declaration_statement() : Expected ')' after function parameters");

    if (parser.current_kind() == lexer::TokenKind::TYPE_ARROW)
    {
      parser.advance();
      fn_decl->return_type = parse_type(parser, DEFAULT);
    }

    ast::BlockStatement *block = dynamic_cast<ast::BlockStatement *>(parse_block_statement(parser));
    fn_decl->kind = ast::StatementKind::FUNCTION_DECLARATION_STATEMENT;
    fn_decl->name = identifier.value;
    fn_decl->parameters = parameters;
    fn_decl->body = block;
    fn_decl->linestart = keyword.linestart;
    fn_decl->lineend = block->lineend;
    fn_decl->columnstart = keyword.columnstart;
    fn_decl->columnend = block->columnend;

    return fn_decl;
  }
  ast::Statement *parse_block_statement(parser_ &parser)
  {
    lexer::Token open_curly = parser.expect(lexer::TokenKind::OPEN_CURLY, "parser.cpp : parse_block_statement() : Expected '{' to start block statement");
    ast::BlockStatement *block = new ast::BlockStatement();
    block->linestart = open_curly.linestart;
    block->columnstart = open_curly.columnstart;

    ast::ReturnStatement *return_stmt = nullptr;

    while (parser.current_kind() != lexer::TokenKind::CLOSE_CURLY)
    {
      ast::Statement *stmt = parse_statement(parser);
      if (stmt->kind == ast::StatementKind::RETURN_STATEMENT)
      {
        if (return_stmt != nullptr)
        {
          error::Error err(
              error::ErrorCode::PARSER_ERROR,
              "Multiple return statements in a body are not allowed.",
              stmt->linestart,
              stmt->lineend,
              stmt->columnstart,
              stmt->columnend,
              "parser.cpp : parse_block_statement()",
              error::ErrorImportance::MODERATE);
          return_stmt = dynamic_cast<ast::ReturnStatement *>(stmt);
        }
        else
        {
          return_stmt = dynamic_cast<ast::ReturnStatement *>(stmt);
        }
      }
      block->body.push_back(stmt);
    }

    lexer::Token close_curly = parser.expect(lexer::TokenKind::CLOSE_CURLY, "parser.cpp : parse_block_statement() : Expected '}' to end block statement");
    block->kind = ast::StatementKind::BLOCK_STATEMENT;
    block->lineend = close_curly.lineend;
    block->columnend = close_curly.columnend;
    block->return_statement = return_stmt;
    return block;
  }
  ast::Statement *parse_return_statement(parser_ &parser)
  {
    lexer::Token keyword = parser.expect(lexer::TokenKind::RETURN, "parser.cpp : parse_return_statement() : Expected 'return' keyword");
    ast::ReturnStatement *return_stmt = new ast::ReturnStatement();
    return_stmt->linestart = keyword.linestart;
    return_stmt->columnstart = keyword.columnstart;

    if (parser.current_kind() != lexer::TokenKind::SEMICOLON)
    {
      return_stmt->value = parse_expression(parser, ASSIGNMENT);
    }

    lexer::Token semicolon = parser.expect(lexer::TokenKind::SEMICOLON, "parser.cpp : parse_return_statement() : Expected ';' after return statement");
    return_stmt->lineend = semicolon.lineend;
    return_stmt->columnend = semicolon.columnend;
    return return_stmt;
  }
  ast::Statement *parse_if_statement(parser_ &parser)
  {
    lexer::Token keyword = parser.expect(lexer::TokenKind::IF, "parser.cpp : parse_if_statement() : Expected 'if' keyword"); // [DEBUG**]
    ast::IfStatement *if_stmt = new ast::IfStatement();
    if_stmt->linestart = keyword.linestart;
    if_stmt->columnstart = keyword.columnstart;

    if_stmt->condition = parse_expression(parser, DEFAULT);
    if_stmt->then_branch = dynamic_cast<ast::BlockStatement *>(parse_block_statement(parser));

    if (parser.current_kind() == lexer::TokenKind::ELSE)
    {
      parser.advance();
      if (parser.current_kind() == lexer::TokenKind::IF)
      {
        if_stmt->else_if_branch = dynamic_cast<ast::IfStatement *>(parse_if_statement(parser));
      }
      else
      {
        if_stmt->else_branch = dynamic_cast<ast::BlockStatement *>(parse_block_statement(parser));
      }
    }

    if_stmt->kind = ast::StatementKind::IF_STATEMENT;
    if_stmt->lineend = if_stmt->else_branch != nullptr ? if_stmt->else_branch->lineend : if_stmt->else_if_branch != nullptr ? if_stmt->else_if_branch->lineend
                                                                                                                            : if_stmt->then_branch->lineend;
    if_stmt->columnend = if_stmt->else_branch != nullptr ? if_stmt->else_branch->columnend : if_stmt->else_if_branch != nullptr ? if_stmt->else_if_branch->columnend
                                                                                                                                : if_stmt->then_branch->columnend;
    return if_stmt;
  }
  ast::Statement *parse_for_loop_statement(parser_ &parser)
  {
    lexer::Token keyword = parser.expect(lexer::TokenKind::FOR, "parser.cpp : parse_for_statement() : Expected 'for' keyword");
    ast::ForLoopStatement *for_stmt = new ast::ForLoopStatement();
    for_stmt->linestart = keyword.linestart;
    for_stmt->columnstart = keyword.columnstart;

    parser.expect(lexer::TokenKind::OPEN_PAREN, "parser.cpp : parse_for_statement() : Expected '(' after 'for' keyword");
    if (parser.current_kind() != lexer::TokenKind::SEMICOLON)
    {
      for_stmt->initializer = parse_expression(parser, DEFAULT);
    }
    if (parser.current_kind() == lexer::TokenKind::OF)
    {
      parser.advance();
      for_stmt->array_of = parse_expression(parser, DEFAULT);
      for_stmt->of_loop = true;
      for_stmt->condition = nullptr;
      for_stmt->post = nullptr;
    }
    else
    {
      parser.expect(lexer::TokenKind::SEMICOLON, "parser.cpp : parse_for_statement() : Expected ';' after for loop initializer");
      if (parser.current_kind() != lexer::TokenKind::SEMICOLON)
      {
        for_stmt->condition = parse_expression(parser, DEFAULT);
      }
      parser.expect(lexer::TokenKind::SEMICOLON, "parser.cpp : parse_for_statement() : Expected ';' after for loop condition");
      if (parser.current_kind() != lexer::TokenKind::CLOSE_PAREN)
      {
        for_stmt->post = parse_expression(parser, DEFAULT);
      }
      for_stmt->of_loop = false;
    }
    parser.expect(lexer::TokenKind::CLOSE_PAREN, "parser.cpp : parse_for_statement() : Expected ')' after for loop post-expression");

    ast::BlockStatement *body = dynamic_cast<ast::BlockStatement *>(parse_block_statement(parser));
    for_stmt->body = body;
    for_stmt->kind = ast::StatementKind::FOR_LOOP_STATEMENT;
    for_stmt->linestart = keyword.linestart;
    for_stmt->lineend = body->lineend;
    for_stmt->columnstart = keyword.columnstart;
    for_stmt->columnend = body->columnend;
    return for_stmt;
  }

  //*-------------------
  //*    EXPRESSIONS
  //*-------------------
  ast::Expression *parse_expression(parser::parser_ &parser, BindingPower binding_power)
  {
    // [DEBUG**] std::cerr << "[parse_expression] Start with token kind: " << lexer::token_kind_to_string(parser.current_kind()) << ", binding_power: " << binding_power << "\n";
    // If current token is not found in the binding_power_lu, break
    if (parser.current_kind() == lexer::TokenKind::CLOSE_PAREN)
    {
      parser.advance();
      return new ast::DummyExpression();
    }
    if (!binding_power_lu.count(parser.current_kind()))
    {
      std::cout << "[parse_expression] No binding power found for token kind: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
      return new ast::DummyExpression();
    }

    // [DEBUG**] std::cout << "[parse_expression] Current token kind: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
    lexer::TokenKind token_kind = parser.current_kind();
    // [DEBUG**] std::cout << "[parse_expression] Token kind: " << lexer::token_kind_to_string(token_kind) << "\n";
    auto nud_handler = nud_lu[token_kind];
    // [DEBUG**] std::cout << "[parse_expression] NUD handler: " << (nud_handler ? "available" : "null") << "\n";

    if (nud_handler == nullptr)
    {
      // [DEBUG**] std::cout << "[parse_expression] No NUD handler found for token kind: " << lexer::token_kind_to_string(token_kind) << "\n";
      error::Error err(
          error::ErrorCode::PARSER_ERROR,
          "No NUD function found for token of kind '" + lexer::token_kind_to_string(token_kind) + "'",
          parser.current_token().linestart,
          parser.current_token().lineend,
          parser.current_token().columnstart,
          parser.current_token().columnend,
          "parser.cpp : parse_expression()",
          error::ErrorImportance::MODERATE);
      // [DEBUG**] std::cout << "[parse_expression] Returning dummy expression\n";
      return new ast::DummyExpression();
    }

    ast::Expression *left = nud_handler(parser);
    // [DEBUG**] std::cout << "[parse_expression] Left expression kind: " << (left ? ast::statement_kind_to_string(left->kind) : "null") << "\n";

    while (binding_power_lu.count(parser.current_kind()) &&
           *binding_power_lu[parser.current_kind()] > binding_power)
    {
      // [DEBUG**] std::cerr << "[parse_expression] In while loop, token kind: " << lexer::token_kind_to_string(parser.current_kind()) << ", next binding_power: " << *binding_power_lu[parser.current_kind()] << "\n";
      token_kind = parser.current_kind();
      // [DEBUG**] std::cerr << "[parse_expression] Token kind: " << lexer::token_kind_to_string(token_kind) << "\n";
      auto led_handler = led_lu[token_kind];
      // [DEBUG**] std::cerr << "[parse_expression] LED handler: " << (led_handler ? "available" : "null") << "\n";
      if (led_handler == nullptr)
      {
        // [DEBUG**] std::cerr << "[parse_expression] No LED handler found for token kind: " << lexer::token_kind_to_string(token_kind) << "\n";
        error::Error err(
            error::ErrorCode::PARSER_ERROR,
            "No LED function found for token of kind '" + lexer::token_kind_to_string(token_kind) + "'",
            parser.current_token().linestart,
            parser.current_token().lineend,
            parser.current_token().columnstart,
            parser.current_token().columnend,
            "parser.cpp : parse_expression() : while loop",
            error::ErrorImportance::MODERATE);
        return left ? left : new ast::DummyExpression();
      }

      // [DEBUG**] std::cerr << "[parse_expression] Calling LED handler for token kind: " << lexer::token_kind_to_string(token_kind) << "\n";
      left = led_handler(parser, left, *binding_power_lu[parser.current_kind()]);
      // [DEBUG**] std::cerr << "[parse_expression] LED handler returned expression of kind: " << (left ? ast::statement_kind_to_string(left->kind) : "null") << "\n";
    }

    // [DEBUG**] std::cerr << "[parse_expression] Returning expression of kind: " << (left ? ast::statement_kind_to_string(left->kind) : "null expression") << "\n";
    return left;
  }
  ast::Expression *parse_primary_expression(parser::parser_ &parser)
  {
    // [DEBUG**] std::cerr << "[parse_primary_expression] Current token kind: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
    switch (parser.current_kind())
    {
    case lexer::TokenKind::NUMBER:
    {
      auto token = parser.current_token();
      ast::NumberExpression *literal = new ast::NumberExpression(std::stod(token.value));
      // [DEBUG**] std::cout << "[parse_primary_expression] Creating NumberExpression with value: " << parser.current_token().value << "\n";
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
    case lexer::TokenKind::OPEN_SQUARE:
    {
      ast::ArrayExpression *array = new ast::ArrayExpression();
      array->linestart = parser.current_token().linestart;
      array->lineend = parser.current_token().lineend;
      array->columnstart = parser.current_token().columnstart;
      array->columnend = parser.current_token().columnend;
      array->kind = ast::StatementKind::ARRAY_EXPRESSION;
      parser.advance();
      while (parser.current_kind() != lexer::TokenKind::CLOSE_SQUARE)
      {
        if (parser.current_kind() == lexer::TokenKind::COMMA)
        {
          parser.advance();
          continue;
        }
        array->elements.push_back(parse_expression(parser, DEFAULT));
      }
      parser.expect(lexer::TokenKind::CLOSE_SQUARE, "parser.cpp : parse_primary_expression() : Expected ']' to close array expression");
      return array;
    } /*
     case lexer::TokenKind::OPEN_CURLY:
     {
       ast::ObjectExpression *object = new ast::ObjectExpression();
       object->linestart = parser.current_token().linestart;
       object->columnstart = parser.current_token().columnstart;
       object->kind = ast::StatementKind::OBJECT_EXPRESSION;
       parser.advance();
       object->properties = parse_properties(parser);
       object->lineend = parser.current_token().lineend;
       object->columnend = parser.current_token().columnend;
       parser.expect(lexer::TokenKind::CLOSE_CURLY, "parser.cpp : parse_primary_expression() : Expected '}' to close object expression");
       return object;
     }*/
    default:
    {
      // [DEBUG**] std::cerr << "[parse_primary_expression] Returning null expression from default case\n";
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
    // [DEBUG**] std::cerr << "[parse_binary_expression] Left expression kind: " << (left ? ast::statement_kind_to_string(left->kind) : "null") << ", operator token: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
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

    // [DEBUG**] std::cerr << "[parse_binary_expression] Returning BinaryExpression\n";
    return binary_expr;
  }
  ast::Expression *parse_call_expression(parser::parser_ &parser, ast::Expression *left, BindingPower binding_power)
  {
    // [DEBUG**] std::cerr << "[parse_call_expression] Left expression kind: " << (left ? ast::statement_kind_to_string(left->kind) : "null") << ", operator token: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
    lexer::Token open_paren = parser.expect(lexer::TokenKind::OPEN_PAREN, "parser.cpp : parse_call_expression() : Expected '(' after function name");
    std::vector<ast::Expression *> args = parse_args(parser);
    lexer::Token close_paren = parser.expect(lexer::TokenKind::CLOSE_PAREN, "parser.cpp : parse_call_expression() : Expected ')' after function arguments");

    ast::CallExpression *call_expr = new ast::CallExpression();
    call_expr->function = left;
    call_expr->args = args;
    call_expr->linestart = left->linestart;
    call_expr->lineend = close_paren.lineend;
    call_expr->columnstart = left->columnstart;
    call_expr->columnend = close_paren.columnend;
    call_expr->kind = ast::StatementKind::CALL_EXPRESSION;

    // [DEBUG**] std::cerr << "[parse_call_expression] Returning CallExpression\n";
    return call_expr;
  }
  ast::Expression *parse_assignment_expression(parser::parser_ &parser, ast::Expression *left, BindingPower binding_power)
  {
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Left expression kind: " << (left ? ast::statement_kind_to_string(left->kind) : "null") << ", operator token: " << lexer::token_kind_to_string(parser.current_kind()) << "\n";
    lexer::Token op = parser.advance();
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Operator token: " << lexer::token_kind_to_string(op.kind) << "\n";
    ast::Expression *right;
    if (op.kind == lexer::TokenKind::PLUS_PLUS || op.kind == lexer::TokenKind::MINUS_MINUS)
    {
      // [DEBUG**] std::cerr << "[parse_assignment_expression] Operator is increment/decrement\n";
      right = nullptr;
      // [DEBUG**] std::cerr << "[parse_assignment_expression] Right expression set to null\n";
    }
    else
    {
      // [DEBUG**] std::cerr << "[parse_assignment_expression] Operator is assignment\n";
      right = parse_expression(parser, binding_power);
      // [DEBUG**] std::cerr << "[parse_assignment_expression] Right expression kind: " << (right ? ast::statement_kind_to_string(right->kind) : "null") << "\n";
    }
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Creating AssignmentExpression\n";
    ast::AssignmentExpression *assignment_expr = new ast::AssignmentExpression();
    // [DEBUG**] std::cerr << "[parse_assignment_expression] AssignmentExpression created\n";
    assignment_expr->left = left;
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Left expression assigned to AssignmentExpression\n";
    assignment_expr->op = op;
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Operator token assigned to AssignmentExpression\n";
    assignment_expr->right = right;
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Right expression assigned to AssignmentExpression\n";
    assignment_expr->increment_decrement = op.kind == lexer::TokenKind::PLUS_PLUS || op.kind == lexer::TokenKind::MINUS_MINUS;
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Increment/Decrement flag set to " << (assignment_expr->increment_decrement ? "true" : "false") << "\n";
    assignment_expr->linestart = left->linestart;
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Linestart assigned to AssignmentExpression\n";
    assignment_expr->lineend = right != nullptr ? right->lineend : op.lineend;
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Lineend assigned to AssignmentExpression\n";
    assignment_expr->columnstart = left->columnstart;
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Columnstart assigned to AssignmentExpression\n";
    assignment_expr->columnend = right != nullptr ? right->columnend : op.columnend;
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Columnend assigned to AssignmentExpression\n";
    assignment_expr->kind = ast::StatementKind::ASSIGNMENT_EXPRESSION;
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Statement kind set to AssignmentExpression\n";
    // [DEBUG**] std::cerr << "[parse_assignment_expression] Returning AssignmentExpression\n";
    return assignment_expr;
  }
  ast::Expression *parse_variable_declaration_expression(parser_ &parser)
  {
    ast::VariableDeclarationExpression *var_decl = new ast::VariableDeclarationExpression();
    lexer::Token keyword = parser.current_token();
    ast::Type type;
    std::vector<ast::Expression *> fields;
    ast::Expression *value = nullptr;
    bool is_const = false;
    bool is_public = false;

    if (keyword.kind == lexer::TokenKind::CONST)
    {
      is_const = true;
      parser.advance();
    }
    else if (keyword.kind == lexer::TokenKind::PUBLIC)
    {
      is_public = true;
      parser.advance();
      if (parser.current_kind() == lexer::TokenKind::CONST)
      {
        is_const = true;
        parser.advance();
      }
      else
      {
        parser.expect(lexer::TokenKind::LET, "parser.cpp : parse_variable_declaration_expression() : Expected 'let' after 'public'");
      }
    }
    else
    {
      parser.advance();
    }

    lexer::Token identifier = parser.expect(lexer::TokenKind::IDENTIFIER, "parser.cpp : parse_variable_declaration_expression() : Expected identifier after 'let' or 'const'");

    if (parser.current_kind() == lexer::TokenKind::COLON)
    {
      parser.advance();
      type = parse_type(parser, DEFAULT);
    }

    if (parser.current_kind() == lexer::TokenKind::ASSIGNMENT)
    {
      parser.advance();
      value = parse_expression(parser, ASSIGNMENT);
    }

    var_decl->kind = ast::StatementKind::VARIABLE_DECLARATION_EXPRESSION;
    var_decl->name = identifier.value;
    var_decl->value = value;
    var_decl->type = type;
    var_decl->is_const = is_const;
    var_decl->is_public = is_public;
    var_decl->linestart = keyword.linestart;
    var_decl->lineend = value != nullptr ? value->lineend : identifier.lineend;
    var_decl->columnstart = keyword.columnstart;
    var_decl->columnend = value != nullptr ? value->columnend : identifier.columnend;

    return var_decl;
  }
  ast::Expression *parse_object_expression(parser::parser_ &parser)
  {
    ast::ObjectExpression *object = new ast::ObjectExpression();
    object->linestart = parser.current_token().linestart;
    object->columnstart = parser.current_token().columnstart;
    object->kind = ast::StatementKind::OBJECT_EXPRESSION;
    object->structure = nullptr;
    parser.advance();
    object->properties = parse_properties(parser);
    object->lineend = parser.current_token().lineend;
    object->columnend = parser.current_token().columnend;
    parser.expect(lexer::TokenKind::CLOSE_CURLY, "parser.cpp : parse_primary_expression() : Expected '}' to close object expression");
    return object;
  }
  ast::Expression *parse_member_expression(parser::parser_ &parser, ast::Expression *left, BindingPower binding_power)
  {
    lexer::Token op = parser.advance();
    ast::Expression *right = parse_expression(parser, binding_power);
    ast::MemberExpression *member_expr = new ast::MemberExpression();
    lexer::Token close_square;
    bool is_computed = false;
    if (parser.current_kind() == lexer::TokenKind::CLOSE_SQUARE)
    {
      is_computed = true;
      close_square = parser.advance();
    }
    member_expr->object = left;
    member_expr->property = right;
    member_expr->is_computed = is_computed;
    member_expr->linestart = left->linestart;
    member_expr->lineend = is_computed == true ? close_square.lineend : right->lineend;
    member_expr->columnstart = left->columnstart;
    member_expr->columnend = is_computed == true ? close_square.columnend : right->columnend;
    member_expr->kind = ast::StatementKind::MEMBER_EXPRESSION;

    return member_expr;
  }

  //*---------------
  //*    HELPERS
  //*---------------
  std::vector<ast::Expression *> parse_args(parser_ &parser)
  {
    std::vector<ast::Expression *> args;
    while (parser.current_kind() != lexer::TokenKind::CLOSE_PAREN && parser.current_kind() != lexer::TokenKind::EOF_)
    {
      ast::Expression *arg = parse_expression(parser, DEFAULT);
      args.push_back(arg);
      if (parser.current_kind() == lexer::TokenKind::CLOSE_PAREN)
        break;
      parser.expect(lexer::TokenKind::COMMA, "parser.cpp : parse_args() : Expected ',' between arguments");
    }
    return args;
  }
  std::vector<ast::ParameterExpression *> parse_parameters(parser_ &parser)
  {
    std::vector<ast::ParameterExpression *> params;
    bool first_param = true;

    while (parser.current_kind() != lexer::TokenKind::CLOSE_PAREN)
    {
      // [DEBUG**] std::cout << "[parse_parameters] processing token: " << parser.current_token().value << " (" << lexer::token_kind_to_string(parser.current_kind()) << ")\n";

      if (!first_param)
      {
        if (parser.current_kind() != lexer::TokenKind::COMMA)
        {
          error::Error err(
              error::ErrorCode::PARSER_ERROR,
              "Expected ',' between parameters",
              parser.current_token().linestart,
              parser.current_token().lineend,
              parser.current_token().columnstart,
              parser.current_token().columnend,
              "parser.cpp : parse_parameters()",
              error::ErrorImportance::HIGH);
          break;
        }
        parser.advance(); // consume comma
      }

      // Create a new parameter expression
      ast::ParameterExpression *param = new ast::ParameterExpression();

      // Parse parameter name
      if (parser.current_kind() != lexer::TokenKind::IDENTIFIER)
      {
        error::Error err(
            error::ErrorCode::PARSER_ERROR,
            "Expected parameter name",
            parser.current_token().linestart,
            parser.current_token().lineend,
            parser.current_token().columnstart,
            parser.current_token().columnend,
            "parser.cpp : parse_parameters()",
            error::ErrorImportance::HIGH);
        delete param;
        break;
      }

      lexer::Token identifier = parser.current_token();
      parser.advance();
      param->name = identifier.value;
      param->linestart = identifier.linestart;
      param->columnstart = identifier.columnstart;

      // Parse parameter type
      if (parser.current_kind() == lexer::TokenKind::COLON)
      {
        parser.advance(); // consume colon

        param->type = parse_type(parser, DEFAULT);
        params.push_back(param);
      }
      first_param = false;
    }

    return params;
  }
  std::vector<std::pair<ast::Expression *, ast::Expression *>> parse_properties(parser_ &parser)
  {
    std::vector<std::pair<ast::Expression *, ast::Expression *>> properties;
    while (parser.current_kind() != lexer::TokenKind::CLOSE_CURLY)
    {
      if (parser.current_kind() == lexer::TokenKind::COMMA)
      {
        parser.advance();
        continue;
      }
      auto key = parse_expression(parser, DEFAULT);
      parser.expect(lexer::TokenKind::COLON, "parser.cpp : parse_primary_expression() : Expected ':' after object key");
      auto value = parse_expression(parser, DEFAULT);
      properties.push_back(std::make_pair(key, value));
    }
    return properties;
  }
}
