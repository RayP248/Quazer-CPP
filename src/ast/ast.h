#ifndef AST_H
#define AST_H

#include "../lexer/lexer.h"
#include <iostream>
#include <vector>
#include <string>
#include <variant>

namespace ast
{
  enum StatementKind
  {
    STATEMENT,
    PROGRAM,
    EXPRESSION_STATEMENT,
    BLOCK_STATEMENT,
    VARIABLE_DECLARATION_STATEMENT,
    RETURN_STATEMENT,
    FUNCTION_DECLARATION_STATEMENT,
    IF_STATEMENT,
    FOR_LOOP_STATEMENT,
    NUMBER_EXPRESSION,
    SYMBOL_EXPRESSION,
    STRING_EXPRESSION,
    BINARY_EXPRESSION,
    CALL_EXPRESSION,
    ASSIGNMENT_EXPRESSION,
    VARIABLE_DECLARATION_EXPRESSION,
    ARRAY_EXPRESSION,
    DUMMY_EXPRESSION,
  };

  std::string statement_kind_to_string(StatementKind kind);

  // Base Statement class with only common fields
  struct Statement
  {
    StatementKind kind;
    int linestart = 0;
    int lineend = 0;
    int columnstart = 0;
    int columnend = 0;
    virtual ~Statement() = default;
  };

  struct Expression : public Statement
  {
    Expression() { kind = StatementKind::EXPRESSION_STATEMENT; }
    virtual ~Expression() = default;
  };

  struct Program : public Statement
  {
    std::vector<Statement *> body;
    Program()
    {
      kind = StatementKind::PROGRAM;
    }
    ~Program() override
    {
      for (auto &stmt : body)
      {
        delete stmt;
      }
    }
  };

  //*-------------
  //*    TYPES
  //*-------------
  struct Type
  {
    std::string name;
    std::vector<Type> generics;
    std::vector<Type> fields;
    bool is_inferred = false;
    Type() : is_inferred(true) {}
    Type(const std::string &name) : name(name), is_inferred(false) {}
    Type(const std::string &name, const std::vector<Type> &generics) : name(name), generics(generics), is_inferred(false) {}
  };

  //*-------------------
  //*    EXPRESSIONS
  //*-------------------

  // Literal Expressions
  struct NumberExpression : public Expression
  {
    double value;
    NumberExpression(double val)
    {
      kind = NUMBER_EXPRESSION;
      value = val;
    }
  };

  struct StringExpression : public Expression
  {
    std::string value;
    StringExpression()
    {
      kind = StatementKind::STRING_EXPRESSION;
    }
  };

  struct SymbolExpression : public Expression
  {
    std::string value;
    SymbolExpression(const std::string &val)
    {
      kind = SYMBOL_EXPRESSION;
      value = val;
    }
  };

  struct DummyExpression : public Expression
  {
    DummyExpression()
    {
      kind = DUMMY_EXPRESSION;
    }
  };

  // Complex Expressions
  struct BinaryExpression : public Expression
  {
    Expression *left = nullptr;
    Expression *right = nullptr;
    lexer::Token op;

    BinaryExpression()
    {
      kind = BINARY_EXPRESSION;
    }
    ~BinaryExpression() override
    {
      delete left;
      delete right;
    }
  };

  struct CallExpression : public Expression
  {
    ast::Expression *function = nullptr;
    std::vector<Expression *> args;
    CallExpression()
    {
      kind = StatementKind::CALL_EXPRESSION;
    }
    ~CallExpression() override
    {
      delete function;
      for (auto &arg : args)
      {
        delete arg;
      }
    }
  };

  struct AssignmentExpression : public Expression
  {
    Expression *left = nullptr;
    Expression *right = nullptr;
    lexer::Token op;
    bool increment_decrement = false;
    AssignmentExpression()
    {
      kind = StatementKind::ASSIGNMENT_EXPRESSION;
    }
    ~AssignmentExpression() override
    {
      delete left;
      delete right;
    }
  };

  struct VariableDeclarationExpression : public Expression
  {
    std::string name;
    Expression *value = nullptr;
    Type type;
    bool is_const = false;
    bool is_public = false;
    VariableDeclarationExpression()
    {
      kind = StatementKind::VARIABLE_DECLARATION_EXPRESSION;
    }
    ~VariableDeclarationExpression() override
    {
      delete value;
    }
  };

  struct ArrayExpression : public Expression
  {
    std::vector<Expression *> elements;
    ArrayExpression()
    {
      kind = StatementKind::EXPRESSION_STATEMENT;
    }
    ~ArrayExpression() override
    {
      for (auto &element : elements)
      {
        delete element;
      }
    }
  };

  //*-------------------
  //*    STATEMENTS
  //*-------------------
  struct ReturnStatement : public Statement
  {
    Expression *value = nullptr;
    ReturnStatement()
    {
      kind = RETURN_STATEMENT;
    }
    ~ReturnStatement() override
    {
      delete value;
    }
  };

  struct BlockStatement : public Statement
  {
    std::vector<Statement *> body;
    ReturnStatement *return_statement = nullptr;
    BlockStatement()
    {
      kind = StatementKind::BLOCK_STATEMENT;
    }
    ~BlockStatement() override
    {
      for (auto &stmt : body)
      {
        delete stmt;
      }
    }
  };

  struct ExpressionStatement : public Statement
  {
    Expression *expression = nullptr;
    ExpressionStatement()
    {
      kind = EXPRESSION_STATEMENT;
    }
    ~ExpressionStatement() override
    {
      delete expression;
    }
  };

  struct VariableDeclarationStatement : public Statement
  {
    std::string name;
    Expression *value = nullptr;
    Type type;
    bool is_const = false;
    bool is_public = false;

    VariableDeclarationStatement()
    {
      kind = VARIABLE_DECLARATION_STATEMENT;
    }
    ~VariableDeclarationStatement() override
    {
      delete value;
    }
  };

  struct ParameterExpression : public Expression
  {
    std::string name;
    Type type;
  };

  struct FunctionDeclarationStatement : public Statement
  {
    std::string name;
    std::vector<ParameterExpression *> parameters;
    BlockStatement *body;
    ReturnStatement *return_statement;
    Type return_type;
    FunctionDeclarationStatement()
    {
      kind = StatementKind::FUNCTION_DECLARATION_STATEMENT;
    }
    ~FunctionDeclarationStatement() override
    {
      delete body;
      delete return_statement;
      for (auto &param : parameters)
      {
        delete param;
      }
    }
  };

  struct IfStatement : public Statement
  {
    Expression *condition;
    BlockStatement *then_branch = nullptr;
    IfStatement *else_if_branch = nullptr;
    BlockStatement *else_branch = nullptr;
    IfStatement()
    {
      kind = StatementKind::IF_STATEMENT;
    }
    ~IfStatement() override
    {
      delete condition;
      delete then_branch;
      delete else_branch;
    }
  };

  struct ForLoopStatement : public Statement
  {
    Expression *initializer;
    Expression *condition;
    Expression *post;
    Expression *array_of;
    BlockStatement *body;

    ForLoopStatement()
    {
      kind = StatementKind::FOR_LOOP_STATEMENT;
    }
    ~ForLoopStatement() override
    {
      delete initializer;
      delete condition;
      delete post;
      delete array_of;
      delete body;
    }
  };

  //*------------
  //*    MISC
  //*------------
  typedef std::variant<
      ast::Program *,
      ast::ExpressionStatement *,
      ast::BinaryExpression *,
      ast::NumberExpression *,
      ast::StringExpression *,
      ast::SymbolExpression *,
      ast::Statement *, // Fallback
      ast::Expression *>
      ASTVariant; // Fallback
  // TODO: Define new AST node structures for additional language constructs.
  // Example:
  /*
  struct IfStatement : public Statement
  {
    IfStatement() { kind = "IfStatement"; }
    Expression *condition;
    Statement *thenBranch;
    Statement *elseBranch;
    ~IfStatement() override
    {
      delete condition;
      delete thenBranch;
      if (elseBranch)
        delete elseBranch;
    }
  };

  struct ForStatement : public Statement
  {
    ForStatement() { kind = "ForStatement"; }
    Statement *initializer;
    Expression *condition;
    Expression *increment;
    Statement *body;
    ~ForStatement() override
    {
      delete initializer;
      delete condition;
      delete increment;
      delete body;
    }
  };

  struct FunctionDeclaration : public Statement
  {
    FunctionDeclaration() { kind = "FunctionDeclaration"; }
    std::string name;
    std::vector<std::string> parameters;
    Statement *body;
    ~FunctionDeclaration() override
    {
      delete body;
    }
  };
  */

  // TODO: Add new expression or statement types as needed for extended functionality.
} // namespace ast

#endif // AST_H
