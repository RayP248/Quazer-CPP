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
    BINARY_EXPRESSION,
    NUMBER_EXPRESSION,
    SYMBOL_EXPRESSION,
    STRING_EXPRESSION,
    DUMMY_EXPRESSION
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
    Expression *left = nullptr;  // Changed from Statement* to Expression*
    Expression *right = nullptr; // Changed from Statement* to Expression*
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

  //*-------------------
  //*    STATEMENTS
  //*-------------------
  struct BlockStatement : public Statement
  {
    std::vector<Statement *> body;
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
