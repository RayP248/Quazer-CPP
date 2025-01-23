#ifndef AST_H
#define AST_H

#include "../lexer/lexer.h"
#include <iostream>
#include <vector>

namespace ast
{

  struct Statement
  {
    std::string kind = "Statement"; // e.g., "Program", "Expression", etc.
    int linestart = 0;
    int lineend = 0;
    int columnstart = 0;
    int columnend = 0;
    virtual ~Statement() = default;
  };

  struct Expression : public Statement
  {
    Expression() { kind = "Expression"; } // e.g., "BinaryExpression", "Literal", etc.
    virtual ~Expression() = default;
  };

  struct Program : public Statement
  {
    Program() { kind = "Program"; }
    std::vector<Statement *> body;
    ~Program() override
    {
      for (auto &stmt : body)
      {
        delete stmt;
      }
    }
  };
  //*-------------------
  //*    EXPRESSIONS
  //*-------------------

  // Literal Expressions
  struct NumberExpression : public Expression
  {
    NumberExpression() { kind = "NumberExpression"; }
    double value;
  };

  struct StringExpression : public Expression
  {
    StringExpression() { kind = "StringExpression"; }
    std::string value;
  };

  struct SymbolExpression : public Expression
  {
    SymbolExpression() { kind = "SymbolExpression"; }
    std::string value;
  };

  // Complex Expressions
  struct BinaryExpression : public Expression
  {
    BinaryExpression()
    {
      kind = "BinaryExpression";
      left = nullptr;
      right = nullptr;
    } // Initialize pointers
    Expression *left;
    lexer::Token op;
    Expression *right;
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
    BlockStatement() { kind = "BlockStatement"; }
    std::vector<Statement *> body;
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
    ExpressionStatement() { kind = "ExpressionStatement"; }
    Expression *expression;
    ~ExpressionStatement() override
    {
      delete expression;
    }
  };

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
