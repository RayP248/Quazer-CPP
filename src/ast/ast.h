#ifndef AST_H
#define AST_H

#include "../lexer/lexer.h"
#include <vector>
namespace ast
{

  struct Statement
  {
    int linestart;
    int lineend;
    int columnstart;
    int columnend;
    virtual ~Statement() = default;
  };

  struct Expression : public Statement
  {
    virtual ~Expression() = default;
  };

  struct Program : public Statement
  {
    std::vector<Statement *> statements;
    ~Program() override
    {
      for (auto &stmt : statements)
      {
        delete stmt;
      }
    }
  };

  struct BinaryExpression : public Expression
  {
    Expression *left;
    Expression *right;
    lexer::Token op;
    ~BinaryExpression() override
    {
      delete left;
      delete right;
    }
  };

} // namespace ast

#endif // AST_H
