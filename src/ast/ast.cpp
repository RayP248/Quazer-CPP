#include "ast.h"

namespace ast
{
  std::string statement_kind_to_string(StatementKind kind)
  {
    switch (kind)
    {
    case STATEMENT:
      return "Statement";
    case PROGRAM:
      return "Program";
    case EXPRESSION_STATEMENT:
      return "ExpressionStatement";
    case BLOCK_STATEMENT:
      return "BlockStatement";
    case VARIABLE_DECLARATION_STATEMENT:
      return "VariableDeclarationStatement";
    case RETURN_STATEMENT:
      return "ReturnStatement";
    case FUNCTION_DECLARATION_STATEMENT:
      return "FunctionDeclarationStatement";
    case IF_STATEMENT:
      return "IfStatement";
    case FOR_LOOP_STATEMENT:
      return "ForLoopStatement";
    case NUMBER_EXPRESSION:
      return "NumberExpression";
    case SYMBOL_EXPRESSION:
      return "SymbolExpression";
    case STRING_EXPRESSION:
      return "StringExpression";
    case BINARY_EXPRESSION:
      return "BinaryExpression";
    case CALL_EXPRESSION:
      return "CallExpression";
    case ASSIGNMENT_EXPRESSION:
      return "AssignmentExpression";
    case VARIABLE_DECLARATION_EXPRESSION:
      return "VariableDeclarationExpression";
    case ARRAY_EXPRESSION:
      return "ArrayExpression";
    case DUMMY_EXPRESSION:
      return "DummyExpression";
    default:
      return "Unknown";
    }
  }
}
