#include "ast.h"

namespace ast
{
  std::string statement_kind_to_string(StatementKind kind)
  {
    switch (kind)
    {
    case PROGRAM:
      return "Program";
    case STATEMENT:
      return "Statement";
    case EXPRESSION_STATEMENT:
      return "ExpressionStatement";
    case BINARY_EXPRESSION:
      return "BinaryExpression";
    case NUMBER_EXPRESSION:
      return "NumberExpression";
    case SYMBOL_EXPRESSION:
      return "SymbolExpression";
    case STRING_EXPRESSION:
      return "StringExpression";
    case BLOCK_STATEMENT:
      return "BlockStatement";
    default:
      return "Unknown";
    }
  }
}
