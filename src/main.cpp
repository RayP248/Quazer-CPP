#include "./global.h"
#include "./lexer/lexer.h"
#include "./parser/parser.h"
#include <string>
#include <iostream>
#include <fstream>
#include <functional>

/*
Input code:
45.2 + 5 * 4

Desired structure:
Program {
  kind: "Program",
  body: [
    ExpressionStatement {
      kind: "ExpressionStatement",
      expression: BinaryExpression {
        kind: "BinaryExpression",
        left: NumberExpression {
          kind: "NumberExpression",
          value: 45.2,
          linestart: 1,
          lineend: 1,
          columnstart: 1,
          columnend: 5
        },
        op: {
          kind: "PLUS",
          value: "+",
          linestart: 1,
          lineend: 1,
          columnstart: 6,
          columnend: 7
        },
        right: BinaryExpression {
          kind: "BinaryExpression",
          left: NumberExpression {
            kind: "NumberExpression",
            value: 5,
            linestart: 1,
            lineend: 1,
            columnstart: 8,
            columnend: 9
          },
          op: {
            kind: "STAR",
            value: "*",
            linestart: 1,
            lineend: 1,
            columnstart: 10,
            columnend: 11
          },
          right: NumberExpression {
            kind: "NumberExpression",
            value: 4,
            linestart: 1,
            lineend: 1,
            columnstart: 11,
            columnend: 12
          }
        }
      }
      linestart: 1,
      lineend: 1,
      columnstart: 1,
      columnend: 12
    }
  ]
}
*/

static std::string printAST(const ast::Program *stmt, int indent = 0)
{
  std::string output;
  try
  {
    if (!stmt)
      return output;

    auto indentFunc = [](int indent)
    {
      return std::string(indent, ' ');
    };

    std::function<std::string(const ast::Statement *, int)> printStmt;
    std::function<std::string(const ast::Expression *, int)> printExpr;

    printExpr = [&](const ast::Expression *node, int level) -> std::string
    {
      if (!node)
        return "";

      std::string result;
      result += indentFunc(level) + node->kind + " {\n";
      result += indentFunc(level + 2) + "kind: \"" + node->kind + "\",\n";

      if (auto binExpr = dynamic_cast<const ast::BinaryExpression *>(node))
      {
        result += indentFunc(level + 2) + "left:\n" + printExpr(binExpr->left, level + 4) + ",\n";
        result += indentFunc(level + 2) + "op: {\n";
        result += indentFunc(level + 4) + "kind: \"" + lexer::token_kind_to_string(binExpr->op.kind) + "\",\n";
        result += indentFunc(level + 4) + "value: \"" + binExpr->op.value + "\",\n";
        result += indentFunc(level + 4) + "linestart: " + std::to_string(binExpr->op.linestart) + ",\n";
        result += indentFunc(level + 4) + "lineend: " + std::to_string(binExpr->op.lineend) + ",\n";
        result += indentFunc(level + 4) + "columnstart: " + std::to_string(binExpr->op.columnstart) + ",\n";
        result += indentFunc(level + 4) + "columnend: " + std::to_string(binExpr->op.columnend) + "\n";
        result += indentFunc(level + 2) + "},\n";
        result += indentFunc(level + 2) + "right:\n" + printExpr(binExpr->right, level + 4) + "\n";
      }
      else if (auto numExpr = dynamic_cast<const ast::NumberExpression *>(node))
      {
        result += indentFunc(level + 2) + "value: " + std::to_string(numExpr->value) + "\n";
      }
      else if (auto strExpr = dynamic_cast<const ast::StringExpression *>(node))
      {
        result += indentFunc(level + 2) + "value: \"" + strExpr->value + "\"\n";
      }
      else if (auto symExpr = dynamic_cast<const ast::SymbolExpression *>(node))
      {
        result += indentFunc(level + 2) + "value: \"" + symExpr->value + "\"\n";
      }

      result += indentFunc(level) + "}";
      return result;
    };

    printStmt = [&](const ast::Statement *node, int level) -> std::string
    {
      if (!node)
        return "";

      std::string result;
      result += indentFunc(level) + node->kind + " {\n";
      result += indentFunc(level + 2) + "kind: \"" + node->kind + "\",\n";
      result += indentFunc(level + 2) + "linestart: " + std::to_string(node->linestart) + ",\n";
      result += indentFunc(level + 2) + "lineend: " + std::to_string(node->lineend) + ",\n";
      result += indentFunc(level + 2) + "columnstart: " + std::to_string(node->columnstart) + ",\n";
      result += indentFunc(level + 2) + "columnend: " + std::to_string(node->columnend);

      if (auto exprStmt = dynamic_cast<const ast::ExpressionStatement *>(node))
      {
        result += ",\n" + indentFunc(level + 2) + "expression:\n" + printExpr(exprStmt->expression, level + 4);
      }
      else if (auto blockStmt = dynamic_cast<const ast::BlockStatement *>(node))
      {
        result += ",\n" + indentFunc(level + 2) + "body: [\n";
        for (size_t i = 0; i < blockStmt->body.size(); i++)
        {
          result += printStmt(blockStmt->body[i], level + 4);
          if (i + 1 < blockStmt->body.size())
            result += ",\n";
        }
        result += "\n" + indentFunc(level + 2) + "]";
      }

      result += "\n" + indentFunc(level) + "}";
      return result;
    };

    output += "Program {\n";
    output += indentFunc(indent + 2) + "kind: \"Program\",\n";
    output += indentFunc(indent + 2) + "body: [\n";
    for (size_t i = 0; i < stmt->body.size(); i++)
    {
      output += printStmt(stmt->body[i], indent + 4);
      if (i + 1 < stmt->body.size())
        output += ",\n";
      else
        output += "\n";
    }
    output += indentFunc(indent + 2) + "]\n";
    output += "}\n";
  }
  catch (const std::exception &e)
  {
    output += "Error printing AST: ";
    output += e.what();
    output += "\n";
  }
  return output;
}

int main(int argc, char **argv)
{
  global::currfile = argv[1];
  std::ifstream file(global::currfile);
  if (!file) {
    std::cerr << "Unable to open file";
    return 1;
  }

  std::string input((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
  auto tokens = lexer::tokenize(input);
  if (global::errors.size() > 0)
  {
    std::cout << "Errors:\n";
    for (auto &err : global::errors)
    {
      err.print(&global::errors[0] != &err, &global::errors[global::errors.size() - 1] != &err);
    }
  }
  std::cout << "Tokens:\n";
  for (const auto &token : tokens)
  {
    token.debug();
  }
  // std::cout << "Parsing...\n";
  auto program = parser::parse(tokens);
  // std::cout << "Finished Parsing\n";
  if (global::errors.size() > 0)
  {
    std::cout << "Errors:\n";
    for (auto &err : global::errors)
    {
      err.print(&global::errors[0] != &err, &global::errors[global::errors.size() - 1] != &err);
    }
  }
  std::cout << "------------------------------------\n";
  std::string output = printAST(&program);
  std::cout << output;
  std::ofstream outFile("output.txt");
  if (outFile.is_open())
  {
    outFile << output;
    outFile.close();
  }
  else
  {
    std::cerr << "Unable to open output file";
    return 1;
  }
  return 0;
}
