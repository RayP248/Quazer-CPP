#include "./global.h"
#include "./lexer/lexer.h"
#include "./parser/parser.h"
#include "./interpreter/interpreter.h"
#include <string>
#include <iostream>
#include <fstream>
#include <functional>
#include <variant>

/*
Input code:
45.2 + 5 * 4

Desired structure:
Program {
  kind: "Program",
  body: [
    ExpressionStatement {
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

static std::string print_AST(const ast::Program *stmt, int indent = 0)
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
      result += indentFunc(level) + ast::statement_kind_to_string(node->kind) + " {\n";
      result += indentFunc(level + 2) + "kind: \"" + ast::statement_kind_to_string(node->kind) + "\",\n";

      if (auto binExpr = dynamic_cast<const ast::BinaryExpression *>(node))
      {
        result += indentFunc(level + 2) + "left:\n" + printExpr(dynamic_cast<const ast::Expression *>(binExpr->left), level + 4) + ",\n";
        result += indentFunc(level + 2) + "op: {\n";
        result += indentFunc(level + 4) + "kind: \"" + lexer::token_kind_to_string(binExpr->op.kind) + "\",\n";
        result += indentFunc(level + 4) + "value: \"" + binExpr->op.value + "\",\n";
        result += indentFunc(level + 4) + "linestart: " + std::to_string(binExpr->op.linestart) + ",\n";
        result += indentFunc(level + 4) + "lineend: " + std::to_string(binExpr->op.lineend) + ",\n";
        result += indentFunc(level + 4) + "columnstart: " + std::to_string(binExpr->op.columnstart) + ",\n";
        result += indentFunc(level + 4) + "columnend: " + std::to_string(binExpr->op.columnend) + "\n";
        result += indentFunc(level + 2) + "},\n";
        result += indentFunc(level + 2) + "right:\n" + printExpr(dynamic_cast<const ast::Expression *>(binExpr->right), level + 4) + "\n";
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
      result += indentFunc(level) + ast::statement_kind_to_string(node->kind) + " {\n";
      result += indentFunc(level + 2) + "kind: \"" + ast::statement_kind_to_string(node->kind) + "\",\n";
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

static std::string print_value(const interpreter::runtime_value &value)
{
  // filepath: /y:/Lexar/C++Projects/Quazer/src/main.cpp
  if (value.type == "number")
  {
    const interpreter::number_value &num = static_cast<const interpreter::number_value &>(value);
    return std::to_string(num.value);
  }
  else if (value.type == "string")
  {
    const interpreter::string_value &str = static_cast<const interpreter::string_value &>(value);
    return str.value;
  }
  else if (value.type == "boolean")
  {
    const interpreter::boolean_value &bool_val = static_cast<const interpreter::boolean_value &>(value);
    return bool_val.value ? "true" : "false";
  }
  else if (value.type == "null")
  {
    return "null";
  }
  // Add more types as needed
  return "Unknown";
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
  std::cout << "Tokens:\n"; // commented out
  for (const auto &token : tokens)
  {
    token.debug(); // commented out
  }
  std::cout << "------------------------------------\n"; // commented out
  auto program = parser::parse(tokens);
  // std::cout << "Finished Parsing\n";
  std::string output = print_AST(&program);
  std::cout << output; // commented out
  std::ofstream outFile("output.txt");
  if (outFile.is_open())
  {
    // outFile << output; // commented out
    outFile.close();
  }
  else
  {
    std::cerr << "Unable to open output file";
    return 1;
  }
  interpreter::Environment *global_env = interpreter::create_global_environment();
  interpreter::ASTVariant program_variant = &program;
  std::unique_ptr<interpreter::runtime_value> result = interpreter::interpret(&program_variant, global_env);
  if (global::errors.size() > 0)
  {
    std::cout << "Errors:\n";
    for (auto &err : global::errors)
    {
      err.print(&global::errors[0] != &err, &global::errors[global::errors.size() - 1] != &err);
    }
    std::cout << "------------------------------------\n";
  }
  std::cout << "Result: " << print_value(*result) << std::endl; // commented out
  return 0;
}
