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

    std::function<std::string(const ast::Type *, int)> printType;
    std::function<std::string(const ast::Statement *, int)> printStmt;
    std::function<std::string(const ast::Expression *, int)> printExpr;

    printType = [&](const ast::Type *node, int level) -> std::string
    {
      if (!node)
        return "";

      std::string result;
      result += indentFunc(level) + "Type {\n";
      result += indentFunc(level + 2) + "name: \"" + node->name + "\",\n";
      std::string generics_str = "[";
      for (size_t i = 0; i < node->generics.size(); ++i)
      {
        generics_str += printType(&node->generics[i], level + 2);
        if (i + 1 < node->generics.size())
          generics_str += ", ";
      }
      generics_str += "]";
      result += indentFunc(level + 2) + "generics: " + generics_str + ",\n";
      std::string fields_str = "[";
      for (size_t i = 0; i < node->fields.size(); ++i)
      {
        fields_str += printType(&node->fields[i], level + 2);
        if (i + 1 < node->fields.size())
          fields_str += ", ";
      }
      fields_str += "]";
      result += indentFunc(level + 2) + "is_inferred: " + std::to_string(node->is_inferred) + ",\n";
      result += indentFunc(level) + "}";
      return result;
    };

    printExpr = [&](const ast::Expression *node, int level) -> std::string
    {
      if (!node)
        return "";

      std::string result;
      result += indentFunc(level) + ast::statement_kind_to_string(node->kind) + " {\n";
      result += indentFunc(level + 2) + "kind: " + std::to_string(node->kind) + ",\n";
      result += indentFunc(level + 2) + "linestart: " + std::to_string(node->linestart) + ",\n";
      result += indentFunc(level + 2) + "lineend: " + std::to_string(node->lineend) + ",\n";
      result += indentFunc(level + 2) + "columnstart: " + std::to_string(node->columnstart) + ",\n";
      result += indentFunc(level + 2) + "columnend: " + std::to_string(node->columnend) + ",";
      if (auto binExpr = dynamic_cast<const ast::BinaryExpression *>(node))
      {
        result += "\n" + indentFunc(level + 2) + "left:\n" + printExpr(dynamic_cast<const ast::Expression *>(binExpr->left), level + 4) + ",\n";
        result += indentFunc(level + 2) + "op: {\n";
        result += indentFunc(level + 4) + "kind: \"" + lexer::token_kind_to_string(binExpr->op.kind) + "\",\n";
        result += indentFunc(level + 4) + "value: \"" + binExpr->op.value + "\",\n";
        result += indentFunc(level + 4) + "linestart: " + std::to_string(binExpr->op.linestart) + ",\n";
        result += indentFunc(level + 4) + "lineend: " + std::to_string(binExpr->op.lineend) + ",\n";
        result += indentFunc(level + 4) + "columnstart: " + std::to_string(binExpr->op.columnstart) + ",\n";
        result += indentFunc(level + 4) + "columnend: " + std::to_string(binExpr->op.columnend) + "\n";
        result += indentFunc(level + 2) + "},\n";
        result += indentFunc(level + 2) + "right:\n" + printExpr(dynamic_cast<const ast::Expression *>(binExpr->right), level + 4);
      }
      else if (auto numExpr = dynamic_cast<const ast::NumberExpression *>(node))
      {
        result += "\n" + indentFunc(level + 2) + "value: " + std::to_string(numExpr->value);
      }
      else if (auto strExpr = dynamic_cast<const ast::StringExpression *>(node))
      {
        result += "\n" + indentFunc(level + 2) + "value: \"" + strExpr->value + "\"";
      }
      else if (auto symExpr = dynamic_cast<const ast::SymbolExpression *>(node))
      {
        result += "\n" + indentFunc(level + 2) + "value: \"" + symExpr->value + "\"";
      }
      else if (auto paramExpr = dynamic_cast<const ast::ParameterExpression *>(node))
      {
        result += "\n" + indentFunc(level + 2) + "name: \"" + paramExpr->name + "\",\n";
        result += indentFunc(level + 2) + "type: \n" + printType(&paramExpr->type, level + 4);
      }
      else if (auto callExpr = dynamic_cast<const ast::CallExpression *>(node))
      {
        result += "\n" + indentFunc(level + 2) + "function:\n" + printExpr(callExpr->function, level + 4) + ",\n";
        result += indentFunc(level + 2) + "args: [\n";
        for (size_t i = 0; i < callExpr->args.size(); i++)
        {
          result += printExpr(callExpr->args[i], level + 4);
          if (i + 1 < callExpr->args.size())
            result += ",\n";
        }
        result += "\n" + indentFunc(level + 2) + "]";
      }

      result += "\n" + indentFunc(level) + "}";
      return result;
    };

    printStmt = [&](const ast::Statement *node, int level) -> std::string
    {
      if (!node)
        return "";

      std::string result;
      result += indentFunc(level) + ast::statement_kind_to_string(node->kind) + " {\n";
      result += indentFunc(level + 2) + "kind: " + std::to_string(node->kind) + ",\n";
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
      else if (auto var_decl_stmt = dynamic_cast<const ast::VariableDeclarationStatement *>(node))
      {
        result += ",\n" + indentFunc(level + 2) + "name: \"" + var_decl_stmt->name + "\",\n";
        result += indentFunc(level + 2) + "type: \n" + printType(&var_decl_stmt->type, level + 4) + ",\n";
        result += indentFunc(level + 2) + "value: \n" + printExpr(var_decl_stmt->value, level + 4) + ",\n";
        result += indentFunc(level + 2) + "is_const: " + (var_decl_stmt->is_const ? "true" : "false") + ",\n";
        result += indentFunc(level + 2) + "is_public: " + (var_decl_stmt->is_public ? "true" : "false");
      }
      else if (auto fn_decl_stmt = dynamic_cast<const ast::FunctionDeclarationStatement *>(node))
      {
        result += ",\n" + indentFunc(level + 2) + "name: \"" + fn_decl_stmt->name + "\",\n";
        result += indentFunc(level + 2) + "parameters: [\n";
        for (size_t i = 0; i < fn_decl_stmt->parameters.size(); i++)
        {
          result += printExpr(fn_decl_stmt->parameters[i], level + 4);
          if (i + 1 < fn_decl_stmt->parameters.size())
            result += ",\n";
        }
        result += "\n" + indentFunc(level + 2) + "],\n";
        result += indentFunc(level + 2) + "body: \n" + printStmt(fn_decl_stmt->body, level + 4) + ",\n";
        result += indentFunc(level + 2) + "return_type: \n" + printType(&fn_decl_stmt->return_type, level + 4);
        if (fn_decl_stmt->return_statement != nullptr)
        {
          result += "\n" + indentFunc(level + 2) + "return_statement: \n" + printStmt(fn_decl_stmt->return_statement, level + 4);
        }
        else
        {
          result += "\n" + indentFunc(level + 2) + "return_statement: nullptr";
        }
      }
      else if (auto return_stmt = dynamic_cast<const ast::ReturnStatement *>(node))
      {
        result += ",\n" + indentFunc(level + 2) + "value:\n";
        if (return_stmt != nullptr)
        {
          result += printExpr(return_stmt->value, level + 4);
        }
        else
        {
          result += indentFunc(level + 4) + "null";
        }
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

/*
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

*/
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
      err.print(&global::errors[0] == &err, &global::errors[global::errors.size() - 1] == &err);
    }
    return 1;
  }
  std::cout << "Tokens:\n";
  for (const auto &token : tokens)
  {
    token.debug();
  }
  std::cout << "------------------------------------\n";
  auto program = parser::parse(tokens);
  if (global::errors.size() > 0)
  {
    std::cout << "Errors:\n";
    for (auto &err : global::errors)
    {
      err.print(&global::errors[0] == &err, &global::errors[global::errors.size() - 1] == &err);
    }
    return 1;
  }
  std::cout << "Finished Parsing\n";
  std::string output = print_AST(&program);
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
  auto env = interpreter::create_global_environment();
  ast::ASTVariant variant_program = &program;
  auto result = interpreter::interpret(&variant_program, env, false);
  std::cout << "Final Result: " << print_value(*result) << "\n";
  return 0;
}
