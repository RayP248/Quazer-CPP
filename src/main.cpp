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
    {
      return output;
    }

    auto indentFunc = [](int indent)
    {
      return std::string(indent, ' ');
    };

    std::function<std::string(const std::pair<ast::Expression *, ast::Expression *>, int)> printProp;
    std::function<std::string(const ast::Statement *, int)> printStmt;
    std::function<std::string(const ast::Expression *, int)> printExpr;

    printProp = [&](const std::pair<ast::Expression *, ast::Expression *> node, int level) -> std::string
    {
      if (node.first == nullptr || node.second == nullptr)
      {
        return "";
      }

      std::string result;
      result += indentFunc(level) + "Property {\n";
      result += indentFunc(level + 2) + "key:\n" + printExpr(node.first, level + 4) + ",\n";
      result += indentFunc(level + 2) + "value:\n" + printExpr(node.second, level + 4);
      result += "\n" + indentFunc(level) + "}";
      return result;
    };

    printExpr = [&](const ast::Expression *node, int level) -> std::string
    {
      if (!node)
      {
        return "";
      }

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
      else if (auto varDeclExpr = dynamic_cast<const ast::VariableDeclarationExpression *>(node))
      {
        result += ",\n" + indentFunc(level + 2) + "name: \"" + varDeclExpr->name + "\",\n";
        result += indentFunc(level + 2) + "value: \n" + printExpr(varDeclExpr->value, level + 4) + ",\n";
        result += indentFunc(level + 2) + "is_const: " + (varDeclExpr->is_const ? "true" : "false") + ",\n";
        result += indentFunc(level + 2) + "is_public: " + (varDeclExpr->is_public ? "true" : "false");
      }
      else if (auto assignmentExpr = dynamic_cast<const ast::AssignmentExpression *>(node))
      {
        result += ",\n" + indentFunc(level + 2) + "left:\n" + printExpr(assignmentExpr->left, level + 4) + ",\n";
        result += indentFunc(level + 2) + "op: {\n";
        result += indentFunc(level + 4) + "kind: \"" + lexer::token_kind_to_string(assignmentExpr->op.kind) + "\",\n";
        result += indentFunc(level + 4) + "value: \"" + assignmentExpr->op.value + "\",\n";
        result += indentFunc(level + 4) + "linestart: " + std::to_string(assignmentExpr->op.linestart) + ",\n";
        result += indentFunc(level + 4) + "lineend: " + std::to_string(assignmentExpr->op.lineend) + ",\n";
        result += indentFunc(level + 4) + "columnstart: " + std::to_string(assignmentExpr->op.columnstart) + ",\n";
        result += indentFunc(level + 4) + "columnend: " + std::to_string(assignmentExpr->op.columnend) + "\n";
        result += indentFunc(level + 2) + "},\n";
        result += indentFunc(level + 2) + "right:\n" + printExpr(assignmentExpr->right, level + 4) + ",\n";
        result += indentFunc(level + 2) + "increment_decrement: " + (assignmentExpr->increment_decrement ? "true" : "false");
      }
      else if (auto memberExpr = dynamic_cast<const ast::MemberExpression *>(node))
      {
        result += ",\n" + indentFunc(level + 2) + "object:\n" + printExpr(memberExpr->object, level + 4) + ",\n";
        result += indentFunc(level + 2) + "property:\n" + printExpr(memberExpr->property, level + 4);
      }
      else if (auto arrayExpr = dynamic_cast<const ast::ArrayExpression *>(node))
      {
        result += "\n" + indentFunc(level + 2) + "elements: [\n";
        for (size_t i = 0; i < arrayExpr->elements.size(); i++)
        {
          result += printExpr(arrayExpr->elements[i], level + 4);
          if (i + 1 < arrayExpr->elements.size())
            result += ",\n";
        }
        result += "\n" + indentFunc(level + 2) + "]";
      }
      else if (auto objectExpr = dynamic_cast<const ast::ObjectExpression *>(node))
      {
        result += "\n" + indentFunc(level + 2) + "properties: [\n";
        for (size_t i = 0; i < objectExpr->properties.size(); i++)
        {
          result += printProp(objectExpr->properties[i], level + 4);
          if (i + 1 < objectExpr->properties.size())
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
      {
        return "";
      }

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

        result += "\n" + indentFunc(level + 2) + "return_statement: nullptr";
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
      else if (auto if_stmt = dynamic_cast<const ast::IfStatement *>(node))
      {
        result += ",\n" + indentFunc(level + 2) + "condition:\n" + printExpr(if_stmt->condition, level + 4) + ",\n";
        result += indentFunc(level + 2) + "then_branch:\n" + printStmt(if_stmt->then_branch, level + 4) + ",\n";
        result += indentFunc(level + 2) + (if_stmt->else_branch != nullptr ? "else_branch" : "else_if_branch") + ":\n" + printStmt(if_stmt->else_branch != nullptr ? static_cast<const ast::Statement *>(if_stmt->else_branch) : static_cast<const ast::Statement *>(if_stmt->else_if_branch), level + 4);
        return result;
      }
      else if (auto for_loop_stmt = dynamic_cast<const ast::ForLoopStatement *>(node))
      {
        result += ",\n" + indentFunc(level + 2) + "initializer:\n" + printExpr(for_loop_stmt->initializer, level + 4) + ",\n";
        result += indentFunc(level + 2) + "condition:\n" + printExpr(for_loop_stmt->condition, level + 4) + ",\n";
        result += indentFunc(level + 2) + "post:\n" + printExpr(for_loop_stmt->post, level + 4) + ",\n";
        result += indentFunc(level + 2) + "body:\n" + printStmt(for_loop_stmt->body, level + 4);
        return result;
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
    std::cerr << "Unable to open file.";
    exit(1);
  }

  if (global::currfile.rfind(".qz") != global::currfile.size() - 3)
  {
    std::cerr << "Invalid file type. Expected .qz file.";
    exit(1);
  }

  std::string input((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
  auto tokens = lexer::tokenize(input);
  error::display_all_errors(false);
  /* std::cout << "Tokens:\n";
  for (const auto &token : tokens)
  {
    token.debug();
  }
  std::cout << "------------------------------------\n"; */
  auto program = parser::parse(tokens);
  error::display_all_errors(true);
  // std::cout << "Finished Parsing\n";
  std::string output = print_AST(&program);
  // std::cout << "AST:\n";
  // std::cout << output;
  std::ofstream outFile("output.txt");
  if (outFile.is_open())
  {
    outFile << output;
    outFile.close();
  }
  else
  {
    std::cerr << "Unable to open output file";
    exit(1);
  }
  // std::cout << "Creating global env\n";
  auto env = interpreter::create_global_environment();
  // std::cout << "Interpreting program\n";
  ast::ASTVariant variant_program = &program;
  auto result = interpreter::interpret(&variant_program, env, false);
  error::display_all_errors(true);
  // std::cout << "Final Result: " << print_value(*result) << "\n";
  exit(0);
}
