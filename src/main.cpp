#include "./lexer/lexer.h"
#include "./parser/parser.h"
#include "./global.h"
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
static void printAST(const ast::Program *stmt, int indent = 0)
{
  try
  {
    if (!stmt)
      return;

    auto indentFunc = [](int indent)
    {
      for (int i = 0; i < indent; i++)
        std::cout << " ";
    };

    // Prints any Statement with dynamic checks for sub-fields
    std::function<void(const ast::Statement *, int)> printStmt;
    printStmt = [&](const ast::Statement *node, int level)
    {
      if (!node)
        return;
      indentFunc(level);
      std::cout << node->kind << " {\n";

      // Base fields
      indentFunc(level + 2);
      std::cout << "kind: \"" << node->kind << "\",\n";
      indentFunc(level + 2);
      std::cout << "linestart: " << node->linestart << ",\n";
      indentFunc(level + 2);
      std::cout << "lineend: " << node->lineend << ",\n";
      indentFunc(level + 2);
      std::cout << "columnstart: " << node->columnstart << ",\n";
      indentFunc(level + 2);
      std::cout << "columnend: " << node->columnend;

      // Handle specific node types
      if (auto exprStmt = dynamic_cast<const ast::ExpressionStatement *>(node))
      {
        std::cout << ",\n";
        indentFunc(level + 2);
        std::cout << "expression:\n";
        printStmt(exprStmt->expression, level + 4);
      }
      else if (auto binExpr = dynamic_cast<const ast::BinaryExpression *>(node))
      {
        std::cout << ",\n";
        indentFunc(level + 2);
        std::cout << "left:\n";
        printStmt(binExpr->left, level + 4);
        std::cout << ",\n";
        indentFunc(level + 2);
        std::cout << "op: {\n";
        indentFunc(level + 4);
        std::cout << "kind: \"" << lexer::tokenKindToString(binExpr->op.kind) << "\",\n";
        indentFunc(level + 4);
        std::cout << "value: \"" << binExpr->op.value << "\",\n";
        indentFunc(level + 4);
        std::cout << "linestart: " << binExpr->op.linestart << ",\n";
        indentFunc(level + 4);
        std::cout << "lineend: " << binExpr->op.lineend << ",\n";
        indentFunc(level + 4);
        std::cout << "columnstart: " << binExpr->op.columnstart << ",\n";
        indentFunc(level + 4);
        std::cout << "columnend: " << binExpr->op.columnend << "\n";
        indentFunc(level + 2);
        std::cout << "},\n";
        indentFunc(level + 2);
        std::cout << "right:\n";
        printStmt(binExpr->right, level + 4);
      }
      else if (auto numExpr = dynamic_cast<const ast::NumberExpression *>(node))
      {
        std::cout << ",\n";
        indentFunc(level + 2);
        std::cout << "value: " << numExpr->value;
      }
      else if (auto strExpr = dynamic_cast<const ast::StringExpression *>(node))
      {
        std::cout << ",\n";
        indentFunc(level + 2);
        std::cout << "value: \"" << strExpr->value << "\"";
      }
      else if (auto symExpr = dynamic_cast<const ast::SymbolExpression *>(node))
      {
        std::cout << ",\n";
        indentFunc(level + 2);
        std::cout << "value: \"" << symExpr->value << "\"";
      }
      else if (auto blockStmt = dynamic_cast<const ast::BlockStatement *>(node))
      {
        std::cout << ",\n";
        indentFunc(level + 2);
        std::cout << "body: [\n";
        for (size_t i = 0; i < blockStmt->body.size(); i++)
        {
          printStmt(blockStmt->body[i], level + 4);
          if (i + 1 < blockStmt->body.size())
            std::cout << ",\n";
        }
        std::cout << "\n";
        indentFunc(level + 2);
        std::cout << "]";
      }

      std::cout << "\n";
      indentFunc(level);
      std::cout << "}";
    };

    std::cout << "Program {\n";
    indentFunc(indent + 2);
    std::cout << "kind: \"Program\",\n";
    indentFunc(indent + 2);
    std::cout << "body: [\n";

    for (size_t i = 0; i < stmt->body.size(); i++)
    {
      printStmt(stmt->body[i], indent + 4);
      if (i + 1 < stmt->body.size())
        std::cout << ",\n";
      else
        std::cout << "\n";
    }

    indentFunc(indent + 2);
    std::cout << "]\n";
    std::cout << "}\n"; // Ensure closing bracket for Program
  }
  catch (const std::exception &e)
  {
    std::cerr << "Error printing AST: " << e.what() << std::endl;
  }
}

static std::string strOutput(const ast::Program *s, int indent = 0)
{
  std::string result;
  auto indentFunc = [](int indent) -> std::string
  {
    return std::string(indent, ' ');
  };

  std::function<void(const ast::Statement *, int)> printStmt;
  printStmt = [&](const ast::Statement *node, int level)
  {
    if (!node)
      return;
    result += indentFunc(level) + node->kind + " {\n";

    result += indentFunc(level + 2) + "kind: \"" + node->kind + "\",\n";
    result += indentFunc(level + 2) + "linestart: " + std::to_string(node->linestart) + ",\n";
    result += indentFunc(level + 2) + "lineend: " + std::to_string(node->lineend) + ",\n";
    result += indentFunc(level + 2) + "columnstart: " + std::to_string(node->columnstart) + ",\n";
    result += indentFunc(level + 2) + "columnend: " + std::to_string(node->columnend);

    if (auto exprStmt = dynamic_cast<const ast::ExpressionStatement *>(node))
    {
      result += ",\n" + indentFunc(level + 2) + "expression:\n";
      printStmt(exprStmt->expression, level + 4);
    }
    else if (auto binExpr = dynamic_cast<const ast::BinaryExpression *>(node))
    {
      result += ",\n" + indentFunc(level + 2) + "left:\n";
      printStmt(binExpr->left, level + 4);
      result += ",\n" + indentFunc(level + 2) + "op: {\n";
      result += indentFunc(level + 4) + "kind: \"" + lexer::tokenKindToString(binExpr->op.kind) + "\",\n";
      result += indentFunc(level + 4) + "value: \"" + binExpr->op.value + "\",\n";
      result += indentFunc(level + 4) + "linestart: " + std::to_string(binExpr->op.linestart) + ",\n";
      result += indentFunc(level + 4) + "lineend: " + std::to_string(binExpr->op.lineend) + ",\n";
      result += indentFunc(level + 4) + "columnstart: " + std::to_string(binExpr->op.columnstart) + ",\n";
      result += indentFunc(level + 4) + "columnend: " + std::to_string(binExpr->op.columnend) + "\n";
      result += indentFunc(level + 2) + "},\n";
      result += indentFunc(level + 2) + "right:\n";
      printStmt(binExpr->right, level + 4);
    }
    else if (auto numExpr = dynamic_cast<const ast::NumberExpression *>(node))
    {
      result += ",\n" + indentFunc(level + 2) + "value: " + std::to_string(numExpr->value);
    }
    else if (auto strExpr = dynamic_cast<const ast::StringExpression *>(node))
    {
      result += ",\n" + indentFunc(level + 2) + "value: \"" + strExpr->value + "\"";
    }
    else if (auto symExpr = dynamic_cast<const ast::SymbolExpression *>(node))
    {
      result += ",\n" + indentFunc(level + 2) + "value: \"" + symExpr->value + "\"";
    }
    else if (auto blockStmt = dynamic_cast<const ast::BlockStatement *>(node))
    {
      result += ",\n" + indentFunc(level + 2) + "body: [\n";
      for (size_t i = 0; i < blockStmt->body.size(); i++)
      {
        printStmt(blockStmt->body[i], level + 4);
        if (i + 1 < blockStmt->body.size())
          result += ",\n";
      }
      result += "\n" + indentFunc(level + 2) + "]";
    }

    result += "\n" + indentFunc(level) + "}"; // Ensure closing bracket for each node
  };

  result += "Program {\n" + indentFunc(indent + 2) + "kind: \"Program\",\n" + indentFunc(indent + 2) + "body: [\n";

  for (size_t i = 0; i < s->body.size(); i++)
  {
    printStmt(s->body[i], indent + 4);
    if (i + 1 < s->body.size())
      result += ",\n";
    else
      result += "\n";
  }

  result += indentFunc(indent + 2) + "]\n}\n"; // Ensure closing bracket for Program
  return result;
}

// TODO: Enhance main function to handle new command-line arguments or options if needed.
// Example:
// int main(int argc, char **argv) {
//   // Existing code...
//
//   // Handle additional arguments for extended features
//   // e.g., debugging options, verbose output, etc.
// }

int main(int argc, char **argv)
{
  global::currfile = argv[1];
  std::ifstream file(global::currfile);
  if (!file) {
    std::cerr << "Unable to open file";
    return 1;
  }

  std::string input((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
  std::cout << "Tokenizing...\n"; // Debug line
  auto tokens = lexer::tokenize(input);
  std::cout << "Finished tokenizing.\n"; // Debug line
  std::cout << "Tokens:\n";              // Debug line
  for (const auto &token : tokens)
  {
    token.debug(); // Debug line
  }
  std::cout << "Parsing...\n"; // Debug line

  parser::Parser parser(tokens);
  auto ast = parser.parse();

  std::cout << "Finished parsing.\n"; // Debug line
  if (global::errors.size() > 0)
  {
    std::cerr << global::errors.size() << " error(s) found:\n";
    for (auto &error : global::errors)
    {
      error.print(&global::errors[0] == &error, &global::errors.back() == &error);
    }
    return 1;
  }
  if (ast->body.empty())
  {
    std::cerr << "No statements parsed.\n";
    return 1;
  }

  // Enhanced Debugging: Verify AST Structure
  std::cout << "[DEBUG] AST Structure Verification:\n";
  for (size_t i = 0; i < ast->body.size(); ++i)
  {
    std::cout << "  Statement " << i + 1 << ": " << ast->body[i]->kind << "\n";
  }

  // TODO: Integrate new AST nodes into the AST printing and verification functions.
  // Example:
  // if (auto ifStmt = dynamic_cast<const ast::IfStatement*>(node)) {
  //   // Print details specific to IfStatement
  // }
  // Similarly handle other new statement types.

  std::cout << "AST:\n";
  printAST(ast.get()); // Debug line
                       /*
                         std::ofstream outfile("output.txt");
                         if (!outfile.is_open())
                         {
                           std::cerr << "Failed to open output.txt\n";
                           return 1;
                         }
                         std::cout << "Writing AST to output.txt...\n"; // Debug line
                     
                         outfile << strOutput(&ast) << "\n";
                         outfile.close(); */

  return 0;
}
