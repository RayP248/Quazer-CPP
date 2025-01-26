#include "interpreter.h"
#include "../error/error.h"
#include <variant>
#include <memory>
#include <cmath>
#include <fstream>

namespace interpreter
{
  //*--------------
  //*    VALUES
  //*--------------

  //*-------------------
  //*    INTERPRETER
  //*-------------------
  std::unique_ptr<runtime_value> interpret(ast::ASTVariant *statement, interpreter::Environment *env)
  {
    // std::cout << "[interpret] Starting interpretation\n";

    if (!statement)
    {
      // std::cout << "[interpret] Null statement received\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    // Convert all AST nodes to Statement*:
    ast::Statement *node = std::visit([](auto &&arg) -> ast::Statement *
                                      { return static_cast<ast::Statement *>(arg); }, *statement);

    if (!node)
    {
      // std::cout << "[interpret] Null statement received\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    // std::cout << "[interpret] Statement kind: " << ast::statement_kind_to_string(node->kind) << "\n";

    switch (node->kind)
    {
    case ast::StatementKind::PROGRAM:
    {
      auto program_node = dynamic_cast<ast::Program *>(node);
      return interpret_program(program_node, env);
    }
    case ast::StatementKind::EXPRESSION_STATEMENT:
    {
      auto expr_node = dynamic_cast<ast::ExpressionStatement *>(node);
      return interpret_expression_statement(expr_node, env);
    }
    case ast::StatementKind::NUMBER_EXPRESSION:
    {
      auto num_node = dynamic_cast<ast::NumberExpression *>(node);
      // std::cout << "[interpret] Interpreting number: " << num_node->value << "\n";
      return std::make_unique<number_value>(num_node->value);
    }
    case ast::StatementKind::SYMBOL_EXPRESSION:
    {
      auto sym_node = dynamic_cast<ast::SymbolExpression *>(node);
      // std::cout << "[interpret] Interpreting symbol: " << sym_node->value << "\n";
      if (sym_node)
      {
        auto val = env->lookup_variable(sym_node->value, sym_node);
        return val->clone();
      }
      return std::make_unique<null_value>();
    }
    case ast::StatementKind::STRING_EXPRESSION:
    {
      auto str_node = dynamic_cast<ast::StringExpression *>(node);
      // std::cout << "[interpret] Interpreting string: " << str_node->value << "\n";
      return std::make_unique<string_value>(str_node->value);
    }
    case ast::StatementKind::BINARY_EXPRESSION:
    {
      auto bin_node = dynamic_cast<ast::BinaryExpression *>(node);
      // std::cout << "[interpret] Interpreting binary expression with operator: " << bin_node->op.value << "\n";
      return interpret_binary_expression(bin_node, env);
    }
    case ast::StatementKind::VARIABLE_DECLARATION_STATEMENT:
    {
      auto var_decl_node = dynamic_cast<ast::VariableDeclarationStatement *>(node);
      return interpret_variable_declaration_statement(var_decl_node, env);
    }
    default:
      // std::cout << "[interpret] Unhandled statement kind: " << ast::statement_kind_to_string(std::get<ast::Statement *>(*statement)->kind) << "\n";
      return std::make_unique<null_value>();
    }
  }

  //*-------------------
  //*    ENVIRONMENT
  //*-------------------
  Environment::Environment(std::string package_name, Environment *parent)
  {
    this->package_name = std::move(package_name);
    this->parent = parent;
  }

  std::unique_ptr<runtime_value> Environment::declare_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Type *explicit_type, ast::VariableDeclarationStatement *error_expression, bool is_constant, bool is_public)
  {
    if (variables.find(name) != variables.end())
    {
      error::Error err(error::ErrorCode::RUNTIME_ERROR, "Variable '" + name + "' already declared in this scope.", error_expression->linestart, error_expression->lineend, error_expression->columnstart, error_expression->columnend, "interpreter.cpp : Environment::declare_variable : if", error::ErrorImportance::MODERATE);
    }
    variables.emplace(name, std::move(value));
    if (is_constant)
    {
      constants.insert(name);
    }
    if (is_public)
    {
      public_variables.insert(name);
    }
    if (explicit_type == nullptr)
    {
      variable_types.emplace(name, "nulltype");
    }
    else
    {
      variable_types.emplace(name, explicit_type->name);
    }
    return variables[name]->clone();
  }

  std::unique_ptr<runtime_value> Environment::assign_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Statement *error_expression)
  {
    Environment *env = resolve(name, error_expression);

    if (env->constants.find(name) != env->constants.end()) // Changed from contains to find
    {
      error::Error err(error::ErrorCode::RUNTIME_ERROR, "Cannot reassign to constant variable '" + name + "'.", error_expression->linestart, error_expression->lineend, error_expression->columnstart, error_expression->columnend, "interpreter.cpp : Environment::assign_variable : if", error::ErrorImportance::MODERATE);
    }

    env->variables[name] = std::move(value);
    return env->variables[name]->clone();
  }

  runtime_value *Environment::lookup_variable(std::string name, ast::Statement *error_expression)
  {
    Environment *env = Environment::resolve(name, error_expression);
    auto it = env->variables.find(name);
    if (it == env->variables.end())
    {
      error::Error err(
          error::ErrorCode::RUNTIME_ERROR,
          "Variable '" + name + "' not found.",
          error_expression->linestart,
          error_expression->lineend,
          error_expression->columnstart,
          error_expression->columnend,
          "interpreter.cpp : Environment::lookup_variable : if",
          error::ErrorImportance::MODERATE);
    }
    return it->second.get();
  }

  Environment *Environment::resolve(std::string name, ast::Statement *error_expression)
  {
    if (variables.find(name) != variables.end())
    {
      return this;
    }
    if (parent != nullptr)
    {
      return parent->resolve(name, error_expression);
    }
    else
    {
      error::Error err(
          error::ErrorCode::RUNTIME_ERROR,
          "Variable '" + name + "' not found.",
          error_expression->linestart,
          error_expression->lineend,
          error_expression->columnstart,
          error_expression->columnend,
          "interpreter.cpp : Environment::resolve : else",
          error::ErrorImportance::MODERATE);
    }
    return this;
  }

  Environment *create_global_environment()
  {
    Environment *global_env = new Environment();
    global_env->declare_variable("null", std::make_unique<null_value>(), new ast::Type("nulltype"), nullptr, true, false);
    global_env->declare_variable("true", std::make_unique<boolean_value>(true), new ast::Type("bool"), nullptr, true, false);
    global_env->declare_variable("false", std::make_unique<boolean_value>(false), new ast::Type("bool"), nullptr, true, false);
    return global_env;
  }

  //*------------------
  //*    STATEMENTS
  //*------------------
  std::unique_ptr<runtime_value> interpret_program(ast::Program *program, Environment *env)
  {
    // std::cout << "[interpret_program] Starting with " << program->body.size() << " statements\n";
    std::unique_ptr<runtime_value> result = std::make_unique<null_value>();

    for (const auto &stmt : program->body)
    {
      // std::cout << "[interpret_program] Interpreting statement of kind: " << ast::statement_kind_to_string(stmt->kind) << "\n";
      ast::ASTVariant variant = stmt;
      result = interpret(&variant, env);
    }

    return result;
  }

  std::unique_ptr<runtime_value> interpret_expression_statement(ast::ExpressionStatement *statement, interpreter::Environment *env)
  {
    auto exprStmt = dynamic_cast<ast::ExpressionStatement *>(statement);
    ast::ASTVariant variant = exprStmt->expression;
    return interpret(&variant, env);
  }

  std::unique_ptr<runtime_value> interpret_binary_expression(ast::BinaryExpression *expression, interpreter::Environment *env)
  {
    auto bin = dynamic_cast<ast::BinaryExpression *>(expression);
    if (!bin || !bin->left || !bin->right)
    {
      // std::cout << "[interpret_binary_expression] Invalid binary expression\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    ast::ASTVariant left_variant = bin->left;
    auto lhs = interpret(&left_variant, env);
    ast::ASTVariant right_variant = bin->right;
    auto rhs = interpret(&right_variant, env);

    // std::cout << "[interpret_binary_expression] " << bin->op.value
    //           << " LHS: " << lhs->to_string()
    //           << " RHS: " << rhs->to_string() << "\n";

    if (auto lnum = dynamic_cast<number_value *>(lhs.get()))
    {
      if (auto rnum = dynamic_cast<number_value *>(rhs.get()))
      {
        if (bin->op.value == "+")
        {
          return std::make_unique<number_value>(lnum->value + rnum->value);
        }
        else if (bin->op.value == "*")
        {
          return std::make_unique<number_value>(lnum->value * rnum->value);
        }
        else if (bin->op.value == "-")
        {
          return std::make_unique<number_value>(lnum->value - rnum->value);
        }
        else if (bin->op.value == "/")
        {
          if (rnum->value == 0)
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR, "Division by zero.", bin->linestart, bin->lineend, bin->columnstart, bin->columnend, "interpreter.cpp : interpret_binary_expression : if", error::ErrorImportance::MODERATE);
          }
          return std::make_unique<number_value>(lnum->value / rnum->value);
        }
        else if (bin->op.value == "%")
        {
          return std::make_unique<number_value>(std::fmod(lnum->value, rnum->value));
        }
        else if (bin->op.value == "==")
        {
          return std::make_unique<boolean_value>(lnum->value == rnum->value);
        }
        else if (bin->op.value == "!=")
        {
          return std::make_unique<boolean_value>(lnum->value != rnum->value);
        }
        else if (bin->op.value == ">")
        {
          return std::make_unique<boolean_value>(lnum->value > rnum->value);
        }
        else if (bin->op.value == ">=")
        {
          return std::make_unique<boolean_value>(lnum->value >= rnum->value);
        }
        else if (bin->op.value == "<")
        {
          return std::make_unique<boolean_value>(lnum->value < rnum->value);
        }
        else if (bin->op.value == "<=")
        {
          return std::make_unique<boolean_value>(lnum->value <= rnum->value);
        }
        else if (bin->op.value == "&&")
        {
          return std::make_unique<boolean_value>(lnum->value && rnum->value);
        }
        else if (bin->op.value == "||")
        {
          return std::make_unique<boolean_value>(lnum->value || rnum->value);
        }
        else
        {
          error::Error err(error::ErrorCode::RUNTIME_ERROR, "Invalid operator for number types: '" + bin->op.value + "'", bin->linestart, bin->lineend, bin->columnstart, bin->columnend, "interpreter.cpp : interpret_binary_expression : else", error::ErrorImportance::MODERATE);
        }
      }
    }

    return std::make_unique<null_value>();
  }

  std::unique_ptr<runtime_value> interpret_variable_declaration_statement(ast::VariableDeclarationStatement *statement, interpreter::Environment *env)
  {
    auto var_decl = dynamic_cast<ast::VariableDeclarationStatement *>(statement);
    std::unique_ptr<runtime_value> value = nullptr;

    if (var_decl->value)
    {
      ast::ASTVariant variant = var_decl->value;
      value = interpret(&variant, env);
    }
    std::unique_ptr<runtime_value> finalval = env->declare_variable(var_decl->name, std::move(value), &var_decl->type, var_decl, var_decl->is_const, var_decl->is_public);
    return finalval;
  }
  std::string print_value(const interpreter::runtime_value &value)
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
}
