#ifndef INTERPRETER_H
#define INTERPRETER_H

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <memory>
#include "../ast/ast.h"

namespace interpreter
{
  //*--------------
  //*    VALUES
  //*--------------
  struct runtime_value
  {
    std::string type;
    virtual ~runtime_value() = default;
    virtual std::string to_string() const
    {
      return type;
    }
  };

  struct null_value : runtime_value
  {
    void *value;
    null_value()
    {
      value = nullptr;
      type = "null";
    }
    std::string to_string() const override
    {
      return "null";
    }
  };

  struct number_value : runtime_value
  {
    double value;
    number_value(double val)
    {
      type = "number"; // Make sure type is set
      value = val;
    }
    std::string to_string() const override
    {
      return std::to_string(value);
    }
  };

  struct string_value : runtime_value
  {
    std::string value;
    string_value(std::string val)
    {
      value = std::move(val);
      type = "string";
    }
    std::string to_string() const override
    {
      return value;
    }
  };

  struct boolean_value : runtime_value
  {
    bool value;
    boolean_value(bool val)
    {
      value = val;
      type = "boolean";
    }
    std::string to_string() const override
    {
      return value ? "true" : "false";
    }
  };

  //*-------------------
  //*    ENVIRONMENT
  //*-------------------
  class Environment
  {
  private:
    Environment *parent = nullptr;
    std::unordered_map<std::string, std::unique_ptr<runtime_value>> variables; // Change to store by unique_ptr
    std::unordered_set<std::string> constants;
    std::unordered_set<std::string> public_variables;
    std::unordered_map<std::string, std::string> variable_types;
    std::unordered_map<std::string, Environment *> imported_packages;
    std::string package_name = "";

  public:
    Environment(std::string package_name = "", Environment *parent = nullptr);
    std::unique_ptr<runtime_value> declare_variable(std::string name, std::unique_ptr<runtime_value> value, std::string explicit_type, ast::Statement *error_expression, bool is_constant, bool is_public);
    std::unique_ptr<runtime_value> assign_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Statement *error_expression);
    runtime_value *lookup_variable(std::string name, ast::Statement *error_expression);
    Environment *resolve(std::string name, ast::Statement *error_expression);
  };
  Environment *create_global_environment();

  //*------------------
  //*    STATEMENTS
  //*------------------
  using ASTVariant = std::variant<
      ast::Program *,
      ast::ExpressionStatement *,
      ast::BinaryExpression *,
      ast::NumberExpression *,
      ast::StringExpression *,
      ast::SymbolExpression *,
      ast::Statement *,   // Fallback
      ast::Expression *>; // Fallback

  std::unique_ptr<runtime_value> interpret(ASTVariant *statement, interpreter::Environment *env);
  std::unique_ptr<runtime_value> interpret_program(ast::Program *program, Environment *env);
  std::unique_ptr<runtime_value> interpret_expression_statement(ast::ExpressionStatement *statement, interpreter::Environment *env);
  std::unique_ptr<runtime_value> interpret_binary_expression(ast::BinaryExpression *expression, interpreter::Environment *env);
}

#endif // INTERPRETER_H
