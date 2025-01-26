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
    virtual std::unique_ptr<runtime_value> clone() const = 0;
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
    null_value(const null_value &other)
    {
      value = other.value;
      type = other.type;
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<null_value>(*this);
    }
  };

  struct dummy_value : runtime_value
  {
    dummy_value()
    {
      type = "dummy";
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<dummy_value>(*this);
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
    number_value(const number_value &other)
    {
      value = other.value;
      type = other.type;
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<number_value>(*this);
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
    string_value(const string_value &other)
    {
      value = other.value;
      type = other.type;
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<string_value>(*this);
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
    boolean_value(const boolean_value &other)
    {
      value = other.value;
      type = other.type;
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<boolean_value>(*this);
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
    std::unique_ptr<runtime_value> declare_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Type *explicit_type, ast::VariableDeclarationStatement *error_expression = nullptr, bool is_constant = false, bool is_public = false);
    std::unique_ptr<runtime_value> assign_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Statement *error_expression);
    runtime_value *lookup_variable(std::string name, ast::Statement *error_expression);
    Environment *resolve(std::string name, ast::Statement *error_expression);
  };
  Environment *create_global_environment();

  //*------------------
  //*    STATEMENTS
  //*------------------
  std::unique_ptr<runtime_value> interpret(ast::ASTVariant *statement, interpreter::Environment *env);
  std::unique_ptr<runtime_value> interpret_program(ast::Program *program, Environment *env);
  std::unique_ptr<runtime_value> interpret_expression_statement(ast::ExpressionStatement *statement, interpreter::Environment *env);
  std::unique_ptr<runtime_value> interpret_variable_declaration_statement(ast::VariableDeclarationStatement *statement, interpreter::Environment *env);

  //*-------------------
  //*    EXPRESSIONS
  //*-------------------
  std::unique_ptr<runtime_value> interpret_binary_expression(ast::BinaryExpression *expression, interpreter::Environment *env);

  //*-------------------
  //*    MISC
  //*-------------------
  std::string print_value(const interpreter::runtime_value &value);
}

#endif // INTERPRETER_H
