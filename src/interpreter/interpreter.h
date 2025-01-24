#ifndef INTERPRETER_H
#define INTERPRETER_H

#include <string>
#include <unordered_map>
#include <unordered_set>
#include "../ast/ast.h"

namespace interpreter
{
  //*-------------------
  //*    INTERPRETER
  //*-------------------
  runtime_value interpret(ast::Statement *statement, Environment *env);

  //*--------------
  //*    VALUES
  //*--------------
  typedef std::string value_type;
  typedef std::string overlying_type;

  struct runtime_value;
  struct null_value;
  null_value make_null();
  struct number_value;
  number_value make_number(double value);
  struct string_value;
  string_value make_string(std::string value);
  struct boolean_value;
  boolean_value make_boolean(bool value);

  struct Variable;

  //*-------------------
  //*    ENVIRONMENT
  //*-------------------
  struct Environment
  {
  private:
    Environment *parent = nullptr;
    std::unordered_map<Variable, runtime_value> variables;
    std::unordered_set<std::string> constants;
    std::unordered_set<std::string> public_variables;
    std::unordered_map<std::string, Environment *> imported_packages;
    std::string package_name;

  public:
    Environment(std::string package_name, Environment *parent = nullptr);
    runtime_value declare_variable(std::string name, runtime_value value, value_type explicit_type, bool is_constant = false, bool is_public = false, ast::Expression error_expression);
    runtime_value assign_variable(std::string name, runtime_value value, ast::Expression error_expression);
    runtime_value lookup_variable(std::string name, ast::Expression error_expression);
    runtime_value resolve_variable(std::string name, ast::Expression error_expression);
  };
  Environment *create_global_environment();
}

#endif // INTERPRETER_H
