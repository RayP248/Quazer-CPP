#ifndef INTERPRETER_H
#define INTERPRETER_H

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <map>
#include <variant>
#include <memory>
#include <functional>
#include <optional>
#include <fstream>
#include <iostream>
#include "../ast/ast.h"
#include "../global.h"

namespace interpreter
{
  //*--------------
  //*    VALUES
  //*--------------
  struct runtime_value
  {
    bool returned_value = false;
    virtual ~runtime_value() = default;
    virtual std::string to_string() const
    {
      return "runtime_value";
    }
    virtual std::unique_ptr<runtime_value> clone() const = 0;
  };

  //*-------------------
  //*    ENVIRONMENT
  //*-------------------
  class Environment
  {
  private:
    std::unordered_map<std::string, std::unique_ptr<runtime_value>> variables; // Change to store by unique_ptr
    std::unordered_set<std::string> constants;
    std::unordered_set<std::string> public_variables;
    std::unordered_map<std::string, Environment *> imported_packages;
    std::string package_name = "";

  public:
    Environment *parent = nullptr;
    Environment(std::string package_name = "", Environment *parent = nullptr);
    std::unique_ptr<runtime_value> declare_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Statement *error_expression = nullptr, bool is_constant = false, bool is_public = false);
    runtime_value *get_object_of_property_path(const std::string &name, ast::Statement *error_expression);
    std::unique_ptr<runtime_value> assign_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Statement *error_expression);
    runtime_value *lookup_variable(std::string name, ast::Statement *error_expression);
    runtime_value *delete_variable(std::string name, ast::Statement *error_expression);
    bool is_variable_constant(std::string name);
    bool is_variable_public(std::string name);
    Environment *resolve(std::string name, ast::Statement *error_expression);
  };
  Environment *create_global_environment();

  //*----------------------
  //*    DEFINED VALUES
  //*----------------------

  struct null_value : runtime_value
  {
    void *value;
    null_value(bool is_returned = false)
    {
      value = nullptr;
      returned_value = is_returned;
      // [DEBUG**]std::cout << "[Debug] Created null_value\n"; // Debug
    }
    std::string to_string() const override
    {
      return "null";
    }
    null_value(const null_value &other, bool is_returned = false)
    {
      value = other.value;
      returned_value = is_returned;
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
      // [DEBUG**]std::cout << "[Debug] Created dummy_value\n";
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<dummy_value>(*this);
    }
  };

  struct number_value : runtime_value
  {
    double value;
    number_value(double val, bool is_returned = false) : value(val)
    {
      returned_value = is_returned;
      // [DEBUG**]std::cout << "[Debug] Created number_value: " << value << "\n"; // Debug
    }
    std::string to_string() const override
    {
      return std::to_string(value);
    }
    number_value(const number_value &other, bool is_returned = false)
    {
      value = other.value;
      returned_value = is_returned;
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      // [DEBUG**]std::cout << "[Debug] Cloning number_value: " << value << "\n"; // Debug
      auto finalval = std::make_unique<number_value>(value, returned_value);
      // [DEBUG**]std::cout << "[Debug] Cloned number_value: " << finalval->value << "\n"; // Debug
      return finalval;
    }
  };

  struct string_value : runtime_value
  {
    std::string value;
    string_value(std::string val, bool is_returned = false)
    {
      value = std::move(val);
      returned_value = is_returned;
      // [DEBUG**]std::cout << "[Debug] Created string_value: " << value << "\n"; // Debug
    }
    std::string to_string() const override
    {
      return value;
    }
    string_value(const string_value &other, bool is_returned = false)
    {
      value = other.value;
      returned_value = is_returned;
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<string_value>(*this);
    }
  };

  struct boolean_value : runtime_value
  {
    bool value;
    boolean_value(bool val, bool is_returned = false)
    {
      value = val;
      returned_value = is_returned;
      // [DEBUG**]std::cout << "[Debug] Created boolean_value: " << (value ? "true" : "false") << "\n"; // Debug
    }
    std::string to_string() const override
    {
      return value ? "true" : "false";
    }
    boolean_value(const boolean_value &other, bool is_returned = false)
    {
      value = other.value;
      returned_value = is_returned;
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<boolean_value>(*this);
    }
  };

  struct function_value : runtime_value
  {
    std::string name;
    std::vector<ast::ParameterExpression *> parameters;
    ast::BlockStatement *body;
    ast::ReturnStatement *return_statement;
    Environment *closure;
    function_value(std::string name, std::vector<ast::ParameterExpression *> parameters, ast::BlockStatement *body, ast::ReturnStatement *return_statement, interpreter::Environment *closure)
    {
      this->name = name;
      this->parameters = parameters;
      this->body = body;
      this->return_statement = return_statement;
      this->closure = closure;
    }
    std::string to_string() const override
    {
      std::string str;
      str += name + " ( ";
      for (size_t i = 0; i < parameters.size(); i++)
      {
        str += parameters[i]->name;
        if (i + 1 < parameters.size())
        {
          str += ", ";
        }
      }
      str += " ) { ... }";
      return str;
    }
    function_value(const function_value &other)
    {
      name = other.name;
      parameters = other.parameters;
      body = other.body;
      closure = other.closure;
      return_statement = other.return_statement;
    }
    ~function_value()
    {
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<function_value>(*this);
    }
  };

  struct native_function_value : runtime_value
  {
    std::string name;
    std::function<std::unique_ptr<runtime_value>(const std::vector<std::variant<std::unique_ptr<runtime_value>, ast::ReferenceExpression>> &, ast::Expression &, Environment *)> body;
    size_t arity;

    native_function_value(std::string name, int arity, std::function<std::unique_ptr<runtime_value>(const std::vector<std::variant<std::unique_ptr<runtime_value>, ast::ReferenceExpression>> &, ast::Expression &, Environment *)> body)
    {
      this->name = name;
      this->arity = arity;
      this->body = body;
    }
    std::string to_string() const override
    {
      return "<native_function>";
    }
    native_function_value(const native_function_value &other)
    {
      name = other.name;
      arity = other.arity;
      body = other.body;
    }
    ~native_function_value()
    {
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<native_function_value>(*this);
    }
  };

  struct array_value : runtime_value
  {
    std::vector<std::unique_ptr<runtime_value>> elements;
    array_value()
    {
      returned_value = false;
    }
    array_value(std::vector<std::unique_ptr<runtime_value>> elements, bool is_returned = false)
    {
      this->elements = std::move(elements);
      returned_value = is_returned;
    }
    array_value(const array_value &other)
    {
      for (const auto &element : other.elements)
      {
        elements.push_back(element->clone());
      }
      returned_value = other.returned_value;
    }
    std::string to_string() const override
    {
      std::string str = "[";
      for (size_t i = 0; i < elements.size(); i++)
      {
        str += elements[i]->to_string();
        if (i + 1 < elements.size())
        {
          str += ", ";
        }
      }
      str += "]";
      return str;
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      /*auto new_arr = std::make_unique<array_value>(*this);
      // Optionally deep-clone each element:
      for (size_t i = 0; i < new_arr->elements.size(); i++)
      {
        new_arr->elements[i] = std::unique_ptr<runtime_value>(
            this->elements[i]->clone().release());
      }
      return new_arr;*/
      return std::make_unique<array_value>(*this);
    }
  };

  struct object_value : runtime_value
  {
    std::map<std::string, std::unique_ptr<runtime_value>> properties;
    object_value(std::map<std::string, std::unique_ptr<runtime_value>> properties, bool is_returned = false)
    {
      this->properties = std::move(properties);
      returned_value = is_returned;
    }
    std::string to_string() const override
    {
      std::string str = "{";
      for (auto it = properties.begin(); it != properties.end(); ++it)
      {
        str += it->first + ": " + it->second->to_string();
        if (std::next(it) != properties.end())
        {
          str += ", ";
        }
      }
      str += "}";
      return str;
    }
    object_value(const object_value &other)
    {
      for (const auto &pair : other.properties)
      {
        properties[pair.first] = pair.second->clone();
      }
    }
    ~object_value()
    {
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<object_value>(*this);
    }
  };

  //*------------------
  //*    STATEMENTS
  //*------------------
  std::unique_ptr<runtime_value> interpret(ast::ASTVariant *statement, interpreter::Environment *env, bool is_returned);
  std::unique_ptr<runtime_value> interpret_program(ast::Program *program, Environment *env);
  std::unique_ptr<runtime_value> interpret_block_statement(ast::BlockStatement *program, Environment *env);
  std::unique_ptr<runtime_value> interpret_expression_statement(ast::ExpressionStatement *statement, interpreter::Environment *env, bool is_returned);
  std::unique_ptr<runtime_value> interpret_variable_declaration_statement(ast::VariableDeclarationStatement *statement, interpreter::Environment *env, bool is_returned);
  std::unique_ptr<runtime_value> interpret_return_statement(ast::ReturnStatement *statement, interpreter::Environment *env);
  std::unique_ptr<runtime_value> interpret_function_declaration_statement(ast::FunctionDeclarationStatement *statement, interpreter::Environment *env, bool is_returned);
  std::unique_ptr<runtime_value> interpret_if_statement(ast::IfStatement *statement, interpreter::Environment *env, bool is_returned);
  std::unique_ptr<runtime_value> interpret_for_loop_statement(ast::ForLoopStatement *statement, interpreter::Environment *env, bool is_returned);

  //*-------------------
  //*    EXPRESSIONS
  //*-------------------
  std::unique_ptr<runtime_value> interpret_binary_expression(ast::BinaryExpression *expression, interpreter::Environment *env, bool is_returned);
  std::unique_ptr<runtime_value> interpret_call_expression(ast::CallExpression *expression, interpreter::Environment *env, bool is_returned);
  std::unique_ptr<runtime_value> interpret_variable_declaration_expression(ast::VariableDeclarationExpression *expression, interpreter::Environment *env, bool is_returned);
  std::unique_ptr<runtime_value> interpret_assignment_expression(ast::AssignmentExpression *expresssion, interpreter::Environment *env, bool is_returned);
  std::unique_ptr<runtime_value> interpret_member_expression(ast::MemberExpression *expression, interpreter::Environment *env, bool is_returned);

  //*-------------------
  //*    MISC
  //*-------------------
  std::string print_value(const interpreter::runtime_value &value);
}

#endif // INTERPRETER_H
