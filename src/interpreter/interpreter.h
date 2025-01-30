#ifndef INTERPRETER_H
#define INTERPRETER_H

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <memory>
#include <functional>
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
    std::string type;
    bool returned_value = false;
    virtual ~runtime_value() = default;
    virtual std::string to_string() const
    {
      return type;
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
    std::unordered_map<std::string, std::string> variable_types;
    std::unordered_map<std::string, Environment *> imported_packages;
    std::string package_name = "";

  public:
    Environment *parent = nullptr;
    Environment(std::string package_name = "", Environment *parent = nullptr);
    std::unique_ptr<runtime_value> declare_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Type *explicit_type, ast::Statement *error_expression = nullptr, bool is_constant = false, bool is_public = false);
    std::unique_ptr<runtime_value> assign_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Statement *error_expression);
    runtime_value *lookup_variable(std::string name, ast::Statement *error_expression);
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
      type = "null";
      returned_value = is_returned;
      /* [DEBUG**] */ std::cout << "[Debug] Created null_value\n"; // Debug
    }
    std::string to_string() const override
    {
      return "null";
    }
    null_value(const null_value &other, bool is_returned = false)
    {
      value = other.value;
      type = other.type;
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
      type = "dummy";
      /* [DEBUG**] */ std::cout << "[Debug] Created dummy_value\n"; // Debug
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<dummy_value>(*this);
    }
  };

  struct number_value : runtime_value
  {
    double value;
    number_value(double val, bool is_returned = false)
    {
      type = "number"; // Make sure type is set
      value = val;
      returned_value = is_returned;
      /* [DEBUG**] */ std::cout << "[Debug] Created number_value: " << value << "\n"; // Debug
    }
    std::string to_string() const override
    {
      return std::to_string(value);
    }
    number_value(const number_value &other, bool is_returned = false)
    {
      value = other.value;
      type = other.type;
      returned_value = is_returned;
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<number_value>(*this);
    }
  };

  struct string_value : runtime_value
  {
    std::string value;
    string_value(std::string val, bool is_returned = false)
    {
      value = std::move(val);
      type = "string";
      returned_value = is_returned;
      /* [DEBUG**] */ std::cout << "[Debug] Created string_value: " << value << "\n"; // Debug
    }
    std::string to_string() const override
    {
      return value;
    }
    string_value(const string_value &other, bool is_returned = false)
    {
      value = other.value;
      type = other.type;
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
      type = "boolean";
      returned_value = is_returned;
      /* [DEBUG**] */ std::cout << "[Debug] Created boolean_value: " << (value ? "true" : "false") << "\n"; // Debug
    }
    std::string to_string() const override
    {
      return value ? "true" : "false";
    }
    boolean_value(const boolean_value &other, bool is_returned = false)
    {
      value = other.value;
      type = other.type;
      returned_value = is_returned;
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<boolean_value>(*this);
    }
  };
  /*
    struct type_value : runtime_value {
      std::string name;
      std::vector<ast::Type> generics;
      std::vector<ast::Type> fields;
      bool is_inferred = false;
      type_value(std::string name)
      {
        this->name = name;
        type = "type";
      }
      std::string to_string() const override
      {
        return name;
      }
      type_value(const type_value &other)
      {
        name = other.name;
        type = other.type;
        generics = other.generics;
        fields = other.fields;
      }
      std::unique_ptr<runtime_value> clone() const override
      {
        return std::make_unique<type_value>(*this);
      }
    };

    struct parameter_value : runtime_value
    {
      std::string name;
      type_value *type;
      parameter_value(std::string name)
      {
        this->name = name;
        type = new type_value("any");
      }
      std::string to_string() const override
      {
        return name;
      }
      parameter_value(const parameter_value &other)
      {
        name = other.name;
        type = other.type;
      }
      std::unique_ptr<runtime_value> clone() const override
      {
        return std::make_unique<parameter_value>(*this);
      }
    };
  */

  struct function_value : runtime_value
  {
    std::string name;
    std::vector<ast::ParameterExpression *> parameters;
    ast::BlockStatement *body;
    ast::Type return_type;
    ast::ReturnStatement *return_statement;
    Environment *closure;
    function_value(std::string name, std::vector<ast::ParameterExpression *> parameters, ast::BlockStatement *body, ast::Type return_type, ast::ReturnStatement *return_statement, interpreter::Environment *closure)
    {
      this->name = name;
      this->parameters = parameters;
      this->body = body;
      this->return_type = return_type;
      this->return_statement = return_statement;
      this->closure = closure;
      type = "function";
    }
    std::string to_string() const override
    {
      std::string str;
      str += name + " ( ";
      for (size_t i = 0; i < parameters.size(); i++)
      {
        str += parameters[i]->name;
        if (parameters[i]->type.name != "")
        {
          str += ": ";
          str += parameters[i]->type.name;
        }
        if (i + 1 < parameters.size())
        {
          str += ", ";
        }
      }
      str += " ) -> ";
      str += return_type.name;
      str += " {\n";
      {
        std::ifstream file(global::currfile);
        if (file.is_open())
        {
          std::vector<std::string> lines;
          std::string rawline;
          while (std::getline(file, rawline))
          {
            lines.push_back(rawline);
          }

          int snippetStart = body->lineend - 1 > 5 ? body->lineend - 5 : 1;
          for (int i = snippetStart; i < body->lineend - 1; i++)
          {
            std::cout << lines[i] << "\n";
          }
        }
      }
      str += "}";
      return str;
    }
    function_value(const function_value &other)
    {
      name = other.name;
      parameters = other.parameters;
      body = other.body;
      closure = other.closure;
      return_type = other.return_type;
      return_statement = other.return_statement;
      type = other.type;
    }
    ~function_value()
    {
      // Removed calls to delete body and parameters to avoid double-free
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<function_value>(*this);
    }
  };
  struct native_function_value : runtime_value
  {
    std::string name;
    std::function<std::unique_ptr<runtime_value>(std::vector<std::unique_ptr<runtime_value>> &, interpreter::Environment *)> body;
    ast::Type return_type;
    native_function_value(std::string name, std::function<std::unique_ptr<runtime_value>(std::vector<std::unique_ptr<runtime_value>> &, interpreter::Environment *)> body, ast::Type return_type)
    {
      this->name = name;
      this->body = body;
      this->return_type = return_type;
      type = "native_function";
    }
    std::string to_string() const override
    {
      return "native_function";
    }
    native_function_value(const native_function_value &other)
    {
      name = other.name;
      body = other.body;
      return_type = other.return_type;
      type = other.type;
    }
    ~native_function_value()
    {
    }
    std::unique_ptr<runtime_value> clone() const override
    {
      return std::make_unique<native_function_value>(*this);
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

  //*-------------------
  //*    EXPRESSIONS
  //*-------------------
  std::unique_ptr<runtime_value> interpret_binary_expression(ast::BinaryExpression *expression, interpreter::Environment *env, bool is_returned);
  std::unique_ptr<runtime_value> interpret_call_expression(ast::CallExpression *expression, interpreter::Environment *env, bool is_returned);

  //*-------------------
  //*    MISC
  //*-------------------
  std::string print_value(const interpreter::runtime_value &value);
}

#endif // INTERPRETER_H
