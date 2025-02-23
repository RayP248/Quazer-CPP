#include "interpreter.h"
#include "../error/error.h"
#include "../parser/parser.h"
#include <variant>
#include <memory>
#include <cmath>
#include <fstream>
#include <functional>
#include <regex>
#include <algorithm>
#include <cstdlib> // For malloc and free
namespace interpreter
{
  //*--------------
  //*    VALUES
  //*--------------

  //*-------------------
  //*    INTERPRETER
  //*-------------------
  std::unique_ptr<runtime_value> interpret(ast::ASTVariant *statement, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpret] Starting interpretation\n";

    if (!statement)
    {
      /* [DEBUG**] */ std::cout << "[interpret] Null statement received\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }
    // Convert all AST nodes to Statement*:
    ast::Statement *node = std::visit([](auto &&arg) -> ast::Statement *
                                      { return static_cast<ast::Statement *>(arg); }, *statement);

    if (!node)
    {
      /* [DEBUG**] */ std::cout << "[interpret] Null statement received\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    /* [DEBUG**] */ std::cout << "[interpret] Statement kind: " << ast::statement_kind_to_string(node->kind) << "\n";

    switch (node->kind)
    {
    case ast::StatementKind::PROGRAM:
    {
      auto program_node = dynamic_cast<ast::Program *>(node);
      return interpret_program(program_node, env);
    }
    case ast::StatementKind::BLOCK_STATEMENT:
    {
      auto block_node = dynamic_cast<ast::BlockStatement *>(node);
      return interpret_block_statement(block_node, env);
    }
    case ast::StatementKind::EXPRESSION_STATEMENT:
    {
      auto expr_node = dynamic_cast<ast::ExpressionStatement *>(node);
      return interpret_expression_statement(expr_node, env, is_returned);
    }
    case ast::StatementKind::REFERENCE_EXPRESSION:
    {
      auto ref_node = dynamic_cast<ast::ReferenceExpression *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting symbol: " << ref_node->name << "\n";
      if (ref_node)
      {
        auto val = env->lookup_variable(ref_node->name, ref_node);
        return val->clone();
      }
      return std::make_unique<null_value>();
    }
    case ast::StatementKind::NUMBER_EXPRESSION:
    {
      auto num_node = dynamic_cast<ast::NumberExpression *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting number: " << num_node->value << "\n";
      return std::make_unique<number_value>(num_node->value, is_returned);
    }
    case ast::StatementKind::SYMBOL_EXPRESSION:
    {
      auto sym_node = dynamic_cast<ast::SymbolExpression *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting symbol: " << sym_node->value << "\n";
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
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting string: " << str_node->value << "\n";
      return std::make_unique<string_value>(str_node->value, is_returned);
    }
    case ast::StatementKind::ARRAY_EXPRESSION:
    {
      auto arr_node = dynamic_cast<ast::ArrayExpression *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting array expression\n";
      std::vector<std::unique_ptr<runtime_value>> elements;
      for (auto *expr : arr_node->elements)
      {
        ast::ASTVariant variant = expr;
        elements.push_back(std::unique_ptr<runtime_value>(interpret(&variant, env, is_returned).release()));
      }
      return std::make_unique<array_value>(std::move(elements), is_returned);
    }
    case ast::StatementKind::OBJECT_EXPRESSION:
    {
      auto obj_node = dynamic_cast<ast::ObjectExpression *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting object expression\n";
      std::map<std::string, std::unique_ptr<runtime_value>> properties;
      for (auto &[key, value] : obj_node->properties)
      {
        std::string key_str;
        if (auto str_expr = dynamic_cast<ast::StringExpression *>(key))
        {
          key_str = str_expr->value;
        }
        else if (auto sym_expr = dynamic_cast<ast::SymbolExpression *>(key))
        {
          key_str = sym_expr->value;
        }
        else
        {
          key_str = "";
        }
        ast::ASTVariant variant = value;
        properties.emplace(key_str, std::unique_ptr<runtime_value>(interpret(&variant, env, is_returned).release()));
      }
      return std::make_unique<object_value>(std::move(properties), is_returned);
    }
    case ast::StatementKind::BINARY_EXPRESSION:
    {
      auto bin_node = dynamic_cast<ast::BinaryExpression *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting binary expression with operator: " << bin_node->op.value << "\n";
      auto result = interpret_binary_expression(bin_node, env, is_returned);
      /* [DEBUG**] */ std::cout << "[interpret] Result of bin expr: " << result->to_string() << "\n";
      return result;
    }
    case ast::StatementKind::VARIABLE_DECLARATION_STATEMENT:
    {
      auto var_decl_node = dynamic_cast<ast::VariableDeclarationStatement *>(node);
      return interpret_variable_declaration_statement(var_decl_node, env, is_returned);
    }
    case ast::StatementKind::RETURN_STATEMENT:
    {
      auto return_node = dynamic_cast<ast::ReturnStatement *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting return statement\n";
      return interpret_return_statement(return_node, env);
    }
    case ast::StatementKind::FUNCTION_DECLARATION_STATEMENT:
    {
      auto fn_decl_node = dynamic_cast<ast::FunctionDeclarationStatement *>(node);
      return interpret_function_declaration_statement(fn_decl_node, env, is_returned);
    }
    case ast::StatementKind::CALL_EXPRESSION:
    {
      auto call_node = dynamic_cast<ast::CallExpression *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting call expression\n";
      return interpret_call_expression(call_node, env, is_returned);
    }
    case ast::StatementKind::IF_STATEMENT:
    {
      auto if_node = dynamic_cast<ast::IfStatement *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting if statement\n";
      return interpret_if_statement(if_node, env, is_returned);
    }
    case ast::StatementKind::FOR_LOOP_STATEMENT:
    {
      auto for_node = dynamic_cast<ast::ForLoopStatement *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting for loop statement\n";
      return interpret_for_loop_statement(for_node, env, is_returned);
    }
    case ast::StatementKind::VARIABLE_DECLARATION_EXPRESSION:
    {
      auto var_decl_node = dynamic_cast<ast::VariableDeclarationExpression *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting variable declaration expression\n";
      return interpret_variable_declaration_expression(var_decl_node, env, is_returned);
    }
    case ast::StatementKind::ASSIGNMENT_EXPRESSION:
    {
      auto assign_node = dynamic_cast<ast::AssignmentExpression *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting assignment expression\n";
      return interpret_assignment_expression(assign_node, env, is_returned);
    }
    case ast::StatementKind::MEMBER_EXPRESSION:
    {
      auto mem_node = dynamic_cast<ast::MemberExpression *>(node);
      /* [DEBUG**] */ std::cout << "[interpret] Interpreting member expression\n";
      return interpret_member_expression(mem_node, env, is_returned);
    }
    default:
      return std::make_unique<null_value>(is_returned);
    }
    return std::make_unique<null_value>(is_returned);
  }

  //*-------------------
  //*    ENVIRONMENT
  //*-------------------
  Environment::Environment(std::string package_name, Environment *parent)
  {
    this->package_name = std::move(package_name);
    this->parent = parent;
    /* [DEBUG**] */ std::cout << "[Environment] Created environment with package name: " << this->package_name << "\n";
  }

  std::unique_ptr<runtime_value> Environment::declare_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Statement *error_expression, bool is_constant, bool is_public)
  {
    /* [DEBUG**] */ std::cout << "[declare_variable] Declaring variable: " << name << "\n";
    if (variables.find(name) != variables.end())
    {
      error::Error err(error::ErrorCode::RUNTIME_ERROR, "Variable '" + name + "' already declared in this scope.", error_expression->linestart, error_expression->lineend, error_expression->columnstart, error_expression->columnend, "interpreter.cpp : Environment::declare_variable : if", error::ErrorImportance::MODERATE);
      error::display_all_errors(false);
      if (variables.find(name) != variables.end())
      {
        if (variables.find(name) != variables.end() && variables[name])
        {
          return variables[name]->clone();
        }
        else
        {
          error::Error err(error::ErrorCode::RUNTIME_ERROR, "Variable '" + name + "' not found or is null.", error_expression->linestart, error_expression->lineend, error_expression->columnstart, error_expression->columnend, "interpreter.cpp : Environment::declare_variable : clone", error::ErrorImportance::MODERATE);
          error::display_all_errors(false);
          return std::make_unique<null_value>();
        }
      }
      else
      {
        error::Error err(error::ErrorCode::RUNTIME_ERROR, "Variable '" + name + "' not found.", error_expression->linestart, error_expression->lineend, error_expression->columnstart, error_expression->columnend, "interpreter.cpp : Environment::declare_variable : else", error::ErrorImportance::MODERATE);
        error::display_all_errors(false);
        return std::make_unique<null_value>();
      }
    }
    /* [DEBUG**] */ std::cout << "[declare_variable] Variable not found in current scope\n";
    variables.emplace(name, std::move(value));
    /* [DEBUG**] */ std::cout << "[declare_variable] Variable inserted into map\n";
    /* [DEBUG**] */ std::cout << "[declare_variable] Variable is " << (is_constant ? "constant" : "not constant") << "\n";
    if (is_constant)
    {
      /* [DEBUG**] */ std::cout << "[declare_variable] Variable is constant\n";
      constants.insert(name);
    }
    /* [DEBUG**] */ std::cout << "[declare_variable] Variable is " << (is_public ? "public" : "not public") << "\n";
    if (is_public)
    {
      /* [DEBUG**] */ std::cout << "[declare_variable] Variable is public\n";
      public_variables.insert(name);
    }
    /* [DEBUG**] */ std::cout << "[declare_variable] Returning variable: " << name << "\n";
    /* [DEBUG**] */ std::cout << "[declare_variable] Variable is: " << variables[name]->clone()->to_string() << "\n";
    return variables[name]->clone();
  }

  runtime_value *Environment::get_object_of_property_path(const std::string &name, ast::Statement *error_expression)
  {
    std::vector<std::string> tokens;
    size_t pos = 0, found = 0;
    while ((found = name.find('.', pos)) != std::string::npos)
    {
      tokens.push_back(name.substr(pos, found - pos));
      pos = found + 1;
    }
    tokens.push_back(name.substr(pos));

    Environment *resolvedEnv = Environment::resolve(tokens[0], error_expression);
    auto it = resolvedEnv->variables.find(tokens[0]);
    if (it == resolvedEnv->variables.end())
    {
      error::Error err(
          error::ErrorCode::RUNTIME_ERROR,
          "Variable '" + tokens[0] + "' not found.",
          error_expression->linestart,
          error_expression->lineend,
          error_expression->columnstart,
          error_expression->columnend,
          "interpreter.cpp : Environment::resolve_property_path : first token",
          error::ErrorImportance::MODERATE);
    }
    runtime_value *current = it->second.get();

    for (size_t i = 1; i < tokens.size() - 1; i++)
    {
      auto obj_val = dynamic_cast<object_value *>(current);
      if (!obj_val)
      {
        error::Error err(
            error::ErrorCode::RUNTIME_ERROR,
            "Attempted to access property '" + tokens[i] + "' on a non-object value.",
            error_expression->linestart,
            error_expression->lineend,
            error_expression->columnstart,
            error_expression->columnend,
            "interpreter.cpp : Environment::resolve_property_path : member access",
            error::ErrorImportance::MODERATE);
      }
      auto propIt = obj_val->properties.find(tokens[i]);
      if (propIt == obj_val->properties.end())
      {
        error::Error err(
            error::ErrorCode::RUNTIME_ERROR,
            "Property '" + tokens[i] + "' not found in the object.",
            error_expression->linestart,
            error_expression->lineend,
            error_expression->columnstart,
            error_expression->columnend,
            "interpreter.cpp : Environment::resolve_property_path : property lookup",
            error::ErrorImportance::MODERATE);
      }
      current = propIt->second.get();
    }
    return current;
  }

  std::unique_ptr<runtime_value> Environment::assign_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Statement *error_expression)
  {
    /* [DEBUG**] */ std::cout << "[assign_variable] Assigning variable: " << name << "\n";

    // Check for dot notation
    if (name.find('.') != std::string::npos)
    {
      runtime_value *parent_obj = get_object_of_property_path(name, error_expression);
      auto obj_val = dynamic_cast<object_value *>(parent_obj);
      if (!obj_val)
      {
        error::Error err(
            error::ErrorCode::RUNTIME_ERROR,
            "Attempted to assign to property '" + name + "' on a non-object value.",
            error_expression->linestart,
            error_expression->lineend,
            error_expression->columnstart,
            error_expression->columnend,
            "interpreter.cpp : Environment::assign_variable : dot-notation",
            error::ErrorImportance::MODERATE);
      }
      obj_val->properties[name.substr(name.find_last_of('.') + 1)] = std::move(value);
      return obj_val->properties[name.substr(name.find_last_of('.') + 1)]->clone();
    }
    else
    {
      /* [DEBUG**] */ std::cout << "[assign_variable] Resolving environment for variable: " << name << "\n";
      Environment *env = resolve(name, error_expression);
      /* [DEBUG**] */ std::cout << "[assign_variable] Resolved environment for variable: " << name << "\n";
      if (env->constants.find(name) != env->constants.end())
      {
        /* [DEBUG**] */ std::cout << "[assign_variable] Variable is constant: " << name << "\n";
        error::Error err(
            error::ErrorCode::RUNTIME_ERROR,
            "Cannot reassign to constant variable '" + name + "'.",
            error_expression->linestart,
            error_expression->lineend,
            error_expression->columnstart,
            error_expression->columnend,
            "interpreter.cpp : Environment::assign_variable : if",
            error::ErrorImportance::MODERATE);
        /* [DEBUG**] */ std::cout << "[assign_variable] Error displayed\n";
        return env->variables[name]->clone();
      }
      /* [DEBUG**] */ std::cout << "[assign_variable] Assigning variable: " << name << "\n";
      env->variables[name] = std::move(value);
      /* [DEBUG**] */ std::cout << "[assign_variable] Variable assigned: " << name << " with value of: " << env->variables[name]->clone()->to_string() << "\n";
      return env->variables[name]->clone();
    }
  }

  runtime_value *Environment::lookup_variable(std::string name, ast::Statement *error_expression)
  {
    if (name.find('.') != std::string::npos)
    {
      runtime_value *parent_obj = get_object_of_property_path(name, error_expression);
      auto obj_val = dynamic_cast<object_value *>(parent_obj);
      if (!obj_val)
      {
        error::Error err(
            error::ErrorCode::RUNTIME_ERROR,
            "Attempted to access property '" + name + "' on a non-object value.",
            error_expression->linestart,
            error_expression->lineend,
            error_expression->columnstart,
            error_expression->columnend,
            "interpreter.cpp : Environment::lookup_variable : dot-notation",
            error::ErrorImportance::MODERATE);
      }
      return obj_val->properties[name.substr(name.find_last_of('.') + 1)].get();
    }
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
    return it->second->clone().release();
  }

  runtime_value *Environment::delete_variable(std::string name, ast::Statement *error_expression)
  {
    /* [DEBUG**] */ std::cout << "[delete_variable] Deleting variable: " << name << "\n";
    if (name.find('.') != std::string::npos)
    {
      runtime_value *parent_obj = get_object_of_property_path(name, error_expression);
      auto obj_val = dynamic_cast<object_value *>(parent_obj);
      if (!obj_val)
      {
        error::Error err(
            error::ErrorCode::RUNTIME_ERROR,
            "Attempted to access property '" + name + "' on a non-object value.",
            error_expression->linestart,
            error_expression->lineend,
            error_expression->columnstart,
            error_expression->columnend,
            "interpreter.cpp : Environment::delete_variable : dot-notation",
            error::ErrorImportance::MODERATE);
      }
      obj_val->properties.erase(name.substr(name.find_last_of('.') + 1));
      return std::make_unique<null_value>().get();
    }
    else
    {
      /* [DEBUG**] */ std::cout << "[delete_variable] Resolving environment for variable: " << name << "\n";
      Environment *env = Environment::resolve(name, error_expression);
      /* [DEBUG**] */ std::cout << "[delete_variable] Looking up variable: " << name << "\n";
      auto it = env->variables.find(name);
      if (it == env->variables.end())
      {
        /* [DEBUG**] */ std::cout << "[delete_variable] Variable not found: " << name << "\n";
        error::Error err(
            error::ErrorCode::RUNTIME_ERROR,
            "Variable '" + name + "' not found.",
            error_expression->linestart,
            error_expression->lineend,
            error_expression->columnstart,
            error_expression->columnend,
            "interpreter.cpp : Environment::delete_variable : if",
            error::ErrorImportance::MODERATE);
        return std::make_unique<null_value>().get();
      }
      /* [DEBUG**] */ std::cout << "[delete_variable] Erasing variable: " << name << "\n";
      env->variables.erase(it);
      if (env->constants.find(name) != env->constants.end())
      {
        /* [DEBUG**] */ std::cout << "[delete_variable] Erasing constant: " << name << "\n";
        env->constants.erase(name);
      }
      if (env->public_variables.find(name) != env->public_variables.end())
      {
        /* [DEBUG**] */ std::cout << "[delete_variable] Erasing public variable: " << name << "\n";
        env->public_variables.erase(name);
      }
      /* [DEBUG**] */ std::cout << "[delete_variable] Returning null value for variable: " << name << "\n";
      return std::make_unique<null_value>().get();
    }
  }

  bool Environment::is_variable_constant(std::string name)
  {
    return constants.find(name) != constants.end();
  }

  bool Environment::is_variable_public(std::string name)
  {
    return public_variables.find(name) != public_variables.end();
  }

  Environment *Environment::resolve(std::string name, ast::Statement *error_expression)
  {
    /* [DEBUG**] */ std::cout << "[resolve] Resolving variable: " << name << "\n";
    if (variables.find(name) != variables.end())
    {
      /* [DEBUG**] */ std::cout << "[resolve] Found variable in current environment\n";
      return this;
    }
    if (parent != nullptr)
    {
      /* [DEBUG**] */ std::cout << "[resolve] Variable not found in current environment, checking parent\n";
      return parent->resolve(name, error_expression);
    }
    else
    {
      /* [DEBUG**] */ std::cout << "[resolve] Variable not found in any environment\n";
      error::Error err(
          error::ErrorCode::RUNTIME_ERROR,
          "Variable '" + name + "' not found.",
          error_expression->linestart,
          error_expression->lineend,
          error_expression->columnstart,
          error_expression->columnend,
          "interpreter.cpp : Environment::resolve : else",
          error::ErrorImportance::CRITICAL);
      error::display_all_errors(true);
      return nullptr; // Ensure a return value
    }
  }

  Environment *create_global_environment()
  {
    /* [DEBUG**] */ std::cout << "[create_global_environment] Creating global environment\n";
    Environment *global_env = new Environment();
    global_env->declare_variable("null", std::make_unique<null_value>(false), nullptr, true, false);
    global_env->declare_variable("true", std::make_unique<boolean_value>(true, false), nullptr, true, false);
    global_env->declare_variable("false", std::make_unique<boolean_value>(false, false), nullptr, true, false);

    auto out_fn = std::make_unique<native_function_value>(
        "out",
        -1,
        [](const std::vector<std::variant<std::unique_ptr<runtime_value>, ast::ReferenceExpression>> &args, ast::Expression &error_expression, Environment *env) -> std::unique_ptr<runtime_value>
        {
          for (const auto &arg : args)
          {
            std::string s = std::visit([](auto const &v) -> std::string
                                       {
                if constexpr (std::is_same_v<std::decay_t<decltype(v)>, std::unique_ptr<runtime_value>>)
                    return v ? v->to_string() : "";
                else if constexpr (std::is_same_v<std::decay_t<decltype(v)>, ast::ReferenceExpression>)
                    return "";
                else
                    return ""; }, arg);
            for (size_t i = 0; i < s.size(); ++i)
            {
              if (s[i] == '\\' && i + 1 < s.size() && s[i + 1] == 'n')
              {
                std::cout << "\n";
                i++;
              }
              else
              {
                std::cout << s[i];
              }
            }
          }
          std::cout << std::flush;
          return std::make_unique<null_value>(false);
        });
    auto outln_fn = std::make_unique<native_function_value>(
        "outln",
        -1,
        [](const std::vector<std::variant<std::unique_ptr<runtime_value>, ast::ReferenceExpression>> &args, ast::Expression &error_expression, Environment *env) -> std::unique_ptr<runtime_value>
        {
          for (const auto &arg : args)
          {
            std::string s = std::visit([](auto const &v) -> std::string
                                       {
                if constexpr (std::is_same_v<std::decay_t<decltype(v)>, std::unique_ptr<runtime_value>>)
                    return v ? v->to_string() : "";
                else if constexpr (std::is_same_v<std::decay_t<decltype(v)>, ast::ReferenceExpression>)
                    return "";
                else
                    return ""; }, arg);
            for (size_t i = 0; i < s.size(); ++i)
            {
              if (s[i] == '\\' && i + 1 < s.size() && s[i + 1] == 'n')
              {
                std::cout << "\n";
                i++;
              }
              else
              {
                std::cout << s[i];
              }
            }
          }
          std::cout << "\n";
          std::cout << std::flush;
          return std::make_unique<null_value>(false);
        });

    auto in = std::make_unique<native_function_value>(
        "in",
        -1,
        [](const std::vector<std::variant<std::unique_ptr<runtime_value>, ast::ReferenceExpression>> &args, ast::Expression &error_expression, Environment *env) -> std::unique_ptr<runtime_value>
        {
          for (const auto &arg : args)
          {
            std::string s = std::visit([](auto &&v) -> std::string
                                       {
                if constexpr (std::is_same_v<std::decay_t<decltype(v)>, std::unique_ptr<runtime_value>>)
                    return v ? v->to_string() : "";
                else if constexpr (std::is_same_v<std::decay_t<decltype(v)>, ast::ReferenceExpression>)
                    return "";
                else
                    return ""; }, arg);
            std::cout << s;
          }
          std::string input;
          std::getline(std::cin, input);
          return std::make_unique<string_value>(input, false);
        });
    auto alloc = std::make_unique<native_function_value>(
        "alloc",
        2,
        [](const std::vector<std::variant<std::unique_ptr<runtime_value>, ast::ReferenceExpression>> &args, ast::Expression &error_expression, Environment *env) -> std::unique_ptr<runtime_value>
        {
          runtime_value *value_rv = nullptr;
          if (auto value_ptr_variant = std::get_if<std::unique_ptr<runtime_value>>(&args[0]))
          {
            value_rv = value_ptr_variant->get();
          }
          else
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR,
                             "Invalid first argument to alloc.",
                             error_expression.linestart, error_expression.lineend,
                             error_expression.columnstart, error_expression.columnend,
                             "interpreter.cpp : alloc", error::ErrorImportance::CRITICAL);
            error::display_all_errors(true);
            return std::make_unique<null_value>(false);
          }

          auto size_ptr_variant = std::get_if<std::unique_ptr<runtime_value>>(&args[1]);
          if (!size_ptr_variant)
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR,
                             "Invalid second argument to alloc.",
                             error_expression.linestart, error_expression.lineend,
                             error_expression.columnstart, error_expression.columnend,
                             "interpreter.cpp : alloc", error::ErrorImportance::CRITICAL);
            error::display_all_errors(true);
            return std::make_unique<null_value>(false);
          }
          auto size = dynamic_cast<number_value *>(size_ptr_variant->get());
          if (!size)
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR,
                             "Second argument to alloc must be a number.",
                             error_expression.linestart, error_expression.lineend,
                             error_expression.columnstart, error_expression.columnend,
                             "interpreter.cpp : alloc", error::ErrorImportance::CRITICAL);
            error::display_all_errors(true);
            return std::make_unique<null_value>(false);
          }
          if (auto arr = dynamic_cast<array_value *>(value_rv))
          {
            std::vector<std::unique_ptr<runtime_value>> elems;
            elems.reserve(static_cast<size_t>(size->value));
            for (size_t i = 0; i < static_cast<size_t>(size->value); ++i)
            {
              if (i < arr->elements.size())
              {
                elems.push_back(arr->elements[i]->clone());
              }
            }
            return std::make_unique<array_value>(std::move(elems), arr->returned_value);
          }
          else if (auto str = dynamic_cast<string_value *>(value_rv))
          {
            std::string s = str->value;
            s.resize(static_cast<size_t>(size->value));
            return std::make_unique<string_value>(s, str->returned_value);
          }
          else
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR,
                             "First argument to alloc must be an array, string, or number.",
                             error_expression.linestart, error_expression.lineend,
                             error_expression.columnstart, error_expression.columnend,
                             "interpreter.cpp : alloc", error::ErrorImportance::CRITICAL);
            return std::make_unique<null_value>(false);
          }
          return std::make_unique<null_value>(false);
        });
    auto free = std::make_unique<native_function_value>(
        "free",
        1,
        [](const std::vector<std::variant<std::unique_ptr<runtime_value>, ast::ReferenceExpression>> &args,
           ast::Expression &error_expression, Environment *env) -> std::unique_ptr<runtime_value>
        {
          auto args0 = &args[0];
          const ast::ReferenceExpression *ref = std::get_if<ast::ReferenceExpression>(args0);
          if (!ref)
          {
            // Errors are behaving weirdly by shifting to the right by 1, so we shift them back
            error::Error err(error::ErrorCode::RUNTIME_ERROR,
                             "Invalid argument to free. Must be a reference to a variable.",
                             error_expression.linestart, error_expression.lineend,
                             error_expression.columnstart, error_expression.columnend,
                             "interpreter.cpp : free", error::ErrorImportance::CRITICAL);
            error::display_all_errors(true);
            return std::make_unique<null_value>(false);
          }
          error_expression.columnstart--;
          error_expression.columnend--;
          env->delete_variable(ref->name, &error_expression);
          error::display_all_errors(true);
          return std::make_unique<null_value>(false);
        });
    auto append = std::make_unique<native_function_value>(
        "append",
        2,
        [](const std::vector<std::variant<std::unique_ptr<runtime_value>, ast::ReferenceExpression>> &args,
           ast::Expression &error_expression, Environment *env) -> std::unique_ptr<runtime_value>
        {
          runtime_value *value_rv = nullptr;
          const ast::ReferenceExpression *value_ref = nullptr;
          if (auto value_ptr_variant = std::get_if<std::unique_ptr<runtime_value>>(&args[0]))
          {
            value_rv = value_ptr_variant->get();
            std::cout << "[DEBUG] value_rv: " << (value_rv ? value_rv->to_string() : "null") << "\n";
          }
          else if (auto ref_expr_variant = std::get_if<ast::ReferenceExpression>(&args[0]))
          {
            value_ref = ref_expr_variant;
            std::cout << "[DEBUG] value_ref: " << (value_ref ? value_ref->name : "null") << "\n";
          }
          else
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR,
                             "Invalid first argument to append.",
                             error_expression.linestart, error_expression.lineend,
                             error_expression.columnstart, error_expression.columnend,
                             "interpreter.cpp : append", error::ErrorImportance::CRITICAL);
            error::display_all_errors(true);
            return std::make_unique<null_value>(false);
          }

          auto elem_ptr_variant = std::get_if<std::unique_ptr<runtime_value>>(&args[1]);
          if (!elem_ptr_variant)
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR,
                             "Invalid second argument to append.",
                             error_expression.linestart, error_expression.lineend,
                             error_expression.columnstart, error_expression.columnend,
                             "interpreter.cpp : append", error::ErrorImportance::CRITICAL);
            error::display_all_errors(true);
            return std::make_unique<null_value>(false);
          }
          runtime_value *elem = elem_ptr_variant->get();
          std::cout << "[DEBUG] elem: " << (elem ? elem->to_string() : "nullt") << "\n";
          if (!elem)
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR,
                             "Second argument to append must be a value.",
                             error_expression.linestart, error_expression.lineend,
                             error_expression.columnstart, error_expression.columnend,
                             "interpreter.cpp : append", error::ErrorImportance::CRITICAL);
            error::display_all_errors(true);
            return std::make_unique<null_value>(false);
          }
          if (auto arr = dynamic_cast<array_value *>(value_rv))
          {
            arr->elements.push_back(elem->clone());
            std::cout << "[DEBUG] array after append: " << arr->to_string() << "\n";
            return arr->clone();
          }
          else if (auto ref = dynamic_cast<ast::ReferenceExpression *>(const_cast<ast::ReferenceExpression *>(value_ref)))
          {
            auto var = env->lookup_variable(ref->name, ref);
            std::cout << "[DEBUG] var: " << (var ? var->to_string() : "null") << "\n";
            if (auto arr = dynamic_cast<array_value *>(var))
            {
              arr->elements.push_back(elem->clone());
              auto updated = std::make_unique<array_value>(std::move(arr->elements), arr->returned_value);
              std::cout << "[DEBUG] updated array: " << updated->to_string() << "\n";
              return std::move(updated);
            }
          }
          else
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR,
                             "First argument to append must be an array.",
                             error_expression.linestart, error_expression.lineend,
                             error_expression.columnstart, error_expression.columnend,
                             "interpreter.cpp : append", error::ErrorImportance::CRITICAL);
            return std::make_unique<null_value>(false);
          }
          return std::make_unique<null_value>(false);
        });
    global_env->declare_variable("out", std::move(out_fn), nullptr, true, false);
    global_env->declare_variable("outln", std::move(outln_fn), nullptr, true, false);
    global_env->declare_variable("in", std::move(in), nullptr, true, false);
    global_env->declare_variable("alloc", std::move(alloc), nullptr, true, false);
    global_env->declare_variable("free", std::move(free), nullptr, true, false);
    global_env->declare_variable("append", std::move(append), nullptr, true, false);
    return global_env;
  }

  //*------------------
  //*    STATEMENTS
  //*------------------
  std::unique_ptr<runtime_value> interpret_program(ast::Program *program, Environment *env)
  {
    /* [DEBUG**] */ std::cout << "[interpret_program] Starting with " << program->body.size() << " statements\n";
    std::unique_ptr<runtime_value> result = std::make_unique<null_value>(false);

    for (const auto &stmt : program->body)
    {
      /* [DEBUG**] */ std::cout << "[interpret_program] Interpreting statement of kind: " << ast::statement_kind_to_string(stmt->kind) << "\n";
      ast::ASTVariant variant = stmt;
      result = interpret(&variant, env, false);
    }

    /* [DEBUG**] */ std::cout << "[interpret_program] Finished interpreting program with result of " << result->to_string() << "\n";
    return result;
  }

  std::unique_ptr<runtime_value> interpret_block_statement(ast::BlockStatement *program, Environment *env)
  {
    /* [DEBUG**] */ std::cout << "[interpret_block_statement] Starting with " << program->body.size() << " statements\n";
    std::unique_ptr<runtime_value> result = std::make_unique<null_value>(false);

    for (const auto &stmt : program->body)
    {
      /* [DEBUG**] */ std::cout << "[interpret_block_statement] Interpreting statement of kind: " << ast::statement_kind_to_string(stmt->kind) << "\n";
      ast::ASTVariant variant = stmt;
      std::unique_ptr<runtime_value> res = interpret(&variant, env, false);
      if (res->returned_value || stmt->kind == ast::StatementKind::RETURN_STATEMENT)
      {
        /* [DEBUG**] */ std::cout << "[interpret_block_statement] Found return statement\n";
        result = std::move(res);
        break;
      }
      else
      {
        result = std::move(res);
      }
    }

    return result;
  }

  std::unique_ptr<runtime_value> interpret_expression_statement(ast::ExpressionStatement *statement, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpret_expression_statement] Interpreting expression statement\n";
    auto exprStmt = dynamic_cast<ast::ExpressionStatement *>(statement);
    ast::ASTVariant variant = exprStmt->expression;
    auto result = interpret(&variant, env, is_returned);
    /* [DEBUG**] */ std::cout << "[interpret_expression_statement] Result of expression: " << result->to_string() << "\n";
    return result;
  }

  std::unique_ptr<runtime_value> interpret_variable_declaration_statement(ast::VariableDeclarationStatement *statement, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpret_variable_declaration_statement] Interpreting variable declaration statement\n";
    auto var_decl = dynamic_cast<ast::VariableDeclarationStatement *>(statement);
    std::unique_ptr<runtime_value> value = nullptr;

    if (var_decl->value)
    {
      ast::ASTVariant variant = var_decl->value;
      value = interpret(&variant, env, is_returned);
    }
    std::unique_ptr<runtime_value> finalval = env->declare_variable(var_decl->name, std::move(value), var_decl, var_decl->is_const, var_decl->is_public);
    return finalval;
  }

  std::unique_ptr<runtime_value> interpret_return_statement(ast::ReturnStatement *statement, interpreter::Environment *env)
  {
    /* [DEBUG**] */ std::cout << "[interpret_return_statement] Interpreting return statement\n";
    auto return_stmt = dynamic_cast<ast::ReturnStatement *>(statement);
    std::unique_ptr<runtime_value> value = nullptr;

    if (return_stmt->value)
    {
      ast::ASTVariant variant = return_stmt->value;
      value = interpret(&variant, env, true);
    }

    return value;
  }

  std::unique_ptr<runtime_value> interpret_function_declaration_statement(ast::FunctionDeclarationStatement *statement, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpret_function_declaration_statement] Interpreting function declaration statement\n";
    auto fn_decl = dynamic_cast<ast::FunctionDeclarationStatement *>(statement);
    std::unique_ptr<runtime_value> fn = env->declare_variable(fn_decl->name, std::make_unique<function_value>(fn_decl->name, fn_decl->parameters, fn_decl->body, fn_decl->return_statement, env), fn_decl, false, false);
    return fn;
  }

  std::unique_ptr<runtime_value> interpret_if_statement(ast::IfStatement *statement, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpret_if_statement] Interpreting if statement\n";
    auto if_stmt = dynamic_cast<ast::IfStatement *>(statement);
    ast::ASTVariant condition_variant = if_stmt->condition;
    std::unique_ptr<runtime_value> condition = interpret(&condition_variant, env, false);
    if (auto cond_val = dynamic_cast<boolean_value *>(condition.get()))
    {
      if (cond_val->value)
      {
        ast::ASTVariant then_variant = if_stmt->then_branch;
        return interpret(&then_variant, env, false);
      }
      else if (if_stmt->else_if_branch != nullptr)
      {
        return interpret_if_statement(if_stmt->else_if_branch, env, is_returned);
      }
      else if (if_stmt->else_branch != nullptr)
      {
        ast::ASTVariant else_variant = if_stmt->else_branch;
        return interpret(&else_variant, env, is_returned);
      }
    }
    return std::make_unique<null_value>(false);
  };

  std::unique_ptr<runtime_value> interpret_for_loop_statement(ast::ForLoopStatement *statement, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpret_for_loop_statement] Interpreting for loop statement\n";
    auto for_loop = dynamic_cast<ast::ForLoopStatement *>(statement);
    /* [DEBUG**] */ std::cout << "[interpret_for_loop_statement] For loop with array of: " << (for_loop->of_loop) << "\n";
    if (for_loop->of_loop == false)
    {
      /* Create a new child environment for the loop scope */
      Environment *loopEnv = new Environment("", env);
      if (for_loop->initializer)
      {
        ast::ASTVariant init_variant = for_loop->initializer;
        interpret(&init_variant, loopEnv, false);
      }
      else
      {
        /* [DEBUG**] */ std::cout << "[interpret_for_loop_statement] No initializer found\n";
      }
      int num_iterations = 0;
      std::cout << "[interpret_for_loop_statement] Starting while loop\n";
      while (true)
      {
        std::cout << "[interpret_for_loop_statement] Iteration: " << num_iterations << "\n";
        ast::ASTVariant cond_variant = for_loop->condition;
        std::cout << "[interpret_for_loop_statement] Evaluating loop condition\n";
        auto cond_val = interpret(&cond_variant, loopEnv, false);
        std::cout << "[interpret_for_loop_statement] Condition evaluated to: " << cond_val->to_string() << "\n";
        auto cond_bool = dynamic_cast<boolean_value *>(cond_val.get());
        if (!cond_bool || !cond_bool->value)
        {
          std::cout << "[interpret_for_loop_statement] Condition false, breaking loop\n";
          break;
        }
        if (for_loop->body)
        {
          std::cout << "[interpret_for_loop_statement] Evaluating loop body\n";
          ast::ASTVariant body_variant = for_loop->body;
          std::unique_ptr<runtime_value> result = interpret(&body_variant, loopEnv, false);
          std::cout << "[interpret_for_loop_statement] Loop body result: " << result->to_string() << "\n";
          if (result->returned_value)
          {
            std::cout << "[interpret_for_loop_statement] Return detected inside loop, breaking with returned value\n";
            return result;
          }
        }
        if (for_loop->post)
        {
          std::cout << "[interpret_for_loop_statement] Evaluating post-expression\n";
          ast::ASTVariant post_variant = for_loop->post;
          interpret(&post_variant, loopEnv, false);
        }
        num_iterations++;
        std::cout << "[interpret_for_loop_statement] End of iteration " << num_iterations << "\n";
        if (num_iterations > 10000000)
        {
          std::cout << "[interpret_for_loop_statement] Warning: Possible infinite loop detected after " << num_iterations << " iterations\n";
          error::Error err(
              error::ErrorCode::RUNTIME_ERROR,
              "Infinite loop detected.",
              for_loop->linestart,
              for_loop->lineend,
              for_loop->columnstart,
              for_loop->columnend,
              "interpreter.cpp : interpret_for_loop_statement : while",
              error::ErrorImportance::CRITICAL);
          error::display_all_errors(true);
        }
      }
    }
    else
    {
      // [Modified] Handle both VariableDeclarationExpression and SymbolExpression for the loop variable.
      std::string var_name;
      if (auto var_decl_expr = dynamic_cast<ast::VariableDeclarationExpression *>(for_loop->initializer))
      {
        var_name = var_decl_expr->name;
      }
      else if (auto sym_expr = dynamic_cast<ast::SymbolExpression *>(for_loop->initializer))
      {
        var_name = sym_expr->value;
      }
      else
      {
        error::Error err(error::ErrorCode::RUNTIME_ERROR,
                         "Invalid loop variable in for-of loop.",
                         for_loop->linestart, for_loop->lineend,
                         for_loop->columnstart, for_loop->columnend,
                         "interpreter.cpp : interpret_for_loop_statement : for-of", error::ErrorImportance::CRITICAL);
        return std::make_unique<null_value>(false);
      }

      if (auto array_sym = dynamic_cast<ast::SymbolExpression *>(for_loop->array_of))
      {
        runtime_value *arr_var = env->lookup_variable(array_sym->value, for_loop->array_of);
        if (auto arr = dynamic_cast<array_value *>(arr_var))
        {
          for (size_t i = 0; i < arr->elements.size(); i++)
          {
            env->declare_variable(var_name, arr->elements[i]->clone(), for_loop->initializer, false, false);
            if (for_loop->body)
            {
              ast::ASTVariant body_variant = for_loop->body;
              auto result = interpret(&body_variant, env, false);
              if (result->returned_value)
              {
                return result;
              }
            }
            runtime_value *updated = env->lookup_variable(var_name, for_loop->initializer);
            arr->elements[i] = std::unique_ptr<runtime_value>(updated->clone().release());
            env->delete_variable(var_name, for_loop->initializer);
          }
        }
      }
      else
      {
        ast::ASTVariant arr_variant = for_loop->array_of;
        auto arr_val = interpret(&arr_variant, env, false);
        if (auto arr = dynamic_cast<array_value *>(arr_val.get()))
        {
          for (size_t i = 0; i < arr->elements.size(); i++)
          {
            env->declare_variable(var_name, arr->elements[i]->clone(), for_loop->initializer, false, false);
            if (for_loop->body)
            {
              ast::ASTVariant body_variant = for_loop->body;
              auto result = interpret(&body_variant, env, false);
              if (result->returned_value)
              {
                return result;
              }
            }
            runtime_value *updated = env->lookup_variable(var_name, for_loop->initializer);
            arr->elements[i] = std::unique_ptr<runtime_value>(updated->clone().release());
            env->delete_variable(var_name, for_loop->initializer);
          }
        }
      }
    }
    std::unique_ptr<runtime_value> result = std::make_unique<null_value>(false);
    return result;
  };

  //*-------------------
  //*    EXPRESSIONS
  //*-------------------
  std::unique_ptr<runtime_value> interpret_binary_expression(ast::BinaryExpression *expression, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Interpreting binary expression\n";
    auto bin = dynamic_cast<ast::BinaryExpression *>(expression);
    if (!bin || !bin->left || !bin->right)
    {
      /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Invalid binary expression\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    ast::ASTVariant left_variant = bin->left;
    auto lhs = interpret(&left_variant, env, false);
    ast::ASTVariant right_variant = bin->right;
    auto rhs = interpret(&right_variant, env, false);

    /* [DEBUG**] */ std::cout << "[interpret_binary_expression] " << bin->op.value << " LHS: " << lhs->to_string() << " RHS: " << rhs->to_string() << "\n";

    if (auto lnum = dynamic_cast<number_value *>(lhs.get()))
    {
      if (auto rnum = dynamic_cast<number_value *>(rhs.get()))
      {
        if (bin->op.value == "+")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Adding numbers\n";
          return std::make_unique<number_value>(lnum->value + rnum->value, is_returned);
        }
        else if (bin->op.value == "*")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Multiplying numbers\n";
          return std::make_unique<number_value>(lnum->value * rnum->value, is_returned);
        }
        else if (bin->op.value == "-")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Subtracting numbers\n";
          return std::make_unique<number_value>(lnum->value - rnum->value, is_returned);
        }
        else if (bin->op.value == "/")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Dividing numbers\n";
          if (rnum->value == 0)
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR, "Division by zero.", bin->linestart, bin->lineend, bin->columnstart, bin->columnend, "interpreter.cpp : interpret_binary_expression : if", error::ErrorImportance::MODERATE);
            error::display_all_errors(true);
          }
          return std::make_unique<number_value>(lnum->value / rnum->value, is_returned);
        }
        else if (bin->op.value == "%")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Modulo of numbers\n";
          return std::make_unique<number_value>(std::fmod(lnum->value, rnum->value), is_returned);
        }
        else if (bin->op.value == "==")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Comparing numbers for equality\n";
          return std::make_unique<boolean_value>(lnum->value == rnum->value, is_returned);
        }
        else if (bin->op.value == "!=")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Comparing numbers for inequality\n";
          return std::make_unique<boolean_value>(lnum->value != rnum->value, is_returned);
        }
        else if (bin->op.value == ">")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Comparing numbers for greater than\n";
          return std::make_unique<boolean_value>(lnum->value > rnum->value, is_returned);
        }
        else if (bin->op.value == ">=")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Comparing numbers for greater than or equal\n";
          return std::make_unique<boolean_value>(lnum->value >= rnum->value, is_returned);
        }
        else if (bin->op.value == "<")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Comparing numbers for less than\n";
          return std::make_unique<boolean_value>(lnum->value < rnum->value, is_returned);
        }
        else if (bin->op.value == "<=")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Comparing numbers for less than or equal\n";
          return std::make_unique<boolean_value>(lnum->value <= rnum->value, is_returned);
        }
        else if (bin->op.value == "&&")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Logical AND of numbers\n";
          return std::make_unique<boolean_value>(lnum->value && rnum->value, is_returned);
        }
        else if (bin->op.value == "||")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Logical OR of numbers\n";
          return std::make_unique<boolean_value>(lnum->value || rnum->value, is_returned);
        }
        else if (bin->op.value == "+=")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Adding numbers and assigning\n";
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value + rnum->value, is_returned), bin);
        }
        else if (bin->op.value == "-=")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Subtracting numbers and assigning\n";
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value - rnum->value, is_returned), bin);
        }
        else if (bin->op.value == "*=")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Multiplying numbers and assigning\n";
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value * rnum->value, is_returned), bin);
        }
        else if (bin->op.value == "/=")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Dividing numbers and assigning\n";
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value / rnum->value, is_returned), bin);
        }
        else if (bin->op.value == "%=")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Modulo of numbers and assigning\n";
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(std::fmod(lnum->value, rnum->value), is_returned), bin);
        }
        else if (bin->op.value == "++")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Incrementing number\n";
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value + 1, is_returned), bin);
        }
        else if (bin->op.value == "--")
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Decrementing number\n";
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value - 1, is_returned), bin);
        }
        else
        {
          /* [DEBUG**] */ std::cout << "[interpret_binary_expression] Invalid operator for number types: '" << bin->op.value << "'\n";
          error::Error err(error::ErrorCode::RUNTIME_ERROR, "Invalid operator for number types: '" + bin->op.value + "'", bin->linestart, bin->lineend, bin->columnstart, bin->columnend, "interpreter.cpp : interpret_binary_expression : else", error::ErrorImportance::MODERATE);
        }
      }
    }
    return std::make_unique<null_value>(is_returned);
  }

  std::unique_ptr<runtime_value> interpret_call_expression(ast::CallExpression *expression, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpret_call_expression] Interpreting call expression\n";
    auto call = dynamic_cast<ast::CallExpression *>(expression);
    /* [DEBUG**] */ std::cout << "[interpret_call_expression] Call made\n";
    if (!call || !call->function)
    {
      /* [DEBUG**] */ std::cout << "[interpret_call_expression] Invalid call expression\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    /* [DEBUG**] */ std::cout << "[interpret_call_expression] Getting function\n";
    ast::ASTVariant fn_variant = call->function;
    /* [DEBUG**] */ std::cout << "[interpret_call_expression] Getting function variant\n";
    auto fn = interpret(&fn_variant, env, false);
    /* [DEBUG**] */ std::cout << "[interpret_call_expression] Got function\n";

    if (auto fn_val = dynamic_cast<function_value *>(fn.get()))
    {
      /* [DEBUG**] */ std::cout << "[interpret_call_expression] Calling function: " << fn_val->name << "\n";
      Environment *closure = new Environment();
      closure->parent = fn_val->closure;
      for (size_t i = 0; i < fn_val->parameters.size(); i++)
      {
        std::unique_ptr<runtime_value> arg;
        ast::ASTVariant arg_variant = call->args[i];
        arg = interpret(&arg_variant, env, false);
        closure->declare_variable(fn_val->parameters[i]->name, std::move(arg), call->args[i], false, false);
      }
      ast::ASTVariant variant(fn_val->body);
      return interpret(&variant, closure, is_returned);
    }
    else if (auto native_fn_val = dynamic_cast<native_function_value *>(fn.get()))
    {
      /* [DEBUG**] */ std::cout << "Calling native function: " << native_fn_val->name << "\n";
      /* [DEBUG**] */ std::cout << "[interpret_call_expression] Calling native function: " << native_fn_val->name << "\n";
      std::vector<std::variant<std::unique_ptr<runtime_value>, ast::ReferenceExpression>> args;
      for (const auto &arg : call->args)
      {
        if (native_fn_val->name == "alloc" && args.size() == 0)
        {
          if (auto ref_expr = dynamic_cast<ast::ReferenceExpression *>(arg))
          {
            args.push_back(*ref_expr);
            continue;
          }
        }
        if (native_fn_val->name == "alloc" && args.size() == 1)
        {
          if (auto ptr = std::get_if<std::unique_ptr<runtime_value>>(&args[0]))
          {
            if (auto arr = dynamic_cast<array_value *>((*ptr).get()))
            {
              args.push_back(std::move(*ptr));
              continue;
            }
          }
        }
        if (native_fn_val->name == "free")
        {
          if (auto ref_expr = dynamic_cast<ast::ReferenceExpression *>(arg))
          {
            args.push_back(*ref_expr);
            continue;
          }
        }
        if (auto ref_expr = dynamic_cast<ast::ReferenceExpression *>(arg))
        {
          /* [DEBUG**] */ std::cout << "[interpret_call_expression] Argument Ref: " << ref_expr->name << "\n";
        }
        ast::ASTVariant arg_variant = arg;
        auto res = interpret(&arg_variant, env, false);
        /* [DEBUG**] */ std::cout << "[interpret_call_expression] Argument: " << res->to_string() << "\n";
        args.push_back(std::move(res));
        /* [DEBUG**] */ std::cout << "[interpret_call_expression] Argument pushed\n";
      }
      /* [DEBUG**] */ std::cout << "[interpret_call_expression] Checking argument count\n";
      if (args.size() < native_fn_val->arity && native_fn_val->arity != -1)
      {
        /* [DEBUG**] */ std::cout << "[interpret_call_expression] Too few arguments\n";
        error::Error err(error::ErrorCode::RUNTIME_ERROR, "Native function '" + native_fn_val->name + "' called with too few arguments. Expected " + std::to_string(native_fn_val->arity) + " arguments, got " + std::to_string(args.size()) + " arguments.", call->linestart, call->lineend, call->columnstart, call->columnend, "interpreter.cpp : interpret_call_expression : if", error::ErrorImportance::MODERATE);
        /* [DEBUG**] */ std::cout << "[interpret_call_expression] Error displayed\n";
      }
      else if (args.size() > native_fn_val->arity && native_fn_val->arity != -1)
      {
        /* [DEBUG**] */ std::cout << "[interpret_call_expression] Too many arguments\n";
        error::Error err(error::ErrorCode::RUNTIME_ERROR, "Native function '" + native_fn_val->name + "' called with too many arguments. Expected " + std::to_string(native_fn_val->arity) + " arguments, got " + std::to_string(args.size()) + " arguments.", call->linestart, call->lineend, call->columnstart, call->columnend, "interpreter.cpp : interpret_call_expression : if", error::ErrorImportance::MODERATE);
        /* [DEBUG**] */ std::cout << "[interpret_call_expression] Error displayed\n";
      }
      else if (native_fn_val->arity == -1 || args.size() == native_fn_val->arity)
      {
        /* [DEBUG**] */ std::cout << "[interpret_call_expression] Correct number of arguments\n";
        auto finalval = native_fn_val->body(args, *call, env);
        /* [DEBUG**] */ std::cout << "[interpret_call_expression] Native function called\n";
        return finalval;
      }
      /* [DEBUG**] */ std::cout << "[interpret_call_expression] Returning null\n";
      return std::make_unique<null_value>(is_returned);
    }
    /* [DEBUG**] */ std::cout << "[interpret_call_expression] Returning null\n";
    return std::make_unique<null_value>(is_returned);
  }

  std::unique_ptr<runtime_value> interpret_variable_declaration_expression(ast::VariableDeclarationExpression *expression, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpret_variable_declaration_expression] Interpreting variable declaration expression\n";
    auto var_decl = dynamic_cast<ast::VariableDeclarationExpression *>(expression);
    std::unique_ptr<runtime_value> value = nullptr;

    if (var_decl->value)
    {
      ast::ASTVariant variant = var_decl->value;
      value = interpret(&variant, env, is_returned);
    }
    std::unique_ptr<runtime_value> finalval = env->declare_variable(var_decl->name, std::move(value), var_decl, false, false);
    return finalval;
  }

  std::unique_ptr<runtime_value> interpret_assignment_expression(ast::AssignmentExpression *expresssion, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpet_assignment_expression] Interpreting assignment expression\n";
    auto assign = dynamic_cast<ast::AssignmentExpression *>(expresssion);
    if (assign->increment_decrement)
    {
      /* [DEBUG**] */ std::cout << "[interpet_assignment_expression] Increment/Decrement assignment\n";
      if (assign->op.value == "++")
      {
        /* [DEBUG**] */ std::cout << "[interpet_assignment_expression] Incrementing\n";
        ast::ASTVariant left_variant = assign->left;
        auto lhs = interpret(&left_variant, env, false);
        if (auto lnum = dynamic_cast<number_value *>(lhs.get()))
        {
          if (auto sym = dynamic_cast<ast::SymbolExpression *>(assign->left))
            return env->assign_variable(sym->value, std::make_unique<number_value>(lnum->value + 1, is_returned), assign);
          else if (auto mem = dynamic_cast<ast::MemberExpression *>(assign->left))
          {
            std::string objectName;
            if (auto symObj = dynamic_cast<ast::SymbolExpression *>(mem->object))
            {
              objectName = symObj->value;
            }
            std::string propertyName;
            if (auto symProp = dynamic_cast<ast::SymbolExpression *>(mem->property))
            {
              propertyName = symProp->value;
            }
            else if (auto strProp = dynamic_cast<ast::StringExpression *>(mem->property))
            {
              propertyName = strProp->value;
            }
            return env->assign_variable(objectName + "." + propertyName, std::make_unique<number_value>(lnum->value + 1, is_returned), assign);
          }
        }
      }
      else if (assign->op.value == "--")
      {
        /* [DEBUG**] */ std::cout << "[interpet_assignment_expression] Decrementing\n";
        ast::ASTVariant left_variant = assign->left;
        auto lhs = interpret(&left_variant, env, false);
        if (auto lnum = dynamic_cast<number_value *>(lhs.get()))
        {
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(assign->left)->value, std::make_unique<number_value>(lnum->value - 1, is_returned), assign);
        }
      }
    }
    else if (!assign || !assign->left || !assign->right)
    {
      /* [DEBUG**] */ std::cout << "[interpet_assignment_expression] Invalid assignment expression\n";
      return std::make_unique<dummy_value>();
    }

    ast::ASTVariant left_variant = assign->left;
    auto lhs = interpret(&left_variant, env, false);
    ast::ASTVariant right_variant = assign->right;
    auto rhs = interpret(&right_variant, env, false);

    if (auto symExpr = dynamic_cast<ast::SymbolExpression *>(assign->left))
    {
      auto updated = env->assign_variable(symExpr->value, rhs->clone(), assign);
      return updated->clone();
    }
    else if (auto mem_expr = dynamic_cast<ast::MemberExpression *>(assign->left))
    {
      std::string objectName;
      if (auto symObj = dynamic_cast<ast::SymbolExpression *>(mem_expr->object))
        objectName = symObj->value;
      std::string propertyName;
      if (auto symProp = dynamic_cast<ast::SymbolExpression *>(mem_expr->property))
        propertyName = symProp->value;
      else if (auto strProp = dynamic_cast<ast::StringExpression *>(mem_expr->property))
        propertyName = strProp->value;
      auto updated = env->assign_variable(objectName + "." + propertyName, rhs->clone(), assign);
      return updated->clone();
    }
    else
    {
      return std::make_unique<null_value>(is_returned);
    }
  }

  std::unique_ptr<runtime_value> interpret_member_expression(ast::MemberExpression *expression, interpreter::Environment *env, bool is_returned)
  {
    /* [DEBUG**] */ std::cout << "[interpret_member_expression] Interpreting member expression\n";
    auto mem = dynamic_cast<ast::MemberExpression *>(expression);
    if (!mem || !mem->object || !mem->property)
    {
      /* [DEBUG**] */ std::cout << "[interpret_member_expression] Invalid member expression\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    ast::ASTVariant obj_variant = mem->object;
    auto obj = interpret(&obj_variant, env, false);
    std::string property;
    if (auto prop = dynamic_cast<ast::SymbolExpression *>(mem->property))
    {
      property = prop->value;
    }
    else if (auto prop = dynamic_cast<ast::StringExpression *>(mem->property))
    {
      property = prop->value;
    }
    else
    {
      /* [DEBUG**] */ std::cout << "[interpret_member_expression] Invalid member expression\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    if (property == "string")
    {
      if (auto num = dynamic_cast<number_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "string",
            0,
            [num, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>> &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              return std::make_unique<string_value>(std::to_string(num->value), is_returned);
            });
      }
      if (auto str = dynamic_cast<string_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "string",
            0,
            [str, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>> &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              return std::make_unique<string_value>(str->value, is_returned);
            });
      }
      if (auto bool_val = dynamic_cast<boolean_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "string",
            0,
            [bool_val, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>>
                    &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              return std::make_unique<string_value>(bool_val->value ? "true" : "false", is_returned);
            });
      }
      if (auto obj_val = dynamic_cast<object_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "string",
            -1,
            [obj_val, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>>
                    &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              if (auto value_ptr = std::get_if<std::unique_ptr<runtime_value>>(&args[0]))
              {
                if (auto bool_val = dynamic_cast<boolean_value *>(value_ptr->get()))
                {
                  if (bool_val->value == true)
                  {
                    std::string result = "{";
                    for (const auto &prop : obj_val->properties)
                    {
                      result += prop.first + ": " + prop.second->to_string() + ", ";
                    }
                    result.pop_back();
                    result.pop_back();
                    result += "}";
                    return std::make_unique<string_value>(result, is_returned);
                  }
                }
              }
              else
              {
                std::string result = "{";
                for (const auto &prop : obj_val->properties)
                {
                  result += prop.first + ": " + prop.second->to_string() + ", ";
                }
                if (!obj_val->properties.empty())
                {
                  result.pop_back();
                  result.pop_back();
                }
                result += "}";
                return std::make_unique<string_value>(result, is_returned);
              }
              return std::make_unique<string_value>("<object>", is_returned);
            });
      }
      if (auto arr_val = dynamic_cast<array_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "string",
            0,
            [arr_val, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>>
                    &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              std::string result = "[";
              for (const auto &elem : arr_val->elements)
              {
                result += elem->to_string() + ", ";
              }
              if (!arr_val->elements.empty())
              {
                result.pop_back();
                result.pop_back();
              }
              result += "]";
              return std::make_unique<string_value>(result, is_returned);
            });
      }
      if (auto null_val = dynamic_cast<null_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "string",
            0,
            [null_val, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>>
                    &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              return std::make_unique<string_value>("null", is_returned);
            });
      }
      if (auto dummy_val = dynamic_cast<dummy_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "string",
            0,
            [dummy_val, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>>
                    &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              return std::make_unique<string_value>("<dummy>", is_returned);
            });
      }
      if (auto fn_val = dynamic_cast<function_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "string",
            0,
            [fn_val, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>>
                    &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              std::string str;
              str += "function " + fn_val->name + " (";
              for (size_t i = 0; i < fn_val->parameters.size(); i++)
              {
                str += fn_val->parameters[i]->name;
                if (i + 1 < fn_val->parameters.size())
                {
                  str += ", ";
                }
              }
              str += ") { ... }";
              return std::make_unique<string_value>(str, is_returned);
            });
      }
      if (auto native_fn_val = dynamic_cast<native_function_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "string",
            0,
            [native_fn_val, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>>
                    &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              return std::make_unique<string_value>("<native function>", is_returned);
            });
      }
    }
    else if (property == "num")
    {
      if (auto str = dynamic_cast<string_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "num",
            0,
            [str, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>>
                    &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              try
              {
                return std::make_unique<number_value>(std::stod(str->value), is_returned);
              }
              catch (const std::invalid_argument &e)
              {
                error::Error err(
                    error::ErrorCode::RUNTIME_ERROR,
                    "Invalid number conversion from string '" + str->value + "'.",
                    error_expr.linestart, error_expr.lineend, error_expr.columnstart, error_expr.columnend,
                    "interpreter.cpp : interpret_member_expression : for",
                    error::ErrorImportance::MODERATE);
                return std::make_unique<null_value>(is_returned);
              }
            });
      }
      if (auto num = dynamic_cast<number_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "num",
            0,
            [num, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>>
                    &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              return std::make_unique<number_value>(num->value, is_returned);
            });
      }
      if (auto bool_val = dynamic_cast<boolean_value *>(obj.get()))
      {
        return std::make_unique<native_function_value>(
            "num",
            0,
            [bool_val, is_returned](
                const std::vector<
                    std::variant<
                        std::unique_ptr<runtime_value>,
                        ast::ReferenceExpression>>
                    &args,
                ast::Expression &error_expr,
                interpreter::Environment *env) -> std::unique_ptr<runtime_value>
            {
              return std::make_unique<number_value>(bool_val->value ? 1 : 0, is_returned);
            });
      }
    }
    else if (property == "push")
    {
      if (auto arr = dynamic_cast<array_value *>(obj.get()))
      {
        if (auto ref = dynamic_cast<ast::ReferenceExpression *>(mem->object))
        {
          return std::make_unique<native_function_value>(
              "push",
              1,
              [arr, ref, is_returned](
                  const std::vector<
                      std::variant<
                          std::unique_ptr<runtime_value>,
                          ast::ReferenceExpression>>
                      &args,
                  ast::Expression &error_expr,
                  interpreter::Environment *env) -> std::unique_ptr<runtime_value>
              {
                /* [DEBUG**] */ std::cout << "[interpret_member_expression] Array push\n";
                bool is_constant = env->is_variable_constant(ref->name);
                /* [DEBUG**] */ std::cout << "[interpret_member_expression] Made variable 'is_constant'\n";
                bool is_public = env->is_variable_public(ref->name);
                /* [DEBUG**] */ std::cout << "[interpret_member_expression] Made variable 'is_public'\n";

                if (args.empty())
                {
                  /* [DEBUG**] */ std::cout << "[interpret_member_expression] No arguments provided for 'push'\n";
                  error::Error err(
                      error::ErrorCode::RUNTIME_ERROR,
                      "No arguments provided for 'push' property.",
                      error_expr.linestart, error_expr.lineend, error_expr.columnstart, error_expr.columnend,
                      "interpreter.cpp : interpret_member_expression : for",
                      error::ErrorImportance::MODERATE);
                  return std::make_unique<null_value>(is_returned);
                }

                auto elem = std::get_if<std::unique_ptr<runtime_value>>(&args[0]);
                /* [DEBUG**] */ std::cout << "[interpret_member_expression] Got element\n";
                if (!elem)
                {
                  /* [DEBUG**] */ std::cout << "[interpret_member_expression] Invalid argument for 'push' property\n";
                  error::Error err(
                      error::ErrorCode::RUNTIME_ERROR,
                      "Invalid argument for 'push' property.",
                      error_expr.linestart, error_expr.lineend, error_expr.columnstart, error_expr.columnend,
                      "interpreter.cpp : interpret_member_expression : for",
                      error::ErrorImportance::MODERATE);
                  /* [DEBUG**] */ std::cout << "[interpret_member_expression] Returning null\n";
                  return std::make_unique<null_value>(is_returned);
                }
                /* [DEBUG**] */ std::cout << "[interpret_member_expression] Pushing element\n";
                if (arr && *elem)
                {
                  arr->elements.push_back((*elem)->clone());
                }
                /* [DEBUG**] */ std::cout << "[interpret_member_expression] Assigning variable\n";
                // env->delete_variable(ref->name, &error_expr);
                // env->declare_variable(ref->name, std::make_unique<array_value>(*arr), new ast::Type(variable_type), &error_expr, is_constant, is_public);
                env->assign_variable(ref->name, arr->clone(), &error_expr);
                /* [DEBUG**] */ std::cout << "[interpret_member_expression] Array push: " << print_value(*arr) << "\n";
                return std::make_unique<null_value>(is_returned);
              });
        }
        else
        {
          return std::make_unique<native_function_value>(
              "push",
              1,
              [mem, is_returned](
                  const std::vector<
                      std::variant<
                          std::unique_ptr<runtime_value>,
                          ast::ReferenceExpression>>
                      &args,
                  ast::Expression &error_expr,
                  interpreter::Environment *env) -> std::unique_ptr<runtime_value>
              {
                auto ref = dynamic_cast<ast::ReferenceExpression *>(mem->object);
                if (!ref)
                {
                  error::Error err(
                      error::ErrorCode::RUNTIME_ERROR,
                      "Invalid array reference in 'push' property.",
                      error_expr.linestart, error_expr.lineend, error_expr.columnstart, error_expr.columnend,
                      "interpreter.cpp : interpret_member_expression : push",
                      error::ErrorImportance::MODERATE);
                  return std::make_unique<null_value>(is_returned);
                }
                runtime_value *array_obj = env->lookup_variable(ref->name, &error_expr);
                auto arr = dynamic_cast<array_value *>(array_obj);
                if (!arr)
                {
                  error::Error err(
                      error::ErrorCode::RUNTIME_ERROR,
                      "Variable '" + ref->name + "' is not an array in 'push' property.",
                      error_expr.linestart, error_expr.lineend, error_expr.columnstart, error_expr.columnend,
                      "interpreter.cpp : interpret_member_expression : push",
                      error::ErrorImportance::MODERATE);
                  return std::make_unique<null_value>(is_returned);
                }
                auto elem = std::get_if<std::unique_ptr<runtime_value>>(&args[0]);
                if (!elem)
                {
                  error::Error err(
                      error::ErrorCode::RUNTIME_ERROR,
                      "Invalid argument for 'push' property.",
                      error_expr.linestart, error_expr.lineend, error_expr.columnstart, error_expr.columnend,
                      "interpreter.cpp : interpret_member_expression : push",
                      error::ErrorImportance::MODERATE);
                  return std::make_unique<null_value>(is_returned);
                }
                arr->elements.push_back((*elem)->clone());
                return std::make_unique<null_value>(is_returned);
              });
        }
      }
    }

    if (auto obj_val = dynamic_cast<object_value *>(obj.get()))
    {
      auto it = obj_val->properties.find(property);
      if (it != obj_val->properties.end())
      {
        return it->second->clone();
      }
      else
      {
        obj_val->properties[property] = std::make_unique<null_value>(is_returned);
        return obj_val->properties[property]->clone();
      }
    }
    return std::make_unique<null_value>(is_returned);
  }

  //*---------------
  //*    HELPERS
  //*---------------
  std::string print_value(const interpreter::runtime_value &value)
  {
    if (static_cast<const interpreter::number_value *>(&value) != nullptr)
    {
      const interpreter::number_value &num = static_cast<const interpreter::number_value &>(value);
      return std::to_string(num.value);
    }
    else if (static_cast<const interpreter::string_value *>(&value) != nullptr)
    {
      const interpreter::string_value &str = static_cast<const interpreter::string_value &>(value);
      return str.value;
    }
    else if (static_cast<const interpreter::boolean_value *>(&value) != nullptr)
    {
      const interpreter::boolean_value &bool_val = static_cast<const interpreter::boolean_value &>(value);
      return bool_val.value ? "true" : "false";
    }
    else if (static_cast<const interpreter::function_value *>(&value) != nullptr)
    {
      const interpreter::function_value &fn = static_cast<const interpreter::function_value &>(value);
      std::string str;
      str += "function " + fn.name + " (";
      for (size_t i = 0; i < fn.parameters.size(); i++)
      {
        str += fn.parameters[i]->name;
        if (i + 1 < fn.parameters.size())
        {
          str += ", ";
        }
      }
      str += ") { ... }";
      return str;
    }
    else if (static_cast<const interpreter::object_value *>(&value) != nullptr)
    {
      const interpreter::object_value &obj = static_cast<const interpreter::object_value &>(value);
      std::string s = "{";
      for (auto it = obj.properties.begin(); it != obj.properties.end(); ++it)
      {
        s += it->first + ": " + print_value(*it->second) + ", ";
      }
      s += "}";
      return s;
    }
    else if (static_cast<const interpreter::array_value *>(&value) != nullptr)
    {
      const interpreter::array_value &arr = static_cast<const interpreter::array_value &>(value);
      std::string s = "[";
      for (size_t i = 0; i < arr.elements.size(); i++)
      {
        s += print_value(*arr.elements[i]);
        if (i + 1 < arr.elements.size())
        {
          s += ", ";
        }
      }
      s += "]";
      return s;
    }
    else if (static_cast<const interpreter::null_value *>(&value) != nullptr)
    {
      return "null";
    }
    else if (static_cast<const interpreter::dummy_value *>(&value) != nullptr)
    {
      return "<dummy>";
    }
    else if (static_cast<const interpreter::native_function_value *>(&value) != nullptr)
    {
      return "<native function>";
    }
    else
    {
      return "<unknown>";
    }
  }
}
