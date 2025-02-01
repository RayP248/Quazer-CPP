#include "interpreter.h"
#include "../error/error.h"
#include <variant>
#include <memory>
#include <cmath>
#include <fstream>
#include <functional>

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
    // [DEBUG**] std::cout << "[interpret] Starting interpretation\n";

    if (!statement)
    {
      // [DEBUG**] std::cout << "[interpret] Null statement received\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    // Convert all AST nodes to Statement*:
    ast::Statement *node = std::visit([](auto &&arg) -> ast::Statement *
                                      { return static_cast<ast::Statement *>(arg); }, *statement);

    if (!node)
    {
      // [DEBUG**] std::cout << "[interpret] Null statement received\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    // [DEBUG**] std::cout << "[interpret] Statement kind: " << ast::statement_kind_to_string(node->kind) << "\n";

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
    case ast::StatementKind::NUMBER_EXPRESSION:
    {
      auto num_node = dynamic_cast<ast::NumberExpression *>(node);
      // [DEBUG**] std::cout << "[interpret] Interpreting number: " << num_node->value << "\n";
      return std::make_unique<number_value>(num_node->value, is_returned);
    }
    case ast::StatementKind::SYMBOL_EXPRESSION:
    {
      auto sym_node = dynamic_cast<ast::SymbolExpression *>(node);
      // [DEBUG**] std::cout << "[interpret] Interpreting symbol: " << sym_node->value << "\n";
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
      // [DEBUG**] std::cout << "[interpret] Interpreting string: " << str_node->value << "\n";
      return std::make_unique<string_value>(str_node->value, is_returned);
    }
    case ast::StatementKind::BINARY_EXPRESSION:
    {
      auto bin_node = dynamic_cast<ast::BinaryExpression *>(node);
      // [DEBUG**] std::cout << "[interpret] Interpreting binary expression with operator: " << bin_node->op.value << "\n";
      auto result = interpret_binary_expression(bin_node, env, is_returned);
      // [DEBUG**] std::cout << "[interpret] Result of bin expr: " << result->to_string() << "\n";
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
      // [DEBUG**] std::cout << "[interpret] Interpreting return statement\n";
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
      // [DEBUG**] std::cout << "[interpret] Interpreting call expression\n";
      return interpret_call_expression(call_node, env, is_returned);
    }
    case ast::StatementKind::IF_STATEMENT:
    {
      auto if_node = dynamic_cast<ast::IfStatement *>(node);
      // [DEBUG**] std::cout << "[interpret] Interpreting if statement\n";
      return interpret_if_statement(if_node, env, is_returned);
    }
    case ast::StatementKind::FOR_LOOP_STATEMENT:
    {
      auto for_node = dynamic_cast<ast::ForLoopStatement *>(node);
      // [DEBUG**] std::cout << "[interpret] Interpreting for loop statement\n";
      return interpret_for_loop_statement(for_node, env, is_returned);
    }
    case ast::StatementKind::VARIABLE_DECLARATION_EXPRESSION:
    {
      auto var_decl_node = dynamic_cast<ast::VariableDeclarationExpression *>(node);
      // [DEBUG**] std::cout << "[interpret] Interpreting variable declaration expression\n";
      return interpret_variable_declaration_expression(var_decl_node, env, is_returned);
    }
    case ast::StatementKind::ASSIGNMENT_EXPRESSION:
    {
      auto assign_node = dynamic_cast<ast::AssignmentExpression *>(node);
      // [DEBUG**] std::cout << "[interpret] Interpreting assignment expression\n";
      return interpret_assignment_expression(assign_node, env, is_returned);
    }
    default:
      // [DEBUG**] std::cout << "[interpret] Unhandled statement kind: " << ast::statement_kind_to_string(node->kind) << "\n";
      return std::make_unique<null_value>(is_returned);
    }
  }

  //*-------------------
  //*    ENVIRONMENT
  //*-------------------
  Environment::Environment(std::string package_name, Environment *parent)
  {
    this->package_name = std::move(package_name);
    this->parent = parent;
    // [DEBUG**] std::cout << "[Environment] Created environment with package name: " << this->package_name << "\n";
  }

  std::unique_ptr<runtime_value> Environment::declare_variable(std::string name, std::unique_ptr<runtime_value> value, ast::Type *explicit_type, ast::Statement *error_expression, bool is_constant, bool is_public)
  {
    // [DEBUG**] std::cout << "[declare_variable] Declaring variable: " << name << "\n";
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
    // [DEBUG**] std::cout << "[assign_variable] Assigning variable: " << name << "\n";
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
    // [DEBUG**] std::cout << "[lookup_variable] Looking up variable: " << name << "\n";
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
    // [DEBUG**] std::cout << "[resolve] Resolving variable: " << name << "\n";
    if (variables.find(name) != variables.end())
    {
      // [DEBUG**] std::cout << "[resolve] Found variable in current environment\n";
      return this;
    }
    if (parent != nullptr)
    {
      // [DEBUG**] std::cout << "[resolve] Variable not found in current environment, checking parent\n";
      return parent->resolve(name, error_expression);
    }
    else
    {
      // [DEBUG**] std::cout << "[resolve] Variable not found in any environment\n";
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
    }
  }

  Environment *create_global_environment()
  {
    // [DEBUG**] std::cout << "[create_global_environment] Creating global environment\n";
    Environment *global_env = new Environment();
    global_env->declare_variable("null", std::make_unique<null_value>(false), new ast::Type("nulltype"), nullptr, true, false);
    global_env->declare_variable("true", std::make_unique<boolean_value>(true, false), new ast::Type("bool"), nullptr, true, false);
    global_env->declare_variable("false", std::make_unique<boolean_value>(false, false), new ast::Type("bool"), nullptr, true, false);

    auto out_fn = std::make_unique<native_function_value>(
        "out",
        [](const std::vector<std::unique_ptr<runtime_value>> &args) -> std::unique_ptr<runtime_value>
        {
          for (const auto &arg : args)
          {
            std::string s = arg->to_string();
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

    global_env->declare_variable("out", std::move(out_fn), new ast::Type("native_function"), nullptr, true, false);
    return global_env;
  }

  //*------------------
  //*    STATEMENTS
  //*------------------
  std::unique_ptr<runtime_value> interpret_program(ast::Program *program, Environment *env)
  {
    // [DEBUG**] std::cout << "[interpret_program] Starting with " << program->body.size() << " statements\n";
    std::unique_ptr<runtime_value> result = std::make_unique<null_value>(false);

    for (const auto &stmt : program->body)
    {
      // [DEBUG**] std::cout << "[interpret_program] Interpreting statement of kind: " << ast::statement_kind_to_string(stmt->kind) << "\n";
      ast::ASTVariant variant = stmt;
      result = interpret(&variant, env, false);
    }

    // [DEBUG**] std::cout << "[interpret_program] Finished interpreting program with result of " << result->to_string() << "\n";
    return result;
  }

  std::unique_ptr<runtime_value> interpret_block_statement(ast::BlockStatement *program, Environment *env)
  {
    // [DEBUG**] std::cout << "[interpret_block_statement] Starting with " << program->body.size() << " statements\n";
    std::unique_ptr<runtime_value> result = std::make_unique<null_value>(false);

    for (const auto &stmt : program->body)
    {
      // [DEBUG**] std::cout << "[interpret_block_statement] Interpreting statement of kind: " << ast::statement_kind_to_string(stmt->kind) << "\n";
      ast::ASTVariant variant = stmt;
      std::unique_ptr<runtime_value> res = interpret(&variant, env, false);
      if (res->returned_value || stmt->kind == ast::StatementKind::RETURN_STATEMENT)
      {
        // [DEBUG**] std::cout << "[interpret_block_statement] Found return statement\n";
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
    // [DEBUG**] std::cout << "[interpret_expression_statement] Interpreting expression statement\n";
    auto exprStmt = dynamic_cast<ast::ExpressionStatement *>(statement);
    ast::ASTVariant variant = exprStmt->expression;
    // [DEBUG**] std::cout << "[interpret_expression_statement] Result of expression: " << interpret(&variant, env, is_returned)->to_string() << "\n";
    return interpret(&variant, env, is_returned);
  }

  std::unique_ptr<runtime_value> interpret_variable_declaration_statement(ast::VariableDeclarationStatement *statement, interpreter::Environment *env, bool is_returned)
  {
    // [DEBUG**] std::cout << "[interpret_variable_declaration_statement] Interpreting variable declaration statement\n";
    auto var_decl = dynamic_cast<ast::VariableDeclarationStatement *>(statement);
    std::unique_ptr<runtime_value> value = nullptr;

    if (var_decl->value)
    {
      ast::ASTVariant variant = var_decl->value;
      value = interpret(&variant, env, is_returned);
    }
    std::unique_ptr<runtime_value> finalval = env->declare_variable(var_decl->name, std::move(value), &var_decl->type, var_decl, var_decl->is_const, var_decl->is_public);
    return finalval;
  }

  std::unique_ptr<runtime_value> interpret_return_statement(ast::ReturnStatement *statement, interpreter::Environment *env)
  {
    // [DEBUG**] std::cout << "[interpret_return_statement] Interpreting return statement\n";
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
    // [DEBUG**] std::cout << "[interpret_function_declaration_statement] Interpreting function declaration statement\n";
    auto fn_decl = dynamic_cast<ast::FunctionDeclarationStatement *>(statement);
    std::unique_ptr<runtime_value> fn = env->declare_variable(fn_decl->name, std::make_unique<function_value>(fn_decl->name, fn_decl->parameters, fn_decl->body, fn_decl->return_type, fn_decl->return_statement, env), &fn_decl->return_type, fn_decl, false, false);
    return fn;
  }

  std::unique_ptr<runtime_value> interpret_if_statement(ast::IfStatement *statement, interpreter::Environment *env, bool is_returned)
  {
    // [DEBUG**] std::cout << "[interpret_if_statement] Interpreting if statement\n";
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
    // [DEBUG**] std::cout << "[interpret_for_loop_statement] Interpreting for loop statement\n";
    auto for_loop = dynamic_cast<ast::ForLoopStatement *>(statement);
    if (for_loop->initializer)
    {
      // [DEBUG**] std::cout << "[interpret_for_loop_statement] Found initializer\n";
      ast::ASTVariant init_variant = for_loop->initializer;
      interpret(&init_variant, env, false);
    }
    else
    {
      // [DEBUG**] std::cout << "[interpret_for_loop_statement] No initializer found\n";
    }
    int num_iterations = 0;
    while (true)
    {
      if (for_loop->condition)
      {
        ast::ASTVariant cond_variant = for_loop->condition;
        auto cond_val = interpret(&cond_variant, env, false);
        if (auto cond_bool = dynamic_cast<boolean_value *>(cond_val.get()))
        {
          if (!cond_bool->value)
          {
            break;
          }
        }
      }
      else
      {
        break;
      }
      if (for_loop->body)
      {
        ast::ASTVariant body_variant = for_loop->body;
        std::unique_ptr<runtime_value> result = interpret(&body_variant, env, false);
        if (result->returned_value)
        {
          return result;
        }
      }
      if (for_loop->post)
      {
        ast::ASTVariant update_variant = for_loop->post;
        interpret(&update_variant, env, false);
      }
      num_iterations++;
      if (num_iterations > 10000)
      {
        std::cerr << "Error: Infinite loop detected in for loop.\n";
        exit(1);
      }
    }
    return std::make_unique<null_value>(false);
  };

  //*-------------------
  //*    EXPRESSIONS
  //*-------------------
  std::unique_ptr<runtime_value> interpret_binary_expression(ast::BinaryExpression *expression, interpreter::Environment *env, bool is_returned)
  {
    // [DEBUG**] std::cout << "[interpret_binary_expression] Interpreting binary expression\n";
    auto bin = dynamic_cast<ast::BinaryExpression *>(expression);
    if (!bin || !bin->left || !bin->right)
    {
      // [DEBUG**] std::cout << "[interpret_binary_expression] Invalid binary expression\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    ast::ASTVariant left_variant = bin->left;
    auto lhs = interpret(&left_variant, env, false);
    ast::ASTVariant right_variant = bin->right;
    auto rhs = interpret(&right_variant, env, false);

    // [DEBUG**] std::cout << "[interpret_binary_expression] " << bin->op.value << " LHS: " << lhs->to_string() << " RHS: " << rhs->to_string() << "\n";

    if (auto lnum = dynamic_cast<number_value *>(lhs.get()))
    {
      if (auto rnum = dynamic_cast<number_value *>(rhs.get()))
      {
        // [DEBUG**] std::cout << "[interpret_binary_expression] Adding numbers: " << lnum->value << " + " << rnum->value << "\n";
        if (bin->op.value == "+")
        {
          return std::make_unique<number_value>(lnum->value + rnum->value, is_returned);
        }
        else if (bin->op.value == "*")
        {
          return std::make_unique<number_value>(lnum->value * rnum->value, is_returned);
        }
        else if (bin->op.value == "-")
        {
          return std::make_unique<number_value>(lnum->value - rnum->value, is_returned);
        }
        else if (bin->op.value == "/")
        {
          if (rnum->value == 0)
          {
            error::Error err(error::ErrorCode::RUNTIME_ERROR, "Division by zero.", bin->linestart, bin->lineend, bin->columnstart, bin->columnend, "interpreter.cpp : interpret_binary_expression : if", error::ErrorImportance::MODERATE);
          }
          return std::make_unique<number_value>(lnum->value / rnum->value, is_returned);
        }
        else if (bin->op.value == "%")
        {
          return std::make_unique<number_value>(std::fmod(lnum->value, rnum->value), is_returned);
        }
        else if (bin->op.value == "==")
        {
          return std::make_unique<boolean_value>(lnum->value == rnum->value, is_returned);
        }
        else if (bin->op.value == "!=")
        {
          return std::make_unique<boolean_value>(lnum->value != rnum->value, is_returned);
        }
        else if (bin->op.value == ">")
        {
          return std::make_unique<boolean_value>(lnum->value > rnum->value, is_returned);
        }
        else if (bin->op.value == ">=")
        {
          return std::make_unique<boolean_value>(lnum->value >= rnum->value, is_returned);
        }
        else if (bin->op.value == "<")
        {
          return std::make_unique<boolean_value>(lnum->value < rnum->value, is_returned);
        }
        else if (bin->op.value == "<=")
        {
          return std::make_unique<boolean_value>(lnum->value <= rnum->value, is_returned);
        }
        else if (bin->op.value == "&&")
        {
          return std::make_unique<boolean_value>(lnum->value && rnum->value, is_returned);
        }
        else if (bin->op.value == "||")
        {
          return std::make_unique<boolean_value>(lnum->value || rnum->value, is_returned);
        }
        else if (bin->op.value == "+=")
        {
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value + rnum->value, is_returned), bin);
        }
        else if (bin->op.value == "-=")
        {
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value - rnum->value, is_returned), bin);
        }
        else if (bin->op.value == "*=")
        {
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value * rnum->value, is_returned), bin);
        }
        else if (bin->op.value == "/=")
        {
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value / rnum->value, is_returned), bin);
        }
        else if (bin->op.value == "%=")
        {
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(std::fmod(lnum->value, rnum->value), is_returned), bin);
        }
        else if (bin->op.value == "++")
        {
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value + 1, is_returned), bin);
        }
        else if (bin->op.value == "--")
        {
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(bin->left)->value, std::make_unique<number_value>(lnum->value - 1, is_returned), bin);
        }
        else
        {
          error::Error err(error::ErrorCode::RUNTIME_ERROR, "Invalid operator for number types: '" + bin->op.value + "'", bin->linestart, bin->lineend, bin->columnstart, bin->columnend, "interpreter.cpp : interpret_binary_expression : else", error::ErrorImportance::MODERATE);
        }
      }
    }
    return std::make_unique<null_value>(is_returned);
  }

  std::unique_ptr<runtime_value> interpret_call_expression(ast::CallExpression *expression, interpreter::Environment *env, bool is_returned)
  {
    auto call = dynamic_cast<ast::CallExpression *>(expression);
    if (!call || !call->function)
    {
      // [DEBUG**] std::cout << "[interpret_call_expression] Invalid call expression\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    ast::ASTVariant fn_variant = call->function;
    auto fn = interpret(&fn_variant, env, false);

    if (auto fn_val = dynamic_cast<function_value *>(fn.get()))
    {
      // [DEBUG**] std::cout << "[interpret_call_expression] Calling function: " << fn_val->name << "\n";
      Environment *closure = new Environment();
      closure->parent = fn_val->closure;
      for (size_t i = 0; i < fn_val->parameters.size(); i++)
      {
        std::unique_ptr<runtime_value> arg;
        ast::ASTVariant arg_variant = call->args[i];
        arg = interpret(&arg_variant, env, false);

        if (arg->type != fn_val->parameters[i]->type.name)
        {
          error::Error err(error::ErrorCode::RUNTIME_ERROR, "Argument type mismatch for parameter '" + fn_val->parameters[i]->name + "'. Expected '" + fn_val->parameters[i]->type.name + "' type, got '" + arg->type + "' type.", call->linestart, call->lineend, call->columnstart, call->columnend, "interpreter.cpp : interpret_call_expression : for", error::ErrorImportance::MODERATE);
        }
        closure->declare_variable(fn_val->parameters[i]->name, std::move(arg), &fn_val->parameters[i]->type, call->args[i], false, false);
      }
      ast::ASTVariant variant(fn_val->body);
      return interpret(&variant, closure, is_returned);
    }
    else if (auto native_fn_val = dynamic_cast<native_function_value *>(fn.get()))
    {
      // [DEBUG**] std::cout << "[interpret_call_expression] Calling native function: " << native_fn_val->name << "\n";
      std::vector<std::unique_ptr<runtime_value>> args;
      for (const auto &arg : call->args)
      {
        ast::ASTVariant arg_variant = arg;
        args.push_back(interpret(&arg_variant, env, false));
      }
      auto finalval = native_fn_val->body(args);
    }

    return std::make_unique<null_value>(is_returned);
  }

  std::unique_ptr<runtime_value> interpret_variable_declaration_expression(ast::VariableDeclarationExpression *expression, interpreter::Environment *env, bool is_returned)
  {
    // [DEBUG**] std::cout << "[interpret_variable_declaration_expression] Interpreting variable declaration expression\n";
    auto var_decl = dynamic_cast<ast::VariableDeclarationExpression *>(expression);
    std::unique_ptr<runtime_value> value = nullptr;

    if (var_decl->value)
    {
      ast::ASTVariant variant = var_decl->value;
      value = interpret(&variant, env, is_returned);
    }
    std::unique_ptr<runtime_value> finalval = env->declare_variable(var_decl->name, std::move(value), &var_decl->type, var_decl, false, false);
    return finalval;
  }

  std::unique_ptr<runtime_value> interpret_assignment_expression(ast::AssignmentExpression *expresssion, interpreter::Environment *env, bool is_returned)
  {
    // [DEBUG**] std::cout << "[interpet_assignment_expression] Interpreting assignment expression\n";
    auto assign = dynamic_cast<ast::AssignmentExpression *>(expresssion);
    if (assign->increment_decrement)
    {
      // [DEBUG**] std::cout << "[interpet_assignment_expression] Increment/Decrement assignment\n";
      if (assign->op.value == "++")
      {
        // [DEBUG**] std::cout << "[interpet_assignment_expression] Incrementing\n";
        ast::ASTVariant left_variant = assign->left;
        auto lhs = interpret(&left_variant, env, false);
        if (auto lnum = dynamic_cast<number_value *>(lhs.get()))
        {
          return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(assign->left)->value, std::make_unique<number_value>(lnum->value + 1, is_returned), assign);
        }
      }
      else if (assign->op.value == "--")
      {
        // [DEBUG**] std::cout << "[interpet_assignment_expression] Decrementing\n";
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
      // [DEBUG**] std::cout << "[interpet_assignment_expression] Invalid assignment expression\n";
      return std::make_unique<dummy_value>(); // replaced null_value
    }

    ast::ASTVariant left_variant = assign->left;
    auto lhs = interpret(&left_variant, env, false);
    ast::ASTVariant right_variant = assign->right;
    auto rhs = interpret(&right_variant, env, false);

    if (auto lnum = dynamic_cast<number_value *>(lhs.get()))
    {
      if (auto rnum = dynamic_cast<number_value *>(rhs.get()))
      {
        // [DEBUG**] std::cout << "[interpet_assignment_expression] Assigning number: " << lnum->value << " to " << rnum->value << "\n";
        return env->assign_variable(dynamic_cast<ast::SymbolExpression *>(assign->left)->value, std::make_unique<number_value>(rnum->value, is_returned), assign);
      }
    }
    return std::make_unique<null_value>(is_returned);
  }

  //*---------------
  //*    HELPERS
  //*---------------
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
    else if (value.type == "function")
    {
      const interpreter::function_value fn = static_cast<const interpreter::function_value &>(value);
      std::string str;
      str += "function " + fn.name + " (";
      for (size_t i = 0; i < fn.parameters.size(); i++)
      {
        str += fn.parameters[i]->name;
        if (&fn.parameters[i]->type != nullptr)
        {
          str += ": ";
          str += fn.parameters[i]->type.name;
        }
        if (i + 1 < fn.parameters.size())
        {
          str += ", ";
        }
      }
      str += ") -> ";
      str += fn.return_type.name;
      str += " { ... }";
      return str;
    }
    else if (value.type == "null")
    {
      return "null";
    }
    // Add more types as needed
    return "Unknown";
  }
}
