#ifndef TYPE_CHECKER_H
#define TYPE_CHECKER_H

#include <string>
#include <vector>
#include <variant>
#include "../ast/ast.h"
#include "../interpreter/interpreter.h"

namespace type_checker {
  template <typename... Values>
  bool is_matching_type(ast::Type type, Values... args);
  bool is_matching_type(const ast::Type &r1, const interpreter::runtime_value &r2);
  bool is_matching_type(const interpreter::runtime_value &r1, const interpreter::runtime_value &r2);

  std::vector<std::string> split_type(const std::string &type);
}

#endif
