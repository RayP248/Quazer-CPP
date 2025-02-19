#include "./types.h"

namespace type_checker
{
  template <typename... Values>
  bool is_matching_type(ast::Type type, Values... args)
  {
    std::vector<std::string> types = {};
    (void)std::initializer_list<int>{(types.push_back(args.type), 0)...};
    bool matches = true;
    for (const auto &t : types)
    {
      if (t != type.raw_name && type.raw_name != "" && type.raw_name != "any" && type.raw_name != "[]any")
      {
        matches = false;
        break;
      }
    }
    return matches;
  }
  bool is_matching_type(const ast::Type &r1, const interpreter::runtime_value &r2)
  {
    if (r1.raw_name == "" || r1.raw_name == "any" || r1.raw_name == "[]any")
      return true;
    return r1.raw_name == r2.type;
  }
  bool is_matching_type(const interpreter::runtime_value &r1, const interpreter::runtime_value &r2)
  {
    if (r1.type == "" || r1.type == "any" || r1.type == "[]any")
      return true;
    return r1.type == r2.type;
  }
} // namespace type_checker
