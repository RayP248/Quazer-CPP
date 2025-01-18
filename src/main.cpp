#include "./lexer/lexer.h"
#include "./global.h"
#include <string>
#include <iostream>
#include <fstream>

int main() {
  std::ifstream file(global::currfile);
  if (!file) {
    std::cerr << "Unable to open file";
    return 1;
  }

  std::string input((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
  auto tokens = lexer::tokenize(input);
  for (auto& token : tokens) {
    token.debug();
  }
  return 0;
}
