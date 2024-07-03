
#ifdef TRACY_ENABLE
#  include "tracy/Tracy.hpp"
#endif

#ifdef TRACY_MEMORY
void* operator new(std::size_t count) {
    auto ptr = malloc(count);
    TracyAlloc(ptr, count);
    return ptr;
}

void operator delete(void* ptr) noexcept {
    TracyFree(ptr);
    free(ptr);
}
#endif

#include <optional>
#include <iostream>

#include "lexer.hpp"
#include "parser.hpp"

int main() {

    // Lexer lexer;
    // lexer.load_input_from_file("../test_input/math_test.hg");

    // std::vector<Token> tkns;
    // while(lexer.next_token()) {
    // 	tkns.push_back(lexer.tkn_at(0));
    // 	lexer.print_token(lexer.tkn_at(0));
    // }
    // std::cout << "\nnumber of tokens: " << tkns.size() << '\n';

    Parser parser;
    // parser.parse_file("../test_input/type_test.hg");
    // parser.ast.print();
}
