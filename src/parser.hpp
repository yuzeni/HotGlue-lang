#include "lexer.hpp"
#include "ast.hpp"


class Parser {
public:
    void parse_file(const char* file_path);
    void parse_string(const char* string);

    Ast ast{};

private:
    Lexer lexer{}; // LF_PRINT_TOKENS
};
