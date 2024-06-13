// TODO-LIST:
// TODO: handle use of 'first' in cases .first and \first

// This parser is based on the Pratt/TDOP parser. With the slight modification
// of explicit righ-biding-power alongside the usually sufficient
// left-biding-power. The following sources were helpful guides:
// -  https://tdop.github.io/
// -  https://www.oilshell.org/blog/2016/11/01.html

// on error handling:
// -  Every led and nud is responsible only for the validity of its subs.

#pragma once

#include "lexer.hpp"
#include "ast.hpp"
#include "log_and_debug.hpp"

struct Scope_info {
    
    Scope_info next_scope(Ast_node* scope_ident, uint64_t scope_hash);
    void restore_scope(Scope_info scope_info) {*this = scope_info;}
    
    int depth = 0;
    Ast_node* scope_ident = nullptr;
    uint64_t scope_hash = utils::default_str_hash_value;
};

class Parser {
public:
    void parse_file(const char* file_path);
    void parse_string(const char* string);

    void type_error(Ast_node* node, const char* msg);

    template<typename... Args>
    void type_error(Ast_node* node, const char* msg, Args... args)
    {
	lexer.print_error(node->tkn.ptr, HG_err::type, msg, args...);
	++type_error_cnt;
    }
    int type_error_cnt = 0;
    
    Ast ast{};
    Scope_info scope_info;

private:
    Lexer lexer{}; // LF_PRINT_TOKENS
};

Ast_node *parse_expression(Lexer &lexer, Parser &parser, Ast_node *super, int rbp = 0);
