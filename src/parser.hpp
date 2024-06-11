#pragma once
#include "lexer.hpp"
#include "ast.hpp"

struct Scope_info {
    Scope_info next_scope(Ast_node* scope_ident, uint64_t scope_hash)
    {
	Scope_info old = *this;
	this->depth++;
	this->scope_ident = scope_ident;
	this->scope_hash = scope_hash;
	return old;
    }

    void restore_scope(Scope_info scope_info)
    {
	*this = scope_info;
    }
    
    int depth = 0;
    Ast_node* scope_ident = nullptr;
    uint64_t scope_hash = utils::default_str_hash_value;
};

class Parser {
public:
    void parse_file(const char* file_path);
    void parse_string(const char* string);

    Ast ast{};
    Scope_info scope_info;

private:
    Lexer lexer{}; // LF_PRINT_TOKENS
};

Ast_node *parse_expression(Lexer &lexer, Parser &parser, Ast_node *super, int rbp = 0);
