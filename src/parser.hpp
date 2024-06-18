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

struct Scope_info
{
    Scope_info(Ast_node* first_scope_ident) : scope_ident(first_scope_ident) {}
    
    Scope_info next_scope(Ast_node* scope_ident);
    void restore_scope(Scope_info scope_info) {*this = scope_info;}
    
    int depth = 0;
    Ast_node* scope_ident = nullptr;
};

class Parser
{
public:
    Parser() : ast{}, scope_info{&ast.global_scope} {}
    
    void parse_file(const char* file_path);
    void parse_string(const char* string);
    void exit() { early_exit = true; }
    
    template<typename... Args>
    void type_error(Ast_node* node, const char* msg, Args... args)
    {
	auto left_right_nodes = get_left_and_right_most_nodes(node);
	lexer.print_error_expression(left_right_nodes.first->tkn.ptr, size_t(left_right_nodes.second->tkn.ptr - left_right_nodes.first->tkn.ptr + 1), HG_err::type, msg, args...);
	++type_error_cnt;
    }
    
    int type_error_cnt = 0;
    Ast ast;
    bool early_exit = false;
    Scope_info scope_info;
    
private:
    std::pair<Ast_node*, Ast_node*> get_left_and_right_most_nodes(Ast_node* node) const;
    Lexer lexer{}; // LF_PRINT_TOKENS
};

Ast_node *parse_expression(Lexer &lexer, Parser &parser, Ast_node *super, int rbp = 0);
