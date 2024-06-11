// TODO-LIST:
// TODO: handle use of 'first' in cases .first and \first

// This parser is based on the Pratt/TDOP parser. With the slight modification
// of explicit righ-biding-power alongside the usually sufficient
// left-biding-power. The following sources were helpful guides:
// -  https://tdop.github.io/
// -  https://www.oilshell.org/blog/2016/11/01.html

// on error handling:
// -  Every led and nud is responsible only for the validity of its subs.

#include "parser.hpp"

#include <array>
#include <iostream>

#include "lexer.hpp"
#include "ast.hpp"

#include "semantics_table.hpp"

#include "type_checker.hpp"
#include "log_and_debug.hpp"
#include "utils.hpp"

// call only on a unary op or argument
Ast_node *parse_expression(Lexer &lexer, Parser &parser, Ast_node *super, int rbp)
{
    Semantic_code tkn_sema = op_semantics_table[lexer.tkn_at(0).type];
    Ast_node* left = nullptr;
    left = tkn_sema.nud(lexer.tkn_at(0).type, lexer, parser, nullptr, super);
    if(!lexer.not_eof() || !left)
	return left;

    tkn_sema = op_semantics_table[lexer.tkn_at(0).type];
    while(rbp < tkn_sema.lbp ) {
	Ast_node* new_left = nullptr;
	new_left = tkn_sema.led(lexer.tkn_at(0).type, lexer, parser, left, super);
	// TODO: depricate this!
	if(!new_left) // failsafe, for when the led shouldn't be applied, as decided by the led.
	    return left;
	left = new_left;
	if(!lexer.not_eof())
	    return left;
	tkn_sema = op_semantics_table[lexer.tkn_at(0).type];
    }

    return left;
}

static bool tkn_legal_in_global_space(Token_enum type)
{
    return type == ':' || type == tkn_do;
}

static void build_ast(Lexer &lexer, Parser &parser)
{
    parser.scope_info = Scope_info{};
    lexer.next_token();
    Ast_node* prev_alt;
    
    if(lexer.not_eof()) {
    get_first_node:
	parser.ast.global_scope.sub = parse_expression(lexer, parser, &parser.ast.global_scope, 0);
	if(parser.ast.global_scope.sub != nullptr) {
	    prev_alt = parser.ast.global_scope.sub;
	    if(!tkn_legal_in_global_space(prev_alt->tkn.type))
		lexer.parsing_error(prev_alt->tkn, "Unexpected token, only object declarations are allowed in global space.");
	}
	else
	    goto get_first_node;
    }
    
    while(lexer.not_eof()) {
	const char* start_ptr = lexer.tkn_at(0).ptr;
	prev_alt->alt_sub = parse_expression(lexer, parser, &parser.ast.global_scope, 0);
	if(prev_alt->alt_sub != nullptr) {
	    prev_alt->alt_sub->super = &parser.ast.global_scope;
	    prev_alt = prev_alt->alt_sub;
	    if(!tkn_legal_in_global_space(prev_alt->tkn.type))
		lexer.parsing_error(prev_alt->tkn, "Unexpected token, only object declarations are allowed in global space.");
	}
	// failsafe for when we are not making progress.
	if(start_ptr == lexer.tkn_at(0).ptr)
	    lexer.next_token();
    }
}

static void parse_latest(Lexer &lexer, Parser& parser)
{
    build_ast(lexer, parser);
    if(lexer.parsing_error_cnt)
	std::cout << "Compilation interrupted: " HG_ERROR_COLOR << lexer.parsing_error_cnt << " parsing errors" HG_END_COLOR ".\n";
    else
	std::cout << "Parsing finished" HG_SUCCESS_COLOR " successfully" HG_END_COLOR ".\n";
}

void Parser::parse_file(const char *file_path)
{
    lexer.load_input_from_file(file_path);
    parse_latest(lexer, *this);
}

void Parser::parse_string(const char *string)
{
    lexer.load_input_from_string(string);
    parse_latest(lexer, *this);
}
