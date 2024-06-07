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
#include "log_and_debug.hpp"
#include "utils.hpp"

#define NUD_ARGS                                                               \
  [[maybe_unused]] Token_enum tkn_type, [[maybe_unused]] Lexer &lexer,         \
      [[maybe_unused]] Ast &ast, [[maybe_unused]] Ast_node *left,              \
      [[maybe_unused]] Ast_node *super

#define LED_ARGS                                                               \
  [[maybe_unused]] Token_enum tkn_type, [[maybe_unused]] Lexer &lexer,         \
      [[maybe_unused]] Ast &ast, [[maybe_unused]] Ast_node *left,              \
      [[maybe_unused]] Ast_node *super

Ast_node *nud_left(NUD_ARGS);
Ast_node *nud_right(NUD_ARGS);
Ast_node *nud_arg(NUD_ARGS);
Ast_node *nud_this(NUD_ARGS);
// Ast_node *nud_ident(NUD_ARGS);
Ast_node *nud_bracket(NUD_ARGS);
Ast_node *nud_set_op(NUD_ARGS); // used by: all, first
Ast_node *nud_error(NUD_ARGS);
Ast_node *nud_delimiter(NUD_ARGS);

Ast_node *nud_not(NUD_ARGS);
Ast_node *nud_increment(NUD_ARGS);
Ast_node *nud_decrement(NUD_ARGS);
Ast_node *nud_ident_flag(NUD_ARGS);

Ast_node *led_normal(LED_ARGS);
Ast_node *led_parenthesis(LED_ARGS);
Ast_node *led_bracket(LED_ARGS);
Ast_node *led_declare(LED_ARGS);
Ast_node *led_dot(LED_ARGS);
Ast_node *led_error(LED_ARGS);

Ast_node *led_to_imp(LED_ARGS);
Ast_node *led_not_to (LED_ARGS);
Ast_node *led_func_body(LED_ARGS);
Ast_node *led_set_eq(LED_ARGS);
Ast_node *led_update_add(LED_ARGS);
Ast_node *led_update_sub(LED_ARGS);
Ast_node *led_update_mul(LED_ARGS);
Ast_node *led_update_div(LED_ARGS);

Ast_node *led_or(LED_ARGS);
Ast_node *led_and(LED_ARGS);
Ast_node *led_eq(LED_ARGS);
Ast_node *led_neq(LED_ARGS);
Ast_node *led_less(LED_ARGS);
Ast_node *led_greater(LED_ARGS);
Ast_node *led_less_eq(LED_ARGS);
Ast_node *led_greater_eq(LED_ARGS);
Ast_node *led_add(LED_ARGS);
Ast_node *led_sub(LED_ARGS);
Ast_node *led_mul(LED_ARGS);
Ast_node *led_div(LED_ARGS);
Ast_node *led_modulo(LED_ARGS);
Ast_node *led_pow(LED_ARGS);

struct Semantic_code {
    int lbp = 0; // left-binding-power
    int rbp = 0; // right-binding-power
                 // zero means none are taken
                 // specifying lbp as well as rbp is not necessary, but solves left and right associativity problems,
                 // where lbp <= rbp guarantees left- and lbp > rbp right associativity.
    Ast_node* (*nud)(NUD_ARGS) = nud_error;
    Ast_node* (*led)(LED_ARGS) = led_error;
};

consteval std::array<Semantic_code, tkn_SIZE> get_op_semantics_table()
{
    std::array<Semantic_code, tkn_SIZE> table;
    
    table.fill({});
    
    table[tkn_ident]       = {0, 0, nud_arg};
    table[tkn_int]         = {0, 0, nud_arg};
    table[tkn_real]        = {0, 0, nud_arg};
    table[tkn_string]      = {0, 0, nud_arg};
    table[tkn_str]         = {0, 0, nud_arg};
    table[tkn_bool]        = {0, 0, nud_arg};
    table[tkn_true]        = {0, 0, nud_arg};
    table[tkn_false]       = {0, 0, nud_arg};
    table[tkn_s8]          = {0, 0, nud_arg};
    table[tkn_s16]         = {0, 0, nud_arg};
    table[tkn_s32]         = {0, 0, nud_arg};
    table[tkn_s64]         = {0, 0, nud_arg};
    table[tkn_u8]          = {0, 0, nud_arg};
    table[tkn_u16]         = {0, 0, nud_arg};
    table[tkn_u32]         = {0, 0, nud_arg};
    table[tkn_u64]         = {0, 0, nud_arg};
    table[tkn_f8]          = {0, 0, nud_arg};
    table[tkn_f16]         = {0, 0, nud_arg};
    table[tkn_f32]         = {0, 0, nud_arg};
    table[tkn_f64]         = {0, 0, nud_arg};
    table[tkn_placeholder] = {0, 0, nud_arg};
    table[tkn_this]        = {0, 0, nud_this};

    table[tkn_do]          = {1, 1, nud_error, led_normal};
    table[tkn_expa]        = {2, 2, nud_error, led_normal};
    table[tkn_using]       = {2, 2, nud_right};
    table[':']             = {3, 3, nud_error, led_declare};
    table[tkn_func]        = {3, 3, nud_error, led_normal};
    table[tkn_req]         = {4, 4, nud_error, led_normal};
    table[tkn_else]        = {5, 5, nud_error, led_normal};
    table[tkn_trigger]     = {6, 6, nud_error, led_normal};

    table[tkn_to_imp]      = {6, 6, nud_error, led_to_imp};
    table[tkn_not_to]      = {6, 6, nud_error, led_not_to };
    table[tkn_func_body]   = {6, 6, nud_error, led_func_body};
    table['=']             = {7, 7, nud_error, led_set_eq};
    table[tkn_update_add]  = {7, 7, nud_error, led_update_add};
    table[tkn_update_sub]  = {7, 7, nud_error, led_update_sub};
    table[tkn_update_mul]  = {7, 7, nud_error, led_update_mul};
    table[tkn_update_div]  = {7, 7, nud_error, led_update_div};
    table[tkn_increment]   = {7, 7, nud_increment};
    table[tkn_decrement]   = {7, 7, nud_decrement};

    table[tkn_or]          = {8, 8, nud_error, led_or};
    table[tkn_and]         = {9, 9, nud_error, led_and};
    table[tkn_eq]          = {10, 10, nud_error, led_eq};
    table[tkn_neq]         = {10, 10, nud_error, led_neq};
    table['<']             = {10, 10, nud_error, led_less};
    table['>']             = {10, 10, nud_error, led_greater};
    table[tkn_less_eq]     = {10, 10, nud_error, led_less_eq};
    table[tkn_greater_eq]  = {10, 10, nud_error, led_greater_eq};
    table['+']             = {11, 11, nud_right, led_add};
    table['-']             = {11, 11, nud_right, led_sub};
    table['*']             = {12, 12, nud_error, led_mul};
    table['/']             = {12, 12, nud_error, led_div};
    table['%']             = {12, 12, nud_error, led_modulo};
    table[tkn_pow]         = {14, 13, nud_error, led_pow};
    table['!']             = {15, 15, nud_not};
    
    table[tkn_all]         = {16, 16, nud_set_op};
    table[tkn_last]        = {17, 17, nud_right};
    table[tkn_first]       = {17, 17, nud_set_op};
    table[tkn_from_decl]   = {18, 17, nud_error, led_normal};
     			      	    
    table['.']             = {19, 19, nud_error, led_dot};
    table['\\']            = {19, 19, nud_error, led_normal};
    table['|']             = {0, 20, nud_ident_flag};
    table[tkn_extern]      = {0, 20, nud_ident_flag};
    table[tkn_exread]      = {0, 20, nud_ident_flag};
    table[tkn_exwrite]     = {0, 20, nud_ident_flag};
    table[tkn_exlayout]    = {0, 20, nud_ident_flag};
    table[tkn_AoS]         = {0, 21, nud_ident_flag}; // require exlaout be the super
    table[tkn_SoA]         = {0, 21, nud_ident_flag}; // require exlaout be the super
 			      	    
    table['(']             = {18, 0, nud_bracket, led_parenthesis};
    table['[']             = {18, 0, nud_bracket, led_bracket};
    table['{']             = {18, 0, nud_bracket};
    table[')']             = {0, 0, nud_delimiter};
    table[']']             = {0, 0, nud_delimiter};
    table['}']             = {0, 0, nud_delimiter};
    table[',']             = {0, 0, nud_delimiter};
    
    return table;
}

static const std::array<Semantic_code, tkn_SIZE> op_semantics_table = get_op_semantics_table();

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

Scope_info g_scope_info;

// call only on a unary op or argument
Ast_node *parse_expression(Lexer &lexer, Ast &ast, Ast_node *super, int rbp = 0)
{
    Semantic_code tkn_sema = op_semantics_table[lexer.tkn_at(0).type];
    Ast_node* left = nullptr;
    left = tkn_sema.nud(lexer.tkn_at(0).type, lexer, ast, nullptr, super);
    if(!lexer.not_eof() || !left)
	return left;

    tkn_sema = op_semantics_table[lexer.tkn_at(0).type];
    while(rbp < tkn_sema.lbp ) {
	Ast_node* new_left = nullptr;
	new_left = tkn_sema.led(lexer.tkn_at(0).type, lexer, ast, left, super);
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

bool is_base_type(Token_enum t)
{
    return t == tkn_s8 || t == tkn_s16 || t == tkn_s32 || t == tkn_s64 ||
	t == tkn_u8 || t == tkn_u16 || t == tkn_u32 || t == tkn_u64 ||
	t == tkn_f8 || t == tkn_f16 || t == tkn_f32 || t == tkn_f64 ||
	t == tkn_str || t == tkn_bool;
}

bool is_numeric_type(Token_enum t)
{
    return t == tkn_s8 || t == tkn_s16 || t == tkn_s32 || t == tkn_s64 ||
	t == tkn_u8 || t == tkn_u16 || t == tkn_u32 || t == tkn_u64 ||
	t == tkn_f8 || t == tkn_f16 || t == tkn_f32 || t == tkn_f64;
}

bool is_sint_type(Token_enum t)
{
    return t == tkn_s8 || t == tkn_s16 || t == tkn_s32 || t == tkn_s64;
}

bool add_single_sub(Ast_node *node, Ast_node *sub)
{
    if(node && sub) {
	node->sub = sub;
	sub->super = node;
    }
    if(sub) return true;
    return false;
}

// NOTE: automatically updates prev_sub
bool add_alternative_sub(Ast_node *node, Ast_node **prev_sub, Ast_node *alt_sub)
{
    if(prev_sub && *prev_sub && alt_sub && node) {
	(*prev_sub)->alt_sub = alt_sub;
	alt_sub->super = node;
	*prev_sub = (*prev_sub)->alt_sub;
    }
    if(alt_sub) return true;
    return false;
}

bool add_alternative_sub(Ast_node *node, Ast_node *prev_sub, Ast_node *alt_sub)
{
    if(prev_sub && alt_sub && node) {
	prev_sub->alt_sub = alt_sub;
	alt_sub->super = node;
    }
    if(alt_sub) return true;
    return false;
}

Ast_node *nud_error(NUD_ARGS)
{
    lexer.parsing_error(lexer.tkn_at(0), "The token '%s' has no unary method.", get_token_name_str(tkn_type).c_str());
    Semantic_code tkn_sema = op_semantics_table[tkn_type];
    // if it has a led try that one instead.
    if(tkn_sema.led != led_error && left)
	return tkn_sema.led(tkn_type, lexer, ast, left, super);
    lexer.next_token();
    return nullptr;
}

Ast_node *nud_delimiter(NUD_ARGS)
{
    lexer.next_token();
    return nullptr;
}

Ast_node *nud_left(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    lexer.next_token();
    return node;
}

Ast_node *nud_right(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    lexer.next_token();
    // get the right node
    if(!(lexer.not_eof() && add_single_sub(node, parse_expression(lexer, ast, node, op_semantics_table[tkn_type].rbp))))
	lexer.parsing_error(node->tkn, "Missing right argument for unary-right operator '%s'.", get_token_name_str(tkn_type).c_str());
    return node;
}

Ast_node *nud_arg(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    lexer.next_token();
    return node;
}

Ast_node *nud_this(NUD_ARGS)
{
    Ast_node* node = nud_arg(tkn_type, lexer, ast, left, super);
    node->tkn.type = tkn_ident;
    node->tkn.sv = g_scope_info.scope_ident->tkn.sv;
    return node;
}

// Ast_node *nud_ident(NUD_ARGS)
// {
//     Ast_node* node = nud_arg(tkn_type, lexer, ast, left, super);
//     node->id = ast.find_or_add_ident(node, g_scope_info.scope_hash);
//     HG_DEB_assert(node->id != 0, "ident id is 0 when it should be defined");
//     return node;
// }

Token_enum bracket_opposite(Token_enum tkn_type)
{
    HG_DEB_assert(is_open_bracket(tkn_type), "token type must be a bracket in %s", __func__);
    switch(tkn_type) {
    case '(': return Token_enum(')');
    case ')': return Token_enum('(');
    case '[': return Token_enum(']');
    case ']': return Token_enum('[');
    case '{': return Token_enum('}');
    case '}': return Token_enum('{');
    }
    return tkn_parse_error;
}

Ast_node *nud_bracket(NUD_ARGS)
{
    HG_DEB_assert(is_open_bracket(tkn_type), "token type must be a bracket in nud_bracket");
    Ast_node* bracket = nullptr;
    if(!is_delim_tkn_right(lexer.peek_next_token().type)) {
	bracket = nud_right(tkn_type, lexer, ast, left, super);
	Ast_node* prev_alt =  bracket->sub;

	// collect all statements within the brackets
	while(lexer.tkn_at(0).type != bracket_opposite(tkn_type)) {
	    if(lexer.tkn_at(0).type == tkn_eof) {
		lexer.parsing_error(lexer.tkn_at(-1), "Missing closing bracket.");
		break;
	    }
	    add_alternative_sub(bracket, &prev_alt, parse_expression(lexer, ast, bracket, 0));
	}
    }
    else
	lexer.next_token();
    lexer.next_token();
    return bracket;
}

Ast_node *nud_set_op(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    lexer.next_token();
    // TODO: make more sophisticated when necessary
    if(!is_binary_set_op(super->tkn.type)) {
	if(!(lexer.not_eof() && add_single_sub(node, parse_expression(lexer, ast, node, op_semantics_table[tkn_type].rbp))))
	    lexer.parsing_error(node->tkn, "Missing right argument for '%s'.", get_token_name_str(tkn_type).c_str());
    }
    return node;
}

Ast_node *led_error(LED_ARGS)
{
    lexer.parsing_error(lexer.tkn_at(0), "The token '%s' has no binary method.", get_token_name_str(tkn_type).c_str());
    Semantic_code tkn_sema = op_semantics_table[tkn_type];
    // We cant try nud, because we don't know super
    if(tkn_sema.nud != nud_error)
	return tkn_sema.nud(tkn_type, lexer, ast, left, super);
    lexer.next_token();
    return nullptr;
}

Ast_node *led_normal(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    
    lexer.next_token();
    // adding right
    if(!(lexer.not_eof() && add_alternative_sub(node, node->sub, parse_expression(lexer, ast, node, op_semantics_table[tkn_type].rbp))))
	lexer.parsing_error(node->tkn, "Missing right argument for binary operator '%s'.", get_token_name_str(tkn_type).c_str());
    return node;
}

Ast_node *led_req(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    
    lexer.next_token();
    
    Ast_node* prev_sub = node->sub;
    if(lexer.not_eof()) {
	add_alternative_sub(node, &prev_sub, parse_expression(lexer, ast, node, op_semantics_table[tkn_type].rbp));
	if(lexer.tkn_at(0).type == tkn_else) // optional 'else' argument
	    add_alternative_sub(node, &prev_sub, parse_expression(lexer, ast, node, op_semantics_table[tkn_type].rbp));
    }
    else {
	lexer.parsing_error(node->tkn, "Missing right argument for binary operator '%s'.", get_token_name_str(tkn_type).c_str());
    }
    return node;
}

Ast_node *led_trigger(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    lexer.next_token();
    if(!(lexer.not_eof() && add_alternative_sub(node, node->sub, parse_expression(lexer, ast, node, op_semantics_table[tkn_type].rbp)))) // add right sub
	lexer.parsing_error(node->tkn, "Missing right argument for binary operator '%s'.", get_token_name_str(tkn_type).c_str());
    return node;
}

// TODO: enhance this funciton such that it catches all nodes, which can call a function
static bool is_func_call(Ast_node *node) { return node->tkn.type == tkn_ident; }

Ast_node *led_parenthesis(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    
    if(is_func_call(left)) node->type = nt_func_call;
    else                   return nullptr;
    
    add_alternative_sub(node, node->sub, nud_bracket(tkn_type, lexer, ast, nullptr, node));
    return node;
}

// TODO: maybe enhance!
static bool is_type(Ast_node *node)
{
    return node->tkn.type == tkn_ident || is_base_type(node->tkn.type);
}

Ast_node *led_bracket(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    if(is_type(left) || left->tkn.type == '[')
	node->type = nt_array_type;
    else
	return nullptr;
    add_alternative_sub(node, node->sub, nud_bracket(tkn_type, lexer, ast, nullptr, node));
    return node;
}

// declarations open new scopes
Ast_node *led_declare(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);

    Scope_info enclosing_scope = g_scope_info.next_scope(left, hash_string_view(left->tkn.sv, g_scope_info.scope_hash));
    
    lexer.next_token();
    // adding right
    if(!(lexer.not_eof() && add_alternative_sub(node, node->sub, parse_expression(lexer, ast, node, op_semantics_table[tkn_type].rbp))))
	lexer.parsing_error(node->tkn, "Missing right argument for binary operator '%s'.", get_token_name_str(tkn_type).c_str());
    
    Ast_node* right = node->sub->alt_sub;
    Ast_node* prev_sub = right;
    Type_flags flags = TF_none;

    if(!left || !right)
	goto end;

    if(left->tkn.type != tkn_ident) {
	lexer.parsing_error(node->tkn, "Expected an identifier, got '%s'", get_token_name_str(left->tkn.type).c_str());
	goto end;
    }
    
    std::cout << "type: " << get_token_name_str(left->tkn.type) << '\n';
    HG_DEB_assert(left->id != 0, "id should be defined");
    std::cout << "id: " << left->id << " sv: " << left->tkn.sv << '\n';
    HG_DEB_assert(ast.identifier_set.find(left->id) != ast.identifier_set.end(), "id should have an identifier_set entry");
    
    if(right->tkn.type == tkn_ident)
	flags = Type_flags(flags | TF_ident);
    if(right->type == nt_array_type)
	flags = Type_flags(flags | TF_array);
    if(is_base_type(right->tkn.type))
	flags = Type_flags(flags | TF_base_type);
    if(right->tkn.type == '{')
	flags = Type_flags(flags | TF_set);
    ast.identifier_set.at(left->id) = Identifier_info{left, flags};

    while(lexer.tkn_at(0).type == ':') {
	lexer.next_token();
	// add the next declaration
	if(!(lexer.not_eof() && add_alternative_sub(node, &prev_sub, parse_expression(lexer, ast, node, op_semantics_table[Token_enum(':')].rbp)))) {
	    lexer.parsing_error(lexer.tkn_at(0), "Missing declaration statemenet after ':'.");
	    goto end;
	}
    }

end:
    g_scope_info.restore_scope(enclosing_scope);
    return node;
}

Ast_node *led_dot(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    
    if(!is_reference_tkn(left->tkn.type)) {
	lexer.parsing_error(left->tkn, "Expected an identifier, 'this' or a placeholder, but got '%s'.", get_token_name_str(left->tkn.type).c_str());
	return node;
    }

    Scope_info enclosing_scope = g_scope_info.next_scope(left, hash_string_view(left->tkn.sv, g_scope_info.scope_hash));
    
    // adding right
    lexer.next_token();
    if(!(lexer.not_eof() && add_alternative_sub(node, node->sub, parse_expression(lexer, ast, node, op_semantics_table[tkn_type].rbp))))
	lexer.parsing_error(node->tkn, "Missing right argument for binary operator '%s'.", get_token_name_str(tkn_type).c_str());

    g_scope_info.restore_scope(enclosing_scope);
    return node;
}

// NOTE: left can be a set or identifer
Ast_node *led_to_decl(LED_ARGS)
{
    Ast_node* node = led_normal(tkn_type, lexer, ast, left, super);
    if(left->tkn.type == tkn_ident) {
	uint64_t left_id = ast.find_ident(left, g_scope_info.scope_hash);
	if(!left_id) {	    lexer.parsing_error(left->tkn, "This object is not in scope.");
	    return node;
	}
	left->id = left_id;
	// CONTINUE HERE
    }
    return node;
}

bool tkn_legal_in_global_space(Token_enum type)
{
    return type == ':' || type == tkn_func || type == tkn_do;
}

void build_ast(Lexer &lexer, Ast &ast)
{
    g_scope_info = Scope_info{};
    lexer.next_token();
    Ast_node* prev_alt;
    
    if(lexer.not_eof()) {
    get_first_node:
	ast.global_scope.sub = parse_expression(lexer, ast, &ast.global_scope, 0);
	if(ast.global_scope.sub != nullptr) {
	    prev_alt = ast.global_scope.sub;
	    if(!tkn_legal_in_global_space(prev_alt->tkn.type))
		lexer.parsing_error(prev_alt->tkn, "Unexpected token, only object declarations are allowed in global space.");
	}
	else
	    goto get_first_node;
    }
    
    while(lexer.not_eof()) {
	const char* start_ptr = lexer.tkn_at(0).ptr;
	prev_alt->alt_sub = parse_expression(lexer, ast, &ast.global_scope, 0);
	if(prev_alt->alt_sub != nullptr) {
	    prev_alt->alt_sub->super = &ast.global_scope;
	    prev_alt = prev_alt->alt_sub;
	    if(!tkn_legal_in_global_space(prev_alt->tkn.type))
		lexer.parsing_error(prev_alt->tkn, "Unexpected token, only object declarations are allowed in global space.");
	}
	// failsafe for when we are not making progress.
	if(start_ptr == lexer.tkn_at(0).ptr)
	    lexer.next_token();
    }
}

void parse_latest(Lexer &lexer, Ast &ast)
{
    build_ast(lexer, ast);
    if(lexer.parsing_error_cnt)
	std::cout << "Compilation interrupted: " HG_ERROR_COLOR << lexer.parsing_error_cnt << " parsing errors" HG_END_COLOR ".\n";
    else
	std::cout << "Parsing finished" HG_SUCCESS_COLOR " successfully" HG_END_COLOR ".\n";
}

void Parser::parse_file(const char *file_path)
{
    lexer.load_input_from_file(file_path);
    parse_latest(lexer, ast);
}

void Parser::parse_string(const char *string)
{
    lexer.load_input_from_string(string);
    parse_latest(lexer, ast);
}
