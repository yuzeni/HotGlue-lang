
#include "tdop_functions.hpp"

#include <cstdint>
#include <limits>
#include <cmath>

#include "ast.hpp"
#include "lexer.hpp"
#include "semantics.hpp"
#include "parser.hpp"

#include "utils.hpp"
#include "log_and_debug.hpp"

static bool add_single_sub(Ast_node *node, Ast_node *sub)
{
    if(node && sub) {
	node->sub = sub;
	sub->super = node;
    }
    if(sub) return true;
    return false;
}

// NOTE: automatically updates prev_sub
static bool add_alternative_sub(Ast_node *node, Ast_node **prev_sub, Ast_node *alt_sub)
{
    if(prev_sub && *prev_sub && alt_sub && node) {
	(*prev_sub)->alt_sub = alt_sub;
	alt_sub->super = node;
	*prev_sub = (*prev_sub)->alt_sub;
    }
    if(alt_sub) return true;
    return false;
}

static bool add_alternative_sub(Ast_node *node, Ast_node *prev_sub, Ast_node *alt_sub)
{
    if(prev_sub && alt_sub && node) {
	prev_sub->alt_sub = alt_sub;
	alt_sub->super = node;
    }
    if(alt_sub) return true;
    return false;
}

static bool get_and_add_right_unary(Token_enum tkn_type, Ast_node* node, Lexer& lexer, Parser& parser)
{
    if(!(lexer.not_eof() && add_single_sub(node, parse_expression(lexer, parser, node, tkn_semantics_table[tkn_type].rbp)))) {
	lexer.parsing_error(node->tkn, "Missing right argument for unary operator '%s'.", get_token_name_str(tkn_type).c_str());
	return false;
    }
    return true;
}

static bool get_and_add_right_binary(Token_enum tkn_type, Ast_node* node, Lexer& lexer, Parser& parser)
{
    if(!(lexer.not_eof() && add_alternative_sub(node, node->sub, parse_expression(lexer, parser, node, tkn_semantics_table[tkn_type].rbp)))) {
	lexer.parsing_error(node->tkn, "Missing right argument for binary operator '%s'.", get_token_name_str(tkn_type).c_str());
    	return false;
    }
    return true;
}

static bool get_and_add_next_sub(Token_enum tkn_type, Ast_node* node, Ast_node** prev_sub, Lexer& lexer, Parser& parser)
{
    if(!(lexer.not_eof() && add_alternative_sub(node, prev_sub, parse_expression(lexer, parser, node, tkn_semantics_table[tkn_type].rbp)))) {
	lexer.parsing_error(lexer.tkn_at(0), "Missing statemenet after '%s'.");
	return false;
    }
    return true;
}

static Ast_node* replace_node_with_single_sub(Ast_node *node)
{
    HG_DEB_assert(node->sub->alt_sub == nullptr, "node must only have a single sub");
    Ast_node sub = *(node->sub);
    delete node->sub;
    Ast_node* super = node->super;
    *node = sub;
    node->super = super;
    return node;
}

Ast_node *nud_error(NUD_ARGS)
{
    lexer.parsing_error(lexer.tkn_at(0), "The token '%s' has no unary method.", get_token_name_str(tkn_type).c_str());
    Semantic_code tkn_sema = tkn_semantics_table[tkn_type];
    // if it has a led try that one instead.
    if(tkn_sema.led != led_error && left)
	return tkn_sema.led(PASS_LED_ARGS);
    lexer.next_token();
    return nullptr;
}

Ast_node *nud_ident(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_None};
    lexer.next_token();
    node->id = parser.ast.find_ident_in_scope(node, parser.scope_info.scope_ident);
    if(node->id)
	node->type_result = T_Declared_Object;
    return node;
}

Ast_node *nud_int(NUD_ARGS)
{
    Type_enum type_result = T_None;
    if(lexer.tkn_at(0).i == 0 || lexer.tkn_at(0).i == 1)
	type_result = T_bool;
    else if(lexer.tkn_at(0).i > 0) {
	for(uint16_t type = T_u8; type < T_u64; ++type) {
	    if(lexer.tkn_at(0).i <= int64_t(Type_num_limits_table[type].max)) {
		type_result = Type_enum(type);
		break;
	    }
	}
    }
    else {
	for(uint16_t type = T_i8; type < T_i64; ++type) {
	    if(lexer.tkn_at(0).i <= int64_t(Type_num_limits_table[type].max && lexer.tkn_at(0).i >= int64_t(Type_num_limits_table[type].min))) {
		type_result = Type_enum(type);
		break;
	    }
	}
    }
    
    Ast_node* node = nud_arg(PASS_NUD_ARGS);
    node->type_result = type_result;
    return node;
}

Ast_node *nud_real(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_f64};
    lexer.next_token();
    return node;
}

Ast_node *nud_string(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_str};
    lexer.next_token();
    return node;
}

Ast_node *nud_true(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_bool};
    lexer.next_token();
    return node;
}

Ast_node *nud_false(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_bool};
    lexer.next_token();
    return node;
}

Ast_node *nud_placeholder(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_placeholder};
    lexer.next_token();
    return node;
}

Ast_node *nud_exit(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_None};
    parser.exit();
    lexer.next_token();
    return node;
}

Ast_node *nud_types(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_None};
    lexer.next_token();
    return node;
}

Ast_node *nud_this(NUD_ARGS)
{
    Ast_node* node = nud_arg(PASS_NUD_ARGS);
    node->tkn.type = tkn_ident;
    if(parser.scope_info.scope_ident)
	node->tkn.sv = parser.scope_info.scope_ident->tkn.sv;
    else
	lexer.parsing_error(node->tkn, "Can't use 'this' outside a scope.");
    return node;
}

Ast_node *nud_all(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_All};
    lexer.next_token();
    return node;
}

Ast_node *nud_size(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_u64};
    lexer.next_token();
    get_and_add_right_unary(tkn_type, node, lexer, parser);

    if(node->sub->type_result == T_Type_Object || node->sub->type_result == T_Data_Object) {
	HG_DEB_not_implemented;
    }
    else {
	parser.type_error(node->sub, "Expected a 'type object' or 'data object', but got '%s'.", type_enum_name_table[node->sub->type_result]);
	// tansform to int
	node->tkn.type = tkn_int;
	node->tkn.i = 0;
    }
    
    return node;
}

Ast_node *nud_do(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_None};
    lexer.next_token();
    get_and_add_right_unary(tkn_type, node, lexer, parser);
    return node;
}

Ast_node *nud_expand(NUD_ARGS)
{
    Ast_node* expand_node = new Ast_node{lexer.tkn_at(0), super, T_Unnamed_Object};
    lexer.next_token();
    // get the set to expand over
    get_and_add_right_unary(tkn_type, expand_node, lexer, parser);
    lexer.next_token();

    // do type checking of the expand set
    // if the set is known already and also small, expand everything alread.
    // otherwise just make the necessary links.
    
    Ast_node* prev_sub = expand_node->sub;
    // get the statement to expand over
    get_and_add_next_sub(tkn_type, expand_node, &prev_sub, lexer, parser);

    // do type checking on the expand statement
    
    return expand_node;
}

Ast_node *nud_trigger(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_None};
    lexer.next_token();
    get_and_add_right_unary(tkn_type, node, lexer, parser);
    return node;
}

Ast_node *nud_delimiter(NUD_ARGS)
{
    lexer.next_token();
    return nullptr;
}

// BUG: this cannot be a nud!!
Ast_node *nud_left(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    lexer.next_token();
    add_single_sub(node, left);
    return node;
}

Ast_node *led_left(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    lexer.next_token();
    add_single_sub(node, left);
    return node;
}

Ast_node *nud_right(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    lexer.next_token();
    get_and_add_right_unary(tkn_type, node, lexer, parser);
    return node;
}

Ast_node *nud_arg(NUD_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    lexer.next_token();
    return node;
}

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
	bracket = nud_right(tkn_type, lexer, parser, left, super);
	Ast_node* prev_alt =  bracket->sub;

	// collect all statements within the brackets
	while(lexer.tkn_at(0).type != bracket_opposite(tkn_type)) {
	    if(lexer.tkn_at(0).type == tkn_eof) {
		lexer.parsing_error(lexer.tkn_at(-1), "Missing closing bracket.");
		break;
	    }
	    add_alternative_sub(bracket, &prev_alt, parse_expression(lexer, parser, bracket, 0));
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
    if(!is_binary_set_op(super->tkn.type))
	get_and_add_right_unary(tkn_type, node, lexer, parser);
    return node;
}

Ast_node *led_error(LED_ARGS)
{
    lexer.parsing_error(lexer.tkn_at(0), "The token '%s' has no binary method.", get_token_name_str(tkn_type).c_str());
    Semantic_code tkn_sema = tkn_semantics_table[tkn_type];    // We cant try nud, because we don't know super
    if(tkn_sema.nud != nud_error)
	return tkn_sema.nud(tkn_type, lexer, parser, left, super);
    lexer.next_token();
    return nullptr;
}

Ast_node *led_normal(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    lexer.next_token();
    get_and_add_right_binary(tkn_type, node, lexer, parser);
    return node;
}

Ast_node *led_req(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    lexer.next_token();
    get_and_add_right_binary(tkn_type, node, lexer, parser);
    return node;
}

Ast_node *led_trigger(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    lexer.next_token();
    get_and_add_right_binary(tkn_type, node, lexer, parser);
    return node;
}

// TODO: enhance this funciton such that it catches all nodes, which can call a function
static bool is_func_call(Ast_node *node) { return node->tkn.type == tkn_ident; }

Ast_node *led_parenthesis(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    
    if(!is_func_call(left))
	return nullptr;
    
    add_alternative_sub(node, node->sub, nud_bracket(tkn_type, lexer, parser, nullptr, node));
    return node;
}

// TODO: maybe enhance!
static bool is_type(Ast_node *node)
{
    return node->tkn.type == tkn_ident || (node->tkn.type >= tkn_i8 && node->tkn.type <= tkn_placeholder);
}

Ast_node *led_bracket(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    add_single_sub(node, left);
    if(!(is_type(left) || left->tkn.type == '['))
	return nullptr;
    add_alternative_sub(node, node->sub, nud_bracket(tkn_type, lexer, parser, nullptr, node));
    return node;
}

static Ast_node* handle_declare_signifiers(Ast_node* left, Lexer& lexer)
{
    // add signifier type flags

    Type_flags type_flags = TF_None;
    switch(left->tkn.type) {
    case tkn_is_type:
	type_flags = TF_Pure_type;
	break;
    case '|':
	type_flags = TF_Complete_const;
	break;
    case tkn_extern:
	type_flags = TF_Extern;
	break;
    case tkn_exread:
	type_flags = TF_Exread;
	break;
    case tkn_exwrite:
	type_flags = TF_Exwrite;
	break;
    case tkn_exlayout:
	if(left->sub) {
	    if(left->sub->tkn.type == tkn_AoS)
		type_flags = TF_AoS;
	    else if (left->sub->tkn.type == tkn_SoA)
		type_flags = TF_SoA;
	    else
		lexer.parsing_error(left->sub->tkn, "Expected either token 'AoS' or 'SoA', but got '%s'.", get_token_name_str(left->sub->tkn.type).c_str());
	    if(left->sub->sub)
		left->sub = replace_node_with_single_sub(left->sub);
	}
	else {
	    lexer.parsing_error(left->tkn, "Missing right argument for '%s'.", get_token_name_str(left->tkn.type).c_str());
	}
	break;
    case tkn_AoS:
    case tkn_SoA:
	lexer.parsing_error(left->tkn, "You must put '%s' infront of memory layout specifiers.", get_token_name_str(tkn_exlayout).c_str());
	break;
    }
	
    if(left->sub) {
	left = replace_node_with_single_sub(left);
	left->type_flags = type_flags;
    }
    else {
	lexer.parsing_error(left->tkn, "Expected an identifier after '%s'.", get_token_name_str(left->tkn.type).c_str());
    }
    return left;
}

// declarations open new scopes
Ast_node *led_declare(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    lexer.next_token();

    if(is_declare_signifier_tkn(left->tkn.type))
	left = handle_declare_signifiers(left, lexer);
    
    add_single_sub(node, left);


    if(left->tkn.type == tkn_ident) {
	if(left->type_result == T_Declared_Object || parser.ast.add_ident(left, parser.scope_info.scope_ident) == 0)
	    lexer.parsing_error(left->tkn, "An object with the identifier '%s' was already declared in this scope.", std::string(left->tkn.sv).c_str());
	
	Scope_info this_scope = parser.scope_info.next_scope(left);
	{
	    get_and_add_right_binary(tkn_type, node, lexer, parser);
	    node->type_result = node->sub->alt_sub->type_result;
	    node->type_flags = node->sub->alt_sub->type_flags;
	}
	parser.scope_info.restore_scope(this_scope);
	return node;
    }
    else if(left->tkn.type == ':') {
	get_and_add_right_binary(tkn_type, node, lexer, parser);
	Ast_node* right = node->sub->alt_sub;
	
	Type_compare tc = compare_types(left->sub->alt_sub, right);
	if(tc == Type_compare::B_subset_A || tc == Type_compare::Equal) {
	    node_delete(left->sub->alt_sub);
	    left->sub->alt_sub = node;
	}
	else {
	    lexer.parsing_error(right->tkn, "This type is not a subset of- or equivalnet to the type in the previous declaration.");
	}
	return left;
    }
    else {
	lexer.parsing_error(left->tkn, "Expected a declaration or an identifier, but got '%s'", get_token_name_str(left->tkn.type).c_str());
	return node;
    }
}

Ast_node *led_dot(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super, T_None};
    lexer.next_token();

    add_single_sub(node, left);

    if(left->tkn.type == tkn_placeholder)
	left->type_result = T_Declared_Object;
    
    if(left->type_result != T_Declared_Object) {
	parser.type_error(left, "Expected type '%s', but got type '%s'.", type_enum_name_table[T_Declared_Object], type_enum_name_table[left->type_result]);
	return node;
    }
    HG_DEB_assert(left->id, "id must be defined.");
    
    Scope_info this_scope = parser.scope_info.next_scope(parser.ast.identifier_set.at(left->id));
    {
	get_and_add_right_binary(tkn_type, node, lexer, parser);
	
	Ast_node* right = node->sub->alt_sub;
	if(right->type_result == T_Declared_Object) {
	    HG_DEB_assert(right->id, "id must be defined.");
	    node->id = parser.ast.find_ident_in_scope(right, parser.scope_info.scope_ident);
	}
	else {
	    if(right->tkn.type == tkn_ident)
		lexer.parsing_error(right->tkn, "The identifier is not declared in this scope.");
	    else
		parser.type_error(right, "Expected type '%s', but got type '%s'.", type_enum_name_table[T_Declared_Object], type_enum_name_table[right->type_result]);
	}
    }
    parser.scope_info.restore_scope(this_scope);
    
    return node;
}

// // NOTE: left can be a set or identifer
// Ast_node *led_to_decl(LED_ARGS)
// {
//     Ast_node* node = led_normal(tkn_type, lexer, parser, left, super);
//     if(left->tkn.type == tkn_ident) {
// 	uint64_t left_id = parser.ast.find_ident(left, parser.scope_info.scope_hash);
// 	if(!left_id) {
// 	    lexer.parsing_error(left->tkn, "This object is not in scope.");
// 	    return node;
// 	}
// 	left->id = left_id;
// 	// CONTINUE HERE
//     }
//     return node;
// }

// static Ast_node *led_to_imp(LED_ARGS);
// static Ast_node *led_not_to (LED_ARGS);
// static Ast_node *led_func_body(LED_ARGS);
// static Ast_node *led_set_eq(LED_ARGS);
// static Ast_node *led_update_add(LED_ARGS);
// static Ast_node *led_update_sub(LED_ARGS);
// static Ast_node *led_update_mul(LED_ARGS);
// static Ast_node *led_update_div(LED_ARGS);

// static Ast_node *led_or(LED_ARGS);
// static Ast_node *led_and(LED_ARGS);
// static Ast_node *led_eq(LED_ARGS);
// static Ast_node *led_neq(LED_ARGS);
// static Ast_node *led_less(LED_ARGS);
// static Ast_node *led_greater(LED_ARGS);
// static Ast_node *led_less_eq(LED_ARGS);
// static Ast_node *led_greater_eq(LED_ARGS);
// static Ast_node *led_add(LED_ARGS);
// static Ast_node *led_sub(LED_ARGS);
// static Ast_node *led_mul(LED_ARGS);
// static Ast_node *led_div(LED_ARGS);
// static Ast_node *led_modulo(LED_ARGS);
// static Ast_node *led_pow(LED_ARGS); 
