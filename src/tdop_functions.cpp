
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
    Ast_node* node = nud_arg(PASS_NUD_ARGS);
    // lookup declare type of identifier
    // node->type_result = ;
    return node;
}

Ast_node *nud_int(NUD_ARGS)
{
    Type_enum type_result = T_None;
    if(lexer.tkn_at(0).i == 0 || lexer.tkn_at(0).i == 1)
	type_result = T_bool;
    else if(lexer.tkn_at(0).i > 1) {
	if(lexer.tkn_at(0).i <= std::numeric_limits<uint8_t>::max())
	    type_result = T_u8;
	else if(lexer.tkn_at(0).i <= std::numeric_limits<uint16_t>::max())
	    type_result = T_u16;
	else if(lexer.tkn_at(0).i <= std::numeric_limits<uint32_t>::max())
	    type_result = T_u32;
	else if(lexer.tkn_at(0).i <= std::numeric_limits<uint64_t>::max())
	    type_result = T_u64;
    }
    else {
	// can't use abs, because abs(min) != abs(max)
	if(lexer.tkn_at(0).i <= std::numeric_limits<int8_t>::max() && lexer.tkn_at(0).i >= std::numeric_limits<int8_t>::min())
	    type_result = T_s8;
	else if(lexer.tkn_at(0).i <= std::numeric_limits<int16_t>::max() && lexer.tkn_at(0).i >= std::numeric_limits<int16_t>::min())
	    type_result = T_s16;
	else if(lexer.tkn_at(0).i <= std::numeric_limits<int32_t>::max() && lexer.tkn_at(0).i >= std::numeric_limits<int32_t>::min())
	    type_result = T_s32;
	else if(lexer.tkn_at(0).i <= std::numeric_limits<int64_t>::max() && lexer.tkn_at(0).i >= std::numeric_limits<int64_t>::min())
	    type_result = T_s64;
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
    return node->tkn.type == tkn_ident || (node->tkn.type >= tkn_s8 && node->tkn.type <= tkn_placeholder);
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

// declarations open new scopes
Ast_node *led_declare(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    lexer.next_token();
    
    add_single_sub(node, left);
    
    // Scope_info enclosing_scope = g_scope_info.next_scope(left, hash_string_view(left->tkn.sv, g_scope_info.scope_hash));
        
    get_and_add_right_binary(tkn_type, node, lexer, parser);
    
    Ast_node* right = node->sub->alt_sub;
    Ast_node* prev_sub = right;
    // Type_flags flags = TF_none;

    if(!left || !right)
	goto end;

    // if(is_any_type(left)) {
    // 	lexer.parsing_error(node->tkn, "Expected a type, got '%s'", get_token_name_str(left->tkn.type).c_str());
    // 	goto end;
    // }
    
    // std::cout << "type: " << get_token_name_str(left->tkn.type) << '\n';
    // HG_DEB_assert(left->id != 0, "id should be defined");
    // std::cout << "id: " << left->id << " sv: " << left->tkn.sv << '\n';
    // HG_DEB_assert(ast.identifier_set.find(left->id) != ast.identifier_set.end(), "id should have an identifier_set entry");
    
    // if(right->tkn.type == tkn_ident)
    // 	flags = Type_flags(flags | TF_ident);
    // if(right->type == nt_array_type)
    // 	flags = Type_flags(flags | TF_array);
    // if(is_base_type(right->tkn.type))
    // 	flags = Type_flags(flags | TF_base_type);
    // if(right->tkn.type == '{')
    // 	flags = Type_flags(flags | TF_set);
    // ast.identifier_set.at(left->id) = Identifier_info{left, flags};

    while(lexer.tkn_at(0).type == ':') {
	lexer.next_token();
	// add the next declaration
	if(!get_and_add_next_sub(Token_enum(':'), node, &prev_sub, lexer, parser))
	    goto end;
    }

end:
    // g_scope_info.restore_scope(enclosing_scope);
    return node;
}

Ast_node *led_dot(LED_ARGS)
{
    Ast_node* node = new Ast_node{lexer.tkn_at(0), super};
    lexer.next_token();

    add_single_sub(node, left);
    
    if(!is_reference_tkn(left->tkn.type)) {
	lexer.parsing_error(left->tkn, "Expected an identifier, 'this' or a placeholder, but got '%s'.", get_token_name_str(left->tkn.type).c_str());
	std::cout << get_token_name_str(node->tkn.type) << '\n';
	return node;
    }

    Scope_info enclosing_scope = parser.scope_info.next_scope(left, hash_string_view(left->tkn.sv, parser.scope_info.scope_hash));

    get_and_add_right_binary(tkn_type, node, lexer, parser);

    parser.scope_info.restore_scope(enclosing_scope);
    return node;
}

// NOTE: left can be a set or identifer
Ast_node *led_to_decl(LED_ARGS)
{
    Ast_node* node = led_normal(tkn_type, lexer, parser, left, super);
    if(left->tkn.type == tkn_ident) {
	uint64_t left_id = parser.ast.find_ident(left, parser.scope_info.scope_hash);
	if(!left_id) {
	    lexer.parsing_error(left->tkn, "This object is not in scope.");
	    return node;
	}
	left->id = left_id;
	// CONTINUE HERE
    }
    return node;
}

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
