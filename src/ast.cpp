#include "ast.hpp"

#include <iostream>
#include <string_view>
#include <unordered_map>

#include "lexer.hpp"
#include "parser.hpp"
#include "utils.hpp"
#include "log_and_debug.hpp"

bool compare_ident_nodes(Ast_node *id_a, Ast_node *id_b)
{
    // traverse tree and check for equivalence, also include supers for scope hash!
    
    return true;
}

static void super_delete(Ast_node *node)
{
    if(node->sub)
	super_delete(node->sub);
    if(node->alt_sub)
	super_delete(node->alt_sub);
    delete node;
}

Ast_node *node_delete(Ast_node *node)
{
    if(node->sub)
	super_delete(node->sub);
    Ast_node* alt_sub = node->alt_sub;
    delete node;
    return alt_sub;
}

Ast_node *super_copy_expression_in_new_scope(Ast_node *node, Parser& parser) // UNTESTED
{
    Ast_node* new_node = copy_expression_in_new_scope(node, parser);
    if(node->alt_sub) {
	new_node->alt_sub = super_copy_expression_in_new_scope(node->alt_sub, parser);
	new_node->alt_sub->super = new_node->super;
    }
    return new_node;
}

Ast_node *copy_expression_in_new_scope(Ast_node *node, Parser& parser) // UNTESTED
{
    Ast_node* new_node = new Ast_node;
    new_node->type_result = node->type_result;
    new_node->type_flags = node->type_flags;
    if(node->id) {
	HG_DEB_assert(node->tkn.type == tkn_ident, "");
	new_node->id = parser.ast.add_ident(node, parser.scope_info.scope_ident);
	HG_DEB_assert(new_node->id, "It must be guaranteed that the object can be copied");
    }
    new_node->tkn = node->tkn;
    if(node->sub) {
	if(node->type_flags & TF_Declaration) {
	    HG_DEB_assert(new_node->id, "");
	    Scope_info enclosing_scope = parser.scope_info.next_scope(new_node);
	    {
		new_node->sub = super_copy_expression_in_new_scope(node->sub, parser);
	    }
	    parser.scope_info.restore_scope(enclosing_scope);
	}
	else {
	    new_node->sub = super_copy_expression_in_new_scope(node->sub, parser);
	}
	new_node->sub->super = new_node;
    }
    return new_node;
}

Hash Ast::find_ident_in_scope(Ast_node *ident_node, Ast_node* scope_super)
{
    HG_DEB_assert(scope_super->tkn.type == tkn_ident || scope_super->tkn.type == tkn_global_scope, "expected tkn_ident");
    HG_DEB_assert(ident_node->tkn.type == tkn_ident, "expected tkn_ident");
    
    Hash id = scope_super->id;
    id = hash_char('.', id);
    id = hash_string_view(ident_node->tkn.sv, id);

    int itr_cnt = 0;
    decltype(identifier_set)::iterator itr;
    do {
	itr = identifier_set.find(id);
	if(itr == identifier_set.end())
	    return 0;
	++id; // preliminary increment.
	++itr_cnt;
	if(itr_cnt > 10000)
	    HG_DEB_error("infinite loop");
    } while(itr->second->tkn.sv != ident_node->tkn.sv);
    
    return id - 1; // reversing preliminary increment.
}

Hash Ast::find_ident(Ast_node *node, Ast_node* scope_super)
{
    switch(node->tkn.type) {
    case tkn_ident:
    {
	Hash id = 0;
	while(id == 0 && scope_super) {
	    id = find_ident_in_scope(node, scope_super);
	    do{
		scope_super = scope_super->super;
	    } while(scope_super && scope_super->tkn.type != tkn_ident && scope_super->tkn.type != tkn_global_scope);
	}
	return id;
    }
    case '.':
	scope_super = identifier_set.at(find_ident(node->sub, scope_super));
	// alt_sub will always be just an identifier.
	return find_ident_in_scope(node->sub->alt_sub, scope_super);
    case '\\':
	scope_super = identifier_set.at(find_ident(node->sub, scope_super->super));
	return find_ident_in_scope(node->sub->alt_sub, scope_super->super);
    default:
	return 0;
    }
}

// returns 0 if the ident was already added.
// returns the idents new id otherwise, it will also write id to that node.
Hash Ast::add_ident(Ast_node *ident_node, Ast_node *scope_super)
{
    HG_DEB_assert(ident_node->tkn.type == tkn_ident, "expected tkn_ident");
    
    Hash id = scope_super->id;
    id = hash_char('.', id);
    id = hash_string_view(ident_node->tkn.sv, id);

    int itr_cnt = 0;
    decltype(identifier_set)::iterator itr;
    do {
	itr = identifier_set.find(id);
	if(itr == identifier_set.end()) {
	    identifier_set.insert({id, ident_node});
	    ident_node->id = id;
	    // std::cout << "add_ident (id: " << id <<  "): " << ident_node->tkn.sv << '\n';
	    return id;
	}
	++id;
	++itr_cnt;
	if(itr_cnt > 10000)
	    HG_DEB_error("infinite loop");
    } while(itr->second->tkn.sv != ident_node->tkn.sv);
    // std::cout << "add_ident (id: 0): " << ident_node->tkn.sv << '\n';
    return 0;
}


void Ast::print(Print_ast_enum config) const
{
    std::cout << '\n';
    print_node(&global_scope, config);
    std::cout << '\n';
}

void Ast::print_node(const Ast_node *node, Print_ast_enum config, int depth) const
{
    for(int i = 0; i < depth; ++i)
	std::cout << "   ";
    
    if(config & PN_Content) {
	switch(node->tkn.type) {
	case tkn_ident:
	case tkn_string: std::cout << node->tkn.sv; break;
	case tkn_int: std::cout << node->tkn.i; break;
	case tkn_real: std::cout << node->tkn.d; break;
	default: std::cout << HG_BRIGHT_MAGENTA_COLOR << get_token_name_str(node->tkn.type) << HG_END_COLOR;
	}
    }
    else {
	std::cout << HG_BRIGHT_MAGENTA_COLOR << get_token_name_str(node->tkn.type) << HG_END_COLOR;
    }

    if((config & PN_Type_result) && node->type_result != T_None) {
	std:: cout << HG_BRIGHT_BLACK_COLOR << " TR: " << type_enum_name_table[node->type_result] << HG_END_COLOR;
    }

    if(config & PN_Type_flags) {
	std::cout << HG_BRIGHT_BLUE_COLOR;
	if(node->type_flags)
	    std::cout << " TFs: ";
	for(uint32_t iflag = 0; iflag < log2i(uint32_t(TF_SIZE)); ++iflag) {
	    if(check_type_flag(node, Type_flags(1 << iflag)))
		std::cout << get_type_flag_name(Type_flags(1 << iflag)) << " ";
	}
	std::cout << HG_END_COLOR;
    }

    if(config & PN_Ident_idx) {
	// if(node->type_result == T_Declared_Object || node->type_result == T_Data_Object || node->type_result == T_Function_Object)
	if(node->tkn.type == tkn_ident)
	    std:: cout << HG_BRIGHT_CYAN_COLOR << " ID: " << node->id << HG_END_COLOR;
    }
    
    if((config & PN_Super) && node->super) {
	std::cout << HG_BRIGHT_GREEN_COLOR << " S: " << get_token_name_str(node->super->tkn.type) << HG_END_COLOR;
    }
    
    std::cout << '\n';
    
    if(node->sub)
	print_node(node->sub, config, depth + 1);
    if(node->alt_sub)
	print_node(node->alt_sub, config, depth);
}

// uint64_t Ast_node::count_subs(Ast_node* node) const {
//     Ast_node* sub = node->sub;
//     uint64_t cnt = 0;
//     while(sub) {
// 	++cnt;
// 	sub = sub->alt_sub;
//     }
//     return cnt;
// }
