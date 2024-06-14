#include "ast.hpp"

#include <iostream>
#include <string_view>
#include <unordered_map>

#include "lexer.hpp"
#include "utils.hpp"
#include "log_and_debug.hpp"

bool compare_ident_nodes(Ast_node *id_a, Ast_node *id_b)
{
    // traverse tree and check for equivalence, also include supers for scope hash!
    
    return true;
}

void super_delete(Ast_node *node)
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

Hash Ast::find_ident_in_scope(Ast_node* scope_super, Ast_node *ident_node)
{
    HG_DEB_assert(scope_super->tkn.type == tkn_ident, "expected tkn_ident");
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
	id += 1; // preliminary increment.
	++itr_cnt;
	if(itr_cnt > 10000)
	    HG_DEB_error("infinite loop");
    } while(itr->second->tkn.sv != ident_node->tkn.sv);
    
    return id - 1; // reversing preliminary increment.
}

Hash Ast::find_ident(Ast_node* scope_super, Ast_node *node)
{
    switch(node->tkn.type) {
    case tkn_ident:
    {
	Hash id = 0;
	while(id == 0 && scope_super) {
	    id = find_ident_in_scope(scope_super, node);
	    scope_super = scope_super->super;
	}
	return id;
    }
    case '.':
	scope_super = identifier_set.at(find_ident(scope_super, node->sub));
	// alt_sub will always be just an identifier.
	return find_ident_in_scope(scope_super, node->sub->alt_sub);
    case '\\':
	scope_super = identifier_set.at(find_ident(scope_super->super, node->sub));
	return find_ident_in_scope(scope_super->super, node->sub->alt_sub);
    default:
	return 0;
    }
}

Hash Ast::add_ident(Ast_node *node, Hash scope_hash)
{
    HG_DEB_assert(node->tkn.type == tkn_ident, "expected tkn_ident");
    
    Hash id = scope_hash;

    // for loop over the identifier structure and the scope.
    if(node->tkn.type == '.') {
	HG_DEB_not_implemented;
    }
    else if(node->tkn.type == tkn_ident) {
	id = hash_c_str(".", id);
	id = hash_string_view(node->tkn.sv, id);
    }
    else {
	return 0;
    }
    
    auto itr = identifier_set.find(id);
    if(itr != identifier_set.end())
	return 0;
    
    identifier_set.insert({id, Identifier_info{node}});
    return id;
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
    
    if(config & PN_SHOW_CONTENT) {
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
    
    if((config & PN_SUPER) && node->super)
	std::cout << HG_BRIGHT_GREEN_COLOR << " S: " << get_token_name_str(node->super->tkn.type) << HG_END_COLOR;
    
    std::cout << '\n';
    
    if(node->sub)
	print_node(node->sub, config, depth + 1);
    if(node->alt_sub)
	print_node(node->alt_sub, config, depth);
}

uint64_t Ast_node::count_subs(Ast_node* node) const {
    Ast_node* sub = node->sub;
    uint64_t cnt = 0;
    while(sub) {
	++cnt;
	sub = sub->alt_sub;
    }
    return cnt;
}
