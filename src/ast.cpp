#include "ast.hpp"

#include <iostream>
#include <string_view>
#include <unordered_map>

#include "lexer.hpp"
#include "utils.hpp"
#include "log_and_debug.hpp"

const char* node_type_name_table[nt_SIZE] {
    "none",
    "type",
    "array_type",
    "object",
    "attribute",
    "builtin_func",
    "func",
    "func_in",
    "func_out",
    "func_body",
    "func_call",
    "procedure",
    "arithm_op",
    "arithm_expr",
    "object_op",
    "object_expr"
};

bool compare_ident_nodes(Ast_node *id_a, Ast_node *id_b)
{
    // traverse tree and check for equivalence, also include supers for scope hash!
    
    return true;
}


uint64_t Ast::find_ident(Ast_node *node, uint64_t scope_hash)
{
    HG_DEB_assert(node->tkn.type == tkn_ident, "expected tkn_ident");
    
    uint64_t id = scope_hash;

    // for loop over the identifier structure.
    if(node->tkn.type == '.') {
	HG_DEB_not_implemented;	
    }
    else if(node->tkn.type == tkn_ident) {
	id = hash_c_str(".", id);
	id = hash_string_view(node->tkn.sv, id);
    }
    else {
	HG_DEB_error("expected a valid identifier construct");
    }

    std::unordered_map<uint64_t, Identifier_info>::iterator itr;
    do {
	itr = identifier_set.find(id);
	if(itr == identifier_set.end())
	    return 0;
	id += 1;
    } while(itr->second.ident->tkn.sv != node->tkn.sv);
    
    return id - 1;
}


uint64_t Ast::add_ident(Ast_node *node, uint64_t scope_hash)
{
    HG_DEB_assert(node->tkn.type == tkn_ident, "expected tkn_ident");
    
    uint64_t id = scope_hash;

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
    
    if(node->type != nt_none)
	std::cout << HG_BRIGHT_BLUE_COLOR << node_type_name_table[node->type] << HG_END_COLOR " ";
    
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
