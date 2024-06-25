#include "semantics.hpp"
#include <cstdint>

#include "ast.hpp"
#include "lexer.hpp"
#include "parser.hpp"

bool tkn_legal_in_global_space(Token_enum type) {return type == ':' || type == tkn_do;}

#define TC_BASE_TYPES_TABLE_IDX(a, b) (int(a) * T_SIZE + int(b))

consteval std::array<Type_compare, T_SIZE * T_SIZE> get_base_type_compare_table()
{
    std::array<Type_compare, T_SIZE * T_SIZE> table;
    table.fill(Type_compare::Disjoint);

    // NOTE: employs properties of the specific supported integer types.
    for(uint16_t type_a = T_i8; type_a <= T_f64; ++type_a) {
	for(uint16_t type_b = T_i8; type_b <= T_f64; ++type_b) {
	    // all numeric types contain 0
	    table[TC_BASE_TYPES_TABLE_IDX(type_a, type_b)] = Type_compare::Intersecting;
	    
	    if(type_a == type_b)
		table[TC_BASE_TYPES_TABLE_IDX(type_a, type_b)] = Type_compare::Equal;
	    else if(Type_num_limits_table[type_a].max < Type_num_limits_table[type_b].max
		    && Type_num_limits_table[type_a].min >= Type_num_limits_table[type_b].min
		    && !(is_base_type_floating_point(Type_enum(type_a)) && is_base_type_integer(Type_enum(type_b))))
	    {
		table[TC_BASE_TYPES_TABLE_IDX(type_a, type_b)] = Type_compare::A_subset_B;
	    }
	    else if(Type_num_limits_table[type_a].max > Type_num_limits_table[type_b].max
		    && Type_num_limits_table[type_a].min <= Type_num_limits_table[type_b].min
		    && !(is_base_type_floating_point(Type_enum(type_b)) && is_base_type_integer(Type_enum(type_a))))
	    {
		table[TC_BASE_TYPES_TABLE_IDX(type_a, type_b)] = Type_compare::B_subset_A;
	    }
	}
    }
    
    return table;
}

constexpr auto Base_type_compare_table = get_base_type_compare_table();

static Type_compare compare_base_types(Type_enum type_a, Type_enum type_b)
{
    if(type_a == type_b)
	return Type_compare::Equal;
    
    if((is_base_type_integer(type_a) && is_base_type_integer(type_b)) || (is_base_type_floating_point(type_a) && is_base_type_floating_point(type_b)))
	return Base_type_compare_table[TC_BASE_TYPES_TABLE_IDX(type_a, type_b)];
    return Type_compare::Disjoint;
}

Type_compare compare_data_types(Ast_node *node_a, Ast_node *node_b, Parser parser)
{
    return Type_compare::Disjoint;
}

Type_compare compare_function_types(Ast_node *node_a, Ast_node *node_b, Parser parser)
{
    return Type_compare::Disjoint;
}

Type_compare compare_types(Ast_node *node_a, Ast_node *node_b, Parser parser)
{
    if(node_a->type_result == T_Decl_ref)
	node_a = parser.ast.find_object_with_id(node_a->id);
    if(node_b->type_result == T_Decl_ref)
	node_b = parser.ast.find_object_with_id(node_b->id);
    
    HG_DEB_assert(node_a && node_b, "");
    HG_DEB_assert(node_a->typ_result != T_Decl_ref && node_b->type_result != T_Decl_ref, "");

    if((node_a->type_result == T_Data_Object)
	&& (node_b->type_result == T_Data_Object))
    {
	    return compare_data_types(node_a, node_b, parser);
    }
    else if((node_a->type_result == T_Function_Object)
	    && (node_b->type_result == T_Function_Object))
    {
	    return compare_function_types(node_a, node_b, parser);
    }
    else if(is_base_type(node_a->type_result) && is_base_type(node_b->type_result)) {
	return compare_base_types(node_a->type_result, node_b->type_result);
    }
    else {
	return Type_compare::Disjoint;
    }
}
