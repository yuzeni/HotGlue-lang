#include "semantics.hpp"
#include <cstdint>

#include "ast.hpp"
#include "lexer.hpp"

bool tkn_legal_in_global_space(Token_enum type) {return type == ':' || type == tkn_do;}

bool is_object_type(Type_enum t)
{
    return t == T_Unnamed_Object || t == T_Type_Object || t == T_Function_Object || t == T_Data_Object;
}

#define TC_TABLE_IDX(a, b) (int(a) * T_SIZE + int(b))

consteval std::array<Type_compare, T_SIZE * T_SIZE> get_type_compare_table()
{
    std::array<Type_compare, T_SIZE * T_SIZE> table;
    table.fill(Type_compare::Disjoint);
    
    for(uint16_t type_a = T_s8; type_a <= T_u64; ++type_a) {
	for(uint16_t type_b = T_s8; type_b <= T_u64; ++type_b) {
	    if(type_a == type_b)
		table[TC_TABLE_IDX(type_a, type_b)] = Type_compare::Equal;
	    else if(Type_num_limits_table[type_a].max < Type_num_limits_table[type_b].max) {
		if(Type_num_limits_table[type_a].min >= Type_num_limits_table[type_b].min)
		    table[TC_TABLE_IDX(type_a, type_b)] = Type_compare::A_subset_B;
		else
		    table[TC_TABLE_IDX(type_a, type_b)] = Type_compare::Intersecting;
	    }
	    else if(Type_num_limits_table[type_a].max > Type_num_limits_table[type_b].max) {
		if(Type_num_limits_table[type_a].min <= Type_num_limits_table[type_b].min)
		    table[TC_TABLE_IDX(type_a, type_b)] = Type_compare::B_subset_A;
		else
		    table[TC_TABLE_IDX(type_a, type_b)] = Type_compare::Intersecting;
	    }
	    else 
		table[TC_TABLE_IDX(type_a, type_b)] = Type_compare::Intersecting;
	}
    }

    for(uint16_t type_a = T_f8; type_a <= T_f64; ++type_a) {
	for(uint16_t type_b = T_f8; type_b <= T_f64; ++type_b) {
	    if(type_a == type_b)
		table[TC_TABLE_IDX(type_a, type_b)] = Type_compare::Equal;
	    else if(type_a > type_b) // since the are ordered by size
		table[TC_TABLE_IDX(type_a, type_b)] = Type_compare::B_subset_A;
	    else
		table[TC_TABLE_IDX(type_a, type_b)] = Type_compare::A_subset_B;
	}
    }

    // TODO: some ints are also completely contained in some floating point formats (for example: int32 < float64)
    
    return table;
}

inline constexpr auto Type_compare_table = get_type_compare_table();

bool is_base_type_integer(Type_enum type) { return type >= T_s8 && type <= T_u64; }
bool is_base_type_floating_point(Type_enum type) { return type >= T_f8 && type <= T_f64; }

static Type_compare compare_base_types(Type_enum type_a, Type_enum type_b)
{
    if(type_a == type_b)
	return Type_compare::Equal;
    
    if((is_base_type_integer(type_a) && is_base_type_integer(type_b)) || (is_base_type_floating_point(type_a) && is_base_type_floating_point(type_b)))
	return Type_compare_table[TC_TABLE_IDX(type_a, type_b)];
    return Type_compare::Disjoint;
}

Type_compare compare_types(Ast_node *node_a, Ast_node *node_b)
{
    
}
