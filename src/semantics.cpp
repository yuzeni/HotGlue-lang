#include "semantics.hpp"
#include <cstdint>

#include "ast.hpp"
#include "log_and_debug.hpp"
#include "parser.hpp"

#define TC_BASE_TYPES_TABLE_IDX(a, b) (int(a) * T_SIZE + int(b))

consteval std::array<Type_compare, T_SIZE * T_SIZE> get_base_type_compare_table()
{
    std::array<Type_compare, T_SIZE * T_SIZE> table;
    table.fill(TC_Disjoint);

    // NOTE: employs properties of the specific supported integer types.
    for(uint16_t type_a = T_i8; type_a <= T_f64; ++type_a) {
	for(uint16_t type_b = T_i8; type_b <= T_f64; ++type_b) {
	    // all numeric types contain 0
	    table[TC_BASE_TYPES_TABLE_IDX(type_a, type_b)] = TC_Intersecting;
	    
	    if(type_a == type_b)
		table[TC_BASE_TYPES_TABLE_IDX(type_a, type_b)] = TC_Equal;
	    else if(Type_num_limits_table[type_a].max < Type_num_limits_table[type_b].max
		    && Type_num_limits_table[type_a].min >= Type_num_limits_table[type_b].min
		    && !(is_base_type_floating_point(Type_enum(type_a)) && is_base_type_integer(Type_enum(type_b))))
	    {
		table[TC_BASE_TYPES_TABLE_IDX(type_a, type_b)] = TC_A_subset_B;
	    }
	    else if(Type_num_limits_table[type_a].max > Type_num_limits_table[type_b].max
		    && Type_num_limits_table[type_a].min <= Type_num_limits_table[type_b].min
		    && !(is_base_type_floating_point(Type_enum(type_b)) && is_base_type_integer(Type_enum(type_a))))
	    {
		table[TC_BASE_TYPES_TABLE_IDX(type_a, type_b)] = TC_B_subset_A;
	    }
	}
    }

    for(uint16_t type_a = T_None; type_a <= T_placeholder; ++type_a) {
	table[TC_BASE_TYPES_TABLE_IDX(type_a, T_placeholder)] = TC_A_subset_B;
	table[TC_BASE_TYPES_TABLE_IDX(T_placeholder, type_a)] = TC_B_subset_A;
    }
    table[TC_BASE_TYPES_TABLE_IDX(T_placeholder, T_placeholder)] = TC_Equal;
    
    return table;
}

constexpr auto Base_type_compare_table = get_base_type_compare_table();

// TODO: could be a table
static Type_compare type_compare_sum(Type_compare a, Type_compare b)
{
    if (a == TC_Nothing)
	return b;
    if (b == TC_Nothing)
	return a;
    
    switch(a) {
    case TC_Equal:
	if (b == TC_Equal)
	    return TC_Equal;
	if (b == TC_A_subset_B)
	    return TC_A_subset_B;
	if (b == TC_B_subset_A)
	    return TC_B_subset_A;
	return TC_Intersecting;
    case TC_A_subset_B:
	if (b == TC_A_subset_B || b == TC_Equal)
	    return TC_A_subset_B;
	return TC_Intersecting;
    case TC_B_subset_A:
	if (b == TC_B_subset_A || b == TC_Equal)
	    return TC_B_subset_A;
	return TC_Intersecting;
    case TC_Intersecting:
	return TC_Intersecting;
    case TC_Disjoint:
	if (b == TC_Equal || b == TC_A_subset_B || b == TC_B_subset_A || b == TC_Intersecting)
	    return TC_Intersecting;
	return TC_Disjoint;
    }
}

static void type_compare_error(Type_compare result, Type_compare expected_result, Ast_node *node_a, Ast_node *node_b, Parser &parser)
{
    if(!(uint16_t(result) & uint16_t(expected_result))) {
	char expected_tcs[256];
	int expected_tcs_sz = 0;
	for(uint16_t i = 0; i < log2i(uint16_t(TC_SIZE)); ++i) {
	    if(uint16_t(expected_result) & (1 << i)) {
		const char* tc_name = get_type_compare_name(Type_compare(i));
		int new_sz = std::strlen(tc_name);
		
		for(int j = expected_tcs_sz; j < expected_tcs_sz + new_sz; ++j)
		    expected_tcs[j] = tc_name[j - expected_tcs_sz];
		
		if(expected_tcs_sz > 0) {
		    expected_tcs_sz += new_sz + 2;
		    expected_tcs[expected_tcs_sz] = ',';
		    expected_tcs[expected_tcs_sz + 1] = ' ';
		}
		else {
		    expected_tcs_sz += new_sz;
		}
	    }
	}
	expected_tcs[expected_tcs_sz - 1] = '\0';
	parser.type_error(node_a, node_b, "Expected type comparison(s) '%s', but got '%s'", expected_tcs, get_type_compare_name(result));
    }
}

static Type_compare compare_base_types(Ast_node* node_a, Ast_node* node_b, Parser& parser, Type_compare expected_result)
{
    HG_DEB_assert(is_base_type(node_a->type_result) && is_base_type(node_b->type_result), "must be base types");
    Type_compare tc;
    if(node_a->type_result == node_b->type_result)
	tc = TC_Equal;
    else
	tc = Base_type_compare_table[TC_BASE_TYPES_TABLE_IDX(node_a->type_result, node_b->type_result)];
    
    type_compare_error(tc, expected_result, node_a, node_b, parser);
    return tc;
}

static Ast_node *dereference_node(Ast_node* node, Parser& parser)
{
    if(node) {
	while (node->sub && !node->sub->alt_sub) {
	    node = node->sub;
	}
	if (check_type_flag(node, TF_Reference)) {
	    node = parser.ast.find_object_with_id(node->id);
	}
    }
    return node;
}

static Type_compare compare_data_objects(Ast_node *node_a, Ast_node *node_b, Parser& parser, Type_compare expected_result)
{
    node_a = dereference_node(node_a, parser);
    node_b = dereference_node(node_b, parser);

    node_a = node_a->sub ? node_a->sub : node_a;
    node_b = node_b->sub ? node_b->sub : node_b;

    Type_compare tc = TC_Nothing;
    
    while(node_a && node_b) {
	tc = type_compare_sum(compare_types(node_a, node_b, parser, expected_result), tc);
	node_a = dereference_node(node_a->alt_sub, parser);
	node_b = dereference_node(node_b->alt_sub, parser);
    }
    
    if(bool(node_a) ^ bool(node_b))
	tc = TC_Disjoint;

    type_compare_error(tc, expected_result, node_a, node_b, parser);
    return tc;
}

// only checks for placeholder right now (not sure if I need more)
static Type_compare compare_procedures(Ast_node *node_a, Ast_node *node_b, Parser& parser, Type_compare expected_result)
{
    HG_DEB_assert(node_a->sub && node_b->sub, "must have subs");

    Type_compare tc = TC_Nothing;
    
    if(dereference_node(node_a, parser)->type_result == T_placeholder) {
	if(dereference_node(node_b, parser)->type_result == T_placeholder)
	    tc = TC_Equal;
	else
	    tc = TC_B_subset_A;
    }
    else if(dereference_node(node_b, parser)->type_result == T_placeholder) {
	if(dereference_node(node_a, parser)->type_result == T_placeholder)
	    tc = TC_Equal;
	else
	    tc = TC_A_subset_B;
    }
    else {
	tc = TC_Disjoint;
    }

    type_compare_error(tc, expected_result, node_a, node_b, parser);
    return tc;
}

static int get_argument_position(Ast_node *arg_list, Ast_node *arg)
{
    int pos = 0;
    Ast_node* sub = arg_list->sub;
    while(sub) {
	if(arg->id == sub->id)
	    return pos;
	++pos;
	sub = sub->alt_sub;
    }
    return 0;
}


// checks for matching ordering of returned objects
// quadratic algorithm (not great)
static Type_compare compare_function_returns(Ast_node *return_a, Ast_node *return_b, Ast_node *arg_a, Ast_node *arg_b, Parser& parser, Type_compare expected_result)
{
    HG_DEB_assert(return_a->sub && return_b->sub && arg_a->sub && arg_b->sub, "must have subs");
    
    Ast_node* ret_sub_a = return_a->sub ? return_a->sub : return_a;
    Ast_node* ret_sub_b = return_b->sub ? return_b->sub : return_b;
    
    Type_compare tc = TC_Nothing;

    while(ret_sub_a && ret_sub_b) {
	if (get_argument_position(arg_a, ret_sub_a) != get_argument_position(arg_b, ret_sub_b)) {
	    tc = TC_Disjoint;
	    goto exit;
	}
	ret_sub_a = ret_sub_a->alt_sub;
	ret_sub_b = ret_sub_b->alt_sub;
    }
    
    if (bool(ret_sub_a) ^ bool(ret_sub_b)) {
	tc = TC_Disjoint;
	goto exit;
    }

    tc = compare_data_objects(return_a, return_b, parser, expected_result);
    goto exit;

exit:
    type_compare_error(tc, expected_result, return_a, return_b, parser);
    return tc;
}

static Type_compare compare_function_objects(Ast_node *node_a, Ast_node *node_b, Parser& parser, Type_compare expected_result)
{
    node_a = dereference_node(node_a, parser);
    node_b = dereference_node(node_b, parser);
    
    HG_DEB_assert(node_a->sub && node_b->sub, "must have subs");
    Type_compare arg_tc = compare_data_objects(node_a->sub, node_b->sub, parser, expected_result);
    HG_DEB_assert(node_a->sub->alt_sub && node_b->sub->alt_sub, "must have alt subs");
    Type_compare procedure_tc = compare_procedures(node_a->sub->alt_sub, node_b->sub->alt_sub, parser, expected_result);
    HG_DEB_assert(node_a->sub->alt_sub->alt_sub && node_b->sub->alt_sub->alt_sub, "must have alt alt subs");
    Type_compare return_tc = compare_function_returns(node_a->sub->alt_sub->alt_sub, node_b->sub->alt_sub->alt_sub, node_a->sub, node_b->sub, parser, expected_result);
    return type_compare_sum(type_compare_sum(arg_tc, procedure_tc), return_tc);
}

Type_compare compare_types(Ast_node *node_a, Ast_node *node_b, Parser& parser, Type_compare expected_result)
{
    node_a = dereference_node(node_a, parser);
    node_b = dereference_node(node_b, parser);
    
    HG_DEB_assert(node_a && node_b, "");
    HG_DEB_assert(!check_type_flag(node_a, TF_Reference) && !check_type_flag(node_b, TF_Reference), "");

    if((node_a->type_result == T_Data_Object)
	&& (node_b->type_result == T_Data_Object))
    {
	return compare_data_objects(node_a, node_b, parser, expected_result);
    }
    else if((node_a->type_result == T_Function_Object)
	    && (node_b->type_result == T_Function_Object))
    {
	return compare_function_objects(node_a, node_b, parser, expected_result);
    }
    else if(is_base_type(node_a->type_result) && is_base_type(node_b->type_result)) {
	return compare_base_types(node_a, node_b, parser, expected_result);
    }
    else {
	return TC_Disjoint;
    }
}
