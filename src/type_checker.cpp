#include "type_checker.hpp"

#include <initializer_list>
#include <array>

#include "lexer.hpp"

#define TYPE_ANY     Type_enum(0xffffffffffffffff)
#define TYPE_NUMERIC Type_enum(T_s8 | T_s16 | T_s32) // and so on

enum Result_type : int {
    RT_Independent = -2,
    RT_First_arg = 0,
    RT_Second_arg = 1,
    RT_Third_arg = 2
    // and so on
};

struct Type_rule {
    
    Type_enum args[max_operator_args] = {};
    Result_type result_type = RT_Independent;
    Type_enum independent_result_type = T_None;
};



consteval std::array<Type_rule, Token_enum::tkn_SIZE> get_type_rule_array() {
    
    std::array<Type_rule, Token_enum::tkn_SIZE> table;
    table.fill({});
    
    table['=']            = {{TYPE_ANY}, RT_First_arg};
    table[tkn_update_add] = {{TYPE_ANY}, RT_First_arg};
    // table[tkn_update_sub] = 
    // table[tkn_update_mul] =
    // table[tkn_update_div] =
    // table[tkn_increment]  =
    // table[tkn_decrement]  =

    // table[tkn_or]         =
    // table[tkn_and]        =
    // table[tkn_eq]         =
    // table[tkn_neq]        =
    // table['<']            =
    // table['>']            =
    // table[tkn_less_eq]    =
    // table[tkn_greater_eq] =
    // table['+']            =
    // table['-']            =
    // table['*']            =
    // table['/']            =
    // table['%']            =
    // table[tkn_pow]        =
    // table['!']            =
	
    return table;
}

static const std::array<Type_rule, Token_enum::tkn_SIZE> type_rule_table = get_type_rule_array();

Type_enum eval_type_expression(Ast_node *node, int depth = 0)
{
    // handle base types
    switch(node->tkn.type) {
    case tkn_s8:         return T_s8;
    case tkn_s16:        return T_s16;
    case tkn_s32:        return T_s32;
    case tkn_s64:        return T_s64;
    case tkn_u8:         return T_u8;
    case tkn_u16:        return T_u16;
    case tkn_u32:        return T_u32;
    case tkn_u64:        return T_u64;
    case tkn_f8:         return T_f8;
    case tkn_f16:        return T_f16;
    case tkn_f32:        return T_f32;
    case tkn_f64:        return T_f64;
    case tkn_str:        return T_str;
    case tkn_bool:       return T_bool;
    case tkn_ident_type: return T_ident_type;
    case tkn_symbol:     return T_symbol;
    case tkn_this:       return T_this;
    case tkn_all:        return T_all;
    }

    if(node->tkn.type == tkn_ident) {
	// 1) figure out, if it is a new ident, or a defined one!
	// 2) if new, return TYPE_ANY else return type stored in the identifier lookup
    }

    // if(is_op_tkn(node->tkn.type)) {
    // 	Type_rule type_rule = type_rule_table[node->tkn.type];
	
    // }
    

    // return Type_enum::none;
}

bool is_any_type(Ast_node *node)
{
    Token_enum t = node->tkn.type;
    // return is_base_type(t) || ;
    return true;
}
