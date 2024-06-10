#pragma once

#include <cstdint>
#include <array>
#include <initializer_list>

#include "lexer.hpp"
#include "ast.hpp"

static const int max_operator_args = 3;

enum Type_enum : uint64_t {
    T_None   = 0,
    T_Set    = 1,
    T_Array  = 1 << 1,
    // list all base types!! And possible composite structures (array, etc)
    
};

#define TYPE_ANY     Type_enum(0xffffffffffffffff)
#define TYPE_NUMERIC Type_enum(T_s8 | T_s16 | T_s32) // and so on

enum Result_type : int {
    RT_Independent = -1,
    RT_First = 0,
    RT_Second = 1,
    RT_Third = 2
    // and so on
};

struct Type_rule {
    
    Type_rule() {}
    Type_rule(std::initializer_list<Type_enum> args_list, Result_type result_type, Type_enum independent_result_type = T_None)
	: result_type(result_type), independent_result_type(independent_result_type)
    {
	for(size_t i = 0; i < args_list.size(); ++i)
	    args[i] = T_None;
    }
    
    Type_enum args[max_operator_args] = {};
    Result_type result_type = RT_Independent;
    Type_enum independent_result_type = T_None;
};

consteval std::array<Type_rule, Token_enum::tkn_SIZE> get_type_rule_array() {
    std::array<Type_rule, Token_enum::tkn_SIZE> table;
    table.fill({});
    
    table['=']            = {{TYPE_ANY}, RT_First};
    table[tkn_update_add] = {{TYPE_ANY}, RT_First};
    table[tkn_update_sub] =
    table[tkn_update_mul] =
    table[tkn_update_div] =
    table[tkn_increment]  =
    table[tkn_decrement]  =

    table[tkn_or]         =
    table[tkn_and]        =
    table[tkn_eq]         =
    table[tkn_neq]        =
    table['<']            =
    table['>']            =
    table[tkn_less_eq]    =
    table[tkn_greater_eq] =
    table['+']            =
    table['-']            =
    table['*']            =
    table['/']            =
    table['%']            =
    table[tkn_pow]        =
    table['!']            =
	
    return table;
}

#undef TYPE_ANY

static const std::array<Type_rule, Token_enum::tkn_SIZE> type_rule_table = get_type_rule_array();

Type_enum eval_type_of_expression(Ast_node* node);

bool is_any_type(Ast_node *node);

