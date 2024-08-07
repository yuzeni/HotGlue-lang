#pragma once

#include <array>
#include <cstdint>

#include "ast.hpp"
#include "lexer.hpp"
#include "tdop_functions.hpp"

struct Semantic_code {
    int lbp = 0; // left-binding-power
    int rbp = 0; // right-binding-power
                 // zero means none are taken
                 // specifying lbp as well as rbp is not necessary, but solves left and right associativity problems,
                 // where lbp <= rbp guarantees left- and lbp > rbp right associativity.
    Ast_node* (*nud)(NUD_ARGS) = nud_error;
    Ast_node* (*led)(LED_ARGS) = led_error;
};

consteval std::array<Semantic_code, tkn_SIZE> get_tkn_semantics_table()
{
    std::array<Semantic_code, tkn_SIZE> table;
    
    table.fill({});

    /* literals */
    table[tkn_ident]       = {0, 0, nud_ident};
    table[tkn_int]         = {0, 0, nud_int};
    table[tkn_real]        = {0, 0, nud_real};
    table[tkn_string]      = {0, 0, nud_string};
    table[tkn_true]        = {0, 0, nud_true};
    table[tkn_false]       = {0, 0, nud_false};
    table[tkn_placeholder] = {0, 0, nud_placeholder};
    table[tkn_exit]        = {0, 0, nud_exit};

    /* base types */
    table[tkn_i8]          = {0, 0, nud_types};
    table[tkn_i16]         = {0, 0, nud_types};
    table[tkn_i32]         = {0, 0, nud_types};
    table[tkn_i64]         = {0, 0, nud_types};
    table[tkn_u8]          = {0, 0, nud_types};
    table[tkn_u16]         = {0, 0, nud_types};
    table[tkn_u32]         = {0, 0, nud_types};
    table[tkn_u64]         = {0, 0, nud_types};
    table[tkn_f8]          = {0, 0, nud_types};
    table[tkn_f16]         = {0, 0, nud_types};
    table[tkn_f32]         = {0, 0, nud_types};
    table[tkn_f64]         = {0, 0, nud_types};
    table[tkn_str]         = {0, 0, nud_types};
    table[tkn_bool]        = {0, 0, nud_types};
    table[tkn_ident_type]  = {0, 0, nud_types};
    table[tkn_symbol]      = {0, 0, nud_types};
    table[tkn_this]        = {0, 0, nud_this}; // nud_type
    table[tkn_all]         = {0, 0, nud_all};

    /* helper functions */
    table[tkn_size]        = {0, 1, nud_size};

    /* structure */
    table[tkn_do]          = {1, 1, nud_do};
    table[tkn_expand]      = {2, 2, nud_expand};
    table[tkn_trigger]     = {2, 2, nud_trigger};
    table[tkn_using]       = {0, 2, nud_right};
    table[':']             = {3, 3, nud_error, led_declare};
    table[tkn_return]      = {4, 4, nud_error, led_return};
    table[tkn_req]         = {4, 4, nud_error, led_normal};
    table[tkn_to]          = {5, 5, nud_error, led_normal};

    /* set operations */
    table['=']             = {6, 6, nud_error, led_normal};//led_set_eq};
    table[tkn_update_add]  = {6, 6, nud_error, led_normal};//led_update_add};
    table[tkn_update_sub]  = {6, 6, nud_error, led_normal};//led_update_sub};
    table[tkn_update_mul]  = {6, 6, nud_error, led_normal};//led_update_mul};
    table[tkn_update_div]  = {6, 6, nud_error, led_normal};//led_update_div};
    table[tkn_increment]   = {6, 0, nud_error, led_left};//nud_left};//nud_increment};
    table[tkn_decrement]   = {6, 0, nud_error, led_left};//nud_left};//nud_decrement};
    
    table[tkn_or]          = {7, 7, nud_error, led_normal};//led_or};
    table[tkn_and]         = {8, 8, nud_error, led_normal};//led_and};
    table[tkn_eq]          = {9, 9, nud_error, led_normal};//led_eq};
    table[tkn_neq]         = {9, 9, nud_error, led_normal};//led_neq};
    table['<']             = {9, 9, nud_error, led_normal};//led_less};
    table['>']             = {9, 9, nud_error, led_normal};//led_greater};
    table[tkn_less_eq]     = {9, 9, nud_error, led_normal};//led_less_eq};
    table[tkn_greater_eq]  = {9, 9, nud_error, led_normal};//led_greater_eq};
    table['+']             = {10, 10, nud_right, led_normal};//led_add};
    table['-']             = {10, 10, nud_right, led_normal};//led_sub};
    table['*']             = {11, 11, nud_error, led_normal};//led_mul};
    table['/']             = {11, 11, nud_error, led_normal};//led_div};
    table['%']             = {11, 11, nud_error, led_normal};//led_modulo};
    table[tkn_pow]         = {13, 12, nud_error, led_normal};//led_pow};
    table['!']             = {0, 14, nud_right};//nud_not};

    /* set creation operations */
    table[tkn_first]       = {0, 14, nud_set_op};
    table[tkn_last]        = {0, 14, nud_right};
    table[tkn_from]        = {15, 14, nud_error, led_normal};
    table[tkn_where]       = {15, 14, nud_error, led_normal};
    
    /* grouping */
    table['(']             = {15, 0, nud_parenthesis, led_parenthesis};
    table['[']             = {15, 0, nud_bracket, led_bracket};
    table['{']             = {15, 0, nud_brace, led_brace};
    table[')']             = {0, 0, nud_delimiter};
    table[']']             = {0, 0, nud_delimiter};
    table['}']             = {0, 0, nud_delimiter};
    table[',']             = {0, 0, nud_delimiter};

    /* very tight set creation operations */
    table['.']             = {16, 16, nud_error, led_dot};
    table['\\']            = {16, 16, nud_error, led_normal};

    /* signifiers */
    table[tkn_is_type]        = {0, 17, nud_right};
    table[tkn_include]     = {0, 17, nud_right};
    table['|']             = {0, 17, nud_right};//nud_ident_flag};
    table['$']             = {0, 17, nud_right};
    table[tkn_extern]      = {0, 17, nud_right};//nud_ident_flag};
    table[tkn_exread]      = {0, 17, nud_right};//nud_ident_flag};
    table[tkn_exwrite]     = {0, 17, nud_right};//nud_ident_flag};
    table[tkn_exlayout]    = {0, 17, nud_right};//nud_ident_flag};
    table[tkn_AoS]         = {0, 18, nud_right};//nud_ident_flag}; // require exlaout be the super
    table[tkn_SoA]         = {0, 18, nud_right};//nud_ident_flag}; // require exlaout be the super
    
    return table;
}

inline constexpr auto tkn_semantics_table = get_tkn_semantics_table();

inline bool node_legal_in_global_space(Ast_node* node) { return check_type_flag(node, TF_Declaration) || node->tkn.type == tkn_do; }

// parser semantics

constexpr bool is_declare_signifier_tkn(Token_enum tkn) { return tkn == '|' || tkn == '$' || (tkn >= tkn_is_type && tkn <= tkn_SoA); }

// type semantics

constexpr bool is_base_type(Type_enum type) { return type >= T_i8 && type <= T_placeholder; }
constexpr bool is_base_type_integer(Type_enum type) { return type >= T_i8 && type <= T_u64; }
constexpr bool is_base_type_floating_point(Type_enum type) { return type >= T_f8 && type <= T_f64; }
constexpr bool is_value_type(Ast_node *node)
{
    return (!check_type_flag(node, TF_Declaration) && (is_base_type(node->type_result) && node->type_result != T_placeholder)) || check_type_flag(node, TF_Reference);
}

enum Type_compare : uint16_t {
    TC_Nothing      = 0,
    TC_Equal        = 1,
    TC_Intersecting = 1 << 1,
    TC_Disjoint     = 1 << 2,
    TC_B_subset_A   = 1 << 3,
    TC_A_subset_B   = 1 << 4,
    TC_SIZE         = 1 << 5,
};

inline const char *type_compare_name_table[] {
    "Nothing",
    "Equal",
    "Intersecting",
    "Disjoint",
    "B_subset_A",
    "A_subset_B",
};

inline const char* get_type_compare_name(Type_compare tc) {
    return type_compare_name_table[log2i(uint16_t(tc)) + 1];
}

class Parser;
Type_compare compare_types(Ast_node* node_a, Ast_node* node_b, Parser& parser, Type_compare expected_result);
