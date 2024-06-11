#pragma once

#include <cstdint>

#include "ast.hpp"

static const int max_operator_args = 3;

enum Type_enum : uint64_t {
    T_None       = 0,
    T_Set        = 1,
    T_Array      = 1 << 1,
    T_s8         = 1 << 2,
    T_s16        = 1 << 3,
    T_s32        = 1 << 4,
    T_s64        = 1 << 5,
    T_u8         = 1 << 6,
    T_u16        = 1 << 7,
    T_u32        = 1 << 8,
    T_u64        = 1 << 9,
    T_f8         = 1 << 10,
    T_f16        = 1 << 11,
    T_f32        = 1 << 12,
    T_f64        = 1 << 13,
    T_str        = 1 << 14, 
    T_bool       = 1 << 15, 
    T_ident_type = 1 << 16,
    T_symbol     = 1 << 17,
    T_this       = 1 << 18,
    T_all        = 1 << 19
    // list all base types!! And possible composite structures (array, etc)
    
};

Type_enum eval_type_of_expression(Ast_node* node);

bool is_any_type(Ast_node *node);

