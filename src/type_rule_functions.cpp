#include "type_rule_functions.hpp"

#include "lexer.hpp"
#include "ast.hpp"
#include "semantics.hpp"
#include "log_and_debug.hpp"

Type_enum tval_none(TYPE_EVAL_ARGS)
{
    return T_None;
}

Type_enum tval_this_type(TYPE_EVAL_ARGS)
{
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
    HG_DEB_error("tval_this_type called with wrong tkn type");
    return T_None;
}

Type_enum tval_obect(TYPE_EVAL_ARGS)
{
    
}
