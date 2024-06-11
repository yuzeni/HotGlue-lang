#include "semantics.hpp"

#include "lexer.hpp"

static const int max_operator_args = 3;

bool is_base_type(Token_enum t)
{
    return t == tkn_s8 || t == tkn_s16 || t == tkn_s32 || t == tkn_s64 ||
	t == tkn_u8 || t == tkn_u16 || t == tkn_u32 || t == tkn_u64 ||
	t == tkn_f8 || t == tkn_f16 || t == tkn_f32 || t == tkn_f64 ||
	t == tkn_str || t == tkn_bool;
}

bool is_numeric_type(Token_enum t)
{
    return t == tkn_s8 || t == tkn_s16 || t == tkn_s32 || t == tkn_s64 ||
	t == tkn_u8 || t == tkn_u16 || t == tkn_u32 || t == tkn_u64 ||
	t == tkn_f8 || t == tkn_f16 || t == tkn_f32 || t == tkn_f64;
}

bool is_sint_type(Token_enum t)
{
    return t == tkn_s8 || t == tkn_s16 || t == tkn_s32 || t == tkn_s64;
}

bool tkn_legal_in_global_space(Token_enum type)
{
    return type == ':' || type == tkn_do;
}

Type_enum eval_type_of_expression(Ast_node* node);
bool is_any_type(Ast_node *node);
