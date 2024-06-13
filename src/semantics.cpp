#include "semantics.hpp"

#include "ast.hpp"
#include "lexer.hpp"

bool tkn_legal_in_global_space(Token_enum type)
{
    return type == ':' || type == tkn_do;
}

bool is_object_type(Type_enum t)
{
    return t == T_Unnamed_Object || t == T_Type_Object || t == T_Function_Object || t == T_Data_Object;
}

Type_enum eval_type_of_expression(Ast_node* node);
bool is_any_type(Ast_node *node);
