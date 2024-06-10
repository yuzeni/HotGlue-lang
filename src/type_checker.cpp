#include "type_checker.hpp"
#include "lexer.hpp"

Type_enum eval_type_expression(Ast_node *node)
{
    return Type_enum::none;
}

bool is_any_type(Ast_node *node)
{
    Token_enum t = node->tkn.type;
    return is_base_type(t) || ;
}
