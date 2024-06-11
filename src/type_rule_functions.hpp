#pragma once

#include "ast.hpp"

class Parser;

#define TYPE_EVAL_ARGS [[maybe_unused]] Ast_node* node, [[maybe_unused]] Parser &parser

Type_enum tval_none(TYPE_EVAL_ARGS);
Type_enum tval_this_type(TYPE_EVAL_ARGS);
Type_enum tval_object(TYPE_EVAL_ARGS);
Type_enum tval_return(TYPE_EVAL_ARGS);
Type_enum tval_req(TYPE_EVAL_ARGS);
