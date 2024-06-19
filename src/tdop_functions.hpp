#pragma once

#include "ast.hpp"

class Lexer;
class Parser;

// TODO: perhaps remove left from nud
#define NUD_ARGS                                                               \
  [[maybe_unused]] Token_enum tkn_type, [[maybe_unused]] Lexer &lexer,         \
      [[maybe_unused]] Parser &parser, [[maybe_unused]] Ast_node *left,        \
      [[maybe_unused]] Ast_node *super

#define LED_ARGS                                                               \
  [[maybe_unused]] Token_enum tkn_type, [[maybe_unused]] Lexer &lexer,         \
      [[maybe_unused]] Parser &parser, [[maybe_unused]] Ast_node *left,        \
      [[maybe_unused]] Ast_node *super

#define PASS_NUD_ARGS tkn_type, lexer, parser, left, super
#define PASS_LED_ARGS tkn_type, lexer, parser, left, super

/* nuds "null denotation" */
// they do not take the left argument

Ast_node *nud_ident(NUD_ARGS);
Ast_node *nud_int(NUD_ARGS);
Ast_node *nud_real(NUD_ARGS);
Ast_node *nud_string(NUD_ARGS);
Ast_node *nud_true(NUD_ARGS);
Ast_node *nud_false(NUD_ARGS);
Ast_node *nud_placeholder(NUD_ARGS);
Ast_node *nud_exit(NUD_ARGS);
Ast_node *nud_types(NUD_ARGS);
Ast_node *nud_all(NUD_ARGS);

Ast_node *nud_size(NUD_ARGS);

Ast_node *nud_do(NUD_ARGS);
Ast_node *nud_expand(NUD_ARGS);
Ast_node *nud_trigger(NUD_ARGS);

Ast_node *nud_left(NUD_ARGS);
Ast_node *nud_right(NUD_ARGS);
Ast_node *nud_arg(NUD_ARGS);
Ast_node *nud_this(NUD_ARGS);
Ast_node *nud_bracket(NUD_ARGS);
Ast_node *nud_set_op(NUD_ARGS); // used by: all, first
Ast_node *nud_error(NUD_ARGS);
Ast_node *nud_delimiter(NUD_ARGS);

Ast_node *nud_not(NUD_ARGS);
Ast_node *nud_increment(NUD_ARGS);
Ast_node *nud_decrement(NUD_ARGS);
Ast_node *nud_ident_flag(NUD_ARGS);

/* leds "left denotation" */

Ast_node *led_left(LED_ARGS);

Ast_node *led_normal(LED_ARGS);
Ast_node *led_parenthesis(LED_ARGS);
Ast_node *led_bracket(LED_ARGS);
Ast_node *led_brace(LED_ARGS);
Ast_node *led_declare(LED_ARGS);
Ast_node *led_dot(LED_ARGS);
Ast_node *led_error(LED_ARGS);

Ast_node *led_to_imp(LED_ARGS);
Ast_node *led_not_to (LED_ARGS);
Ast_node *led_func_body(LED_ARGS);
Ast_node *led_set_eq(LED_ARGS);
Ast_node *led_update_add(LED_ARGS);
Ast_node *led_update_sub(LED_ARGS);
Ast_node *led_update_mul(LED_ARGS);
Ast_node *led_update_div(LED_ARGS);

Ast_node *led_or(LED_ARGS);
Ast_node *led_and(LED_ARGS);
Ast_node *led_eq(LED_ARGS);
Ast_node *led_neq(LED_ARGS);
Ast_node *led_less(LED_ARGS);
Ast_node *led_greater(LED_ARGS);
Ast_node *led_less_eq(LED_ARGS);
Ast_node *led_greater_eq(LED_ARGS);
Ast_node *led_add(LED_ARGS);
Ast_node *led_sub(LED_ARGS);
Ast_node *led_mul(LED_ARGS);
Ast_node *led_div(LED_ARGS);
Ast_node *led_modulo(LED_ARGS);
Ast_node *led_pow(LED_ARGS);

// #undef NUD_ARGS
// #undef LED_ARGS
