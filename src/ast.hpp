#pragma once

#include <iostream>
#include <string_view>
#include <unordered_map>

#include "lexer.hpp"
#include "utils.hpp"

enum Node_type : uint16_t {
    nt_none,

    nt_type,
    nt_array_type, // a type with []
    nt_object,
    nt_attribute, // like: req, not_from, to, ...
    nt_builtin_func,

    nt_func,
    nt_func_in,
    nt_func_out,
    nt_func_body,
    nt_func_call,
    nt_procedure,

    nt_arithm_op,
    nt_arithm_expr,

    nt_object_op,
    nt_object_expr,

    nt_SIZE
};


struct Ast_node {
    Ast_node() : type(nt_none), tkn{} {}
    Ast_node(Token tkn, Ast_node* super = nullptr, Node_type type = nt_none)
	: type(type), super(super), tkn(tkn) {}

    Node_type type;
    uint64_t id = 0; // unique id for identifiers.
    Ast_node* super = nullptr;
    Ast_node* alt_sub = nullptr; // next sub with the same super
    Ast_node* sub = nullptr;
    Token tkn;
};

enum Type_flags : uint16_t {
    TF_none      = 0,
    TF_base_type = 1,      // It is a base type (can additionally be an array)
    TF_array     = 1 << 1, // Doesn't inform about the dimensionality of the array, just that it is one.
    TF_ident     = 1 << 2, // Declaration with previously defined type
    TF_set       = 1 << 3, // Declaration with a set of types
};

struct Identifier_info {
    Identifier_info()
	: ident(nullptr), flags(TF_none) {}
    Identifier_info(Ast_node* ident, Type_flags flags = TF_none)
	: ident(ident), flags(flags) {}
    // points to the identifier. The alt_sub will be the actual declaration, if there is one.
    Ast_node* ident;
    Type_flags flags; // falgs might be useless !!!!!!!!
};

enum Print_ast_enum : uint64_t {
    PN_SHOW_CONTENT = 1,
    PN_SUPER        = 1 << 1,
};


// NOTE: before populating the identifier table, we need to check for hash
// collisions between the identifiers, by comparing the whole string, when the
// hash is equivalent. If there are collisions, then mangle the new colliding
// identifier (give it a new unique name).
// This will be repeated for each new identifier.
// Also include keywords in the collision checking!

struct Ast {

    // uint64_t find_or_add_ident(Ast_node* node, uint64_t scope_hash);
    uint64_t find_ident(Ast_node* node, uint64_t scope_hash);
    uint64_t add_ident(Ast_node* node, uint64_t scope_hash);

    void print(Print_ast_enum config = Print_ast_enum(PN_SHOW_CONTENT)) const; // | PN_SUPER
    void print_node(const Ast_node* node, Print_ast_enum config, int depth = 0) const;

    // the root node
    Ast_node global_scope{Token{tkn_global_scope, nullptr}};

    // set of all identifiers. The hash is precomputed to ensure no hash collisions.
    // this is a form of 'string interning': https://en.wikipedia.org/wiki/String_interning
    std::unordered_map<uint64_t, Identifier_info> identifier_set;
    
    // every scope has its own vector for it's varible hashes.
    // leaving a scope is as simple as popping a vector element of the outer vector.
    std::vector<std::vector<uint64_t>> identifier_scopes;
};
