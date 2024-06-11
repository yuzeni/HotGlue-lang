#pragma once

#include <unordered_map>

#include "lexer.hpp"

enum Type_enum : uint16_t {
    T_None = 0,
    T_All, // a special unnamed object
    T_Unnamed_Object,
    T_Object_Type,
    T_Function_Object,
    T_Data_Object,
    T_Array,
    
    T_s8,
    T_s16,
    T_s32,
    T_s64,
    T_u8,
    T_u16,
    T_u32,
    T_u64,
    T_f8,
    T_f16,
    T_f32,
    T_f64,
    T_str,
    T_bool,
    T_ident_type,
    T_symbol,
    T_this,
    T_placeholder
    // list all base types!! And possible composite structures (array, etc)
    
};

struct Ast_node {
    Ast_node() : type_result(T_None), tkn{} {}
    Ast_node(Token tkn, Ast_node* super = nullptr, Type_enum type_result = T_None)
	: type_result(type_result), super(super), tkn(tkn) {}

    Type_enum type_result;
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
