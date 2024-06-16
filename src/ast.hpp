#pragma once

#include <cstdint>
#include <unordered_map>
#include <array>

#include "lexer.hpp"

enum Type_enum : uint16_t {
    T_None = 0,
    T_All, // a special unnamed object
    T_Unnamed_Object,
    T_Type_Object,
    T_Function_Object,
    T_Data_Object,
    T_Declared_Object,
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
    T_placeholder,

    T_SIZE
    // list all base types!! And possible composite structures (array, etc)
    
};

struct Min_Max {
    int64_t min;
    uint64_t max;
};

consteval std::array<Min_Max, T_SIZE> get_type_num_limits_table()
{
    std::array<Min_Max, T_SIZE> table;
    table.fill({0,0});
    table[T_s8]  = {INT8_MIN, INT8_MAX};
    table[T_s16] = {INT16_MIN, INT16_MAX};
    table[T_s32] = {INT32_MIN, INT32_MAX};
    table[T_s64] = {INT64_MIN, INT64_MAX};
    table[T_u8]  = {0, UINT8_MAX};
    table[T_u16] = {0, UINT16_MAX};
    table[T_u32] = {0, UINT32_MAX};
    table[T_u64] = {0, UINT64_MAX};
    // for floating point formants, there largest representable integer is stored.
    table[T_f8]  = {-16, 16}; // 2^4
    table[T_f16] = {-2048, 2048}; // 2^11
    table[T_f32] = {-16777216, 16777216}; // 2^24
    table[T_f64] = {-9007199254740992, 9007199254740992}; // 2^53
    return table;
}		 

inline constexpr auto Type_num_limits_table = get_type_num_limits_table();

enum Type_flags : uint32_t {
    TF_None           = 0,
    TF_Depends_on_all = 1,
    TF_Complete_const = 1 << 1,
    TF_Overdefined    = 1 << 2,
    TF_Defined        = 1 << 3,
    TF_Underdefined   = 1 << 4,
};

static const char *type_enum_name_table[T_SIZE] {
    "None"
    "All"
    "Unnamed_Object"
    "Type_Object"
    "Function_Object"
    "Data_Object"
    "Array"
    "s8"
    "s16"
    "s32"
    "s64"
    "u8"
    "u16"
    "u32"
    "u64"
    "f8"
    "f16"
    "f32"
    "f64"
    "str"
    "bool"
    "ident_type"
    "symbol"
    "this"
    "placeholder"
};

struct Ast_node {
    Ast_node() : type_result(T_None), type_flags(TF_None), tkn{} {}
    Ast_node(Token tkn, Ast_node* super = nullptr, Type_enum type_result = T_None, Type_flags type_flags = TF_None)
	: type_result(type_result), type_flags(type_flags), super(super), tkn(tkn) {}

    Type_enum type_result;
    Type_flags type_flags;
    uint64_t id = 0; // unique id for identifiers.
    Ast_node* super = nullptr;
    Ast_node* alt_sub = nullptr; // next sub with the same super
    Ast_node* sub = nullptr;
    Token tkn;

    uint64_t count_subs(Ast_node* node) const;
};

// don't call this directly (UNTESTED)
void super_delete(Ast_node *node);

// call this to delete an Ast_node (UNTESTED)
Ast_node *node_delete(Ast_node *node);

struct Identifier_info {
    Identifier_info()
	: ident(nullptr) {}
    Identifier_info(Ast_node* ident)
	: ident(ident) {}
    // points to the identifier. The alt_sub will be the actual declaration, if there is one.
    Ast_node* ident;
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

typedef uint64_t Hash;

struct Ast {

    Hash find_ident_in_scope(Ast_node *ident_node, Ast_node* scope_super);
    Hash find_ident(Ast_node* node, Ast_node* scope_super);
    Hash add_ident(Ast_node* ident_node, Ast_node* scope_super);

    void print(Print_ast_enum config = Print_ast_enum(PN_SHOW_CONTENT)) const; // | PN_SUPER
    void print_node(const Ast_node* node, Print_ast_enum config, int depth = 0) const;

    // the root node
    Ast_node global_scope{Token{tkn_global_scope, nullptr}};

    // set of all identifiers. The hash is precomputed to ensure no hash collisions.
    // this is a form of 'string interning': https://en.wikipedia.org/wiki/String_interning
    std::unordered_map<Hash, Ast_node*> identifier_set;
};
