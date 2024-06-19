#pragma once

#include <cstdint>
#include <unordered_map>
#include <array>

#include "lexer.hpp"
#include "utils.hpp"

enum Type_enum : uint16_t {
    T_None = 0,
    T_All, // a special unnamed object
    T_Function_Object,
    T_Data_Object,
    T_Decl_ref,
    T_Array, // single or multidimensional
    
    T_i8,
    T_i16,
    T_i32,
    T_i64,
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
    T_placeholder,

    T_SIZE
    // list all base types!! And possible composite structures (array, etc)
    
};

inline const char *type_enum_name_table[T_SIZE] {
    "None",
    "All",
    "Function-Object",
    "Data-Object",
    "Declaration-Reference",
    "Array",
    "i8",
    "i16",
    "i32",
    "i64",
    "u8",
    "u16",
    "u32",
    "u64",
    "f8",
    "f16",
    "f32",
    "f64",
    "str",
    "bool",
    "ident_type",
    "symbol",
    "placeholder"
};

struct Min_Max {
    int64_t min;
    uint64_t max;
};

consteval std::array<Min_Max, T_SIZE> get_type_num_limits_table()
{
    std::array<Min_Max, T_SIZE> table;
    table.fill({0,0});
    table[T_i8]  = {INT8_MIN, INT8_MAX};
    table[T_i16] = {INT16_MIN, INT16_MAX};
    table[T_i32] = {INT32_MIN, INT32_MAX};
    table[T_i64] = {INT64_MIN, INT64_MAX};
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
    TF_None            = 0,
    TF_Depends_on_all  = 1,
    TF_Complete_const  = 1 << 1,
    TF_Overdefined     = 1 << 2,
    TF_Defined         = 1 << 3,
    TF_Underdefined    = 1 << 4,
    TF_Extern          = 1 << 5,
    TF_Exread          = 1 << 6,
    TF_Exwrite         = 1 << 7,
    TF_AoS             = 1 << 8,
    TF_SoA             = 1 << 9,
    TF_Pure_type       = 1 << 10,
    TF_Has_placeholder = 1 << 11,
    TF_Declaration     = 1 << 12
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
};

// call this to delete an Ast_node (UNTESTED)
Ast_node *node_delete(Ast_node *node);
// NOTE: creates a new copy of an existing node. (UNTESETED)
class Parser;
Ast_node *copy_expression_in_new_scope(Ast_node *node, Parser &parser);

inline void add_type_flag(Ast_node *node, Type_flags type_flag) {
    node->type_flags = Type_flags(node->type_flags | type_flag);
}

enum Print_ast_enum : uint64_t {
    PN_Content     = 1,
    PN_Super       = 1 << 1,
    PN_Type_result = 1 << 2,
    PN_Type_flags  = 1 << 4,
    PN_Ident_idx   = 1 << 4,
};


// NOTE: before populating the identifier table, we need to check for hash
// collisions between the identifiers, by comparing the whole string, when the
// hash is equivalent. If there are collisions, then mangle the new colliding
// identifier (give it a new unique name).
// This will be repeated for each new identifier.
// Also include keywords in the collision checking!

typedef uint64_t Hash;

struct Ast {

    Ast() : global_scope(Token{tkn_global_scope, nullptr})
    {
	global_scope.id = utils::default_str_hash_value;
    }

    Hash find_ident_in_scope(Ast_node *ident_node, Ast_node* scope_super);
    Hash find_ident(Ast_node* node, Ast_node* scope_super);
    Hash add_ident(Ast_node* ident_node, Ast_node* scope_super);

    // TODO: check if this is actually performance sensitive
    // NOTE: performance sensitive
    Ast_node *find_object_with_id(Hash id)
    {
	auto it = identifier_set.find(id);
	if(it == identifier_set.end())
	    return nullptr;
	return it->second;
    }

    void print(Print_ast_enum config = Print_ast_enum(PN_Content | PN_Type_flags | PN_Type_result | PN_Ident_idx)) const; // | PN_Super
    void print_node(const Ast_node* node, Print_ast_enum config, int depth = 0) const;

    // the root node
    Ast_node global_scope;

    // set of all identifiers. The hash is precomputed to ensure no hash collisions.
    // this is a form of 'string interning': https://en.wikipedia.org/wiki/String_interning
    std::unordered_map<Hash, Ast_node*> identifier_set;
};
