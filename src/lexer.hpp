#pragma once

#include <cstdint>
#include <string_view>
#include <string>
#include <vector>

#include "log_and_debug.hpp"

static constexpr int n_future_tkns = 8;
static constexpr int n_past_tkns = 8;

enum Token_enum : uint32_t {
    tkn_eof = 256,
    tkn_none,
    tkn_global_scope,
    tkn_parse_error,
    tkn_ident,
    tkn_placeholder,

    // literals
    tkn_int,
    tkn_real,
    tkn_string,

    // keywords
    tkn_s8, tkn_s16, tkn_s32, tkn_s64,
    tkn_u8, tkn_u16, tkn_u32, tkn_u64,
    tkn_f8, tkn_f16, tkn_f32, tkn_f64,
    tkn_str,
    tkn_bool, tkn_true, tkn_false,
    tkn_extern, tkn_exread, tkn_exwrite, tkn_exlayout,
    tkn_AoS, tkn_SoA,
    tkn_all, tkn_first, tkn_last,
    tkn_where,
    tkn_not,
    tkn_req, tkn_else,
    tkn_from_decl,
    tkn_expa,
    tkn_trigger,
    tkn_using,
    tkn_do,
    
    // operators
    tkn_func,                 // :>
    tkn_from_imp, tkn_to_imp, // <- ->
    tkn_not_from, tkn_not_to, // !<- !->
    tkn_func_body,            // ::

    tkn_update_add, tkn_update_sub, tkn_update_mul, tkn_update_div, tkn_update_pow, tkn_update_mod,
    tkn_increment, tkn_decrement,
    tkn_pow,
    tkn_and, tkn_or,
    tkn_eq, tkn_neq,
    tkn_less_eq, tkn_greater_eq,
    
    
    tkn_SIZE
};

constexpr const char* get_mul_char_token_name(Token_enum tkn);
std::string get_token_name_str(Token_enum tkn);

// data is null-terminated even though size is stored as well
struct Source {
    Source() {}
    Source(const char* data, size_t size, std::string info) : data(data), size(size), info(info) {}
    Source(Source&& other) : data(other.data), size(other.size), info(other.info) { other.data = nullptr; }
    Source& operator=(Source&& other);
    Source(const Source& other) = delete;
    Source& operator=(const Source& other) = delete;
    ~Source() { delete[] data; }
    
    const char* data = nullptr;
    std::vector<uint32_t> line_begins;
    size_t size = 0;
    std::string info;
};

struct Source_location {
    const char* src_data;
    uint32_t line;
    uint32_t offset;
};

struct Input {
    Source_location get_src_location(const char* ptr) const; 
    const Source& get_src_ref(const char* ptr) const;
    void add_src(const char* data, size_t size, std::string info);
    std::vector<Source> srcs;

private:

    std::vector<const char*> srcs_idx_sorted;
};


enum class Parsing_flags : uint32_t {
  Currently_parsing = 1,
  Finished_parsing = 1 << 1,
};

struct Token {
    Token() {}
    Token(Token_enum type, char* ptr, void* data = nullptr);
    char* ptr = nullptr;
    uint32_t flag = 0;
    Token_enum type = tkn_none;
    union {
	std::string_view sv;
	int64_t  i = 0;
	double   d;
    };
};

enum Lexer_flags : uint64_t {
    LF_PRINT_TOKENS = 1
};

class Lexer {
public:
    Lexer(Lexer_flags flags = Lexer_flags(0)) : flags(flags) {}

    void load_input_from_file(const char* source);
    void load_input_from_string(const char* source);
    bool next_token(); // wrapper function
    Token& peek_next_token();
    bool not_eof() { return tkn_at(0).type != tkn_eof; }
    Token& tkn_at(int offset) { return tkns.tkn_at(offset); }
    void print_token(Token& tkn, bool show_content = false, bool keep_line_location = true) const;

    template<typename... Args>
    void parsing_error(Token& tkn, const char* msg, Args... args) {parsing_error(tkn.ptr, msg, args...); }
    
    template<typename... Args>
    void parsing_error(char* p, const char* msg, Args... args) {
	Source_location src_loca = input.get_src_location(p);
	auto line = get_line_with_error(p, input.get_src_ref(p));
	const Source& src = input.get_src_ref(p);
	hg_error(HG_err::parsing, cte_concat_c_str({"[%s:\033[91m%d\033[0m:\033[94m%d\033[0m]\n\033[93m", msg, "\033[0m\n\'%s\'\n %s"}).c_str(),
		 src.info.c_str(), src_loca.line, src_loca.offset, args..., line.first.c_str(), line.second.c_str());
	++parsing_error_cnt;
    }
    
    int parsing_error_cnt = 0;

private:

    Input input;
    char* p_begin = nullptr;
    char* p = nullptr;
    char* eof = nullptr;

    Lexer_flags flags;

    void after_load_init();
    bool push_next_token();
    std::pair<std::string, std::string> get_line_with_error(const char* p, const Source& source);

    enum class Task_result { exit, exit_result, no_exit };

    struct Token_window {
	bool push(Token tkn);
	bool step();
	bool pushstep(Token tkn);
	Token& tkn_at(int offset) { HG_DEB_assert(uint16_t(idx_last + offset) < size, "tkn_at out of range"); return tkns[n_past_tkns - 1 + offset]; }
	Token& back() { return tkns[idx_last]; }
	int n_preparsed() const { return idx_last - n_past_tkns + 1; };
	int idx_last = n_past_tkns - 1;
	
    private:
	const int size = n_future_tkns + n_past_tkns;
	Token tkns[n_future_tkns + n_past_tkns] {Token{}};
    } tkns;
};

bool is_open_bracket(char c);
bool is_open_bracket(char c);
bool is_close_bracket(char c);
bool is_delim_tkn_left(Token_enum type);
bool is_delim_tkn_right(Token_enum type);
bool is_binary_set_op(Token_enum type);
