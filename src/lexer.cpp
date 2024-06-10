#include "lexer.hpp"

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <string>
#include <algorithm>
#include <optional>
#include <string_view>
#include <vector>
#include <functional>

#include "log_and_debug.hpp"
#include "utils.hpp"

#ifdef TRACY_ENABLE
#  include "tracy/Tracy.hpp"
#endif

Source& Source::operator=(Source&& other)
{
    data = other.data;
    size = other.size;
    info = other.info;
    other.data = nullptr;
    return *this;
}

Source_location Input::get_src_location(const char* ptr) const
{
    uint32_t idx = 0xffffffff;
    if(!srcs_idx_sorted.empty() && ptr >= srcs_idx_sorted.back()) {
	idx = srcs_idx_sorted.size() - 1;
    }
    else {
	auto bound_src = std::upper_bound(srcs_idx_sorted.begin(), srcs_idx_sorted.end(), ptr, std::less());
	if(bound_src != srcs_idx_sorted.end() && bound_src != srcs_idx_sorted.begin())
	    idx = bound_src - 1 - srcs_idx_sorted.begin();
    }
    
    if(idx != 0xffffffff) {
	const char* src_data = srcs[idx].data;
	ptrdiff_t rel_ptr = ptr - src_data;
	uint32_t line_idx = 0xffffffff;
	if(!srcs[idx].line_begins.empty() && rel_ptr >= srcs[idx].line_begins.back()) {
	    line_idx = srcs[idx].line_begins.size() - 1;
	}
	else {
	    auto bound_line = std::upper_bound(srcs[idx].line_begins.begin(), srcs[idx].line_begins.end(), rel_ptr);
	    if(bound_line != srcs[idx].line_begins.end() && bound_line != srcs[idx].line_begins.begin())
		line_idx = bound_line - 1 - srcs[idx].line_begins.begin();
	}
	
	if(line_idx != 0xffffffff) {
	    uint32_t offset = ptr - (srcs[idx].line_begins[line_idx] + src_data);
	    return {src_data, line_idx + 1, offset + 1};
	}
    }
    else {
	HG_DEB_error("ptr doesn't belong to a source.");
    }
    
    return {nullptr, 0, 0};
}

const Source& Input::get_src_ref(const char* ptr) const
{
    auto bound_src = std::upper_bound(srcs_idx_sorted.begin(), srcs_idx_sorted.end(), ptr, std::less());
    return srcs[bound_src - 1 - srcs_idx_sorted.begin()];
}

void Input::add_src(const char* data, size_t size, std::string info)
{
    srcs.emplace_back(Source{data, size, info});
    srcs_idx_sorted.push_back(srcs.back().data);
    srcs.back().line_begins.push_back(0);
    std::sort(srcs_idx_sorted.begin(), srcs_idx_sorted.end(), std::less());
}

const char *token_name_table[tkn_SIZE - 256]{
    "eof",
    "none",
    "global_scope",
    "parse_error",
    
    /* literals */
    
    "identifier",
    "int",
    "real",
    "string",
    "true",
    "false",
    "placeholder",

    /* keywords */
    
    // base types
    "s8",
    "s16",
    "s32",
    "s64",
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
    "ident",
    "symbol",
    "this",
    "all",
    // external business
    "extern",
    "exr",
    "exw",
    "exlayout",
    "AoS",
    "SoA",
    "funC",
    "funCUDA",
    "fun_OCL",
    // set creation operations
    "from",
    "where",
    "first",
    "last",
    // miscellaneous
    "req",
    "expa",
    "trigger",
    "using",
    "do",
    "to",
    "include",

    /* mutliple char operators */
    "->",
    "+=",
    "-=",
    "*=",
    "/=",
    "**=",
    "%=",
    "++",
    "--",
    "**",
    "&&",
    "||",
    "==",
    "!=",
    "<=",
    ">=",
};

constexpr const char *get_mul_char_token_name(Token_enum tkn)
{
    HG_DEB_assert(tkn >= 256, "Token not in name table, get the char instead!");
    return token_name_table[tkn - 256];
}

std::string get_token_name_str(Token_enum tkn)
{
    if(tkn >= 256)
	return token_name_table[tkn - 256];
    return std::string{char(tkn)};
}

// little bit ugly (I would like to iterate over keywords), but fast
Token_enum keyword_compare(const std::string_view sv)
{
#ifdef TRACY_ALL
    ZoneScoped;
#endif

    // NOTE: There may be collisions for anything longer than 8.
    //       And currently all keywords are smaller than that.
    if (sv.size() > 8)
	return tkn_ident;

    switch(hash_string_view(sv)) {
    case cte_hash_c_str("s8"): return tkn_s8;
    case cte_hash_c_str("s16"): return tkn_s16;
    case cte_hash_c_str("s32"): return tkn_s32;
    case cte_hash_c_str("s64"): return tkn_s64;
    case cte_hash_c_str("u8"): return tkn_u8;
    case cte_hash_c_str("u16"): return tkn_u16;
    case cte_hash_c_str("u32"): return tkn_u32;
    case cte_hash_c_str("u64"): return tkn_u64;
    case cte_hash_c_str("f8"): return tkn_f8;
    case cte_hash_c_str("f16"): return tkn_f16;
    case cte_hash_c_str("f32"): return tkn_f32;
    case cte_hash_c_str("f64"): return tkn_f64;
    case cte_hash_c_str("str"): return tkn_str;
    case cte_hash_c_str("bool"): return tkn_bool;
    case cte_hash_c_str("ident"): return tkn_ident_type;
    case cte_hash_c_str("symbol"): return tkn_symbol;
    case cte_hash_c_str("this"): return tkn_this;
    case cte_hash_c_str("extern"): return tkn_extern;
    case cte_hash_c_str("exr"): return tkn_exread;
    case cte_hash_c_str("exw"): return tkn_exwrite;
    case cte_hash_c_str("exlayout"): return tkn_exlayout;
    case cte_hash_c_str("AoS"): return tkn_AoS;
    case cte_hash_c_str("SoA"): return tkn_SoA;
    case cte_hash_c_str("funC"): return tkn_funC;
    case cte_hash_c_str("funCUDA"): return tkn_funCUDA;
    case cte_hash_c_str("fun_OCL"): return tkn_fun_OCL;
    case cte_hash_c_str("all"): return tkn_all;
    case cte_hash_c_str("from"): return tkn_from;
    case cte_hash_c_str("where"): return tkn_where;
    case cte_hash_c_str("first"): return tkn_first;
    case cte_hash_c_str("last"): return tkn_last;
    case cte_hash_c_str("req"): return tkn_req;
    case cte_hash_c_str("expa"): return tkn_expa;
    case cte_hash_c_str("trigger"): return tkn_trigger;
    case cte_hash_c_str("using"): return tkn_using;
    case cte_hash_c_str("do"): return tkn_do;
    case cte_hash_c_str("to"): return tkn_to;
    case cte_hash_c_str("include"): return tkn_include;
    default: return tkn_ident;
    }
}

std::optional<Token> check_for_operator(char **p, char *eof, Token_enum type,
                                        const char *op)
{
    if((*p)+strlen(op) >= eof)
	return {};
    size_t i;
    for(i = 0; i < strlen(op); ++i)
	if(op[i] != (*p)[i])
	    return {};
    (*p) += strlen(op);
    return Token{type, *p - i};
}

bool is_whitespace(char c)
{
    return c == ' ' || c == '\n' || c == '\t' || c == '\f' || c == '\r' || c == '\v';
}

bool is_new_line(char c)
{
    return c == '\n' || c == '\f' || c == '\r';
}

bool is_alpha(char c)
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

bool is_simple_char(char c)
{
    return (c >= '!' && c <= '/') || (c >= ':' && c <= '@') || (c >= '[' && c <= '`') || (c >= '{' && c <= '~');
}

bool is_open_bracket(char c)
{
    return c == '(' || c == '[' || c == '{';
}

bool is_close_bracket(char c)
{
    return c == ')' || c == ']' || c == '}';
}

bool is_delim_tkn_left(Token_enum type)
{
    if(type >= 256)
	return false;
    char c = (char)type;
    return is_open_bracket(c) || c == ',';
}

bool is_delim_tkn_right(Token_enum type)
{
    if(type >= 256)
	return false;
    char c = (char)type;
    return is_close_bracket(c) || c == ',';
}

bool is_binary_set_op(Token_enum type)
{
  return type == '.' || type == '\\';
}

bool is_reference_tkn(Token_enum type)
{
    return type == tkn_ident || type == tkn_placeholder;
}

Token::Token(Token_enum type, char *ptr, void *data)
    : ptr(ptr), type(type), i(0)
{
    bool d_a = false;
    if (data) {
	switch (type) {
	case tkn_ident:
	case tkn_string: d_a = true; sv = *reinterpret_cast<std::string_view*>(data); break;
	case tkn_int:    d_a = true; i = *reinterpret_cast<int64_t*>(data); break;
	case tkn_real:   d_a = true; d = *reinterpret_cast<double*>(data);  break;
	default: hg_error(HG_err::parsing, "The token type does not support data.");
	}
    }
    
    if (d_a)
	HG_DEB_assert(data, "The data ptr was empty");
}

void Lexer::load_input_from_string(const char *data)
{
    static int from_string_cnt = 0;
    from_string_cnt++;
    input.add_src(data, std::strlen(data), "raw input " + std::to_string(from_string_cnt));

    after_load_init();
}

void Lexer::load_input_from_file(const char *file_path)
{
    std::pair<char*, size_t> file = parse_file_cstr(file_path);
    input.add_src(file.first, file.second, file_path);
    
    after_load_init();
}

void Lexer::after_load_init()
{
    p = (char*)input.srcs.back().data;
    p_begin = p;
    eof = p + input.srcs.back().size;
}

bool Lexer::next_token()
{
    bool result = push_next_token();
    result &= tkns.step();
    
    if(flags & LF_PRINT_TOKENS)
	print_token(tkn_at(0), true);
    return result;
}

Token &Lexer::peek_next_token()
{
    push_next_token();
    // will just read Token{0} if get_next_token fails
    return tkns.tkn_at(1);
}

bool Lexer::push_next_token()
{
#ifdef TRACY_ALL
    ZoneScoped;
#endif
    if (p == eof)
	p = nullptr;
    if (!p)
	return tkns.push(Token{tkn_eof, eof});
    if (tkns.n_preparsed())
	return true;
	
    // skip whitespace and comments
    for (;;) {
	while (p != eof && is_whitespace(*p)) {
	    if (*p == '\n') {
		input.srcs.back().line_begins.push_back(p - p_begin);
		++p;
	    }
	    else {
		++p;
	    }
	}
	if (p[0] == '/' && p[1] == '*') {
	    p += 2;
	    while (p != eof && (p[0] != '*' || p[1] != '/'))
		++p;
	    ++p;
	    if (p != eof)
		++p;
	    continue;
	}
	if (p[0] == '/' && p[1] == '/') {
	    p += 2;
	    while (p != eof && (*p != '\n'))
		++p;
	    if(p != eof)
		++p;
	    input.srcs.back().line_begins.push_back(p - p_begin);
	    continue;
	}
	break;
    }
    
    // check for identifiers and keywords
    if (p != eof && (is_alpha(*p))) { // no leading _ or digit
	char* begin = p;
	++p;
	while (p != eof && (is_alpha(*p) || *p == '_' || is_digit(*p)))
	    ++p;
	std::string_view sv(begin, p);
	bool result = tkns.push(Token{tkn_ident, begin, &sv});
	tkns.back().type = keyword_compare(tkns.back().sv);
	return result;
    }
    
    // TODO: handle INF, INFINITY, NAN, etc.
    // check for numbers
    if (p != eof && (is_digit(*p) || (((p[0] == '.') || (p[0] == '+') || (p[0] == '-')) && is_digit(*(p + 1))))) {
	bool has_point = (*p == '.');
	bool has_exp = false;
	char* begin = p;
	++p;
	while (p != eof && (is_digit(*p) || *p == '.' || is_alpha(*p))) {
	    if (has_point && (*p == '.'))
		break;
	    has_point |= (*p == '.');
	    has_exp |= (*p == 'e' || *p == 'E');
	    ++p;
	}
	if (has_point || has_exp) {
	    double number = strtod(begin, &p);
	    HG_DEB_assert(errno != ERANGE, "parsed number was out of range");
	    return tkns.push(Token{tkn_real, begin, &number});
	}
	else {
	    int64_t number = strtoll(begin, &p, 0);
	    HG_DEB_assert(errno != ERANGE, "parsed number was out of range"); // TODO this should be an hg_error
	    return tkns.push(Token{tkn_int, begin, &number});
	}
    }
    
    // check for strings
    if (p != eof && *p == '\"') {
	++p;
	char* begin = p;
	while (p != eof && *p != '\"')
	    ++p;
	std::string_view sv(begin, p);
	++p;
	return tkns.push(Token{tkn_string, begin, &sv});
    }

    // check for operators
    for(uint32_t i = tkn_return; i <= tkn_greater_eq; ++i) {
	auto opt_tkn = check_for_operator(&p, eof, Token_enum(i), get_mul_char_token_name(Token_enum(i)));
	if(opt_tkn)
	    return tkns.push(opt_tkn.value());
    }

    // check for placeholders
    if (p < eof && is_delim_tkn_right(Token_enum(*p)) && is_delim_tkn_left(tkns.back().type)) {
	tkns.push(Token{tkn_placeholder, p});
    }
    
    if (p < eof && *p == '_')  {
	++p;
	return tkns.push(Token{tkn_placeholder, p-1});
    }

    // check for all remaining single chars
    if (p < eof && is_simple_char(*p)) {
	++p;
	return tkns.push(Token{Token_enum(*(p-1)), p-1});
    }

    // TODO: is this really necessary?
    if (tkns.n_preparsed())
	return true;

    if (p == eof) return tkns.push(Token{tkn_eof, eof});

    // when stuck, just get the next token.
    parsing_error(tkns.back(), "Failed to parse the character after this token.");
    ++p;
    return next_token();
}

void Lexer::print_token(Token &tkn, bool show_content, bool keep_line_location) const
{
    static int scope_cnt = 0;
    static int tab_cnt = 0;
    static uint32_t current_line = 0;

    if (keep_line_location) {
	Source_location src_loca = input.get_src_location(tkn.ptr);
	if(current_line != src_loca.line) {
	    std::cout << '\n';
	    current_line = src_loca.line;
	    tab_cnt = scope_cnt;
	    for (int j = 0; j < tab_cnt; ++j)
		std::cout << "   ";    
	}
    }
    else std::cout << '\n';

    if (is_open_bracket(tkn.type))
	++scope_cnt;
    else if (is_close_bracket(tkn.type))
	--scope_cnt;
	    
    if (tkn.type < 256) {
	std::cout << "\033[92m" << (char)tkn.type << "\033[0m";
    }
    else if (show_content) {
	switch (tkn.type) {
	case tkn_ident:
	case tkn_string: std::cout << "\033[91m" << tkn.sv << "\033[0m"; break;
	case tkn_int: std::cout << "\033[94m" << tkn.i << "\033[0m"; break;
	case tkn_real: std::cout << "\033[94m" << tkn.d << "\033[0m"; break;
	default: std::cout << "\033[93m" << get_mul_char_token_name(tkn.type) << "\033[0m";
	}
    }
    else std::cout << "\033[95m" << get_mul_char_token_name(tkn.type) << "\033[0m";
    std::cout << " ";
}

std::pair<std::string, std::string> Lexer::get_line_with_error(const char *p, const Source &source)
{
    int64_t begin = 0, end = 0;
    std::string line, error_ptr = "\033[91m";

    if (!is_whitespace(*p)) {
	while (!is_new_line(*(p + begin)) && (p + begin) > source.data)
	    --begin;
	while (is_whitespace(*(p + begin)) && (p + begin) < source.data + source.size)
	    ++begin;
	while (!is_new_line(*(p + end)) &&  (p + end) < source.data + source.size)
	    ++end;
	line = std::string{ p + begin, size_t(abs(end - begin)) };
	for (int i = 0; i < p - (p + begin); ++i)
	    error_ptr += " ";
    }
    error_ptr += "^\033[0m";

    return { line, error_ptr };
}

bool Lexer::Token_window::push(Token tkn)
{
    if(idx_last + 1 > size)
	return false;
    ++idx_last;
    tkns[idx_last] = tkn;
    return true;
}

bool Lexer::Token_window::step()
{
    HG_DEB_assert(n_preparsed(), "Can't step, no preparsed tokens.");

    tkns[size - 1] = Token{};
    for(int i = 0; i < size - 1;  ++i)
	tkns[i] = tkns[i + 1];
    --idx_last;
    return true;
}

bool Lexer::Token_window::pushstep(Token tkn)
{
    if(!push(tkn))
	return false;
    return step();
}
