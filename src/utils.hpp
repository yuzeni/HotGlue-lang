#pragma once

#include <cstdint>
#include <utility>
#include <string>
#include <initializer_list>

namespace utils {
    constexpr uint64_t default_str_hash_value = 5381;
};

std::pair<char *, size_t> parse_file_cstr(const char *file_name);

constexpr std::string cte_concat_c_str(std::initializer_list<std::string> strs) {
    std::string result;
    for(const std::string& str : strs)
	result += str;
    return result;
}

// string hash funciton 'djb2' from: http://www.cse.yorku.ca/~oz/hash.html
uint64_t hash_string_view(std::string_view s_v, uint64_t hash = utils::default_str_hash_value);

constexpr uint64_t hash_c_str(const char *str, uint64_t hash = utils::default_str_hash_value)
{
    int c;
    while((c = *(str++)))
	hash = (hash * 33) ^ c;
    return hash;
}

constexpr uint64_t hash_char(char c, uint64_t hash = utils::default_str_hash_value)
{
    return (hash * 33) ^ c;
}

consteval uint64_t cte_hash_c_str(const char *str, uint64_t hash = utils::default_str_hash_value)
{
    int c;
    while((c = *(str++)))
	hash = (hash * 33) ^ c;
    return hash;
}

template <typename T>
constexpr T log2i(T x)
{
    int cnt = 0;
    while(x >>= 1) ++cnt;
    return cnt;
}
