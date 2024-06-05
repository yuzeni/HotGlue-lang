#pragma once

#include <utility>
#include <string>
#include <initializer_list>

std::pair<char *, size_t> parse_file_cstr(const char *file_name);

constexpr std::string cte_concat_c_str(std::initializer_list<std::string> strs) {
    std::string result;
    for(const std::string& str : strs)
	result += str;
    return result;
}

// string hash funciton 'djb2' from: http://www.cse.yorku.ca/~oz/hash.html
uint64_t hash_string_view(std::string_view s_v, uint64_t hash = 5381);

constexpr uint64_t hash_c_str(const char* str, uint64_t hash = 5381) {
    int c;
    while((c = *(str++)))
	hash = (hash * 33) ^ c;
    return hash;
}

consteval uint64_t cte_hash_c_str(const char* str, uint64_t hash = 5381) {
    int c;
    while((c = *(str++)))
	hash = (hash * 33) ^ c;
    return hash;
}
