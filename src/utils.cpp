#include "utils.hpp"

#include <iostream>
#include <fstream>

#include "log_and_debug.hpp"

std::pair<char*, size_t> parse_file_cstr(const char* file_name) {

    std::ifstream file(file_name, std::ios::binary);
    
    if(!file.is_open()) {
	hg_error(HG_err::IO, "Failed to open file: %s", file_name);
	return {nullptr, 0};
    }
    
    file.seekg(0, file.end); // go to the end of the file
    size_t file_size = file.tellg();
    file.seekg(0, file.beg); // go to the beginning of the file

    char* data = new char[file_size + 1];
    file.read(data, file_size);

    if(!file) {
	hg_error(HG_err::IO, "Error reading file: %s. Only %d could be read", file_name, file.gcount());
	return {nullptr, 0};
    }
    
    data[file_size] = '\0';
    return { data, file_size };
}

uint64_t hash_string_view(std::string_view s_v, uint64_t hash) {
    for(size_t i = 0; i < s_v.size(); ++i)
	hash = (hash * 33) ^ s_v[i];
    return hash;
}

