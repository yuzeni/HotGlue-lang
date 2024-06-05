// TODO: write a bump / arena allocator. It should follow the std requierments
// https://www.youtube.com/watch?v=CVbnjydW5M0
// https://pastebin.com/v9JdHnQN
// https://en.cppreference.com/w/cpp/named_req/Allocator

#pragma once

#include <cstddef> // size_t
#include <vector>

static constexpr bool is_pow_of_2(size_t x) {
    return x && !(x & (x - 1));
}

static consteval int CTE_log2(size_t x) {
    int result = 0;
    while(x >>= 1) ++result;
    return result;
}

enum class Bump_alloc_type {
  LINEAR,
  QUADRATIC,
};

struct Bump_allocation {

    size_t ptr;
};

constexpr size_t KiB(size_t kib) { return kib * 2e10; }
constexpr size_t MiB(size_t mib) { return mib * 2e20; }
constexpr size_t GiB(size_t gib) { return gib * 2e30; }

template<Bump_alloc_type type = Bump_alloc_type::LINEAR, size_t init_size = KiB(1), size_t limit = 0>
class Bump_allocator {
    
public:
    
    Bump_allocator() {
	static_assert(type == Bump_alloc_type::QUADRATIC && is_pow_of_2(init_size),
		      "init_size of Bump_allocator of type QUADRATIC must be a power of 2");
    }
    ~Bump_allocator() { deallocate(); }

    template<typename T>
    void* allocate() {

	if(bumps.empty() || sizeof(T) > bumps.back().remaining()) {
	    
	    if (sizeof(T) > bump_capacity(bumps.size()))
		return nullptr;
	    if constexpr(limit)
		if(until_bump_capacity(bumps.size()) >= limit)
		    return nullptr;

	    size_t new_alloc_size = 0;
	    if constexpr(type == Bump_alloc_type::LINEAR)
		new_alloc_size = init_size;
	    else if constexpr(type == Bump_alloc_type::QUADRATIC)
		new_alloc_size = init_size << bumps.size();
	    
	    bumps.emplace_back({new std::byte[new_alloc_size], new_alloc_size, 0});
	}

	bumps.back().ptr += sizeof(T);
	return bumps.back().data + sizeof(T);
    }

    void deallocate() {
	for(auto& bump : bumps)
	    delete bump.data;
	bumps.clear();
    }

private:

    struct Bump {
	Bump(void* data, size_t capacity) : data(data), capacity(capacity), ptr(0) {}
	void* data = nullptr;
	size_t capacity;
	size_t ptr;
	size_t remaining() const {return capacity - ptr;}
    };
    
    std::vector<Bump> bumps;

    constexpr size_t bump_capacity(size_t bump_idx) {
	if constexpr(type == Bump_alloc_type::QUADRATIC)
	    return init_size << bump_idx;
	else if constexpr(type == Bump_alloc_type::LINEAR)
	    return init_size;
    }

    constexpr size_t until_bump_capacity(size_t bump_idx) {
	if constexpr(type == Bump_alloc_type::QUADRATIC)
	    return ((1ULL << (bump_idx + 1)) - 1) << CTE_log2(init_size);
	else if constexpr(type == Bump_alloc_type::LINEAR)
	    return bump_idx * init_size;
    }
};
