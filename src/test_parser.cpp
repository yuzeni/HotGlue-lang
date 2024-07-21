
#ifdef TRACY_ENABLE
#  include "tracy/Tracy.hpp"
#endif

#ifdef TRACY_MEMORY
void* operator new(std::size_t count) {
    auto ptr = malloc(count);
    TracyAlloc(ptr, count);
    return ptr;
}

void operator delete(void* ptr) noexcept {
    TracyFree(ptr);
    free(ptr);
}
#endif

#include "parser.hpp"

int main() {

    Parser parser;
    parser.parse_file("../test_input/type_test.hg");
    parser.ast.print();
}
