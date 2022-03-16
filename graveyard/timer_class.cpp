#include <chrono>

template<typename unit>
struct Timer {
    std::chrono::_V2::system_clock::time_point begin;
    Timer() {
        begin = std::chrono::high_resolution_clock::now();
    }

    u64 GetTime() {
        auto end = std::chrono::high_resolution_clock::now();
        return std::chrono::duration_cast<unit>(end - begin).count();
    }

    ~Timer() {
        auto end = std::chrono::high_resolution_clock::now();
        auto delta = std::chrono::duration_cast<unit>(end - begin).count();
        std::cout << delta << std::endl;
    }
};
template<typename unit>
struct Timer {
    std::chrono::_V2::system_clock::time_point begin;
    Timer() {
        begin = std::chrono::high_resolution_clock::now();
    }

    auto Now() {
        return std::chrono::high_resolution_clock::now();
    }
    u64 FromBegin() {
        auto end = std::chrono::high_resolution_clock::now();
        return std::chrono::duration_cast<unit>(end - begin).count();
    }
    u64 From(std::chrono::_V2::system_clock::time_point begin_) {
        auto end = std::chrono::high_resolution_clock::now();
        return std::chrono::duration_cast<unit>(end - begin_).count();
    }

    ~Timer() {
        auto end = std::chrono::high_resolution_clock::now();
        auto delta = std::chrono::duration_cast<unit>(end - begin).count();
        global_print("%s%i%\n", "elapsed: ", delta);
    }
};