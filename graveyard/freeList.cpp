#include <stdarg.h>
#include <memory.h>
#include <iostream>
#include <sys/mman.h>
#include <unistd.h>

typedef int8_t  i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef u8 byte;

static_assert(sizeof(u8)  == 1 && sizeof(i8)  == 1);
static_assert(sizeof(u16) == 2 && sizeof(i16) == 2);
static_assert(sizeof(u32) == 4 && sizeof(i32) == 4);
static_assert(sizeof(u64) == 8 && sizeof(i64) == 8);

typedef float       f32;
typedef double      f64;
typedef long double f128;

static_assert(sizeof(f32)  == 4);
static_assert(sizeof(f64)  == 8);
static_assert(sizeof(f128) == 16);

constexpr u64 KILO_BYTE = 1000;
constexpr u64 MEGA_BYTE = 1000 * KILO_BYTE;
constexpr u64 GIGA_BYTE = 1000 * MEGA_BYTE;
constexpr u64 TERA_BYTE = 1000 * GIGA_BYTE;
constexpr u32 MICRO_SEC = 1000;
constexpr u32 MILI_SEC = 1000 * MICRO_SEC;

#define DEBUG_BUILD

#ifdef __GNUC__
    #define _FORCE_INLINE __attribute__((always_inline))
    #define _RESTRICT __restrict__
    #define _TRAP __builtin_trap()

    template <typename T>
    using v2 = T __attribute__((vector_size(sizeof(T) * 2)));
    template <typename T>
    using v4 = T __attribute__((vector_size(sizeof(T) * 4)));
    template <typename T>
    using v8 = T __attribute__((vector_size(sizeof(T) * 8)));

#else
    #ifdef _MSC_VER
    #endif
#endif

#ifdef DEBUG_BUILD
    #define LOG_ASSERT(x, y)                                                                \
        if (!(x)) {                                                                         \
            std::cerr << #x << " " << y << " " << __FILE__ << " " << __LINE__ << std::endl; \
            _TRAP;                                                                          \
        }
    #define ASSERT(x)                                                                                      \
        if (!(x)) {                                                                                        \
            std::cerr << #x << " triggered builtin trap in: " << __FILE__ << " " << __LINE__ << std::endl; \
            _TRAP;                                                                                         \
        }
    #define LOG(x) x;
#else 
    #define LOG(x) x;
    #define ASSERT(x) x;
    #define LOG_ASSERT(x, y) x;
#endif

template <typename T>
T &Mem(void *mem) {
    return *((T *)mem);
}
template <typename T>
T Min(T t0, T t1) {
    return t0 > t1 ? t1 : t0;
}
template <typename T>
T Max(T t0, T t1) {
    return t0 > t1 ? t0 : t1;
}
template <typename T>
T Abs(T t0) {
    return Max<T>(t0, -t0);
}
template <typename T>
T Clamp(T t0, T max, T min) {
    return Min(Max(t0, min), max);
}

struct FreeList {
    FreeList* next;
};
struct FreeListState {
    FreeList* head;
};
void* free_list_allocate(FreeListState* list) {
    auto tmp = list->head;
    list->head = list->head->next;
    return tmp;
}
void free_list_free(FreeListState* list, void* memory) {
    Mem<FreeList>(memory).next = list->head;
    list->head = (FreeList*)memory;
}

template<u32 slabSize>
struct MemoryPool {
    byte* base;
    FreeListState list;
    u32 top;
    u32 poolSize;
};

template<u32 slabSize>
MemoryPool<slabSize> make_memory_pool(void* base, u32 size) {
    static_assert(slabSize >= sizeof(void*));
    return {(byte*)base, nullptr, 0, size};
}

template<u32 slabSize>
void* pool_allocate(MemoryPool<slabSize>* pool) {
    static_assert(slabSize >= sizeof(void*));

    if(pool->list.head) {
        return free_list_allocate(&pool->list);
    }
    auto ret = pool->base + pool->top;
    pool->top += slabSize;
    LOG_ASSERT(pool->top <= pool->poolSize, "pool overflow");
    return ret;
}

template<u32 slabSize>
void pool_free(MemoryPool<slabSize>* pool, void* memory) {
    static_assert(slabSize >= sizeof(void*));
    free_list_free(&pool->list, memory);
}

i32 main() {

    auto pool = make_memory_pool<sizeof(u64)>(alloca(64*KILO_BYTE), 64*KILO_BYTE);

    void* allocs[10];
    for(u32 i = 0; i < 10; i++) {
        allocs[i] = pool_allocate(&pool);
        std::cout << allocs[i] << std::endl;
    }
    for(u32 i = 0; i < 5; i++) {
        pool_free(&pool, allocs[i]);
    }
    std::cout << std::endl;
    for(u32 i = 0; i < 10; i++) {
        allocs[i] = pool_allocate(&pool);
        std::cout << allocs[i] << std::endl;
    }
}