#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <memory.h>
#include <sys/mman.h>
#include <unistd.h>

#include <type_traits>

typedef int8_t  i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef u8  bit_mask8;
typedef u16 bit_mask16;
typedef u32 bit_mask32;
typedef u64 bit_mask64;

typedef u8 byte;

#define int i64
#define uint u64

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

struct f16 {
    u16 bits;
};

constexpr u64 KILO_BYTE       = 1000 * 1;
constexpr u64 MEGA_BYTE       = 1000 * KILO_BYTE;
constexpr u64 GIGA_BYTE       = 1000 * MEGA_BYTE;
constexpr u64 TERA_BYTE       = 1000 * GIGA_BYTE;
constexpr u64 MICRO_SEC       = 1000 * 1;
constexpr u64 MILI_SEC        = 1000 * MICRO_SEC;
constexpr u64 SECOND          = 1000 * MILI_SEC;
constexpr u64 CACHE_LINE_SIZE = 64;

#define DEBUG_BUILD
#ifdef __GNUC__

    #define _FORCE_INLINE       __attribute__((always_inline))
    #define _NO_INLINE          __attribute__((noinline))
    #define _RESTRICT           __restrict__
    #define _TRAP               __builtin_trap()
    #define _LIKELY(x)          __bultin_expect(x, 1)
    #define _UN_LIKELY(x)       __bultin_expect(x, 0)

    template <typename T>
    using v2 = T __attribute__((vector_size(sizeof(T) * 2)));
    template <typename T>
    using v4 = T __attribute__((vector_size(sizeof(T) * 4)));
    template <typename T>
    using v8 = T __attribute__((vector_size(sizeof(T) * 8)));

    typedef v2<u64> bit_mask128;
    typedef v4<u64> bit_mask256;
#else
    #ifdef _MSC_VER
    #endif
#endif

void global_io_flush();
void global_print(const char* format ...);
void runtime_panic(const char* file, u32 line);

#define RUNTIME_CHECK(x)                          \
            if(!x) {                              \
                runtime_panic(__FILE__, __LINE__);\
            }                                     \

#ifdef DEBUG_BUILD
    #define LOG_ASSERT(x, y)                                                                     \
        if (!(x)) {                                                                              \
            global_print("%c%s%s%s%c%s%c%i%\n", '(', #x, ") ", y, ' ', __FILE__, ' ', __LINE__); \
            global_io_flush();                                                                   \
            _TRAP;                                                                               \
        }
    #define ASSERT(x)                                                                                           \
        if (!(x)) {                                                                                             \
            global_print("%c%s%s%s%c%i%\n", '(', #x, ") triggered builtin trap in: ", __FILE__, ' ', __LINE__); \
            global_io_flush();                                                                                  \
            _TRAP;                                                                                              \
        }
    #define LOG(x) x;
#else
    #define LOG(x) x;
    #define ASSERT(x) x;
    #define LOG_ASSERT(x, y) x;
#endif

u64 Kilobyte(u64 n) {
    return n * KILO_BYTE;
}
u64 Megabyte(u64 n) {
    return n * MEGA_BYTE;
}
u64 Gigabyte(u64 n) {
    return n * GIGA_BYTE;
}
u64 Terabyte(u64 n) {
    return n * TERA_BYTE;
}
u64 Milisecond(u64 n) {
    return n * MILI_SEC;
}
u64 Second(u64 n) {
    return n * SECOND;
}
u64 Minute(u64 n) {
    return n * SECOND * 60;
}
u64 Hour(u64 n) {
    return n * SECOND * 60 * 60;
}
template<typename T>
T SetBitMaskBit(T mask, u32 bit) {   
    T set = 1 << bit;
    return mask | set;
}
template<typename T>
T ClearBitMaskBit(T mask, u32 bit) {   
    T set = ~(1 << bit);
    return mask & set;
}
template<typename T>
bool GetBitMaskBit(T mask, u32 bit) {
    return bool(mask >> bit);
}
template<typename T>
T SetBitMaskBitTo(T mask, u32 bit, bool to) {
    T set = ~(T(1) << bit);
    T clear = mask & set;
    return clear | (T(to) << bit);
}
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
template<typename T>
T* GetPointer(void* base, u32 ptr) {  
    return ptr ? (T*)(((byte*)base) + ptr) : nullptr;
}
u64 align(u64 n, u64 alignment) {
    bool aligned = (n % alignment != 0);
    return n + aligned * alignment - (n % alignment);
}
void* align_pointer(void* n, u64 alignment) {
    bool aligned = ( (intptr_t)n % alignment != 0);
    return n + aligned * alignment - ( (intptr_t)n % alignment);
}
union f32_union_u32 {
    f32 f;
    u32 u;
};
f32 f16_to_f32(f16 f) {
    
    bool sign = f.bits >> 15;

    u32 fraction = u32(f.bits) & (~u32(0) >> 22);
    u32 exponent = u32(f.bits >> 10) & (~u32(0) >> 27);

    fraction = fraction << 13;
    u32 biased = exponent - 15;
    biased = Clamp(biased + 127, (u32)255, (u32)0);

    f32_union_u32 bit_pattern;
    bit_pattern.u = (u32(sign) << 31) | (biased << 23) | (fraction);

    return bit_pattern.f;
}
f16 f32_to_f16(f32 f) {

    f32_union_u32 bit_pattern;
    bit_pattern.f = f;
    u32 fract = bit_pattern.u & (~u32(0) >> 9);
    u32 exp = (bit_pattern.u >> 23) & (~u32(0) >> 24);
    bool sign = bit_pattern.u >> 31;

    u32 biased = exp - 127;
    biased = Clamp(biased + 15, (u32)31, (u32)0);

    fract = fract >> 13;

    f16 ret;
    ret.bits = (u16(sign) << 15) | (u16(biased) << 10) | u16(fract);

    return ret;
}

template<typename T>
void meminc(T* dst, T* end) {
    for(; dst != end; dst++) {
        (*dst)++;
    }
}

template<typename T>
struct SelfRelativePointer {
    i32 add;
    T* operator*() {
        ASSERT(add != 0);
        return (T*)(&add + add);
    }
    T* operator->() {
        ASSERT(add != 0);
        return (T*)(&add + add);
    }
};
template<typename T>
struct ForwardRelativePointer {
    u32 add;
    T* operator*() {
        ASSERT(add != 0);
        return (T*)(&add + add);
    }
    T* operator->() {
        ASSERT(add != 0);
        return (T*)(&add + add);
    }
};


struct LinearAllocator {
    byte* base;
    u32 cap;
    u32 top;
};
LinearAllocator make_linear_allocator(void* base, u32 size) {
    LinearAllocator stack;
    stack.base = (byte*)base;
    stack.cap = size;
    stack.top = 0;
    return stack;
}

void* linear_allocate(LinearAllocator* stack, u32 size) {
    auto mem = stack->base + stack->top;
    stack->top += size;
    LOG_ASSERT(stack->top <= stack->cap, "stack overflow");
    return mem;
}
void* linear_allocate_reversed(LinearAllocator* stack, u32 size) {
    auto mem = stack->base - stack->top;
    stack->top += size;
    LOG_ASSERT(stack->top <= stack->cap, "stack overflow");
    return mem;
}
void linear_deallocate(LinearAllocator* stack, u32 size) {
    LOG_ASSERT(stack->top >= size, "stack underflow");
    stack->top -= size;
}
void roll_back_linear_allocator(LinearAllocator* stack, void* pos) {
    ASSERT(pos >= stack->base && pos <= (stack->base+stack->top));
    stack->top = (byte*)pos - stack->base;
}
void* linear_allocator_top(LinearAllocator* stack) {
    return stack->base + stack->top;
}
void* linear_allocator_top_reversed(LinearAllocator* stack) {
    return stack->base - stack->top;
}
u32 linear_allocator_free_size(LinearAllocator* stack) {
    return stack->cap - stack->top;
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

template<u32 slab_size>
struct MemoryPool {
    byte* base;
    FreeListState list;
    u32 top;
    u32 poolSize;
};

template<u32 slab_size>
MemoryPool<slab_size> make_memory_pool(void* base, u32 size) {
    static_assert(slab_size >= sizeof(void*));
    base = align_pointer(base, CACHE_LINE_SIZE);
    return {(byte*)base, nullptr, 0, size};
}

template<u32 slab_size>
void* pool_allocate(MemoryPool<slab_size>* pool) {
    static_assert(slab_size >= sizeof(void*));

    if(pool->list.head) {
        return free_list_allocate(&pool->list);
    }
    auto ret = pool->base + pool->top;
    pool->top += slab_size;
    LOG_ASSERT(pool->top <= pool->poolSize, "pool overflow");
    return ret;
}

template<u32 slab_size>
void pool_free(MemoryPool<slab_size>* pool, void* memory) {
    static_assert(slab_size >= sizeof(void*));
    free_list_free(&pool->list, memory);
}
template<typename T>
struct LocalList {
    LocalList* next;
    u32 count;
    T arr[];
    #define ALLOCATE_LOCAL_LIST(prev, size) prev = (decltype(prev))alloca(size + sizeof(decltype(prev)));
};

_NO_INLINE
void runtime_panic(const char* file, u32 line) {

    global_io_flush();
    global_print("%s%s%s%i%\n", "runtime panic in file: ", file, " at line: ", line);
    global_io_flush();
    exit(1);
}

// ------------- shared mutable global state BEGIN --------------------
typedef void(*malloc_handler_t)();
byte *global_malloc_base;
malloc_handler_t global_out_of_memory_handler;
LinearAllocator io;
// ------------- shared mutable global state END ----------------------

struct MemoryBlockHeader {
    u32 left_ptr;
    u32 right_ptr;
    u32 size;
    static constexpr u32 FREE_BIT_MASK = ~u32(0) >> 1;
};
bool extract_free_bit(u32 mem) {
    return bool((mem >> 31) & 1);
}
void set_free_bit(MemoryBlockHeader* block) {
    block->size |= (1 << 31);
}
void clear_free_bit(MemoryBlockHeader* block) {
    block->size = block->size & MemoryBlockHeader::FREE_BIT_MASK;
}
void set_size_in_block(MemoryBlockHeader* block, u32 size) {
    u32 free_bit = extract_free_bit(block->size);
    ASSERT(!extract_free_bit(size));
    block->size = size | (free_bit << 31);
}
u32 get_size_in_block(MemoryBlockHeader* block) {
    return block->size & block->FREE_BIT_MASK;
}
MemoryBlockHeader* get_block_ptr(byte* base, u32 smallPtr) {
    return (MemoryBlockHeader*)(smallPtr ? base + smallPtr : nullptr);
}

// observes shared global mutable state
MemoryBlockHeader *search_free_block(u32 size) {

    MemoryBlockHeader *block = (MemoryBlockHeader*)(global_malloc_base+1);
    while (block) {

#ifdef DEBUG_BUILD
        if (block->left_ptr) {
            MemoryBlockHeader* left = (MemoryBlockHeader*)(global_malloc_base + block->left_ptr);
            LOG_ASSERT(left->right_ptr == (byte*)block - global_malloc_base, "internal allocator corruption");
        }
        if (block->right_ptr) {
            MemoryBlockHeader* right = (MemoryBlockHeader*)(global_malloc_base + block->right_ptr);
            LOG_ASSERT(right->left_ptr == (byte*)block - global_malloc_base, "internal allocator corruption");
        }
#endif

        if (extract_free_bit(block->size) && get_size_in_block(block) >= size) return block;
        block = get_block_ptr(global_malloc_base, block->right_ptr);
    }

    return nullptr;
}

// mutates shared global state
void init_global_malloc(void *base_, u32 size, malloc_handler_t handler) {
    
    //null pointers are reserved
    global_malloc_base = (byte*)base_;
    MemoryBlockHeader* first_block = (MemoryBlockHeader*)(global_malloc_base+1);
    first_block->left_ptr = 0;
    first_block->right_ptr = 0;
    set_size_in_block(first_block, size);
    set_free_bit(first_block);
    global_out_of_memory_handler = handler;
}
// mutates shared global state

void* global_malloc(u32 size) {

    if (!size) return nullptr;

    MemoryBlockHeader *free_block = search_free_block(size);
    if (free_block) {
        LOG_ASSERT(extract_free_bit(free_block->size), "internal corruption");
        clear_free_bit(free_block);
        if (get_size_in_block(free_block) - size > sizeof(MemoryBlockHeader)) {

            byte *free_block_end = ((byte *)(free_block + 1)) + size;
            MemoryBlockHeader *new_free_block = (MemoryBlockHeader*)free_block_end;
            *new_free_block = {};

            set_size_in_block(new_free_block, (get_size_in_block(free_block) - size) - sizeof(MemoryBlockHeader));
            set_free_bit(new_free_block);
            new_free_block->right_ptr = free_block->right_ptr;
            new_free_block->left_ptr = (byte*)free_block - global_malloc_base;
            if (free_block->right_ptr) {
                MemoryBlockHeader* right = (MemoryBlockHeader*)(global_malloc_base + free_block->right_ptr);
                right->left_ptr = (byte*)new_free_block - global_malloc_base;
            }

            free_block->right_ptr = (byte*)new_free_block - global_malloc_base;
            set_size_in_block(free_block, size);
        }
        return free_block + 1;
    }

    global_out_of_memory_handler();
    return nullptr;
}
// mutates shared global state
void global_free(void *block) {
    if (!block) return;

    MemoryBlockHeader *header = ((MemoryBlockHeader*)block) - 1;
    LOG_ASSERT(!extract_free_bit(header->size), "double free");
    set_free_bit(header);

    auto next_block = get_block_ptr(global_malloc_base, header->right_ptr);
    auto previous_block = get_block_ptr(global_malloc_base, header->left_ptr);

#ifdef DEBUG_BUILD
    if (next_block) {
        ASSERT(next_block->left_ptr == (byte*)header - global_malloc_base);
        ASSERT(get_size_in_block(next_block) != 0);
    }
    if (previous_block) {
        ASSERT(previous_block->right_ptr == (byte*)header - global_malloc_base);
        ASSERT(get_size_in_block(previous_block) != 0);
    }
#endif

    while (next_block) {
        if (!extract_free_bit(next_block->size))
            break;

        u32 header_size = get_size_in_block(header) + get_size_in_block(next_block) + sizeof(MemoryBlockHeader);
        set_size_in_block(header, header_size);
        header->right_ptr = next_block->right_ptr;
        if (header->right_ptr) {
            auto right = (MemoryBlockHeader*)(global_malloc_base + header->right_ptr);
            right->left_ptr = (byte*)header - global_malloc_base;
        }

        next_block = get_block_ptr(global_malloc_base, header->right_ptr);
    }
    while (previous_block) {
        if (!extract_free_bit(previous_block->size))
            break;

        u32 previous_block_size = get_size_in_block(previous_block) + get_size_in_block(header) + sizeof(MemoryBlockHeader);
        set_size_in_block(previous_block, previous_block_size);
        previous_block->right_ptr = header->right_ptr;

        if (previous_block->right_ptr) {

            auto right = (MemoryBlockHeader*)(global_malloc_base + previous_block->right_ptr);
            right->left_ptr = (byte*)previous_block - global_malloc_base;
        }

        header = previous_block;
        previous_block = get_block_ptr(global_malloc_base, previous_block->left_ptr);
    }
}

void global_print(const char* format ...);

// observes shared global state
void print_heap_info() {
    MemoryBlockHeader *block = (MemoryBlockHeader*)(global_malloc_base+1);
    
    u32 total = 0;
    u32 totalBlockCount = 0;
    u32 allocatedTotal = 0;
    u32 allocatedTotalBlockCount = 0;
    u32 freeTotal = 0;
    u32 freeBlockCount = 0;
    u32 fragmented = 0;
    u32 maxFreeBlock = 0;

    while (block) {

        u32 block_size = get_size_in_block(block);
        total += block_size;
        totalBlockCount++;
        if(extract_free_bit(block->size)) {
            fragmented += block_size;
            freeTotal += block_size;
            freeBlockCount++;
            maxFreeBlock = Max(maxFreeBlock, block_size);
        }
        else {
            allocatedTotal += block_size;
            allocatedTotalBlockCount++;
        }

        global_print("%i%c%i%c%i%c", (u64)block, ' ', extract_free_bit(block->size), ' ', get_size_in_block(block), '\n');
        block = get_block_ptr(global_malloc_base, block->right_ptr);
    }


    global_print("%s%i%\n", "total: "               , total);
    global_print("%s%i%\n", "total block count: "   , totalBlockCount);
    global_print("%s%i%\n", "in use: "              , allocatedTotal);
    global_print("%s%i%\n", "in use block count: "  , allocatedTotalBlockCount);
    global_print("%s%i%\n", "free: "                , freeTotal);
    global_print("%s%i%\n", "free block count: "    , freeBlockCount);
    global_print("%s%f%c%\n", "fragmentation: "     , ((f64)(freeTotal - maxFreeBlock) / (f64)freeTotal) * 100.0, '%' );
}
// observes shared global state
u32 check_live_mem(void *block) {

    if (!block)
        return ~u32(0);
    MemoryBlockHeader *header = ((MemoryBlockHeader*)block) - 1;
    ASSERT(!extract_free_bit(header->size));

    MemoryBlockHeader *next_block = get_block_ptr(global_malloc_base, header->right_ptr);
    MemoryBlockHeader *previous_block = get_block_ptr(global_malloc_base, header->left_ptr);

    if (next_block) {
        ASSERT(next_block->left_ptr == (byte*)header - global_malloc_base);
        ASSERT(get_size_in_block(next_block) != 0);
    }
    if (previous_block) {
        ASSERT(previous_block->right_ptr == (byte*)header - global_malloc_base);
        ASSERT(get_size_in_block(previous_block) != 0);
    }

    return header->size;
}
// observes shared global state
bool check_memory_integrity(void *mem) {

    if (!mem)
        return true;
    u32 size = *((u32 *)((byte *)mem - 64));
    byte *back_guard = ((byte *)mem) - 60;
    byte *front_guard = ((byte *)mem) + size;

    ASSERT((check_live_mem(back_guard - 4) - (size + 128)) <= sizeof(MemoryBlockHeader));

    bool corrupt = false;
    for (u32 i = 0; i < 60; i++) {
        corrupt |= back_guard[i] != 255;
    }
    for (u32 i = 0; i < 64; i++) {
        corrupt |= front_guard[i] != 255;
    }

    if (corrupt) {

        global_print("%\n");
        for (u32 i = 0; i < 60; i++) {
            global_print("%i%c", (u32)back_guard[i], ' ');
        }
        global_print("%\n");
        for (u32 i = 0; i < 64; i++) {
            global_print("%i%c", (u32)front_guard[i], ' ');
        }
        global_print("%s%\n", "heap corruption detected");
    }

    ASSERT(!corrupt);
    return corrupt;
}
// observes shared global state
void check_all_memory(void *check) {
    if (check != nullptr) {
        ASSERT(!check_memory_integrity(check));
    }

    bool found = false;
    const MemoryBlockHeader *block = (MemoryBlockHeader*)(global_malloc_base+1);
    while (block) {
        byte *mem = (byte *)block;
        if (!extract_free_bit(block->size)) {
            check_memory_integrity(mem + 64 + sizeof(MemoryBlockHeader));
        }
        if (check != nullptr && check == (mem + 64 + sizeof(MemoryBlockHeader))) {
            found = true;
        }
        block = get_block_ptr(global_malloc_base, block->right_ptr);
    }

    if (check != nullptr) {
        ASSERT(found);
    }
}
// mutates shared global state
void *global_malloc_debug(u32 size) {

#ifdef DEBUG_BUILD
    byte *mem = (byte *)global_malloc(size + 128);
    Mem<u32>(mem) = size;
    memset(mem + sizeof(u32), 255, 60 + 64 + size);
    check_all_memory(mem + 64);
    ASSERT((check_live_mem(mem) - (size + 128)) <= sizeof(MemoryBlockHeader));
    return mem + 64;
#else
    return global_malloc(size);
#endif
}
u32 get_allocation_size_debug(void* mem) {

#ifdef DEBUG_BUILD
    byte* allocation = (byte*)mem - 64;
    MemoryBlockHeader *header = ((MemoryBlockHeader*)allocation) - 1;
    return get_size_in_block(header);
#else
    MemoryBlockHeader *header = ((MemoryBlockHeader*)mem) - 1;
    return get_size_in_block(header);
#endif
}
// mutates shared global state
void global_free_debug(void *mem) {

#ifdef DEBUG_BUILD
    if (!mem)
        return;
    u32 size = *((u32 *)((byte *)mem - 64));
    byte *back_guard = ((byte *)mem) - 60;
    byte *front_guard = ((byte *)mem) + size;

    for (u32 i = 0; i < 60; i++) {
        ASSERT(back_guard[i] == 255);
    }
    for (u32 i = 0; i < 64; i++) {
        ASSERT(front_guard[i] == 255);
    }
    check_all_memory(mem);
    ASSERT((check_live_mem(back_guard - 4) - (size + 128)) <= sizeof(MemoryBlockHeader));
    global_free(back_guard - 4);
#else
    return global_free(mem);
#endif
}

struct  LocalMallocState {
    MemoryBlockHeader* headBlock;
};

byte* get_local_malloc_base(LocalMallocState state) {
    return ((byte*)state.headBlock) - 1;
}
LocalMallocState make_local_malloc(byte* base, u32 size) {

    LocalMallocState state;
    state.headBlock = (MemoryBlockHeader*)(base+1);
    state.headBlock->left_ptr = 0;
    state.headBlock->right_ptr = 0;
    set_free_bit(state.headBlock);
    set_size_in_block(state.headBlock, size);
    return state;
}
MemoryBlockHeader *local_search_free_block(LocalMallocState* state, u32 size) {

    byte* base = get_local_malloc_base(*state);
    MemoryBlockHeader *block = state->headBlock;
    while (block) {

#ifdef DEBUG_BUILD
        if (block->left_ptr) {
            MemoryBlockHeader* left = (MemoryBlockHeader*)(base + block->left_ptr);
            LOG_ASSERT(left->right_ptr == (byte*)block - base, "internal allocator corruption");
        }
        if (block->right_ptr) {
            MemoryBlockHeader* right = (MemoryBlockHeader*)(base + block->right_ptr);
            LOG_ASSERT(right->left_ptr == (byte*)block - base, "internal allocator corruption");
        }
#endif

        if (extract_free_bit(block->size) && get_size_in_block(block) >= size) return block;
        block = get_block_ptr(base, block->right_ptr);
    }

    return nullptr;
}
void* local_malloc(LocalMallocState* state, u32 size) {
    
    if (!size)
        return nullptr;

    byte* base = get_local_malloc_base(*state);
    MemoryBlockHeader *free_block = local_search_free_block(state, size);
    if (free_block) {
        LOG_ASSERT(extract_free_bit(free_block->size), "internal corruption");
        clear_free_bit(free_block);
        if (get_size_in_block(free_block) - size > sizeof(MemoryBlockHeader)) {

            byte *free_block_end = ((byte *)(free_block + 1)) + size;
            MemoryBlockHeader *new_free_block = (MemoryBlockHeader*)free_block_end;
            *new_free_block = {};

            set_size_in_block(new_free_block, (get_size_in_block(free_block) - size) - sizeof(MemoryBlockHeader));
            set_free_bit(new_free_block);
            new_free_block->right_ptr = free_block->right_ptr;
            new_free_block->left_ptr = (byte*)free_block - base;
            if (free_block->right_ptr) {
                MemoryBlockHeader* right = (MemoryBlockHeader*)(base + free_block->right_ptr);
                right->left_ptr = (byte*)new_free_block - base;
            }

            free_block->right_ptr = (byte*)new_free_block - base;
            set_size_in_block(free_block, size);
        }
        return free_block + 1;
    }
    ASSERT(false);
}
void local_free(LocalMallocState* state, void* block) {
    
    if (!block) return;

    byte* base = get_local_malloc_base(*state);
    MemoryBlockHeader *header = ((MemoryBlockHeader*)block) - 1;
    LOG_ASSERT(!extract_free_bit(header->size), "double free");
    set_free_bit(header);

    auto next_block = get_block_ptr(base, header->right_ptr);
    auto previous_block = get_block_ptr(base, header->left_ptr);

#ifdef DEBUG_BUILD
    if (next_block) {
        ASSERT(next_block->left_ptr == (byte*)header - base);
        ASSERT(get_size_in_block(next_block) != 0);
    }
    if (previous_block) {
        ASSERT(previous_block->right_ptr == (byte*)header - base);
        ASSERT(get_size_in_block(previous_block) != 0);
    }
#endif

    while (next_block) {
        if (!extract_free_bit(next_block->size))
            break;

        u32 header_size = get_size_in_block(header) + get_size_in_block(next_block) + sizeof(MemoryBlockHeader);
        set_size_in_block(header, header_size);
        header->right_ptr = next_block->right_ptr;
        if (header->right_ptr) {
            auto right = (MemoryBlockHeader*)(base + header->right_ptr);
            right->left_ptr = (byte*)header - base;
        }

        next_block = get_block_ptr(base, header->right_ptr);
    }
    while (previous_block) {
        if (!extract_free_bit(previous_block->size))
            break;

        u32 previous_block_size = get_size_in_block(previous_block) + get_size_in_block(header) + sizeof(MemoryBlockHeader);
        set_size_in_block(previous_block, previous_block_size);
        previous_block->right_ptr = header->right_ptr;

        if (previous_block->right_ptr) {

            auto right = (MemoryBlockHeader*)(base + previous_block->right_ptr);
            right->left_ptr = (byte*)previous_block - base;
        }

        header = previous_block;
        previous_block = get_block_ptr(base, previous_block->left_ptr);
    }
}
void local_malloc_shrink(LocalMallocState* state, void* block, u32 size) {

    LOG_ASSERT(size, "size must be > 0");
    MemoryBlockHeader* header = ((MemoryBlockHeader*)block) - 1;
    LOG_ASSERT(!extract_free_bit(header->size), "use after free");
    u32 block_size = get_size_in_block(header);
    LOG_ASSERT(size <= block_size, "block cannot grow");

    byte* base = get_local_malloc_base(*state);
    MemoryBlockHeader* right_block = get_block_ptr(base, header->right_ptr);

    MemoryBlockHeader* fresh_block = (MemoryBlockHeader*)((byte*)block + size);
    fresh_block->left_ptr = (byte*)header - base;
    fresh_block->right_ptr = (byte*)right_block - base;
    set_free_bit(fresh_block);
    set_size_in_block(fresh_block, block_size - size);

    header->right_ptr = (byte*)fresh_block - base;
    if(right_block) {
        right_block->left_ptr = (byte*)fresh_block - base;
    }

    set_size_in_block(header, size);
}
u32 local_malloc_allocation_size(void* block) {
    MemoryBlockHeader* header = ((MemoryBlockHeader*)block) - 1;
    return get_size_in_block(header);
}
void* local_max_malloc(LocalMallocState* state) {

    MemoryBlockHeader* it = state->headBlock;
    byte* base = get_local_malloc_base(*state);

    MemoryBlockHeader* max_block = it;
    u32 max_size = 0;
    while(it) {

        u32 size = get_size_in_block(it);
        bool free = extract_free_bit(it->size);
        bool cond = (size > max_size) && free;
        max_size = cond ? size : max_size;
        max_block = cond ? it : max_block;

        it = get_block_ptr(base, it->right_ptr);
    }

    clear_free_bit(max_block);
    return (void*)(max_block + 1);
}


u32 str_hash(const char *str, u32 c) {
    u32 hash = 7;
    for (u32 i = 0; i < c; i++) {
        hash = hash * 31 + str[i];
    }
    return hash;
}

// counts sentinel
u32 str_len(const char *str) {
    const char* it = str;
    while(*it++);
    return it - str;
}
void* str_cpy(void* dst, const char* src) {

    ASSERT(dst && src);
    char* dst_str = (char*)dst;
    for(;;) {
        *dst_str = *src;
        dst_str++;
        if(! (*src++) ) break;
    }

    return dst_str;
}
bool str_cmp(const char *str0, const char *str1) {
    ASSERT(str0 != nullptr && str1 != nullptr);
    for(;;) {
        if(!*str0) break;
        if(!*str1) break;
        if(*str0++ != *str1++) return false;
    }
    
    return *str0 == *str1;
}
i64 i64_power(i64 base, i64 exp) {
    i64 ret = base;
    for(u32 i = 0; i < exp; i++) {
        ret *= base;
    }
    return ret;
}
u32 u64_to_string(char *buffer, u32 buffer_size, u64 n) {

    static_assert(sizeof(char) == 1);
    // buffer too small
    if(buffer_size == 0) return 0;

    // can't divide by 0 early out
    if (n == 0) {
        buffer[0] = '0';
        return 1;
    }

    i64 i = 0;
    u64 m = n;
    while (m != 0) {

        // compiler should optimize division by constant
        m /= 10;
        if(i != buffer_size) {
            // count number of digits to write upto the remaining buffer size
            i++;
        }
        else {
            // otherwise shift (n) one digit at a time
            n /= 10;
        }
    }

    u32 size = i--;
    // stop at i == 0
    for (; i > -1; i--) {
        // write digit
        buffer[i] = (n % 10 + '0');
        n /= 10;
    }
    return size;
}
u32 f32_to_string(char* buff, u32 buff_size, f32 n, u32 precision) {
    
    // char must be 1 byte
    static_assert(sizeof(char) == 1);
    // buffer too small
    if(buff_size == 0) return 0;
    u32 buffer_offset = 0;
    if(n < 0.f) {
        // n negative
        // buffer is atleast 1 byte
        buff[buffer_offset++] = '-';
    }

    n = Abs(n);
    u64 integer_part = (u64)n;
    f32 fractional_part = n - (f32)integer_part;
    // write integer part into remaining buffer
    buffer_offset += u64_to_string(buff+buffer_offset, buff_size-buffer_offset, integer_part);
    
    if(buff_size > buffer_offset && precision != 0) {

        // write fractional part if buffer has enough space
        buff[buffer_offset++] = '.';
        fractional_part *= (f32)i64_power(10, precision);
        u32 fract_size = u64_to_string(buff+buffer_offset, buff_size-buffer_offset, (u64)fractional_part);
        buffer_offset += fract_size;
    }
    return buffer_offset;
}

// expects sentinel
f64 str_to_f64(const char* str) {

    ASSERT(str);
    u64 integer = 0;
    for(;;) {
        if(*str && *str == '.') {
            str++;
            break;
        }
        u64 digit = (u64)(*str++ - '0');
        integer = integer * 10 + digit;
    }
    u64 fract = 0;
    f64 div = 1;
    for(;*str;) {
        u64 digit = (u64)(*str++ - '0');
        fract = fract * 10 + digit;
        div *= 0.1;
    }
    return (f64)integer + ((f64)fract * div);
}

template<typename T>
i64 QPartition(T* arr, i64 low, i64 high) {

    i64 pivot = arr[high];
    i64 index = low - 1;

    for(i64 i = low; i < high; i++) {
        if(arr[i] < pivot) {
            index++;
            auto tmp = arr[index];
            arr[index] = arr[i];
            arr[i] = tmp;
        }
    }

    auto tmp = arr[high];
    arr[high] = arr[index + 1];
    arr[index + 1] = tmp;
    return index + 1;
}
template<typename T>
void Qsort(T* arr, i64 low, i64 high) {

    if(low < high) {
        i64 pivot = QPartition(arr, low, high);
        Qsort(arr, low, pivot-1);
        Qsort(arr, pivot+1, high);
    }
}

byte* print_fn_v(byte* buffer, u32 buffer_size, const char* format, va_list args) {

    if(buffer_size == 0) return buffer;

    auto buffer_end = buffer + buffer_size;
    auto buffer_begin = buffer;
    while(*format && buffer != buffer_end) {
        
        while(*format != '%') format++;
        format++;

        switch(*format++) {
        default:
            Mem<char>(buffer++) = format[-1];
            break;
        case 'c':// char
            {
                char arg = va_arg(args, int);
                Mem<char>(buffer++) = arg;
                break;
            }
        case 'i':// i64
            {
                i64 arg = va_arg(args, i64);
                if(arg < 0) Mem<char>(buffer++) = '-';
                arg = Abs(arg);
                buffer += u64_to_string((char*)buffer, buffer_end - buffer, arg);
                break;
            }
        case 'f': // floating-point
            {
                f32 arg = va_arg(args, f64);
                buffer += f32_to_string((char*)buffer, buffer_end - buffer, arg, 7);
                break;
            }
        case 's':// null-terminated string
            {
                char next_char = *format;
                char* arg = va_arg(args, char*);//s* sized string
                i64 len;
                
                if(next_char == '*') {
                    format++;
                    len = va_arg(args, u64);
                }
                else {
                    len = str_len(arg);
                }

                len = Min(len, buffer_end - buffer);
                ASSERT(len > -1);
                memcpy(buffer, arg, len);
                buffer += len;
                break;
            }
        }
    }
    return buffer;
}

// mutates shared global state
void init_global_print(LinearAllocator memory) {
    ASSERT(memory.base);
    ASSERT(memory.cap > 64);
    ASSERT(memory.top == 0);
    io = memory;
}
void print_out_of_memory() {
    global_print("%s%\n", "out of memory");
    global_io_flush();
    runtime_panic(__FILE__, __LINE__);
}
byte* init_global_state(u32 heapSize, u32 miscMemoySize) {

    auto memory = mmap(nullptr, heapSize + miscMemoySize, PROT_WRITE | PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
    if(!memory) {
        size_t size = sizeof("initial memory request failed\n");
        write(STDOUT_FILENO, "initial memory request failed\n", size);
    }

    init_global_malloc(memory, heapSize, print_out_of_memory);
    auto io_base = LOG(global_malloc_debug(KILO_BYTE));
    init_global_print( make_linear_allocator((byte*)io_base, KILO_BYTE) );

    return (byte*)memory + heapSize;
}
// mutates shared global state
void global_io_flush() {
    write(STDOUT_FILENO, io.base, io.top);
    roll_back_linear_allocator(&io, io.base);
}
// mutates shared global state
void global_print(const char* format ...) {

    va_list args;
    va_start(args, format);
    auto end = print_fn_v(io.base+io.top, linear_allocator_free_size(&io), format, args);
    va_end(args);

    auto top = (byte*)linear_allocator_top(&io);
    if( (end - io.base) >= io.cap) {
        global_io_flush();
        top = (byte*)linear_allocator_top(&io);

        va_start(args, format);
        end = print_fn_v(top, linear_allocator_free_size(&io), format, args);
        va_end(args);

        ASSERT(end != top);
    }
    linear_allocate(&io, end-top);
}
byte* local_print(byte* buffer, u32 buffer_size, const char* format ...) {
    va_list args;
    va_start(args, format);
    auto end = print_fn_v(buffer, buffer_size, format, args);
    va_end(args);
    return end;
}

// uses global_malloc_debug
byte* ReadFileTerminated(const char *fileName, byte *buffer, u32* size_) {

    byte *sourceString = nullptr;
    FILE *file = fopen(fileName, "r");
    if (file) {

        fseek(file, 0, SEEK_END);
        u32 size = ftell(file);
        fseek(file, 0, SEEK_SET);
        *size_ = size + 1;

        if (!buffer) {
            sourceString = (byte *)LOG(global_malloc_debug(size + 1));
        }
        else {
            sourceString = buffer;
        }

        fread(sourceString, size, 1, file);
        sourceString[size] = 0;
        fclose(file);
    }

#ifdef DEBUG_BUILD
    check_all_memory(nullptr);
#endif

    return sourceString;
}
byte* ReadFile(const char* fileName, byte* buffer, u32* size_) {

    byte *sourceString = nullptr;
    FILE *file = fopen(fileName, "r");
    if (file) {

        fseek(file, 0, SEEK_END);
        u32 size = ftell(file);
        fseek(file, 0, SEEK_SET);
        *size_ = size + 1;

        if (!buffer) {
            sourceString = (byte *)LOG(global_malloc_debug(size));
        }
        else {
            sourceString = buffer;
        }

        fread(sourceString, size, 1, file);
        fclose(file);
    }

#ifdef DEBUG_BUILD
    check_all_memory(nullptr);
#endif

    return sourceString;
}


template <typename K, typename V>
struct HashNode {
    K key;
    V value;
};

template <typename K, typename V, u64 (*HASH_FUNCTION)(void *, K), bool (*EQ_FUNCTION)(void *, K, K), K INVALID_KEY>
struct HashTable {
    HashNode<K, V> *array;
    void *user;
    u32 cap;
    u32 occupancy;
    static constexpr f32 loadFactor = 0.5;

    void Init(void *user_) {
        user = user_;
        cap = 2;
        array = (HashNode<K, V> *)LOG(global_malloc_debug(sizeof(HashNode<K, V>) * 2));
        array[0].key = INVALID_KEY;
        array[1].key = INVALID_KEY;

#ifdef DEBUG_BUILD
        check_memory_integrity(array);
#endif
    }
    void CopyInit(HashTable<K, V, HASH_FUNCTION, EQ_FUNCTION, INVALID_KEY> *other) {

        memcpy(this, other, sizeof(*this));
        array = (HashNode<K, V> *)LOG(global_malloc_debug(sizeof(HashNode<K, V>) * other->cap));
        memcpy(array, other->array, sizeof(HashNode<K, V>) * other->cap);

#ifdef DEBUG_BUILD
        check_memory_integrity(array);
#endif
    }
    void Delete(K key) {
        occupancy--;
        array[Find(key)].key = INVALID_KEY;
    }

    u32 Find(K key) {

        u32 index = HASH_FUNCTION(user, key) & (cap - 1);
        for (;;) {

            if (EQ_FUNCTION(user, array[index].key, key)) {
                return index;
            }
            index++;
            if (index == cap) {
                return ~u32(0);
            }
        }
    }


    void Insert(K key, V val) {

        if (cap * loadFactor < (occupancy + 1)) {
            GrowAndReHash();
        }

        occupancy++;
        u32 index = HASH_FUNCTION(user, key) & (cap - 1);
        for (;;) {
            if (EQ_FUNCTION(user, array[index].key, INVALID_KEY)) {
                array[index].key = key;
                array[index].value = val;
                return;
            }

            index++;
            if (index == cap) {
                GrowAndReHash();
                index = HASH_FUNCTION(user, key) & (cap - 1);
            }
        }
    }

    void GrowAndReHash() {

        u32 newCap = cap * 2;
        Begin:

#ifdef DEBUG_BUILD
        check_memory_integrity(array);
#endif

        HashNode<K, V> *tmp = (HashNode<K, V> *)LOG(global_malloc_debug(sizeof(HashNode<K, V>) * newCap));
        for (u32 i = 0; i < newCap; i++) {
            tmp[i].key = INVALID_KEY;
        }
#ifdef DEBUG_BUILD
        check_memory_integrity(array);
        check_memory_integrity(tmp);
#endif

        for (u32 i = 0; i < cap; i++) {

            if (!EQ_FUNCTION(user, array[i].key, INVALID_KEY)) {

                u32 index = HASH_FUNCTION(user, array[i].key) & (newCap - 1);
                for (;;) {
                    if (EQ_FUNCTION(user, tmp[index].key, INVALID_KEY)) {
                        tmp[index].key = array[i].key;
                        tmp[index].value = array[i].value;
                        break;
                    }
                    index++;
                    if (index == newCap) {
                        newCap *= 2;
                        LOG(global_free_debug(tmp));
                        goto Begin;
                    }
                }
            }
        }

        LOG(global_free_debug(array));
        array = tmp;
        cap = newCap;
    }
    void Free() {
        LOG(global_free_debug(array));
        array = nullptr;
    }
};

template <typename T, u64 (*HASH_FUNCTION)(void *, T), bool (*EQ_FUNCTION)(void *, T, T), T INVALID_VALUE, u64 INVALID_HASH>
struct HashSet {

    HashNode<u64, T>* mem;
    u32 cap;
    u32 occupancy;
    static constexpr f32 loadFactor = 0.5;

    void Init() {
        cap = 2;
        mem = (HashNode<u64, T> *)LOG(global_malloc_debug(sizeof(HashNode<u64, T>) * 2));
        occupancy = 0;
        mem[0].key = INVALID_HASH;
        mem[0].value = INVALID_VALUE;
        mem[1].key = INVALID_HASH;
        mem[1].value = INVALID_VALUE;

#ifdef DEBUG_BUILD
        check_memory_integrity(mem);
#endif
    }
    void CopyInit(HashSet<T, HASH_FUNCTION,EQ_FUNCTION,INVALID_VALUE, INVALID_HASH> *other) {

        memcpy(this, other, sizeof(*this));
        mem = (HashNode<u64, T> *)LOG(global_malloc_debug(sizeof(HashNode<u64, T>) * other->cap));
        memcpy(mem, other->mem, sizeof(HashNode<u64, T>) * other->cap);

#ifdef DEBUG_BUILD
        check_memory_integrity(mem);
#endif
    }
    void Delete(void* user, T value) {
        occupancy--;
        auto index = Find(user, value);
        mem[index].value = INVALID_VALUE;
        mem[index].key = INVALID_HASH;
    }
    void Free() {
        LOG(global_free_debug(mem));
    }
    void Clear() {
        occupancy = 0;
        for(u32 i = 0; i < cap; i++) {
            mem[i].value = INVALID_VALUE;
            mem[i].key = INVALID_HASH;
        }
    }
    u32 Find(void* user, T value) {

        auto hash = HASH_FUNCTION(user, value);
        u32 index = hash & (cap - 1);
        for(;;) {
            if(mem[index].key == hash) {
                if(EQ_FUNCTION(user, mem[index].value, value)) return index;
            }

            if(++index == cap) return ~u32(0);
        }
    }

    void Insert(void* user, T value) {
        
        occupancy++;
        if(occupancy > cap * loadFactor) {
            Grow(user);
        }

        auto hash = HASH_FUNCTION(user, value);
        begin:
        u32 index = hash & (cap - 1);

        for(;;) {
            if(mem[index].key == INVALID_HASH) {
                if(EQ_FUNCTION(user, mem[index].value, INVALID_VALUE)) {
                    mem[index].key = hash;
                    mem[index].value = value;

                    return;
                }
            }

            index++;
            if(index == cap) {
                Grow(user);
                goto begin;
            }
        }
    }
    void Grow(void* user) {

        begin:
        auto tmp = (HashNode<u64, T>*)LOG(global_malloc_debug(2 * cap * sizeof(HashNode<u64, T>) ));
        for(u32 i = 0; i < cap*2; i++) {
            tmp[i].value = INVALID_VALUE;
            tmp[i].key = INVALID_HASH;
        }

        for(u32 i = 0; i < cap; i++) {

            if(mem[i].key != INVALID_HASH) {
                if(!EQ_FUNCTION(user, mem[i].value, INVALID_VALUE)) {

                    auto index = mem[i].key & (2*cap - 1);
                    for(;;) {

                        if(tmp[index].key == INVALID_HASH) {
                            if(EQ_FUNCTION(user, tmp[index].value, INVALID_VALUE)) {
                                tmp[index] = mem[i];
                                break;
                            }
                        }

                        index++;
                        if(index == 2*cap) {
                            cap *= 2;
                            LOG(global_free_debug(tmp));
                            goto begin;
                        }
                    }
                }
            }
        }

        LOG(global_free_debug(mem));
        mem = tmp;
        cap *= 2;
    }

    T& operator[](u32 index) {
        return mem[index].value;
    }
};

template<typename T, typename state_t, void* ALLOCATE(state_t* state, u32 size), void FREE(state_t* state, void* mem)>
struct DynamicBufferLocal {
    T *mem;
    u32 cap;
    u32 size;

    void Init(state_t* allocState) {
        mem = (T*)LOG(ALLOCATE(allocState, sizeof(T)));
        cap = 1;
        size = 0;
    }
    void CopyInit(state_t* allocState, DynamicBufferLocal<T,state_t,ALLOCATE,FREE> *buff) {
        SetCapacity(allocState, buff->cap);
        size = buff->size;
        cap = buff->cap;
        memcpy(mem, buff->mem, sizeof(T) * size);
    }

    T &Back() {
        ASSERT(mem != nullptr && size != 0);
        return mem[size - 1];
    }
    T &Front() {
        ASSERT(mem != nullptr && size != 0);
        return mem[0];
    }

    void SetCapacity(state_t* allocState, u32 capacity) {
        mem = (T*)LOG(ALLOCATE(allocState, capacity * sizeof(T)));
        cap = capacity;
        size = 0;
    }
    u32 PushBack(state_t* allocState, T e) {

        if (cap < size + 1) {

            T* tmp = (T*)LOG(ALLOCATE(allocState, sizeof(T) * cap * 2 ));
            ASSERT(cap * 2 >= size + 1);
            memcpy(tmp, mem, size * sizeof(T));
            LOG(FREE(allocState, mem));
            mem = tmp;

            cap *= 2;
        }

        mem[size] = e;
        return size++;
    }
    void Clear() {
        size = 0;
    }
    u32 PopBack() {
        return --size;
    }
    u32 Shrink(state_t* allocState) {
        T* tmp = (T*)LOG(ALLOCATE(allocState, sizeof(T) * (size + 1) * 2 ));
        for (u32 i = 0; i < size * sizeof(T); i++) {
            ((byte *)tmp)[i] = ((byte *)mem)[i];
        }
        LOG(FREE(allocState, mem));
        mem = tmp;
        cap = size;
        return size;
    }

    T &operator[](u32 i) {
        return mem[i];
    }
    void Free(state_t* allocState) {
        LOG(FREE(allocState, mem));
        mem = nullptr;
        cap = 0;
        size = 0;
    }

    void Delete(u32 i) {
        for (; i < size - 1; i++) {
            mem[i] = mem[i + 1];
        }
    }
    void Remove(u32 i) {
        mem[i] = Back();
        PopBack();
    }
};

template <typename T, void* ALLOCATE(u32 size), void FREE(void* mem)>
struct DynamicBufferGlobal {
    T *mem;
    u32 cap;
    u32 size;

    void Init() {
        mem = (T*)LOG(ALLOCATE(sizeof(T)));
        cap = 1;
        size = 0;
    }
    void CopyInit(DynamicBufferGlobal<T,ALLOCATE,FREE> *buff) {
        SetCapacity(buff->cap);
        size = buff->size;
        cap = buff->cap;
        memcpy(mem, buff->mem, sizeof(T) * size);
    }

    T &Back() {
        ASSERT(mem != nullptr && size != 0);
        return mem[size - 1];
    }
    T &Front() {
        ASSERT(mem != nullptr && size != 0);
        return mem[0];
    }

    void SetCapacity(u32 capacity) {
        mem = (T*)LOG(ALLOCATE(capacity * sizeof(T) ));
        cap = capacity;
        size = 0;
    }
    u32 PushBack(T e) {

        if (cap < size + 1) {

#ifdef DEBUG_BUILD
            check_all_memory(nullptr);
#endif
            T* tmp = (T*)LOG(ALLOCATE(sizeof(T) * cap * 2 ));
#ifdef DEBUG_BUILD
            check_all_memory(nullptr);
#endif
            ASSERT(cap * 2 >= size + 1);

            memcpy(tmp, mem, size * sizeof(T));

            LOG(FREE(mem));
            mem = tmp;

            cap *= 2;
        }

        mem[size] = e;
        return size++;
    }
    void Clear() {
        size = 0;
    }
    u32 PopBack() {
        return --size;
    }
    u32 Shrink() {
        T* tmp = (T*)LOG(ALLOCATE(sizeof(T) * (size + 1) * 2 ));
        for (u32 i = 0; i < size * sizeof(T); i++) {
            ((byte *)tmp)[i] = ((byte *)mem)[i];
        }
        LOG(FREE(mem));
        mem = tmp;
        cap = size;
        return size;
    }

    T &operator[](u32 i) {
        return mem[i];
    }
    void Free() {
        LOG(FREE(mem));
        mem = nullptr;
        cap = 0;
        size = 0;
    }

    void Delete(u32 i) {
        for (; i < size - 1; i++) {
            mem[i] = mem[i + 1];
        }
    }
    void Remove(u32 i) {
        mem[i] = Back();
        PopBack();
    }
};


template <typename T, void* ALLOCATE(u32 size), void FREE(void* mem)>
struct StaticBufferGlobal {
    T *memory;
    u32 size;
    void Init(u32 size_) {
        static_assert(std::is_trivially_destructible<T>::value, "T must be trivally destructible");
        static_assert(std::is_trivially_copyable<T>::value, "T must be trivally copyable");

        ASSERT(!memory);
        size = size_;
        memory = (T *)LOG(ALLOCATE(size * sizeof(T)));
    }
    void Free() {
        LOG(FREE(memory));
    }
    void Fill(T *mem, u32 c) {
        ASSERT(c <= size);
        memcpy(memory, mem, sizeof(T) * c);
    }
    T &operator[](u32 i) {
        return memory[i];
    }
};
template <typename T, typename state_t, void* ALLOCATE(state_t* state, u32 size), void FREE(state_t* state, void* mem)>
struct StaticBufferLocal {
    T *memory;
    u32 size;
    void Init(state_t* allocatorState, u32 size_) {
        static_assert(std::is_trivially_destructible<T>::value, "T must be trivally destructible");
        static_assert(std::is_trivially_copyable<T>::value, "T must be trivally copyable");

        ASSERT(!memory);
        size = size_;
        memory = (T *)LOG(ALLOCATE(allocatorState, size * sizeof(T)));
    }
    void Free(state_t* allocatorState) {
        LOG(FREE(allocatorState, memory));
    }
    void Fill(T *mem, u32 c) {
        ASSERT(c <= size);
        memcpy(memory, mem, sizeof(T) * c);
    }
    T &operator[](u32 i) {
        return memory[i];
    }
};

template<typename T> struct SmallStaticBuffer2 {
    u32 memory;
    u32 size;
    void Init(byte* base, T* memory_, u32 size_) {
        static_assert(std::is_trivially_copyable<T>::value, "T must be trivally copyable");
        size = size_;
        memory = (byte*)memory_ - (byte*)base;
    }
    void Fill(T* from, byte* base, u32 size_) {

        ASSERT(size_ <= size);
        T* buffer = (T*)(base + memory);
        memcpy(buffer, from, size_ * sizeof(T));
    }
    T& GetElement(byte* base, u32 index) {
        return *((T*)(base + memory + index * sizeof(T)));
    }
};

template<typename T>
using DynamicBufferDebugMalloc = DynamicBufferGlobal<T, global_malloc_debug,global_free_debug>;
template<typename T>
using DynamicBufferLocalMalloc = DynamicBufferLocal<T, LocalMallocState, local_malloc, local_free>;
template<typename T>
using StaticBufferDebugMalloc = StaticBufferGlobal<T, global_malloc_debug,global_free_debug>;
template<typename T>
using StaticBufferLocalMalloc = StaticBufferLocal<T, LocalMallocState, local_malloc, local_free>;

// ------------------------------------ Front-End BEGIN --------------------------------
// ------------------------------------ Lexer BEGIN ------------------------------------
enum TokenType {
    TOKEN_EOF,
    TOKEN_IDENTIFIER,

    TOKEN_KEYWORD_MAIN,
    TOKEN_KEYWORD_PRINT,
    TOKEN_KEYWORD_IF,
    TOKEN_KEYWORD_ELSE,
    TOKEN_KEYWORD_FOR,
    TOKEN_KEYWORD_DO,
    TOKEN_KEYWORD_RETURN,
    TOKEN_KEYWORD_STRUCT,
    TOKEN_KEYWORD_ARROW,
    TOKEN_KEYWORD_FN,
    TOKEN_KEYWORD_ASSUME,
    TOKEN_KEYWORD_COLD,
    TOKEN_KEYWORD_HOT,
    TOKEN_KEYWORD_AUTO,
    TOKEN_KEYWORD_RESTRICT,
    TOKEN_KEYWORD_VOLATILE,
    TOKEN_KEYWORD_ATOMIC,
    TOKEN_KEYWORD_CONST,

    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_OPEN_BRACKET,
    TOKEN_CLOSE_BRACKET,
    TOKEN_OPEN_BRACES,
    TOKEN_CLOSE_BRACES,

    TOKEN_PLUS_EQUALS,
    TOKEN_MINUS_EQUALS,
    TOKEN_ASTERISTK_EQUALS,
    TOKEN_SLASH_EQUALS,
    TOKEN_AMPERSAND_EQUALS,
    TOKEN_VERTICAL_BAR_EQUALS,
    TOKEN_CIRCUMFLEX_EQUALS,
    TOKEN_TILDE_EQUALS,

    TOKEN_EQUALS_EQUALS,
    TOKEN_EXCLAMATION_EQUALS,
    TOKEN_AMPERSAND_AMPERSAND,
    TOKEN_VERTICAL_BAR_VERTICAL_BAR,
    TOKEN_TILDE,
    TOKEN_AMPERSAND,
    TOKEN_VERTICAL_BAR,
    TOKEN_CIRCUMFLEX,
    TOKEN_NEG,

    TOKEN_ASTERISK,
    TOKEN_SLASH,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_LSHIFT_LSHIFT,
    TOKEN_RSHIFT_RSHIFT,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_LSHIFT_EQUALS,
    TOKEN_RSHIFT_EQUALS,
    
    TOKEN_DOT,
    TOKEN_COMMA,
    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_PLUS_PLUS,
    TOKEN_MINUS_MINUS,
    TOKEN_EQUAL_SIGN,
    TOKEN_SUB_SCRIPT_OPR,
    TOKEN_EXCLAMATION_MARK,

    TOKEN_CHAR_LITERAL,
    TOKEN_STRING_LITERAL,
    TOKEN_NUMBER_LITERAL,
    TOKEN_BOOL_LITERAL,
    TOKEN_NULL_LITERAL,

    TOKEN_UNKNOWN,
    TOKEN_COUNT,
};

struct TokenKeyWord {
    TokenType type;
    const char* name;
};

constexpr TokenKeyWord keyWords[] = {
    {TOKEN_KEYWORD_PRINT,       "print"},
    {TOKEN_KEYWORD_IF,          "if"},
    {TOKEN_KEYWORD_ELSE,        "else"},
    {TOKEN_KEYWORD_FOR,         "for"},
    {TOKEN_KEYWORD_RETURN,      "return"},
    {TOKEN_KEYWORD_STRUCT,      "struct"},
    {TOKEN_KEYWORD_AUTO,        "auto"},
    {TOKEN_KEYWORD_FN,          "fn"},
    {TOKEN_KEYWORD_ARROW,       "->"},
    {TOKEN_KEYWORD_ASSUME,      "assume"},
    {TOKEN_KEYWORD_DO,          "do"},
    {TOKEN_KEYWORD_COLD,        "cold"},
    {TOKEN_KEYWORD_HOT,         "hot"},
    {TOKEN_KEYWORD_RESTRICT,    "restrict"},
    {TOKEN_KEYWORD_VOLATILE,    "volatile"},
    {TOKEN_KEYWORD_ATOMIC,      "atomic"},
    {TOKEN_KEYWORD_CONST,       "const"},
    {TOKEN_KEYWORD_MAIN,        "main"},
};

struct Token {
    char *text;
    u32 lenght;
    TokenType type;
};

struct Tokenizer {
    char *at;
    u32 line = 1;
};
Tokenizer MakeTokenizer(char* source) {
    return {source,1};
}
const char *GetTokenStr(TokenType t) {
    switch (t) {
    case TOKEN_KEYWORD_PRINT:
        return "TOKEN_KEYWORD_PRINT";
    case TOKEN_KEYWORD_IF:
        return "TOKEN_KEYWORD_IF";
    case TOKEN_KEYWORD_ELSE:
        return "TOKEN_KEYWORD_ELSE";
    case TOKEN_KEYWORD_FN:
        return "TOKEN_KEYWORD_FN";
    case TOKEN_KEYWORD_ARROW:
        return "TOKEN_KEYWORD_ARROW";
    case TOKEN_KEYWORD_RETURN:
        return "TOKEN_KEYWORD_RETURN";
    case TOKEN_KEYWORD_MAIN:
        return "TOKEN_KEYWORD_MAIN";
    case TOKEN_IDENTIFIER:
        return "TOKEN_IDENTIFIER";
    case TOKEN_OPEN_PAREN:
        return "TOKEN_OPEN_PAREN";
    case TOKEN_CLOSE_PAREN:
        return "TOKEN_CLOSE_PAREN";
    case TOKEN_OPEN_BRACKET:
        return "TOKEN_OPEN_BRACKET";
    case TOKEN_CLOSE_BRACKET:
        return "TOKEN_CLOSE_BRACKET";
    case TOKEN_OPEN_BRACES:
        return "TOKEN_OPEN_BRACES";
    case TOKEN_CLOSE_BRACES:
        return "TOKEN_CLOSE_BRACES";
    case TOKEN_PLUS_EQUALS:
        return "TOKEN_PLUS_EQUALS";
    case TOKEN_MINUS_EQUALS:
        return "TOKEN_MINUS_EQUALS";
    case TOKEN_ASTERISTK_EQUALS:
        return "TOKEN_ASTERISTK_EQUALS";
    case TOKEN_SLASH_EQUALS:
        return "TOKEN_SLASH_EQUALS";
    case TOKEN_AMPERSAND_EQUALS:
        return "TOKEN_AMPERSAND_EQUALS";
    case TOKEN_VERTICAL_BAR_EQUALS:
        return "TOKEN_VERTICAL_BAR_EQUALS";
    case TOKEN_CIRCUMFLEX_EQUALS:
        return "TOKEN_CIRCUMFLEX_EQUALS";
    case TOKEN_LSHIFT_EQUALS:
        return "TOKEN_LSHIFT_EQUALS";
    case TOKEN_RSHIFT_EQUALS:
        return "TOKEN_RSHIFT_EQUALS";
    case TOKEN_SUB_SCRIPT_OPR:
        return "TOKEN_SUB_SCRIPT_OPR";
    case TOKEN_EXCLAMATION_EQUALS:
        return "TOKEN_EXCLAMATION_EQUALS";
    case TOKEN_TILDE_EQUALS:
        return "TOKEN_TILDE_EQUALS";
    case TOKEN_CIRCUMFLEX:
        return "TOKEN_CIRCUMFLEX";
    case TOKEN_DOT:
        return "TOKEN_DOT";
    case TOKEN_COMMA:
        return "TOKEN_COMA";
    case TOKEN_COLON:
        return "TOKEN_COLON";
    case TOKEN_SEMICOLON:
        return "TOKEN_SEMICOLON";
    case TOKEN_ASTERISK:
        return "TOKEN_ASTERISK";
    case TOKEN_AMPERSAND:
        return "TOKEN_AMPERSAND";
    case TOKEN_TILDE:
        return "TOKEN_TILDE";
    case TOKEN_EXCLAMATION_MARK:
        return "TOKEN_EXLAMATION_MARK";
    case TOKEN_PLUS:
        return "TOKEN_PLUS";
    case TOKEN_PLUS_PLUS:
        return "TOKEN_PLUS_PLUS";
    case TOKEN_MINUS:
        return "TOKEN_MINUS";
    case TOKEN_MINUS_MINUS:
        return "TOKEN_MINUS_MINUS";
    case TOKEN_SLASH:
        return "TOKEN_SLASH";
    case TOKEN_EQUAL_SIGN:
        return "TOKEN_EQUAL_SIGN";
    case TOKEN_LSHIFT:
        return "TOKEN_LSHIFT";
    case TOKEN_RSHIFT:
        return "TOKEN_RSHIFT";
    case TOKEN_LSHIFT_LSHIFT:
        return "TOKEN_LSHIFT_LSHIFT";
    case TOKEN_RSHIFT_RSHIFT:
        return "TOKEN_RSHIFT_RSHIFT";
    case TOKEN_STRING_LITERAL:
        return "TOKEN_STRING_LITERAL";
    case TOKEN_NUMBER_LITERAL:
        return "TOKEN_NUMBER_LITERAL";
    case TOKEN_UNKNOWN:
        return "TOKEN_UNKNOWN";
    case TOKEN_EOF:
        return "TOKEN_EOF";
    case TOKEN_AMPERSAND_AMPERSAND:
        return "TOKEN_AMPERSAND_AMPERSAND";
    case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
        return "TOKEN_VERTICAL_BAR_VERTICAL_BAR";
    case TOKEN_EQUALS_EQUALS:
        return "TOKEN_EQUALS_EQUALS";
    case TOKEN_BOOL_LITERAL:
        return "TOKEN_BOOL_LITERAL";
    case TOKEN_NULL_LITERAL:
        return "TOKEN_NULL_LITERAL";
    case TOKEN_KEYWORD_ASSUME:
        return "TOKEN_KEYWORD_ASSUME";
    case TOKEN_KEYWORD_DO:
        return "TOKEN_KEYWORD_DO";
    case TOKEN_KEYWORD_COLD:
        return "TOKEN_KEYWORD_COLD";
    case TOKEN_KEYWORD_HOT:
        return "TOKEN_KEYWORD_HOT";
    case TOKEN_KEYWORD_RESTRICT:
        return "TOKEN_KEYWORD_RESTRICT";
    case TOKEN_KEYWORD_VOLATILE:
        return "TOKEN_KEYWORD_VOLATILE";
    case TOKEN_KEYWORD_ATOMIC:
        return "TOKEN_KEYWORD_ATOMIC";
    default:
        return nullptr;
        break;
    }
}
void PrintToken(Token t) {
    global_print("%s*", t.text, t.lenght);
}

i64 GetI64(Token t) {
    char str[t.lenght + 1]{0};
    for (u32 i = 0; i < t.lenght; i++) {
        str[i] = t.text[i];
    }

    return atoi(str);
}
u64 GetU64(Token t) {
    char str[t.lenght + 1]{0};
    for (u32 i = 0; i < t.lenght; i++) {
        str[i] = t.text[i];
    }
    return strtoul(t.text, nullptr, 10);
}

f64 GetF64(Token t) {
    char str[t.lenght + 1]{0};
    for (u32 i = 0; i < t.lenght; i++) {
        str[i] = t.text[i];
    }
    return atof(str);
}

bool IsWhiteSpace(char c) {
    return (c == ' ') ||
           (c == '\n') ||
           (c == '\t') ||
           (c == '\r');
}

u32 GetLineNumber(char *source, char *at) {
    u32 c = 1;
    while (source != at) {
        if (*source == '\n')
            c++;
        source++;
    }
    return c;
}
bool IsAlpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
bool IsNumeric(char c) {
    return (c >= '0' && c <= '9');
}
bool TokenEquals(Token t, const char* match) {
    
    auto ptr = t.text;
    auto end = t.text + t.lenght;
    for(;;) {
        if(!*match) return ptr == end;
        if(ptr == end) return *match == 0;
        if(*ptr++ != *match++) return false;
    }
}
bool TokensEquals(Token t0, Token t1) {

    if (t0.text == t1.text) {
        return true;
    }
    if (t0.lenght != t1.lenght) {
        return false;
    }

    auto ptr = t0.text;
    auto end = t0.text + t0.lenght;
    for(;ptr != end;) {
        if(*ptr++ != *t1.text++) return false;
    }

    return true;
}
void EatMacors(Tokenizer *tokenizer) {

    while (tokenizer->at[0]) {

        tokenizer->line += tokenizer->at[0] == '\n';
        if (tokenizer->at[0] == '\\' && tokenizer->at[1] == '\\') {
            tokenizer->at++;
        }
        else if (tokenizer->at[0] == '\n') {
        }
        else {
            break;
        }
        ++tokenizer->at;
    }
}
void EatWhiteSpace(Tokenizer *tokenizer) {

    while (tokenizer->at[0]) {
        if (IsWhiteSpace(tokenizer->at[0])) {
            tokenizer->line += (tokenizer->at[0] == '\n');
            tokenizer->at++;
        } else if (tokenizer->at[0] == '/' && tokenizer->at[1] == '/') {
            tokenizer->at += 2;
            while (tokenizer->at[0] && !(tokenizer->at[0] == '\n'))
                ++tokenizer->at;
            tokenizer->line++;
            tokenizer->at += 1;
        } else if (tokenizer->at[0] == '/' && tokenizer->at[1] == '*') {
            tokenizer->at += 2;
            while (tokenizer->at[0] && !(tokenizer->at[0] == '*' && tokenizer->at[1] == '/'))
                tokenizer->line += (tokenizer->at++)[0] == '\n';
            tokenizer->at += 2;
        } else if (tokenizer->at[0] == '#') {
            EatMacors(tokenizer);
        } else {
            break;
        }
    }
}

Token GetToken(Tokenizer *tokenizer) {

    EatWhiteSpace(tokenizer);

    Token token{};
    token.lenght = 1;

    char c = tokenizer->at[0];
    token.text = tokenizer->at++;

    switch (c) {
    case '\0':
        token.type = TOKEN_EOF;
        break;
    case '(':
        token.type = TOKEN_OPEN_PAREN;
        break;
    case ')':
        token.type = TOKEN_CLOSE_PAREN;
        break;
    case '[':
        token.type = TOKEN_OPEN_BRACKET;
        break;
    case ']':
        token.type = TOKEN_CLOSE_BRACKET;
        break;
    case '{':
        token.type = TOKEN_OPEN_BRACES;
        break;
    case '}':
        token.type = TOKEN_CLOSE_BRACES;
        break;
    case ',':
        token.type = TOKEN_COMMA;
        break;
    case ':':
        token.type = TOKEN_COLON;
        break;
    case ';':
        token.type = TOKEN_SEMICOLON;
        break;
    case '=':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_EQUALS_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else {
            token.type = TOKEN_EQUAL_SIGN;
        }
        break;
    case '*':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_ASTERISTK_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else {
            token.type = TOKEN_ASTERISK;
        }
        break;
    case '&':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_AMPERSAND_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else if (tokenizer->at[0] == '&') {
            token.type = TOKEN_AMPERSAND_AMPERSAND;
            token.lenght++;
            tokenizer->at++;
        } else {
            token.type = TOKEN_AMPERSAND;
        }
        break;
    case '!':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_EXCLAMATION_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else {
            token.type = TOKEN_EXCLAMATION_MARK;
        }
        break;
    case '+':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_PLUS_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else if (tokenizer->at[0] == '+') {
            token.lenght++;
            tokenizer->at++;
            token.type = TOKEN_PLUS_PLUS;
        } else {
            token.type = TOKEN_PLUS;
        }
        break;
    case '~':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_TILDE_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else {
            token.type = TOKEN_TILDE;
        }
        break;
    case '-':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_MINUS_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else if (tokenizer->at[0] == '>') {
            token.lenght++;
            tokenizer->at++;
            token.type = TOKEN_KEYWORD_ARROW;
        } else if (tokenizer->at[0] == '-') {
            token.lenght++;
            tokenizer->at++;
            token.type = TOKEN_MINUS_MINUS;
        } else {
            token.type = TOKEN_MINUS;
        }
        break;
    case '/':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_SLASH_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else {
            token.type = TOKEN_SLASH;
        }
        break;
    case '>':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_RSHIFT_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else if (tokenizer->at[0] == '>') {
            token.type = TOKEN_RSHIFT_RSHIFT;
            token.lenght++;
            tokenizer->at++;
        } else {
            token.type = TOKEN_RSHIFT;
        }
        break;
    case '<':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_LSHIFT_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else if (tokenizer->at[0] == '<') {
            token.type = TOKEN_LSHIFT_LSHIFT;
            token.lenght++;
            tokenizer->at++;
        } else {
            token.type = TOKEN_LSHIFT;
        }
        break;
    case '|':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_VERTICAL_BAR_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else if (tokenizer->at[0] == '|') {
            token.type = TOKEN_VERTICAL_BAR_VERTICAL_BAR;
            token.lenght++;
            tokenizer->at++;
        } else {
            token.type = TOKEN_VERTICAL_BAR;
        }
        break;
    case '^':
        if (tokenizer->at[0] == '=') {
            token.type = TOKEN_CIRCUMFLEX_EQUALS;
            token.lenght++;
            tokenizer->at++;
        } else {
            token.type = TOKEN_CIRCUMFLEX;
        }
        break;
    case '\'': {
        token.type = TOKEN_CHAR_LITERAL;
        token.text = tokenizer->at;
        token.lenght = 0;

        if (token.text[0] == '\\' && token.text[1] == 'n') {
            tokenizer->at += 2;
            token.lenght += 2;
        } else {
            tokenizer->at++;
            token.lenght++;
        }

        if (tokenizer->at[0] != '\'') {
            global_print("%s%i\n", "ERROR: ilformed character literal at line: ", tokenizer->line);
            token.type = TOKEN_UNKNOWN;
        }
        tokenizer->at++;
    } break;

    case '"': {
        token.text = tokenizer->at;
        token.type = TOKEN_STRING_LITERAL;

        while (tokenizer->at[0] && tokenizer->at[0] != '"') {

            if (tokenizer->at[0] == '\\' && tokenizer->at[1]) {
                ++tokenizer->at;
            }
            ++tokenizer->at;
        }
        token.lenght = tokenizer->at - token.text;
        if (tokenizer->at[0] == '"')
            ++tokenizer->at;
        break;
    }

    case '.':
        if (!IsNumeric(tokenizer->at[0])) {
            token.type = TOKEN_DOT;
            break;
        }
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': {
        bool e = true;
        bool point = (c == '.');
        token.type = TOKEN_NUMBER_LITERAL;
        while (IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '.') {

            if (tokenizer->at[0] == '.' && !point) {
                point = true;
            } else if (tokenizer->at[0] == '.' && point && e) {
                e = false;
                global_print("%s%i\n", "ERROR: ilformed number literal at line: ", tokenizer->line);
                token.type = TOKEN_UNKNOWN;
            }

            tokenizer->at++;
        }
        if (tokenizer->at[0] == 'f') {
            tokenizer->at++;

            if (IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '.') {
                global_print("%s%i\n", "ERROR: ilformed number literal at line: ", tokenizer->line);
                token.type = TOKEN_UNKNOWN;
            }
        }

        token.lenght = tokenizer->at - token.text;
        break;
    }

    default:
        if (IsAlpha(c)) {
            token.type = TOKEN_IDENTIFIER;
            while (IsAlpha(tokenizer->at[0]) || IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '_')
                tokenizer->at++;

            token.lenght = tokenizer->at - token.text;
            for(auto i = keyWords; i < 1[&keyWords]; i++) {
                if(TokenEquals(token, i->name)) {
                    token.type = i->type;
                }
            }
            if (TokenEquals(token, "true") || TokenEquals(token, "false")) {
                token.type = TOKEN_BOOL_LITERAL;
            }
            else if (TokenEquals(token, "null")) {
                token.type = TOKEN_NULL_LITERAL;
            }
        }
        else {
            token.type = TOKEN_UNKNOWN;
            global_print("%s%i%\n", "ERROR: unknown token at line: ", tokenizer->line);
        }
        break;
    }

    return token;
}

// --------------------------------------- Lexer END --------------------------------

enum TypeName {
    TYPE_NON,

    TYPE_STRUCTURE,
    TYPE_STRUCTURE_MEMBER,

    TYPE_PRIMARY_AUTO,
    TYPE_PRIMARY_VOID,
    TYPE_PRIMARY_BOOL,
    TYPE_PRIMARY_CHAR,
    TYPE_PRIMARY_FN,
    TYPE_PRIMARY_NATIVE_FN,

    TYPE_PRIMARY_INT8,
    TYPE_PRIMARY_INT16,
    TYPE_PRIMARY_INT32,
    TYPE_PRIMARY_INT64,

    TYPE_PRIMARY_UINT8,
    TYPE_PRIMARY_UINT16,
    TYPE_PRIMARY_UINT32,
    TYPE_PRIMARY_UINT64,

    TYPE_PRIMARY_F32,
    TYPE_PRIMARY_F64,

    TYPE_MODIFIER_RESTRICTED_POINTER,
    TYPE_MODIFIER_POINTER,
    TYPE_MODIFIER_ARRAY,
    TYPE_MODIFIER_CONST,
    TYPE_MODIFIER_VOLATILE,
    TYPE_MODIFIER_ATOMIC,

    TYPE_COUNT,
};
struct StructName {
    Token name;
    u32 ptr;
};
struct TypeExpr {
    u32 index;
};
enum ExprType {
    EXPRESSION_NULL,

    EXPRESSION_IMMEDIATE,
    EXPRESSION_LITERAL,
    EXPRESSION_UNARY,
    EXPRESSION_BINARY,

    EXPRESSION_VARIABLE,
    EXPRESSION_VARIABLE_ASSIGNMENT,
    EXPRESSION_MEMORY_LOAD,
    EXPRESSION_MEMORY_STORE,

    EXPRESSION_ADDRESS_OF,

    EXPRESSION_GET,
    EXPRESSION_PEEL_TYPE,
    EXPRESSION_MEM_COPY,

    EXPRESSION_CALL,
    EXPRESSION_CONVERSION,
    
    EXPRESSION_MISC_COLD,
    EXPRESSION_MISC_HOT,

    EXPRESSION_COUNT,
};

struct Value {
    byte mem[8];
    u8 type;
};
struct Expr {
    u32 index;
};
struct Symbol {
    Token name;
    TypeExpr type;
    u32 scope;
    u32 extra;
};
struct Variable {
    Token name;
    TypeExpr type;
    u32 scope;
    u32 extra;
};
struct LiteralExpr : Expr {
    Token literal;
};
struct ImmediateExpr : Expr {
    Value v;
};
struct GroupExpr : Expr {
    Expr expr;
};
struct UnaryExpr : Expr {
    TokenType opr;
    Expr primaryExpr;
};
struct BinaryExpr : Expr {
    Expr left;
    Expr right;
    TokenType opr;
};
struct PeelTypeExpr : Expr {
    Expr expr;
};
struct DotExpr : Expr {
    Token name;
    Expr prev;
    u32 offset;
};
struct AddressOf : Expr {
    Expr expr;
};
struct VariableExpr : Expr {
    Variable var;
};
struct VariableAssignmentExpr : Expr {
    Variable var;
    Expr value;
};
struct MemoryLoadExpr : Expr {
    Expr address;
};
struct MemoryStoreExpr : Expr {
    Expr address;
    Expr value;
};
struct MemCopyExpr : Expr {
    Expr src;
    Expr dst;
    u32 size;
};
struct CallExpr : Expr {
    Expr calleeExpr;
    Expr args;
};
struct ConversionExpr : Expr {
    Expr from;
    TypeExpr type;
};
struct FunctionExpr {
    Token name;
    u32 paramCount = 0;
    u32 params;
    TypeExpr ret_t;
};
struct MiscExpr : Expr {
    
};
enum StatementType {
    STATEMENT_NON,

    STATEMENT_ENTRY_POINT,
    STATEMENT_FUNCTION_ENTRY,
    STATEMENT_FUNCTION_EXIT,

    STATEMENT_EXPRESSION,
    STATEMENT_PRINT,
    
    STATEMENT_BRANCH,
    STATEMENT_FOR_LOOP,

    STATEMENT_REPEAT,
    STATEMENT_RET_ASSIGN,

    STATEMENT_VAR_DECL,
    STATEMENT_ABORT,
    STATEMENT_SKIP,
    STATEMENT_ASSUME,

    STATEMENT_COUNT,
};

struct Stmt {
    u32 index;
};
struct ExprStmt : Stmt {
    Expr expr;
};
struct PrintStmt : Stmt {
    u32 exprCount;
    Expr exprString[0];// inline buffer
};
struct FunctionStmt : Stmt {
    u32 functionPtr;
};
struct SkipStmt : Stmt {
    Stmt target;
    Stmt end;
};
struct BranchStmt : Stmt {
    Expr cond;
    Stmt thenBranch;
    Stmt elseBranch;
    Stmt end;
};
struct ForStmt : Stmt {
    Expr cond;
    Stmt init;
    Stmt inc;
    Stmt body;
    Stmt end;
};
struct RetAssignStmt : Stmt {
    Expr retExpr;
};
struct AssumeStmt : Stmt {
    u32 exprCount;
    Expr exprString[0]; // inline buffer
};

u64 HashSSA(void *compiler, u32 vReg);
bool EQSSAHelper(void *compiler, u32 k0, u32 k1);
bool EQSSA(void *compiler, u32 k0, u32 k1, u32 *visited);

enum SSAEnums : u32 {
    SSA_NONE,
    SSA_ANY,
    
    // ssa ops
    SSA_MEMORY_LOAD = EXPRESSION_MEMORY_LOAD,
    SSA_MEMORY_STORE = EXPRESSION_MEMORY_STORE,
    SSA_CMP_EQ = TOKEN_EQUALS_EQUALS,
    SSA_CMP_NOT_EQ = TOKEN_EXCLAMATION_EQUALS,
    SSA_CMP_LESS_THAN = TOKEN_LSHIFT,      // <
    SSA_CMP_BIGGER_THAN = TOKEN_RSHIFT,    // >
    SSA_CMP_LESS_THAN_OR_EQ = TOKEN_LSHIFT_EQUALS,
    SSA_CMP_BIGGER_THAN_OR_EQ = TOKEN_RSHIFT_EQUALS,
    SSA_LOGICAL_AND = TOKEN_AMPERSAND_AMPERSAND,
    SSA_LOGICAL_OR,
    SSA_BITWISE_NEG,
    SSA_BITWISE_AND,
    SSA_BITWISE_OR,
    SSA_BITWISE_XOR,
    SSA_MINUS,
    SSA_MUL,
    SSA_DIV,
    SSA_ADD,
    SSA_SUB,
    SSA_LEFT_BIT_SHIFT,
    SSA_RIGHT_BIT_SHIFT,

    SSA_LOGICAL_NEG = TOKEN_EXCLAMATION_MARK,
    SSA_CONSTANT = EXPRESSION_IMMEDIATE,
    SSA_CALL = EXPRESSION_CALL,
    SSA_CONVERT = EXPRESSION_CONVERSION,
    SSA_PHI_NODE = TOKEN_COUNT,
    SSA_FN_PARAMETER,
    SSA_UN_INIT,
    SSA_FUNCTION,
    SSA_ALLOCA,
    SSA_COPY,

    // control flow
    JMP,
    BRANCH,
    RET,
    EXIT,
    
    // complex ops
    SSA_MAX,
    SSA_MIN,
    SSA_ABS,
    SSA_SELECT,
    SSA_POWER,
    SSA_SIN,
    SSA_COS,
    SSA_TAN,
    SSA_COT,
    SSA_SQRT,
    SSA_LOG,
    
    // built-in functions
    //MALLOC,
    //FREE
    //MEMCPY,
    //MEMSET,
    //MEMMOVE,

    // misc
    CONSTRAINT,
    RANGE_EXPRESSION,
    UNION,
    INTERSECTION,
    WIDEN,
    NARROW,
    POSITIVE_INF,
    NEGATIVE_INF,
    
    // info
    CALL_INFO,
    STORE_INFO,
    LOAD_INFO,
    CONSTRAINT_INFO,
    SCEV_INFO,

    SSA_ENUM_COUNT,
};
struct SymbolicRange {
    u32 lower;
    u32 upper;
};
struct ScalarRecurrence {
    u32 begin;
    u32 opr;
    u32 step;
    u32 loop;
};

struct SSADefinition {
    u16 opr;
    u16 value;
    u32 operand0;
    u32 operand1; // load baseIndex
    u32 nextDef;
    u32 prevDef;
    u32 block;
    u32 extraPtr = ~u32(0);
    TypeExpr type;
};
struct SSABranch {
    u32 conditionDef;
    u32 thenBlock;
    u32 elseBlock;
};
struct SSAJmp {
    u32 targetBlock;
};
struct SSARet {
    u32 retDef;
};
struct NextBlock {
    u32 opr;
    union {
        SSABranch branch;
        SSAJmp jmp;
        SSARet ret;
    };
};
struct VarDefintion {
    Token name;
    u32 defPtr;
};

template<typename T> struct InlineBuffer {
    u32 elemCount;
    T buffer[0];
};
struct SSACallInfo {
    u32 infoType;
    u32 base;
    u32 memoryDefCount;
    u32 memoryUseCount;
    u32 memoryDefs[0];  // inline buffer u32
    byte memoryUses[0]; // RangeWithBase inline buffer
};
struct SSALoadInfo {
    u32 infoType;
    u32 memoryUseCount;
    u32 memoryUses[0]; // inline buffer u32
};
struct SSAStoreInfo {
    u32 infoType;
    u32 memoryDefCount;
    u32 memoryDefs[0]; // inline buffer u32
};
struct SSAConstraintInfo {
    u32 infoType;
    u32 constriantCount;
    u32 constraint;
};
struct CFGEdges {
    u32 edges;
    u32 edgeCount;
};
enum AliasType {
    ALIAS_TYPE_NONE,

    ALIAS_TYPE_MIGHT_ALIAS,
    ALIAS_TYPE_MUST_ALIAS,
    ALIAS_TYPE_NO_ALIAS,

    ALIAS_TYPE_COUNT,
};
struct AliasEdgeList {
    AliasEdgeList *next;
    AliasType type;
    u32 offset;
    u32 index;
};
struct AliasVertex {
    AliasEdgeList* aliases;
    u32 basePtr;
};
struct MemoryAliasGraph {
    AliasVertex* vertices;
    u32 vertexCount;
};

enum SSAMemoryEnum {
    MEMORY_NONE,

    MEMORY_UNKOWN,
    MEMORY_PHI,
    MEMORY_DEF,

    MEMORY_TYPE_COUNT,
};
typedef CFGEdges SmallStaticBuffer;
struct SSAMemoryDef {
    SSAMemoryEnum type;
    SmallStaticBuffer predecessors;
    SmallStaticBuffer successors;
    SmallStaticBuffer addressRanges;
    u32 basePtrDef;
    u32 ssaDef;
    u32 block;
    u32 next;
    u32 prev;
    u32 name;
};

struct SSABasicBlock {

    DynamicBufferDebugMalloc<HashNode<u32,u32>> memoryPtrs;
    HashSet<u32, HashSSA, EQSSAHelper, ~u32(0), ~u32(0)> values;

    u32 firstDef = 0; // linked list head*
    u32 lastDef = 0;
    u32 firstMem = 0;
    u32 lastMem = 0;
    
    CFGEdges predecessors;
    CFGEdges successors;

    u32 phis;
    u32 phiCount;

    u32 name;
    NextBlock nextBlock;
    
    u32 memoryPhis;
    u32 memoryPhiCount;

    u32 lowerSlotIndex;
    u32 upperSlotIndex;

    f32 hotness;
    bool incomplete;
};

struct SSABlockInfo {
    f32 hotness;
};
struct LoopInfo {
    SSABasicBlock *headerBlock;
    SSABasicBlock *preHeader;
    SSABasicBlock *exitBlock;
   
    u32 bodyCount;
    SSABasicBlock* bodyBlocks[0];
};
struct BranchAnalysis {
    SSABasicBlock *condBlock;
    SSABasicBlock *thenBlock;
    SSABasicBlock *elseBlock;
    SSABasicBlock *exitBlock;
};
struct ParameterInfo {
    u32 ssaDef;
    StaticBufferDebugMalloc<u32> observed;
    StaticBufferDebugMalloc<u32> clobbered;
    bool used;
};
struct IndirectAddress {
    SmallStaticBuffer indirects; // IndirectMemory*
    SmallStaticBuffer storedRanges; // IntegerRange
    u32 baseAaddress;
    u32 storedAddress;
};
struct SSAFunction {
    SSABasicBlock *entry;
    u32 maxBlockName;
    u32 maxSSAName;

    CFGEdges outBlocks;

    Expr funcExpr;
    Token name;
    StaticBufferDebugMalloc<ParameterInfo> params;
    MemoryAliasGraph graph;
};


struct IncompletePhi {
    SSABasicBlock *block;
    Token symbol;
    TypeExpr symbolType;
    u32 phi;
};
struct IncompleteMemoryPhi {
    SSABasicBlock* block;
    u32 basePtrDef;
    u32 phi;
};
struct IncompleteMemoryOp {
    u32 ssaDef;
};

enum MachineIROp : u32 {
    MIR_NONE,

    MIR_READ,
    MIR_WRITE,
    MIR_MEM,
    MIR_VREG, // virtual
    MIR_PREG, // physical
    MIR_BIT,
    MIR_CONSTANT,

    MIR_MUL,
    MIR_SDIV,
    MIR_UDIV,
    MIR_SREM,
    MIR_UREM,

    MIR_ADD,
    MIR_SUB,

    MIR_AND,
    MIR_OR,
    MIR_XOR,
    MIR_NEG,
    MIR_SHL,
    MIR_SHR,
    MIR_SAR,
    MIR_SAL,
    
    MIR_ROR,// ?
    MIR_ROL,

    MIR_SETBIT,

    MIR_CARRY_FLAG,
    MIR_PARITY_FLAG,
    MIR_ADJUT_FLAG,
    MIR_ZERO_FLAG,
    MIR_SIGN_FLAG,
    MIR_OVERFLOW_FLAG,

    MIR_CALL,
    MIR_JMP,
    MIR_CJMP,
    MIR_RET,
    MIR_BLOCK,
    MIR_FALLTHROUGH,

    MIR_COUNT,
};
struct InstMIR {
    u32 chainCount;
    u32 order;
    void* opChains;
    InstMIR* prev;
    InstMIR* next;
};
struct OpMIR {
    u16 opr;
    u16 operandCount;
    TypeExpr type;
    u32 name;
    void* operands;
};
struct BlockMIR {
    u16 type;
    u16 name;
    u32 alignment;
    u32 order;
    InstMIR* firstInst;
    InstMIR* lastInst;
};
struct FunctionMIR {
    BlockMIR* entry;
    MemoryPool<sizeof(BlockMIR)> blockPool;
    MemoryPool<sizeof(InstMIR)> instPool;
    MemoryPool<sizeof(OpMIR)> opPool;
    u32 maxBlockName;
    u32 maxOpName;
};
struct ProgramMIR {
    u32 fnCount;
    FunctionMIR* fns;
};

enum IntegerLimitsMin {

    I8_MIN = i8(u8(1) << 7),
    I16_MIN = i16(u16(1) << 15),
    I32_MIN = i32(u32(1) << 31),
    I64_MIN = i64(u64(1) << 63),

    U8_MIN = 0,
    U16_MIN = 0,
    U32_MIN = 0,
    U64_MIN = 0,
};
enum IntegerLimitsMax {
    I8_MAX = (u8(1) << 7) - 1,
    I16_MAX = (u16(1) << 15) - 1,
    I32_MAX = (u32(1) << 31) - 1,
    I64_MAX = (u64(1) << 63) - 1,

    U8_MAX = ~u8(0),
    U16_MAX = ~u16(0),
    U32_MAX = ~u32(0),
    U64_MAX = ~u64(0),
};

struct Parser {
    DynamicBufferDebugMalloc<Token> tokenBuffer;
    DynamicBufferDebugMalloc<StructName> structs;

    Tokenizer tokenizer;
    char *source;

    u32 scope;
    bool error;
};

u64 HashToken(void *user, Token t) {
    return str_hash(t.text, t.lenght);
}
bool EQToken(void *user, Token t0, Token t1) {
    return TokensEquals(t0, t1);
}

void InsertAliasEdge(MemoryAliasGraph *graph, u32 i, u32 k, AliasType type, u32 offset) {

    if(i == k) return;

    u32 max = Max(i,k)+1;

    if(graph->vertexCount < max) {

        AliasVertex* tmp = (AliasVertex*)LOG(global_malloc_debug(max * sizeof(AliasVertex)));
        memcpy(tmp, graph->vertices, graph->vertexCount * sizeof(AliasVertex));
        memset(tmp+graph->vertexCount, 0, (max - graph->vertexCount) * sizeof(AliasVertex));
        LOG(global_free_debug(graph->vertices));
        graph->vertices = tmp;
        graph->vertexCount = max;
    }

    auto v = graph->vertices[i].aliases;
    for(; v ; v = v->next );
    if(!v) {
        auto adj = (AliasEdgeList*)LOG(global_malloc_debug(sizeof(AliasEdgeList)));
        *adj = {};
        adj->index = i;
        adj->type = type;
        if(type == ALIAS_TYPE_MUST_ALIAS) {
            adj->offset = offset;
        }
        adj->next = graph->vertices[k].aliases;
        graph->vertices[i].aliases = adj;
    }

    v = graph->vertices[k].aliases;
    for(; v ; v = v->next );

    if(!v) {
        auto adj = (AliasEdgeList*)LOG(global_malloc_debug(sizeof(AliasEdgeList)));
        *adj = {};
        adj->index = k;
        adj->type = type;
        if(type == ALIAS_TYPE_MUST_ALIAS) {
            adj->offset = offset;
        }
        adj->next = graph->vertices[i].aliases;
        graph->vertices[k].aliases = adj;
    }
}

void InsertMemoryAliasVertex(MemoryAliasGraph *graph, u32 i) {

    if (graph->vertexCount < i + 1) {

        AliasVertex* tmp = (AliasVertex*)LOG(global_malloc_debug((i + 1) * sizeof(AliasVertex)));
        memcpy(tmp, graph->vertices, sizeof(AliasVertex) * graph->vertexCount);

        LOG(global_free_debug(graph->vertices));
        graph->vertices = tmp;
        graph->vertexCount = i + 1;
        tmp[i] = {};
    }
}

// ------------------ Target descriptions ----------------
enum RegisterProperty : bit_mask64 {
    REGISTER_NONE,

    REGISTER_SINGLE_FLOAT_OPS   = 1 << 0,
    REGISTER_DOUBLE_FLOAT_OPS   = 1 << 1,
    REGISTER_INTEGER_OPS        = 1 << 2,
    REGISTER_SCALAR_OPS         = 1 << 3,
    REGISTER_VECTOR_OPS         = 1 << 4,
    REGISTER_FLAG               = 1 << 5,

    REGISTER_COUNT,
};



struct ABI {
    u32 redZone;
    u32 alignment;
    u32 returnRegCount;
    u32 paramRegCount;
    u32 calleeSavedRegCount;
    u32 callerSavedRegCount;

    u32* returnRegs;
    u32* paramRegs;
    u32* callerSavedRegs;
    u32* calleeSavedRegs;
};
struct RegisterDescriptor {
    const char* str;
    u32* clobbersOnWrite;
    bit_mask64 properties;
    u32 clobberCount;
    u32 width; // bit width
};
enum SSAPatternConstraintEnum : u32 {

    CONSTRAINT_SINGLE_USE,
    CONSTRAINT_NON_COMMUMATIVE,
    CONSTRAINT_COUNT,
};
struct SSAPatternConstraint {

};
struct SSAPattern {
    u16 implements;
    u16 name;
    u16 constraintCount;
    u16 operandCount;
    SSAPattern** operands;
    SSAPatternConstraint* constraints;
    OpMIR* link;
};
struct MIRMacro {
    u32 instCount;
    u32 patternCount;
    i64 cost;
    InstMIR** insts;
    SSAPattern** patterns;
};
struct VirtualRegisterMIR {
    u32 name;
    u32 regClass;
};
struct InstDescriptor {
    MIRMacro macro;
    const char* str;
};
struct ArhitectureDescriptor {
    const char* arhitecture;
    u32 wordSize;
    u32 registerCount;
    u32 instCount;
    RegisterDescriptor* regDescriptorTable;
    InstDescriptor* instDescriptorTable;

    ABI abi;
};

enum MachineProperty : bit_mask64 {
    MACHINE_NONE,
    
    MACHINE_PIPELINED               = 1 << 0,
    MACHINE_WIDE_ISSUE_SUPER_SCALAR = 1 << 1,
    MACHINE_OUT_OF_ORDER            = 1 << 2,
    MACHINE_REGISTER_RENAME         = 1 << 3,
    MACHINE_BRANCH_PREDICT          = 1 << 4,
    MACHINE_SPECULATIVE_EXEC        = 1 << 5,

    MACHINE_COUNT,
};
enum FuncUnitOp : bit_mask64 {
    FUN_OP_NONE,
    
    FUN_OP_LOGICAL        = 1 << 0,
    FUN_OP_SHIFT          = 1 << 1,
    FUN_OP_INT_ADD        = 1 << 2,
    FUN_OP_INT_MUL        = 1 << 3,
    FUN_OP_INT_DIV        = 1 << 4,

    FUN_OP_SFLOAT_ADD     = 1 << 5,
    FUN_OP_SFLOAT_MUL     = 1 << 6,
    FUN_OP_SFLOAT_DIV     = 1 << 7,
    FUN_OP_SFLOAT_FMA     = 1 << 8,

    FUN_OP_DFLOAT_ADD     = 1 << 9,
    FUN_OP_DFLOAT_MUL     = 1 << 10,
    FUN_OP_DFLOAT_DIV     = 1 << 11,
    FUN_OP_DFLOAT_FMA     = 1 << 12,

    FUN_OP_LOAD           = 1 << 13,
    FUN_OP_STORE          = 1 << 14,

    FUN_OP_INT_VECTOR     = 1 << 15,
    FUN_OP_FLOAT_VECTOR   = 1 << 16,
};
struct FunctionalUnitDescriptor {
    bit_mask64 capabilites;
};
struct ExecutionCluster {
    u32 unitCount;
    u32 reorderBuffer;
    FunctionalUnitDescriptor* units;
};
struct MachineDescriptor {

    const char* microArhitecture;
    ArhitectureDescriptor* arhitecture;
    bit_mask64 properties;

    u32 IFLoadSize;
    u32 IFLoadAlignment;
    u32 IFCycleCount;
    u32 IDCycleCount;
    u32 IDMaxOpsProduced;

    u32 clusterCount;
    ExecutionCluster* clusters;
};

constexpr Token inv_token = Token{};
struct Compiler {
    Parser parser;

    DynamicBufferLocalMalloc<Variable> symbolTable;
    DynamicBufferLocalMalloc<IncompleteMemoryOp> incompleteMemoryOps;
    DynamicBufferLocalMalloc<HashNode<VarDefintion*, u32>> basicBlockVarDefs;
    DynamicBufferLocalMalloc<HashNode<u32,u32>> rangeTable;

    byte* mem;
    byte* cfgTraversalMemory;
    byte* scratchMem;
    
    MachineDescriptor* machine;
    
    LocalMallocState localHeap;
    MemoryPool<sizeof(SSABasicBlock)> ssaBlockPool;
    MemoryPool<sizeof(SSADefinition)> ssaDefPool;
    MemoryPool<sizeof(SSAMemoryDef)> memoryDefPool;
    LinearAllocator symbolicMem;

    u32 currentFunction;
    u32 exprAllocator;
    u32 StmtAllocator;
    u32 miscAllocatorSSA;

    u32 memoryName;
    u32 uniqueMemoryName;

    TypeExpr basicTypes[TYPE_COUNT];
    Stmt entryPoint;
    u32 totalMem;

    bool optimize;
    bool panic;
    bool error;
};

struct LiveInterval {
    u32 lowerIndex;
    u32 upperIndex;
    u32 block;
};
struct LiveTrace {
    u32 def;
    u32 blockCount;
    LiveInterval trace[0];
};
struct CallEdge {
    SSADefinition* call;
    u32 caller;
    u32 callee;
};
struct CallVertex {
    SmallStaticBuffer calls;
    SmallStaticBuffer callers;
    u32 fn;
};
struct CallGraph {
    CallVertex* vertices;
    u32 vertexCount;
};
struct SSAProgram {
    u32 functionCount;
    SSAFunction* functions[];
};
struct CompilerContext {
    Compiler* compiler;
    SSAFunction* fn;
};

struct CompileConfig {
    bool optimize;
    bool dumpIR;
    f32 growthBudget;
    MachineDescriptor* machine;
};

u32 x86_64_gpr_alias_classes[16][4] = {
    {0,16,32,48},
    {1,17,33,49},
    {2,18,34,50},
    {3,19,35,51},
    {4,20,36,52},
    {5,21,37,53},
    {6,22,38,54},
    {7,23,39,55},
    {8,24,40,56},
    {9,25,41,57},
    {10,26,42,58},
    {11,27,43,59},
    {12,28,44,60},
    {13,29,45,61},
    {14,30,46,62},
    {15,31,47,63},
};
u32 x86_64_xmm_alias_classes[16][1] = {
    {0},{1},{2},{3},{4},{5},{6},{7},{8},{9},{10},{11},{12},{13},{14},{15},
};
u32 x86_64_flags_register_alias_class[] = {};
bit_mask64 all_reg_prop =
    REGISTER_SINGLE_FLOAT_OPS | REGISTER_DOUBLE_FLOAT_OPS | 
    REGISTER_INTEGER_OPS      | REGISTER_SCALAR_OPS |
    REGISTER_VECTOR_OPS;
static RegisterDescriptor x86_64_registers[] = {
    {"rax", x86_64_gpr_alias_classes[0], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 0
    {"rbx", x86_64_gpr_alias_classes[1], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 1
    {"rcx", x86_64_gpr_alias_classes[2], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 2
    {"rdx", x86_64_gpr_alias_classes[3], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 3
    {"rsi", x86_64_gpr_alias_classes[4], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 4
    {"rdi", x86_64_gpr_alias_classes[5], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 5
    {"rbp", x86_64_gpr_alias_classes[6], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 6
    {"rsp", x86_64_gpr_alias_classes[7], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 7
    {"r8",  x86_64_gpr_alias_classes[8], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 8
    {"r9",  x86_64_gpr_alias_classes[9], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 9
    {"r10", x86_64_gpr_alias_classes[10], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 10
    {"r11", x86_64_gpr_alias_classes[11], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 11
    {"r12", x86_64_gpr_alias_classes[12], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 12
    {"r13", x86_64_gpr_alias_classes[13], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 13
    {"r14", x86_64_gpr_alias_classes[14], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 14
    {"r15", x86_64_gpr_alias_classes[15], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 64}, // 15

    {"eax", x86_64_gpr_alias_classes[0], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"ebx", x86_64_gpr_alias_classes[1], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"ecx", x86_64_gpr_alias_classes[2], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"edx", x86_64_gpr_alias_classes[3], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"esi", x86_64_gpr_alias_classes[4], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"edi", x86_64_gpr_alias_classes[5], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"ebp", x86_64_gpr_alias_classes[6], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"esp", x86_64_gpr_alias_classes[7], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"r8d",  x86_64_gpr_alias_classes[8], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"r9d",  x86_64_gpr_alias_classes[9], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"r10d", x86_64_gpr_alias_classes[10], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"r11d", x86_64_gpr_alias_classes[11], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"r12d", x86_64_gpr_alias_classes[12], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"r13d", x86_64_gpr_alias_classes[13], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"r14d", x86_64_gpr_alias_classes[14], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},
    {"r15d", x86_64_gpr_alias_classes[15], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 32},

    {"ax", x86_64_gpr_alias_classes[0], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"bx", x86_64_gpr_alias_classes[1], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"cx", x86_64_gpr_alias_classes[2], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"dx", x86_64_gpr_alias_classes[3], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"si", x86_64_gpr_alias_classes[4], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"di", x86_64_gpr_alias_classes[5], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"bp", x86_64_gpr_alias_classes[6], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"sp", x86_64_gpr_alias_classes[7], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"r8w",  x86_64_gpr_alias_classes[8], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"r9w",  x86_64_gpr_alias_classes[9], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"r10w", x86_64_gpr_alias_classes[10], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"r11w", x86_64_gpr_alias_classes[11], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"r12w", x86_64_gpr_alias_classes[12], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"r13w", x86_64_gpr_alias_classes[13], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"r14w", x86_64_gpr_alias_classes[14], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},
    {"r15w", x86_64_gpr_alias_classes[15], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 16},

    {"al", x86_64_gpr_alias_classes[0], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"bl", x86_64_gpr_alias_classes[1], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"cl", x86_64_gpr_alias_classes[2], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"dl", x86_64_gpr_alias_classes[3], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"sil", x86_64_gpr_alias_classes[4], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"dil", x86_64_gpr_alias_classes[5], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"bpl", x86_64_gpr_alias_classes[6], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"spl", x86_64_gpr_alias_classes[7], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"r8b",  x86_64_gpr_alias_classes[8], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"r9b",  x86_64_gpr_alias_classes[9], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"r10b", x86_64_gpr_alias_classes[10], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"r11b", x86_64_gpr_alias_classes[11], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"r12b", x86_64_gpr_alias_classes[12], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"r13b", x86_64_gpr_alias_classes[13], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"r14b", x86_64_gpr_alias_classes[14], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},
    {"r15b", x86_64_gpr_alias_classes[15], REGISTER_SCALAR_OPS | REGISTER_INTEGER_OPS, 4, 8},

    {"xmm0", x86_64_xmm_alias_classes[0], all_reg_prop, 1, 128},
    {"xmm1", x86_64_xmm_alias_classes[1], all_reg_prop, 1, 128},
    {"xmm2", x86_64_xmm_alias_classes[2], all_reg_prop, 1, 128},
    {"xmm3", x86_64_xmm_alias_classes[3], all_reg_prop, 1, 128},
    {"xmm4", x86_64_xmm_alias_classes[4], all_reg_prop, 1, 128},
    {"xmm5", x86_64_xmm_alias_classes[5], all_reg_prop, 1, 128},
    {"xmm6", x86_64_xmm_alias_classes[6], all_reg_prop, 1, 128},
    {"xmm7", x86_64_xmm_alias_classes[7], all_reg_prop, 1, 128},
    {"xmm8",  x86_64_xmm_alias_classes[8], all_reg_prop, 1, 128},
    {"xmm9",  x86_64_xmm_alias_classes[9], all_reg_prop, 1, 128},
    {"xmm10", x86_64_xmm_alias_classes[10], all_reg_prop, 1, 128},
    {"xmm11", x86_64_xmm_alias_classes[11], all_reg_prop, 1, 128},
    {"xmm12", x86_64_xmm_alias_classes[12], all_reg_prop, 1, 128},
    {"xmm13", x86_64_xmm_alias_classes[13], all_reg_prop, 1, 128},
    {"xmm14", x86_64_xmm_alias_classes[14], all_reg_prop, 1, 128},
    {"xmm15", x86_64_xmm_alias_classes[15], all_reg_prop, 1, 128},

    {"rflag", x86_64_flags_register_alias_class, REGISTER_FLAG, 1, 64},
};
u32 x86_64_callee_save_regs[] = {
    1,6,11,12,13,14
};
u32 x86_64_caller_saved_regs[] = {
    0,2,3,4,5,7,8,9,10
};
u32 x86_64_param_regs[] = {
    5,4,3,2,7,8
};
u32 x86_64_ret_reg = 0;

u32 x86_64_register_classes[] = {

};

static InstDescriptor x86_64_instructions[] = {};
static ABI x86_64_linux_abi = {
    128,
    16,
    1,
    6,
    6,
    9,
    &x86_64_ret_reg,
    x86_64_param_regs,
    x86_64_caller_saved_regs,
    x86_64_callee_save_regs,
};
static ArhitectureDescriptor x86_64_arch = {
    "x86_64",
    2,
    32,
    0,
    x86_64_registers,
    x86_64_instructions,
    x86_64_linux_abi,
};
FunctionalUnitDescriptor piledriverIntPipe[4] = {
    {FUN_OP_LOGICAL | FUN_OP_SHIFT | FUN_OP_INT_ADD | FUN_OP_INT_DIV},
    {FUN_OP_LOGICAL | FUN_OP_SHIFT | FUN_OP_INT_ADD | FUN_OP_INT_MUL},
    {FUN_OP_LOAD | FUN_OP_STORE},
    {FUN_OP_LOAD},
};
FunctionalUnitDescriptor piledriverFloatPipe[4] = {
    {
        FUN_OP_SFLOAT_ADD   | FUN_OP_SFLOAT_MUL | FUN_OP_SFLOAT_DIV |
        FUN_OP_SFLOAT_FMA   | FUN_OP_DFLOAT_ADD | FUN_OP_DFLOAT_MUL | 
        FUN_OP_DFLOAT_DIV   | FUN_OP_DFLOAT_FMA | FUN_OP_INT_VECTOR |
        FUN_OP_FLOAT_VECTOR | FUN_OP_INT_ADD    | FUN_OP_INT_MUL
    },
    {
        FUN_OP_SFLOAT_ADD   | FUN_OP_SFLOAT_MUL | FUN_OP_SFLOAT_DIV |
        FUN_OP_SFLOAT_FMA   | FUN_OP_DFLOAT_ADD | FUN_OP_DFLOAT_MUL | 
        FUN_OP_DFLOAT_DIV   | FUN_OP_DFLOAT_FMA | FUN_OP_INT_VECTOR |
        FUN_OP_FLOAT_VECTOR | FUN_OP_LOGICAL    | FUN_OP_SHIFT
    },
    {FUN_OP_LOGICAL | FUN_OP_SHIFT | FUN_OP_INT_VECTOR | FUN_OP_INT_ADD},
    {FUN_OP_LOGICAL | FUN_OP_SHIFT | FUN_OP_INT_VECTOR | FUN_OP_INT_ADD | FUN_OP_STORE},
};
ExecutionCluster piledriverClusters[2] = {
    {4, 40, piledriverIntPipe},
    {4, 60, piledriverFloatPipe},
};
static MachineDescriptor piledriver {
    "piledriver",
    &x86_64_arch,
    MACHINE_PIPELINED       | MACHINE_WIDE_ISSUE_SUPER_SCALAR |
    MACHINE_OUT_OF_ORDER    | MACHINE_REGISTER_RENAME         |
    MACHINE_BRANCH_PREDICT  | MACHINE_SPECULATIVE_EXEC,
    32,
    32,
    2,
    1,
    4,
    2,
    piledriverClusters
};

void PrintAliasGraph(Compiler* compiler, MemoryAliasGraph *g) {

    sizeof(Compiler);

    for (u32 i = 0; i < g->vertexCount; i++) {

        SSADefinition* def = (SSADefinition*)(compiler->mem + g->vertices[i].basePtr);
        global_print("%s%i%s", "base ", def->value, " aliases: ");
        for (auto k = g->vertices[i].aliases; k; k = k->next) {
            SSADefinition* alias = (SSADefinition*)(compiler->mem + g->vertices[k->index].basePtr);
            global_print("%i%c", alias->value , ' ');
        }
        global_print("\n");
    }
}

struct ParserState {
    Tokenizer tokenizer;
    u32 tokenBufferSize;
    u32 allocator;
    bool error;
};

struct FnTypeExpr : TypeExpr {
    u32 param_count = 0;
    u32 params = 0;
    TypeExpr ret_t;
    TypeExpr modifier;
};
struct ArrayTypeExpr : TypeExpr {
    u32 arraySize;
};
struct StructMemberTypeExpr : TypeExpr {
    Token    name;
    TypeExpr type;
};
struct StructTypeExpr : TypeExpr {
    Token    name;
    TypeExpr members;
};


void* AllocateExpr(Compiler *compiler, u32 size) {
    u32 i = compiler->exprAllocator;
    compiler->exprAllocator += size;
    return compiler->mem + i;
}
template <typename T>
T *AllocateExpr(Compiler *compiler) {
    u32 i = compiler->exprAllocator;
    compiler->exprAllocator += sizeof(T);
    return (T *)(compiler->mem + i);
}
template <typename T>
T *AllocateStmt(Compiler *compiler) {
    u32 i = compiler->StmtAllocator;
    compiler->StmtAllocator += sizeof(T);
    return (T *)(compiler->mem + i);
}
template <typename T>
T *AllocateSSA(Compiler *compiler) {
    u32 i = compiler->miscAllocatorSSA;
    compiler->miscAllocatorSSA += sizeof(T);
    return (T *)(compiler->mem + i);
}
void *AllocateSSA(Compiler *compiler, u32 size) {
    u32 i = compiler->miscAllocatorSSA;
    compiler->miscAllocatorSSA += size;
    return (void *)(compiler->mem + i);
}

u32 FindSymbol(DynamicBufferLocalMalloc<Variable>* vars, Token name) {

    for (i32 i = vars->size - 1; i > -1; i--) {
        if (TokensEquals((*vars)[i].name, name)) {
            return i;
        }
    }
    return ~(u32(0));
}

Token PreviousToken(Parser *parser) {
    return parser->tokenBuffer.mem[parser->tokenBuffer.size - 1];
}
Token NextToken(Parser *parser) {
    u32 i = parser->tokenBuffer.PushBack(GetToken(&parser->tokenizer));
    return parser->tokenBuffer[i];
}

Token PeekToken(Tokenizer peek) {
    return GetToken(&peek);
}

bool Check(Parser *parser, TokenType type) {
    if (!parser->tokenizer.at[0])
        return type == TOKEN_EOF;

    Tokenizer peek = parser->tokenizer;
    Token nextToken = GetToken(&peek);
    return (nextToken.type == type);
}

bool Match(Parser *parser, TokenType *types, u32 count) {
    for (u32 i = 0; i < count; i++) {
        if (Check(parser, types[i])) {
            NextToken(parser);
            return true;
        }
    }
    return false;
}
bool CheckPeek(Tokenizer tokenizer, TokenType type) {
    if (!tokenizer.at[0])
        return type == TOKEN_EOF;

    Token nextToken = GetToken(&tokenizer);
    return (nextToken.type == type);
}
bool MatchPeek(Tokenizer *tokenizer, TokenType *types, u32 count) {
    for (u32 i = 0; i < count; i++) {
        if (CheckPeek(*tokenizer, types[i])) {
            GetToken(tokenizer);
            return true;
        }
    }
    return false;
}

const char *GetExprTypeStr(ExprType t) {
    switch (t) {
    case EXPRESSION_NULL:
        return "EXPRESSION_NULL";
    case EXPRESSION_LITERAL:
        return "EXPRESSION_LITERAL";
    case EXPRESSION_UNARY:
        return "EXPRESSION_UNARY";
    case EXPRESSION_BINARY:
        return "EXPRESSION_BINARY";
    case EXPRESSION_VARIABLE:
        return "EXPRESSION_VARIABLE";
    case EXPRESSION_MEMORY_LOAD:
        return "EXPRESSION_MEMORY_LOAD";
    case EXPRESSION_VARIABLE_ASSIGNMENT:
        return "EXPRESSION_VARIABLE_ASSIGNMENT";
    case EXPRESSION_MEMORY_STORE:
        return "EXPRESSION_MEMORY_STORE";
    case EXPRESSION_ADDRESS_OF:
        return "EXPRESSION_ADDRESS_OF";
    case EXPRESSION_GET:
        return "EXPRESSION_GET";
    case EXPRESSION_PEEL_TYPE:
        return "EXPRESSION_PEEL_TYPE";
    case EXPRESSION_MEM_COPY:
        return "EXPRESSION_MEM_COPY";
    case EXPRESSION_CALL:
        return "EXPRESSION_CALL";
    case EXPRESSION_CONVERSION:
        return "EXPRESSION_CONVERSION";
    }
    return nullptr;
}
void ExpectToken(Parser *parser, TokenType e) {

    Token token = NextToken(parser);

    if (token.type != e) {
        global_print("%s%s%s%s*%s%i%\n", "ERROR: expected: ", GetTokenStr(e), " got: ", token.text, token.lenght, " at line: ", parser->tokenizer.line);
        parser->error = true;
    }
}
TypeName GetNumberType(Token t) {

    if (t.text[t.lenght - 1] == 'f')
        return TYPE_PRIMARY_F32;

    for (u32 i = 0; i < t.lenght; i++) {
        if (t.text[i] == '.') {
            return TYPE_PRIMARY_F64;
        }
    }

    return TYPE_PRIMARY_INT64;
}

void Warning(Parser *parser, bool cond, const char *warnMsg) {
    if (!cond) {
        global_print("%s%s%s%i%\n", "WARNING: ", warnMsg, " at line: ", parser->tokenizer.line);
    }
}
void Expect(Parser *parser, bool cond, const char *errMsg) {
    if (!cond) {
        global_print("%s%s%s%i%\n", "ERROR: ", errMsg, " at line: ", parser->tokenizer.line);
        parser->error = true;
    }
}
ParserState SaveParserState(Parser *parser, u32 exprAllocator) {
    return ParserState{parser->tokenizer, parser->tokenBuffer.size, exprAllocator, parser->error};
}
void RestoreParserState(Parser *parser, ParserState state) {
    parser->tokenizer = state.tokenizer;
    parser->tokenBuffer.size = state.tokenBufferSize;
    parser->error = state.error;
}

f64 GetF64FromToken(Token l) {
    char str[l.lenght + 1]{0};
    for (u32 i = 0; i < l.lenght; i++) {
        str[i] = l.text[i];
    }
    return strtod(str, nullptr);
}
f32 GetF32FromToken(Token t) {
    char str[t.lenght + 1]{0};
    for (u32 i = 0; i < t.lenght; i++) {
        str[i] = t.text[i];
    }
    return (f32)strtod(str, nullptr);
}
bool GetBoolFromToken(Token t) {
    return TokenEquals(t, "true");
}
i64 GetIntFromToken(Token t) {
    i64 r = 0;
    u32 e = 1;
    for (i32 i = t.lenght - 1; i > -1; i--) {
        r += (t.text[i] - '0') * e;
        e *= 10;
    }
    return r;
}
u64 GetUIntFromToken(Token t) {
    u64 r = 0;
    u32 e = 1;
    for (i32 i = t.lenght - 1; i > -1; i--) {
        r += (t.text[i] - '0') * e;
        e *= 10;
    }
    return r;
}
u32 HashType(byte *mem, TypeExpr te) {

    u32 hash = 7;
    TypeName t = Mem<TypeName>(mem + te.index);

    while (t != TYPE_NON) {
        switch (t) {
        case TYPE_PRIMARY_FN: {
            FnTypeExpr *fn = (FnTypeExpr *)(mem + te.index);
            hash = hash * 31 + t;
            for (u32 i = 0; i < fn->param_count; i++) {
                hash = hash * 31 + HashType(mem, Mem<TypeExpr>(mem + fn->params));
            }
            hash = hash * 31 + HashType(mem, fn->ret_t);
            te = fn->modifier;
            break;
        }
        case TYPE_MODIFIER_ARRAY: {
            ArrayTypeExpr *arr = (ArrayTypeExpr *)(mem + te.index);
            hash = hash * 31 + t + arr->arraySize;
            te.index += sizeof(ArrayTypeExpr);
        } break;
        default:
            hash = hash * 31 + t;
            te.index += sizeof(TypeExpr);
            break;
        }
        t = Mem<TypeName>(mem + te.index);
    }
    return hash;
}
void PrintValue(Value v) {
    switch (v.type) {
    case TYPE_MODIFIER_POINTER:
        global_print("%s", Mem<char *>(v.mem));
        break;
    case TYPE_PRIMARY_BOOL:
        global_print("%s", (Mem<bool>(v.mem) ? "true" : "false") );
        break;
    case TYPE_PRIMARY_INT8:
        global_print("%i", (i64)Mem<i8>(v.mem));
        break;
    case TYPE_PRIMARY_INT16:
        global_print("%i", Mem<i16>(v.mem));
        break;
    case TYPE_PRIMARY_INT32:
        global_print("%i", Mem<i32>(v.mem));
        break;
    case TYPE_PRIMARY_INT64:
        global_print("%i", Mem<i64>(v.mem));
        break;
    case TYPE_PRIMARY_UINT8:
        global_print("%i", (i64)Mem<u8>(v.mem));
        break;
    case TYPE_PRIMARY_UINT16:
        global_print("%i", Mem<u16>(v.mem));
        break;
    case TYPE_PRIMARY_UINT32:
        global_print("%i", Mem<u32>(v.mem));
        break;
    case TYPE_PRIMARY_UINT64:
        global_print("%i", Mem<u64>(v.mem));
        break;
    case TYPE_PRIMARY_F32:
        global_print("%f", Mem<f64>(v.mem));
        break;
    case TYPE_PRIMARY_F64:
        global_print("%f", Mem<f64>(v.mem));
        break;
    }
}
const char *GetValueTStr(Value v) {
    switch (v.type) {
    case TYPE_MODIFIER_POINTER:
        return "*";
    case TYPE_PRIMARY_BOOL:
        return "bool";
    case TYPE_PRIMARY_INT8:
        return "i8";
    case TYPE_PRIMARY_INT16:
        return "i16";
    case TYPE_PRIMARY_INT32:
        return "i32";
    case TYPE_PRIMARY_INT64:
        return "i64";
    case TYPE_PRIMARY_UINT8:
        return "u8";
    case TYPE_PRIMARY_UINT16:
        return "u16";
    case TYPE_PRIMARY_UINT32:
        return "u32";
    case TYPE_PRIMARY_UINT64:
        return "u64";
    case TYPE_PRIMARY_F32:
        return "f32";
    case TYPE_PRIMARY_F64:
        return "f64";
    }
}
u64 HashValue(Value v) {

    char str[256]{};
    switch (v.type) {
    case TYPE_PRIMARY_F32:
        return str_hash(str, f32_to_string(str, 256, Mem<f32>(v.mem), 7));
    case TYPE_PRIMARY_F64:
        return str_hash(str, f32_to_string(str, 256, Mem<f64>(v.mem), 7));
    default:
        return str_hash(str, u64_to_string(str, 256, Mem<u64>(v.mem)));
    }
}
TypeName GetLiteralType(Token literal) {
    switch (literal.type) {
    case TOKEN_NUMBER_LITERAL: {
        return GetNumberType(literal);
        break;
    }
    case TOKEN_BOOL_LITERAL: {
        return TYPE_PRIMARY_BOOL;
        break;
    }
    case TOKEN_NULL_LITERAL: {
        return TYPE_PRIMARY_VOID;
        break;
    }
    case TOKEN_STRING_LITERAL: {
        return TYPE_MODIFIER_POINTER;
        break;
    }
    case TOKEN_CHAR_LITERAL: {
        return TYPE_PRIMARY_CHAR;
        break;
    }
    default:
        LOG_ASSERT(false, "Unkown literal type")
        break;
    }

    ASSERT(false);
}

bool IsOprCommutative(u32 opr) {
    return opr == TOKEN_PLUS |
           opr == TOKEN_ASTERISK |
           opr == TOKEN_EQUALS_EQUALS |
           opr == TOKEN_VERTICAL_BAR |
           opr == TOKEN_AMPERSAND |
           opr == TOKEN_AMPERSAND_AMPERSAND |
           opr == TOKEN_CIRCUMFLEX |
           opr == TOKEN_VERTICAL_BAR_VERTICAL_BAR;
}
u32 *GetPhiOperandPtr(byte* baseMem, SSADefinition *phi) {
    return GetPointer<u32>(baseMem, phi->operand1);
}
u32 GetPhiOperandCount(SSADefinition* phi) {
    return phi->operand0;
}

struct FuncCallPtr {
    u32 *ptr;
    u32 argCount;
};
FuncCallPtr GetCallArgs(byte* baseMem, SSADefinition *call) {

    FuncCallPtr ret{};
    u32 *args = (u32 *)(baseMem + call->operand1);

    i32 i = 0;
    while (args[i++]);
    ret.ptr = args;
    ret.argCount = Max(i - 1, 0);
    return ret;
}
struct TypeKeyWord {
    const char* name;
    TypeName type;
};
constexpr TypeKeyWord typeTokens[] = {
    {"i8", TYPE_PRIMARY_INT8},
    {"i16", TYPE_PRIMARY_INT16},
    {"i32", TYPE_PRIMARY_INT32},
    {"i64", TYPE_PRIMARY_INT64},
    {"u8", TYPE_PRIMARY_UINT8},
    {"u16", TYPE_PRIMARY_UINT16},
    {"u32", TYPE_PRIMARY_UINT32},
    {"u64", TYPE_PRIMARY_UINT64},
    {"f32", TYPE_PRIMARY_F32},
    {"f64", TYPE_PRIMARY_F64},
    {"char", TYPE_PRIMARY_CHAR},
    {"bool", TYPE_PRIMARY_BOOL},
    {"void", TYPE_PRIMARY_VOID},
};

bool IsType(Parser *parser, Tokenizer peek) {

    Token t = GetToken(&peek);

    switch (t.type) {
    case TOKEN_IDENTIFIER:
        for(auto i = typeTokens; i < 1[&typeTokens]; i++) {
            if(TokenEquals(t, i->name)) {
                return true;
            }
        }
        for (u32 i = 0; i < parser->structs.size; i++) {
            if (TokensEquals(t, parser->structs[i].name)) {
                return true;
            }
        }
        break;
    case TOKEN_KEYWORD_FN:
        return true;
    case TOKEN_OPEN_PAREN:
        return IsType(parser, peek);
    case TOKEN_KEYWORD_AUTO:
        return true;
    default:
        return false;
    }

    return false;
}



TypeExpr ParseTypeExpression(Compiler *compiler);
TypeExpr PrimaryTypeExpression(Compiler *compiler) {

    TypeExpr ret{compiler->exprAllocator};

    Token base = NextToken(&compiler->parser);
    for(auto i = typeTokens; i < 1[&typeTokens]; i++) {
        if(TokenEquals(base, i->name)) {
            *AllocateExpr<TypeName>(compiler) = i->type;
            return ret;
        }
    }
    if (base.type == TOKEN_KEYWORD_FN) {

        FnTypeExpr *node = AllocateExpr<FnTypeExpr>(compiler);
        ExpectToken(&compiler->parser, TOKEN_OPEN_PAREN);
        node->index = TYPE_PRIMARY_FN;
        node->param_count = 0;
        node->params = 0;

        TokenType close = TOKEN_CLOSE_PAREN;
        TokenType comma = TOKEN_COMMA;
        Tokenizer peek = compiler->parser.tokenizer;
        if (!Match(&compiler->parser, &close, 1)) {
            do {
                node->param_count++;
                Token q;
                while ((q = PeekToken(peek)).type != TOKEN_COMMA && q.type != TOKEN_CLOSE_PAREN)
                    GetToken(&peek);
            } while (MatchPeek(&peek, &comma, 1));

            node->params = compiler->exprAllocator;
            for (u32 i = 0; i < node->param_count; i++)
                AllocateExpr<TypeExpr>(compiler);

            node->param_count = 0;
            do {
                Mem<TypeExpr>(compiler->mem + node->params + node->param_count * sizeof(TypeExpr)) = ParseTypeExpression(compiler);
                node->param_count++;
            } while (Match(&compiler->parser, &comma, 1));

            ExpectToken(&compiler->parser, TOKEN_CLOSE_PAREN);
        }

        TokenType arrow = TOKEN_KEYWORD_ARROW;
        if (Match(&compiler->parser, &arrow, 1)) {
            node->ret_t = ParseTypeExpression(compiler);
        }
        else {
            node->ret_t.index = compiler->exprAllocator;
            *AllocateExpr<TypeName>(compiler) = TYPE_PRIMARY_VOID;
            *AllocateExpr<TypeName>(compiler) = TYPE_NON;
        }
    } 
    else if (base.type == TOKEN_OPEN_PAREN) {
        PrimaryTypeExpression(compiler);
        ExpectToken(&compiler->parser, TOKEN_CLOSE_PAREN);
    }
    else {
        for (u32 i = 0; i < compiler->parser.structs.size; i++) {
            if (TokensEquals(base, compiler->parser.structs[i].name)) {
                AllocateExpr<TypeExpr>(compiler)->index = compiler->parser.structs[i].ptr;
                return ret;
            }
        }
        Expect(&compiler->parser, false, "unknown type identifier used in type expression");
    }

    return ret;
}

TypeExpr ParseTypeModifierExpression(Compiler* compiler) {


    TypeExpr expr{compiler->exprAllocator};
    if (PeekToken(compiler->parser.tokenizer).type == TOKEN_OPEN_PAREN) {
        ExpectToken(&compiler->parser, TOKEN_OPEN_PAREN);
        ParseTypeModifierExpression(compiler);
        ExpectToken(&compiler->parser, TOKEN_CLOSE_PAREN);
    }

    TokenType restricted = TOKEN_KEYWORD_RESTRICT;
    for(bool run = true;run;) {
        const auto peek = PeekToken(compiler->parser.tokenizer);
        switch (peek.type) {
        case TOKEN_ASTERISK:
            {
                NextToken(&compiler->parser);
                if(Match(&compiler->parser, &restricted, 1)) {
                    *AllocateExpr<TypeName>(compiler) = TYPE_MODIFIER_RESTRICTED_POINTER;
                }
                else {
                    *AllocateExpr<TypeName>(compiler) = TYPE_MODIFIER_POINTER;
                }
                break;
            }
        case TOKEN_OPEN_PAREN:
            ParseTypeModifierExpression(compiler);
            ExpectToken(&compiler->parser, TOKEN_CLOSE_PAREN);
            break;
        case TOKEN_KEYWORD_VOLATILE:
            NextToken(&compiler->parser);
            *AllocateExpr<TypeName>(compiler) = TYPE_MODIFIER_VOLATILE;
            break;
        case TOKEN_KEYWORD_CONST:
            NextToken(&compiler->parser);
            *AllocateExpr<TypeName>(compiler) = TYPE_MODIFIER_CONST;
            break;
        case TOKEN_KEYWORD_ATOMIC:
            NextToken(&compiler->parser);
            *AllocateExpr<TypeName>(compiler) = TYPE_MODIFIER_ATOMIC;
            break;
        case TOKEN_OPEN_BRACKET:
            {
                NextToken(&compiler->parser);
                Token arrCount = NextToken(&compiler->parser);
                TypeName t = GetNumberType(arrCount);
                Expect(&compiler->parser, t == TYPE_PRIMARY_INT64, "array size must be of integral type");
                u64 size = GetIntFromToken(arrCount);
                ArrayTypeExpr *node = AllocateExpr<ArrayTypeExpr>(compiler);
                node->index = TYPE_MODIFIER_ARRAY;
                node->arraySize = size;

                ExpectToken(&compiler->parser, TOKEN_CLOSE_BRACKET);
                break;
            } 
        default:
            run = false;
            break;
        }
    }

    return expr;
}
TypeExpr ParseTypeExpression(Compiler *compiler) {

    TypeExpr expr = PrimaryTypeExpression(compiler);
    if (*((TypeName*)(compiler->mem + expr.index)) == TYPE_PRIMARY_FN) {
        ((FnTypeExpr*)(compiler->mem+expr.index))->modifier = ParseTypeModifierExpression(compiler);
    } else {
        ParseTypeModifierExpression(compiler);
    }

    *AllocateExpr<TypeName>(compiler) = TYPE_NON;
    return expr;
}

u32 GetTypeExprSize(void *mem, TypeExpr expr) {

    u32 begin = expr.index;
    while (Mem<TypeName>((byte *)mem + expr.index) != TYPE_NON) {
        switch (Mem<TypeName>((byte *)mem + expr.index)) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            expr = Mem<FnTypeExpr>((byte *)mem + expr.index).modifier;
            break;
        case TYPE_MODIFIER_ARRAY:
            expr.index += sizeof(ArrayTypeExpr);
            break;
        default:
            expr.index += sizeof(TypeName);
            break;
        }
    }
    expr.index += sizeof(TypeName);
    return expr.index - begin;
}

TypeExpr CpyTypeExpr(byte *dst, TypeExpr dstExpr, byte *src, TypeExpr srcExpr) {

    TypeName t = Mem<TypeName>(src + srcExpr.index);
    while (t != TYPE_NON) {

        switch (t) {
        case TYPE_PRIMARY_FN: {
            FnTypeExpr *node = (FnTypeExpr *)(src + srcExpr.index);
            FnTypeExpr *dstNode = (FnTypeExpr *)(dst + dstExpr.index);
            memcpy(dst + dstExpr.index, src + srcExpr.index, sizeof(FnTypeExpr));
            dstExpr.index += sizeof(FnTypeExpr);

            if (node->param_count != 0) {
                ASSERT(node->params != 0);

                dstNode->params = dstExpr.index;
                dstExpr.index += node->param_count * sizeof(TypeExpr);
                for (u32 i = 0; i < node->param_count; i++) {
                    Mem<TypeExpr>(dst + dstNode->params + i * sizeof(TypeExpr)) = dstExpr;
                    dstExpr = CpyTypeExpr(dst, dstExpr, src, Mem<TypeExpr>(src + node->params + i * sizeof(TypeExpr)));
                }
            }

            dstNode->ret_t = dstExpr;
            dstExpr = CpyTypeExpr(dst, dstExpr, src, node->ret_t);

            dstNode->modifier = dstExpr;
            srcExpr = node->modifier;
        } break;
        case TYPE_MODIFIER_ARRAY:
            memcpy(dst + dstExpr.index, src + srcExpr.index, sizeof(ArrayTypeExpr));
            srcExpr.index += sizeof(ArrayTypeExpr);
            dstExpr.index += sizeof(ArrayTypeExpr);
            break;
        default:
            memcpy(dst + dstExpr.index, src + srcExpr.index, sizeof(TypeName));
            srcExpr.index += sizeof(TypeName);
            dstExpr.index += sizeof(TypeName);
            break;
        }
        t = Mem<TypeName>(src + srcExpr.index);
    }

    Mem<TypeName>(dst + dstExpr.index) = TYPE_NON;
    dstExpr.index += sizeof(TypeName);
    return dstExpr;
}

TypeName GetLastType(byte *mem, TypeExpr expr) {

    TypeName last = Mem<TypeName>(mem + expr.index);
    while (Mem<TypeName>(mem + expr.index) != TYPE_NON) {

        last = Mem<TypeName>(mem + expr.index);

        switch (Mem<TypeName>(mem + expr.index)) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            expr = Mem<FnTypeExpr>(mem + expr.index).modifier;
            break;
        case TYPE_MODIFIER_ARRAY:
            expr.index += sizeof(ArrayTypeExpr);
            break;
        default:
            expr.index += sizeof(TypeName);
            break;
        }
    }

    return last;
}
TypeExpr GetNthType(byte *mem, TypeExpr expr, u32 nth) {

    TypeExpr ptrs[nth + 1]{};

    while (Mem<TypeName>(mem + expr.index) != TYPE_NON) {

        for (i32 i = nth; i > 0; i--) {
            ptrs[i] = ptrs[i - 1];
        }
        ptrs[0] = expr;

        TypeName type = Mem<TypeName>(mem + expr.index);
        switch (type) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            expr = Mem<FnTypeExpr>(mem + expr.index).modifier;
            break;
        case TYPE_MODIFIER_ARRAY:
            expr.index += sizeof(ArrayTypeExpr);
            break;
        default:
            expr.index += sizeof(TypeName);
            break;
        }
    }

    ASSERT(Mem<TypeName>(mem + ptrs[nth].index) != TYPE_NON);
    return ptrs[nth];
}

bool TypesEqual(byte *structMem0, byte *structMem1, byte *mem0, byte *mem1, TypeExpr e0, TypeExpr e1) {

    TypeName t0 = Mem<TypeName>(mem0 + e0.index);
    TypeName t1 = Mem<TypeName>(mem1 + e1.index);
    if (t0 != t1)
        return false;

    while (Mem<TypeName>(mem0 + e0.index) != TYPE_NON && Mem<TypeName>(mem1 + e1.index) != TYPE_NON && Mem<TypeName>(mem0 + e0.index) == Mem<TypeName>(mem1 + e1.index)) {

        switch (Mem<TypeName>(mem0 + e0.index)) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN: {

            FnTypeExpr fn0 = Mem<FnTypeExpr>(mem0 + e0.index);
            FnTypeExpr fn1 = Mem<FnTypeExpr>(mem1 + e1.index);
            if (fn0.param_count != fn1.param_count)
                return false;

            for (u32 i = 0; i < fn0.param_count; i++) {
                TypeExpr p0 = Mem<TypeExpr>(mem0 + fn0.params + i * sizeof(TypeExpr));
                TypeExpr p1 = Mem<TypeExpr>(mem1 + fn1.params + i * sizeof(TypeExpr));
                if (!TypesEqual(structMem0, structMem1, mem0, mem1, p0, p1))
                    return false;
            }
            if (!TypesEqual(structMem0, structMem1, mem0, mem1, fn0.ret_t, fn1.ret_t))
                return false;

            e0 = Mem<FnTypeExpr>(mem0 + e0.index).modifier;
            e1 = Mem<FnTypeExpr>(mem1 + e1.index).modifier;
        } break;
        case TYPE_MODIFIER_ARRAY: {
            ArrayTypeExpr fn0 = Mem<ArrayTypeExpr>(mem0 + e0.index);
            ArrayTypeExpr fn1 = Mem<ArrayTypeExpr>(mem1 + e1.index);
            if (fn0.arraySize != fn1.arraySize)
                return false;
        }

            e0.index += sizeof(ArrayTypeExpr);
            e1.index += sizeof(ArrayTypeExpr);
            break;
        default:
            e0.index += sizeof(TypeName);
            e1.index += sizeof(TypeName);
            break;
        }
    }

    return Mem<TypeName>(mem0 + e0.index) == Mem<TypeName>(mem1 + e1.index);
}

ExprType GetLastExprType(byte* baseMem, Expr *expr) {

    ExprType t = Mem<ExprType>(baseMem + expr->index);
    bool searching = true;
    while (searching) {
        switch (t) {
        default:
            t = Mem<ExprType>(baseMem + expr->index);
            searching = false;
            break;
        }
    }
    return t;
}
bool IsIntegral(TypeName t) {
    return t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_UINT64;
}
bool IsIntegral(byte *mem, TypeExpr expr) {
    TypeName t = GetLastType(mem, expr);
    return t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_UINT64;
}
Value GetValueFromLiteral(Token literal);
TypeName GetMinimalTypeforValue(Value v) {

    if (IsIntegral((TypeName)v.type)) {
        i64 i = Mem<i64>(v.mem);
        if (i < 128) {
            return TYPE_PRIMARY_INT8;
        } else if (i < 255) {
            return TYPE_PRIMARY_UINT8;
        } else if (i < 32768) {
            return TYPE_PRIMARY_INT16;
        } else if (i < 65535) {
            return TYPE_PRIMARY_UINT16;
        } else if (i < 2147483648) {
            return TYPE_PRIMARY_INT32;
        } else if (i < 4294967295) {
            return TYPE_PRIMARY_UINT32;
        } else if (i < 9223372036854775807) {
            return TYPE_PRIMARY_INT64;
        } else {
            return TYPE_PRIMARY_UINT64;
        }
    }
    return (TypeName)v.type;
}
TypeName GetMinimumLiteralType(Token literal) {

    Value v = GetValueFromLiteral(literal);
    return GetMinimalTypeforValue(v);
}

bool ImmLitrEQ(ImmediateExpr *imm, LiteralExpr *litr) {

    TypeName litr_t = GetMinimumLiteralType(litr->literal);

    bool isLiteralIntegral = IsIntegral(litr_t);
    bool isImmIntegral = IsIntegral((TypeName)imm->v.type);

    if (!(isLiteralIntegral && isImmIntegral) && (imm->v.type != litr_t)) {
        return false;
    }

    switch (litr_t) {
    case TYPE_PRIMARY_INT8:
    case TYPE_PRIMARY_INT16:
    case TYPE_PRIMARY_INT32:
    case TYPE_PRIMARY_INT64:
    case TYPE_PRIMARY_UINT8:
    case TYPE_PRIMARY_UINT16:
    case TYPE_PRIMARY_UINT32:
    case TYPE_PRIMARY_UINT64: {
        i64 n = GetIntFromToken(litr->literal);
        return Mem<i64>(imm->v.mem) == n;
    } break;
    case TYPE_PRIMARY_F32: {
        f32 n = GetF32FromToken(litr->literal);
        return Mem<f32>(imm->v.mem) == n;
    } break;
    case TYPE_PRIMARY_F64: {
        f64 n = GetF64FromToken(litr->literal);
        return Mem<f64>(imm->v.mem) == n;
    } break;
    case TYPE_PRIMARY_BOOL: {
        bool n = GetBoolFromToken(litr->literal);
        return Mem<bool>(imm->v.mem) == n;
    } break;
    case TYPE_PRIMARY_CHAR: {
        char c = litr->literal.text[0];
        if (litr->literal.text[0] == '\\' && litr->literal.text[1] == 'n')
            c = '\n';
        return Mem<char>(imm->v.mem) == c;
    } break;
    default:
        LOG_ASSERT(false, "Unkown literal type")
        break;
    }
}
void PrintTypeModifierExpression(byte *mem, TypeExpr modifier) {

    while (Mem<TypeName>(mem + modifier.index) != TYPE_NON) {

        switch (Mem<TypeName>(mem + modifier.index)) {
        case TYPE_MODIFIER_RESTRICTED_POINTER:
            global_print("%s", "*restrict");
            modifier.index += sizeof(TypeName);
            break;
        case TYPE_MODIFIER_POINTER:
            global_print("%c", '*');
            modifier.index += sizeof(TypeName);
            break;
        case TYPE_MODIFIER_CONST:
            global_print("%s", " const");
            modifier.index += sizeof(TypeName);
            break;
        case TYPE_MODIFIER_ATOMIC:
            global_print("%s", " atomic");
            modifier.index += sizeof(TypeName);
            break;
        case TYPE_MODIFIER_VOLATILE:
            global_print("%s", " volatile");
            modifier.index += sizeof(TypeName);
            break;
        case TYPE_MODIFIER_ARRAY:
            global_print("%c%i%c", '[', Mem<ArrayTypeExpr>(mem + modifier.index).arraySize, ']');
            modifier.index += sizeof(ArrayTypeExpr);
            break;
        default: {
            global_print("%s%\n", "unkonw type modifier");
            ASSERT(false);
        }
        }
    }
}
void PrintTypeExpression(byte *strucures, byte *mem, TypeExpr expr) {

    TypeName n = Mem<TypeName>(mem + expr.index);
    switch (n) {
    case TYPE_PRIMARY_UINT8:
        global_print("%s", "u8");
        break;
    case TYPE_PRIMARY_UINT16:
        global_print("%s", "u16");
        break;
    case TYPE_PRIMARY_UINT32:
        global_print("%s", "u32");
        break;
    case TYPE_PRIMARY_UINT64:
        global_print("%s", "u64");
        break;

    case TYPE_PRIMARY_INT8:
        global_print("%s", "i8");
        break;
    case TYPE_PRIMARY_INT16:
        global_print("%s", "i16");
        break;
    case TYPE_PRIMARY_INT32:
        global_print("%s", "i32");
        break;
    case TYPE_PRIMARY_INT64:
        global_print("%s", "i64");
        break;

    case TYPE_PRIMARY_F32:
        global_print("%s", "f32");
        break;
    case TYPE_PRIMARY_F64:
        global_print("%s", "f64");
        break;

    case TYPE_PRIMARY_CHAR:
        global_print("%s", "char");
        break;
    case TYPE_PRIMARY_VOID:
        global_print("%s", "void");
        break;
    case TYPE_PRIMARY_BOOL:
        global_print("%s", "bool");
        break;

    case TYPE_PRIMARY_NATIVE_FN:
    case TYPE_PRIMARY_FN: {
        FnTypeExpr fn = Mem<FnTypeExpr>(mem + expr.index);
        if (fn.modifier.index != TYPE_NON) {
            global_print("%s", "(fn(");
            for (u32 i = 0; i < fn.param_count; i++) {
                PrintTypeExpression(strucures, mem, Mem<TypeExpr>(mem + fn.params + i * sizeof(TypeExpr)));
                if (i != fn.param_count - 1) {
                    global_print("%s", ", ");
                }
            }

            global_print("%s", ") ->  ");
            PrintTypeExpression(strucures, mem, fn.ret_t);
            global_print("%c", ')');
            PrintTypeModifierExpression(mem, fn.modifier);
        } else {
            global_print("%s", "fn(");
            for (u32 i = 0; i < fn.param_count; i++) {
                PrintTypeExpression(strucures, mem, Mem<TypeExpr>(mem + fn.params + i * sizeof(TypeExpr)));
                if (i != fn.param_count - 1) {
                    global_print("%s", ", ");
                }
            }

            global_print("%s", ") -> ");
            PrintTypeExpression(strucures, mem, fn.ret_t);
        }
        return;
    } break;
    default: {
        StructTypeExpr st = Mem<StructTypeExpr>(strucures + n);
        global_print("%s*", st.name.text, st.name.lenght);
    } break;
    }

    expr.index += sizeof(TypeName);
    PrintTypeModifierExpression(mem, expr);
}
void PrintStruct(byte *structures, byte *mem, TypeExpr ptr) {

    char *name = Mem<StructTypeExpr>(mem + ptr.index).name.text;
    u32 len = Mem<StructTypeExpr>(mem + ptr.index).name.lenght;
    global_print("%s%s*%s%\n", "struct ", name, len, " {");

    TypeExpr members = Mem<StructTypeExpr>(mem + ptr.index).members;

    while (Mem<TypeExpr>(mem + members.index).index != 0) {

        TypeExpr member = Mem<TypeExpr>(mem + members.index);
        TypeExpr type = Mem<StructMemberTypeExpr>(mem + member.index).type;
        Token name = Mem<StructMemberTypeExpr>(mem + member.index).name;
        global_print("%c", '\t');
        PrintTypeExpression(structures, mem, type);
        global_print("%c%s*%c%\n", ' ', name.text, name.lenght, ';');
        members.index += sizeof(TypeExpr);
    }

    global_print("%c%\n", '}');
}
ExprType FirstNotGroupExpr(byte* baseMem, Expr *expr) {

    auto t = Mem<ExprType>(baseMem + expr->index);
    bool searching = true;
    while (searching) {
        switch (t) {
        case EXPRESSION_CONVERSION: {
            auto g = Mem<ConversionExpr>(baseMem + expr->index);
            *expr = g.from;
        } break;
        case EXPRESSION_PEEL_TYPE: {
            auto g = Mem<PeelTypeExpr>(baseMem + expr->index);
            *expr = g.expr;
            break;
        }
        default:
            searching = false;
            break;
        }
        t = Mem<ExprType>(baseMem + expr->index);
    }
    return t;
}
u32 GetTypeNameSize(TypeName type) {

    switch (type) {
    case TYPE_PRIMARY_INT8:
    case TYPE_PRIMARY_UINT8:
    case TYPE_PRIMARY_CHAR:
    case TYPE_PRIMARY_BOOL:
        return 1;
    case TYPE_PRIMARY_INT16:
    case TYPE_PRIMARY_UINT16:
        return 2;
    case TYPE_PRIMARY_INT32:
    case TYPE_PRIMARY_UINT32:
    case TYPE_PRIMARY_F32:
        return 4;
    case TYPE_PRIMARY_INT64:
    case TYPE_PRIMARY_UINT64:
    case TYPE_PRIMARY_F64:
    case TYPE_MODIFIER_POINTER:
    case TYPE_MODIFIER_RESTRICTED_POINTER:
        return 8;
    default:
        return 0;
    }

    return 0;
}

u32 GetTypeSize(byte *structmem, byte *mem, TypeExpr type);
u32 GetTypeSizeHelper(byte *structmem, byte *mem, i32 *types, u32 i) {

    TypeName type = Mem<TypeName>(mem + types[i]);
    switch (type) {
    case TYPE_PRIMARY_INT8:
    case TYPE_PRIMARY_UINT8:
    case TYPE_PRIMARY_CHAR:
    case TYPE_PRIMARY_BOOL:
        return 1;
    case TYPE_PRIMARY_INT16:
    case TYPE_PRIMARY_UINT16:
        return 2;
    case TYPE_PRIMARY_INT32:
    case TYPE_PRIMARY_UINT32:
    case TYPE_PRIMARY_F32:
        return 4;
    case TYPE_PRIMARY_INT64:
    case TYPE_PRIMARY_UINT64:
    case TYPE_PRIMARY_F64:
    case TYPE_MODIFIER_RESTRICTED_POINTER:
    case TYPE_MODIFIER_POINTER:
        return 8;
    case TYPE_MODIFIER_ARRAY:
        return Mem<ArrayTypeExpr>(mem + types[i]).arraySize * GetTypeSizeHelper(structmem, mem, types, i - 1);
    default: {
        TypeExpr members = Mem<StructTypeExpr>(structmem + type).members;
        u32 size = 0;
        while (Mem<TypeExpr>(structmem + members.index).index != 0) {
            TypeExpr member = Mem<TypeExpr>(structmem + members.index);
            TypeExpr memberType = Mem<StructMemberTypeExpr>(structmem + member.index).type;
            size += GetTypeSize(structmem, structmem, memberType);
            members.index += sizeof(TypeExpr);
        }
        return size;
    }
    }
    ASSERT(false);
}

u32 GetTypeSize(byte *structMem, byte *mem, TypeExpr type) {

    u32 i = type.index;
    u32 size = 0;
    while (Mem<TypeName>(mem + i)) {
        size++;
        switch (Mem<TypeName>(mem + i)) {
        case TYPE_MODIFIER_ARRAY: {
            i += sizeof(ArrayTypeExpr);
        } break;
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN: {
            FnTypeExpr *node = (FnTypeExpr *)(mem + i);
            i = node->modifier.index;
        } break;
        default:
            i += sizeof(TypeName);
            break;
        }
    }

    i32 types[size];
    i = type.index;
    size = 0;
    while (Mem<TypeName>(mem + i)) {
        switch (Mem<TypeName>(mem + i)) {
        case TYPE_MODIFIER_ARRAY: {
            types[size++] = i;
            i += sizeof(ArrayTypeExpr);
        } break;
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN: {
            types[size++] = i;
            FnTypeExpr *node = (FnTypeExpr *)(mem + i);
            i = node->modifier.index;
        } break;
        default:
            types[size++] = i;
            i += sizeof(TypeName);
            break;
        }
    }

    return GetTypeSizeHelper(structMem, mem, types, size - 1);
}

const char *GetTypeExprStr(TypeName t) {
    switch (t) {
    case TYPE_STRUCTURE:
        return "TYPE_STRUCTURE";
    case TYPE_STRUCTURE_MEMBER:
        return "TYPE_STRUCTURE_MEMBER";
    case TYPE_PRIMARY_AUTO:
        return "TYPE_PRIMARY_AUTO";
    case TYPE_PRIMARY_VOID:
        return "TYPE_PRIMARY_VOID";
    case TYPE_PRIMARY_CHAR:
        return "TYPE_PRIMARY_CHAR";
    case TYPE_PRIMARY_BOOL:
        return "TYPE_PRIMARY_BOOL";
    case TYPE_PRIMARY_FN:
        return "TYPE_PRIMARY_FN";
    case TYPE_PRIMARY_NATIVE_FN:
        return "TYPE_PRIMARY_NATIVE_FN";
    case TYPE_PRIMARY_INT8:
        return "TYPE_PRIMARY_INT8";
    case TYPE_PRIMARY_INT16:
        return "TYPE_PRIMARY_INT16";
    case TYPE_PRIMARY_INT32:
        return "TYPE_PRIMARY_INT32";
    case TYPE_PRIMARY_INT64:
        return "TYPE_PRIMARY_INT64";
    case TYPE_PRIMARY_UINT8:
        return "TYPE_PRIMARY_UINT8";
    case TYPE_PRIMARY_UINT16:
        return "TYPE_PRIMARY_UINT16";
    case TYPE_PRIMARY_UINT32:
        return "TYPE_PRIMARY_UINT32";
    case TYPE_PRIMARY_UINT64:
        return "TYPE_PRIMARY_UINT64";
    case TYPE_PRIMARY_F32:
        return "TYPE_PRIMARY_F32";
    case TYPE_PRIMARY_F64:
        return "TYPE_PRIMARY_F64";
    case TYPE_MODIFIER_POINTER:
        return "TYPE_MODIFIER_POINTER";
    case TYPE_MODIFIER_RESTRICTED_POINTER:
        return "TYPE_MODIFIER_RESTRICTED_POINTER";
    case TYPE_MODIFIER_ARRAY:
        return "TYPE_MODIFIER_ARRAY";
    default:
        return nullptr;
    }
}
const char *GetTypeExprStrUserMsg(TypeName t) {
    switch (t) {
    case TYPE_STRUCTURE:
        return "struct";
    case TYPE_STRUCTURE_MEMBER:
        return "struct member";
    case TYPE_PRIMARY_AUTO:
        return "auto";
    case TYPE_PRIMARY_VOID:
        return "void";
    case TYPE_PRIMARY_CHAR:
        return "char";
    case TYPE_PRIMARY_BOOL:
        return "bool";
    case TYPE_PRIMARY_FN:
        return "fn";
    case TYPE_PRIMARY_NATIVE_FN:
        return "fn";
    case TYPE_PRIMARY_INT8:
        return "i8";
    case TYPE_PRIMARY_INT16:
        return "i16";
    case TYPE_PRIMARY_INT32:
        return "i32";
    case TYPE_PRIMARY_INT64:
        return "i64";
    case TYPE_PRIMARY_UINT8:
        return "u8";
    case TYPE_PRIMARY_UINT16:
        return "u16";
    case TYPE_PRIMARY_UINT32:
        return "u32";
    case TYPE_PRIMARY_UINT64:
        return "u64";
    case TYPE_PRIMARY_F32:
        return "f32";
    case TYPE_PRIMARY_F64:
        return "f64";
    case TYPE_MODIFIER_POINTER:
        return "pointer";
    case TYPE_MODIFIER_RESTRICTED_POINTER:
        return "restricted pointer";
    case TYPE_MODIFIER_ARRAY:
        return "array";
    default:
        return nullptr;
    }
}
TypeExpr PeelType(byte *structMem, byte *dst, byte *src, TypeExpr srcT, TypeExpr dstT) {

    CpyTypeExpr(dst, dstT, src, srcT);
    TypeExpr nth = GetNthType(dst, dstT, 0);
    Mem<TypeName>(dst + nth.index) = TYPE_NON;
    nth.index += sizeof(TypeName);
    return nth;
}

TypeExpr GetTypeExpr(byte* baseMem, Expr expr, byte *mem, TypeExpr alloc) {

    ExprType q = Mem<ExprType>(baseMem + expr.index);
    switch (q) {
    case EXPRESSION_NULL:
        return {~u32(0)};
    case EXPRESSION_IMMEDIATE: {

        auto node = (ImmediateExpr*)(baseMem + expr.index);
        Mem<TypeName>((byte *)mem + alloc.index) = (TypeName)node->v.type;
        Mem<TypeName>((byte *)mem + alloc.index + sizeof(TypeName)) = TYPE_NON;
        alloc.index += sizeof(TypeName) * 2;
        return alloc;
    }
    case EXPRESSION_LITERAL: {
        auto node = (LiteralExpr*)(baseMem + expr.index);
        switch (node->literal.type) {
        case TOKEN_NUMBER_LITERAL:
            Mem<TypeName>((byte *)mem + alloc.index) = GetLiteralType(node->literal);
            break;
        case TOKEN_BOOL_LITERAL:
            Mem<TypeName>((byte *)mem + alloc.index) = TYPE_PRIMARY_BOOL;
            break;
        case TOKEN_NULL_LITERAL:
            Mem<TypeName>((byte *)mem + alloc.index) = TYPE_PRIMARY_VOID;
            break;
        case TOKEN_CHAR_LITERAL:
            Mem<TypeName>((byte *)mem + alloc.index) = TYPE_PRIMARY_CHAR;
            break;
        case TOKEN_STRING_LITERAL:
            Mem<TypeName>((byte *)mem + alloc.index) = TYPE_PRIMARY_CHAR;
            alloc.index += sizeof(TypeName);
            Mem<TypeName>((byte *)mem + alloc.index) = TYPE_MODIFIER_POINTER;
            alloc.index += sizeof(TypeName);
            Mem<TypeName>((byte *)mem + alloc.index) = TYPE_NON;
            alloc.index += sizeof(TypeName);
            return alloc;
            break;
        default:
            LOG_ASSERT(false, "Unkown literal type");
            break;
        }
        Mem<TypeName>((byte *)mem + alloc.index + sizeof(TypeName)) = TYPE_NON;
        alloc.index += sizeof(TypeName) * 2;
        return alloc;
    }
    case EXPRESSION_UNARY: {
        auto node = (UnaryExpr*)(baseMem + expr.index);
        TypeExpr ex = GetTypeExpr(baseMem, node->primaryExpr, mem, alloc);
        TypeName t = GetLastType((byte *)mem, alloc);
        return ex;
    }
    case EXPRESSION_BINARY: {
        auto node = (BinaryExpr*)(baseMem + expr.index);

        TypeExpr leftAlloc = GetTypeExpr(baseMem, node->left, mem, alloc);
        GetTypeExpr(baseMem, node->right, mem, leftAlloc);

        TypeName left_t = GetLastType(mem, alloc);
        TypeName right_t = GetLastType(mem, leftAlloc);

        switch (node->opr) {
        case TOKEN_SUB_SCRIPT_OPR:
            if (left_t == TYPE_MODIFIER_ARRAY || left_t == TYPE_MODIFIER_POINTER || left_t == TYPE_MODIFIER_RESTRICTED_POINTER)
                return leftAlloc;
            else if (right_t == TYPE_MODIFIER_POINTER || right_t == TYPE_MODIFIER_RESTRICTED_POINTER) {
                return GetTypeExpr(baseMem, node->right, mem, alloc);
            }
        case TOKEN_PLUS:
        case TOKEN_MINUS:
            if (left_t == TYPE_MODIFIER_POINTER || left_t == TYPE_MODIFIER_RESTRICTED_POINTER)
                return leftAlloc;
            else if (right_t == TYPE_MODIFIER_POINTER || right_t == TYPE_MODIFIER_RESTRICTED_POINTER)
                return GetTypeExpr(baseMem, node->right, mem, alloc);
            else
                return leftAlloc;
        case TOKEN_ASTERISK:
        case TOKEN_SLASH:
            return leftAlloc;
        case TOKEN_EQUALS_EQUALS:
        case TOKEN_EXCLAMATION_EQUALS:
            Mem<TypeName>((byte *)mem + alloc.index) = TYPE_PRIMARY_BOOL;
            Mem<TypeName>((byte *)mem + alloc.index + sizeof(TypeName)) = TYPE_NON;
            alloc.index += sizeof(TypeName) * 2;
            return alloc;
        case TOKEN_RSHIFT:
        case TOKEN_RSHIFT_EQUALS:
        case TOKEN_LSHIFT:
        case TOKEN_LSHIFT_EQUALS:
            Mem<TypeName>((byte *)mem + alloc.index) = TYPE_PRIMARY_BOOL;
            Mem<TypeName>((byte *)mem + alloc.index + sizeof(TypeName)) = TYPE_NON;
            alloc.index += sizeof(TypeName) * 2;
            return alloc;
        case TOKEN_LSHIFT_LSHIFT:
        case TOKEN_RSHIFT_RSHIFT:
            return leftAlloc;
        case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
        case TOKEN_AMPERSAND_AMPERSAND:
            Mem<TypeName>((byte *)mem + alloc.index) = TYPE_PRIMARY_BOOL;
            Mem<TypeName>((byte *)mem + alloc.index + sizeof(TypeName)) = TYPE_NON;
            alloc.index += sizeof(TypeName) * 2;
            return alloc;
        case TOKEN_VERTICAL_BAR:
        case TOKEN_AMPERSAND:
        case TOKEN_CIRCUMFLEX:
            return leftAlloc;
        default:
            LOG_ASSERT(false, "Unkown binary operation");
            break;
        }
    }
    case EXPRESSION_CONVERSION: {
        
        auto node = (ConversionExpr*)(baseMem + expr.index);
        GetTypeExpr(baseMem, node->from, mem, alloc);
        TypeName from = GetLastType((byte *)mem, alloc);
        TypeName to = GetLastType(baseMem, node->type);

        if (to == TYPE_PRIMARY_AUTO) {
            Mem<TypeName>((byte *)mem + alloc.index) = TYPE_PRIMARY_AUTO;
            Mem<TypeName>((byte *)mem + alloc.index + sizeof(TypeName)) = TYPE_NON;
            return alloc;
        }
        return CpyTypeExpr((byte *)mem, alloc, baseMem, node->type);
    }
    case EXPRESSION_VARIABLE: {

        auto node = (VariableExpr*)(baseMem + expr.index);
        return CpyTypeExpr((byte *)mem, alloc, baseMem, node->var.type);
    }
    case EXPRESSION_MEMORY_LOAD: {

        auto node = (MemoryLoadExpr*)(baseMem + expr.index);
        GetTypeExpr(baseMem, node->address, mem, alloc);
        TypeExpr nth = GetNthType(mem, alloc, 0);
        Mem<TypeName>(mem + nth.index) = TYPE_NON;

        return {nth.index + sizeof(TypeName)};
    }
    case EXPRESSION_MEMORY_STORE: {
        auto node = (MemoryStoreExpr*)(baseMem + expr.index);
        return GetTypeExpr(baseMem, node->value, mem, alloc);
    }
    case EXPRESSION_VARIABLE_ASSIGNMENT: {

        auto node = (VariableAssignmentExpr*)(baseMem + expr.index);
        return GetTypeExpr(baseMem, node->value, mem, alloc);
    }
    case EXPRESSION_CALL: {

        auto node = (CallExpr*)(baseMem + expr.index);
        TypeExpr calleeEnd = GetTypeExpr(baseMem, node->calleeExpr, mem, alloc);

        FnTypeExpr *fn;
        TypeName callee_t = GetLastType(mem, alloc);
        if (callee_t == TYPE_PRIMARY_FN) {
            //TypeExpr tPtr = GetNthType(mem, alloc, 1);
            //ASSERT(Mem<TypeName>(mem + tPtr.index) == TYPE_PRIMARY_FN);
            fn = (FnTypeExpr *)(mem + alloc.index);
        } else if (callee_t == TYPE_MODIFIER_POINTER || callee_t == TYPE_MODIFIER_RESTRICTED_POINTER) {
            TypeExpr tPtr = GetNthType(mem, alloc, 1);
            callee_t = Mem<TypeName>(mem + tPtr.index);
            fn = (FnTypeExpr *)(mem + tPtr.index);
        } else {
            return calleeEnd;
        }

        CpyTypeExpr(mem, calleeEnd, mem, fn->ret_t);
        return CpyTypeExpr(mem, alloc, mem, calleeEnd);
    }
    case EXPRESSION_MEM_COPY: {

        auto node = (MemCopyExpr*)(baseMem + expr.index);
        return GetTypeExpr(baseMem, node->dst, mem, alloc);
    }
    case EXPRESSION_ADDRESS_OF: {

        auto node = (AddressOf*)(baseMem + expr.index);
        TypeExpr end = GetTypeExpr(baseMem, node->expr, mem, alloc);
        TypeName t = GetLastType(mem, alloc);
        Mem<TypeName>(mem + end.index - sizeof(TypeName)) = TYPE_MODIFIER_POINTER;
        Mem<TypeName>(mem + end.index) = TYPE_NON;
        return {end.index + sizeof(TypeName)};
    }
    case EXPRESSION_PEEL_TYPE: {

        auto node = (PeelTypeExpr*)(baseMem + expr.index);
        GetTypeExpr(baseMem, node->expr, mem, alloc);
        TypeExpr last = GetNthType((byte *)mem, alloc, 0);
        Mem<TypeName>((byte *)mem + last.index) = TYPE_NON;
        last.index += sizeof(TypeName);
        return last;
    }
    case EXPRESSION_GET: {

        auto node = (DotExpr*)(baseMem + expr.index);

        TypeExpr end = GetTypeExpr(baseMem, node->prev, mem, alloc);
        TypeName t = GetLastType(mem, alloc);

        ASSERT(t > TYPE_COUNT);
        StructTypeExpr *structExpr = (StructTypeExpr *)(baseMem + t);

        TypeName member_t = Mem<TypeName>(baseMem + structExpr->members.index);
        for (u32 i = structExpr->members.index; member_t != 0; i += sizeof(TypeExpr)) {
            TypeExpr memberPtr = Mem<TypeExpr>(baseMem + i);
            StructMemberTypeExpr *member = (StructMemberTypeExpr *)(baseMem + memberPtr.index);
            if (TokensEquals(member->name, node->name)) {
                auto ret = CpyTypeExpr(mem, alloc, baseMem, member->type);
                Mem<TypeName>(mem + ret.index - sizeof(TypeName)) = TYPE_MODIFIER_POINTER;
                Mem<TypeName>(mem + ret.index) = TYPE_NON;
                return {ret.index + sizeof(TypeName)};
            }
        }
        ASSERT(false);
    }
    default:
        LOG_ASSERT(false, "Unkown expression type");
        break;
    };

    ASSERT(false);
    return alloc;
}

Expr MakeConversionNodeSimple(Compiler *compiler, Expr expr, TypeName t) {

    Expr ret{compiler->exprAllocator};
    ConversionExpr *conv = AllocateExpr<ConversionExpr>(compiler);
    *conv = {};
    conv->index = EXPRESSION_CONVERSION;
    conv->from = expr;
    conv->type = compiler->basicTypes[t];

    return ret;
}

bool IsUnsigned(TypeName t) {
    return t >= TYPE_PRIMARY_UINT8 && t <= TYPE_PRIMARY_UINT64 || t == TYPE_MODIFIER_POINTER || t == TYPE_MODIFIER_RESTRICTED_POINTER;
}
void VerifyTypes(Compiler* compiler, byte* baseMem, Expr expr, byte *mem) {

    auto q = Mem<ExprType>(baseMem + expr.index);
    switch (q) {
    case EXPRESSION_NULL:
    case EXPRESSION_LITERAL:
        return;
    case EXPRESSION_IMMEDIATE: {

        auto node = (ImmediateExpr*)(baseMem + expr.index);
        TypeName v_t = GetMinimalTypeforValue(node->v);
        ASSERT((TypeName)node->v.type >= v_t);
        break;
    }
    case EXPRESSION_UNARY: {

        auto node = (UnaryExpr*)(baseMem + expr.index);
        VerifyTypes(compiler, baseMem, node->primaryExpr, mem);
        TypeExpr ex = GetTypeExpr(baseMem, node->primaryExpr, mem, {0});
        TypeName t = GetLastType((byte *)mem, {0});

        switch (node->opr) {
        case TOKEN_TILDE:
            Expect(&compiler->parser, IsIntegral(t) || t == TYPE_PRIMARY_BOOL, "unary (~) operator expects boolean or integral expressions");
            break;
        case TOKEN_EXCLAMATION_MARK:
            Expect(&compiler->parser, t == TYPE_PRIMARY_BOOL, "unary (!) operator expects boolean expressions");
            break;
        case TOKEN_NEG:
            Expect(&compiler->parser, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_UINT64) || t == TYPE_PRIMARY_F32 || t == TYPE_PRIMARY_F64, "unary (-) operator expects integral or floating point types");
            break;
        case TOKEN_PLUS_PLUS:
        case TOKEN_MINUS_MINUS:
            Expect(&compiler->parser, IsIntegral(t) || t == TYPE_PRIMARY_F32 || t == TYPE_PRIMARY_F64 || t == TYPE_MODIFIER_POINTER || t == TYPE_MODIFIER_RESTRICTED_POINTER , "operator (++/--) expects integral, pointer or floating point types");
            break;
        }
        break;
    }
    case EXPRESSION_BINARY: {
        auto node = (BinaryExpr*)(baseMem + expr.index);

        VerifyTypes(compiler, baseMem, node->left, mem);
        VerifyTypes(compiler, baseMem, node->right, mem);
        TypeExpr leftAlloc = GetTypeExpr(baseMem, node->left, mem, {0});
        GetTypeExpr(baseMem, node->right, mem, leftAlloc);

        TypeName left_t = GetLastType(mem, {0});
        TypeName right_t = GetLastType(mem, leftAlloc);

        bool isLeftIntegral = IsIntegral((byte *)mem, {0});
        bool isRightIntegral = IsIntegral((byte *)mem, leftAlloc);

        bool cond = (left_t == right_t) ||
                    (left_t == TYPE_MODIFIER_RESTRICTED_POINTER && isRightIntegral) ||
                    (right_t == TYPE_MODIFIER_RESTRICTED_POINTER && isLeftIntegral) ||
                    (left_t == TYPE_MODIFIER_POINTER && isRightIntegral) ||
                    (right_t == TYPE_MODIFIER_POINTER && isLeftIntegral) ||
                    (node->opr == TOKEN_SUB_SCRIPT_OPR && isRightIntegral) ||
                    (isRightIntegral && isRightIntegral);

        if (isLeftIntegral && isRightIntegral && (left_t != right_t)) {
            u32 lSize = GetTypeNameSize(left_t);
            u32 rSize = GetTypeNameSize(right_t);
            if (rSize != lSize) {
                if (lSize > rSize) {
                    node->right = MakeConversionNodeSimple(compiler, node->right, left_t);
                } else {
                    node->left = MakeConversionNodeSimple(compiler, node->left, right_t);
                }
            } else if (IsUnsigned(left_t) != IsUnsigned(right_t)) {
                if (!IsUnsigned(left_t)) {
                    node->left = MakeConversionNodeSimple(compiler, node->left, right_t);
                } else {
                    node->right = MakeConversionNodeSimple(compiler, node->right, left_t);
                }
            }
        }

        Expect(&compiler->parser, cond, "left and right operands have incompatible type");

        switch (node->opr) {
        case TOKEN_SUB_SCRIPT_OPR:
            cond = (left_t == TYPE_MODIFIER_ARRAY && isRightIntegral) ||
                   (left_t == TYPE_MODIFIER_POINTER && isRightIntegral) ||
                   (right_t == TYPE_MODIFIER_POINTER && isLeftIntegral) ||
                   (left_t == TYPE_MODIFIER_RESTRICTED_POINTER && isRightIntegral) ||
                   (right_t == TYPE_MODIFIER_RESTRICTED_POINTER && isLeftIntegral);
            Expect(&compiler->parser, cond, "operator ([]) expects pointer or array and integral type");
            break;
        case TOKEN_PLUS:
        case TOKEN_MINUS:
            cond = (left_t == right_t && isLeftIntegral) ||
                   (isLeftIntegral && isRightIntegral) ||
                   (left_t == TYPE_MODIFIER_POINTER && isRightIntegral) ||
                   (right_t == TYPE_MODIFIER_POINTER && isLeftIntegral) ||
                   (right_t == TYPE_MODIFIER_RESTRICTED_POINTER && isRightIntegral) ||
                   (right_t == TYPE_MODIFIER_RESTRICTED_POINTER && isLeftIntegral) ||
                   (left_t == right_t && (left_t == TYPE_PRIMARY_F32 || left_t == TYPE_PRIMARY_F64));
            Expect(&compiler->parser, cond, "operators (+,-) expect pointer, integral, float type");
            break;
        case TOKEN_ASTERISK:
        case TOKEN_SLASH:
            cond = ((left_t == right_t) || (isLeftIntegral && isRightIntegral)) && (isLeftIntegral || left_t == TYPE_PRIMARY_F32 || right_t == TYPE_PRIMARY_F64);
            Expect(&compiler->parser, cond, "operators (*,/) expect integral or floating point type");
            break;
        case TOKEN_EQUALS_EQUALS:
        case TOKEN_EXCLAMATION_EQUALS:
            cond = (left_t == right_t || isLeftIntegral && isRightIntegral);
            Expect(&compiler->parser, cond, "operators (==,!=) expect same type");
            break;
        case TOKEN_RSHIFT:
        case TOKEN_RSHIFT_EQUALS:
        case TOKEN_LSHIFT:
        case TOKEN_LSHIFT_EQUALS:
            cond = (left_t == right_t || isLeftIntegral && isRightIntegral) && (isLeftIntegral || left_t == TYPE_PRIMARY_F32 || right_t == TYPE_PRIMARY_F64);
            Expect(&compiler->parser, cond, "operators (>,>=,<=) expect integral or floating point type");
            break;
        case TOKEN_LSHIFT_LSHIFT:
        case TOKEN_RSHIFT_RSHIFT:
            cond = isLeftIntegral && isRightIntegral;
            Expect(&compiler->parser, cond, "operators (>>,<<) expect integral types");
            break;
        case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
        case TOKEN_AMPERSAND_AMPERSAND:
            cond = (left_t == right_t) && (left_t == TYPE_PRIMARY_BOOL);
            Expect(&compiler->parser, cond, "operators (||,&&) expect boolean types");
            break;
        case TOKEN_VERTICAL_BAR:
        case TOKEN_AMPERSAND:
        case TOKEN_CIRCUMFLEX:
            cond = (left_t == right_t || isLeftIntegral && isRightIntegral) && (left_t == TYPE_PRIMARY_BOOL || isLeftIntegral);
            Expect(&compiler->parser, cond, "operators (|,&,^) expect integral types");
            break;
        default:
            LOG_ASSERT(false, "Unkown binary operation");
            break;
        }
        break;
    }
    case EXPRESSION_CONVERSION: {

        auto *node = (ConversionExpr*)(baseMem + expr.index);
        VerifyTypes(compiler, baseMem, node->from, mem);
        GetTypeExpr(baseMem, node->from, mem, {0});
        TypeName from = GetLastType((byte *)mem, {0});
        TypeName to = GetLastType(baseMem, node->type);

        Expect(&compiler->parser, !(from == TYPE_PRIMARY_AUTO && to == TYPE_PRIMARY_AUTO), "cannot deduce type");
        Expect(&compiler->parser, from != TYPE_PRIMARY_VOID, "conversion from void is invalid");
        Expect(&compiler->parser, to != TYPE_PRIMARY_VOID, "conversion to void is invalid");
        Expect(&compiler->parser, to != TYPE_MODIFIER_ARRAY, "conversion to array is invalid");
        Expect(&compiler->parser, !((to == TYPE_MODIFIER_POINTER || to == TYPE_MODIFIER_RESTRICTED_POINTER) && (from == TYPE_PRIMARY_F32 || from == TYPE_PRIMARY_F64)), "conversion from floating point to pointer is invalid");
        Expect(&compiler->parser, !((to == TYPE_MODIFIER_POINTER || to == TYPE_MODIFIER_RESTRICTED_POINTER) && (to == TYPE_PRIMARY_F32 || to == TYPE_PRIMARY_F64)), "conversion from pointer to floating point is invalid");
        break;
    }
    case EXPRESSION_VARIABLE:
        break;
    case EXPRESSION_MEMORY_LOAD: {
        auto *node = (MemoryLoadExpr*)(baseMem + expr.index);
        VerifyTypes(compiler, baseMem, node->address, mem);
        GetTypeExpr(baseMem, node->address, mem, {0});
        TypeName add_t = GetLastType(mem, {0});
        Expect(&compiler->parser, add_t == TYPE_MODIFIER_POINTER || add_t == TYPE_MODIFIER_RESTRICTED_POINTER || add_t == TYPE_MODIFIER_ARRAY, "loading from non memory is invalid");
        break;
    }
    case EXPRESSION_MEMORY_STORE: {

        auto *node = (MemoryStoreExpr*)(baseMem + expr.index);
        VerifyTypes(compiler, baseMem, node->address, mem);
        TypeExpr ret = GetTypeExpr(baseMem, node->address, mem, {0});
        TypeName adddress_t = GetLastType(mem, {0});
        Expect(&compiler->parser, adddress_t == TYPE_MODIFIER_POINTER || adddress_t == TYPE_MODIFIER_RESTRICTED_POINTER || adddress_t == TYPE_MODIFIER_ARRAY, "storing to non memory is invalid");

        TypeExpr peeled = GetNthType(mem, {0}, 0);
        Mem<TypeName>(mem + peeled.index) = TYPE_NON;
        TypeName peeledAddress_t = GetLastType(mem, {0});

        VerifyTypes(compiler, baseMem, node->value, mem);
        GetTypeExpr(baseMem, node->value, mem, peeled);
        TypeName rval_t = GetLastType(mem, peeled);

        bool cond = rval_t == peeledAddress_t;
        bool isLvalIntegral = IsIntegral(peeledAddress_t);
        bool isRvalIntegral = IsIntegral(rval_t);

        if (isLvalIntegral && isRvalIntegral) {
            cond = true;
            Warning(&compiler->parser, peeledAddress_t >= rval_t, "narrowing assignment");
        }

        Expect(&compiler->parser, cond, "assignment of incompatible types");
        break;
    }
    case EXPRESSION_VARIABLE_ASSIGNMENT: {

        auto *node = (VariableAssignmentExpr*)(baseMem + expr.index);

        VerifyTypes(compiler, baseMem, node->value, mem);
        GetTypeExpr(baseMem, node->value, mem, {0});
        bool cond = TypesEqual(baseMem, baseMem, baseMem, mem, node->var.type, {0});

        TypeName lval_t = GetLastType(baseMem, node->var.type);
        TypeName rval_t = GetLastType(mem, {0});
        bool isLvalIntegral = IsIntegral(lval_t);
        bool isRvalIntegral = IsIntegral(rval_t);

        if (isLvalIntegral && isRvalIntegral) {
            cond = true;
            Warning(&compiler->parser, lval_t >= rval_t, "narrowing assignment");
            node->value = MakeConversionNodeSimple(compiler, node->value, lval_t);
        }

        Expect(&compiler->parser, lval_t != TYPE_PRIMARY_FN, "functions are immutable");
        Expect(&compiler->parser, cond, "assignment of incompatible types");
        break;
    }
    case EXPRESSION_ADDRESS_OF: {

        auto *node = (AddressOf*)(baseMem + expr.index);
        VerifyTypes(compiler, baseMem, node->expr, mem);
        GetTypeExpr(baseMem, node->expr, mem, {0});
        break;
    }
    case EXPRESSION_CALL: {
        auto *node = (CallExpr*)(baseMem + expr.index);
        VerifyTypes(compiler, baseMem, node->calleeExpr, mem);
        TypeExpr calleeEnd = GetTypeExpr(baseMem, node->calleeExpr, mem, {0});

        FnTypeExpr *fn;
        TypeName callee_t = GetLastType(mem, {0});
        if (callee_t == TYPE_PRIMARY_FN) {
            TypeExpr tPtr = GetNthType(mem, {0}, 1);
            ASSERT(Mem<TypeName>(mem + tPtr.index) == TYPE_PRIMARY_FN);
            fn = (FnTypeExpr *)(mem + tPtr.index);
        } else if (callee_t == TYPE_MODIFIER_POINTER || callee_t == TYPE_MODIFIER_RESTRICTED_POINTER) {
            TypeExpr tPtr = GetNthType(mem, {0}, 1);
            callee_t = Mem<TypeName>(mem + tPtr.index);
            fn = (FnTypeExpr *)(mem + tPtr.index);
            Expect(&compiler->parser, callee_t == TYPE_PRIMARY_FN, "callee expression must be a function or pointer to function type");
        } else {
            break;
        }

        u32 i = 0;
        for (; i < fn->param_count; i++) {

            TypeExpr paramT = Mem<TypeExpr>(mem + fn->params + i * sizeof(TypeExpr));
            Expr arg = Mem<Expr>(baseMem + node->args.index + i * sizeof(Expr));
            ASSERT(paramT.index != 0);

            if (arg.index == 0)
                break;

            VerifyTypes(compiler, baseMem, arg, mem + calleeEnd.index);
            TypeExpr end = GetTypeExpr(baseMem, arg, mem, calleeEnd);
            bool eq = TypesEqual(baseMem,baseMem , mem, mem, calleeEnd, paramT);

            char *err = (char *)(mem + end.index);
            if (!eq) {
                memcpy(err, "function call with incompatible arguments at parameter: ", 56);
                (err + 56)[u64_to_string(err + 56, 32, i)] = 0;
            }
            Expect(&compiler->parser, eq, err);
        }

        Expr lastArg = Mem<Expr>(baseMem + node->args.index + i * sizeof(Expr));
        bool cond = ((i == fn->param_count || fn->params == 0) && lastArg.index == 0);
        Expect(&compiler->parser, cond, "function call argument type mismatch");
        break;
    }
    case EXPRESSION_MEM_COPY: {

        auto node = (MemCopyExpr*)(baseMem + expr.index);

        VerifyTypes(compiler, baseMem, node->dst, mem);
        TypeExpr src = GetTypeExpr(baseMem, node->dst, mem, {0});
        VerifyTypes(compiler, baseMem, node->src, mem);
        GetTypeExpr(baseMem, node->src, mem, src);

        bool eq = TypesEqual(baseMem, baseMem, mem, mem, {0}, src);
        Expect(&compiler->parser, eq, "assignment of incompatible types");
        break;
    }
    case EXPRESSION_PEEL_TYPE: {

        auto node = (PeelTypeExpr*)(baseMem + expr.index);
        VerifyTypes(compiler, baseMem, node->expr, mem);
        break;
    }
    case EXPRESSION_GET: {

        auto node = (DotExpr*)(baseMem + expr.index);
        VerifyTypes(compiler, baseMem, node->prev, mem);

        TypeExpr end = GetTypeExpr(baseMem, node->prev, mem, {0});
        TypeName t = GetLastType(mem, {0});
        if (t == TYPE_MODIFIER_POINTER || t == TYPE_MODIFIER_RESTRICTED_POINTER) {

            byte scratch[end.index];
            PeelType(baseMem, scratch, mem, {0}, {0});
            t = GetLastType(scratch, {0});
        }

        Expect(&compiler->parser, t > TYPE_COUNT, "not a structure");
        StructTypeExpr *structExpr = (StructTypeExpr *)(baseMem + t);

        TypeName member_t = Mem<TypeName>(baseMem + structExpr->members.index);
        bool memberFound = false;
        for (u32 i = structExpr->members.index; member_t != 0; i += sizeof(TypeExpr)) {
            TypeExpr memberPtr = Mem<TypeExpr>(baseMem + i);
            StructMemberTypeExpr *member = (StructMemberTypeExpr *)(baseMem + memberPtr.index);
            if (TokensEquals(member->name, node->name)) {
                memberFound = true;
                break;
            }
        }
        Expect(&compiler->parser, memberFound, "not a member");
    } break;
    default:
        LOG_ASSERT(false, "Unkown expression type");
        break;
    }
}

u32 MemberOffset(byte* baseMem, Token memberName, Expr prev, byte* scratchMem) {

    u32 size = 0;
    while (true) {

        GetTypeExpr(baseMem, prev, scratchMem, TypeExpr{0});
        TypeName ptr = GetLastType(scratchMem, TypeExpr{0});
        StructTypeExpr str = Mem<StructTypeExpr>(baseMem + ptr);
        TypeExpr members = str.members;

        while (Mem<TypeName>(baseMem + members.index) != TYPE_NON) {
            TypeExpr memberPtr = Mem<TypeExpr>(baseMem + members.index);
            StructMemberTypeExpr member = Mem<StructMemberTypeExpr>(baseMem + memberPtr.index);

            if (TokensEquals(member.name, memberName))
                break;
            size += GetTypeSize(baseMem, baseMem, member.type);
            members.index += sizeof(TypeExpr);
        }

        switch (Mem<ExprType>(baseMem + prev.index)) {
        case EXPRESSION_GET: {
            memberName = ((DotExpr*)(baseMem + prev.index))->name;
            prev = ((DotExpr*)(baseMem + prev.index))->prev;
        } break;
        default:
            return size;
        }
    }
}
Expr GetExprFromPtr(byte* baseMem, void *node) {
    return{(byte*)node - baseMem};
}
bool IsMemberOf(byte *structures, byte *mem, TypeExpr st, Token name) {

    TypeExpr members = Mem<StructTypeExpr>(structures + Mem<TypeExpr>(mem + st.index).index).members;

    while (Mem<TypeExpr>(structures + members.index).index != 0) {
        TypeExpr member = Mem<TypeExpr>(structures + members.index);
        Token t = Mem<StructMemberTypeExpr>(structures + member.index).name;

        if (TokensEquals(t, name)) {
            return true;
        }
        members.index += sizeof(TypeExpr);
    }

    return false;
}

bool IsImmOne(Value v) {
    switch (v.type) {
    case TYPE_PRIMARY_INT8:
        return Mem<i8>(v.mem) == 1;
    case TYPE_PRIMARY_INT16:
        return Mem<i16>(v.mem) == 1;
    case TYPE_PRIMARY_INT32:
        return Mem<i32>(v.mem) == 1;
    case TYPE_PRIMARY_INT64:
        return Mem<i64>(v.mem) == 1;
    case TYPE_PRIMARY_UINT8:
        return Mem<u8>(v.mem) == 1;
    case TYPE_PRIMARY_UINT16:
        return Mem<u16>(v.mem) == 1;
    case TYPE_PRIMARY_UINT32:
        return Mem<u32>(v.mem) == 1;
    case TYPE_PRIMARY_UINT64:
        return Mem<u64>(v.mem) == 1;
    case TYPE_PRIMARY_F32:
        return Mem<f32>(v.mem) == 1;
    case TYPE_PRIMARY_F64:
        return Mem<f64>(v.mem) == 1;
    default:
        break;
    }
}
bool IsImmZero(Value v) {
    switch (v.type) {
    case TYPE_PRIMARY_INT8:
        return Mem<i8>(v.mem) == 0;
    case TYPE_PRIMARY_INT16:
        return Mem<i16>(v.mem) == 0;
    case TYPE_PRIMARY_INT32:
        return Mem<i32>(v.mem) == 0;
    case TYPE_PRIMARY_INT64:
        return Mem<i64>(v.mem) == 0;
    case TYPE_PRIMARY_UINT8:
        return Mem<u8>(v.mem) == 0;
    case TYPE_PRIMARY_UINT16:
        return Mem<u16>(v.mem) == 0;
    case TYPE_PRIMARY_UINT32:
        return Mem<u32>(v.mem) == 0;
    case TYPE_PRIMARY_UINT64:
        return Mem<u64>(v.mem) == 0;
    case TYPE_PRIMARY_F32:
        return Mem<f32>(v.mem) == 0;
    case TYPE_PRIMARY_F64:
        return Mem<f64>(v.mem) == 0;
    case TYPE_PRIMARY_BOOL:
        return Mem<bool>(v.mem) == 0;
    default:
        break;
    }
}
Value MakeImm(TypeName t, i64 v) {

    Value ret;
    ret.type = t;
    switch (t) {
    case TYPE_PRIMARY_CHAR:
        Mem<char>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_BOOL:
        Mem<bool>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_INT8:
        Mem<i8>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_INT16:
        Mem<i16>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_INT32:
        Mem<i32>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_INT64:
        Mem<i64>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_UINT8:
        Mem<u8>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_UINT16:
        Mem<u16>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_UINT32:
        Mem<u32>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_UINT64:
        Mem<u64>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_F32:
        Mem<f32>(ret.mem) = v;
        break;
    case TYPE_PRIMARY_F64:
        Mem<f64>(ret.mem) = v;
        break;
    default:
        break;
    }
    return ret;
}
u64 HashExpr(byte* baseMem, Expr e) {

    u64 hash = 7;
    ExprType e_t = Mem<ExprType>(baseMem + e.index);
    switch (e_t) {
    case EXPRESSION_NULL: {
        return 0;
    }
    case EXPRESSION_LITERAL: {
        auto *node = (LiteralExpr*)(baseMem + e.index);
        hash += str_hash(node->literal.text, node->literal.lenght);
        return hash;
    }
    case EXPRESSION_IMMEDIATE: {
        auto *node = (ImmediateExpr*)(baseMem + e.index);
        hash += HashValue(node->v);
        return hash;
    }
    case EXPRESSION_UNARY: {
        auto *node = (UnaryExpr*)(baseMem + e.index);
        hash += node->opr;
        hash += HashExpr(baseMem, node->primaryExpr) * 31;
        return hash;
    }
    case EXPRESSION_BINARY: {
        auto *node = (BinaryExpr*)(baseMem + e.index);
        hash += node->opr;
        hash += HashExpr(baseMem, node->left) * 31;
        hash += HashExpr(baseMem, node->right) * 31;
        return hash;
    }
    case EXPRESSION_VARIABLE: {
        auto *node = (VariableExpr*)(baseMem + e.index);
        hash += str_hash(node->var.name.text, node->var.name.lenght) * 31;
        return hash;
    }
    case EXPRESSION_VARIABLE_ASSIGNMENT: {
        auto *node = (VariableAssignmentExpr*)(baseMem + e.index);
        hash += str_hash(node->var.name.text, node->var.name.lenght);
        hash += HashExpr(baseMem, node->value) * 31;
        return hash;
    }
    case EXPRESSION_MEMORY_LOAD: {
        auto *node = (MemoryLoadExpr*)(baseMem + e.index);
        hash += HashExpr(baseMem, node->address) * 31;
        return hash;
    }
    case EXPRESSION_MEMORY_STORE: {
        auto *node = (MemoryStoreExpr*)(baseMem + e.index);
        hash += HashExpr(baseMem, node->value) * 31;
        hash += HashExpr(baseMem, node->address) * 31;
        return hash;
    }
    case EXPRESSION_ADDRESS_OF: {
        auto *node = (AddressOf*)(baseMem + e.index);
        hash += HashExpr(baseMem, node->expr) * 31;
        break;
    }
    case EXPRESSION_GET: {
        auto *node = (DotExpr*)(baseMem + e.index);
        hash += HashExpr(baseMem, node->prev) * 31;
        hash = str_hash(node->name.text, node->name.lenght);
        return hash;
    }
    case EXPRESSION_PEEL_TYPE: {
        auto *node = (PeelTypeExpr*)(baseMem + e.index);
        return HashExpr(baseMem, node->expr);
    }
    case EXPRESSION_MEM_COPY: {
        auto *node = (MemCopyExpr*)(baseMem + e.index);

        hash += node->size * 31;
        hash += HashExpr(baseMem, node->dst) * 31;
        hash += HashExpr(baseMem, node->src) * 31;
        return hash;
    }
    case EXPRESSION_CALL: {
        auto *node = (CallExpr*)(baseMem + e.index);

        hash += HashExpr(baseMem, node->calleeExpr) * 31;
        Expr i = node->args;
        while (Mem<Expr>(baseMem + i.index).index != 0) {
            hash += HashExpr(baseMem, Mem<Expr>(baseMem + i.index)) * 31;
            i.index += sizeof(Expr);
        }
        return hash;
    }
    case EXPRESSION_CONVERSION: {
        auto *node = (ConversionExpr*)(baseMem + e.index);
        hash += HashExpr(baseMem, node->from) * 31;
        hash += HashType(baseMem, node->type);
        return hash;
    }
    }
}

Expr Cannonicalize(Compiler *compiler, byte* baseMem, Expr e, Expr *bins, Expr *leafs);
void CannonicalizeHelper(Compiler *compiler, byte* baseMem, TokenType opr, Expr e, Expr *bins, Expr *leafs) {

    ExprType e_t = Mem<ExprType>(baseMem + e.index);
    auto bin = (BinaryExpr*)(baseMem + e.index);

    if (e_t == EXPRESSION_BINARY) {

        if (bin->opr == opr) {
            bins[++bins[0].index] = e;
            CannonicalizeHelper(compiler, baseMem, opr, bin->left, bins, leafs);
            CannonicalizeHelper(compiler, baseMem, opr, bin->right, bins, leafs);
        } else {
            u32 leafCount = ++leafs[0].index;
            bins[bins[0].index + 1].index = 0;
            leafs[leafs[0].index + 1].index = 0;
            leafs[leafCount] = Cannonicalize(compiler, baseMem, e, bins + bins[0].index + 1, leafs + leafs[0].index + 1);
        }
    } else {
        u32 leafCount = ++leafs[0].index;
        bins[bins[0].index + 1].index = 0;
        leafs[leafs[0].index + 1].index = 0;
        leafs[leafCount] = Cannonicalize(compiler, baseMem, e, bins + bins[0].index + 1, leafs + leafs[0].index + 1);
    }
}
Expr Cannonicalize(Compiler *compiler, byte* baseMem, Expr e, Expr *bins, Expr *leafs) {

    ExprType e_t = Mem<ExprType>(baseMem + e.index);
    switch (e_t) {
    case EXPRESSION_NULL: {
        return {0};
    }
    case EXPRESSION_IMMEDIATE: {
        return e;
    }
    case EXPRESSION_LITERAL: {
        return e;
    }
    case EXPRESSION_UNARY: {
        auto node = (UnaryExpr*)(baseMem + e.index);
        node->primaryExpr = Cannonicalize(compiler, baseMem, node->primaryExpr, bins, leafs);
        return e;
    }
    case EXPRESSION_BINARY: {

        auto bin = (BinaryExpr*)(baseMem + e.index);
        if(IsOprCommutative(bin->opr)) {

            CannonicalizeHelper(compiler, baseMem, bin->opr, bin->left, bins, leafs);
            CannonicalizeHelper(compiler, baseMem, bin->opr, bin->right, bins, leafs);
            bins[++bins[0].index] = e;

            u32 leafC = leafs[0].index;
            u64 leafHashes[leafC]{};
            for (u32 i = 0; i < leafC; i++) {
                leafHashes[i] = HashExpr(baseMem, leafs[i + 1]);
            }

            for (u32 i = 0; i < leafC - 1; i++) {
                u32 min = i;
                for (u32 k = i + 1; k < leafC; k++) {
                    if (leafHashes[k] < leafHashes[min]) {
                        min = k;
                    }
                }

                auto tmp = leafHashes[min];
                leafHashes[min] = leafHashes[i];
                leafHashes[i] = tmp;

                auto leafTmp = leafs[min + 1];
                leafs[min + 1] = leafs[i + 1];
                leafs[i + 1] = leafTmp;
            }

            u32 binCount = bins[0].index;
            ASSERT(leafC != 0);
            Expr last = leafs[1];
            for (u32 i = 2; i < leafC + 1; i++) {

                BinaryExpr *b;
                if (binCount > 0) {
                    b = (BinaryExpr*)(baseMem + bins[binCount].index);
                    binCount--;
                } else {
                    b = AllocateExpr<BinaryExpr>(compiler);
                }

                b->left = last;
                b->right = leafs[i];

                last = GetExprFromPtr(baseMem, b);
            }

            return last;
        }
        return e;
    }
    case EXPRESSION_VARIABLE: {
        auto v = (VariableExpr*)(baseMem + e.index);
        return e;
    }
    case EXPRESSION_VARIABLE_ASSIGNMENT: {
        auto *node = (VariableAssignmentExpr*)(baseMem + e.index);
        node->value = Cannonicalize(compiler, baseMem, node->value, bins, leafs);
        return e;
    }
    case EXPRESSION_MEMORY_LOAD: {
        auto *node = (MemoryLoadExpr*)(baseMem + e.index);
        node->address = Cannonicalize(compiler, baseMem, node->address, bins, leafs);
        return e;
    }
    case EXPRESSION_MEMORY_STORE: {
        auto *node = (MemoryStoreExpr*)(baseMem + e.index);
        node->address = Cannonicalize(compiler, baseMem, node->address, bins, leafs);
        node->value = Cannonicalize(compiler, baseMem, node->value, bins, leafs);
        return e;
    }
    case EXPRESSION_ADDRESS_OF: {
        auto *node = (AddressOf*)(baseMem + e.index);
        node->expr = Cannonicalize(compiler, baseMem, node->expr, bins, leafs);
        return e;
    }
    case EXPRESSION_GET: {
        auto *node = (DotExpr*)(baseMem + e.index);
        node->prev = Cannonicalize(compiler, baseMem, node->prev, bins, leafs);
        return e;
    }
    case EXPRESSION_PEEL_TYPE: {
        auto *node = (PeelTypeExpr*)(baseMem + e.index);
        node->expr = Cannonicalize(compiler, baseMem, node->expr, bins, leafs);
        return e;
    }
    case EXPRESSION_MEM_COPY: {
        auto *node = (MemCopyExpr*)(baseMem + e.index);
        node->dst = Cannonicalize(compiler, baseMem, node->dst, bins, leafs);
        node->src = Cannonicalize(compiler, baseMem, node->src, bins, leafs);
        return e;
    }
    case EXPRESSION_CALL: {
        auto *node = (CallExpr*)(baseMem + e.index);

        node->calleeExpr = Cannonicalize(compiler, baseMem, node->calleeExpr, bins, leafs);
        Expr i = node->args;
        while (Mem<Expr>(baseMem + i.index).index != 0) {
            Mem<Expr>(baseMem + i.index) = Cannonicalize(compiler, baseMem, Mem<Expr>(baseMem + i.index), bins, leafs);
            i.index += sizeof(Expr);
        }
        return e;
    }
    case EXPRESSION_CONVERSION: {
        auto *node = (ConversionExpr*)(baseMem + e.index);
        node->from = Cannonicalize(compiler, baseMem, node->from, bins, leafs);
        return e;
    }
    }
    ASSERT(false);
    return e;
}

Expr Expression(Compiler *compiler);
Expr PrimaryExpression(Compiler *compiler) {

    Expr expr{EXPRESSION_NULL};

    auto parser = &compiler->parser;
    TokenType literals[5] = {TOKEN_BOOL_LITERAL, TOKEN_STRING_LITERAL, TOKEN_NULL_LITERAL, TOKEN_NUMBER_LITERAL, TOKEN_CHAR_LITERAL};
    TokenType open = TOKEN_OPEN_PAREN;
    TokenType cast = TOKEN_LSHIFT;
    TokenType identifier = TOKEN_IDENTIFIER;
    TokenType asterisk = TOKEN_ASTERISK;
    TokenType addressOf = TOKEN_AMPERSAND;

    if (Match(parser, literals, 5)) {

        expr = {compiler->exprAllocator};
        LiteralExpr *node = AllocateExpr<LiteralExpr>(compiler);
        node->index = EXPRESSION_LITERAL;
        node->literal = PreviousToken(parser);
    }
    else if (Match(parser, &cast, 1)) {

        expr = {compiler->exprAllocator};
        ConversionExpr *node = AllocateExpr<ConversionExpr>(compiler);
        node->index = EXPRESSION_CONVERSION;
        node->type = ParseTypeExpression(compiler);
        ExpectToken(parser, TOKEN_RSHIFT);
        node->from = PrimaryExpression(compiler);
    }
    else if (Match(parser, &open, 1)) {
        expr = Expression(compiler);
        ExpectToken(parser, TOKEN_CLOSE_PAREN);
    }
    else if (Match(parser, &identifier, 1)) {

        expr = {compiler->exprAllocator};
        auto *node = AllocateExpr<VariableExpr>(compiler);
        node->index = EXPRESSION_VARIABLE;
        node->var.name = PreviousToken(parser);

        u32 index = FindSymbol(&compiler->symbolTable, node->var.name);
        if (index == ~u32(0)) {
            parser->error = true;
            global_print("%s%s*%s%i%\n", "ERROR: unknown variable(", node->var.name.text, node->var.name.lenght, ") referenced in expression at: ", parser->tokenizer.line);
            compiler->error = true;
            node->var.type = compiler->basicTypes[TYPE_PRIMARY_VOID];
        }
        else {
            node->var = compiler->symbolTable[index];
        }

    }
    else if (Match(parser, &asterisk, 1)) {

        expr = PrimaryExpression(compiler);
        GetTypeExpr(compiler->mem, expr, compiler->scratchMem, TypeExpr{0});
        TypeName last = GetLastType(compiler->scratchMem, TypeExpr{0});

        Expect(parser, last == TYPE_MODIFIER_POINTER || last == TYPE_MODIFIER_RESTRICTED_POINTER, "operator (*) expects pointer type");
        if (last == TYPE_MODIFIER_POINTER || last == TYPE_MODIFIER_RESTRICTED_POINTER) {
            TypeExpr arr = GetNthType(compiler->scratchMem, {0}, 1);
            TypeName arr_t = Mem<TypeName>(compiler->scratchMem + arr.index);
            Expr ret{compiler->exprAllocator};
            if (arr_t != TYPE_MODIFIER_ARRAY && arr_t < TYPE_COUNT) {

                MemoryLoadExpr *node = AllocateExpr<MemoryLoadExpr>(compiler);
                node->index = EXPRESSION_MEMORY_LOAD;
                node->address = expr;
            }
            else {
                PeelTypeExpr *node = AllocateExpr<PeelTypeExpr>(compiler);
                node->index = EXPRESSION_PEEL_TYPE;
                node->expr = expr;
            }
            expr = ret;
        }
    }
    else if (Match(parser, &addressOf, 1)) {
        expr = PrimaryExpression(compiler);

        ExprType t = FirstNotGroupExpr(compiler->mem, &expr);
        bool cond = (t == EXPRESSION_MEMORY_LOAD || t == EXPRESSION_VARIABLE);
        if(t == EXPRESSION_VARIABLE) {
            VariableExpr* varExpr = (VariableExpr*)(compiler->mem + expr.index);
            auto var_t = GetLastType(compiler->mem, varExpr->var.type);
            cond &= (var_t > TYPE_COUNT || var_t == TYPE_PRIMARY_FN || var_t == TYPE_MODIFIER_ARRAY);
        }
        else if(t == EXPRESSION_MEMORY_LOAD) {
            MemoryLoadExpr* loadExpr = (MemoryLoadExpr*)(compiler->mem + expr.index);
            loadExpr->index = EXPRESSION_PEEL_TYPE;
        }
        Expect(parser, cond, "operator (&) expects structs,arrays or functions");

        Expr ret{compiler->exprAllocator};
        AddressOf *addressOf = AllocateExpr<AddressOf>(compiler);
        addressOf->index = EXPRESSION_ADDRESS_OF;
        addressOf->expr = expr;
        expr = ret;
    }

    return expr;
}
Expr RestOfCall(Compiler *compiler, Expr callee) {

    auto *parser = &compiler->parser;

    CallExpr *call = AllocateExpr<CallExpr>(compiler);
    call->index = EXPRESSION_CALL;
    call->calleeExpr = callee;

    u32 argCount = 0;
    ParserState save = SaveParserState(parser, compiler->exprAllocator);
    if (!Check(parser, TOKEN_CLOSE_PAREN)) {
        do {
            argCount++;
            Expression(compiler);
        } while (NextToken(parser).type == TOKEN_COMMA);
    }
    RestoreParserState(parser, save);
    compiler->exprAllocator = save.allocator;

    Expr args{compiler->exprAllocator + (u32)sizeof(Expr)};
    compiler->exprAllocator += sizeof(Expr) * (argCount + 1);
    AllocateExpr<Expr>(compiler)->index = EXPRESSION_NULL;

    u32 i = args.index;
    if (!Check(parser, TOKEN_CLOSE_PAREN)) {
        do {
            Mem<Expr>(compiler->mem + i) = Expression(compiler);
            i += sizeof(Expr);
        } while (NextToken(parser).type == TOKEN_COMMA);

        TokenType t = parser->tokenBuffer.Back().type;
        Expect(parser, t == TOKEN_CLOSE_PAREN, "expected )");
    } else {
        NextToken(parser);
    }
    call->args = args;
    {
        GetTypeExpr(compiler->mem, call->calleeExpr, compiler->scratchMem, TypeExpr{0});

        FnTypeExpr *fn = (FnTypeExpr*)(compiler->scratchMem);
        TypeName retT = GetLastType(compiler->scratchMem, fn->ret_t);
        if (retT == TYPE_MODIFIER_ARRAY || retT > TYPE_COUNT) {

            call->args.index -= sizeof(Expr);
            Mem<Expr>(compiler->mem + call->args.index).index = compiler->exprAllocator;

            ConversionExpr *conv = AllocateExpr<ConversionExpr>(compiler);
            conv->from.index = compiler->exprAllocator;
            VariableExpr *hidden = AllocateExpr<VariableExpr>(compiler);

            conv->index = EXPRESSION_CONVERSION;
            conv->type.index = compiler->exprAllocator;
            TypeExpr end = CpyTypeExpr(compiler->mem, conv->type, compiler->scratchMem, fn->ret_t);
            Mem<TypeName>(compiler->mem + end.index - sizeof(TypeName)) = TYPE_MODIFIER_POINTER;
            Mem<TypeName>(compiler->mem + end.index) = TYPE_NON;
            compiler->exprAllocator = end.index + sizeof(TypeName);

            hidden->index = EXPRESSION_VARIABLE;
            hidden->var.type.index = compiler->exprAllocator;
            end = CpyTypeExpr(compiler->mem, hidden->var.type, compiler->scratchMem, fn->ret_t);
            compiler->exprAllocator = end.index + sizeof(TypeName);
        }
    }

    return GetExprFromPtr(compiler->mem, call);
}

Expr EqualityExpression(Compiler *compiler);
Expr MemoryExpression(Compiler *compiler) {

    auto parser = &compiler->parser;
    Expr expr = PrimaryExpression(compiler);
    TokenType openBracket = TOKEN_OPEN_BRACKET;
    TokenType dot = TOKEN_DOT;
    TokenType arrow = TOKEN_KEYWORD_ARROW;

    Token peek = PeekToken(parser->tokenizer);
    if (peek.type == TOKEN_OPEN_BRACKET || peek.type == TOKEN_DOT || peek.type == TOKEN_KEYWORD_ARROW) {

        Expr address = expr;

        while (peek.type == dot || peek.type == openBracket || peek.type == arrow) {

            if (Match(parser, &openBracket, 1)) {

                GetTypeExpr(compiler->mem, address, compiler->scratchMem, {0});
                TypeName t = GetLastType(compiler->scratchMem, {0});

                Expect(parser, t == TYPE_MODIFIER_ARRAY || t == TYPE_MODIFIER_POINTER || t == TYPE_MODIFIER_RESTRICTED_POINTER, "operator ([]) expects pointer or array types");

                TypeExpr peeled = GetNthType(compiler->scratchMem, {0}, 1);
                t = Mem<TypeName>(compiler->scratchMem + peeled.index);
                Expr curr{compiler->exprAllocator};
                if (t != TYPE_MODIFIER_ARRAY && t < TYPE_COUNT) {

                    MemoryLoadExpr *load = AllocateExpr<MemoryLoadExpr>(compiler);
                    load->index = EXPRESSION_MEMORY_LOAD;
                    load->address = {compiler->exprAllocator};
                } else {
                    PeelTypeExpr *peel = AllocateExpr<PeelTypeExpr>(compiler);
                    peel->expr = {compiler->exprAllocator};
                    peel->index = EXPRESSION_PEEL_TYPE;
                }

                BinaryExpr *sub = AllocateExpr<BinaryExpr>(compiler);
                sub->index = EXPRESSION_BINARY;
                sub->opr = TOKEN_SUB_SCRIPT_OPR;
                sub->left = address;
                sub->right = EqualityExpression(compiler);

                ExpectToken(parser, TOKEN_CLOSE_BRACKET);

                address = curr;
            } else if (Match(parser, &dot, 1)) {

                ExpectToken(parser, TOKEN_IDENTIFIER);
                Expr curr{compiler->exprAllocator};

                DotExpr *memberExpr = AllocateExpr<DotExpr>(compiler);
                memberExpr->index = EXPRESSION_GET;
                memberExpr->prev = address;
                memberExpr->name = PreviousToken(parser);
                memberExpr->offset = MemberOffset(compiler->mem, memberExpr->name, address, compiler->scratchMem);

                GetTypeExpr(compiler->mem, curr, compiler->scratchMem, {0});
                TypeExpr nth = GetNthType(compiler->scratchMem, {0}, 0);
                Mem<TypeName>(compiler->scratchMem + nth.index) = TYPE_NON;
                TypeName t = GetLastType(compiler->scratchMem, {0});

                if (t != TYPE_MODIFIER_ARRAY && t < TYPE_COUNT) {
                    address = {compiler->exprAllocator};
                    MemoryLoadExpr *load = AllocateExpr<MemoryLoadExpr>(compiler);
                    load->index = EXPRESSION_MEMORY_LOAD;
                    load->address = curr;
                } else {
                    address = {compiler->exprAllocator};
                    PeelTypeExpr *peel = AllocateExpr<PeelTypeExpr>(compiler);
                    peel->index = EXPRESSION_PEEL_TYPE;
                    peel->expr = curr;
                }
            }
            else if(Match(parser, &arrow, 1)) {

                ExpectToken(parser, TOKEN_IDENTIFIER);

                GetTypeExpr(compiler->mem, address, compiler->scratchMem, {0});
                TypeName t = GetLastType(compiler->scratchMem, {0});

                Expr r{compiler->exprAllocator};
                PeelTypeExpr *peel = AllocateExpr<PeelTypeExpr>(compiler);
                peel->index = EXPRESSION_PEEL_TYPE;
                peel->expr = address;

                Expr curr{compiler->exprAllocator};
                DotExpr *memberExpr = AllocateExpr<DotExpr>(compiler);
                memberExpr->index = EXPRESSION_GET;
                memberExpr->prev = r;
                memberExpr->name = PreviousToken(parser);
                memberExpr->offset = MemberOffset(compiler->mem, memberExpr->name, r, compiler->scratchMem);

                if (t != TYPE_MODIFIER_ARRAY && t < TYPE_COUNT) {
                    address = {compiler->exprAllocator};
                    MemoryLoadExpr *load = AllocateExpr<MemoryLoadExpr>(compiler);
                    load->index = EXPRESSION_MEMORY_LOAD;
                    load->address = curr;
                    curr = address;
                }
                else {
                    
                    Expr r{compiler->exprAllocator};
                    PeelTypeExpr *peel = AllocateExpr<PeelTypeExpr>(compiler);
                    peel->index = EXPRESSION_PEEL_TYPE;
                    peel->expr = curr;
                    curr = r;
                }
            }
            peek = PeekToken(parser->tokenizer);
        }
        return address;
    }

    return expr;
}

Expr CallExpression(Compiler *compiler) {

    Expr expr = MemoryExpression(compiler);
    for (;;) {

        if (Check(&compiler->parser, TOKEN_OPEN_PAREN)) {
            NextToken(&compiler->parser);
            expr = RestOfCall(compiler, expr);
        } else {
            break;
        }
    }
    return expr;
}

Expr UnaryExpression(Compiler *compiler) {


    auto parser = &compiler->parser;
    TokenType unaries[4] = {TOKEN_EXCLAMATION_MARK, TOKEN_MINUS, TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS};
    if (Match(parser, unaries + 2, 2)) {

        TokenType opr = (PreviousToken(parser).type == TOKEN_PLUS_PLUS) ? TOKEN_PLUS : TOKEN_MINUS;

        Expr lval = CallExpression(compiler);
        Expr lvalCpy = lval;
        ExprType r = GetLastExprType(compiler->mem, &lvalCpy);

        Expr oneExpr{compiler->exprAllocator};
        ImmediateExpr *one = AllocateExpr<ImmediateExpr>(compiler);
        one->index = EXPRESSION_IMMEDIATE;

        Expr bin{compiler->exprAllocator};
        BinaryExpr *inc = AllocateExpr<BinaryExpr>(compiler);
        inc->index = EXPRESSION_BINARY;
        inc->left = lval;
        inc->opr = opr;
        inc->right = oneExpr;
        if (r == EXPRESSION_VARIABLE) {

            auto *varExpr = (VariableExpr*)(compiler->mem + lvalCpy.index);
            one->v = MakeImm(GetLastType(compiler->mem, varExpr->var.type), 1);

            Expr ret{compiler->exprAllocator};
            auto *assignExpr = AllocateExpr<VariableAssignmentExpr>(compiler);
            assignExpr->index = EXPRESSION_VARIABLE_ASSIGNMENT;
            assignExpr->var = varExpr->var;
            assignExpr->value = bin;
            return ret;
        } else if (r == EXPRESSION_MEMORY_LOAD) {

            auto *load = (MemoryLoadExpr*)(compiler->mem + lvalCpy.index);
            Expr ret{compiler->exprAllocator};
            MemoryStoreExpr *store = AllocateExpr<MemoryStoreExpr>(compiler);
            store->index = EXPRESSION_MEMORY_STORE;
            store->address = load->address;
            store->value = bin;
            return ret;
        }
    }
    while (Match(parser, unaries, 2)) {

        auto *node = AllocateExpr<UnaryExpr>(compiler);
        node->index = EXPRESSION_UNARY;
        node->opr = PreviousToken(parser).type;
        if(node->opr == TOKEN_MINUS) {
            node->opr = TOKEN_NEG;
        }
        node->primaryExpr = CallExpression(compiler);

        return GetExprFromPtr(compiler->mem, node);
    }

    return CallExpression(compiler);
}

Expr FactorExpression(Compiler *compiler) {

    auto parser = &compiler->parser;
    Expr expr = UnaryExpression(compiler);

    TokenType binarie[2] = {TOKEN_ASTERISK, TOKEN_SLASH};
    while (Match(parser, binarie, 2)) {

        BinaryExpr *node = AllocateExpr<BinaryExpr>(compiler);
        node->index = EXPRESSION_BINARY;
        node->opr = PreviousToken(parser).type;
        node->left = expr;
        node->right = UnaryExpression(compiler);

        expr = GetExprFromPtr(compiler->mem, node);
    }

    return expr;
}

void DeSugarPointerArithmetic(Compiler *compiler, BinaryExpr *bin, byte *mem, bool s) {

    TypeExpr alloc = GetTypeExpr(compiler->mem, bin->left, mem, {0});
    GetTypeExpr(compiler->mem, bin->right, mem, alloc);

    TypeName left_t = GetLastType(mem, {0});
    TypeName right_t = GetLastType(mem, alloc);

    bool l = s ? (left_t == TYPE_MODIFIER_POINTER || left_t == TYPE_MODIFIER_RESTRICTED_POINTER) : (left_t == TYPE_MODIFIER_RESTRICTED_POINTER || left_t == TYPE_MODIFIER_POINTER || left_t == TYPE_MODIFIER_ARRAY);
    bool r = s ? (right_t == TYPE_MODIFIER_POINTER || right_t == TYPE_MODIFIER_RESTRICTED_POINTER) : (right_t == TYPE_MODIFIER_RESTRICTED_POINTER || right_t == TYPE_MODIFIER_POINTER || right_t == TYPE_MODIFIER_ARRAY);

    if ((l ^ r) && (IsIntegral(left_t) ^ IsIntegral(right_t)) &&
        (bin->opr == TOKEN_PLUS || bin->opr == TOKEN_MINUS)) {

        BinaryExpr *scale = AllocateExpr<BinaryExpr>(compiler);
        scale->index = EXPRESSION_BINARY;
        scale->opr = TOKEN_ASTERISK;
        scale->left = {compiler->exprAllocator};

        ImmediateExpr *imm = AllocateExpr<ImmediateExpr>(compiler);
        imm->index = EXPRESSION_IMMEDIATE;

        u32 scalerSize;
        if (l) {
            TypeExpr last = GetNthType(mem, TypeExpr{0}, 0);
            Mem<TypeName>(mem + last.index) = TYPE_NON;
            scalerSize = GetTypeSize(compiler->mem, mem, {0});

            scale->right = bin->right;
            bin->right = GetExprFromPtr(compiler->mem, scale);
        } else if (r) {
            TypeExpr last = GetNthType(mem, alloc, 0);
            TypeExpr nth = GetNthType(mem, alloc, 1);
            Mem<TypeName>(mem + last.index) = TYPE_NON;
            scalerSize = GetTypeSize(compiler->mem, mem, nth);

            scale->right = bin->left;
            bin->left = GetExprFromPtr(compiler->mem, scale);
        }
        imm->v.type = TYPE_PRIMARY_INT64;
        Mem<i64>(imm->v.mem) = scalerSize;
    }
}

Expr TermExpression(Compiler *compiler) {


    Expr expr = FactorExpression(compiler);
    TokenType binaries[7] = {TOKEN_PLUS, TOKEN_MINUS, TOKEN_AMPERSAND_AMPERSAND, TOKEN_VERTICAL_BAR_VERTICAL_BAR, TOKEN_CIRCUMFLEX, TOKEN_AMPERSAND, TOKEN_VERTICAL_BAR};
    while (Match(&compiler->parser, binaries, 7)) {

        BinaryExpr *node = AllocateExpr<BinaryExpr>(compiler);
        node->index = EXPRESSION_BINARY;
        node->opr = PreviousToken(&compiler->parser).type;
        node->left = expr;
        node->right = FactorExpression(compiler);

        if (node->opr == TOKEN_PLUS || node->opr == TOKEN_MINUS) {
            DeSugarPointerArithmetic(compiler, node, compiler->scratchMem, true);
        }

        expr = GetExprFromPtr(compiler->mem, node);
    }

    return expr;
}
Expr ShiftExpression(Compiler *compiler) {

    Expr expr = TermExpression(compiler);

    TokenType shifts[2] = {TOKEN_LSHIFT_LSHIFT, TOKEN_RSHIFT_RSHIFT};
    while (Match(&compiler->parser, shifts, 2)) {

        BinaryExpr *node = AllocateExpr<BinaryExpr>(compiler);
        node->index = EXPRESSION_BINARY;
        node->opr = PreviousToken(&compiler->parser).type;
        node->left = expr;
        node->right = TermExpression(compiler);

        expr = GetExprFromPtr(compiler->mem, node);
    }

    return expr;
}

Expr ComparisonExpression(Compiler *compiler) {

    Expr expr = ShiftExpression(compiler);

    TokenType comp[4] = {TOKEN_RSHIFT, TOKEN_LSHIFT, TOKEN_RSHIFT_EQUALS, TOKEN_LSHIFT_EQUALS};
    while (Match(&compiler->parser, comp, 4)) {

        BinaryExpr *node = AllocateExpr<BinaryExpr>(compiler);
        node->index = EXPRESSION_BINARY;
        node->opr = PreviousToken(&compiler->parser).type;
        node->left = expr;
        node->right = ShiftExpression(compiler);

        expr = GetExprFromPtr(compiler->mem, node);
    }

    return expr;
}

Expr EqualityExpression(Compiler *compiler) {

    Expr expr = ComparisonExpression(compiler);
    TokenType eq[2]{TOKEN_EXCLAMATION_EQUALS, TOKEN_EQUALS_EQUALS};

    while (Match(&compiler->parser, eq, 4)) {

        BinaryExpr *node = AllocateExpr<BinaryExpr>(compiler);
        node->index = EXPRESSION_BINARY;
        node->opr = PreviousToken(&compiler->parser).type;
        node->left = expr;
        node->right = ComparisonExpression(compiler);

        expr = GetExprFromPtr(compiler->mem, node);
    }

    return expr;
}

Expr AssignmentExpression(Compiler *compiler) {

    auto parser = &compiler->parser;
    Expr lval = EqualityExpression(compiler);

    TokenType eq = TOKEN_EQUAL_SIGN;
    if (Match(parser, &eq, 1)) {

        Expr rval = AssignmentExpression(compiler);

        Expr i = rval;
        if (FirstNotGroupExpr(compiler->mem, &i) == EXPRESSION_CALL) {

            auto call = (CallExpr*)(compiler->mem + i.index);

            TypeExpr end = GetTypeExpr(compiler->mem, call->calleeExpr, compiler->scratchMem, TypeExpr{0});
            TypeExpr fnExpr = GetNthType(compiler->scratchMem, TypeExpr{0}, 0);
            FnTypeExpr *fn = (FnTypeExpr *)(compiler->scratchMem + fnExpr.index);
            TypeName retT = GetLastType(compiler->scratchMem, fn->ret_t);

            GetTypeExpr(compiler->mem, lval, compiler->scratchMem, end);
            Expect(parser, TypesEqual(compiler->mem, compiler->mem, compiler->scratchMem, compiler->scratchMem, fn->ret_t, end), "assignment of incompatible types");

            if (retT == TYPE_MODIFIER_ARRAY || retT > TYPE_COUNT) {

                Expr paramExpr = Mem<Expr>(compiler->mem + call->args.index);
                ConversionExpr *lvalAdd = (ConversionExpr *)(compiler->mem + paramExpr.index);
                lvalAdd->from = lval;

                return GetExprFromPtr(compiler->mem, call);
            }
        }

        i = lval;
        ExprType expressionType = FirstNotGroupExpr(compiler->mem, &i);
        Expect(parser, (expressionType == EXPRESSION_MEMORY_LOAD) || (expressionType == EXPRESSION_VARIABLE), "expression must be a modifiable l-value");
        if (expressionType == EXPRESSION_MEMORY_LOAD) {

            auto *l = (MemoryLoadExpr*)(compiler->mem + i.index);

            GetTypeExpr(compiler->mem, lval, compiler->scratchMem, TypeExpr{0});
            TypeName lval_type = GetLastType(compiler->scratchMem, TypeExpr{0});
            if (lval_type > TYPE_COUNT || lval_type == TYPE_MODIFIER_ARRAY) {
                MemCopyExpr *node = AllocateExpr<MemCopyExpr>(compiler);
                node->index = EXPRESSION_MEM_COPY;
                node->dst = l->address;
                node->src = rval;
                node->size = GetTypeSize(compiler->mem, compiler->scratchMem, TypeExpr{0});

                return GetExprFromPtr(compiler->mem, node);
            }

            MemoryStoreExpr *node = AllocateExpr<MemoryStoreExpr>(compiler);

            node->index = EXPRESSION_MEMORY_STORE;
            node->address = l->address;
            node->value = rval;

            return GetExprFromPtr(compiler->mem, node);
        } else if (expressionType == EXPRESSION_VARIABLE) {

            GetTypeExpr(compiler->mem, lval, compiler->scratchMem, TypeExpr{0});
            TypeName lval_type = GetLastType(compiler->scratchMem, TypeExpr{0});
            if (lval_type > TYPE_COUNT || lval_type == TYPE_MODIFIER_ARRAY) {
                MemCopyExpr *node = AllocateExpr<MemCopyExpr>(compiler);
                node->index = EXPRESSION_MEM_COPY;
                node->dst = lval;
                node->src = rval;
                node->size = GetTypeSize(compiler->mem, compiler->scratchMem, TypeExpr{0});

                return GetExprFromPtr(compiler->mem, node);
            }

            auto l = (VariableExpr*)(compiler->mem + i.index);
            VariableAssignmentExpr *node = AllocateExpr<VariableAssignmentExpr>(compiler);
            node->index = EXPRESSION_VARIABLE_ASSIGNMENT;
            node->var = l->var;
            node->value = rval;

            return GetExprFromPtr(compiler->mem, node);
        }
    }

    return lval;
}

Expr Expression(Compiler *compiler) {
    Expr ret = AssignmentExpression(compiler);
    return ret;
}

void PrintExpression(byte* baseMem, Expr e, u32 depth) {

    ExprType t = Mem<ExprType>(baseMem + e.index);
    switch (t) {
    case EXPRESSION_NULL: {
        global_print("%s%\n", GetExprTypeStr(t));
        break;
    }
    case EXPRESSION_IMMEDIATE: {
        auto *node = (ImmediateExpr*)(baseMem + e.index);
        PrintValue(node->v);
        global_print("%\n");
        break;
    }
    case EXPRESSION_LITERAL: {
        auto *node = (LiteralExpr*)(baseMem + e.index);
        PrintToken(node->literal);
        break;
    }
    case EXPRESSION_UNARY: {
        auto *node = (UnaryExpr*)(baseMem + e.index);
        global_print("%s%c%\n", GetTokenStr(node->opr), '\t');
        PrintExpression(baseMem, node->primaryExpr, depth+1);
        break;
    }
    case EXPRESSION_BINARY: {
        auto *node = (BinaryExpr*)(baseMem + e.index);
        PrintExpression(baseMem, node->left, depth+1);

        global_print("%s%c", GetTokenStr(node->opr), ' ');
        PrintExpression(baseMem, node->right, depth+1);
        break;
    }
    case EXPRESSION_VARIABLE: {
        auto *node = (VariableExpr*)(baseMem + e.index);
        PrintToken(node->var.name);
        break;
    }
    case EXPRESSION_MEMORY_LOAD: {
        auto *node = (MemoryLoadExpr*)(baseMem + e.index);

        global_print("%s%\n", GetExprTypeStr(t));
        PrintExpression(baseMem, node->address, depth+1);
        break;
    }
    case EXPRESSION_VARIABLE_ASSIGNMENT: {
        auto *node = (VariableAssignmentExpr*)(baseMem + e.index);
        PrintToken(node->var.name);
        global_print("%s", " = ");
        PrintExpression(baseMem, node->value, depth+1);
        break;
    }
    case EXPRESSION_MEMORY_STORE: {

        auto *node = (MemoryStoreExpr*)(baseMem + e.index);
        global_print("%s%\n", GetExprTypeStr(t));
        PrintExpression(baseMem, node->address, depth+1);
        PrintExpression(baseMem, node->value, depth+1);
        break;
    }
    case EXPRESSION_GET: {
        auto *node = (DotExpr*)(baseMem + e.index);
        global_print("%s%c%s*%c", GetExprTypeStr(t), ' ', node->name.text, node->name.lenght, ' ');
        PrintExpression(baseMem, node->prev, depth+1);
        break;
    }
    case EXPRESSION_PEEL_TYPE: {
        auto *node = (PeelTypeExpr*)(baseMem + e.index);
        global_print("%s%c", GetExprTypeStr(t), ' ');
        PrintExpression(baseMem, node->expr, depth+1);
        break;
    }
    case EXPRESSION_MEM_COPY: {
        auto *node = (MemCopyExpr*)(baseMem + e.index);

        global_print("%s%\n", GetExprTypeStr(t));
        PrintExpression(baseMem, node->src, depth+1);
        PrintExpression(baseMem, node->dst, depth+1);
        global_print("%i%\n", node->size);
        break;
    }
    case EXPRESSION_CALL: {
        auto *node = (CallExpr*)(baseMem + e.index);

        global_print("%s%\n", GetExprTypeStr(t));
        PrintExpression(baseMem, node->calleeExpr, depth+1);
        u32 i = node->args.index;
        Expr arg = Mem<Expr>(baseMem + i);
        while (arg.index != 0) {
            PrintExpression(baseMem, arg, depth+1);
            i += 4;
            arg = Mem<Expr>(baseMem + i);
        }

        break;
    }
    case EXPRESSION_CONVERSION: {
        auto *node = (ConversionExpr*)(baseMem + e.index);
        global_print("%s%\n", GetExprTypeStr(t));
        PrintExpression(baseMem, node->from, depth+1);
        break;
    }
    }
}

Value GetValueFromLiteral(Token literal) {
    Value ret{};
    auto t = GetLiteralType(literal);
    switch (t) {
    case TYPE_PRIMARY_INT64:
        ret.type = TYPE_PRIMARY_INT64;
        Mem<i64>(ret.mem) = GetIntFromToken(literal);
        return ret;
    case TYPE_PRIMARY_F32:
        ret.type = TYPE_PRIMARY_F32;
        Mem<f32>(ret.mem) = GetF32FromToken(literal);
        return ret;
    case TYPE_PRIMARY_F64:
        ret.type = TYPE_PRIMARY_F64;
        Mem<f32>(ret.mem) = GetF64FromToken(literal);
        return ret;
    case TYPE_PRIMARY_BOOL:
        ret.type = TYPE_PRIMARY_BOOL;
        Mem<bool>(ret.mem) = GetBoolFromToken(literal);
        return ret;
    case TYPE_PRIMARY_CHAR:
        ret.type = TYPE_PRIMARY_CHAR;
        Mem<char>(ret.mem) = literal.text[0];
        return ret;
    case TYPE_MODIFIER_RESTRICTED_POINTER:
    case TYPE_MODIFIER_POINTER:
        ret.type = t;
        Mem<Token *>(ret.mem) = (Token *)global_malloc_debug(sizeof(Token));
        *Mem<Token *>(ret.mem) = literal;
        return ret;
    }
}
bool IsLiteralOne(Token literal) {
    if (literal.type == TOKEN_NUMBER_LITERAL) {
        switch (GetNumberType(literal)) {
        case TYPE_PRIMARY_INT64:
            return GetIntFromToken(literal) == 1;
        case TYPE_PRIMARY_F32:
            return GetF32FromToken(literal) == 1.0;
        case TYPE_PRIMARY_F64:
            return GetF64FromToken(literal) == 1.0;
        }
    }
    return false;
}
bool IsLiteralZero(Token literal) {
    if (literal.type == TOKEN_NUMBER_LITERAL) {
        switch (GetNumberType(literal)) {
        case TYPE_PRIMARY_INT64:
            return GetIntFromToken(literal) == 0;
        case TYPE_PRIMARY_F32:
            return GetF32FromToken(literal) == 0;
        case TYPE_PRIMARY_F64:
            return GetF64FromToken(literal) == 0;
        }
    }

    return false;
}

bool ExprEq(byte *structMem0, byte *structMem1, byte *mem0, byte *mem1, Expr expr0, Expr expr1, bool byValue) {

    Token token0;
    Token token1;
    ExprType t0 = Mem<ExprType>(mem0 + expr0.index);
    ExprType t1 = Mem<ExprType>(mem1 + expr1.index);
    bool eq = true;
    while (t0 == t1 && eq) {

        switch (t0) {
        case EXPRESSION_IMMEDIATE:
            if (byValue) {
                eq = Mem<ImmediateExpr>(mem0 + expr0.index).v.type == Mem<ImmediateExpr>(mem1 + expr1.index).v.type;
                eq &= Mem<u64>(Mem<ImmediateExpr>(mem0 + expr0.index).v.mem) == Mem<u64>(Mem<ImmediateExpr>(mem1 + expr1.index).v.mem);
                return eq;
            }
            return true;
        case EXPRESSION_LITERAL:
            if (byValue) {
                token0 = Mem<LiteralExpr>(mem0 + expr0.index).literal;
                token1 = Mem<LiteralExpr>(mem1 + expr1.index).literal;
                eq = TokensEquals(token0, token1);
                return eq;
            }
            return true;
        case EXPRESSION_UNARY:
            eq = Mem<UnaryExpr>(mem0 + expr0.index).opr == Mem<UnaryExpr>(mem1 + expr1.index).opr;
            expr0 = Mem<UnaryExpr>(mem0 + expr0.index).primaryExpr;
            expr1 = Mem<UnaryExpr>(mem1 + expr1.index).primaryExpr;
            return eq;
            break;
        case EXPRESSION_BINARY:
            eq = Mem<BinaryExpr>(mem0 + expr0.index).opr == Mem<BinaryExpr>(mem1 + expr1.index).opr;
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Mem<BinaryExpr>(mem0 + expr0.index).left, Mem<BinaryExpr>(mem1 + expr1.index).left, byValue);
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Mem<BinaryExpr>(mem0 + expr0.index).right, Mem<BinaryExpr>(mem1 + expr1.index).right, byValue);
            return eq;
            break;
        case EXPRESSION_VARIABLE:
            token0 = Mem<VariableExpr>(mem0 + expr0.index).var.name;
            token1 = Mem<VariableExpr>(mem1 + expr1.index).var.name;
            eq = TokensEquals(token0, token1);
            return eq;
            break;
        case EXPRESSION_MEMORY_LOAD:
            eq = ExprEq(structMem0, structMem1, mem0, mem1, Mem<MemoryLoadExpr>(mem0 + expr0.index).address, Mem<MemoryLoadExpr>(mem1 + expr1.index).address, byValue);
            return eq;
            break;
        case EXPRESSION_VARIABLE_ASSIGNMENT:
            eq = ExprEq(structMem0, structMem1, mem0, mem1, Mem<VariableAssignmentExpr>(mem0 + expr0.index).value, Mem<VariableAssignmentExpr>(mem1 + expr1.index).value, byValue);
            token0 = Mem<VariableAssignmentExpr>(mem0 + expr0.index).var.name;
            token1 = Mem<VariableAssignmentExpr>(mem1 + expr1.index).var.name;
            eq &= TokensEquals(token0, token1);
            return eq;
            break;
        case EXPRESSION_MEMORY_STORE:
            eq = ExprEq(structMem0, structMem1, mem0, mem1, Mem<MemoryStoreExpr>(mem0 + expr0.index).value, Mem<MemoryStoreExpr>(mem1 + expr1.index).value, byValue);
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Mem<MemoryStoreExpr>(mem0 + expr0.index).address, Mem<MemoryStoreExpr>(mem1 + expr1.index).address, byValue);
            return eq;
            break;
        case EXPRESSION_ADDRESS_OF:
            ASSERT(false);
            break;
        case EXPRESSION_GET:
            token0 = Mem<DotExpr>(mem0 + expr0.index).name;
            token1 = Mem<DotExpr>(mem1 + expr1.index).name;
            eq = TokensEquals(token0, token1);
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Mem<DotExpr>(mem0 + expr0.index).prev, Mem<DotExpr>(mem1 + expr1.index).prev, byValue);
            return eq;
            break;
        case EXPRESSION_PEEL_TYPE:
            expr0 = Mem<PeelTypeExpr>(mem0 + expr0.index).expr;
            expr1 = Mem<PeelTypeExpr>(mem1 + expr1.index).expr;
            break;
        case EXPRESSION_MEM_COPY:
            eq = Mem<MemCopyExpr>(mem0 + expr0.index).size == Mem<MemCopyExpr>(mem1 + expr1.index).size;
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Mem<MemCopyExpr>(mem0 + expr0.index).dst, Mem<MemCopyExpr>(mem1 + expr1.index).dst, byValue);
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Mem<MemCopyExpr>(mem0 + expr0.index).src, Mem<MemCopyExpr>(mem1 + expr1.index).src, byValue);
            return eq;
            break;
        case EXPRESSION_CALL: {
            eq = ExprEq(structMem0, structMem1, mem0, mem1, Mem<CallExpr>(mem0 + expr0.index).calleeExpr, Mem<CallExpr>(mem1 + expr1.index).calleeExpr, byValue);
            u32 i0 = Mem<CallExpr>(mem0 + expr0.index).args.index;
            u32 i1 = Mem<CallExpr>(mem1 + expr1.index).args.index;
            while (Mem<Expr>(mem0 + i0).index != 0 && Mem<Expr>(mem1 + i1).index != 0) {

                Expr p0 = Mem<Expr>(mem0 + i0);
                Expr p1 = Mem<Expr>(mem1 + i1);
                if (!ExprEq(structMem0, structMem1, mem0, mem1, p0, p1, byValue))
                    return false;
                i0 += sizeof(Expr);
                i1 += sizeof(Expr);
            }
            eq &= Mem<Expr>(mem0 + i0).index == Mem<Expr>(mem1 + i1).index;
            return eq;
        } break;
        case EXPRESSION_CONVERSION:
            eq = TypesEqual(structMem0, structMem1, mem0, mem1, Mem<ConversionExpr>(mem0 + expr0.index).type, Mem<ConversionExpr>(mem1 + expr1.index).type);
            if (!eq)
                return false;
            expr0 = Mem<ConversionExpr>(mem0 + expr0.index).from;
            expr1 = Mem<ConversionExpr>(mem1 + expr1.index).from;
            break;
        }

        t0 = Mem<ExprType>(mem0 + expr0.index);
        t1 = Mem<ExprType>(mem1 + expr1.index);
    }

    return t0 == t1;
}

void PopVariables(Compiler *compiler) {

    while (compiler->symbolTable.size != 0 && compiler->symbolTable.Back().scope > compiler->parser.scope) {
        compiler->symbolTable.PopBack();
    }
}
bool ParseStatement(Compiler *compiler) {

    Token peek = PeekToken(compiler->parser.tokenizer);
    switch (peek.type) {
    case TOKEN_EOF:
        return false;
    case TOKEN_KEYWORD_MAIN: {
        ExpectToken(&compiler->parser, TOKEN_KEYWORD_MAIN);
        Token name = PreviousToken(&compiler->parser);

        Expect(&compiler->parser, compiler->parser.scope == 0, "main has to be a global smybol");
        Expect(&compiler->parser, PeekToken(compiler->parser.tokenizer).type != TOKEN_OPEN_PAREN, "main is not recursive");
        Expect(&compiler->parser, compiler->entryPoint.index == 0, "there can be only one main in a program");
        ExpectToken(&compiler->parser, TOKEN_OPEN_BRACES);
        compiler->entryPoint.index = compiler->StmtAllocator;

        Stmt *main = AllocateStmt<Stmt>(compiler);
        main->index = STATEMENT_ENTRY_POINT;
        compiler->currentFunction = compiler->exprAllocator;

        Stmt program{compiler->StmtAllocator};
        FunctionStmt *enter = AllocateStmt<FunctionStmt>(compiler);
        enter->index = STATEMENT_FUNCTION_ENTRY;
        enter->functionPtr = compiler->currentFunction;

        FunctionExpr *fn = AllocateExpr<FunctionExpr>(compiler);
        *fn = FunctionExpr{};

        fn->name = name;
        fn->paramCount = 0;

        compiler->parser.scope++;
        ExpectToken(&compiler->parser, TOKEN_OPEN_BRACES);
        while(ParseStatement(compiler));

        compiler->parser.scope--;
        PopVariables(compiler);
        ExpectToken(&compiler->parser, TOKEN_CLOSE_BRACES);

        RetAssignStmt *out = AllocateStmt<RetAssignStmt>(compiler);
        out->index = STATEMENT_RET_ASSIGN;
        out->retExpr = {0};
    } break;
    case TOKEN_KEYWORD_FN: {
        NextToken(&compiler->parser);
        ExpectToken(&compiler->parser, TOKEN_IDENTIFIER);
        Expect(&compiler->parser, compiler->parser.scope == 0, "functions can be only defined in global scope");

        compiler->currentFunction = compiler->exprAllocator;
        FunctionExpr *f = AllocateExpr<FunctionExpr>(compiler);
        *f = FunctionExpr{};
        f->name = PreviousToken(&compiler->parser);

        Variable fn;
        fn.scope = compiler->parser.scope;
        fn.name = f->name;
        fn.type.index = compiler->exprAllocator;
        FnTypeExpr *fnExpr = AllocateExpr<FnTypeExpr>(compiler);
        *fnExpr = {};
        fnExpr->index = TYPE_PRIMARY_FN;

        u32 index = FindSymbol(&compiler->symbolTable, fn.name);
        if (index != ~u32(0)) {
            global_print("%s%s*%s%i%\n", "ERROR: function redecleration (", fn.name.text, fn.name.lenght, ") at line: ", compiler->parser.tokenizer.line);
            compiler->error = true;
        }
        compiler->symbolTable.PushBack(&compiler->localHeap, fn);

        Stmt program{compiler->StmtAllocator};
        FunctionStmt *enter = AllocateStmt<FunctionStmt>(compiler);
        enter->index = STATEMENT_FUNCTION_ENTRY;
        enter->functionPtr = compiler->currentFunction;

        TokenType close = TOKEN_CLOSE_PAREN;
        TokenType comma = TOKEN_COMMA;
        TokenType arrow = TOKEN_KEYWORD_ARROW;
        Tokenizer peek = compiler->parser.tokenizer;
        compiler->parser.scope++;
        ExpectToken(&compiler->parser, TOKEN_OPEN_PAREN);

        u32 stackSize = compiler->symbolTable.size;
        bool hidden = false;
        u32 hidden_index;
        {
            ParserState save = SaveParserState(&compiler->parser, compiler->exprAllocator);
            if (!Match(&compiler->parser, &close, 1)) {
                do {
                    ParseTypeExpression(compiler);
                    NextToken(&compiler->parser);
                } while (Match(&compiler->parser, &comma, 1));
                NextToken(&compiler->parser);
            }

            if (Match(&compiler->parser, &arrow, 1)) {

                TypeExpr retT = ParseTypeExpression(compiler);
                TypeName last = GetLastType(compiler->mem, retT);
                if (last > TYPE_COUNT || last == TYPE_MODIFIER_ARRAY) {
                    TypeExpr end = CpyTypeExpr(compiler->scratchMem, TypeExpr{0}, compiler->mem, retT);
                    Mem<TypeName>(compiler->scratchMem + end.index - sizeof(TypeName)) = TYPE_MODIFIER_RESTRICTED_POINTER;
                    Mem<TypeName>(compiler->scratchMem + end.index) = TYPE_NON;

                    Variable hidden_param;
                    hidden_param.scope = compiler->parser.scope;
                    hidden_param.name.lenght = sizeof("hidden_param0") - 1;
                    hidden_param.name.type = TOKEN_IDENTIFIER;

                    hidden_param.name.text = (char*)AllocateExpr(compiler, sizeof("hidden_param0"));
                    memcpy(hidden_param.name.text, "hidden_param0", sizeof("hidden_param0"));
                    hidden_index = compiler->symbolTable.PushBack(&compiler->localHeap, hidden_param);

                    fnExpr->param_count = 1;
                    hidden = true;
                }
            }

            RestoreParserState(&compiler->parser, save);
            compiler->exprAllocator = save.allocator;
        }

        if (!Match(&compiler->parser, &close, 1)) {

            ParserState save = SaveParserState(&compiler->parser, compiler->exprAllocator);
            do {
                fnExpr->param_count++;
                ParseTypeExpression(compiler);
                NextToken(&compiler->parser);
            } while (Match(&compiler->parser, &comma, 1));
            RestoreParserState(&compiler->parser, save);
            compiler->exprAllocator = save.allocator;

            if (hidden) {
                fnExpr->params = compiler->exprAllocator;
                for (u32 i = 0; i < fnExpr->param_count; i++)
                    AllocateExpr<TypeExpr>(compiler);
                fnExpr->param_count = 1;

                compiler->symbolTable[hidden_index].type = {compiler->exprAllocator};
                TypeExpr end = CpyTypeExpr(compiler->mem, {compiler->exprAllocator}, compiler->scratchMem, TypeExpr{0});
                Mem<TypeExpr>(compiler->mem + fnExpr->params).index = compiler->exprAllocator;
                compiler->exprAllocator = end.index;
            } else {
                fnExpr->params = compiler->exprAllocator;
                for (u32 i = 0; i < fnExpr->param_count; i++) AllocateExpr<TypeExpr>(compiler);
                fnExpr->param_count = 0;
            }

            do {
                Variable param;
                param.type = ParseTypeExpression(compiler);
                param.name = NextToken(&compiler->parser);

                Expect(&compiler->parser, param.name.type == TOKEN_IDENTIFIER, "expected identifier");
                param.scope = compiler->parser.scope;

                compiler->symbolTable.PushBack(&compiler->localHeap, param);
                Mem<TypeExpr>(compiler->mem + fnExpr->params + fnExpr->param_count * sizeof(TypeExpr)) = param.type;

                fnExpr->param_count++;
            } while (Match(&compiler->parser, &comma, 1));
            ExpectToken(&compiler->parser, TOKEN_CLOSE_PAREN);

        } else if (hidden && fnExpr->param_count == 1) {
            fnExpr->params = compiler->exprAllocator;
            for (u32 i = 0; i < fnExpr->param_count; i++)
                AllocateExpr<TypeExpr>(compiler);

            compiler->symbolTable[hidden_index].type = {compiler->exprAllocator};
            TypeExpr end = CpyTypeExpr(compiler->mem, {compiler->exprAllocator}, compiler->scratchMem, TypeExpr{0});
            Mem<TypeExpr>(compiler->mem + fnExpr->params).index = compiler->exprAllocator;
            compiler->exprAllocator = end.index;
        }
        f->paramCount = fnExpr->param_count;

        if (Match(&compiler->parser, &arrow, 1)) {
            fnExpr->ret_t = ParseTypeExpression(compiler);
            f->ret_t = fnExpr->ret_t;
        } else {
            fnExpr->ret_t.index = compiler->exprAllocator;
            f->ret_t = fnExpr->ret_t;
            *AllocateExpr<TypeName>(compiler) = TYPE_PRIMARY_VOID;
            *AllocateExpr<TypeName>(compiler) = TYPE_NON;
        }
        fnExpr->modifier.index = compiler->exprAllocator;
        *AllocateExpr<TypeName>(compiler) = TYPE_NON;

        u32 paramsSize = compiler->symbolTable.size - stackSize;
        f->params = compiler->exprAllocator;
        compiler->exprAllocator += paramsSize * sizeof(Variable);
        memcpy(compiler->mem + f->params, compiler->symbolTable.mem + stackSize, paramsSize * sizeof(Variable));

        ExpectToken(&compiler->parser, TOKEN_OPEN_BRACES);
        while(ParseStatement(compiler));
        *AllocateStmt<StatementType>(compiler) = STATEMENT_FUNCTION_EXIT;

    } break;
    case TOKEN_KEYWORD_STRUCT: {
        ExpectToken(&compiler->parser, TOKEN_KEYWORD_STRUCT);
        compiler->parser.structs.PushBack(StructName{NextToken(&compiler->parser), compiler->exprAllocator});
        
        StructTypeExpr *node = AllocateExpr<StructTypeExpr>(compiler);
        node->index = TYPE_STRUCTURE;
        node->name = compiler->parser.structs.Back().name;
        Expect(&compiler->parser, node->name.type == TOKEN_IDENTIFIER, "expected identifier");
        ExpectToken(&compiler->parser, TOKEN_OPEN_BRACES);

        node->members.index = compiler->exprAllocator;
        u32 allocator = compiler->exprAllocator;
        u32 size = compiler->parser.tokenBuffer.size;
        Tokenizer restore = compiler->parser.tokenizer;

        u32 memberCount = 0;
        while (PeekToken(compiler->parser.tokenizer).type != TOKEN_CLOSE_BRACES) {
            memberCount++;
            ParseTypeExpression(compiler);
            ExpectToken(&compiler->parser, TOKEN_IDENTIFIER);
            ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
        }

        compiler->exprAllocator = allocator;
        compiler->parser.tokenBuffer.size = size;
        compiler->parser.tokenizer = restore;

        for (u32 i = 0; i < memberCount; i++)
            AllocateExpr<TypeExpr>(compiler);
        AllocateExpr<TypeExpr>(compiler)->index = TYPE_NON;

        for (u32 i = 0; i < memberCount; i++) {

            Mem<TypeExpr>(compiler->mem + node->members.index + i * sizeof(TypeExpr)).index = compiler->exprAllocator;
            StructMemberTypeExpr *member = AllocateExpr<StructMemberTypeExpr>(compiler);
            member->index = TYPE_STRUCTURE_MEMBER;
            member->type = ParseTypeExpression(compiler);
            member->name = NextToken(&compiler->parser);
            ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
            ASSERT(member->name.type == TOKEN_IDENTIFIER);
        }

        ExpectToken(&compiler->parser, TOKEN_CLOSE_BRACES);
    } break;
    case TOKEN_OPEN_BRACES:
        NextToken(&compiler->parser);
        Expect(&compiler->parser, compiler->parser.scope != 0, "expected a declaration");
        compiler->parser.scope++;
        while(ParseStatement(compiler));
        break;
    case TOKEN_CLOSE_BRACES:
        NextToken(&compiler->parser);
        compiler->parser.scope--;
        PopVariables(compiler);
        return false;
        break;
    case TOKEN_KEYWORD_RETURN: {
        auto f = (FunctionExpr*)(compiler->mem + compiler->currentFunction);
        ExpectToken(&compiler->parser, TOKEN_KEYWORD_RETURN);
        RetAssignStmt *node = AllocateStmt<RetAssignStmt>(compiler);
        node->index = STATEMENT_RET_ASSIGN;
        node->retExpr.index = 0;
        if (PeekToken(compiler->parser.tokenizer).type != TOKEN_SEMICOLON) {
            node->retExpr = Expression(compiler);
            GetTypeExpr(compiler->mem, node->retExpr, compiler->scratchMem, TypeExpr{0});
            bool match = TypesEqual(compiler->mem, compiler->mem, compiler->scratchMem, compiler->mem, TypeExpr{0}, f->ret_t);
            Expect(&compiler->parser, match, "function return type mismatch");

            TypeName type = GetLastType(compiler->scratchMem, TypeExpr{0});
            if (type == TYPE_MODIFIER_ARRAY || type > TYPE_COUNT) {

                MemCopyExpr *memCopy = AllocateExpr<MemCopyExpr>(compiler);
                memCopy->index = EXPRESSION_MEM_COPY;
                memCopy->size = GetTypeSize(compiler->mem, compiler->scratchMem, TypeExpr{0});
                memCopy->src = node->retExpr;
                memCopy->dst.index = compiler->exprAllocator;

                VariableExpr *retPtr = AllocateExpr<VariableExpr>(compiler);
                retPtr->index = EXPRESSION_VARIABLE;
                for (u32 i = 0; i < compiler->symbolTable.size; i++) {
                    if (TokensEquals(f->name, compiler->symbolTable[i].name)) {
                        retPtr->var = compiler->symbolTable[i + 1];
                        break;
                    }
                }

                node->retExpr = GetExprFromPtr(compiler->mem, memCopy);
            }
        }
        ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
    } break;
    case TOKEN_KEYWORD_IF: {
        Expect(&compiler->parser, compiler->parser.scope != 0, "statement (if) at global scope");
        NextToken(&compiler->parser);
        ExpectToken(&compiler->parser, TOKEN_OPEN_PAREN);

        BranchStmt *b = AllocateStmt<BranchStmt>(compiler);
        b->index = STATEMENT_BRANCH;
        b->cond = Expression(compiler);

        GetTypeExpr(compiler->mem, b->cond, compiler->scratchMem, TypeExpr{0});
        TypeName cond_t = GetLastType(compiler->scratchMem, TypeExpr{0});
        Expect(&compiler->parser, cond_t == TYPE_PRIMARY_BOOL, "branch condition must be a boolean expression");

        ExpectToken(&compiler->parser, TOKEN_CLOSE_PAREN);
        b->thenBranch.index = compiler->StmtAllocator;
        *AllocateStmt<StatementType>(compiler) = STATEMENT_REPEAT;
        ParseStatement(compiler);
        *AllocateStmt<StatementType>(compiler) = STATEMENT_NON;
        b->elseBranch.index = 0;

        TokenType keywordelse = TOKEN_KEYWORD_ELSE;
        if (Match(&compiler->parser, &keywordelse, 1)) {
            b->elseBranch.index = compiler->StmtAllocator;
            *AllocateStmt<StatementType>(compiler) = STATEMENT_REPEAT;
            ParseStatement(compiler);
            *AllocateStmt<StatementType>(compiler) = STATEMENT_NON;
        }
        b->end.index = compiler->StmtAllocator;
    } break;
    case TOKEN_KEYWORD_FOR:
    {
        Expect(&compiler->parser, compiler->parser.scope != 0, "statement (for) at global scope");
        NextToken(&compiler->parser);
        ExpectToken(&compiler->parser, TOKEN_OPEN_PAREN);

        ForStmt *forStmt = AllocateStmt<ForStmt>(compiler);
        forStmt->index = STATEMENT_FOR_LOOP;

        compiler->parser.scope++;
        forStmt->init.index = compiler->StmtAllocator;
        *AllocateStmt<StatementType>(compiler) = STATEMENT_REPEAT;

        TokenType op = TOKEN_OPEN_BRACES;
        if(Match(&compiler->parser, &op, 1)) {
            while (PeekToken(compiler->parser.tokenizer).type != TOKEN_CLOSE_BRACES)
                ParseStatement(compiler);
            NextToken(&compiler->parser);
        }
        else if (PeekToken(compiler->parser.tokenizer).type != TOKEN_SEMICOLON) {
            ParseStatement(compiler);
        }
        else {
            ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
        }
        *AllocateStmt<StatementType>(compiler) = STATEMENT_NON;

        if (PeekToken(compiler->parser.tokenizer).type != TOKEN_SEMICOLON) {
            forStmt->cond = Expression(compiler);
            GetTypeExpr(compiler->mem, forStmt->cond, compiler->scratchMem, TypeExpr{0});
            TypeName t = GetLastType(compiler->scratchMem, TypeExpr{0});
            Expect(&compiler->parser, t == TYPE_PRIMARY_BOOL, "loop condition must be a boolean expression");
        } else {
            forStmt->cond.index = EXPRESSION_NULL;
        }
        ExpectToken(&compiler->parser, TOKEN_SEMICOLON);

        forStmt->inc.index = compiler->StmtAllocator;
        *AllocateStmt<StatementType>(compiler) = STATEMENT_REPEAT;
        if (PeekToken(compiler->parser.tokenizer).type != TOKEN_SEMICOLON) {
            ParseStatement(compiler);
        } else {
            ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
        }
        *AllocateStmt<StatementType>(compiler) = STATEMENT_NON;
        ExpectToken(&compiler->parser, TOKEN_CLOSE_PAREN);

        forStmt->body.index = compiler->StmtAllocator;
        *AllocateStmt<StatementType>(compiler) = STATEMENT_REPEAT;
        if (PeekToken(compiler->parser.tokenizer).type != TOKEN_SEMICOLON) {
            ParseStatement(compiler);
        } else {
            ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
        }
        *AllocateStmt<StatementType>(compiler) = STATEMENT_NON;
        forStmt->end.index = compiler->StmtAllocator;
        compiler->parser.scope--;
        PopVariables(compiler);
    } break;
    case TOKEN_KEYWORD_DO:
        {
            Expect(&compiler->parser, compiler->parser.scope != 0, "statement (do) in global scope");
            NextToken(&compiler->parser);

            ForStmt *forStmt = AllocateStmt<ForStmt>(compiler);
            forStmt->index = STATEMENT_FOR_LOOP;

            compiler->parser.scope++;
            forStmt->init.index = compiler->StmtAllocator;
            forStmt->inc.index  = compiler->StmtAllocator;
            *AllocateStmt<StatementType>(compiler) = STATEMENT_NON;

            forStmt->cond.index = compiler->exprAllocator;
            ImmediateExpr* imm = AllocateExpr<ImmediateExpr>(compiler);
            *imm = {};
            imm->index = EXPRESSION_IMMEDIATE;
            imm->v = MakeImm(TYPE_PRIMARY_BOOL, true);

            forStmt->body.index = compiler->StmtAllocator;
            *AllocateStmt<StatementType>(compiler) = STATEMENT_REPEAT;
            ParseStatement(compiler);
            *AllocateStmt<StatementType>(compiler) = STATEMENT_NON;

            forStmt->end.index = compiler->StmtAllocator;
            compiler->parser.scope--;
            PopVariables(compiler);
            break;
        }
    case TOKEN_KEYWORD_ASSUME:
        {
            Expect(&compiler->parser, compiler->parser.scope != 0, "statement (assume) in global scope");
            NextToken(&compiler->parser);

            AssumeStmt *assume = AllocateStmt<AssumeStmt>(compiler);
            *assume = {};
            assume->index = STATEMENT_ASSUME;

            bool empty = true;
            TokenType comma = TOKEN_COMMA;
            if (!Check(&compiler->parser, TOKEN_SEMICOLON)) {
                do {
                    assume->exprCount++;
                    Expr *expr = AllocateStmt<Expr>(compiler);
                    empty = false;

                    auto peek = PeekToken(compiler->parser.tokenizer);
                    if(peek.type == TOKEN_KEYWORD_HOT || peek.type == TOKEN_KEYWORD_COLD) {
                        *expr = {compiler->exprAllocator};
                        AllocateExpr<MiscExpr>(compiler)->index = peek.type == TOKEN_KEYWORD_COLD ? EXPRESSION_MISC_COLD : EXPRESSION_MISC_HOT;
                        GetToken(&compiler->parser.tokenizer);
                    }
                    else {
                        *expr = Expression(compiler);
                        VerifyTypes(compiler, compiler->mem, *expr, compiler->scratchMem);
                        Expect(&compiler->parser, expr->index != 0, "(assume) error");

                        GetTypeExpr(compiler->mem, *expr, compiler->scratchMem, TypeExpr{0});
                        TypeName last = GetLastType(compiler->scratchMem, TypeExpr{0});
                        Expect(&compiler->parser, last == TYPE_PRIMARY_BOOL, "(assume) expects boolean expressions");
                    }

                } while (Match(&compiler->parser, &comma, 1));
            }

            Expect(&compiler->parser, !empty, "assume statement without expression");
            ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
            break;
        }
    case TOKEN_KEYWORD_PRINT: {
        Expect(&compiler->parser, compiler->parser.scope != 0, "statement (print) in global scope");
        NextToken(&compiler->parser);

        PrintStmt *print = AllocateStmt<PrintStmt>(compiler);
        print->index = STATEMENT_PRINT;

        bool empty = true;
        TokenType comma = TOKEN_COMMA;

        if (!Check(&compiler->parser, TOKEN_SEMICOLON)) {
            do {
                print->exprCount++;
                Expr *expr = AllocateStmt<Expr>(compiler);
                AllocateStmt<u32>(compiler);
                *expr = Expression(compiler);
                VerifyTypes(compiler, compiler->mem, *expr, compiler->scratchMem);
                Expect(&compiler->parser, expr->index != 0, "print error");

                GetTypeExpr(compiler->mem, *expr, compiler->scratchMem, TypeExpr{0});
                TypeName last = GetLastType(compiler->scratchMem, TypeExpr{0});
                Expect(&compiler->parser, last >= TYPE_PRIMARY_CHAR && last <= TYPE_MODIFIER_POINTER, "invalid print type");

                empty = false;
            } while (Match(&compiler->parser, &comma, 1));
        }

        Expect(&compiler->parser, !empty, "print statement without expression");
        ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
    } break;
    case TOKEN_IDENTIFIER:
    identifier_stmt : {
        if (!IsType(&compiler->parser, compiler->parser.tokenizer)) {
            goto expr_stmt;
        }
        Variable var;
        var = {};

        TokenType auto_token = TOKEN_KEYWORD_AUTO;
        if (Match(&compiler->parser, &auto_token, 1)) {
            ParserState state = SaveParserState(&compiler->parser, compiler->exprAllocator);
            ExpectToken(&compiler->parser, TOKEN_IDENTIFIER);
            ExpectToken(&compiler->parser, TOKEN_EQUAL_SIGN);
            Expr val = Expression(compiler);
            RestoreParserState(&compiler->parser, state);
            compiler->exprAllocator = state.allocator;

            VerifyTypes(compiler, compiler->mem, val, compiler->scratchMem);
            GetTypeExpr(compiler->mem, val, compiler->scratchMem, {0});

            TypeName type = GetLastType(compiler->scratchMem, {0});
            Expect(&compiler->parser, type != TYPE_PRIMARY_AUTO && type != TYPE_PRIMARY_VOID, "can not deduce type of declaration");
            var.type = {compiler->exprAllocator};
            compiler->exprAllocator = CpyTypeExpr(compiler->mem, {compiler->exprAllocator}, compiler->scratchMem, {0}).index;
        } else {
            var.type = ParseTypeExpression(compiler);
        }

        TypeName t = GetLastType(compiler->mem, var.type);
        Expect(&compiler->parser, t != TYPE_PRIMARY_FN, "variable with function type");
        var.name = PeekToken(compiler->parser.tokenizer);
        var.scope = compiler->parser.scope;

        auto fn = (FunctionExpr*)(compiler->mem + compiler->currentFunction);
        u32 varIndex = FindSymbol(&compiler->symbolTable, var.name);
        if (varIndex != ~u32(0)) {
            global_print("%s%s*%s%i%\n", "ERROR: variable redecleration (", var.name.text, var.name.lenght, ") at line: ", compiler->parser.tokenizer.line);
            compiler->error = true;
        }
        varIndex = compiler->symbolTable.PushBack(&compiler->localHeap, var);

        Tokenizer peek = compiler->parser.tokenizer;
        GetToken(&peek);
        if (GetToken(&peek).type == TOKEN_EQUAL_SIGN) {
            ExprStmt *stmt = AllocateStmt<ExprStmt>(compiler);
            stmt->index = STATEMENT_EXPRESSION;
            stmt->expr = Expression(compiler);

            VerifyTypes(compiler, compiler->mem, stmt->expr, compiler->scratchMem);
        } else {
            ExpectToken(&compiler->parser, TOKEN_IDENTIFIER);
        }
        ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
        break;
    }
    default: {
        goto identifier_stmt;
    expr_stmt:

        if (TokenEquals(PeekToken(compiler->parser.tokenizer), "abort")) {
            NextToken(&compiler->parser);
            ExpectToken(&compiler->parser, TOKEN_OPEN_PAREN);
            ExpectToken(&compiler->parser, TOKEN_CLOSE_PAREN);
            ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
            Stmt *abortStmt = AllocateStmt<Stmt>(compiler);
            abortStmt->index = STATEMENT_ABORT;
            break;
        }

        ExprStmt *stmt = AllocateStmt<ExprStmt>(compiler);
        stmt->index = STATEMENT_EXPRESSION;
        stmt->expr = Expression(compiler);
        VerifyTypes(compiler, compiler->mem, stmt->expr, compiler->scratchMem);
        ExpectToken(&compiler->parser, TOKEN_SEMICOLON);
        break;
    }
    }

    return true;
}





// ------------------------ Optimizer BEGIN ------------------------------
f64 GetF64FromValue(Value v) {

    f64 ret;
    switch (v.type) {
    case TYPE_PRIMARY_INT8:
        ret = Mem<i8>(v.mem);
        break;
    case TYPE_PRIMARY_INT16:
        ret = Mem<i16>(v.mem);
        break;
    case TYPE_PRIMARY_INT32:
        ret = Mem<i32>(v.mem);
        break;
    case TYPE_PRIMARY_INT64:
        ret = Mem<i64>(v.mem);
        break;
    case TYPE_PRIMARY_UINT8:
        ret = Mem<u8>(v.mem);
        break;
    case TYPE_PRIMARY_UINT16:
        ret = Mem<u16>(v.mem);
        break;
    case TYPE_PRIMARY_UINT32:
        ret = Mem<u32>(v.mem);
        break;
    case TYPE_MODIFIER_POINTER:
    case TYPE_MODIFIER_RESTRICTED_POINTER:
        ret = Mem<u64>(v.mem);
        break;
    case TYPE_PRIMARY_UINT64:
        ret = Mem<f32>(v.mem);
        break;
    case TYPE_PRIMARY_F32:
        ret = Mem<f32>(v.mem);
        break;
    case TYPE_PRIMARY_F64:
        ret = Mem<f64>(v.mem);
        break;
    }
    return ret;
}

Value MaxValue(Value v0, Value v1) {

    ASSERT(v0.type == v1.type);
    switch (v0.type) {
    case TYPE_PRIMARY_BOOL:
        return (Mem<bool>(v0.mem) > Mem<bool>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_CHAR:
        return (Mem<char>(v0.mem) > Mem<char>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_INT8:
        return (Mem<i8>(v0.mem) > Mem<i8>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_INT16:
        return (Mem<i16>(v0.mem) > Mem<i16>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_INT32:
        return (Mem<i32>(v0.mem) > Mem<i32>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_INT64:
        return (Mem<i64>(v0.mem) > Mem<i64>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_UINT8:
        return (Mem<u8>(v0.mem) > Mem<u8>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_UINT16:
        return (Mem<u16>(v0.mem) > Mem<u16>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_UINT32:
        return (Mem<u32>(v0.mem) > Mem<u32>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_UINT64:
        return (Mem<u64>(v0.mem) > Mem<u64>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_F32:
        return (Mem<f32>(v0.mem) > Mem<f32>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_F64:
        return (Mem<f64>(v0.mem) > Mem<f64>(v1.mem) ? v0 : v1);
    }
}

bool ValuesEq(Value v0, Value v1) {

    switch (v0.type) {
    case TYPE_PRIMARY_BOOL:
        return Mem<bool>(v0.mem) == Mem<bool>(v1.mem);
    case TYPE_PRIMARY_CHAR:
        return Mem<char>(v0.mem) == Mem<char>(v1.mem);
    case TYPE_PRIMARY_INT8:
    case TYPE_PRIMARY_INT16:
    case TYPE_PRIMARY_INT32:
    case TYPE_PRIMARY_INT64:
    case TYPE_PRIMARY_UINT8:
    case TYPE_PRIMARY_UINT16:
    case TYPE_PRIMARY_UINT32:
    case TYPE_PRIMARY_UINT64:
        return Mem<i64>(v0.mem) == Mem<i64>(v1.mem);
    case TYPE_PRIMARY_F32:
        return Mem<f32>(v0.mem) == Mem<f32>(v1.mem);
    case TYPE_PRIMARY_F64:
        return Mem<f64>(v0.mem) == Mem<f64>(v1.mem);
    }
}

bool ValueBG(Value v0, Value v1) {

    switch (v0.type) {
    case TYPE_PRIMARY_BOOL:
        return Mem<bool>(v0.mem) < Mem<bool>(v1.mem);
    case TYPE_PRIMARY_CHAR:
        return Mem<char>(v0.mem) < Mem<char>(v1.mem);
    case TYPE_PRIMARY_INT8:
    case TYPE_PRIMARY_INT16:
    case TYPE_PRIMARY_INT32:
    case TYPE_PRIMARY_INT64:
        if (IsUnsigned((TypeName)v1.type)) {
            return Mem<i64>(v0.mem) < Mem<u64>(v1.mem);
        } else {
            return Mem<i64>(v0.mem) < Mem<i64>(v1.mem);
        }
    case TYPE_PRIMARY_UINT8:
    case TYPE_PRIMARY_UINT16:
    case TYPE_PRIMARY_UINT32:
    case TYPE_PRIMARY_UINT64:
        if (IsUnsigned((TypeName)v1.type)) {
            return Mem<u64>(v0.mem) < Mem<u64>(v1.mem);
        } else {
            return Mem<u64>(v0.mem) < Mem<i64>(v1.mem);
        }
    case TYPE_PRIMARY_F32:
        return Mem<f32>(v0.mem) < Mem<f32>(v1.mem);
    case TYPE_PRIMARY_F64:
        return Mem<f64>(v0.mem) < Mem<f64>(v1.mem);
    }
}
Value MinValue(Value v0, Value v1) {

    ASSERT(v0.type == v1.type);
    switch (v0.type) {
    case TYPE_PRIMARY_BOOL:
        return (Mem<bool>(v0.mem) < Mem<bool>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_CHAR:
        return (Mem<char>(v0.mem) < Mem<char>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_INT8:
        return (Mem<i8>(v0.mem) < Mem<i8>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_INT16:
        return (Mem<i16>(v0.mem) < Mem<i16>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_INT32:
        return (Mem<i32>(v0.mem) < Mem<i32>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_INT64:
        return (Mem<i64>(v0.mem) < Mem<i64>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_UINT8:
        return (Mem<u8>(v0.mem) < Mem<u8>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_UINT16:
        return (Mem<u16>(v0.mem) < Mem<u16>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_UINT32:
        return (Mem<u32>(v0.mem) < Mem<u32>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_UINT64:
        return (Mem<u64>(v0.mem) < Mem<u64>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_F32:
        return (Mem<f32>(v0.mem) < Mem<f32>(v1.mem) ? v0 : v1);
    case TYPE_PRIMARY_F64:
        return (Mem<f64>(v0.mem) < Mem<f64>(v1.mem) ? v0 : v1);
    }
}

Value ConvertValue(Value in, TypeName to);
Value EvalOpr(Value v0, Value v1, u32 opr, u32 size) {

    switch (opr) {
    case SSA_POWER:
        {
            Value counter = ConvertValue(v1, TYPE_PRIMARY_UINT64);
            ASSERT(counter.type == TYPE_PRIMARY_UINT64);
            u64 exp = Mem<u64>(counter.mem);
            Value res = v0;
            for(u32 i = 0; i < exp; i++) {
                res = EvalOpr(res, res, TOKEN_ASTERISK, size);
            }
            return res;
        }
    case SSA_MAX:
        {
            Value bgEq = EvalOpr(v0,v1, TOKEN_LSHIFT_EQUALS, size);
            ASSERT(bgEq.type == TYPE_PRIMARY_BOOL);
            return Mem<bool>(bgEq.mem) ? v0 : v1;
        }
    case SSA_MIN:
        {
            Value lsEq = EvalOpr(v0,v1, TOKEN_RSHIFT_EQUALS, size);
            ASSERT(lsEq.type == TYPE_PRIMARY_BOOL);
            return Mem<bool>(lsEq.mem) ? v0 : v1;
        }
    case TOKEN_RSHIFT_RSHIFT:
        {
            Value v1Same = ConvertValue(v1,TYPE_PRIMARY_INT64);
            switch (v0.type) {
            case TYPE_PRIMARY_INT8:
                Mem<i8>(v0.mem) = Mem<i8>(v0.mem) >> Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_INT16:
                Mem<i16>(v0.mem) = Mem<i16>(v0.mem) >> Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_INT32:
                Mem<i32>(v0.mem) = Mem<i32>(v0.mem) >> Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_INT64:
                Mem<i64>(v0.mem) = Mem<i64>(v0.mem) >> Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_UINT8:
                Mem<u8>(v0.mem) = Mem<u8>(v0.mem) >> Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_UINT16:
                Mem<u16>(v0.mem) = Mem<u16>(v0.mem) >> Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_UINT32:
                Mem<u32>(v0.mem) = Mem<u32>(v0.mem) >> Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_UINT64:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) >> Mem<i64>(v1Same.mem);break;
            }
            break;
        }
    case TOKEN_LSHIFT_LSHIFT:
        {
            Value v1Same = ConvertValue(v1,TYPE_PRIMARY_INT64);
            switch (v0.type) {
            case TYPE_PRIMARY_INT8:
                Mem<i8>(v0.mem) = Mem<i8>(v0.mem) << Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_INT16:
                Mem<i16>(v0.mem) = Mem<i16>(v0.mem) << Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_INT32:
                Mem<i32>(v0.mem) = Mem<i32>(v0.mem) << Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_INT64:
                Mem<i64>(v0.mem) = Mem<i64>(v0.mem) << Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_UINT8:
                Mem<u8>(v0.mem) = Mem<u8>(v0.mem) << Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_UINT16:
                Mem<u16>(v0.mem) = Mem<i16>(v0.mem) << Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_UINT32:
                Mem<u32>(v0.mem) = Mem<i32>(v0.mem) << Mem<i64>(v1Same.mem);break;
            case TYPE_PRIMARY_UINT64:
                Mem<u64>(v0.mem) = Mem<i64>(v0.mem) << Mem<i64>(v1Same.mem);break;
            }
            break;
        }
    case TOKEN_LSHIFT_EQUALS:
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<bool>(v0.mem) = Mem<i8>(v0.mem) <= Mem<i8>(v1.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<bool>(v0.mem) = Mem<i16>(v0.mem) <= Mem<i16>(v1.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<bool>(v0.mem) = Mem<i32>(v0.mem) <= Mem<i32>(v1.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<bool>(v0.mem) = Mem<i64>(v0.mem) <= Mem<i64>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<bool>(v0.mem) = Mem<u8>(v0.mem) <= Mem<u8>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<bool>(v0.mem) = Mem<u16>(v0.mem) <= Mem<u16>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<bool>(v0.mem) = Mem<u32>(v0.mem) <= Mem<u32>(v1.mem);
            break;
        case TYPE_MODIFIER_RESTRICTED_POINTER:
        case TYPE_MODIFIER_POINTER:
        case TYPE_PRIMARY_UINT64:
            Mem<bool>(v0.mem) = Mem<u64>(v0.mem) <= Mem<u64>(v1.mem);
            break;
        case TYPE_PRIMARY_F32:
            Mem<bool>(v0.mem) = Mem<f32>(v0.mem) <= Mem<f32>(v1.mem);
            break;
        case TYPE_PRIMARY_F64:
            Mem<bool>(v0.mem) = Mem<f64>(v0.mem) <= Mem<f64>(v1.mem);
            break;
        }
        break;
    case TOKEN_RSHIFT_EQUALS:
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<bool>(v0.mem) = Mem<i8>(v0.mem) >= Mem<i8>(v1.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<bool>(v0.mem) = Mem<i16>(v0.mem) >= Mem<i16>(v1.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<bool>(v0.mem) = Mem<i32>(v0.mem) >= Mem<i32>(v1.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<bool>(v0.mem) = Mem<i64>(v0.mem) >= Mem<i64>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<bool>(v0.mem) = Mem<u8>(v0.mem) >= Mem<u8>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<bool>(v0.mem) = Mem<u16>(v0.mem) >= Mem<u16>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<bool>(v0.mem) = Mem<u32>(v0.mem) >= Mem<u32>(v1.mem);
            break;
        case TYPE_MODIFIER_RESTRICTED_POINTER:
        case TYPE_MODIFIER_POINTER:
        case TYPE_PRIMARY_UINT64:
            Mem<bool>(v0.mem) = Mem<u64>(v0.mem) >= Mem<u64>(v1.mem);
            break;
        case TYPE_PRIMARY_F32:
            Mem<bool>(v0.mem) = Mem<f32>(v0.mem) >= Mem<f32>(v1.mem);
            break;
        case TYPE_PRIMARY_F64:
            Mem<bool>(v0.mem) = Mem<f64>(v0.mem) >= Mem<f64>(v1.mem);
            break;
        }
        break;
    case TOKEN_EXCLAMATION_EQUALS:
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<bool>(v0.mem) = Mem<i8>(v0.mem) != Mem<i8>(v1.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<bool>(v0.mem) = Mem<i16>(v0.mem) != Mem<i16>(v1.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<bool>(v0.mem) = Mem<i32>(v0.mem) != Mem<i32>(v1.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<bool>(v0.mem) = Mem<i64>(v0.mem) != Mem<i64>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<bool>(v0.mem) = Mem<u8>(v0.mem) != Mem<u8>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<bool>(v0.mem) = Mem<u16>(v0.mem) != Mem<u16>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<bool>(v0.mem) = Mem<u32>(v0.mem) != Mem<u32>(v1.mem);
            break;
        case TYPE_MODIFIER_RESTRICTED_POINTER:
        case TYPE_MODIFIER_POINTER:
        case TYPE_PRIMARY_UINT64:
            Mem<bool>(v0.mem) = Mem<u64>(v0.mem) != Mem<u64>(v1.mem);
            break;
        case TYPE_PRIMARY_F32:
            Mem<bool>(v0.mem) = Mem<f32>(v0.mem) != Mem<f32>(v1.mem);
            break;
        case TYPE_PRIMARY_F64:
            Mem<bool>(v0.mem) = Mem<f64>(v0.mem) != Mem<f64>(v1.mem);
            break;
        }
        break;
    case TOKEN_LSHIFT:
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<bool>(v0.mem) = Mem<i8>(v0.mem) < Mem<i8>(v1.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<bool>(v0.mem) = Mem<i16>(v0.mem) < Mem<i16>(v1.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<bool>(v0.mem) = Mem<i32>(v0.mem) < Mem<i32>(v1.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<bool>(v0.mem) = Mem<i64>(v0.mem) < Mem<i64>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<bool>(v0.mem) = Mem<u8>(v0.mem) < Mem<u8>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<bool>(v0.mem) = Mem<u16>(v0.mem) < Mem<u16>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<bool>(v0.mem) = Mem<u32>(v0.mem) < Mem<u32>(v1.mem);
            break;
        case TYPE_MODIFIER_RESTRICTED_POINTER:
        case TYPE_MODIFIER_POINTER:
        case TYPE_PRIMARY_UINT64:
            Mem<bool>(v0.mem) = Mem<u64>(v0.mem) < Mem<u64>(v1.mem);
            break;
        case TYPE_PRIMARY_F32:
            Mem<bool>(v0.mem) = Mem<f32>(v0.mem) < Mem<f32>(v1.mem);
            break;
        case TYPE_PRIMARY_F64:
            Mem<bool>(v0.mem) = Mem<f64>(v0.mem) < Mem<f64>(v1.mem);
            break;
        }
        break;
    case TOKEN_RSHIFT:
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<bool>(v0.mem) = Mem<i8>(v0.mem) > Mem<i8>(v1.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<bool>(v0.mem) = Mem<i16>(v0.mem) > Mem<i16>(v1.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<bool>(v0.mem) = Mem<i32>(v0.mem) > Mem<i32>(v1.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<bool>(v0.mem) = Mem<i64>(v0.mem) > Mem<i64>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<bool>(v0.mem) = Mem<u8>(v0.mem) > Mem<u8>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<bool>(v0.mem) = Mem<u16>(v0.mem) > Mem<u16>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<bool>(v0.mem) = Mem<u32>(v0.mem) > Mem<u32>(v1.mem);
            break;
        case TYPE_MODIFIER_RESTRICTED_POINTER:
        case TYPE_MODIFIER_POINTER:
        case TYPE_PRIMARY_UINT64:
            Mem<bool>(v0.mem) = Mem<u64>(v0.mem) > Mem<u64>(v1.mem);
            break;
        case TYPE_PRIMARY_F32:
            Mem<bool>(v0.mem) = Mem<f32>(v0.mem) > Mem<f32>(v1.mem);
            break;
        case TYPE_PRIMARY_F64:
            Mem<bool>(v0.mem) = Mem<f64>(v0.mem) > Mem<f64>(v1.mem);
            break;
        }
        break;
    case TOKEN_AMPERSAND_AMPERSAND:
        Mem<bool>(v0.mem) = Mem<bool>(v0.mem) && Mem<bool>(v1.mem);
        break;
    case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
        Mem<bool>(v0.mem) = Mem<bool>(v0.mem) || Mem<bool>(v1.mem);
        break;
    case TOKEN_EQUALS_EQUALS:
        switch (v0.type) {
        case TYPE_PRIMARY_BOOL:
            Mem<bool>(v0.mem) = Mem<bool>(v0.mem) == Mem<bool>(v1.mem);
            break;
        case TYPE_PRIMARY_INT8:
            Mem<bool>(v0.mem) = Mem<i8>(v0.mem) == Mem<i8>(v1.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<bool>(v0.mem) = Mem<i16>(v0.mem) == Mem<i16>(v1.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<bool>(v0.mem) = Mem<i32>(v0.mem) == Mem<i32>(v1.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<bool>(v0.mem) = Mem<i64>(v0.mem) == Mem<i64>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<bool>(v0.mem) = Mem<u8>(v0.mem) == Mem<u8>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<bool>(v0.mem) = Mem<u16>(v0.mem) == Mem<u16>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<bool>(v0.mem) = Mem<u32>(v0.mem) == Mem<u32>(v1.mem);
            break;
        case TYPE_MODIFIER_RESTRICTED_POINTER:
        case TYPE_MODIFIER_POINTER:
        case TYPE_PRIMARY_UINT64:
            Mem<bool>(v0.mem) = Mem<u64>(v0.mem) == Mem<u64>(v1.mem);
            break;
        case TYPE_PRIMARY_F32:
            Mem<bool>(v0.mem) = Mem<f32>(v0.mem) == Mem<f32>(v1.mem);
            break;
        case TYPE_PRIMARY_F64:
            Mem<bool>(v0.mem) = Mem<f64>(v0.mem) == Mem<f64>(v1.mem);
            break;
        }
        break;
    case TOKEN_VERTICAL_BAR:
        Mem<u64>(v0.mem) = Mem<u64>(v0.mem) | Mem<u64>(v1.mem);
        break;
    case TOKEN_CIRCUMFLEX:
        Mem<u64>(v0.mem) = Mem<u64>(v0.mem) ^ Mem<u64>(v1.mem);
        break;
    case TOKEN_AMPERSAND:
        Mem<u64>(v0.mem) = Mem<u64>(v0.mem) & Mem<u64>(v1.mem);
        break;
    case TOKEN_ASTERISK:
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<i8>(v0.mem) = Mem<i8>(v0.mem) * Mem<i8>(v1.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<i16>(v0.mem) = Mem<i16>(v0.mem) * Mem<i16>(v1.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<i32>(v0.mem) = Mem<i32>(v0.mem) * Mem<i32>(v1.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<i64>(v0.mem) = Mem<i64>(v0.mem) * Mem<i64>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<u8>(v0.mem) = Mem<u8>(v0.mem) * Mem<u8>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<u16>(v0.mem) = Mem<u16>(v0.mem) * Mem<u16>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<u32>(v0.mem) = Mem<u32>(v0.mem) * Mem<u32>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT64:
            Mem<u64>(v0.mem) = Mem<u64>(v0.mem) * Mem<u64>(v1.mem);
            break;
        case TYPE_PRIMARY_F32:
            Mem<f32>(v0.mem) = Mem<f32>(v0.mem) * Mem<f32>(v1.mem);
            break;
        case TYPE_PRIMARY_F64:
            Mem<f64>(v0.mem) = Mem<f64>(v0.mem) * Mem<f64>(v1.mem);
            break;
        }
        break;
    case TOKEN_SLASH:
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<i8>(v0.mem) = Mem<i8>(v0.mem) / Mem<i8>(v1.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<i16>(v0.mem) = Mem<i16>(v0.mem) / Mem<i16>(v1.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<i32>(v0.mem) = Mem<i32>(v0.mem) / Mem<i32>(v1.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<i64>(v0.mem) = Mem<i64>(v0.mem) / Mem<i64>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<u8>(v0.mem) = Mem<u8>(v0.mem) / Mem<u8>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<u16>(v0.mem) = Mem<u16>(v0.mem) / Mem<u16>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<u32>(v0.mem) = Mem<u32>(v0.mem) / Mem<u32>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT64:
            Mem<u64>(v0.mem) = Mem<u64>(v0.mem) / Mem<u64>(v1.mem);
            break;
        case TYPE_PRIMARY_F32:
            Mem<f32>(v0.mem) = Mem<f32>(v0.mem) / Mem<f32>(v1.mem);
            break;
        case TYPE_PRIMARY_F64:
            Mem<f64>(v0.mem) = Mem<f64>(v0.mem) / Mem<f64>(v1.mem);
            break;
        }
        break;
    case TOKEN_PLUS:
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<i8>(v0.mem) = Mem<i8>(v0.mem) + Mem<i8>(v1.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<i16>(v0.mem) = Mem<i16>(v0.mem) + Mem<i16>(v1.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<i32>(v0.mem) = Mem<i32>(v0.mem) + Mem<i32>(v1.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<i64>(v0.mem) = Mem<i64>(v0.mem) + Mem<i64>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<u8>(v0.mem) = Mem<u8>(v0.mem) + Mem<u8>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<u16>(v0.mem) = Mem<u16>(v0.mem) + Mem<u16>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<u32>(v0.mem) = Mem<u32>(v0.mem) + Mem<u32>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT64:
            Mem<u64>(v0.mem) = Mem<u64>(v0.mem) + Mem<u64>(v1.mem);
            break;
        case TYPE_PRIMARY_F32:
            Mem<f32>(v0.mem) = Mem<f32>(v0.mem) + Mem<f32>(v1.mem);
            break;
        case TYPE_PRIMARY_F64:
            Mem<f64>(v0.mem) = Mem<f64>(v0.mem) + Mem<f64>(v1.mem);
            break;

        case TYPE_MODIFIER_RESTRICTED_POINTER:
        case TYPE_MODIFIER_POINTER:
            switch (v1.type) {
            case TYPE_PRIMARY_INT8:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) + Mem<i8>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_INT16:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) + Mem<i16>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_INT32:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) + Mem<i32>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_INT64:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) + Mem<i64>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_UINT8:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) + Mem<u8>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_UINT16:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) + Mem<u16>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_UINT32:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) + Mem<u32>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_UINT64:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) + Mem<u64>(v1.mem) * size;
                break;
            }
            break;
        }
        break;
    case TOKEN_MINUS:
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<i8>(v0.mem) = Mem<i8>(v0.mem) - Mem<i8>(v1.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<i16>(v0.mem) = Mem<i16>(v0.mem) - Mem<i16>(v1.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<i32>(v0.mem) = Mem<i32>(v0.mem) - Mem<i32>(v1.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<i64>(v0.mem) = Mem<i64>(v0.mem) - Mem<i64>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<u8>(v0.mem) = Mem<u8>(v0.mem) - Mem<u8>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<u16>(v0.mem) = Mem<u16>(v0.mem) - Mem<u16>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<u32>(v0.mem) = Mem<u32>(v0.mem) - Mem<u32>(v1.mem);
            break;
        case TYPE_PRIMARY_UINT64:
            Mem<u64>(v0.mem) = Mem<u64>(v0.mem) - Mem<u64>(v1.mem);
            break;
        case TYPE_PRIMARY_F32:
            Mem<f32>(v0.mem) = Mem<f32>(v0.mem) - Mem<f32>(v1.mem);
            break;
        case TYPE_PRIMARY_F64:
            Mem<f64>(v0.mem) = Mem<f64>(v0.mem) - Mem<f64>(v1.mem);
            break;

        case TYPE_MODIFIER_RESTRICTED_POINTER:
        case TYPE_MODIFIER_POINTER:
            switch (v1.type) {
            case TYPE_PRIMARY_INT8:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) - Mem<i8>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_INT16:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) - Mem<i16>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_INT32:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) - Mem<i32>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_INT64:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) - Mem<i64>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_UINT8:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) - Mem<u8>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_UINT16:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) - Mem<u16>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_UINT32:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) - Mem<u32>(v1.mem) * size;
                break;
            case TYPE_PRIMARY_UINT64:
                Mem<u64>(v0.mem) = Mem<u64>(v0.mem) - Mem<u64>(v1.mem) * size;
                break;
            }
            break;
        }
        break;
    }
    return v0;
}
Value GetMaximumValueOfType(TypeName t) {

    Value v;
    v.type = t;
    switch (t) {
    case TYPE_PRIMARY_BOOL:
        Mem<i64>(v.mem) = 1;
        break;
    case TYPE_PRIMARY_CHAR:
    case TYPE_PRIMARY_INT8:
        Mem<i64>(v.mem) = (u8(1) << 7) - 1;
        break;
    case TYPE_PRIMARY_INT16:
        Mem<i64>(v.mem) = (u16(1) << 15) - 1;
        break;
    case TYPE_PRIMARY_INT32:
        Mem<i64>(v.mem) = (u32(1) << 31) - 1;
        break;
    case TYPE_PRIMARY_INT64:
        Mem<i64>(v.mem) = (u64(1) << 63) - 1;
        break;
    case TYPE_PRIMARY_UINT8:
        Mem<u64>(v.mem) = u8(~u8(0));
        break;
    case TYPE_PRIMARY_UINT16:
        Mem<u64>(v.mem) = u16(~u16(0));
        break;
    case TYPE_PRIMARY_UINT32:
        Mem<u64>(v.mem) = u32(~u32(0));
        break;
    case TYPE_PRIMARY_UINT64:
        Mem<u64>(v.mem) = ~u64(0);
        break;
    case TYPE_PRIMARY_F32:
        Mem<f32>(v.mem) = 0;
        break;
    case TYPE_PRIMARY_F64:
        Mem<f64>(v.mem) = 0;
        break;
    }
    return v;
}
Value GetMinimumValueOfType(TypeName t) {

    Value v;
    v.type = t;
    switch (t) {
    case TYPE_PRIMARY_BOOL:
        Mem<i64>(v.mem) = 0;
        break;
    case TYPE_PRIMARY_CHAR:
    case TYPE_PRIMARY_INT8:
        Mem<i64>(v.mem) = i8(u8(1) << 7);
        break;
    case TYPE_PRIMARY_INT16:
        Mem<i64>(v.mem) = i16(u16(1) << 15);
        break;
    case TYPE_PRIMARY_INT32:
        Mem<i64>(v.mem) = i32(u32(1) << 31);
        break;
    case TYPE_PRIMARY_INT64:
        Mem<i64>(v.mem) = i64(u64(1) << 63);
        break;
    case TYPE_PRIMARY_UINT8:
        Mem<i64>(v.mem) = 0;
        break;
    case TYPE_PRIMARY_UINT16:
        Mem<i64>(v.mem) = 0;
        break;
    case TYPE_PRIMARY_UINT32:
        Mem<i64>(v.mem) = 0;
        break;
    case TYPE_PRIMARY_UINT64:
        Mem<i64>(v.mem) = 0;
        break;
    case TYPE_PRIMARY_F32:
        Mem<f32>(v.mem) = 0;
        break;
    case TYPE_PRIMARY_F64:
        Mem<f64>(v.mem) = 0;
        break;
    }
    return v;
}
Value ConvertValue(Value in, TypeName to) {

    Value out = in;
    out.type = to;

    switch (to) {
    case TYPE_PRIMARY_BOOL:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_INT64:
        case TYPE_PRIMARY_UINT8:
        case TYPE_PRIMARY_UINT16:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_UINT64:
            Mem<i64>(out.mem) = Mem<bool>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<bool>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            Mem<bool>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
        break;
    case TYPE_PRIMARY_CHAR:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_INT64:
        case TYPE_PRIMARY_UINT8:
        case TYPE_PRIMARY_UINT16:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_UINT64:
            Mem<i64>(out.mem) = Mem<char>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<char>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            Mem<char>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
    case TYPE_PRIMARY_INT8:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_INT64:
        case TYPE_PRIMARY_UINT8:
        case TYPE_PRIMARY_UINT16:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_UINT64:
            Mem<i64>(out.mem) = Mem<i8>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<i8>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            Mem<i8>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
    case TYPE_PRIMARY_INT16:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_INT64:
        case TYPE_PRIMARY_UINT8:
        case TYPE_PRIMARY_UINT16:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_UINT64:
            Mem<i64>(out.mem) = Mem<i16>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<i16>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            Mem<i16>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
    case TYPE_PRIMARY_INT32:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_INT64:
        case TYPE_PRIMARY_UINT8:
        case TYPE_PRIMARY_UINT16:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_UINT64:
            Mem<i64>(out.mem) = Mem<i32>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<i32>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            Mem<i32>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
    case TYPE_PRIMARY_INT64:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_INT64:
        case TYPE_PRIMARY_UINT8:
        case TYPE_PRIMARY_UINT16:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_UINT64:
            Mem<i64>(out.mem) = Mem<i64>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<i64>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            Mem<i64>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
    case TYPE_PRIMARY_UINT8:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_INT64:
        case TYPE_PRIMARY_UINT8:
        case TYPE_PRIMARY_UINT16:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_UINT64:
            Mem<u64>(out.mem) = Mem<u8>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<u8>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            Mem<u8>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
    case TYPE_PRIMARY_UINT16:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_INT64:
        case TYPE_PRIMARY_UINT8:
        case TYPE_PRIMARY_UINT16:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_UINT64:
            Mem<u64>(out.mem) = Mem<u16>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<u16>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            Mem<u16>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
    case TYPE_PRIMARY_UINT32:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_INT64:
        case TYPE_PRIMARY_UINT8:
        case TYPE_PRIMARY_UINT16:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_UINT64:
            Mem<u64>(out.mem) = Mem<u32>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<u32>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            Mem<u32>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
    case TYPE_PRIMARY_UINT64:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_INT64:
            Mem<u64>(out.mem) = Mem<i64>(in.mem);
            return out;
        case TYPE_PRIMARY_UINT8:
        case TYPE_PRIMARY_UINT16:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_UINT64:
            Mem<u64>(out.mem) = Mem<u64>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<u64>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            Mem<u64>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
    case TYPE_PRIMARY_F32:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
            Mem<f32>(out.mem) = Mem<i8>(in.mem);
            return out;
        case TYPE_PRIMARY_INT16:
            Mem<f32>(out.mem) = Mem<i16>(in.mem);
            return out;
        case TYPE_PRIMARY_INT32:
            Mem<f32>(out.mem) = Mem<i32>(in.mem);
            return out;
        case TYPE_PRIMARY_INT64:
            Mem<f32>(out.mem) = Mem<i64>(in.mem);
            return out;
        case TYPE_PRIMARY_UINT8:
            Mem<f32>(out.mem) = Mem<u8>(in.mem);
            return out;
        case TYPE_PRIMARY_UINT16:
            Mem<f32>(out.mem) = Mem<u16>(in.mem);
            return out;
        case TYPE_PRIMARY_UINT32:
            Mem<f32>(out.mem) = Mem<u32>(in.mem);
            return out;
        case TYPE_PRIMARY_UINT64:
            Mem<f32>(out.mem) = Mem<u64>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            return out;
        case TYPE_PRIMARY_F64:
            Mem<f32>(out.mem) = Mem<f64>(in.mem);
            return out;
        }
    case TYPE_PRIMARY_F64:
        switch (in.type) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
            Mem<f64>(out.mem) = Mem<i8>(in.mem);
            return out;
        case TYPE_PRIMARY_INT16:
            Mem<f64>(out.mem) = Mem<i16>(in.mem);
            return out;
        case TYPE_PRIMARY_INT32:
            Mem<f64>(out.mem) = Mem<i32>(in.mem);
            return out;
        case TYPE_PRIMARY_INT64:
            Mem<f64>(out.mem) = Mem<i64>(in.mem);
            return out;
        case TYPE_PRIMARY_UINT8:
            Mem<f64>(out.mem) = Mem<u8>(in.mem);
            return out;
        case TYPE_PRIMARY_UINT16:
            Mem<f64>(out.mem) = Mem<u16>(in.mem);
            return out;
        case TYPE_PRIMARY_UINT32:
            Mem<f64>(out.mem) = Mem<u32>(in.mem);
            return out;
        case TYPE_PRIMARY_UINT64:
            Mem<f64>(out.mem) = Mem<u64>(in.mem);
            return out;
        case TYPE_PRIMARY_F32:
            Mem<f64>(out.mem) = Mem<f32>(in.mem);
            return out;
        case TYPE_PRIMARY_F64:
            return out;
        }
    }
    return out;
}

bool IsMemoryReachable(Compiler *compiler, u32 from, u32 to, u32* visited) {

    byte *const baseMem = compiler->mem;
    SSAMemoryDef *mem = (SSAMemoryDef *)(baseMem + from);

    if (from == to)
        return true;

    if(from == ~u32(0)) {
        SSAMemoryDef* toMem = (SSAMemoryDef*)(baseMem + to);
        for(u32 i = 0; i < toMem->predecessors.edgeCount; i++) {
            u32 pred = Mem<u32>(baseMem + toMem->predecessors.edges + i * sizeof(u32));
            if(pred == from) return true;
        }
        return false;
    }

    for (u32 i = 0; i < mem->successors.edgeCount; i++) {
        u32 succ = Mem<u32>(baseMem + mem->successors.edges + i * sizeof(u32));
        if (succ == to)
            return true;
        
        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == to)
                return true;
            if (visited[k] == succ) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        visited[++visited[0]] = succ;

        if (IsMemoryReachable(compiler, succ, to, visited))
            return true;
    }

    return false;
}
u32 FindMemoryMergePointBackWards(Compiler *compiler, u32 from, u32 visitedCount, u32 *visited) {

    for(u32 i = 0; i < visitedCount; i++) {
        if(visited[i] == from) {
            return from;
        }
    }
    visited[visitedCount++] = from;

    ASSERT(from != ~u32(0));
    byte *const baseMem = compiler->mem;
    SSAMemoryDef *memory = (SSAMemoryDef *)(baseMem + from);

    for (u32 i = 0; i < memory->predecessors.edgeCount; i++) {
        u32 predMemPtr = Mem<u32>(baseMem + memory->predecessors.edges + i * sizeof(u32));
        SSAMemoryDef* predMem = (SSAMemoryDef *)(baseMem + predMemPtr);

        auto r = FindMemoryMergePointBackWards(compiler, predMemPtr, visitedCount, visited);
        if(r != ~u32(0)) return r;
    }
    return ~u32(0);
}
u32 WalkMemoryBackWards(Compiler *compiler, u32 from, u32 to, u32 *result) {

    u32 ret = 0;
    ASSERT(from != ~u32(0));
    byte *const baseMem = compiler->mem;
    SSAMemoryDef *memory = (SSAMemoryDef *)(baseMem + from);
    
    if(from == to) return ret;
    result[ret++] = from;
    for (u32 i = 0; i < memory->predecessors.edgeCount; i++) {
        u32 predMemPtr = Mem<u32>(baseMem + memory->predecessors.edges + i * sizeof(u32));
        SSAMemoryDef* predMem = (SSAMemoryDef *)(baseMem + predMemPtr);

        ret += WalkMemoryBackWards(compiler, predMemPtr, to, result+ret);
    }
    return ret;
}
void WalkMemory(Compiler *compiler, u32 from, u32 to, u32 *result) {

    ASSERT(from != ~u32(0) && to != ~u32(0));
    byte *const baseMem = compiler->mem;
    SSAMemoryDef *memory = (SSAMemoryDef *)(baseMem + from);
    
    if(memory->type == MEMORY_DEF) {
        result[++result[0]] = from;
    }

    if(from == to) return;
    for (u32 i = 0; i < memory->successors.edgeCount; i++) {
        u32 succMemPtr = Mem<u32>(baseMem + memory->successors.edges + i * sizeof(u32));
        SSAMemoryDef* succMem = (SSAMemoryDef *)(baseMem + succMemPtr);

        bool memoryVisited = false;
        for(u32 k = 1; k < result[0]+1; k++) {
            if(result[k] == succMemPtr) {
                memoryVisited = true;
                break;
            }
        }
        if(memoryVisited) continue;

        WalkMemory(compiler, succMemPtr, to, result);
    }
    return;
}
bool DependsOnMemory(Compiler* compiler, u32 defPtr) {

    byte* const baseMem = compiler->mem;
    SSADefinition* def = (SSADefinition*)(baseMem + defPtr);

    if(def->opr == SSA_MEMORY_LOAD) return true;
    if(def->opr != SSA_CALL) return false;

    SSADefinition* callee = (SSADefinition*)(baseMem + def->operand0);
    if(callee->opr == SSA_FUNCTION) {
        SSAFunction* fn = (SSAFunction*)(baseMem + callee->operand0);
        for(u32 i = 0; i < fn->params.size; i++) {
            auto& it = fn->params[i];
            if(it.used) {
                if(it.observed.size) {
                    return true;
                }
            }
        }
    }
    else {
        FnTypeExpr* fn = (FnTypeExpr*)(baseMem + callee->type.index);
        for(u32 i = 0; i < fn->param_count; i++) {
            TypeExpr arg = Mem<TypeExpr>(baseMem + fn->params + i * sizeof(TypeExpr));
            auto last = GetLastType(baseMem, arg);
            if(last == TYPE_MODIFIER_POINTER || last == TYPE_MODIFIER_RESTRICTED_POINTER) {
                return true;
            }
        }
    }
    return false;
}
bool HasSideEffects(Compiler* compiler, u32 defPtr) {

    byte* const baseMem = compiler->mem;
    SSADefinition* def = (SSADefinition*)(baseMem + defPtr);

    if(def->opr == CONSTRAINT) return true;
    if(def->opr == SSA_MEMORY_STORE) return true;
    if(def->opr != SSA_CALL) return false;

    SSADefinition* callee = (SSADefinition*)(baseMem + def->operand0);
    if(callee->opr == SSA_FUNCTION) {
        SSAFunction* fn = (SSAFunction*)(baseMem + callee->operand0);
        for(u32 i = 0; i < fn->params.size; i++) {
            auto& it = fn->params[i];
            if(it.used) {
                if(it.clobbered.size) {
                    return true;
                }
            }
        }
    }
    else {
        FnTypeExpr* fn = (FnTypeExpr*)(baseMem + callee->type.index);
        for(u32 i = 0; i < fn->param_count; i++) {
            TypeExpr arg = Mem<TypeExpr>(baseMem + fn->params + i * sizeof(TypeExpr));
            auto last = GetLastType(baseMem, arg);
            if(last == TYPE_MODIFIER_POINTER || last == TYPE_MODIFIER_RESTRICTED_POINTER) {
                return true;
            }
        }
    }
    return false;
}

bool EQSSAHelper(void *user, u32 defPtr0, u32 defPtr1) {

    Compiler *compiler = (Compiler *)user;
    Mem<u32>(compiler->cfgTraversalMemory + 32 * KILO_BYTE) = 0;
    return EQSSA(compiler, defPtr0, defPtr1, (u32*)(compiler->cfgTraversalMemory + 32 * KILO_BYTE));
}

bool SSACallEQ(Compiler* compiler, u32 callPtr0, u32 callPtr1, u32* mem) {

    byte *const baseMem = compiler->mem;

    SSADefinition* call0 = (SSADefinition*)(baseMem + callPtr0);
    SSADefinition* call1 = (SSADefinition*)(baseMem + callPtr1);
    {
        mem[0] = 0;
        const bool eq = EQSSA(compiler, call0->operand0, call1->operand0, mem);
        if(!eq) return false;
    }

    if(HasSideEffects(compiler, callPtr0) || HasSideEffects(compiler, callPtr1)) return false;
    if(DependsOnMemory(compiler, callPtr0) || DependsOnMemory(compiler, callPtr1)) {
        return false;
    }

    SSADefinition* callee0 =  (SSADefinition*)(baseMem + call0->operand0);
    SSADefinition* callee1 =  (SSADefinition*)(baseMem + call1->operand0);

    auto args0 = GetCallArgs(baseMem, call0);
    auto args1 = GetCallArgs(baseMem, call1);
    if(args0.argCount != args1.argCount) return false;

    if(callee0->opr == SSA_FUNCTION) {
        SSAFunction* fn0 = (SSAFunction*)(baseMem + callee0->operand0);
        SSAFunction* fn1 = (SSAFunction*)(baseMem + callee1->operand0);
        ASSERT(fn0 == fn1);

        for(u32 i = 0; i < args0.argCount; i++) {

            auto& it0 = fn0->params[i];
            if(it0.used) {
                mem[0] = 0;
                bool eq = EQSSA(compiler, args0.ptr[i], args1.ptr[i], mem);
                if(!eq) return false;
            }
        }
        return true;
    }

    for(u32 i = 0; i < args0.argCount; i++) {
        mem[0] = 0;
        bool eq = EQSSA(compiler, args0.ptr[i], args1.ptr[i], mem);
        if(!eq) return false;
    }

    return true;
}

bool IsDefUnary(SSADefinition *def) {
    return def->opr == SSA_BITWISE_NEG ||
           def->opr == SSA_SUB ||
           def->opr == SSA_CONVERT ||
           def->opr == SSA_COPY ||
           def->opr == SSA_ALLOCA;
}
bool IsDefBinaryAll(SSADefinition *def) {
    return def->opr != SSA_CONSTANT &&
           def->opr != SSA_PHI_NODE &&
           def->opr != SSA_UN_INIT &&
           def->opr != SSA_FN_PARAMETER &&
           def->opr != SSA_FUNCTION && 
           def->opr != POSITIVE_INF && 
           def->opr != NEGATIVE_INF;
}
bool IsDefBinary(SSADefinition *def) {
    return def->opr != SSA_CONSTANT &&
           def->opr != SSA_PHI_NODE &&
           def->opr != SSA_ALLOCA &&
           def->opr != SSA_COPY &&
           def->opr != SSA_CALL &&
           def->opr != SSA_MEMORY_LOAD &&
           def->opr != SSA_MEMORY_STORE &&
           def->opr != SSA_UN_INIT &&
           def->opr != SSA_FN_PARAMETER &&
           def->opr != SSA_MINUS &&
           def->opr != SSA_BITWISE_NEG &&
           def->opr != SSA_CONVERT &&
           def->opr != SSA_FUNCTION && 
           def->opr != POSITIVE_INF && 
           def->opr != NEGATIVE_INF;
}
bool DefOp2(SSADefinition* def) {
    return def->opr != SSA_CONVERT &&
           def->opr != SSA_PHI_NODE &&
           def->opr != SSA_CALL &&
           def->opr != SSA_UN_INIT &&
           def->opr != SSA_ALLOCA &&
           def->opr != SSA_FUNCTION &&
           def->opr != SSA_FN_PARAMETER &&
           def->opr != SSA_MEMORY_LOAD &&
           def->opr != SSA_MINUS &&
           def->opr != SSA_BITWISE_NEG &&
           def->opr != SSA_CONVERT &&
           def->opr != SSA_FUNCTION && 
           def->opr != POSITIVE_INF && 
           def->opr != NEGATIVE_INF &&
           def->opr != SSA_COPY;
}
bool DefOp1(SSADefinition* def) {
    return def->opr == SSA_MEMORY_LOAD ||
           def->opr == SSA_CONVERT ||
           def->opr == SSA_BITWISE_NEG ||
           def->opr == SSA_MINUS ||
           def->opr == SSA_COPY;
}

bool StrictEQSSA(byte* const baseMem, u32 defTree0, u32 defTree1, bool* traversed) {

    if(defTree0 == defTree1) return true;

    auto def0 = (SSADefinition*)(baseMem + defTree0);
    auto def1 = (SSADefinition*)(baseMem + defTree1);

    if(traversed[def0->value]) return false;
    traversed[def0->value] = true;

    if(def0->opr == def1->opr) {
        if(IsDefBinary(def0)) {
            return StrictEQSSA(baseMem, def0->operand0, def1->operand0, traversed) && StrictEQSSA(baseMem, def0->operand1, def1->operand1, traversed);
        }
        else if(IsDefUnary(def0)) {
            return StrictEQSSA(baseMem, def0->operand0, def1->operand0, traversed);
        }
        else if(def0->opr == SSA_PHI_NODE) {
            u32 count0 = GetPhiOperandCount(def0);
            u32 count1 = GetPhiOperandCount(def1);
            if(count0 != count1) return false;

            u32* operands0 = GetPhiOperandPtr(baseMem, def0);
            u32* operands1 = GetPhiOperandPtr(baseMem, def1);

            for(u32 i = 0; i < count0; i++) {
                if(!StrictEQSSA(baseMem, operands0[i], operands1[i], traversed)) return false;
            }
            return true;
        }
        else if(def0->opr == SSA_CALL) {
            auto args0 = GetCallArgs(baseMem, def0);
            auto args1 = GetCallArgs(baseMem, def1);

            if(args0.argCount != args1.argCount) return false;

            for(u32 i = 0; i < args0.argCount; i++) {
                if(!StrictEQSSA(baseMem, args0.ptr[i], args1.ptr[i], traversed)) return false;
            }
            return true;
        }
        else if(def0->opr == SSA_CONSTANT) {
            auto v0 = Mem<Value>(baseMem + def0->operand0);
            auto v1 = Mem<Value>(baseMem + def1->operand0);
            return ValuesEq(v0, v1);
        }
    }
    return false;
}

bool EQSSA(void *user, u32 defPtr0, u32 defPtr1, u32 *visited) {

    if (defPtr0 == defPtr1)
        return true;
    if ((defPtr0 == ~u32(0)) && (defPtr1 == ~u32(0)))
        return true;
    if ((defPtr0 == ~u32(0)) ^ (defPtr1 == ~u32(0)))
        return false;

    Compiler *compiler = (Compiler *)user;
    byte *const baseMem = compiler->mem;
    SSADefinition *inst0 = (SSADefinition *)(baseMem + defPtr0);
    SSADefinition *inst1 = (SSADefinition *)(baseMem + defPtr1);
    if (inst0->opr == SSA_FN_PARAMETER || inst1->opr == SSA_FN_PARAMETER) {
        if (inst0->opr == inst1->opr && defPtr0 == defPtr1) {
            return true;
        }
        return false;
    }
    if (inst0->opr == EXPRESSION_MEMORY_STORE || inst1->opr == EXPRESSION_MEMORY_STORE) {
        return false;
    }

    bool eq = (inst0->opr == inst1->opr);
    if (inst0->opr == SSA_PHI_NODE || inst1->opr == SSA_PHI_NODE && (inst1->opr != inst0->opr)) {

        SSADefinition *phi = inst0->opr == SSA_PHI_NODE ? inst0 : inst1;
        u32 other = inst0->opr == SSA_PHI_NODE ? defPtr1 : defPtr0;

        u32 *ptr = GetPhiOperandPtr(baseMem, phi);
        for (u32 i = 1; i < visited[0] + 1; i++) {
            if (visited[i] == other)
                return false;
        }

        visited[++visited[0]] = defPtr0;
        visited[++visited[0]] = defPtr1;
        bool eq = phi->operand0 != 0;
        for (u32 i = 0; i < phi->operand0; i++) {
            for (u32 k = 0; k < phi->operand0; k++) {
                if (!EQSSA(compiler, ptr[k], ptr[i], visited))
                    return false;
            }
        }
        if (eq) {
            return EQSSA(compiler, ptr[0], other, visited);
        }
        return false;
    }

    if (!eq)
        return false;

    switch (inst0->opr) {
    case CONSTRAINT:
    case CONSTRAINT_INFO:return false;
    case SSA_COPY:
        return EQSSA(compiler, inst0->operand0, inst1->operand0, visited);
    case SSA_CONSTANT:
        eq &= Mem<u64>(Mem<Value>(compiler->mem + inst0->operand0).mem) == Mem<u64>(Mem<Value>(compiler->mem + inst1->operand0).mem);
        return eq;
    case SSA_BITWISE_NEG:
        eq &= inst0->opr == inst1->opr;
        eq &= TypesEqual(baseMem, baseMem, baseMem, baseMem, inst0->type, inst0->type);
        eq &= EQSSA(compiler, inst0->operand0, inst1->operand0, visited);
        return eq;
    case SSA_FUNCTION:
        return inst0->operand0 == inst1->operand0;
    case SSA_CONVERT: {
        if (!TypesEqual(baseMem, baseMem, baseMem, baseMem, inst0->type, inst1->type)) {
            return false;
        }
        eq &= EQSSA(compiler, inst0->operand0, inst1->operand0, visited);
        return eq;
    }
    case SSA_UN_INIT:
        return TypesEqual(baseMem, baseMem, baseMem, baseMem, inst0->type, inst1->type);
    case SSA_MEMORY_STORE:
        return false;
    case SSA_MEMORY_LOAD:
        return false;
    case SSA_PHI_NODE: {
        if (inst0->operand0 != inst1->operand0)
            return false;

        u32 *ptr0 = GetPhiOperandPtr(baseMem, inst0);
        u32 *ptr1 = GetPhiOperandPtr(baseMem, inst1);

        visited[++visited[0]] = defPtr0;
        visited[++visited[0]] = defPtr1;
        for (u32 i = 0; i < inst0->operand0; i++) {
            eq &= (ptr0[i] == ptr1[i]);
        }
        return eq;
    }
    case SSA_CALL:
        return SSACallEQ(compiler, defPtr0, defPtr1, visited+visited[0]);
    default:
        if (IsOprCommutative(inst0->opr)) {

            if (EQSSA(user, inst0->operand0, inst1->operand0, visited)) {
                eq &= EQSSA(user, inst0->operand1, inst1->operand1, visited);
            } else if (EQSSA(user, inst0->operand0, inst1->operand1, visited)) {
                eq &= EQSSA(user, inst0->operand1, inst1->operand0, visited);
            } else {
                eq &= false;
            }
        } else {
            eq &= EQSSA(user, inst0->operand0, inst1->operand0, visited);
            eq &= EQSSA(user, inst0->operand1, inst1->operand1, visited);
        }
        eq &= TypesEqual(compiler->mem, compiler->mem, compiler->mem, compiler->mem, inst0->type, inst0->type);
        return eq;
    }
}
u64 HashSSA(void *user, u32 defPtr) {
    u64 hash = 7;

    Compiler* compiler = (Compiler*)user;
    byte* const baseMem = compiler->mem;
    SSADefinition *def = (SSADefinition *)(baseMem + defPtr);
    
    def->opr;
    switch (def->opr) {
    case CONSTRAINT:return 7;
    case SSA_COPY:
        return HashSSA(user, def->operand0) * 31 + def->opr;
    case SSA_ALLOCA:
        return HashSSA(user, def->operand0);
    case SSA_CONSTANT: {
        auto v = (Value*)(baseMem + def->operand0);
        hash += HashValue(*v);
    } break;
    case SSA_BITWISE_NEG:
        hash += def->opr;
        hash += HashSSA(user, def->operand0) * 31;
        hash += HashType(baseMem, def->type);
        break;
    case SSA_FUNCTION: {
        hash += def->opr * 31;
        hash += def->operand0 * 31;
        break;
    }
    case SSA_CONVERT: {
        hash += HashSSA(user, def->operand0) * 31;
        hash += HashType(baseMem, def->type);
        break;
    }
    case SSA_FN_PARAMETER:
    case SSA_UN_INIT:
        hash += HashType(baseMem, def->type);
        return hash;
    case SSA_MEMORY_LOAD: {
        hash += HashSSA(user, def->operand0) * 31;
        hash += HashType(baseMem, def->type);
        break;
    }
    case SSA_MEMORY_STORE: {
        hash += HashSSA(user, def->operand0) * 31;
        hash += HashSSA(user, def->operand1) * 31;
        return hash;
    }
    case SSA_PHI_NODE: {

        u32* ptr = GetPhiOperandPtr(baseMem , def);
        u32 count = GetPhiOperandCount(def);

        bool eq = def->operand0 != 0;
        hash += count * 31;
        for (u32 i = 0; i < count; i++) {
            hash += ptr[i] * 31;
        }
        break;
    }
    default:
        hash += def->opr;
        hash += HashSSA(user, def->operand0) * 31;
        hash += HashSSA(user, def->operand1) * 31;
        hash += HashType(baseMem, def->type);
        break;
    case SSA_CALL: {
        hash += HashSSA(user, def->operand0) * 31;

        if (def->operand1 != 0) {
            u32 i = 0;
            auto op1Ptr = baseMem + def->operand1;
            u32 arg = Mem<u32>(baseMem + def->operand1);
            while (arg != 0) {
                hash += HashSSA(user, arg) * 31;
                i += sizeof(u32);
                arg = Mem<u32>(baseMem + def->operand1 + i);
            }
        }
        break;
    }
    }
    return hash;
}
bool IsDefInBlock(Compiler* compiler, u32 defPtr, SSABasicBlock* block) {
    byte* const baseMem = compiler->mem;
    auto blockPtr = ((SSADefinition*)(baseMem + defPtr))->block;
    return ((SSABasicBlock*)(baseMem + blockPtr)) == block;
}
SSADefinition *GetNextDef(const byte *const mem, u32 ptr) {
    return ptr == 0 ? nullptr : (SSADefinition *)(mem + ptr);
}
SSAMemoryDef *GetNextMem(const byte *const mem, u32 ptr) {
    return ptr == 0 ? nullptr : (SSAMemoryDef*)(mem + ptr);
}
void RemoveDefFromBlock(CompilerContext context, u32 defPtr) {


    byte* const baseMem = context.compiler->mem;
    SSADefinition *def = (SSADefinition *)(baseMem + defPtr);

    SSADefinition *next = GetNextDef(baseMem, def->nextDef);
    SSADefinition *prev = GetNextDef(baseMem, def->prevDef);

    SSABasicBlock* block = (SSABasicBlock*)(baseMem + def->block);

    if (next) {
        next->prevDef = def->prevDef;
    } else {
        block->lastDef = def->prevDef;
        if (prev) {
            Mem<SSADefinition>(baseMem + block->lastDef).nextDef = 0;
        }
    }
    if (prev) {
        prev->nextDef = def->nextDef;
    } else {
        block->firstDef = def->nextDef;
        if (next) {
            Mem<SSADefinition>(baseMem + block->firstDef).prevDef = 0;
        }
    }

    def->prevDef = 0;
    def->nextDef = 0;
}


void RemoveDef(CompilerContext context, u32 defPtr) {

    byte* const baseMem = context.compiler->mem;
    SSADefinition *def = (SSADefinition *)(baseMem + defPtr);
    SSABasicBlock* block = (SSABasicBlock*)(baseMem + def->block);

    RemoveDefFromBlock(context, defPtr);
    pool_free(&context.compiler->ssaDefPool, def);
}
void PushFrontMem(Compiler *compiler, SSABasicBlock *block, u32 memPtr) {

    byte* baseMem = compiler->mem;
    SSAMemoryDef *def = (SSAMemoryDef *)(compiler->mem + memPtr);
    if (block->firstMem == 0) {
        block->firstMem = memPtr;
        block->lastMem = memPtr;
        def->next = 0;
        def->prev = 0;
    } else {
        ASSERT(GetPointer<SSAMemoryDef>(baseMem, block->firstMem)->prev == 0);
        Mem<SSAMemoryDef>(baseMem + block->firstMem).prev = memPtr;
        def->next = (u64)block->firstMem - (u64)compiler->mem;
        def->prev = 0;
        block->firstMem = memPtr;
    }
    def->block = (byte*)block - compiler->mem;
}
void PushBackMem(byte* baseMem, SSABasicBlock *block, u32 memPtr) {

    SSAMemoryDef *def = (SSAMemoryDef*)(baseMem + memPtr);
    if (block->firstMem == 0) {
        block->firstMem = memPtr;
        block->lastMem = memPtr;
        def->prev = 0;
        def->next = 0;
    } else {
        ASSERT(block->lastDef);
        Mem<SSAMemoryDef>(baseMem + block->lastMem).next = memPtr;
        def->prev = (u64)block->lastMem - (u64)baseMem;
        block->lastMem = memPtr;
        def->next = 0;
    }
    def->block = (byte*)block - baseMem;
}

void PushBackDefIntoBlock(CompilerContext context, SSABasicBlock *block, u32 defPtr) {

    byte* baseMem = context.compiler->mem;
    SSADefinition *def = (SSADefinition *)(context.compiler->mem + defPtr);
    if (block->firstDef == 0) {

        block->firstDef = defPtr;
        block->lastDef = defPtr;
        def->prevDef = 0;
        def->nextDef = 0;
    }
    else {
        ASSERT(block->lastDef);
        Mem<SSADefinition>(baseMem + block->lastDef).nextDef = defPtr;
        def->prevDef = block->lastDef;
        block->lastDef = defPtr;
        def->nextDef = 0;
    }
    def->block = (byte*)block - context.compiler->mem;
}
void PushBackDef(CompilerContext context, SSABasicBlock *block, u32 defPtr) {

    PushBackDefIntoBlock(context, block, defPtr);
    SSADefinition *def = (SSADefinition *)(context.compiler->mem + defPtr);
    def->block = (u64)block - (u64)context.compiler->mem;
    u32 index = block->values.Find(context.compiler, defPtr);
    if(index == ~u32()) block->values.Insert(context.compiler, defPtr);
}
void PushFrontDef(Compiler *compiler, SSABasicBlock *block, u32 defPtr) {

    byte* baseMem = compiler->mem;
    SSADefinition *def = (SSADefinition *)(compiler->mem + defPtr);
    if (block->firstDef == 0) {
        block->firstDef = defPtr;
        block->lastDef = defPtr;
        def->nextDef = 0;
        def->prevDef = 0;
    } else {
        auto firstDef = GetPointer<SSADefinition>(baseMem, block->firstDef);
        ASSERT(firstDef->prevDef == 0);
        firstDef->prevDef = defPtr;
        def->nextDef = block->firstDef;
        def->prevDef = 0;
        block->firstDef = defPtr;
    }
    def->block = (u64)block - (u64)compiler->mem;
}
void InsertAfterDef(Compiler *compiler, SSABasicBlock *block, u32 defPtr, u32 defPtrInsert) {

    ASSERT(defPtr && defPtrInsert);
    byte* const baseMem = compiler->mem;
    auto def = GetPointer<SSADefinition>(baseMem, defPtr);
    auto defInsert = GetPointer<SSADefinition>(baseMem, defPtrInsert);
    auto next = GetPointer<SSADefinition>(baseMem, def->nextDef);

    defInsert->nextDef = def->nextDef;
    defInsert->prevDef = defPtr;
    def->nextDef = defPtrInsert;
    
    if (next) {
        next->prevDef = defPtrInsert;
    } else {
        block->lastDef = defPtrInsert;
    }

}
void InsertBeforeDef(Compiler *compiler, SSABasicBlock *block, u32 pos, u32 defPtrInsert) {

    ASSERT(pos && defPtrInsert);
    byte* const baseMem = compiler->mem;

    auto def = GetPointer<SSADefinition>(baseMem, pos);
    auto defInsert = GetPointer<SSADefinition>(baseMem, defPtrInsert);
    auto prev = GetPointer<SSADefinition>(baseMem, def->nextDef);

    defInsert->nextDef = pos;
    defInsert->prevDef = def->prevDef;
    def->prevDef = defPtrInsert;
    
    if (prev) {
        prev->nextDef = defPtrInsert;
    } else {
        block->firstDef = defPtrInsert;
    }
}

SSABasicBlock* GetBlockFromMemory(byte* baseMem, SSAMemoryDef* memory) {
    switch(memory->type) {
    case MEMORY_DEF:
        {
            SSADefinition* def = (SSADefinition*)(baseMem + memory->ssaDef);
            return (SSABasicBlock*)(baseMem + def->block);
        }
    case MEMORY_PHI:
    case MEMORY_UNKOWN:
        return (SSABasicBlock*)(baseMem + memory->ssaDef);
    }

    ASSERT(false);
}
void MoveMem(Compiler* compiler, u32 memDefPtr, SSABasicBlock* to, SSABasicBlock* from) {

    byte* const baseMem = compiler->mem;
    SSAMemoryDef* mem = (SSAMemoryDef*)(baseMem + memDefPtr);
    ASSERT(GetBlockFromMemory(baseMem, mem) == from);    
    ASSERT(mem->type != MEMORY_PHI);

    SSAMemoryDef *next = (mem->next == 0 ? nullptr : (SSAMemoryDef*)(baseMem + mem->next));
    SSAMemoryDef *prev = (mem->prev == 0 ? nullptr : (SSAMemoryDef*)(baseMem + mem->prev));
    
    if (next) {
        next->prev = mem->prev;
    } else {
        from->lastMem = (byte*)prev - baseMem;
        if (prev) {
            GetPointer<SSAMemoryDef>(baseMem, from->lastMem)->next = 0;
        }
    }
    if (prev) {
        prev->next = mem->next;
    } else {
        from->firstMem = (byte*)next - baseMem;
        if (next) {
            GetPointer<SSAMemoryDef>(baseMem, from->firstMem)->prev = 0;
        }
    }
    PushBackMem(baseMem, to, memDefPtr);
}
void MoveDef(CompilerContext context, u32 defPtr, SSABasicBlock* to, SSABasicBlock* from) {

    byte* const baseMem = context.compiler->mem;
    SSADefinition* def = (SSADefinition*)(baseMem + defPtr);
    ASSERT((SSABasicBlock*)(baseMem + def->block) == from);

    switch(def->opr) {
    case SSA_CONSTANT:
    case SSA_FUNCTION:
    case SSA_FN_PARAMETER:
    case SSA_UN_INIT:
    case SSA_ALLOCA:
        break;
    case SSA_CALL:
        {
            u32 index = to->values.Find(context.compiler, def->operand0);
            if(index == ~u32(0)) {
                MoveDef(context, def->operand0, to, from);
            }
            auto args = GetCallArgs(baseMem, def);
            for(u32 i = 0; i < args.argCount; i++) {
                index = to->values.Find(context.compiler, args.ptr[i]);
                if(index == ~u32(0)) {
                    MoveDef(context, args.ptr[i], to, from);
                }
            }
            break;
        }
    case SSA_MEMORY_LOAD:
    case SSA_CONVERT:
    case SSA_SUB:
    case SSA_BITWISE_NEG:
        {
            u32 index = to->values.Find(context.compiler, def->operand0);
            if(index == ~u32(0)) {
                MoveDef(context, def->operand0, to, from);
            }
        }
        break;
    case SSA_MEMORY_STORE:
    default:
        {
            u32 index = to->values.Find(context.compiler, def->operand0);
            if(index == ~u32(0)) {
                MoveDef(context, def->operand0, to, from);
            }
            index = to->values.Find(context.compiler, def->operand1);
            if(index == ~u32(0)) {
                MoveDef(context, def->operand1, to, from);
            }
            break;
        }
    }

    RemoveDefFromBlock(context, defPtr);
    PushBackDefIntoBlock(context, to, defPtr);
    def->block = (byte*)to - baseMem;
}



u32 MakeDefWithSideEffects(CompilerContext context, SSABasicBlock *block, u32 defPtr) {

    SSADefinition *def = (SSADefinition *)(context.compiler->mem + defPtr);
    def->value = context.fn->maxSSAName++;

    PushBackDef(context, block, defPtr);
    return defPtr;
}
u32 MakeDef(CompilerContext context, SSABasicBlock *block, u32 defPtr) {

    SSADefinition *def = (SSADefinition *)(context.compiler->mem + defPtr);
    def->value = context.fn->maxSSAName++;
    PushBackDef(context, block, defPtr);
    return defPtr;
}

void DefineSymbolInBlock(CompilerContext context, SSABasicBlock *block, Token symbol, u32 defPtr) {

    VarDefintion *defs = context.compiler->basicBlockVarDefs[block->name].key;
    u32 count = context.compiler->basicBlockVarDefs[block->name].value;

    SSADefinition *def = (SSADefinition *)(context.compiler->mem + defPtr);
    VarDefintion *tmp = (VarDefintion *)LOG(global_malloc_debug(sizeof(VarDefintion) * (count + 1)));
    memcpy(tmp, defs, sizeof(VarDefintion) * count);
    tmp[count].name = symbol;
    tmp[count].defPtr = defPtr;

    LOG(global_free_debug(defs));
    context.compiler->basicBlockVarDefs[block->name].key = tmp;
    context.compiler->basicBlockVarDefs[block->name].value++;
}

void CFGNoOp(CompilerContext context, SSABasicBlock * block, u32 * visited) {}
template <typename Return_t, typename... Args>
using optimize_pass_t = Return_t (*)(CompilerContext, SSABasicBlock *, bool* , Args...);
template <typename Return_t, typename... Args>
Return_t TraverseCFG(CompilerContext context, SSABasicBlock *currentBlock, bool* visited, optimize_pass_t<Return_t, Args...> fn, Args... args) {

    bool retBool = false;
    u32 retInt = 0;

    for (u32 i = 0; i < currentBlock->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(context.compiler->mem + currentBlock->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(context.compiler->mem + succBlockPtr);

        if(visited[succBlock->name] == true) continue;
        visited[succBlock->name] = true;

        if constexpr (std::is_same<Return_t, bool>::value) {
            retBool |= fn(context, succBlock, visited, args...);
        }
        else if constexpr (std::is_integral<Return_t>::value) {
            retInt += fn(context, succBlock, visited, args...);
        }
        else {
            fn(context, succBlock, visited, args...);
        }
    }

    if constexpr (std::is_same<Return_t, bool>::value) {
        return retBool;
    }
    if constexpr (std::is_integral<Return_t>::value) {
        return retInt;
    }
}

void RemoveDefHelper(CompilerContext context, SSABasicBlock* block, bool* traverseMem, u32 defPtr) {

    byte* const baseMem = context.compiler->mem;
    SSADefinition *def = (SSADefinition *)(baseMem + defPtr);

    u32 index = block->values.Find(context.compiler, defPtr);
    if(index != ~u32(0)) {
        block->values.mem[index].key = ~u32(0);
        block->values.mem[index].value = ~u32(0);
    }
    TraverseCFG(context, block, traverseMem, RemoveDefHelper, defPtr);
}
u32 RerouteDefs(CompilerContext context, SSABasicBlock *block, bool* visited, u32 oldValPtr, u32 newValPtr) {

    u32 ret = 0;
    byte *const baseMem = context.compiler->mem;

    for (u32 i = 0; i < block->phiCount; i++) {
        u32 phiPtr = Mem<u32>(baseMem + block->phis + i * sizeof(u32));
        SSADefinition *phi = (SSADefinition *)(baseMem + phiPtr);

        u32 *ptr = GetPhiOperandPtr(baseMem, phi);
        u32 count = GetPhiOperandCount(phi);
        for (u32 i = 0; i < phi->operand0; i++) {
            if (ptr[i] == oldValPtr) {
                ptr[i] = newValPtr;
                ret++;
            }
        }
    }

    auto memIt = GetPointer<SSAMemoryDef>(baseMem, block->firstMem);
    while(memIt) {
        switch(memIt->type) {
        case MEMORY_DEF:
            {
                auto range = Mem<SymbolicRange>(baseMem + memIt->addressRanges.edges);
                if(range.lower == oldValPtr) {
                    Mem<SymbolicRange>(baseMem + memIt->addressRanges.edges).lower = newValPtr;
                    ret++;
                }
                if(range.upper == oldValPtr) {
                    Mem<SymbolicRange>(baseMem + memIt->addressRanges.edges).upper = newValPtr;
                    ret++;
                }
                break;
            }
        default:break;
        }

        memIt = GetPointer<SSAMemoryDef>(baseMem, memIt->next);
    }

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while (it) {

        switch (it->opr) {
        case SSA_CONSTANT:
        case SSA_UN_INIT:
        case SSA_FN_PARAMETER:
        case SSA_PHI_NODE:
        case SSA_FUNCTION:
        case SSA_ALLOCA:
            break;
        case SSA_MEMORY_LOAD:
            if (it->operand0 == oldValPtr) {
                it->operand0 = newValPtr;
                ret++;
            }
            break;
        case SSA_MEMORY_STORE: {
            if (it->operand0 == oldValPtr) {
                it->operand0 = newValPtr;
                ret++;
            }
            if (it->operand1 == oldValPtr) {
                it->operand1 = newValPtr;
                ret++;
            }
            break;
        }
        case SSA_CALL: {
            if (it->operand0 == oldValPtr) {
                it->operand0 = newValPtr;
                ret++;
            }
            auto args = GetCallArgs(baseMem, it);
            for (u32 i = 0; i < args.argCount; i++) {
                if (args.ptr[i] == oldValPtr) {
                    args.ptr[i] = newValPtr;
                    ret++;
                }
            }
            break;
        }

        case SSA_CONVERT:
            if (it->operand0 == oldValPtr) {
                it->operand0 = newValPtr;
                ret++;
            }
            break;
        default:
            if (it->operand0 == oldValPtr) {
                ret++;
                it->operand0 = newValPtr;
            }
            if (it->operand1 == oldValPtr) {
                ret++;
                it->operand1 = newValPtr;
            }
            break;
        }

        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    switch (block->nextBlock.opr) {
    case BRANCH:
        if (block->nextBlock.branch.conditionDef == oldValPtr) {
            block->nextBlock.branch.conditionDef = newValPtr;
            ret++;
        }
        break;
    case RET:
        if (block->nextBlock.ret.retDef == oldValPtr) {
            block->nextBlock.ret.retDef = newValPtr;
            ret++;
        }
        break;
    }

    return ret + TraverseCFG(context, block, visited, RerouteDefs, oldValPtr, newValPtr);
}
void AddPhiToBlock(Compiler *compiler, SSABasicBlock *block, u32 phi) {

    u32 tmpPtr = compiler->miscAllocatorSSA;
    u32 *tmp = (u32 *)AllocateSSA(compiler, sizeof(u32) * (block->phiCount + 1));
    memcpy(tmp, (u32 *)(compiler->mem + block->phis), sizeof(u32) * block->phiCount);
    tmp[block->phiCount++] = phi;
    block->phis = tmpPtr;

    Mem<SSADefinition>(compiler->mem + phi).block = (byte*)block - compiler->mem;
}
SSADefinition *AddPhiNode(CompilerContext context, SSABasicBlock *block, TypeExpr type) {

    u32 tmpPtr = context.compiler->miscAllocatorSSA;
    u32 *tmp = (u32 *)AllocateSSA(context.compiler, sizeof(u32) * (block->phiCount + 1));

    auto phi = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
    tmp[block->phiCount] = (byte*)phi - context.compiler->mem;
    
    *phi = {};
    phi->opr = SSA_PHI_NODE;
    phi->value = context.fn->maxSSAName++;
    phi->block = (u64)block - (u64)context.compiler->mem;
    phi->type = type;

    memcpy(tmp, (u32 *)(context.compiler->mem + block->phis), sizeof(u32) * block->phiCount);

    block->phis = tmpPtr;
    block->phiCount++;

    return phi;
}
void AddPhiNodeIncoming(CompilerContext context, SSADefinition *phi, u32 *incomingDefPtrs, u32 incomingDefPtrCount) {

    ASSERT(phi->opr == SSA_PHI_NODE);

    byte* const baseMem = context.compiler->mem;
    u32* ptr = GetPhiOperandPtr(baseMem, phi);
    u32 count = GetPhiOperandCount(phi);

    u32 unique[incomingDefPtrCount];
    u32 uniqueCount = 0;
    for (u32 k = 0; k < incomingDefPtrCount; k++) {
        bool found = false;
        for (u32 i = 0; i < count; i++) {
            if (ptr[i] == incomingDefPtrs[k]) {
                found = true;
                break;
            }
        }
        if (!found) {

            if (incomingDefPtrs[k] == ~u32(0)) {
                ASSERT(false);

                auto un_init = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
                incomingDefPtrs[k] = (byte*)un_init - context.compiler->mem;
                *un_init = {};
                un_init->opr = SSA_UN_INIT;
                incomingDefPtrs[k] = MakeDef(context, (SSABasicBlock *)(context.compiler->mem + phi->block), incomingDefPtrs[k]);
            }

            unique[uniqueCount++] = incomingDefPtrs[k];
        }
    }

    phi->operand1 = context.compiler->miscAllocatorSSA;
    u32 *tmp = (u32 *)AllocateSSA(context.compiler, sizeof(u32) * (phi->operand0 + uniqueCount));

    memcpy(tmp, unique, sizeof(u32) * uniqueCount);
    memcpy(tmp + uniqueCount, ptr, sizeof(u32) * phi->operand0);
    phi->operand0 += uniqueCount;
}


u32 GetDefUseCount(CompilerContext ctx, SSABasicBlock* block, bool* visited, u32 def) {
    
    if(visited[block->name]) return 0;
    visited[block->name] = true;

    byte *const baseMem = ctx.compiler->mem;
    u32 ret = 0;
    switch(block->nextBlock.opr) {
    case BRANCH:
        ret += block->nextBlock.branch.conditionDef == def;
        break;
    case RET:
        ret += block->nextBlock.ret.retDef == def;
        break;
    }

    for (u32 i = 0; i < block->phiCount; i++) {
        u32 phiPtr = Mem<u32>(baseMem + block->phis + i * sizeof(u32));
        SSADefinition *phi = (SSADefinition *)(baseMem + phiPtr);
        u32 *ops = GetPhiOperandPtr(baseMem, phi);
        u32 count = GetPhiOperandCount(phi);
        for (u32 k = 0; k < count; k++) {
            ret += ops[k] == def;
        }
    }

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {

        if(IsDefBinary(it)) {
            ret += it->operand0 == def;
            ret += it->operand1 == def;
        }
        else if(IsDefUnary(it)) {
            ret += it->operand0 == def;
        }
        else if(it->opr == SSA_CALL) {
            auto args = GetCallArgs(baseMem, it);
            for(u32 i = 0; i < args.argCount; i++) {
                ret += args.ptr[i] == def;
            }
        }

        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    return ret + TraverseCFG(ctx, block, visited, GetDefUseCount, def);
}
u32 GetValUses(CompilerContext context, SSABasicBlock *block, bool* visited, u32 valPtr, u32 *result) {

    byte *const baseMem = context.compiler->mem;
    u32 ret = 0;

    switch(block->nextBlock.opr) {
    case BRANCH:
        if (block->nextBlock.branch.conditionDef == valPtr) {
            result[ret++] = (u64)(&block->nextBlock) - (u64)baseMem;
        }
        break;
    case RET:
        {
            auto retDef = (SSADefinition*)(baseMem + block->nextBlock.ret.retDef);
            if (block->nextBlock.ret.retDef == valPtr) {
                result[ret++] = (u64)(&block->nextBlock) - (u64)baseMem;
            }
        }
        break;
    }

    for (u32 i = 0; i < block->phiCount; i++) {
        u32 phiPtr = Mem<u32>(baseMem + block->phis + i * sizeof(u32));
        SSADefinition *phi = (SSADefinition *)(baseMem + phiPtr);
        u32 *ops = GetPhiOperandPtr(baseMem, phi);
        u32 count = GetPhiOperandCount(phi);
        for (u32 k = 0; k < count; k++) {
            if (ops[k] == valPtr) {
                result[ret++] = phiPtr;
                break;
            }
        }
    }

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while (it != 0) {

        u32 itPtr = (u64)it - (u64)context.compiler->mem;

        switch (it->opr) {
        case SSA_CONSTANT:
        case SSA_UN_INIT:
        case SSA_FN_PARAMETER:
            break;
        case SSA_COPY:
            if(it->operand0 == valPtr) {
                result[ret++] = itPtr;
            }
            break;
        case SSA_PHI_NODE: {
            u32 *ptr = GetPhiOperandPtr(baseMem, it);
            u32 count = GetPhiOperandCount(it);
            for (u32 i = 0; i < count; i++) {
                if (ptr[i] == valPtr) {
                    result[ret++] = itPtr;
                }
            }
            break;
        }
        case SSA_MEMORY_LOAD:
            if (it->operand0 == valPtr) {
                result[ret++] = itPtr;
            }
            break;
        case SSA_MEMORY_STORE:
            if (it->operand0 == valPtr || it->operand1 == valPtr) {
                result[ret++] = itPtr;
            }
            break;
        case SSA_CALL:
        {
            if (it->operand0 == valPtr) {
                result[ret++] = itPtr;
            }
            auto args = GetCallArgs(baseMem, it);
            for (u32 i = 0; i < args.argCount; i++) {
                if (args.ptr[i] == valPtr) {
                    result[ret++] = itPtr;
                    break;
                }
            }
            break;
        } 

        case SSA_CONVERT:
            if (it->operand0 == valPtr) {
                result[ret++] = itPtr;
            }
            break;
        default:
            if (it->operand0 == valPtr || it->operand1 == valPtr) {
                result[ret++] = itPtr;
            }
            break;
        }

        it = (it->nextDef == 0 ? nullptr : (SSADefinition *)(context.compiler->mem + it->nextDef));
    }

    for(u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);

        if(visited[succBlock->name]) continue;
        visited[succBlock->name] = true;

        ret += GetValUses(context, succBlock, visited, valPtr, result+ret);
    }

    return ret;
}
u32 VisitVals(Compiler* compiler, SSABasicBlock* block, u32 opr,u32* result, u32* visited) {

    u32 ret = 0;
    auto it = GetPointer<SSADefinition>(compiler->mem, block->firstDef);
    while (it) {

        if(it->opr == opr) {
            result[ret++] = (u64)it - (u64)compiler->mem;
        }

        it = (it->nextDef == 0 ? nullptr : (SSADefinition*)(compiler->mem + it->nextDef));
    }

    for (u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(compiler->mem + block->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(compiler->mem + succBlockPtr);

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == succBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        visited[(visited[0]++) + 1] = succBlockPtr;
        ret += VisitVals(compiler, succBlock, opr, result+ret, visited);
    }
    return ret;
}

void RemovePhiFromBlock(CompilerContext context, SSABasicBlock *block, u32 i) {
    u32 last = Mem<u32>(context.compiler->mem + block->phis + (--block->phiCount) * sizeof(u32));
    Mem<u32>(context.compiler->mem + block->phis + i * sizeof(u32)) = last;
}
void RemovePhi(CompilerContext context, SSABasicBlock *block, u32 i) {

    u32 phiPtr = Mem<u32>(context.compiler->mem + block->phis + i * sizeof(u32));
    u32 last = Mem<u32>(context.compiler->mem + block->phis + (--block->phiCount) * sizeof(u32));
    Mem<u32>(context.compiler->mem + block->phis + i * sizeof(u32)) = last;
    
    pool_free(&context.compiler->ssaDefPool, context.compiler->mem + phiPtr);
}
void RemoveMemoyPhiFromBlock(CompilerContext context, SSABasicBlock *block, u32 i) {
    u32 last = Mem<u32>(context.compiler->mem + block->memoryPhis + (--block->memoryPhiCount) * sizeof(u32));
    Mem<u32>(context.compiler->mem + block->memoryPhis + i * sizeof(u32)) = last;
}
void RemoveMemoyPhi(CompilerContext context, SSABasicBlock* block, u32 i) {

    u32 phiPtr = Mem<u32>(context.compiler->mem + block->memoryPhis + i * sizeof(u32));
    u32 last = Mem<u32>(context.compiler->mem + block->memoryPhis + (--block->memoryPhiCount) * sizeof(u32));
    Mem<u32>(context.compiler->mem + block->memoryPhis + i * sizeof(u32)) = last;
    
    pool_free(&context.compiler->memoryDefPool, context.compiler->mem + phiPtr);
}

u32 TryRemoveTrivialPhi(CompilerContext context, SSABasicBlock *block, SSADefinition *phi, bool* mem0, u32 *mem1) {

    bool ret = false;
    byte* const baseMem = context.compiler->mem;
    u32 *operands = GetPhiOperandPtr(baseMem, phi);
    u32 count = GetPhiOperandCount(phi);

    u32 phiPtr = (byte*)phi - baseMem;
    SSADefinition* same = nullptr;
    
    for(u32 i = 0; i < phi->operand0; i++) {
        auto operand = (SSADefinition*)(baseMem + operands[i]);
        if(operand == phi) {
            operands[i--] = operands[--phi->operand0];
        }
    }
    for(u32 i = 0; i < phi->operand0; i++) {
        auto operand = (SSADefinition*)(baseMem + operands[i]);
        if(same == operand) {
            continue;
        }
        if(same != nullptr) {
            return phiPtr;
        }
        same = operand;
    }
    u32 blockCount = context.fn->maxBlockName;
    memset(mem0, false, blockCount);
    u32 useCount = GetValUses(context, block, mem0, phiPtr, mem1);
    memset(mem0, false, blockCount);
    RerouteDefs(context, block, mem0, phiPtr, (byte*)same - baseMem);

    for(u32 i = 0; i < useCount; i++) {
        auto use = (SSADefinition*)(baseMem + mem1[i]);
        if(use->opr == SSA_PHI_NODE) {
            auto useBlock = (SSABasicBlock*)(baseMem + use->block);
            TryRemoveTrivialPhi(context, useBlock, use, mem0, mem1+useCount);
        }
    }

    u32* phis = (u32*)(baseMem + block->phis);
    for(u32 i = 0; i < block->phiCount; i++) {
        if(phis[i] == phiPtr) {
            RemovePhi(context, block, i);
            return (byte*)same - baseMem;
        }
    }
    ASSERT(false);
}
const char* GetSSAOprStr(TokenType opr) {
    switch (opr) {
    case SSA_CMP_LESS_THAN_OR_EQ:
        return "CMP <=";
    case SSA_CMP_BIGGER_THAN_OR_EQ:
        return "CMP >=";
    case SSA_CMP_NOT_EQ:
        return "CMP !=";
    case SSA_CMP_EQ:
        return "CMP ==";
    case SSA_CMP_LESS_THAN:
        return "CMP <";
    case SSA_CMP_BIGGER_THAN:
        return "CMP >";
    case SSA_LOGICAL_AND:
        return "LOGICAL_AND";
    case SSA_LOGICAL_OR:
        return "LOGICAL_OR";
    case SSA_LOGICAL_NEG:
        return "LOGICAL_NEG";
    case SSA_BITWISE_OR:
        return "BITWISE_OR";
    case SSA_BITWISE_XOR:
        return "BITWISE_XOR";
    case SSA_BITWISE_AND:
        return "BITWISE_AND";
    case SSA_BITWISE_NEG:
        return "BITWISE_NEG";
    case SSA_MUL:
        return "MUL";
    case SSA_DIV:
        return "DIV";
    case SSA_ADD:
        return "ADD";
    case SSA_SUB:
        return "SUB";
    case SSA_LEFT_BIT_SHIFT:
        return "LEFT_BIT_SHIFT";
    case SSA_RIGHT_BIT_SHIFT:
        return "RIGHT_BIT_SHIFT";
    default:
        ASSERT(false);
    }

    return nullptr;
}

void PrintSSADef(byte* baseMem, SSADefinition *def) {

    switch (def->opr) {
    case BRANCH:
        {
            SSABranch* branch = (SSABranch*)def;
            SSABasicBlock *thenBlock = (SSABasicBlock *)(baseMem + branch->thenBlock);
            SSABasicBlock *elseBlock = (SSABasicBlock *)(baseMem + branch->elseBlock);

            u32 condPtr = branch->conditionDef; 
            if (condPtr != ~u32(0)) {
                SSADefinition *cond = (SSADefinition *)(baseMem + branch->conditionDef);
                global_print("%s%i%s%i%s%i%\n", "br (", cond->value, ") then: ", thenBlock->name, " else: ", elseBlock->name);
            }
            else {
                global_print("%s%i%s%i%\n", "br (SSA_UN_INIT) then: ", thenBlock->name, " else: ", elseBlock->name);
            }
            break;
        }
    case JMP:
        {
            SSAJmp* jmp = (SSAJmp*)def;
            ASSERT(jmp->targetBlock != ~u32(0));
            SSABasicBlock *target = (SSABasicBlock*)(baseMem + jmp->targetBlock);
            global_print("%s%i%\n", "jmp ", target->name);
            break;
        }
    case RET:
        {
            SSARet* ret = (SSARet*)def;
            global_print("%s", "RET ");
            if(ret->retDef != ~u32(0)) {
                SSADefinition* retDef = (SSADefinition*)(baseMem + ret->retDef);
                global_print("%i", retDef->value);
            }
            global_print("%\n");
            break;
        }
    case SSA_CONSTANT: {
        Value *v = (Value*)(baseMem + def->operand0);
        auto str = GetValueTStr(*v);
        global_print("%i%s%s%c", def->value, " IMMEDIATE ", str , ' ');
        PrintValue(*v);
    } break;
    case SSA_COPY:
        global_print("%i%s%i", def->value," SSA_COPY ", ((SSADefinition*)(baseMem + def->operand0))->value);
        break;
    case SSA_FUNCTION: {
        {
            auto name = ((SSAFunction*)(baseMem + def->operand0))->name;
            global_print("%i%s%s*%c", def->value," FN_ADDRESS \"", name.text, name.lenght, '\"');
        }
        break;
    }
    case CONSTRAINT:
        global_print("%s%s", "CONSTRAINT", def->operand0 > 1 ? "S {" : " {");
        for(u32 i = 0; i < def->operand0; i++) {
            u32* ptr = (u32*)(baseMem + def->operand1);
            PrintSSADef(baseMem, (SSADefinition*)(baseMem + ptr[i]));
            if(i+1 < def->operand0) global_print("%s", " && ");
        }
        global_print("%c", '}');
        break;
    case POSITIVE_INF:
        global_print("%i%s", def->value, " +inf");
        break;
    case NEGATIVE_INF:
        global_print("%i%s", def->value, " -inf");
        break;
    case RANGE_EXPRESSION:
        global_print("%i%s%i%s%i%c", def->value, " [", Mem<SSADefinition>(baseMem + def->operand0).value, " : ", Mem<SSADefinition>(baseMem + def->operand1).value, ']');
        break;
    case SSA_POWER:
        global_print("%i%s%i%c%i", def->value, " SSA_POWER ", Mem<SSADefinition>(baseMem + def->operand0).value, ' ', Mem<SSADefinition>(baseMem + def->operand1).value);
        break;
    case SSA_MAX:
        global_print("%i%s%i%i", def->value, " SSA_MAX ", Mem<SSADefinition>(baseMem + def->operand0).value, Mem<SSADefinition>(baseMem + def->operand1).value);
        break;
    case SSA_MIN:
        global_print("%i%s%i%i", def->value, " SSA_MIN ", Mem<SSADefinition>(baseMem + def->operand0).value, Mem<SSADefinition>(baseMem + def->operand1).value);
        break;
    case SSA_FN_PARAMETER:
        global_print("%i%s", def->value, " SSA_FN_PARAMETER ");
        PrintTypeExpression(baseMem, baseMem, def->type);
        break;
    case SSA_UN_INIT:
        global_print("%i%s", def->value, " SSA_UN_INIT ");
        PrintTypeExpression(baseMem, baseMem, def->type);
        break;
    case SSA_CONVERT: {
        global_print("%i%s", def->value, " VALUE_CONVERSION ");
        PrintTypeExpression(baseMem, baseMem, def->type);
        global_print("%c%i", ' ', ((SSADefinition *)(baseMem + def->operand0))->value);
        break;
    }
    case SSA_MEMORY_LOAD: {
        global_print("%i%s", def->value, " MEMORY_LOAD ");
        PrintTypeExpression(baseMem, baseMem, def->type);
        global_print("%c%i%c", ' ', ((SSADefinition *)(baseMem + def->operand0))->value, ' ');
        break;
    }
    case SSA_MEMORY_STORE: {
        global_print("%i%s", def->value, " MEMORY_STORE ");
        PrintTypeExpression(baseMem, baseMem, def->type);
        global_print("%c%i", ' ', ((SSADefinition *)(baseMem + def->operand0))->value);
        global_print("%c%i%c", ' ', ((SSADefinition *)(baseMem + def->operand1))->value, ' ');
        break;
    }
    case SSA_PHI_NODE: {
        global_print("%i%s",def->value, " SSA_PHI_NODE ");
        PrintTypeExpression(baseMem, baseMem, def->type);
        global_print("%c", ' ');

        u32 *operands = GetPhiOperandPtr(baseMem, def);
        u32 count = GetPhiOperandCount(def);
        for (u32 i = 0; i < count; i++) {
            SSADefinition *operand = (SSADefinition *)(baseMem + operands[i]);
            global_print("%i%c", operand->value,' ');
        }
        break;
    }
    case SSA_LOGICAL_NEG:
    case SSA_BITWISE_NEG:
    case SSA_MINUS:
        {
            const char *str = GetSSAOprStr((TokenType)def->opr);
            global_print("%i%c%s%c", def->value,' ', str, ' ');
            PrintTypeExpression(baseMem, baseMem, def->type);
            global_print("%c%i", ' ', Mem<SSADefinition>(baseMem + def->operand0).value);
        }
        break;
    case SSA_CALL: {
        global_print("%i%s", def->value, " CALL ");
        PrintTypeExpression(baseMem, baseMem, def->type);
        global_print("%c%i%s", ' ' , Mem<SSADefinition>(baseMem + def->operand0).value, "( ");
        if (def->operand1) {

            u32 i = 0;
            u32 arg = Mem<u32>(baseMem + def->operand1);
            while (arg) {
                global_print("%i%c", ((SSADefinition *)(baseMem + arg))->value << ' ');
                i += sizeof(u32);
                arg = Mem<u32>(baseMem + def->operand1 + i);
            }
        }
        global_print("%c", ')');
        break;
    }
    case SSA_ALLOCA:
        global_print("%i%s", def->value, " SSA_ALLOCA ");
        PrintTypeExpression(baseMem, baseMem, def->type);
        global_print("%s%i%c", " (", Mem<SSADefinition>(baseMem + def->operand0).value, ')');
        break;
    default:
        global_print("%i%c%s%c", def->value, ' ', GetSSAOprStr((TokenType)def->opr), ' ');
        PrintTypeExpression(baseMem, baseMem, def->type);
        global_print("%c%i%c%i", ' ', Mem<SSADefinition>(baseMem + def->operand0).value, ' ',
                                      Mem<SSADefinition>(baseMem + def->operand1).value);
        break;
    }
}
void PrintMemory(Compiler* compiler, u32 memoryPtr, u32* mem) {

    byte *const baseMem = compiler->mem;
    SSAMemoryDef* memory = (SSAMemoryDef*)(baseMem + memoryPtr);

    global_print("%s%i", " {", memory->name);
    switch (memory->type) {
    case MEMORY_NONE:
        global_print("%s", " NONE ");break;
    case MEMORY_DEF:
        global_print("%s", " DEF ");break;
    case MEMORY_PHI:
        global_print("%s", " PHI ");break;
    case MEMORY_UNKOWN:
        global_print("%s", " UNKOWN ");break;
    }
    
    global_print("%s", "from(");
    for(u32 i = 0; i < memory->predecessors.edgeCount; i++) {
        u32 predMemPtr = Mem<u32>(baseMem + memory->predecessors.edges + i * sizeof(u32));
        SSAMemoryDef* predMem = (SSAMemoryDef*)(baseMem + predMemPtr);
        global_print("%i%c", predMem->name, ' ');
    }
    global_print("%s", ") to(");
    for(u32 i = 0; i < memory->successors.edgeCount; i++) {
        u32 succMemPtr = Mem<u32>(baseMem + memory->successors.edges + i * sizeof(u32));
        SSAMemoryDef* succMem = (SSAMemoryDef*)(baseMem + succMemPtr);
        global_print("%i%c", succMem->name, ' ');
    }


    SSADefinition* base = (SSADefinition*)(baseMem + memory->basePtrDef);
    global_print("%s%i%s", ") clobbered base(", base->value, ") ");

    if(memory->type == MEMORY_DEF) {

        auto ranges = (SymbolicRange*)(baseMem + memory->addressRanges.edges);
        PrintSSADef(baseMem, (SSADefinition*)(baseMem + ranges->lower));
        global_print("%s", " , ");
        PrintSSADef(baseMem, (SSADefinition*)(baseMem + ranges->upper));
    }
    global_print("%c", '}');
}
u32 GetIncompletePhiCount(byte* mem) {
    return Mem<u32>(mem);
}
byte* GetEndOfIncompletePhiBuffer(byte* mem) {
    auto incompletePhiCount = GetIncompletePhiCount(mem);
    return mem + 4 + incompletePhiCount * sizeof(IncompletePhi);
}
void WriteSymbol(CompilerContext context, SSABasicBlock *block, Token symbol, u32 val) {

    if (val == ~u32(0))
        return;

    VarDefintion *defs = context.compiler->basicBlockVarDefs[block->name].key;
    u32 defCount = context.compiler->basicBlockVarDefs[block->name].value;
    for (u32 i = 0; i < defCount; i++) {
        if (TokensEquals(defs[i].name, symbol)) {
            defs[i].defPtr = val;
            return;
        }
    }
    DefineSymbolInBlock(context, block, symbol, val);
}

u32 MakeUnkownSymbol(CompilerContext context, SSABasicBlock* block, Token symbol, TypeExpr symbolType) {

    byte* const baseMem = context.compiler->mem;
    u32 defPtr;
    TypeName symbol_t = GetLastType(baseMem, symbolType);

    if (symbol_t == TYPE_MODIFIER_ARRAY || symbol_t > TYPE_COUNT) {

        auto allocaSize = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 allocaSizePtr = (byte*)allocaSize - baseMem;
        *allocaSize = {};
        allocaSize->opr = SSA_CONSTANT;

        allocaSize->operand0 = context.compiler->miscAllocatorSSA;
        Value *v = (Value*)AllocateSSA(context.compiler, sizeof(Value));
        Mem<u64>(v->mem) = GetTypeSize(baseMem, baseMem, symbolType);
        v->type = TYPE_PRIMARY_UINT64;

        allocaSizePtr = MakeDef(context, block, allocaSizePtr);

        auto localMem = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        defPtr = (byte*)localMem - baseMem;
        *localMem = {};
        localMem->opr = SSA_ALLOCA;
        localMem->operand0 = allocaSizePtr;
        localMem->type = {context.compiler->exprAllocator};
        context.compiler->exprAllocator = CpyTypeExpr(baseMem, {context.compiler->exprAllocator}, baseMem, symbolType).index;
        Mem<u32>(baseMem + context.compiler->exprAllocator - 4) = TYPE_MODIFIER_RESTRICTED_POINTER;
        Mem<u32>(baseMem + context.compiler->exprAllocator) = TYPE_NON;
        context.compiler->exprAllocator += 4;

        defPtr = MakeDefWithSideEffects(context, block, defPtr);
    }
    else {

        auto unInit = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        defPtr = (byte*)unInit - baseMem;
        *unInit = {};
        unInit->opr = SSA_UN_INIT;
        unInit->type = symbolType;
        defPtr = MakeDef(context, block, defPtr);
    }

    WriteSymbol(context, block, symbol, defPtr);
    return defPtr;
}

u32 ReadSymbol(CompilerContext context, SSABasicBlock *block, Token symbol, TypeExpr symbolType, byte* incompletes, u32 *visited) {

    byte *const baseMem = context.compiler->mem;
    u32 visitedCount = visited[0];
    for (u32 i = 1; i < visitedCount + 1; i++) {
        if ((SSABasicBlock *)(baseMem + visited[i]) == block) {
            return ~u32(0);
        }
    }
    visited[++visited[0]] = (u64)block - (u64)baseMem;

    VarDefintion *defs = context.compiler->basicBlockVarDefs[block->name].key;
    u32 defCount = context.compiler->basicBlockVarDefs[block->name].value;
    for (u32 i = 0; i < defCount; i++) {
        if (TokensEquals(defs[i].name, symbol)) {
            return defs[i].defPtr;
        }
    }

    auto lastType = GetLastType(baseMem, symbolType);
    bool memoryType = (lastType > TYPE_COUNT || lastType == TYPE_MODIFIER_ARRAY);

    u32 ret = ~u32(0);
    if (block->incomplete && !memoryType) {

        auto incompletePhiCount = Mem<u32>(incompletes);
        auto incompletePhis = (IncompletePhi*)(incompletes + 4);
        auto phi = AddPhiNode(context, block, symbolType);

        ret = (u64)phi - (u64)baseMem;
        incompletePhis[incompletePhiCount].phi = block->phiCount - 1;
        incompletePhis[incompletePhiCount].symbol = symbol;
        incompletePhis[incompletePhiCount].symbolType = symbolType;
        incompletePhis[incompletePhiCount].block = block;
        Mem<u32>(incompletes) = incompletePhiCount + 1;

    }
    else if (block->predecessors.edgeCount == 1) {

        u32 predPtr = Mem<u32>(baseMem + block->predecessors.edges);
        SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + predPtr);

        ret = ReadSymbol(context, predBlock, symbol, symbolType, incompletes, visited);
    }
    else if (block->predecessors.edgeCount == 0) {
        ret = MakeUnkownSymbol(context, block, symbol, symbolType);
    }
    else {

        u32 phiIndex = block->phiCount;
        auto witness = AddPhiNode(context, block, symbolType);
        u32 witnessPtr = (u64)witness - (u64)baseMem;
        WriteSymbol(context, block, symbol, witnessPtr);

        const u32 predCount = block->predecessors.edgeCount;
        u32 symbolDefs[predCount];
        for (u32 i = 0; i < predCount; i++) {
            u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
            SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + predBlockPtr);

            visited[visited[0] + 1] = 0;
            symbolDefs[i] = ReadSymbol(context, predBlock, symbol, symbolType, incompletes, visited + visited[0] + 1);
        }

        RemovePhiFromBlock(context, block, phiIndex);

        bool same = true;
        for (u32 i = 0; i < predCount; i++) {
            if (symbolDefs[i] == witnessPtr)
                continue;
            ret = symbolDefs[i];
            for (u32 k = 0; k < predCount; k++) {
                if (symbolDefs[i] != symbolDefs[k]) {
                    same = false;
                    break;
                }
            }
        }
        if (same) {
            WriteSymbol(context, block, symbol, ret);
            return ret;
        }

        bool phiExist = false;
        for (u32 i = 0; i < block->phiCount; i++) {
            u32 phiPtr = Mem<u32>(baseMem + block->phis + i * sizeof(u32));
            SSADefinition *phi = (SSADefinition *)(baseMem + phiPtr);

            u32 *phiOps = GetPhiOperandPtr(baseMem, phi);
            u32 count = GetPhiOperandCount(phi);
            ASSERT(predCount == count);

            bool allSame = true;
            for (u32 k = 0; k < predCount; k++) {
                if (symbolDefs[k] == ~u32(0))
                    continue;

                bool same = false;
                for (u32 j = 0; j < count; j++) {
                    if (symbolDefs[k] == phiOps[j]) {
                        same = true;
                        break;
                    }
                }

                if (!same) {
                    allSame = false;
                    break;
                }
            }

            if (allSame) {
                phiExist = true;
                ret = phiPtr;
                break;
            }
        }

        if (!phiExist) {
            Mem<u32>(baseMem + block->phis + (block->phiCount++) * sizeof(u32)) = witnessPtr;
            u32* mem0 = visited+visited[0]+1;
            u32* mem1 = (u32*)GetEndOfIncompletePhiBuffer(incompletes);
            AddPhiNodeIncoming(context, witness, symbolDefs, predCount);

            ret = TryRemoveTrivialPhi(context, block, witness, (bool*)mem0, mem1);
        }
    }

    WriteSymbol(context, block, symbol, ret);
    return ret;
}

void AddPhiOperand(CompilerContext context, SSABasicBlock *block, SSADefinition *phi, Token symbol, TypeExpr symbolType, byte *mem, u32* visitMem) {

    u32 operands[block->predecessors.edgeCount];
    for (u32 i = 0; i < block->predecessors.edgeCount; i++) {

        u32 predPtr = Mem<u32>(context.compiler->mem + block->predecessors.edges + i * sizeof(u32));
        SSABasicBlock *predBlock = (SSABasicBlock *)(context.compiler->mem + predPtr);

        visitMem[0] = 0;
        operands[i] = ReadSymbol(context, predBlock, symbol, symbolType, mem, visitMem);
        ASSERT(operands[i] != ~u32(0));
    }
    AddPhiNodeIncoming(context, phi, operands, block->predecessors.edgeCount);
}



bool IsValueConst(byte* baseMem, u32 valPtr) {
    SSADefinition *op = (SSADefinition *)(baseMem + valPtr);
    return (op->opr == EXPRESSION_IMMEDIATE) || (op->opr == EXPRESSION_LITERAL);
}

bool IsConversionTrivial(CompilerContext context, ConversionExpr *conv, byte *mem) {

    GetTypeExpr(context.compiler->mem, conv->from, mem, {0});

    TypeName from_t = GetLastType(mem, {0});
    TypeName to_t = GetLastType(context.compiler->mem, conv->type);
    if ((from_t == TYPE_MODIFIER_POINTER && to_t == TYPE_MODIFIER_POINTER) || to_t == from_t) {
        return true;
    }
}

bool IsValBasePointer(byte* baseMem, u32 defPtr) {

    SSADefinition *def = (SSADefinition *)(baseMem + defPtr);
    TypeName t = GetLastType(baseMem, def->type);
    return t == TYPE_MODIFIER_POINTER || t == TYPE_MODIFIER_RESTRICTED_POINTER;
}
u32 GetBaseAddressFromPointer(CompilerContext context, u32 defPtr, u32* result, u32 *visited) {

    byte *const baseMem = context.compiler->mem;
    SSADefinition *def = (SSADefinition *)(baseMem + defPtr);

    for(u32 i = 1; i < visited[0]+1; i++) {
        if(visited[i] == defPtr) return 0;
    }
    visited[++visited[0]] = defPtr;

    switch (def->opr) {
    case SSA_CONSTANT:
    case SSA_FUNCTION:
    case SSA_FN_PARAMETER:
    case SSA_UN_INIT:
    case SSA_MEMORY_LOAD:
    case SSA_CALL:
    case SSA_ALLOCA:
        if (IsValBasePointer(baseMem, defPtr)) {
            result[0] = defPtr;
            return 1;
        }
        break;
    case SSA_PHI_NODE: {
        u32 *operands = GetPhiOperandPtr(baseMem, def);
        u32 count = GetPhiOperandCount(def);
        u32 ret = 0;
        for (u32 i = 0; i < count; i++) {
            ret += GetBaseAddressFromPointer(context, operands[i], result+ret, visited);
        }
        return ret;
    }
    case SSA_MEMORY_STORE:
        ASSERT(false);
    case SSA_CONVERT:
        {
            auto ret = GetBaseAddressFromPointer(context, def->operand0, result, visited);
            if(ret != 0) return ret;
            if(IsValBasePointer(baseMem, defPtr))  {
                result[0] = defPtr;
                return 1;
            }
            break;
        }
    default:
        {
            u32 l = GetBaseAddressFromPointer(context, def->operand0, result, visited);
            if (l != 0)
                return l;
            u32 r = GetBaseAddressFromPointer(context, def->operand1, result, visited);
            if (r != 0)
                return r;
        }
    }
}

void SetBasePtrIndexFromDef(byte* baseMem, SSADefinition* def, u32 index) {

    switch(def->opr) {
    case SSA_CALL:
        ((SSACallInfo*)(baseMem + def->extraPtr))->base = index;
        break;
    case SSA_MEMORY_LOAD:
        def->operand1 = index;
        break;
    default:
        def->extraPtr = index;
        break;
    }
}
u32 GetBasePtrIndexFromDef(byte* baseMem, SSADefinition* def) {

    switch(def->opr) {
    case SSA_CALL:
        return ((SSACallInfo*)(baseMem + def->extraPtr))->base;
    case SSA_MEMORY_LOAD:
        return def->operand1;
    default:
        return def->extraPtr;
    }
}

void DeSugarPointerArithmetic(Compiler *compiler, BinaryExpr *bin, byte *mem, bool s);
void DeSugarSubScriptDef(Compiler *compiler, BinaryExpr *bin, byte *mem) {

    if (bin->opr == TOKEN_SUB_SCRIPT_OPR) {
        bin->opr = TOKEN_PLUS;
        DeSugarPointerArithmetic(compiler, bin, mem, false);
        bin->opr = TOKEN_SUB_SCRIPT_OPR;
    }
}

void WriteMemoryToBlock(SSABasicBlock* block, u32 memoryPtr, u32 name) {
    for(u32 i = 0; i < block->memoryPtrs.size; i++) {
        if(block->memoryPtrs[i].key == name) {
            block->memoryPtrs[i].value = memoryPtr;
        }
    }

    block->memoryPtrs.PushBack({name,memoryPtr});
}

void InsertAlias(CompilerContext context, MemoryAliasGraph* graph, SSADefinition* base, u32* mem) {

    byte* const baseMem = context.compiler->mem;
    TypeName last = GetLastType(baseMem, base->type);
    u32 name = GetBasePtrIndexFromDef(baseMem, base);
    u32 vertexCount = 0;
    if(last != TYPE_MODIFIER_RESTRICTED_POINTER) {

        bool might[graph->vertexCount];
        u32 vertices[graph->vertexCount];
        for(u32 i = 0; i < graph->vertexCount; i++) {
            SSADefinition* mightAlias = (SSADefinition*)(baseMem + graph->vertices[i].basePtr);
            bool eq = TypesEqual(baseMem, baseMem, baseMem, baseMem, mightAlias->type, base->type);

            if(eq) {
                vertices[vertexCount++] = graph->vertices[i].basePtr;
            }
        }

        for(u32 i = 0; i < vertexCount; i++) {
            InsertAliasEdge(graph, vertices[i], name, might[i] ? ALIAS_TYPE_MIGHT_ALIAS : ALIAS_TYPE_MUST_ALIAS, 0);
        }
    }
    if(!vertexCount) {
        InsertMemoryAliasVertex(graph, name);
    }
    graph->vertices[name].basePtr = (u64)base - (u64)baseMem;
}


SSAMemoryDef* AddMemoryPhi(CompilerContext context, SSABasicBlock* block) {

    byte *const baseMem = context.compiler->mem;

    u32 tmpPtr = context.compiler->miscAllocatorSSA;
    u32 *tmp = (u32 *)AllocateSSA(context.compiler, sizeof(u32) * (block->memoryPhiCount + 1));
    memcpy(tmp, baseMem + block->memoryPhis, block->memoryPhiCount*sizeof(u32));
    block->memoryPhis = tmpPtr;

    auto memoryPhi = (SSAMemoryDef*)pool_allocate(&context.compiler->memoryDefPool);
    u32 phiPtr = (byte*)memoryPhi - baseMem;
    *memoryPhi = {};
    memoryPhi->type = MEMORY_PHI;
    memoryPhi->name = context.compiler->uniqueMemoryName++;
    memoryPhi->ssaDef = (u64)block - (u64)baseMem;

    tmp[block->memoryPhiCount++] = phiPtr;
    return memoryPhi;
}

void InsertMemoryEdge(Compiler *compiler, u32 prevMem, u32 nextMem);

bool DoesValueDependOn(byte* baseMem, u32 val, u32 dependent, u32 *visited) {

    if(val == dependent) return true;
    for(u32 i = 1; i < visited[0]+1; i++) {
        if(visited[i] == dependent) return false;
    }

    visited[++visited[0]] = dependent;
    SSADefinition *op = (SSADefinition *)(baseMem + dependent);
    switch (op->opr) {
    case SSA_CONSTANT:
    case SSA_UN_INIT:
    case SSA_FUNCTION:
    case SSA_ALLOCA:
    case SSA_FN_PARAMETER:
        return false;
    case SSA_COPY:
        if(op->operand0 == val) return true;
        return DoesValueDependOn(baseMem, val, op->operand0, visited);
    case SSA_MEMORY_STORE:
        ASSERT(false);
    case SSA_MEMORY_LOAD:
        if(op->operand0 == val) return true;
        return DoesValueDependOn(baseMem, val, op->operand0, visited);
    case SSA_CONVERT:
        if(op->operand0 == val) return true;
        if(DoesValueDependOn(baseMem, val, op->operand0, visited)) return true;
    case SSA_CALL: {
        bool dep = DoesValueDependOn(baseMem, val, op->operand0, visited);
        auto args = GetCallArgs(baseMem, op);
        for (u32 i = 0; i < args.argCount; i++) {
            dep &= DoesValueDependOn(baseMem, val, args.ptr[i], visited);
        }
        return dep;
    }
    case SSA_PHI_NODE: {
        u32* ptr = GetPhiOperandPtr(baseMem, op);
        u32 count = GetPhiOperandCount(op);

        for (u32 i = 0; i < op->operand0; i++) {
            if (ptr[i] == val)
                return true;
        }
        for (u32 i = 0; i < op->operand0; i++) {
            if (DoesValueDependOn(baseMem, val, ptr[i], visited)) return true;
        }
        return false;
    }
    case SSA_LOGICAL_NEG:
    case SSA_BITWISE_NEG:
    case SSA_MINUS:
        return DoesValueDependOn(baseMem, val, op->operand0, visited);
    default:
        {
            if (op->operand0 == val || val == op->operand1)
                return true;
            return DoesValueDependOn(baseMem, val, op->operand0, visited) ||
                DoesValueDependOn(baseMem, val, op->operand1, visited);
        }
    }
}

u32 MakeUnkownMemory(CompilerContext context, u32 baseAddress, u32* visitMem) {

    byte *const baseMem = context.compiler->mem;
    SSADefinition* base = (SSADefinition*)(baseMem + baseAddress);
    u32 name = GetBasePtrIndexFromDef(baseMem, base);
    if(name == ~u32(0)) {

        auto unInitMemory = (SSAMemoryDef*)pool_allocate(&context.compiler->memoryDefPool);
        *unInitMemory = {};
        unInitMemory->type = MEMORY_UNKOWN;
        unInitMemory->basePtrDef = (u64)base - (u64)baseMem;
        unInitMemory->name = context.compiler->uniqueMemoryName++;
        u32 unInitMemoryPtr = (byte*)unInitMemory - baseMem;

        SSABasicBlock* baseBlock = (SSABasicBlock*)(baseMem + base->block);
        PushFrontMem(context.compiler, baseBlock, unInitMemoryPtr);
        name = context.compiler->memoryName++;
        baseBlock->memoryPtrs.PushBack({name, unInitMemoryPtr});
        SetBasePtrIndexFromDef(baseMem, base, name);
        InsertAlias(context, &context.fn->graph, base, visitMem);

        WriteMemoryToBlock(baseBlock, unInitMemoryPtr, name);
        return unInitMemoryPtr;
    }
}
u32 SearchMemoryInBlockFromBaseAddressName(SSABasicBlock* block, u32 name) {

    for(u32 i = 0; i < block->memoryPtrs.size; i++) {
        if(block->memoryPtrs[i].key == name) {
            return block->memoryPtrs[i].value;
        }
    }
    return ~u32(0);
}
u32 MakeMemory(CompilerContext context, SSABasicBlock *block, u32 storePtr, u32 baseAddressPtr, SymbolicRange *clobberedRange, u32 clobberedRangeCount) {

    byte *const baseMem = context.compiler->mem;
    SSADefinition* base = (SSADefinition*)(baseMem + baseAddressPtr);

    auto memoryDef = (SSAMemoryDef*)pool_allocate(&context.compiler->memoryDefPool);
    *memoryDef = {};
    memoryDef->type = MEMORY_DEF;
    memoryDef->name = context.compiler->uniqueMemoryName++;
    memoryDef->basePtrDef = baseAddressPtr;
    memoryDef->ssaDef = storePtr;
    memoryDef->addressRanges.edgeCount = clobberedRangeCount;
    memoryDef->addressRanges.edges = context.compiler->miscAllocatorSSA;
    
    u32 memoryDefPtr = (byte*)memoryDef - baseMem;
    PushBackMem(baseMem, block, memoryDefPtr);

    SymbolicRange* ranges = (SymbolicRange*)AllocateSSA(context.compiler, clobberedRangeCount * sizeof(SymbolicRange));
    memcpy(ranges, clobberedRange, clobberedRangeCount * sizeof(SymbolicRange));
    return memoryDefPtr;
}






bool DoesPhiFormCycle(byte* baseMem, u32 phiPtr, u32 *visitMem);
SSABasicBlock* GetBlockFromDef(byte* baseMem, u32 ssaDefPtr) {
    SSADefinition* ssaDef = (SSADefinition*)(baseMem + ssaDefPtr);
    return (SSABasicBlock*)(baseMem + ssaDef->block);
}

u32 SSAExpr(CompilerContext context, SSABasicBlock *block, Expr e, byte *IncompletePhiBuffer, u32* visited) {

    byte *const baseMem = context.compiler->mem;
    ExprType e_t = Mem<ExprType>(baseMem + e.index);
    switch (e_t) {
    case EXPRESSION_NULL:
        break;
    case EXPRESSION_IMMEDIATE: {

        auto node = (ImmediateExpr*)(baseMem + e.index);

        auto def = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 defPtr = (byte*)def - baseMem;
        *def = {};
        def->opr = SSA_CONSTANT;
        def->operand0 = ((byte*)&node->v) - baseMem;

        return MakeDef(context, block, defPtr);
    }
    case EXPRESSION_LITERAL: {

        auto node = (LiteralExpr*)(baseMem + e.index);
        
        auto def = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 defPtr = (byte*)def - baseMem;
        *def = {};
        def->opr = SSA_CONSTANT;
        def->type = context.compiler->basicTypes[GetLiteralType(node->literal)];

        def->operand0 = context.compiler->miscAllocatorSSA;
        Value* v = AllocateSSA<Value>(context.compiler);
        *v = GetValueFromLiteral(node->literal);

        return MakeDef(context, block, defPtr);
    }
    case EXPRESSION_UNARY: {
        auto node = (UnaryExpr*)(baseMem + e.index);

        u32 primaryDefPtr = SSAExpr(context, block, node->primaryExpr, IncompletePhiBuffer, visited);

        auto def = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 defPtr = (byte*)def - baseMem;

        *def = {};
        def->opr = node->opr;
        def->operand0 = primaryDefPtr;
        def->operand1 = ~u32(0);

        SSADefinition *primary = (SSADefinition *)(baseMem + primaryDefPtr);
        def->type = primary->type;

        return MakeDef(context, block, defPtr);
    }
    case EXPRESSION_BINARY: {

        auto node = (BinaryExpr*)(baseMem + e.index);

        byte *scratch = GetEndOfIncompletePhiBuffer(IncompletePhiBuffer);
        DeSugarSubScriptDef(context.compiler, node, scratch);

        u32 leftDefPtr = SSAExpr(context, block, node->left, IncompletePhiBuffer, visited);
        u32 rightDefPtr = SSAExpr(context, block, node->right, IncompletePhiBuffer, visited);

        auto binaryDef = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 binaryDefPtr = (byte*)binaryDef - baseMem;

        *binaryDef = {};
        binaryDef->opr = (node->opr == TOKEN_SUB_SCRIPT_OPR ? TOKEN_PLUS : node->opr);
        binaryDef->operand0 = leftDefPtr;
        binaryDef->operand1 = rightDefPtr;

        binaryDef->type = {context.compiler->exprAllocator};
        context.compiler->exprAllocator = GetTypeExpr(baseMem, e, baseMem, {context.compiler->exprAllocator}).index;

        return MakeDef(context, block, binaryDefPtr);
    }
    case EXPRESSION_VARIABLE: {
        
        auto node = (VariableExpr*)(baseMem + e.index);
        auto symbol_t = GetLastType(baseMem, node->var.type);

        u32 defPtr;
        if (symbol_t == TYPE_PRIMARY_FN) {

            auto functionName = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
            defPtr = (byte*)functionName - baseMem;

            *functionName = {};
            functionName->opr = SSA_FUNCTION;
            u32 index = FindSymbol(&context.compiler->symbolTable, node->var.name);
            ASSERT(index != ~u32(0));
            functionName->operand0 = context.compiler->symbolTable[index].extra;

            defPtr = MakeDef(context, block, defPtr);
        }
        else {
            visited[0] = 0;
            defPtr = ReadSymbol(context, block, node->var.name, node->var.type, IncompletePhiBuffer, visited);
            WriteSymbol(context, block, node->var.name, defPtr);
        }
        return defPtr;
    }
    case EXPRESSION_VARIABLE_ASSIGNMENT: {

        auto node = (VariableAssignmentExpr*)(baseMem + e.index);
        u32 valDefPtr = SSAExpr(context, block, node->value, IncompletePhiBuffer, visited);
        
        SSABasicBlock* valDefBlock = GetBlockFromDef(baseMem, valDefPtr);
        if(valDefBlock != block) {

            auto copyDef = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
            u32 copyDefPTr = (byte*)copyDef - baseMem;

            *copyDef = {};
            copyDef->opr = SSA_COPY;
            copyDef->operand0 = valDefPtr;
            valDefPtr = MakeDef(context, block, copyDefPTr);
        }

        WriteSymbol(context, block, node->var.name, valDefPtr);
        return valDefPtr;
    }
    case EXPRESSION_MEMORY_LOAD: {

        auto node = (MemoryLoadExpr*)(baseMem + e.index);
        u32 addDefPtr = SSAExpr(context, block, node->address, IncompletePhiBuffer, visited);

        auto loadDef = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 loadDefPtr = (byte*)loadDef - baseMem;

        *loadDef = {};
        loadDef->opr = SSA_MEMORY_LOAD;
        loadDef->operand0 = addDefPtr;
        loadDef->operand1 = ~u32(0);

        loadDef->type = {context.compiler->exprAllocator};
        context.compiler->exprAllocator = GetTypeExpr(baseMem, node->address, baseMem, {context.compiler->exprAllocator}).index;
        TypeExpr nth = GetNthType(baseMem, loadDef->type, 0);
        Mem<TypeName>(baseMem + nth.index) = TYPE_NON;
        context.compiler->exprAllocator -= 4;

        context.compiler->incompleteMemoryOps.PushBack(&context.compiler->localHeap, {loadDefPtr});
        return MakeDefWithSideEffects(context, block, loadDefPtr);
    }
    case EXPRESSION_MEMORY_STORE: {
        auto node = (MemoryStoreExpr*)(baseMem + e.index);

        u32 addDefPtr = SSAExpr(context, block, node->address, IncompletePhiBuffer, visited);
        u32 valDefPtr = SSAExpr(context, block, node->value, IncompletePhiBuffer, visited);


        auto storeDef = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 storeDefPtr = (byte*)storeDef - baseMem;

        *storeDef = {};
        storeDef->operand0 = addDefPtr;
        storeDef->operand1 = valDefPtr;
        storeDef->opr = SSA_MEMORY_STORE;
        SSADefinition *addDef = (SSADefinition *)(baseMem + addDefPtr);

        storeDef->type = {context.compiler->exprAllocator};
        context.compiler->exprAllocator = GetTypeExpr(baseMem, node->address, baseMem, {context.compiler->exprAllocator}).index;
        TypeExpr nth = GetNthType(baseMem, storeDef->type, 0);
        Mem<TypeName>(context.compiler->mem + nth.index) = TYPE_NON;
        context.compiler->exprAllocator -= 4;

        context.compiler->incompleteMemoryOps.PushBack(&context.compiler->localHeap, {storeDefPtr});
        return MakeDefWithSideEffects(context, block, storeDefPtr);
    }
    case EXPRESSION_ADDRESS_OF: {
        
        auto node = (AddressOf*)(baseMem + e.index);
        return SSAExpr(context, block, node->expr, IncompletePhiBuffer, visited);
    }
    case EXPRESSION_GET: {

        auto node = (DotExpr*)(baseMem + e.index);
        u32 offset = node->offset;
        u32 addDefPtr = SSAExpr(context, block, node->prev, IncompletePhiBuffer, visited);

        auto offsetDef = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 offsetDefPtr = (byte*)offsetDef - baseMem;

        *offsetDef = {};
        offsetDef->opr = SSA_CONSTANT;
        offsetDef->operand0 = context.compiler->miscAllocatorSSA;

        Value *v = AllocateSSA<Value>(context.compiler);
        Mem<u64>(v->mem) = offset;
        v->type = TYPE_PRIMARY_UINT64;

        offsetDefPtr = MakeDef(context, block, offsetDefPtr);

        auto getDef = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 getDefPtr = (byte*)getDef - baseMem;

        *getDef = {};
        getDef->opr = SSA_ADD;
        getDef->operand0 = addDefPtr;
        getDef->operand1 = offsetDefPtr;

        getDef->type.index = context.compiler->exprAllocator;
        context.compiler->exprAllocator = GetTypeExpr(baseMem, e, baseMem, {context.compiler->exprAllocator}).index;

        return MakeDef(context, block, getDefPtr);
    }
    case EXPRESSION_PEEL_TYPE: {
        auto node = (PeelTypeExpr*)(baseMem + e.index);
        return SSAExpr(context, block, node->expr, IncompletePhiBuffer, visited);
    }
    case EXPRESSION_MEM_COPY:
        break;
    case EXPRESSION_CALL: {

        auto node = (CallExpr*)(baseMem + e.index);
        Expr arg = Mem<Expr>(baseMem + node->args.index);
        u32 count = 0;
        while (arg.index != 0) {
            count++;
            arg = Mem<Expr>(baseMem + node->args.index + count * sizeof(Expr));
        }

        u32 ssaArgsPtr = 0;
        if (count != 0) {
            ssaArgsPtr = context.compiler->miscAllocatorSSA;
            u32 *ssaArgs = (u32 *)AllocateSSA(context.compiler, (count + 1) * sizeof(u32));
            ssaArgs[count] = 0;
            arg = Mem<Expr>(baseMem + node->args.index);
            count = 0;
            while (arg.index != 0) {

                ssaArgs[count] = SSAExpr(context, block, arg, IncompletePhiBuffer, visited);
                count++;
                arg = Mem<Expr>(baseMem + node->args.index + count * sizeof(Expr));
            }
        }

        u32 callee = SSAExpr(context, block, node->calleeExpr, IncompletePhiBuffer, visited);

        auto callVal = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 callValPtr = (byte*)callVal - baseMem;

        *callVal = {};
        callVal->opr = SSA_CALL;
        callVal->operand0 = callee;
        callVal->operand1 = ssaArgsPtr;
        callVal->type = {context.compiler->exprAllocator};
        context.compiler->exprAllocator = GetTypeExpr(context.compiler->mem, e, baseMem, {context.compiler->exprAllocator}).index;

        u32 *scratch = (u32*)GetEndOfIncompletePhiBuffer(IncompletePhiBuffer);
        SSADefinition *calleeDef = (SSADefinition *)(baseMem + callee);

        callVal->extraPtr = context.compiler->miscAllocatorSSA;
        auto info = AllocateSSA<SSACallInfo>(context.compiler);
        *info = {};
        info->infoType = CALL_INFO;

        if(calleeDef->opr == SSA_FUNCTION) {
            SSAFunction *fn = (SSAFunction *)(baseMem + calleeDef->operand0);
            SSAFunction* calleeFn = (SSAFunction*)(baseMem + context.compiler->currentFunction);
            //ClobberWorldAtFunctionCall(compiler, block, callValPtr, IncompletePhiBuffer, visited);
        }

        return MakeDef(context, block, callValPtr);
    }
    case EXPRESSION_CONVERSION: {
        auto node = (ConversionExpr*)(baseMem + e.index);
        u32 fromDef = SSAExpr(context, block, node->from, IncompletePhiBuffer, visited);

        if(!IsConversionTrivial(context, node, (byte*)visited)) {

            auto convDef = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
            u32 convDefPtr = (byte*)convDef - baseMem;

            *convDef = {};
            convDef->opr = SSA_CONVERT;
            convDef->operand0 = fromDef;
            convDef->type = node->type;
            return MakeDef(context, block, convDefPtr);
        }
        return fromDef;
    }
    }
}

void InsertCFGEdge(CompilerContext context, u32 pred, u32 succ) {

    bool found = false;
    SSABasicBlock *predBlock = (SSABasicBlock *)(context.compiler->mem + pred);
    SSABasicBlock *succBlock = (SSABasicBlock *)(context.compiler->mem + succ);
    byte *mem = context.compiler->mem;

    for (u32 i = 0; i < predBlock->successors.edgeCount; i++) {
        u32 s = Mem<u32>(mem + predBlock->successors.edges + i * sizeof(u32));
        if (s == succ) {
            found = true;
        }
    }
    if (!found) {

        u32 tmpPtr = context.compiler->miscAllocatorSSA;
        u32 *tmp = (u32 *)AllocateSSA(context.compiler, sizeof(u32) * (predBlock->successors.edgeCount + 1));
        tmp[predBlock->successors.edgeCount] = succ;
        memcpy(tmp, (mem + predBlock->successors.edges), sizeof(u32) * predBlock->successors.edgeCount);
        predBlock->successors.edgeCount++;
        predBlock->successors.edges = tmpPtr;
    }

    found = false;
    for (u32 i = 0; i < succBlock->predecessors.edgeCount; i++) {
        u32 s = Mem<u32>(mem + succBlock->predecessors.edges + i * sizeof(u32));
        if (s == pred) {
            found = true;
        }
    }
    if (!found) {

        u32 tmpPtr = context.compiler->miscAllocatorSSA;
        u32 *tmp = (u32 *)AllocateSSA(context.compiler, sizeof(u32) * (succBlock->predecessors.edgeCount + 1));
        tmp[succBlock->predecessors.edgeCount] = pred;
        memcpy(tmp, (mem + succBlock->predecessors.edges), sizeof(u32) * succBlock->predecessors.edgeCount);
        succBlock->predecessors.edgeCount++;
        succBlock->predecessors.edges = tmpPtr;
    }
}
void RemoveCFGEdge(CompilerContext ctx, u32 pred, u32 succ) {

    byte *const baseMem = ctx.compiler->mem;
    SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + pred);
    SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succ);

    for (u32 i = 0; i < predBlock->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + predBlock->successors.edges + i * sizeof(u32));
        if (succBlockPtr == succ) {
            Mem<u32>(baseMem + predBlock->successors.edges + i * sizeof(u32)) = Mem<u32>(baseMem + predBlock->successors.edges + (--predBlock->successors.edgeCount) * sizeof(u32));
            break;
        }
    }

    for (u32 i = 0; i < succBlock->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + succBlock->predecessors.edges + i * sizeof(u32));
        if (predBlockPtr == pred) {
            Mem<u32>(baseMem + succBlock->predecessors.edges + i * sizeof(u32)) = Mem<u32>(baseMem + succBlock->predecessors.edges + (--succBlock->predecessors.edgeCount) * sizeof(u32));
            break;
        }
    }
}


void CopyValues(CompilerContext context, SSABasicBlock *block) {

    ASSERT(context.compiler != nullptr && block != nullptr);
    byte* const baseMem = context.compiler->mem;
    u32 values[block->predecessors.edgeCount];
    for (u32 i = 0; i < block->predecessors.edgeCount; i++) {

        u32 predPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
        SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + predPtr);

        for (u32 k = 0; k < predBlock->values.cap; k++) {
            if (predBlock->values.mem[k].key != ~u32(0)) {

                bool notFound = false;
                auto value = predBlock->values.mem[k].value;
                values[i] = value;

                for (u32 j = 0; j < block->predecessors.edgeCount; j++) {
                    if (j == i)
                        continue;
                    u32 predPtrJ = Mem<u32>(baseMem + block->predecessors.edges + j * sizeof(u32));
                    SSABasicBlock *predBlockJ = (SSABasicBlock *)(baseMem + predPtrJ);

                    notFound = true;
                    for(u32 z = 0; z < predBlockJ->values.cap; z++) {
                        if(predBlockJ->values.mem[z].value == value) {
                            notFound = false;
                            values[j] = predBlockJ->values.mem[z].value;
                            break;
                        }
                    }
                }

                if (!notFound) {
                    u32 index = block->values.Find(context.compiler, value);
                    if (index == ~u32(0)) {
                        block->values.Insert(context.compiler, value);
                    }
                }
            }
        }
    }
}
void RemoveMemoryEdge(CompilerContext context, u32 prevMem, u32 nextMem) {

    if (prevMem == ~u32(0) || nextMem == ~u32(0))
        return;
    byte *const baseMem = context.compiler->mem;
    SSAMemoryDef *pred = (SSAMemoryDef *)(baseMem + prevMem);
    SSAMemoryDef *succ = (SSAMemoryDef *)(baseMem + nextMem);

    for (u32 i = 0; i < pred->successors.edgeCount; i++) {
        u32 succMem = Mem<u32>(baseMem + pred->successors.edges + i * sizeof(u32));
        if (succMem == nextMem) {
            Mem<u32>(baseMem + pred->successors.edges + i * sizeof(u32)) = Mem<u32>(baseMem + pred->successors.edges + (--pred->successors.edgeCount) * sizeof(u32));
            break;
        }
    }
    for (u32 i = 0; i < succ->predecessors.edgeCount; i++) {
        u32 predMem = Mem<u32>(baseMem + succ->predecessors.edges + i * sizeof(u32));
        if (predMem == prevMem) {
            Mem<u32>(baseMem + succ->predecessors.edges + i * sizeof(u32)) = Mem<u32>(baseMem + succ->predecessors.edges + (--succ->predecessors.edgeCount) * sizeof(u32));
            break;
        }
    }
}
u32 GetMemoryUses(Compiler* compiler, SSABasicBlock* block, u32 memoryPtr, u32* result, u32* visited) {

    u32 ret = 0;
    byte* const baseMem = compiler->mem;
    
    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);

    while(it) {

        if(it->opr == SSA_MEMORY_LOAD) {
            auto info = (SSALoadInfo*)(baseMem + it->extraPtr);
            ASSERT(info->infoType == LOAD_INFO);
            for(u32 i = 0; i < info->memoryUseCount; i++) {
                if(info->memoryUses[i] == memoryPtr) {
                    result[ret++] = (byte*)it - baseMem;
                }
            }
        }
        else if(it->opr == SSA_CALL) {
            ASSERT(false);
        }

        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    for(u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == succBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        visited[++visited[0]] = (byte*)block - baseMem;

        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);
        ret += GetMemoryUses(compiler, succBlock, memoryPtr, result+ret, visited);
    }
    
    return ret;
}
u32 RerouteMemoryUses(CompilerContext context, SSABasicBlock* block, u32 oldMemory, u32 replaceMemory, u32* visited) {
    
    u32 ret = 0;
    byte* const baseMem = context.compiler->mem;   

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {

        if(it->opr == SSA_MEMORY_LOAD) {
            auto info = (SSALoadInfo*)(baseMem + it->extraPtr);
            ASSERT(info->infoType == LOAD_INFO);

            bool incRet = true;
            for(u32 i = 0; i < info->memoryUseCount; i++) {
                if(info->memoryUses[i] == oldMemory) {
                    info->memoryUses[i] = replaceMemory;
                    ret += incRet;
                    incRet = false;
                    break;
                }
            }
        }
        else if(it->opr == SSA_CALL) {
            ASSERT(false);
        }

        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    for(u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == succBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        visited[++visited[0]] = (byte*)block - baseMem;

        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);
        ret += RerouteMemoryUses(context, succBlock, oldMemory, replaceMemory, visited);
    }
    
    return ret;
}
bool TryRemoveTrivialMemoryPhi(CompilerContext context, SSABasicBlock *block, SSAMemoryDef *phi, u32 *mem0, u32 *mem1) {

    bool ret = false;
    byte* const baseMem = context.compiler->mem;
    u32 phiPtr = (byte*)phi - baseMem;
    u32 same = ~u32(0);

    for(u32 i = 0; i < phi->predecessors.edgeCount; i++) {
        u32 operandPtr = Mem<u32>(baseMem + phi->predecessors.edges + i * sizeof(u32));
        
        if(operandPtr == phiPtr) {
            RemoveMemoryEdge(context, operandPtr, phiPtr);
            i--;
        }
    }
    for(u32 i = 0; i < phi->predecessors.edgeCount; i++) {
        u32 operandPtr = Mem<u32>(baseMem + phi->predecessors.edges + i * sizeof(u32));

        if(operandPtr == same) {
            continue;
        }
        if(same != ~u32(0)) {
            return false;
        }
        same = operandPtr;
    }

    mem1[0] = 0;
    RerouteMemoryUses(context, block, phiPtr, same, mem1);
    for(u32 i = 0; i < phi->predecessors.edgeCount; i++) {
        u32 operandPtr = Mem<u32>(baseMem + phi->predecessors.edges + i * sizeof(u32));
        auto operand = (SSAMemoryDef*)(baseMem + operandPtr);
        if(operand->type == MEMORY_PHI) {
            auto phiBlock = (SSABasicBlock*)(baseMem + operand->block);
            TryRemoveTrivialMemoryPhi(context, phiBlock, operand, mem0, mem1);
        }
    }

    u32* memPhis = (u32*)(baseMem + block->memoryPhis);
    for(u32 i = 0; i < block->memoryPhiCount; i++) {
        if(memPhis[i] == phiPtr) {
            RemoveMemoyPhi(context, block, i);
            return true;
        }
    }
    ASSERT(false);
}
void InsertMemoryEdge(CompilerContext context, u32 prevMem, u32 nextMem) {

    byte *const baseMem = context.compiler->mem;
    SSAMemoryDef *pred = (SSAMemoryDef *)(baseMem + prevMem);
    SSAMemoryDef *succ = (SSAMemoryDef *)(baseMem + nextMem);

    if (prevMem != ~u32(0)) {

        bool exist = false;
        for (u32 i = 0; i < pred->successors.edgeCount; i++) {
            u32 succMem = Mem<u32>(baseMem + pred->successors.edges + i * sizeof(u32));
            exist |= (succMem == nextMem);
        }

        if (!exist) {

            u32 edges = context.compiler->miscAllocatorSSA;
            u32 *ptr = (u32 *)AllocateSSA(context.compiler, (pred->successors.edgeCount + 1) * sizeof(u32));
            memcpy(ptr, (u32 *)(baseMem + pred->successors.edges), pred->successors.edgeCount * sizeof(u32));
            ptr[pred->successors.edgeCount++] = nextMem;
            pred->successors.edges = edges;
        }
    }

    if (nextMem != ~u32(0)) {

        bool exist = false;
        for (u32 i = 0; i < succ->predecessors.edgeCount; i++) {
            u32 predMem = Mem<u32>(baseMem + succ->predecessors.edges + i * sizeof(u32));
            exist |= (predMem == prevMem);
        }

        if (!exist) {

            u32 edges = context.compiler->miscAllocatorSSA;
            u32 *ptr = (u32 *)AllocateSSA(context.compiler, (succ->predecessors.edgeCount + 1) * sizeof(u32));
            memcpy(ptr, (u32 *)(baseMem + succ->predecessors.edges), succ->predecessors.edgeCount * sizeof(u32));
            ptr[succ->predecessors.edgeCount++] = prevMem;
            succ->predecessors.edges = edges;
        }
    }
}

void RemoveMemoryFromBlock(CompilerContext context, u32 memPtr) {
    
    byte *const baseMem = context.compiler->mem;
    SSAMemoryDef *mem = (SSAMemoryDef *)(baseMem + memPtr);

    SSAMemoryDef *successors[mem->successors.edgeCount];
    for(u32 i = 0; i < mem->successors.edgeCount; i++) {
        u32 s = Mem<u32>(baseMem + mem->successors.edges + i * sizeof(u32));
        successors[i] = (SSAMemoryDef*)(baseMem + s);
    }

    auto preds = mem->predecessors;
    auto succs = mem->successors;

    for (u32 i = 0; i < preds.edgeCount; i++) {
        u32 p = Mem<u32>(baseMem + preds.edges + i * sizeof(u32));
        RemoveMemoryEdge(context, p, memPtr);

        for (u32 k = 0; k < mem->successors.edgeCount; k++) {
            u32 s = Mem<u32>(baseMem + mem->successors.edges + k * sizeof(u32));
            InsertMemoryEdge(context, p, s);
        }
    }
    for (u32 i = 0; i < succs.edgeCount; i++) {
        u32 p = Mem<u32>(baseMem + succs.edges + i * sizeof(u32));
        RemoveMemoryEdge(context, memPtr, p);
    }

    auto block = GetBlockFromMemory(baseMem, mem);
    SSAMemoryDef *next = (mem->next == 0 ? nullptr : (SSAMemoryDef*)(baseMem + mem->next));
    SSAMemoryDef *prev = (mem->prev == 0 ? nullptr : (SSAMemoryDef*)(baseMem + mem->prev));
    
    if (next) {
        next->prev = mem->prev;
    } else {
        block->lastMem = (byte*)prev - baseMem;
        if (prev) {
            Mem<SSAMemoryDef>(baseMem + block->lastMem).next = 0;
        }
    }
    if (prev) {
        prev->next = mem->next;
    } else {
        block->firstMem = (byte*)next - baseMem;
        if (next) {
            Mem<SSAMemoryDef>(baseMem + block->firstMem).prev = 0;
        }
    }

}

void RemoveMemory(CompilerContext context, u32 memPtr) {

    byte *const baseMem = context.compiler->mem;
    SSAMemoryDef *mem = (SSAMemoryDef *)(baseMem + memPtr);
    RemoveMemoryFromBlock(context, memPtr);
    pool_free(&context.compiler->memoryDefPool, mem);
}

SSABasicBlock* MergeCF(CompilerContext context, CFGEdges pred, bool jmp) {

    byte *const baseMem = context.compiler->mem;
    if (pred.edgeCount == 1 && jmp == false) {

        auto block = (SSABasicBlock *)(baseMem + Mem<u32>(baseMem + pred.edges));
        return block;
    } else {

        auto merge = (SSABasicBlock*)pool_allocate(&context.compiler->ssaBlockPool);
        u32 mergePtr = (byte*)merge - baseMem;
        *merge = {};
        merge->name = context.compiler->basicBlockVarDefs.PushBack(&context.compiler->localHeap, {nullptr, 0});
        context.fn->maxBlockName++;
        merge->hotness = 0.f;
        merge->nextBlock.opr = JMP;
        
        merge->values.Init();
        merge->memoryPtrs.Init();

        u32 predPtr = Mem<u32>(baseMem + pred.edges);
        SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + predPtr);
        for (u32 i = 0; i < pred.edgeCount; i++) {
            predPtr = Mem<u32>(baseMem + pred.edges + i * sizeof(u32));
            predBlock = (SSABasicBlock *)(baseMem + predPtr);
            if(predBlock->nextBlock.opr == RET) continue;
            
            if(predBlock->nextBlock.opr == BRANCH) {
                if(!predBlock->nextBlock.branch.elseBlock) {
                    predBlock->nextBlock.branch.elseBlock = mergePtr;
                }
                else if(predBlock->nextBlock.branch.thenBlock) {
                    predBlock->nextBlock.branch.thenBlock = mergePtr;
                }
            }
            else {
                predBlock->nextBlock.opr = JMP;
                predBlock->nextBlock.jmp.targetBlock = mergePtr;
            }
            InsertCFGEdge(context, predPtr, mergePtr);
        }

        CopyValues(context, merge);
        return merge;
    }
}



u32 GetOpr(Compiler *compiler, u32 valPtr) {
    SSADefinition *op = (SSADefinition *)(compiler->mem + valPtr);
    return op->opr;
}

bool IsValueConstRecursive(byte* const baseMem, u32 valPtr) {
    if (valPtr == ~u32(0))
        return false;
    SSADefinition *op = (SSADefinition *)(baseMem + valPtr);
    switch (op->opr) {
    case SSA_CONSTANT:
        return true;
    case RANGE_EXPRESSION:
    case SSA_ALLOCA:
    case SSA_FUNCTION:
    case SSA_UN_INIT:
    case SSA_FN_PARAMETER:
    case SSA_MEMORY_LOAD:
    case SSA_MEMORY_STORE:
    case SSA_CALL:
    case SSA_PHI_NODE:
        return false;
    case SSA_COPY:
    case SSA_BITWISE_NEG:
    case SSA_LOGICAL_NEG:
    case SSA_MINUS:
    case SSA_CONVERT:
        return IsValueConstRecursive(baseMem, op->operand0);
    case SSA_POWER:
    case SSA_MAX:
    case SSA_MIN:
    default:
        return IsValueConstRecursive(baseMem, op->operand0) &&
               IsValueConstRecursive(baseMem, op->operand1);
    }
}


bool DoesBlockDominate(const Compiler *compiler, SSABasicBlock *block0, SSABasicBlock *block1, u32 *visited) {

    if (block0 == block1)
        return true;

    byte *const baseMem = compiler->mem;
    for (u32 i = 0; i < block1->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + block1->predecessors.edges + i * sizeof(u32));
        SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + predBlockPtr);

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == predBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        visited[(visited[0]++) + 1] = predBlockPtr;
        if (!DoesBlockDominate(compiler, block0, predBlock, visited))
            return false;
    }

    return block1->successors.edgeCount != 0;
}

bool IsReachable(byte* baseMem, SSABasicBlock *from, u32 to, u32 *visited) {

    u32 fromPtr = (byte*)from - baseMem;
    if(fromPtr == to) return true;
    for (u32 i = 0; i < from->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + from->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succBlockPtr);

        if(succBlockPtr == to) return true;

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == to)
                return true;
            if (visited[k] == succBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        visited[(visited[0]++) + 1] = succBlockPtr;

        if (IsReachable(baseMem, succBlock, to, visited)) {
            return true;
        }
    }

    return false;
}
bool Dominator(byte* baseMem, SSABasicBlock *entry, SSABasicBlock *dominator, SSABasicBlock *block, u32 *visited) {

    if(block == entry) return false;
    if (dominator == block)
        return true;

    for (u32 i = 0; i < entry->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + entry->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succBlockPtr);

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == succBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited) {
            continue;
        }
        visited[++visited[0]] = succBlockPtr;

        visited[visited[0] + 2] = 0;
        if (IsReachable(baseMem, succBlock, (u64)block - (u64)baseMem, visited + visited[0] + 2)) {
            if(succBlock != dominator) {
                if (!Dominator(baseMem, succBlock, dominator, block, visited)) return false;
            }
        }
    }
    return true;
}


u32 GetValueChainDepth(CompilerContext ctx, u32 valuePtr) {

    byte* const baseMem = ctx.compiler->mem;
    SSADefinition *val = (SSADefinition *)(baseMem + valuePtr);
    switch (val->opr) {
    case SSA_CONSTANT:
    case SSA_PHI_NODE:
    case SSA_FUNCTION:
    case SSA_FN_PARAMETER:
    case SSA_UN_INIT:
    case SSA_MEMORY_LOAD:
    case SSA_CALL:
    case SSA_CONVERT:
        return 1;
    default:
        return 1 + Max<u32>(GetValueChainDepth(ctx, val->operand0), GetValueChainDepth(ctx, val->operand1));
    }
}
void ReorderDefs(CompilerContext ctx, u32 valPtr0, u32 valPtr1) {

    ASSERT(valPtr0 != valPtr1);
    byte* const baseMem = ctx.compiler->mem;
    auto val0 = (SSADefinition *)(baseMem + valPtr0);
    auto val1 = (SSADefinition *)(baseMem + valPtr1);

    u32 val0PrevDef = val0->prevDef;
    u32 val1PrevDef = val1->prevDef;

    auto val0Prev = GetPointer<SSADefinition>(baseMem, val0PrevDef);
    auto val1Prev = GetPointer<SSADefinition>(baseMem, val1PrevDef);

    auto block0 = (SSABasicBlock *)(baseMem + val0->block);
    auto block1 = (SSABasicBlock *)(baseMem + val1->block);

    u32 next0 = val0->nextDef;
    u32 next1 = val1->nextDef;

    auto n0 = GetPointer<SSADefinition>(baseMem, next0);
    auto n1 = GetPointer<SSADefinition>(baseMem, next1);

    if(n0 == val1) {
        ASSERT(block0 == block1);
        if(val0Prev) {
            val0Prev->nextDef = valPtr1;
        }
        else {
            block0->firstDef = valPtr1;
        }

        if(n1) {
            n1->prevDef = valPtr0;
        }
        else {
            block0->lastDef = valPtr0;
        }

        val0->nextDef = valPtr1;
        val1->prevDef = valPtr0;
    }
    else if(n1 == val0) {

        ASSERT(block0 == block1);
        if(val1Prev) {
            val1Prev->nextDef = valPtr0;
        }
        else {
            block0->firstDef = valPtr0;
        }

        if(n0) {
            n0->prevDef = valPtr1;
        }
        else {
            block0->lastDef = valPtr1;
        }

        val1->nextDef = valPtr0;
        val0->prevDef = valPtr1;
    }

    RemoveDefFromBlock(ctx, valPtr0);
    RemoveDefFromBlock(ctx, valPtr1);

    if(val0Prev) {
        InsertAfterDef(ctx.compiler, block0, val0PrevDef, valPtr1);
    }
    else if(next0) {
        InsertBeforeDef(ctx.compiler, block0, next0, valPtr1);
    }
    else {
        ASSERT(!block0->firstDef && !block0->lastDef);
        PushBackDefIntoBlock(ctx, block0, valPtr1);
    }

    if(val1Prev) {
        InsertAfterDef(ctx.compiler, block1, val1PrevDef, valPtr0);
    }
    else if(next0) {
        InsertBeforeDef(ctx.compiler, block1, next1, valPtr0);
    }
    else {
        ASSERT(!block1->firstDef && !block1->lastDef);
        PushBackDefIntoBlock(ctx, block1, valPtr0);
    }

    Mem<SSADefinition>(baseMem + block0->lastDef).nextDef = 0;
    Mem<SSADefinition>(baseMem + block1->lastDef).nextDef = 0;
}

u32 LeftRotateDefs(CompilerContext ctx, u32 valPtr) {

    byte* const baseMem = ctx.compiler->mem;
    SSADefinition *val = (SSADefinition *)(baseMem + valPtr);
    u32 right_ptr = val->operand1;
    SSADefinition *rightVal = (SSADefinition *)(baseMem + right_ptr);

    if (val->opr == rightVal->opr && (IsOprCommutative(val->opr))) {
        val->operand1 = rightVal->operand0;
        rightVal->operand0 = valPtr;
        ReorderDefs(ctx, valPtr, right_ptr);
        return right_ptr;
    }
    return valPtr;
}
u32 RightRotateDefs(CompilerContext ctx, u32 valPtr) {

    byte* const baseMem = ctx.compiler->mem;
    auto val = (SSADefinition *)(baseMem + valPtr);
    u32 left_ptr = val->operand0;
    auto leftVal = (SSADefinition *)(baseMem + left_ptr);

    if (val->opr == leftVal->opr && (IsOprCommutative(val->opr))) {
        val->operand0 = leftVal->operand1;
        leftVal->operand1 = valPtr;
        ReorderDefs(ctx, valPtr, left_ptr);
        return left_ptr;
    } else {

    }
    return valPtr;
}

u32 LeftLeftRotate(CompilerContext ctx, u32 valPtr) {
    return RightRotateDefs(ctx, valPtr);
}
u32 RightRightRotate(CompilerContext ctx, u32 valPtr) {
    return LeftRotateDefs(ctx, valPtr);
}
u32 LeftRightRotate(CompilerContext ctx, u32 valPtr) {

    byte* const baseMem = ctx.compiler->mem;
    auto val = (SSADefinition *)(baseMem + valPtr);
    auto tmp = LeftRotateDefs(ctx, val->operand0);
    if (tmp != val->operand0) {

        val->operand0 = tmp;
        return RightRotateDefs(ctx, valPtr);
    }

    SSADefinition *left = (SSADefinition *)(baseMem + val->operand0);
    if (val->opr == left->opr && IsOprCommutative(val->opr)) {

        auto right = val->operand1;
        val->operand1 = left->operand1;
        left->operand1 = right;

        u32 n0 = ((SSADefinition *)(baseMem + val->operand1))->value;
        u32 n1 = ((SSADefinition *)(baseMem + right))->value;

        if (n0 < n1) {
            ReorderDefs(ctx, right, val->operand1);
        }
        return valPtr;
    }
}
u32 RightLeftRotate(CompilerContext ctx, u32 valPtr) {

    byte* const baseMem = ctx.compiler->mem;
    SSADefinition *val = (SSADefinition *)(baseMem + valPtr);

    auto tmp = RightRotateDefs(ctx, val->operand1);
    if (tmp != val->operand0) {
        val->operand0 = tmp;
        return LeftRotateDefs(ctx, valPtr);
    }

    SSADefinition *right = (SSADefinition *)(baseMem + val->operand1);
    if (val->opr == right->opr && IsOprCommutative(val->opr)) {

        auto left = val->operand0;
        val->operand0 = right->operand0;
        right->operand0 = left;

        u32 n0 = ((SSADefinition *)(baseMem + val->operand1))->value;
        u32 n1 = ((SSADefinition *)(baseMem + left))->value;

        if (n0 < n1) {
            ReorderDefs(ctx, left, val->operand0);
        }
        return valPtr;
    }
}

u32 BalanceDefs(CompilerContext ctx, u32 valPtr, bool *change) {

    byte* const baseMem = ctx.compiler->mem;
    SSADefinition *val = (SSADefinition *)(baseMem + valPtr);
    if (!IsDefBinary(val))
        return valPtr;

    i32 leftDepth = GetValueChainDepth(ctx, val->operand0);
    i32 rightDepth = GetValueChainDepth(ctx, val->operand1);

    if (leftDepth > rightDepth + 1) {

        val->operand0 = BalanceDefs(ctx, val->operand0, change);
        SSADefinition *left = (SSADefinition *)(baseMem + val->operand0);

        i32 leftLeftDepth = GetValueChainDepth(ctx, left->operand0);
        i32 leftRightDepth = GetValueChainDepth(ctx, left->operand1);
        if (leftLeftDepth > leftRightDepth) {
            auto tmp = LeftLeftRotate(ctx, valPtr);
            *change = valPtr != tmp;
            return tmp;
        } else {
            auto tmp = LeftRightRotate(ctx, valPtr);
            *change = valPtr != tmp;
            return tmp;
        }
    } else if (rightDepth > leftDepth + 1) {
        val->operand1 = BalanceDefs(ctx, val->operand1, change);
        SSADefinition *right = (SSADefinition *)(baseMem + val->operand1);

        i32 rightLeftDepth = GetValueChainDepth(ctx, right->operand0);
        i32 rightRightDepth = GetValueChainDepth(ctx, right->operand1);
        if (rightLeftDepth > rightRightDepth) {
            auto tmp = RightLeftRotate(ctx, valPtr);
            *change = valPtr != tmp;
            return tmp;
        } else {
            auto tmp = RightRightRotate(ctx, valPtr);
            *change = valPtr != tmp;
            return tmp;
        }
    }
    return valPtr;
}

bool BalanceSSAChains(CompilerContext ctx, SSABasicBlock *currentBlock, bool* visited) {

    byte* const baseMem = ctx.compiler->mem;
    bool ret = false;
    auto it = GetPointer<SSADefinition>(baseMem, currentBlock->firstDef);
    while(it) {

        u32 itPtr = (byte*)it - baseMem;      
        bool c = false;
        u32 balanced = BalanceDefs(ctx, itPtr, &c);
        if(c) {
            ret = true;
            memset(visited+ctx.fn->maxBlockName+1, 0, ctx.fn->maxBlockName);
            RerouteDefs(ctx, ctx.fn->entry, visited+ctx.fn->maxBlockName+1, itPtr, balanced);
        }

        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    return ret | TraverseCFG(ctx, currentBlock, visited, BalanceSSAChains);
}


Value ConvertValue(Value in, TypeName to);
Value EvalConstDef(byte* baseMem, SSADefinition *def, byte *mem) {

    switch (def->opr) {
    case SSA_CONSTANT:
        return Mem<Value>(baseMem + def->operand0);
    case SSA_FUNCTION:
    case SSA_MEMORY_LOAD:
    case SSA_MEMORY_STORE:
    case SSA_CALL:
        ASSERT(false);
        break;
    case SSA_COPY:
        return EvalConstDef(baseMem, (SSADefinition *)(baseMem + def->operand0), mem);
    case SSA_LOGICAL_NEG:
    case SSA_BITWISE_NEG:
    case SSA_MINUS:
        {
            Value primary = EvalConstDef(baseMem, (SSADefinition *)(baseMem + def->operand0), mem);
            auto op0Type = ((SSADefinition*)(baseMem + def->operand0))->type;
            TypeName t = GetLastType(baseMem, op0Type);

            if (def->opr == SSA_LOGICAL_NEG) {
                Mem<bool>(primary.mem) = !Mem<bool>(primary.mem);
            } else {
                switch (t) {
                case TYPE_PRIMARY_INT8:
                    Mem<u64>(primary.mem) = -Mem<i8>(primary.mem);
                    break;
                case TYPE_PRIMARY_INT16:
                    Mem<u64>(primary.mem) = -Mem<i16>(primary.mem);
                    break;
                case TYPE_PRIMARY_INT32:
                    Mem<u64>(primary.mem) = -Mem<i32>(primary.mem);
                    break;
                case TYPE_PRIMARY_INT64:
                    Mem<u64>(primary.mem) = -Mem<i64>(primary.mem);
                    break;
                case TYPE_PRIMARY_UINT8:
                    Mem<u64>(primary.mem) = -Mem<u8>(primary.mem);
                    break;
                case TYPE_PRIMARY_UINT16:
                    Mem<u64>(primary.mem) = -Mem<u16>(primary.mem);
                    break;
                case TYPE_PRIMARY_UINT32:
                    Mem<u64>(primary.mem) = -Mem<u32>(primary.mem);
                    break;
                case TYPE_PRIMARY_UINT64:
                    Mem<u64>(primary.mem) = -Mem<u64>(primary.mem);
                    break;
                }
            }
            return primary;
        }
    case SSA_CONVERT: {
        Value from = EvalConstDef(baseMem, (SSADefinition *)(baseMem + def->operand0), mem);
        TypeName t = GetLastType(baseMem, def->type);
        return ConvertValue(from, t);
    }
    case SSA_MAX:
    case SSA_MIN:
    default:
        {
            Value left = EvalConstDef(baseMem, (SSADefinition *)(baseMem + def->operand0), mem);
            Value right = EvalConstDef(baseMem, (SSADefinition *)(baseMem + def->operand1), mem);
            auto r = EvalOpr(left, right, (TokenType)def->opr, 3);
            switch(def->opr) {
            case SSA_CMP_EQ:
            case SSA_CMP_NOT_EQ:
            case SSA_CMP_LESS_THAN:
            case SSA_CMP_BIGGER_THAN:
            case SSA_CMP_LESS_THAN_OR_EQ:
            case SSA_CMP_BIGGER_THAN_OR_EQ:
            case SSA_LOGICAL_AND:
            case SSA_LOGICAL_OR:
            case SSA_LOGICAL_NEG:
                r.type = (TypeName)TYPE_PRIMARY_BOOL;
                break;
            default:break;
            }
            return r;
        }
    }
}



Value GetValueFromLiteral(Token literal);

void IncIm(ImmediateExpr *im) {
    switch (im->v.type) {
    case TYPE_PRIMARY_INT8:
        Mem<i8>(im->v.mem)++;
        break;
    case TYPE_PRIMARY_INT16:
        Mem<i16>(im->v.mem)++;
        break;
    case TYPE_PRIMARY_INT32:
        Mem<i32>(im->v.mem)++;
        break;
    case TYPE_PRIMARY_INT64:
        Mem<i64>(im->v.mem)++;
        break;
    case TYPE_PRIMARY_UINT8:
        Mem<u8>(im->v.mem)++;
        break;
    case TYPE_PRIMARY_UINT16:
        Mem<u16>(im->v.mem)++;
        break;
    case TYPE_PRIMARY_UINT32:
        Mem<u32>(im->v.mem)++;
        break;
    case TYPE_PRIMARY_UINT64:
        Mem<u64>(im->v.mem)++;
        break;
    case TYPE_PRIMARY_F32:
        Mem<f32>(im->v.mem)++;
        break;
    case TYPE_PRIMARY_F64:
        Mem<f64>(im->v.mem)++;
        break;
    }
}



typedef u32 (*ssa_match_and_replace_t)(CompilerContext context, u32 ssaDef);
u32 MatchReplaceAddIdentity(CompilerContext context, u32 ssaDef) {

    byte* const baseMem = context.compiler->mem;
    SSADefinition* root = (SSADefinition*)(baseMem + ssaDef);
    SSADefinition* op0 = (SSADefinition*)(baseMem + root->operand0);
    SSADefinition* op1 = (SSADefinition*)(baseMem + root->operand1);
    if(root->opr == SSA_ADD) {
        if(op0->opr == SSA_CONSTANT) {
            auto v = (Value*)(baseMem + op0->operand0);
            if(IsImmZero(*v)) {
                return root->operand1;
            }
        }
        if(op1->opr == SSA_CONSTANT) {
            auto v = (Value*)(baseMem + op1->operand0);
            if(IsImmZero(*v)) {
                return root->operand0;
            }
        }
    }
    if(root->opr == SSA_SUB) {
        
        if(op1->opr == SSA_CONSTANT) {
            Value* v = (Value*)(baseMem + op1->operand0);
            if(IsImmZero(*v)) {
                return root->operand0;
            }
        }
        if(op0 == op1) {
            root->opr = SSA_CONSTANT;
            root->operand0 = context.compiler->miscAllocatorSSA;
            Value* v = AllocateSSA<Value>(context.compiler);
            *v = MakeImm(GetLastType(baseMem, root->type), 0);
            return ssaDef;
        }
    }
    return ~u32(0);
}
u32 MatchReplaceMulIdentity(CompilerContext context, u32 ssaDef) {

    byte* const baseMem = context.compiler->mem;
    SSADefinition* root = (SSADefinition*)(baseMem + ssaDef);
    SSADefinition* op1 = (SSADefinition*)(baseMem + root->operand1);
    if(root->opr == SSA_MUL) {
        SSADefinition* op0 = (SSADefinition*)(baseMem + root->operand0);
        if(op0->opr == SSA_CONSTANT) {
            auto v = Mem<Value>(baseMem + op0->operand0);
            if(IsImmOne(v)) {
                return root->operand1;
            }
            if(IsImmZero(v)) {
                return root->operand0;                
            }
        }
        if(op1->opr == SSA_CONSTANT) {
            auto v = Mem<Value>(baseMem + op1->operand0);
            if(IsImmOne(v)) {
                return root->operand0;
            }
            if(IsImmZero(v)) {
                return root->operand1;
            }
        }
    }
    return ~u32(0);
}
u32 MatchReplaceDivIdentity(CompilerContext context, u32 ssaDef) {

    byte* const baseMem = context.compiler->mem;
    SSADefinition* root = (SSADefinition*)(baseMem + ssaDef);
    SSADefinition* op1 = (SSADefinition*)(baseMem + root->operand1);
    if(root->opr == SSA_DIV) {
        SSADefinition* op0 = (SSADefinition*)(baseMem + root->operand0);
        if(op0->opr == SSA_CONSTANT) {
            auto v = (Value*)(baseMem + op0->operand0);
            if(IsImmOne(*v)) {
                return root->operand1;
            }
            if(IsImmZero(*v)) {
                return root->operand0;
            }
        }
        if(op1->opr == SSA_CONSTANT) {
            auto v = (Value*)(baseMem + op0->operand1);
            if(IsImmOne(*v)) {
                return root->operand0;
            }
        }

        if(op0 == op1) {
            root->opr = SSA_CONSTANT;
            root->operand0 = context.compiler->miscAllocatorSSA;
            Value* v = (Value*)AllocateSSA<Value>(context.compiler);
            *v = MakeImm(GetLastType(baseMem, root->type), 1);
            return ssaDef;
        }
    }

    return ~u32(0);
}
bool Simplify(CompilerContext context, SSABasicBlock *block, u32 blockCount, SSADefinition *val, ssa_match_and_replace_t* rewrites, u32 rewriteCount, bool* visitMem) {

    byte* const baseMem = context.compiler->mem;
    u32 valPtr = (u64)val - (u64)baseMem;

    bool ret = false;
    for(auto i = rewrites; i < rewrites+rewriteCount; i++) {
        auto rewrite = *i;
        auto replaced = rewrite(context, valPtr);
        if(replaced != ~u32(0)) {
            memset(visitMem, 0, blockCount);
            ret |= RerouteDefs(context, block, visitMem, valPtr, replaced) != 0;
        }
    }

    return ret;
}

bool IsDefUsed(CompilerContext ctx, SSABasicBlock* block, bool* visited, u32 def) {

    byte* const baseMem = ctx.compiler->mem;

    switch(block->nextBlock.opr) {
    case BRANCH:
        if (block->nextBlock.branch.conditionDef == def) {
            return true;
        }
        break;
    case RET:
        {
            auto retDef = (SSADefinition*)(baseMem + block->nextBlock.ret.retDef);
            if (block->nextBlock.ret.retDef == def) {
                return true;
            }
        }
        break;
    }

    for (u32 i = 0; i < block->phiCount; i++) {
        u32 phiPtr = Mem<u32>(baseMem + block->phis + i * sizeof(u32));
        SSADefinition *phi = (SSADefinition *)(baseMem + phiPtr);
        u32 *ops = GetPhiOperandPtr(baseMem, phi);
        u32 count = GetPhiOperandCount(phi);
        for (u32 k = 0; k < count; k++) {
            if (ops[k] == def) {
                return true;
            }
        }
    }

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {

        if(IsDefBinary(it)) {
            if(it->operand0 == def) {
                return true;
            }
            if(it->operand1 == def) {
                return true;
            }
        }
        if(IsDefUnary(it)) {
            if(it->operand0 == def) {
                return true;
            }
        }
        if(it->opr == EXPRESSION_CALL) {
            auto args = GetCallArgs(baseMem, it);
            for(u32 i = 0; i < args.argCount; i++) {
                if(args.ptr[i] == def) {
                    return true;
                }
            }
        }

        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    return TraverseCFG(ctx, block, visited, IsDefUsed, def);
}

bool RewriteSSAChains(CompilerContext context, SSABasicBlock *currentBlock, bool* visited) {

    ssa_match_and_replace_t rewrites[] = {
        MatchReplaceAddIdentity,
        MatchReplaceMulIdentity,
        MatchReplaceDivIdentity,
    };

    byte* const baseMem = context.compiler->mem;
    bool ret = false;
    auto *it = GetPointer<SSADefinition>(baseMem, currentBlock->firstDef);
    while (it != 0) {

        u32 itPtr = (u64)it - (u64)context.compiler->mem;
        if (!IsValueConst(baseMem, itPtr)) {
            bool res = true;
            while(res) {
                res = Simplify(context, currentBlock, context.fn->maxBlockName, it, rewrites, 3, visited + context.fn->maxBlockName);
                ret |= res;
            }
        }
        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    return ret | TraverseCFG(context, currentBlock, visited, RewriteSSAChains);
}


bool IsDefTerminator(SSADefinition* def) {
    return def->opr == SSA_CONSTANT ||
           def->opr == SSA_UN_INIT ||
           def->opr == SSA_FN_PARAMETER ||
           def->opr == SSA_FUNCTION || 
           def->opr == POSITIVE_INF || 
           def->opr == NEGATIVE_INF;
}
u32 CpySSAChain(byte* from, LinearAllocator* to, u32 defPtr) {

    auto def = (SSADefinition*)(from + defPtr);
    auto cpy = (SSADefinition*)linear_allocate(to, sizeof(SSADefinition));
    *cpy = *def;

    if(IsDefBinary(def)) {
        cpy->type = {to->top};
        to->top = CpyTypeExpr(to->base, {to->top}, from, def->type).index;

        cpy->operand0 = CpySSAChain(from, to, def->operand0);
        cpy->operand1 = CpySSAChain(from, to, def->operand1);
    }
    else if(IsDefUnary(def)) {
        cpy->type = {to->top};
        to->top = CpyTypeExpr(to->base, {to->top}, from, def->type).index;

        cpy->operand0 = CpySSAChain(from, to, def->operand0);
    }
    else if(def->opr == SSA_CONSTANT) {
        auto val = (Value*)(from + def->operand0);
        auto cpyVal = (Value*)linear_allocate(to, sizeof(Value));
        *cpyVal = *val;
        cpy->operand0 = (byte*)cpyVal - to->base;
        cpy->type = {to->top};
        auto t = (TypeName*)linear_allocate(to, sizeof(TypeName)*2);
        t[0] = (TypeName)val->type;
        t[1] = TYPE_NON;
    }
    else if(IsDefTerminator(def)) {
        cpy->type = {to->top};
        to->top = CpyTypeExpr(to->base, {to->top}, from, def->type).index;

    }
    else if(def->opr == SSA_PHI_NODE){
        
        u32 count = GetPhiOperandCount(def);
        u32* operands = (u32*)linear_allocate(to, count*sizeof(u32));

        u32* ptr = GetPhiOperandPtr(from, def);
        for(u32 i = 0; i < count ; i++) {
            operands[i] = CpySSAChain(from, to, ptr[i]);
        }
    }
    return (byte*)cpy - to->base;
}
u32 GetAddressRanges(CompilerContext context, LinearAllocator* stack, u32 addressPtr, u32 size, u32* visitMem) {

    byte* const baseMem = context.compiler->mem;

    visitMem[0] = 0;
    u32 possibleBaseCount = GetBaseAddressFromPointer(context, addressPtr, (u32*)linear_allocator_top(stack), visitMem);
    memcpy(visitMem, (u32*)linear_allocator_top(stack), sizeof(possibleBaseCount));
    u32* bases = visitMem;
    SSADefinition basesSaved[possibleBaseCount];

    auto range = (SymbolicRange*)linear_allocate(stack, sizeof(SymbolicRange));

    auto constZero = (SSADefinition*)linear_allocate(stack, sizeof(SSADefinition));
    *constZero = {};
    constZero->opr = SSA_CONSTANT;
    constZero->operand0 = stack->top;

    auto v = (Value*)linear_allocate(stack, sizeof(Value));
    *v = MakeImm(TYPE_PRIMARY_INT64, 0);

    for(u32 k = 0; k < possibleBaseCount; k++) {
        auto base = (SSADefinition*)(baseMem + bases[k]);
        basesSaved[k] = *base;
        *base = *constZero;
    }

    auto address = CpySSAChain(baseMem, stack, addressPtr);
    for(u32 k = 0; k < possibleBaseCount; k++) {
        auto base = (SSADefinition*)(baseMem + bases[k]);
        *base = basesSaved[k];
    }

    auto constSize = (SSADefinition*)linear_allocate(stack, sizeof(SSADefinition));
    *constSize = {};
    constSize->opr = SSA_CONSTANT;
    constSize->operand0 = stack->top;

    auto size_ = (Value*)linear_allocate(stack, sizeof(Value));
    *size_ = MakeImm(TYPE_PRIMARY_INT64, size);

    auto plus = (SSADefinition*)linear_allocate(stack, sizeof(SSADefinition));
    *plus = {};
    plus->opr = SSA_ADD;
    plus->operand0 = address;
    plus->operand1 = (byte*)constSize - stack->base;
    plus->type = {stack->top};
    stack->top += CpyTypeExpr(stack->base, {stack->top}, baseMem, context.compiler->basicTypes[TYPE_PRIMARY_INT64]).index;

    range->lower = address;
    range->upper = (byte*)plus - stack->base;
    return 1;
}


u32 FindMightAliases(MemoryAliasGraph* graph, u32 name, u32* result) {

    u32 ret = 0;
    for(auto i = graph->vertices[name].aliases; i ; i = i->next) {
        if(i->index != name && i->type == ALIAS_TYPE_MIGHT_ALIAS) {
            result[ret++] = graph->vertices[i->index].basePtr;
        }
    }
    return ret;
}
u32 FindMustAliases(MemoryAliasGraph* graph, u32 name, u32* result) {

    u32 ret = 0;
    for(auto i = graph->vertices[name].aliases; i ; i = i->next) {
        if(i->index != name && i->type == ALIAS_TYPE_MUST_ALIAS) {
            result[ret++] = graph->vertices[i->index].basePtr;
        }
    }
    return ret;
}

void VisitAllPredessesors(byte* baseMem, SSABasicBlock *headBlock, u32 *result) {

    u32 visitedC = result[0];
    u32 headBlockPtr = (u64)headBlock - (u64)baseMem;
    for (u32 i = 1; i < visitedC + 1; i++) {
        if (result[i] == headBlockPtr)
            return;
    }
    result[++result[0]] = headBlockPtr;

    for (u32 i = 0; i < headBlock->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + headBlock->predecessors.edges + i * sizeof(u32));
        SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + predBlockPtr);

        bool blockVisited = false;
        u32 visitedCount = result[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (result[k] == predBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        result[++result[0]] = predBlockPtr;
        VisitAllPredessesors(baseMem, predBlock, result);
    }
}

void VisitAllSuccessors(byte* baseMem, SSABasicBlock *headBlock, u32 *result) {

    u32 visitedC = result[0];
    u32 headBlockPtr = (u64)headBlock - (u64)baseMem;
    for (u32 i = 1; i < visitedC + 1; i++) {
        if (result[i] == headBlockPtr)
            return;
    }
    result[++result[0]] = headBlockPtr;

    for (u32 i = 0; i < headBlock->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + headBlock->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succBlockPtr);

        bool blockVisited = false;
        u32 visitedCount = result[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (result[k] == succBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        VisitAllSuccessors(baseMem, succBlock, result);
    }
}

SSABasicBlock *FindBranchChokePoint(byte* baseMem, SSABasicBlock *headBlock, u32 *visited) {

    u32 headBlockPtr = (u64)headBlock - (u64)baseMem;
    u32 visitedCount = visited[0] + 1;
    for (u32 i = 1; i < visitedCount; i++) {
        if (visited[i] == headBlockPtr) {
            return headBlock;
        }
    }
    visited[++visited[0]] = headBlockPtr;

    for (u32 i = 0; i < headBlock->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + headBlock->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succBlockPtr);

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == succBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            succBlock;
        auto ret = FindBranchChokePoint(baseMem, succBlock, visited);
        if (ret != nullptr)
            return ret;
    }

    return nullptr;
}
u32 PlaceMemoryPhis(CompilerContext context, SSABasicBlock* block, u32 baseAddressDef, u32 baseName, u32* visited) {

    auto memory = SearchMemoryInBlockFromBaseAddressName(block, baseName);
    if(memory != ~u32(0)) return memory;

    byte* const baseMem = context.compiler->mem;
    for(u32 i = 1; i < visited[0]+1; i++) {
        if(visited[i] == ((byte*)block - baseMem)) return ~u32(0);
    }
    visited[++visited[0]] = (byte*)block - baseMem;

    if(block->predecessors.edgeCount == 1) {
        u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges);
        SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
        auto ret = PlaceMemoryPhis(context, predBlock, baseAddressDef, baseName, visited);
        WriteMemoryToBlock(block, ret, baseName);
        return ret;
    }
    else if(block->predecessors.edgeCount != 0) {

        ASSERT(block->predecessors.edgeCount > 1);
        auto memoryPhi = AddMemoryPhi(context, block);
        memoryPhi->basePtrDef = baseAddressDef;
        u32 memoryPhiPtr = (byte*)memoryPhi - baseMem;
        WriteMemoryToBlock(block, memoryPhiPtr, baseName);
        for(u32 i = 0; i < block->predecessors.edgeCount; i++) {
            u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
            SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
            
            visited[0] = 0;
            auto phiOperand = PlaceMemoryPhis(context, predBlock, baseAddressDef, baseName, visited);
            InsertMemoryEdge(context, phiOperand, memoryPhiPtr);
        }
        
        return memoryPhiPtr;
    }
    
    return MakeUnkownMemory(context, baseAddressDef, visited+visited[0]);
}
SymbolicRange GetSymbolicRange(byte* baseMem, u32 value, u32* mem, u32* visitMem) {

    auto def = (SSADefinition*)(baseMem + value);
    if(def->opr == SSA_PHI_NODE) {
    }
    return {value,value};
}
u32 GetSymbolicRanges(byte* baseMem, u32 value, LinearAllocator* stack, SymbolicRange* result, u32* visited) {
    ASSERT(false);
    /*
    u32 ret = 0;
    auto def = (SSADefinition*)(baseMem + value);
    switch (def->opr) {
    case SSA_CONSTANT:
    case SSA_FN_PARAMETER:
    case SSA_UN_INIT:
    case SSA_FUNCTION:
        {
            u32 cpy = CpySSAChain(baseMem, stack, value);
            result->lower = cpy;
            result->upper = cpy;
            return 1;
        }
    case SSA_ALLOCA:ASSERT(false);break;
    case SSA_COPY:
        return GetSymbolicRanges(baseMem, def->operand0, stack, result, visited);
    case SSA_PHI_NODE:
        {
            visited[visited[0]+1] = 0;
            bool cyclic = DoesPhiFormCycle(baseMem, value, visited+visited[0]+1);

            u32* operands = GetPhiOperandPtr(baseMem, def);
            u32 count = GetPhiOperandCount(def);
            if(!cyclic) {
                for(u32 i = 0; i < count; i++) {
                    ret += GetSymbolicRanges(baseMem, operands[i], stack, result+ret, visited);
                }
            }
            else {

            }
            break;
        }
    default:break;
    }*/
}

void CompletePhis(CompilerContext context, SSABasicBlock* loopHeaderBlock, byte* incompletePhiBuffer, u32* visitMem) {

    byte* const baseMem = context.compiler->mem;
    u32 incompletePhiCount = Mem<u32>(incompletePhiBuffer);
    u32 blockCount = context.fn->maxBlockName;
    auto phiBuffer = (IncompletePhi*)(incompletePhiBuffer + sizeof(u32));

    for(u32 i = 0; i < incompletePhiCount; i++) {

        auto phiIndex   = phiBuffer[i].phi;
        auto symbol     = phiBuffer[i].symbol;
        auto symbolType = phiBuffer[i].symbolType;
        auto block      = phiBuffer[i].block;
        if (block != loopHeaderBlock) continue;

        u32 phiPtr = Mem<u32>(baseMem + block->phis + phiIndex * sizeof(u32));
        auto phi = (SSADefinition *)(phiPtr + baseMem);
        AddPhiOperand(context, block, phi, symbol, symbolType, incompletePhiBuffer, visitMem);

        u32 phiCount = Mem<u32>(incompletePhiBuffer);
        u32* mem = (u32*)(incompletePhiBuffer + phiCount * sizeof(IncompletePhi)) + sizeof(u32);
        phiPtr = TryRemoveTrivialPhi(context, block, phi, (bool*)visitMem, mem);

        WriteSymbol(context, block, symbol, phiPtr);
    }
}

u32 PlaceSSAMemoryPhiHelper(CompilerContext context, u32 baseName, u32 baseAddress, SSABasicBlock* ssaDefBlock, u32* visitMem) {

    byte* const baseMem = context.compiler->mem;
    u32 prevMem;
    if(ssaDefBlock->predecessors.edgeCount == 1) {
        u32 predBlockPtr = Mem<u32>(baseMem + ssaDefBlock->predecessors.edges);
        SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
        visitMem[0] = 0;
        prevMem = PlaceMemoryPhis(context, predBlock, baseAddress, baseName, visitMem);
    }
    else if(ssaDefBlock->predecessors.edgeCount == 0) {
        visitMem[0] = 0;
        prevMem = PlaceMemoryPhis(context, ssaDefBlock, baseAddress, baseName, visitMem);
    }
    else {

        auto memoryPhi = AddMemoryPhi(context, ssaDefBlock);
        memoryPhi->basePtrDef = baseAddress;
        prevMem = (byte*)memoryPhi - baseMem;
        for(u32 j = 0; j < ssaDefBlock->predecessors.edgeCount; j++) {

            u32 predBlockPtr = Mem<u32>(baseMem + ssaDefBlock->predecessors.edges + j * sizeof(u32));
            SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
            visitMem[0] = 0;
            auto phiOperand = PlaceMemoryPhis(context, predBlock, baseAddress, baseName, visitMem);
            InsertMemoryEdge(context, phiOperand, prevMem);
        }
    }

    return prevMem;
}

void SSAMemoryIRStorehelper(CompilerContext context, u32 storeDef, u32* mem, u32* visitMem) {

    byte* const baseMem = context.compiler->mem;
    auto store = (SSADefinition*)(baseMem + storeDef);
    ASSERT(store->opr == SSA_MEMORY_STORE);

    visitMem[0] = 0;
    u32 possibleBaseCount = GetBaseAddressFromPointer(context, store->operand0, mem, visitMem);
    SSADefinition localBasesSaved[possibleBaseCount];

    auto constZero = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
    *constZero = {};
    constZero->opr = SSA_CONSTANT;
    constZero->operand0 = context.compiler->miscAllocatorSSA;
    auto v = (Value*)AllocateSSA<Value>(context.compiler);
    *v = MakeImm(TYPE_PRIMARY_INT64, 0);

    // save
    for(u32 k = 0; k < possibleBaseCount; k++) {
        auto base = (SSADefinition*)(baseMem + mem[k]);
        localBasesSaved[k] = *base;
        *base = *constZero;
    }

    LinearAllocator stack = make_linear_allocator(context.compiler->mem , 32*MEGA_BYTE);
    stack.top += context.compiler->miscAllocatorSSA;
    auto address = CpySSAChain(baseMem, &stack, store->operand0);

    // restore
    for(u32 k = 0; k < possibleBaseCount; k++) {
        auto base = (SSADefinition*)(baseMem + mem[k]);
        *base = localBasesSaved[k];
    }
    context.compiler->miscAllocatorSSA = stack.top;

    auto constSize = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
    *constSize = {};
    constSize->opr = SSA_CONSTANT;
    constSize->operand0 = context.compiler->miscAllocatorSSA;
    auto size = AllocateSSA<Value>(context.compiler);
    *size = MakeImm(TYPE_PRIMARY_INT64, GetTypeSize(baseMem, baseMem, store->type) - 1);

    auto plus = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
    *plus = {};
    plus->opr = SSA_ADD;
    plus->operand0 = address;
    plus->operand1 = (byte*)constSize - baseMem;
    plus->type = context.compiler->basicTypes[TYPE_PRIMARY_INT64];
}

void MakeSSAMemoryIR(CompilerContext context, u32* mem, u32* visitMem) {

    ASSERT(false);
    /*
    byte* const baseMem = context.compiler->mem;
    for(u32 i = 0; i < context.compiler->incompleteMemoryOps.size; i++) {
        auto ssaDefPtr = context.compiler->incompleteMemoryOps[i].ssaDef;
        SSADefinition* ssaDef = (SSADefinition*)(baseMem + ssaDefPtr);
        SSABasicBlock* ssaDefBlock = GetBlockFromDef(baseMem, ssaDefPtr);

        switch(ssaDef->opr) {
        case SSA_MEMORY_STORE:
            {
                visitMem[0] = 0;
                u32 possibleBaseCount = GetBaseAddressFromPointer(context, ssaDef->operand0, mem, visitMem);
                SSADefinition basesSaved[possibleBaseCount];

                auto constZero = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
                *constZero = {};
                constZero->opr = SSA_CONSTANT;
                constZero->operand0 = context.compiler->miscAllocatorSSA;
                auto v = (Value*)AllocateSSA<Value>(context.compiler);
                *v = MakeImm(TYPE_PRIMARY_INT64, 0);

                for(u32 k = 0; k < possibleBaseCount; k++) {
                    auto base = (SSADefinition*)(baseMem + mem[k]);
                    basesSaved[k] = *base;
                    *base = *constZero;
                }
                LinearAllocator stack = make_linear_allocator(context.compiler->mem , 32*MEGA_BYTE);
                stack.top += context.compiler->miscAllocatorSSA;

                auto address = CpySSAChain(context.compiler->mem, &stack, ssaDef->operand0);
                for(u32 k = 0; k < possibleBaseCount; k++) {
                    auto base = (SSADefinition*)(baseMem + mem[k]);
                    *base = basesSaved[k];
                }
                context.compiler->miscAllocatorSSA = stack.top;

                auto constSize = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
                *constSize = {};
                constSize->opr = SSA_CONSTANT;
                constSize->operand0 = context.compiler->miscAllocatorSSA;
                auto size = AllocateSSA<Value>(context.compiler);
                *size = MakeImm(TYPE_PRIMARY_INT64, GetTypeSize(baseMem, baseMem, ssaDef->type) - 1);

                auto plus = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
                *plus = {};
                plus->opr = TOKEN_PLUS;
                plus->operand0 = address;
                plus->operand1 = (byte*)constSize - baseMem;
                plus->type = context.compiler->basicTypes[TYPE_PRIMARY_INT64];

                u32 clobberedMemories[possibleBaseCount];
                for(u32 k = 0; k < possibleBaseCount; k++) {

                    SSADefinition* base = (SSADefinition*)(baseMem + mem[k]);
                    u32 baseName = GetBasePtrIndexFromDef(baseMem, base);
                    if(baseName == ~u32(0)) {
                        MakeUnkownMemory(context, mem[k], visitMem);
                        baseName = GetBasePtrIndexFromDef(baseMem, base);
                    }
                    auto prevMem = SearchMemoryInBlockFromBaseAddressName(ssaDefBlock, baseName);
                    SymbolicRange range = GetSymbolicRange(context.compiler->mem, address, nullptr,nullptr);
                    range.upper = (byte*)plus - baseMem;

                    auto freshMem = MakeMemory(context, ssaDefBlock, ssaDefPtr, mem[k], &range, 1);
                    clobberedMemories[k] = freshMem;
                    WriteMemoryToBlock(ssaDefBlock, freshMem, baseName);
                    if(prevMem != ~u32(0)) {
                        InsertMemoryEdge(context, prevMem, freshMem);
                    }
                }


                ssaDef->extraPtr = context.compiler->miscAllocatorSSA;
                SSAStoreInfo* info = (SSAStoreInfo*)AllocateSSA(context.compiler, sizeof(SSAStoreInfo) + possibleBaseCount * sizeof(u32));
                info->infoType = STORE_INFO;
                info->memoryDefCount = possibleBaseCount;
                for(u32 k = 0; k < possibleBaseCount; k++) {
                    info->memoryDefs[k] = clobberedMemories[k];
                }
                break;
            }
        case EXPRESSION_MEMORY_LOAD:
            {
                visitMem[0] = 0;
                u32 possibleBaseCount = GetBaseAddressFromPointer(context, ssaDef->operand0, mem, visitMem);
                ssaDef->extraPtr = context.compiler->miscAllocatorSSA;
                SSALoadInfo* info = (SSALoadInfo*)AllocateSSA(context.compiler, sizeof(SSALoadInfo) + possibleBaseCount * sizeof(u32));
                info->infoType = LOAD_INFO;
                info->memoryUseCount = possibleBaseCount;
                for(u32 k = 0; k < possibleBaseCount; k++) {
                    SSADefinition* base = (SSADefinition*)(baseMem + mem[k]);
                    u32 baseName = GetBasePtrIndexFromDef(baseMem, base);
                    if(baseName == ~u32(0)) {

                        info->memoryUses[k] = MakeUnkownMemory(context, mem[k], visitMem);
                        baseName = GetBasePtrIndexFromDef(baseMem, base);
                    }
                    else {
                        info->memoryUses[k] = ~u32(0);
                    }
                }
                break;
            }
        }
    }

    for(u32 i = 0; i < context.compiler->incompleteMemoryOps.size; i++) {
        auto ssaDefPtr = context.compiler->incompleteMemoryOps[i].ssaDef;
        SSADefinition* ssaDef = (SSADefinition*)(baseMem + ssaDefPtr);

        switch (ssaDef->opr) {
        case SSA_MEMORY_STORE:
            {
                SSABasicBlock* ssaDefBlock = GetBlockFromDef(baseMem, ssaDefPtr);
                visitMem[0] = 0;
                u32 possibleBaseCount = GetBaseAddressFromPointer(context, ssaDef->operand0, mem, visitMem);
                for(u32 k = 0; k < possibleBaseCount; k++) {

                    SSADefinition* base = (SSADefinition*)(baseMem + mem[k]);
                    u32 baseName = GetBasePtrIndexFromDef(baseMem, base);

                    u32 prevMem = PlaceSSAMemoryPhiHelper(context, baseName, mem[k], ssaDefBlock, visitMem);
                    SSAStoreInfo* info = (SSAStoreInfo*)(baseMem + ssaDef->extraPtr);
                    for(u32 j = 0; j < info->memoryDefCount; j++) {
                        SSAMemoryDef* memoryDef = (SSAMemoryDef*)(baseMem + info->memoryDefs[j]);
                        if(memoryDef->basePtrDef == mem[k]) {
                            if(!memoryDef->predecessors.edgeCount) {
                                InsertMemoryEdge(context, prevMem, info->memoryDefs[j]);
                                break;
                            }
                        }
                    }
                }
                break;
            }
        case SSA_MEMORY_LOAD:
            {
                SSABasicBlock* ssaDefBlock = GetBlockFromDef(baseMem, ssaDefPtr);
                visitMem[0] = 0;
                u32 possibleBaseCount = GetBaseAddressFromPointer(context, ssaDef->operand0, mem, visitMem);
                SSALoadInfo* info = (SSALoadInfo*)(baseMem + ssaDef->extraPtr);
                for(u32 k = 0; k < possibleBaseCount; k++) {
                    SSADefinition* base = (SSADefinition*)(baseMem + mem[k]);
                    u32 baseName = GetBasePtrIndexFromDef(baseMem, base);
                    if(info->memoryUses[k] == ~u32(0)) {
                        info->memoryUses[k] = PlaceSSAMemoryPhiHelper(context, baseName, mem[k], ssaDefBlock, visitMem);
                    }
                }
                break;
            }
        }
    }

    context.compiler->incompleteMemoryOps.Clear();
    */
}
u32 GetReturnBlocksFromFunction(byte* baseMem, SSABasicBlock* entry, bool* visited, u32* result) {

    u32 ret = 0;
    if(entry->successors.edgeCount == 0 || entry->nextBlock.opr == RET) {
        result[ret++] = (byte*)entry - baseMem;
    }

    for (u32 i = 0; i < entry->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + entry->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succBlockPtr);

        if(visited[succBlock->name] == true) continue;
        visited[succBlock->name] = true;   
        ret += GetReturnBlocksFromFunction(baseMem, succBlock, visited, result+ret);
    }
    return ret;
}

SSAEnums ssaOperations[] = {};
CFGEdges CreateCDFG(CompilerContext context, Stmt *program, CFGEdges pred, bool jmp, byte *mem, u32* visitMem) {

    ASSERT(mem != nullptr);
    ASSERT(program != nullptr);
    ASSERT(context.compiler != nullptr);
    byte* const baseMem = context.compiler->mem;

    StatementType t = Mem<StatementType>(baseMem + program->index);
    switch (t) {
    case STATEMENT_NON:
        return pred;
    case STATEMENT_FUNCTION_EXIT:
        {
            program->index += sizeof(StatementType);
            auto merge = MergeCF(context, pred, true);
            merge->nextBlock.opr = RET;
            merge->nextBlock.ret.retDef = ~u32(0);
            CFGEdges edges;
            edges.edgeCount = 1;
            edges.edges = context.compiler->miscAllocatorSSA;
            Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = (byte *)merge - baseMem;
            return edges;
        }
    case STATEMENT_ENTRY_POINT: {
        program->index += sizeof(Stmt);
        CFGEdges edge = CreateCDFG(context, program, pred, jmp, mem, visitMem);
        ASSERT(false);
        return edge;
    }
    case STATEMENT_FUNCTION_ENTRY: {

        auto stmt = (FunctionStmt*)(baseMem + program->index);
        auto funcExpr = (FunctionExpr*)(baseMem + stmt->functionPtr);
        program->index += sizeof(FunctionStmt);

        context.compiler->currentFunction = (byte*)context.fn - baseMem;
        *context.fn = {};
        context.fn->name = funcExpr->name;
        context.fn->funcExpr = {stmt->functionPtr};

        u32 index = FindSymbol(&context.compiler->symbolTable, funcExpr->name);
        ASSERT(index != ~u32(0));
        context.compiler->symbolTable[index].extra = context.compiler->currentFunction;

        auto entry = (SSABasicBlock*)pool_allocate(&context.compiler->ssaBlockPool);
        u32 entryPtr = (byte*)entry - baseMem;

        *entry = {};
        entry->hotness = 0.f;
        entry->name = context.compiler->basicBlockVarDefs.PushBack(&context.compiler->localHeap, {nullptr, 0});
        context.fn->maxBlockName++;
        entry->values.Init();

        entry->memoryPtrs.Init();
        entry->predecessors = pred;
        context.fn->params.Init(funcExpr->paramCount);
        context.fn->entry = entry;

        for (u32 i = 0; i < funcExpr->paramCount; i++) {
            u32 paramPtr = Mem<u32>(baseMem + funcExpr->params);

            Variable *param = (Variable *)(baseMem + funcExpr->params + i * sizeof(Variable));

            auto paramVal = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
            u32 paramValPtr = (byte*)paramVal - baseMem;

            *paramVal = {};
            paramVal->opr = SSA_FN_PARAMETER;
            paramVal->type = param->type;
            MakeDefWithSideEffects(context, entry, paramValPtr);
            DefineSymbolInBlock(context, entry, param->name, paramValPtr);
            context.fn->params[i].ssaDef = paramValPtr;
            context.fn->params[i].clobbered = {};
            context.fn->params[i].observed = {};
        }

        CFGEdges entryPred;
        entryPred.edgeCount = 1;
        entryPred.edges = context.compiler->miscAllocatorSSA;
        Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = entryPtr;

        CreateCDFG(context, program, entryPred, jmp, mem, visitMem);
        CFGEdges retBlocks;
        memset(visitMem, 0, context.compiler->basicBlockVarDefs.size);
        retBlocks.edgeCount = GetReturnBlocksFromFunction(baseMem, entry, (bool*)visitMem, (u32*)mem);
        
        retBlocks.edges = context.compiler->miscAllocatorSSA;
        u32* tmp = (u32*)AllocateSSA(context.compiler, retBlocks.edgeCount*sizeof(u32));
        memcpy(tmp, mem, retBlocks.edgeCount*sizeof(u32));
        context.fn->outBlocks = retBlocks;
        return retBlocks;
    }
    case STATEMENT_RET_ASSIGN:
        {
            auto stmt = (RetAssignStmt*)(baseMem + program->index);
            program->index += sizeof(RetAssignStmt);
            auto exit = MergeCF(context, pred, jmp);
            exit->nextBlock.opr = RET;
            if(stmt->retExpr.index != 0) {
                exit->nextBlock.ret.retDef = SSAExpr(context, exit, stmt->retExpr, mem, visitMem);
            }
            else {
                exit->nextBlock.ret.retDef = ~u32(0);
            }
            auto save = exit->nextBlock;

            CFGEdges edges;
            edges.edgeCount = 1;
            edges.edges = context.compiler->miscAllocatorSSA;
            Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = (byte *)exit - baseMem;
            CreateCDFG(context, program, edges, jmp, mem, visitMem);
            exit->successors.edgeCount = 0;
            return edges;
        }
    case STATEMENT_EXPRESSION: {

        auto merge = MergeCF(context, pred, jmp);
        CFGEdges edges;
        edges.edgeCount = 1;
        edges.edges = context.compiler->miscAllocatorSSA;
        Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = (byte *)merge - baseMem;

        auto stmt = (ExprStmt*)(baseMem + program->index);
        SSAExpr(context, merge, stmt->expr, mem, visitMem);

        program->index += sizeof(ExprStmt);
        return CreateCDFG(context, program, edges, false, mem, visitMem);
    }
    case STATEMENT_REPEAT: {

        auto merge = MergeCF(context, pred, jmp);

        CFGEdges edges;
        edges.edgeCount = 1;
        edges.edges = context.compiler->miscAllocatorSSA;
        Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = (byte *)merge - baseMem;
        program->index += sizeof(Stmt);
        return CreateCDFG(context, program, edges, false, mem, visitMem);
    }
    case STATEMENT_ASSUME:
    {
        auto merge = MergeCF(context, pred, jmp);

        CFGEdges edges;
        edges.edgeCount = 1;
        edges.edges = context.compiler->miscAllocatorSSA;
        Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = (byte*)merge - baseMem;

        auto stmt = (AssumeStmt*)(baseMem + program->index);
        program->index += sizeof(AssumeStmt) + stmt->exprCount * sizeof(Expr);

        auto localCpy = merge->values;
        merge->values.mem = (HashNode<u64, u32>*)LOG(global_malloc_debug(localCpy.cap*sizeof(HashNode<u64, u32>)));
        memcpy(merge->values.mem, localCpy.mem, localCpy.cap*sizeof(HashNode<u64, u32>));

        auto firstDef = merge->firstDef;
        auto lastDef = merge->lastDef;
        auto firstMem = merge->firstMem;
        auto lastMem = merge->lastMem;

        auto constraint = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 constraintPtr = (byte*)constraint - baseMem;

        *constraint = {};
        constraint->opr = CONSTRAINT;
        constraint->operand0 = stmt->exprCount;
        constraint->operand1 = context.compiler->miscAllocatorSSA;
        u32* constraints = (u32*)AllocateSSA(context.compiler, stmt->exprCount * sizeof(u32));

        bool empty = true;
        for(u32 i = 0; i < stmt->exprCount; i++) {

            auto e_t = Mem<ExprType>(baseMem + stmt->exprString[i].index);
            if(e_t == EXPRESSION_MISC_HOT || e_t == EXPRESSION_MISC_COLD) {
                merge->hotness = (e_t == EXPRESSION_MISC_HOT) ? 2.f : 0.5f;
                continue;
            }
            empty = false;
            constraints[i] = SSAExpr(context, merge, stmt->exprString[i], mem, visitMem);
        }

        
        LOG(global_free_debug(merge->values.mem));
        merge->values = localCpy;
        merge->firstDef = firstDef;
        merge->lastDef = lastDef;
        merge->firstMem = firstMem;
        merge->lastMem = lastMem;

        if(empty) {
            pool_free(&context.compiler->ssaDefPool, constraint);
        }
        else {
            MakeDefWithSideEffects(context, merge, constraintPtr);
        }
        return CreateCDFG(context, program, edges, false, mem, visitMem);
    }
    case STATEMENT_PRINT: {
        auto merge = MergeCF(context, pred, jmp);

        CFGEdges edges;
        edges.edgeCount = 1;
        edges.edges = context.compiler->miscAllocatorSSA;
        Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = (byte*)merge - baseMem;

        auto stmt = (PrintStmt*)(baseMem + program->index);
        program->index += sizeof(PrintStmt) + stmt->exprCount * sizeof(Expr);
        for(u32 i = 0; i < stmt->exprCount; i++) {
            SSAExpr(context, merge, stmt->exprString[i], mem, visitMem);
        }
        return CreateCDFG(context, program, edges, false, mem, visitMem);
    }

    case STATEMENT_BRANCH: {

        auto stmt = (BranchStmt*)(baseMem + program->index);

        CFGEdges thenPred{};
        CFGEdges elsePred{};
        CFGEdges endPred{};

        SSABasicBlock *thenBlock{};
        SSABasicBlock *condBlock{};
        u32 thenBlockPtr{};
        u32 condBlockPtr{};

        condBlock = MergeCF(context, pred, jmp);
        condBlockPtr = (byte*)condBlock - baseMem;
        condBlock->nextBlock.opr = BRANCH;
        condBlock->nextBlock.branch.conditionDef = SSAExpr(context, condBlock, stmt->cond, mem, visitMem);

        thenBlock = (SSABasicBlock*)pool_allocate(&context.compiler->ssaBlockPool);
        thenBlockPtr = (byte*)thenBlock - baseMem;

        condBlock->nextBlock.branch.thenBlock = thenBlockPtr;
        condBlock->nextBlock.branch.elseBlock = 0;

        *thenBlock = {};
        thenBlock->hotness = 0.f;
        thenBlock->name = context.compiler->basicBlockVarDefs.PushBack(&context.compiler->localHeap, {nullptr, 0});
        context.fn->maxBlockName++;
        thenBlock->values.CopyInit(&condBlock->values);

        thenBlock->memoryPtrs.CopyInit(&condBlock->memoryPtrs);

        thenBlock->nextBlock.opr = JMP;
        InsertCFGEdge(context, condBlockPtr, thenBlockPtr);

        thenPred.edgeCount = 1;
        thenPred.edges = context.compiler->miscAllocatorSSA;
        Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = thenBlockPtr;

        *program = stmt->thenBranch;
        endPred = CreateCDFG(context, program, thenPred, false, mem, visitMem);

        if (stmt->elseBranch.index != 0) {

            auto elseBlock = (SSABasicBlock*)pool_allocate(&context.compiler->ssaBlockPool);
            u32 elseBlockPtr = (byte*)elseBlock - baseMem;

            condBlock->nextBlock.branch.elseBlock = elseBlockPtr;

            *elseBlock = {};
            elseBlock->hotness = 0.f;
            elseBlock->name = context.compiler->basicBlockVarDefs.PushBack(&context.compiler->localHeap, {nullptr, 0});
            context.fn->maxBlockName++;
            elseBlock->nextBlock.opr = JMP;
            elseBlock->values.CopyInit(&condBlock->values);
            elseBlock->memoryPtrs.CopyInit(&condBlock->memoryPtrs);

            InsertCFGEdge(context, condBlockPtr, elseBlockPtr);

            elsePred.edges = context.compiler->miscAllocatorSSA;
            Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = elseBlockPtr;
            elsePred.edgeCount = 1;

            *program = stmt->elseBranch;
            elsePred = CreateCDFG(context, program, elsePred, false, mem, visitMem);

            u32 tmpPtr = context.compiler->miscAllocatorSSA;
            u32 *tmp = (u32 *)AllocateSSA(context.compiler, sizeof(u32) * (endPred.edgeCount + elsePred.edgeCount));

            memcpy(tmp, baseMem + endPred.edges, sizeof(u32) * endPred.edgeCount);
            memcpy(tmp + endPred.edgeCount, baseMem + elsePred.edges, sizeof(u32) * elsePred.edgeCount);

            endPred.edgeCount = endPred.edgeCount + elsePred.edgeCount;
            endPred.edges = tmpPtr;
        } else {

            u32 tmpPtr = context.compiler->miscAllocatorSSA;
            u32 *tmp = (u32 *)AllocateSSA(context.compiler, sizeof(u32) * (endPred.edgeCount + 1));
            tmp[0] = condBlockPtr;

            memcpy(tmp + 1, baseMem + endPred.edges, sizeof(u32) * endPred.edgeCount);
            endPred.edgeCount++;

            endPred.edges = tmpPtr;
        }

        *program = stmt->end;
        auto ret = CreateCDFG(context, program, endPred, false, mem, visitMem);

        return ret;
    }
    case STATEMENT_FOR_LOOP: {

        auto stmt = (ForStmt*)(baseMem + program->index);
        
        u32 condBlockPtr;
        SSABasicBlock *condBlock;
        SSABasicBlock *initBlock;
        CFGEdges initEdges;

        {
            initBlock = MergeCF(context, pred, jmp);
            u32 initBlockPtr = (byte*)initBlock - baseMem;
            initBlock->nextBlock.opr = JMP;
            CFGEdges initPred;
            initPred.edgeCount = 1;
            initPred.edges = context.compiler->miscAllocatorSSA;
            Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = initBlockPtr;

            *program = stmt->init;
            initEdges = CreateCDFG(context, program, initPred, false, mem, visitMem);
        }
        condBlock = MergeCF(context, initEdges, true);
        condBlockPtr = (byte*)condBlock - baseMem;
        condBlock->incomplete = true;
        condBlock->nextBlock.opr = BRANCH;
        condBlock->nextBlock.branch.elseBlock = 0;
        condBlock->nextBlock.branch.conditionDef = SSAExpr(context, condBlock, stmt->cond, mem, visitMem);

        auto bodyBlock = (SSABasicBlock*)pool_allocate(&context.compiler->ssaBlockPool);
        const u32 bodyBlockPtr = (byte*)bodyBlock - baseMem;

        condBlock->nextBlock.branch.thenBlock = bodyBlockPtr;
        *bodyBlock = {};
        bodyBlock->hotness = 0.f;
        bodyBlock->name = context.compiler->basicBlockVarDefs.PushBack(&context.compiler->localHeap, {nullptr, 0});
        context.fn->maxBlockName++;
        bodyBlock->values.CopyInit(&condBlock->values);
        bodyBlock->memoryPtrs.CopyInit(&condBlock->memoryPtrs);
        bodyBlock->nextBlock.opr = JMP;
        InsertCFGEdge(context, condBlockPtr, bodyBlockPtr);

        CFGEdges bodyPred;
        bodyPred.edges = context.compiler->miscAllocatorSSA;
        Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = bodyBlockPtr;
        bodyPred.edgeCount = 1;

        *program = stmt->body;
        CFGEdges incPred = CreateCDFG(context, program, bodyPred, false, mem, visitMem);

        bool jmp = false;
        if (incPred.edgeCount == 1) {

            u32 incPredBlockPtr = Mem<u32>(baseMem + incPred.edges);
            SSABasicBlock *incPredBlock = (SSABasicBlock *)(baseMem + incPredBlockPtr);
            if (incPredBlock != condBlock && incPredBlock->nextBlock.opr == BRANCH) {
                jmp = true;
            }
        }

        *program = stmt->inc;
        CFGEdges condPred = CreateCDFG(context, program, incPred, jmp, mem, visitMem);

        condBlock->incomplete = false;
        for (u32 i = 0; i < condPred.edgeCount; i++) {
            u32 condPredPtr = Mem<u32>(baseMem + condPred.edges + i * sizeof(u32));
            SSABasicBlock* condPredBlock = (SSABasicBlock*)(baseMem + condPredPtr);

            if(condPredBlock->nextBlock.opr == RET) continue;
            if(condPredBlock->nextBlock.opr == JMP) {
                condPredBlock->nextBlock.jmp.targetBlock = condBlockPtr;
            }
            InsertCFGEdge(context, Mem<u32>(baseMem + condPred.edges + i * sizeof(u32)), condBlockPtr);
        }

        CompletePhis(context, condBlock, mem, (u32*)visitMem);

        CFGEdges endPred;
        endPred.edgeCount = 1;
        endPred.edges = context.compiler->miscAllocatorSSA;
        Mem<u32>(AllocateSSA(context.compiler, sizeof(u32))) = condBlockPtr;

        *program = stmt->end;
        CFGEdges endP = CreateCDFG(context, program, endPred, true, mem, visitMem);

        return endP;
    }

    case STATEMENT_ABORT: {
        ASSERT(false);
        break;
    }
    case STATEMENT_SKIP: {
        ASSERT(false);
        break;
    }
    }
}


void InlineBranch(CompilerContext context, bool takenPath, u32 blockCount, BranchAnalysis branch, u32 *mem) {

    byte *const baseMem = context.compiler->mem;

    SSABasicBlock *const condBlock = branch.condBlock;
    SSABasicBlock *const thenBlock = branch.thenBlock;
    SSABasicBlock *const elseBlock = branch.elseBlock;
    ASSERT(condBlock->nextBlock.branch.elseBlock);

    const u32 thenblockPtr = condBlock->nextBlock.branch.thenBlock;
    const u32 elseblockPtr = condBlock->nextBlock.branch.elseBlock;
    const u32 condBlockPtr = (u64)condBlock - (u64)baseMem;

    mem[0] = 1;
    mem[1] = condBlockPtr;
    VisitAllSuccessors(baseMem, thenBlock, mem);
    SSABasicBlock *const exitBlock = FindBranchChokePoint(baseMem, elseBlock, mem);
    const u32 endBlockPtr = (u64)exitBlock - (u64)baseMem;
    ASSERT(exitBlock);

    mem[0] = 2;
    mem[1] = condBlockPtr;
    mem[2] = endBlockPtr;
    VisitAllSuccessors(baseMem, thenBlock, mem);

    u32 thenBlockCount = mem[0] - 2;
    u32 thenBlocks[thenBlockCount]{};
    memcpy(thenBlocks, mem + 3, thenBlockCount * 4);

    mem[0] = 2;
    mem[1] = condBlockPtr;
    mem[2] = endBlockPtr;
    VisitAllSuccessors(baseMem, elseBlock, mem);

    u32 elseBlockCount;
    if (endBlockPtr != elseblockPtr) {
        elseBlockCount = mem[0] - 2;
    } else {
        elseBlockCount = mem[0] - 1;
    }
    u32 elseBlocks[elseBlockCount]{};
    memcpy(elseBlocks, mem + 2 + (endBlockPtr != elseblockPtr), elseBlockCount * 4);

    for (u32 i = 0; i < exitBlock->phiCount; i++) {
        u32 phiPtr = Mem<u32>(baseMem + exitBlock->phis + i * sizeof(u32));
        SSADefinition *phi = (SSADefinition *)(baseMem + phiPtr);

        u32 *operands = GetPhiOperandPtr(baseMem, phi);
        for (u32 k = 0; k < phi->operand0; k++) {

            SSADefinition *operand = (SSADefinition *)(baseMem + operands[k]);
            SSABasicBlock *operandBlock = (SSABasicBlock *)(baseMem + operand->block);

            mem[0] = phi->operand0;
            u32 index = 0;
            for (u32 j = 0; j < phi->operand0; j++) {
                if (j == k)
                    continue;
                auto bl = ((SSADefinition *)(baseMem + operands[j]))->block;
                mem[++index] = bl;
            }
            mem[phi->operand0] = endBlockPtr;
            VisitAllSuccessors(baseMem, operandBlock, mem);

            u32 size = mem[0];
            bool branch = takenPath;
            bool found = false;
            for (u32 j = phi->operand0; j < size; j++) {
                for (u32 z = 0; z < thenBlockCount; z++) {
                    if (mem[j + 1] == thenBlocks[z]) {
                        branch = true;
                        found = true;
                        break;
                    }
                }
                if(found) break;
                for (u32 z = 0; z < elseBlockCount; z++) {
                    if (mem[j + 1] == elseBlocks[z]) {
                        branch = false;
                        found = true;
                        break;
                    }
                }
                if(found) break;
            }
            if (branch != takenPath) {
                operands[k--] = operands[--phi->operand0];
            }
        }

        ASSERT(phi->operand0 != 0);
        if (phi->operand0 == 1) {
            memset(mem, 0, blockCount),
            RerouteDefs(context, exitBlock, (bool*)mem, phiPtr, operands[0]);
            Mem<u32>(baseMem + exitBlock->phis + (i) * sizeof(u32)) = Mem<u32>(baseMem + exitBlock->phis + (--exitBlock->phiCount) * sizeof(u32));
            i--;
        }
    }

    if (takenPath) {
        u32 elseB = condBlock->nextBlock.branch.thenBlock;
        RemoveCFGEdge(context, condBlockPtr, elseblockPtr);
        for (u32 i = 0; i < elseBlockCount; i++) {
            for (u32 k = 0; k < exitBlock->predecessors.edgeCount; k++) {
                u32 predBlockPtr = Mem<u32>(baseMem + exitBlock->predecessors.edges + k * sizeof(u32));
                if (predBlockPtr == elseBlocks[i]) {
                    RemoveCFGEdge(context, predBlockPtr, endBlockPtr);
                    break;
                }
            }
        }
        condBlock->nextBlock.jmp.targetBlock = elseB;
        condBlock->nextBlock.opr = JMP;
    } else {
        u32 elseB = condBlock->nextBlock.branch.elseBlock;
        RemoveCFGEdge(context, condBlockPtr, thenblockPtr);
        for (u32 i = 0; i < thenBlockCount; i++) {
            for (u32 k = 0; k < exitBlock->predecessors.edgeCount; k++) {
                u32 predBlockPtr = Mem<u32>(baseMem + exitBlock->predecessors.edges + k * sizeof(u32));
                if (predBlockPtr == thenBlocks[i]) {
                    RemoveCFGEdge(context, predBlockPtr, endBlockPtr);
                    break;
                }
            }
        }
        condBlock->nextBlock.jmp.targetBlock = elseB;
        condBlock->nextBlock.opr = JMP;
    }
}

bool PropogateBranchValue(CompilerContext context, u32 blockCount, BranchAnalysis branch, u32 *mem) {

    bool ret = false;
    byte *const baseMem = context.compiler->mem;
    u32 condVal = branch.condBlock->nextBlock.branch.conditionDef;

    auto trueValDef = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
    u32 trueValPtr = (byte*)trueValDef - baseMem;

    *trueValDef = {};
    trueValDef->opr = SSA_CONSTANT;
    trueValDef->operand0 = context.compiler->miscAllocatorSSA;

    u32 save = context.compiler->miscAllocatorSSA;
    Value *vTrue = AllocateSSA<Value>(context.compiler);
    *vTrue = MakeImm(TYPE_PRIMARY_INT64, true);
    vTrue->type = TYPE_PRIMARY_BOOL;
    trueValPtr = MakeDef(context, branch.thenBlock, trueValPtr);
    auto d = (SSADefinition*)(baseMem + trueValPtr);

    memset(mem, 0, blockCount);
    Mem<bool>(mem+branch.exitBlock->name) = true;
    ret |= (RerouteDefs(context, branch.thenBlock, (bool*)mem, condVal, trueValPtr) != 0);
    if (!ret) {
        RemoveDef(context, trueValPtr);
        context.compiler->miscAllocatorSSA = save;
    }
    if (branch.elseBlock != branch.exitBlock) {

        auto falseValDef = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
        u32 falseValPtr = (byte*)falseValDef - baseMem;

        *falseValDef = {};
        falseValDef->opr = SSA_CONSTANT;
        falseValDef->operand0 = context.compiler->exprAllocator;

        save = context.compiler->miscAllocatorSSA;
        Value *vFalse = AllocateSSA<Value>(context.compiler);
        *vFalse = MakeImm(TYPE_PRIMARY_INT64, false);
        vFalse->type = TYPE_PRIMARY_BOOL;
        falseValPtr = MakeDef(context, branch.elseBlock, falseValPtr);

        memset(mem, 0, blockCount);
        Mem<bool>(mem+branch.exitBlock->name) = true;
        bool ret2 = RerouteDefs(context, branch.elseBlock, (bool*)mem, condVal, falseValPtr) != 0;
        if (!ret2) {
            RemoveDef(context, falseValPtr);
            context.compiler->miscAllocatorSSA = save;
        }

        ret |= ret2;
    }

    return ret;
}

bool InlineConstBranches(CompilerContext context, BranchAnalysis branch, u32 *mem) {

    bool ret = false;
    byte * const baseMem = context.compiler->mem;

    u32 cond = branch.condBlock->nextBlock.branch.conditionDef;
    if (IsValueConstRecursive(baseMem, cond)) {
        ret = true;
        SSADefinition *condDef = (SSADefinition *)(baseMem + cond);
        Value condValue = EvalConstDef(baseMem, condDef, (byte*)(mem));
        bool taken = Mem<bool>(condValue.mem);
        InlineBranch(context, taken, context.fn->maxBlockName, branch, mem);
    }

    return ret;
}

bool IsCFCyclicPred(byte* baseMem, SSABasicBlock *root, SSABasicBlock *current) {

    for (u32 i = 0; i < current->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + current->predecessors.edges + i * sizeof(u32));
        SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + predBlockPtr);

        if (predBlock == root)
            return true;
        return IsCFCyclicPred(baseMem, root, predBlock);
    }
    return false;
}
bool IsCFCyclicSucc(Compiler *compiler, SSABasicBlock *root, SSABasicBlock *current) {

    byte *const baseMem = compiler->mem;
    for (u32 i = 0; i < current->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + current->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succBlockPtr);

        if (succBlock == root)
            return true;
        return IsCFCyclicSucc(compiler, root, succBlock);
    }
    return false;
}

bool IsCycilc(CompilerContext context, SSABasicBlock *currentBlock, bool *visited, u32 root) {

    byte *const baseMem = context.compiler->mem;
    auto name = Mem<SSABasicBlock>(baseMem + root).name;
    if(visited[name]) return true;

    for (u32 i = 0; i < currentBlock->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + currentBlock->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succBlockPtr);
        TraverseCFG(context, succBlock, visited, IsCycilc, root);
    }
    return false;
}

SSABasicBlock *FindBranchChokePointPred(byte* baseMem, SSABasicBlock *headBlock, u32 *visited) {

    u32 headBlockPtr = (u64)headBlock - (u64)baseMem;
    u32 visitedCount = visited[0] + 1;
    for (u32 i = 1; i < visitedCount; i++) {
        if (visited[i] == headBlockPtr) {
            return headBlock;
        }
    }
    visited[++visited[0]] = headBlockPtr;

    for (u32 i = 0; i < headBlock->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + headBlock->predecessors.edges + i * sizeof(u32));
        SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + predBlockPtr);

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == predBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            predBlock;
        auto ret = FindBranchChokePointPred(baseMem, predBlock, visited);
        if (ret != nullptr)
            return ret;
    }

    return nullptr;
}

void DFSCFGWalk(Compiler* compiler, SSABasicBlock* block, u32* lowLinkBuffer, bool* visited, u32* seenStack, u32* blockPtrs) {

    blockPtrs[block->name] = (byte*)block - compiler->mem;
    lowLinkBuffer[block->name] = block->name;
    visited[block->name] = true;
    seenStack[++seenStack[0]] = block->name;

    byte* const baseMem = compiler->mem;
    for(u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);
        if(!visited[succBlock->name]) {
            DFSCFGWalk(compiler, succBlock, lowLinkBuffer, visited, seenStack, blockPtrs);
        }

        for(u32 i = 1; i < seenStack[0]+1; i++) {
            if(seenStack[i] == succBlock->name) {
                lowLinkBuffer[block->name] = Min(lowLinkBuffer[block->name], lowLinkBuffer[succBlock->name]);
                break;
            }
        }
    }

    if(lowLinkBuffer[block->name] == block->name) {
        i32 i = seenStack[0];
        for(; i > -1; i--) {
            
            auto seen = seenStack[i];
            visited[seen] = false;
            lowLinkBuffer[seen] = block->name;
            if(seen == block->name) break;
        }
        seenStack[0] = (u32)i;
    }
}
SSABasicBlock* FindLoopEndBlock(byte* baseMem, LoopInfo* loop, SSABasicBlock* block, u32 blockCount, bool* visited) {

    if(visited[block->name]) return nullptr;
    visited[block->name] = true;

    Mem<u32>(visited+blockCount) = 0;
    if(IsReachable(baseMem, block, (byte*)loop->headerBlock - baseMem, (u32*)(visited+blockCount))) {
        loop->bodyBlocks[loop->bodyCount++] = block;

        SSABasicBlock* ret = nullptr;
        for(u32 i = 0; i < block->successors.edgeCount; i++) {
            u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
            auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);
            auto tmp = FindLoopEndBlock(baseMem, loop, succBlock, blockCount, visited);
            ret = tmp ? tmp : ret;
        }
        return ret;
    }
    else {
        return block;
    }
}
u32 FindLoopInitBlock(byte* baseMem, SSABasicBlock *cond, u32 *visited) {

    u32 blocks[cond->predecessors.edgeCount];
    SSABasicBlock *preds = (SSABasicBlock *)(baseMem + cond->predecessors.edges);
    memcpy(blocks, preds, cond->predecessors.edgeCount * sizeof(u32));

    u32 c = cond->predecessors.edgeCount;
    for (u32 i = 0; i < c; i++) {
        SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + blocks[i]);

        if (IsCFCyclicPred(baseMem, cond, predBlock)) {
            blocks[i--] = blocks[--c];
        }
    }

    while (c > 1) {
        SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + blocks[0]);
        SSABasicBlock *predBlock2 = (SSABasicBlock *)(baseMem + blocks[1]);

        visited[0] = 0;
        VisitAllPredessesors(baseMem, predBlock, visited);
        auto merge = FindBranchChokePointPred(baseMem, predBlock2, visited);

        blocks[0] = (u64)merge - (u64)baseMem;
        blocks[1] = blocks[--c];
    }

    return blocks[0];
}

SSABasicBlock *FindBranchEndBlock(byte* baseMem, BranchAnalysis branch, u32 *mem) {

    mem[0] = 1;
    mem[1] = (u64)branch.condBlock - (u64)baseMem;
    VisitAllSuccessors(baseMem, branch.thenBlock, mem);
    SSABasicBlock *const exitBlock = FindBranchChokePoint(baseMem, branch.elseBlock, mem);
    return exitBlock;
}

u32 FindBranches(CompilerContext ctx, SSABasicBlock *block, BranchAnalysis *result, bool* traversed) {

    if(traversed[block->name]) return 0;
    traversed[block->name] = true;

    u32 ret = 0;
    byte* const baseMem = ctx.compiler->mem;
    u32* visited = (u32*)(traversed + ctx.fn->maxBlockName);
    auto entry = ctx.fn->entry;

    if (block->nextBlock.opr == BRANCH) {

        bool loopHeader = false;
        for (u32 k = 0; k < block->predecessors.edgeCount; k++) {
            u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + k * sizeof(u32));
            SSABasicBlock *predBlock = (SSABasicBlock *)(baseMem + predBlockPtr);

            visited[0] = 0;
            if(predBlock == block || !Dominator(baseMem, entry, predBlock, block, visited)) {
                loopHeader = true;
                break;
            }
        }
        if(!loopHeader) {

            SSABasicBlock *thenBlock = (SSABasicBlock *)(baseMem + block->nextBlock.branch.thenBlock);
            BranchAnalysis *branch = result + (ret++);
            branch->condBlock = block;
            branch->thenBlock = thenBlock;
            branch->elseBlock = (SSABasicBlock *)(baseMem + block->nextBlock.branch.elseBlock);

            visited[0] = 0;
            branch->exitBlock = FindBranchEndBlock(baseMem, *branch, visited);
        }
    }

    for (u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succBlockPtr);


        ret += FindBranches(ctx, succBlock, result + ret, traversed);
    }

    return ret;
}
void RewireNextBlock(SSABasicBlock* block, u32 oldTarget, u32 newTarget) {

    ASSERT(block);
    switch(block->nextBlock.opr) {
    case BRANCH:
        if(block->nextBlock.branch.thenBlock == oldTarget) {
            block->nextBlock.branch.thenBlock = newTarget;
        }
        if(block->nextBlock.branch.elseBlock == oldTarget) {
            block->nextBlock.branch.elseBlock = newTarget;
        }
        break;
    case JMP:
        if(block->nextBlock.jmp.targetBlock == oldTarget) {
            block->nextBlock.jmp.targetBlock = newTarget;
        }
        break;
    default:
        break;
    }
}
u32 FindLoopHeaders(CompilerContext context, SSABasicBlock* entry, SSABasicBlock *block, SSABasicBlock** result, u32 *visited) {

    u32 ret = 0;
    byte *const baseMem = context.compiler->mem;
    bool blockVisited = false;
    u32 blockPtr = (byte*)block - baseMem;
    u32 visitedCount = visited[0];
    for (u32 k = 1; k < visitedCount + 1; k++) {
        if (visited[k] == blockPtr) {
            return ret;
        }
    }
    visited[++visited[0]] = blockPtr;

    for(u32 i = 0; i < block->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
        auto* predBlock = (SSABasicBlock *)(baseMem + predBlockPtr);

        visited[visited[0]+1] = 0;
        if(Dominator(baseMem, entry, block, predBlock, visited + visited[0]+1)) {
            result[ret++] = block;
            //entry = block;
            break;
        }
    }

    for(u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);
        ret += FindLoopHeaders(context, entry, succBlock, result+ret, visited);
    }

    return ret;
}
SSABasicBlock* DiscoverLoopEndBlock(CompilerContext ctx, SSABasicBlock* block, bool* traversed, SSABasicBlock* headerBlock, SSABasicBlock* preHeaderBlock) {

    byte* const baseMem = ctx.compiler->mem;
    if(traversed[block->name]) return nullptr;
    traversed[block->name] = true;

    for(u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);

        Mem<u32>(traversed+ctx.fn->maxBlockName+1) = 1;
        Mem<u32>(traversed+ctx.fn->maxBlockName+5) = (byte*)preHeaderBlock - baseMem;
        if(!IsReachable(baseMem, succBlock, (byte*)headerBlock - baseMem, (u32*)(traversed+ctx.fn->maxBlockName+1) )) {
            return succBlock;
        }

        auto r = DiscoverLoopEndBlock(ctx, succBlock, traversed, headerBlock, preHeaderBlock);
        if(r) return r;
    }
    return nullptr;
}

void InitBasicBlock(CompilerContext ctx, SSABasicBlock* block) {
    *block = {};
    block->name = ctx.fn->maxBlockName++;
    block->hotness = 0.f;
    block->values.Init();
    block->memoryPtrs.Init();
}

void GetLoopInfo(CompilerContext ctx, LoopInfo* loop, bool* traveralMem) {

    ASSERT(loop->headerBlock);
    byte* const baseMem = ctx.compiler->mem;
    
    loop->bodyCount = 0;
    auto headerBlock = loop->headerBlock;
    bool backEdges[headerBlock->predecessors.edgeCount]{};
    u32 backEdgeCount = 0;

    for(u32 i = 0; i < headerBlock->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + headerBlock->predecessors.edges + i * sizeof(u32));
        auto* predBlock = (SSABasicBlock *)(baseMem + predBlockPtr);

        Mem<u32>(traveralMem) = 0;
        if(Dominator(baseMem, ctx.fn->entry, headerBlock, predBlock, (u32*)traveralMem)) {
            backEdges[i] = true;
            backEdgeCount++;
        }
    }

    for(u32 i = 0; i < headerBlock->predecessors.edgeCount; i++) {
        if(!backEdges[i]) {
            u32 preHeaderPtr = Mem<u32>(baseMem + headerBlock->predecessors.edges + i * sizeof(u32));
            auto preHeader = (SSABasicBlock*)(baseMem + preHeaderPtr);
            loop->preHeader = preHeader;
            break;
        }
    }

    memset(traveralMem, 0, ctx.fn->maxBlockName);
    traveralMem[loop->preHeader->name] = true;
    auto exitBlock = DiscoverLoopEndBlock(ctx, headerBlock, traveralMem, headerBlock, loop->preHeader);
    ASSERT(exitBlock);
    loop->exitBlock = exitBlock;

    Mem<u32>(traveralMem) = 2;
    Mem<u32>(traveralMem + sizeof(u32) * 1) = (byte*)loop->preHeader - baseMem;
    Mem<u32>(traveralMem + sizeof(u32) * 2) = (byte*)loop->exitBlock - baseMem;
    VisitAllSuccessors(baseMem, loop->headerBlock, (u32*)traveralMem);
    loop->bodyCount = Mem<u32>(traveralMem) - 2;
    for(u32 i = 0; i < loop->bodyCount; i++) {
        loop->bodyBlocks[i] = (SSABasicBlock*)(baseMem + Mem<u32>(traveralMem + sizeof(u32) + (i+2) * sizeof(u32)));
    }
}
void CanonicalizeLoop(CompilerContext ctx, LoopInfo* loop, bool* traveralMem) {

    ASSERT(loop->headerBlock);
    byte* const baseMem = ctx.compiler->mem;
    
    loop->bodyCount = 0;
    auto headerBlock = loop->headerBlock;
    bool backEdges[headerBlock->predecessors.edgeCount]{};
    u32 backEdgeCount = 0;

    for(u32 i = 0; i < headerBlock->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + headerBlock->predecessors.edges + i * sizeof(u32));
        auto* predBlock = (SSABasicBlock *)(baseMem + predBlockPtr);

        Mem<u32>(traveralMem) = 0;
        if(Dominator(baseMem, ctx.fn->entry, headerBlock, predBlock, (u32*)traveralMem)) {
            backEdges[i] = true;
            backEdgeCount++;
        }
    }

    if(headerBlock->predecessors.edgeCount - backEdgeCount == 1) {
        for(u32 i = 0; i < headerBlock->predecessors.edgeCount; i++) {
            if(!backEdges[i]) {
                u32 preHeaderPtr = Mem<u32>(baseMem + headerBlock->predecessors.edges + i * sizeof(u32));
                auto preHeader = (SSABasicBlock*)(baseMem + preHeaderPtr);
                if(preHeader->successors.edgeCount != 1) {

                    auto canonicalPreHeader = (SSABasicBlock*)pool_allocate(&ctx.compiler->ssaBlockPool);
                    InitBasicBlock(ctx, canonicalPreHeader);
                    canonicalPreHeader->nextBlock.opr = JMP;
                    u32 canonicalPreHeaderPtr = (byte*)canonicalPreHeader - baseMem;

                    RewireNextBlock(canonicalPreHeader, 0, (byte*)headerBlock - baseMem);
                    RemoveCFGEdge(ctx, preHeaderPtr, (byte*)headerBlock - baseMem);
                    InsertCFGEdge(ctx, canonicalPreHeaderPtr, (byte*)headerBlock - baseMem);
                    InsertCFGEdge(ctx, preHeaderPtr, canonicalPreHeaderPtr);
                    RewireNextBlock(preHeader, (byte*)headerBlock - baseMem, canonicalPreHeaderPtr);
                    preHeader = canonicalPreHeader;
                }
                loop->preHeader = preHeader;
                break;
            }
        }
    }
    else {

        auto preHeader = (SSABasicBlock*)pool_allocate(&ctx.compiler->ssaBlockPool);
        loop->preHeader = preHeader;
        InitBasicBlock(ctx, preHeader);
        u32 preHeaderPtr = (byte*)preHeader - baseMem;

        u32 headerBlockPtr = (byte*)headerBlock - baseMem;
        for(u32 i = 0; i < headerBlock->predecessors.edgeCount; i++) {
            if(!backEdges[i]) {
                u32 predBlockPtr = Mem<u32>(baseMem + headerBlock->predecessors.edges + i * sizeof(u32));
                RewireNextBlock((SSABasicBlock*)(baseMem + predBlockPtr), headerBlockPtr, preHeaderPtr);
                InsertCFGEdge(ctx, predBlockPtr, preHeaderPtr);
                RemoveCFGEdge(ctx, predBlockPtr, headerBlockPtr);
                i--;
            }
        }
        InsertCFGEdge(ctx, preHeaderPtr, headerBlockPtr);
    }

    memset(traveralMem, 0, ctx.fn->maxBlockName);
    traveralMem[loop->preHeader->name] = true;
    auto exitBlock = DiscoverLoopEndBlock(ctx, headerBlock, traveralMem, headerBlock, loop->preHeader);
    ASSERT(exitBlock);
    loop->exitBlock = exitBlock;

    Mem<u32>(traveralMem) = 2;
    Mem<u32>(traveralMem + sizeof(u32) * 1) = (byte*)loop->preHeader - baseMem;
    Mem<u32>(traveralMem + sizeof(u32) * 2) = (byte*)loop->exitBlock - baseMem;
    VisitAllSuccessors(baseMem, loop->headerBlock, (u32*)traveralMem);
    loop->bodyCount = Mem<u32>(traveralMem) - 2;
    for(u32 i = 0; i < loop->bodyCount; i++) {
        loop->bodyBlocks[i] = (SSABasicBlock*)(baseMem + Mem<u32>(traveralMem + sizeof(u32) + (i+2) * sizeof(u32)));
    }
}
byte* FindLoops(CompilerContext ctx, byte* loops, u32 *visited) {

    auto res = (SSABasicBlock**)loops;
    visited[0] = 0;
    u32 headerCount = FindLoopHeaders(ctx, ctx.fn->entry, ctx.fn->entry, res, visited);
    SSABasicBlock* headers[headerCount];
    memcpy(headers, res, headerCount * sizeof(SSABasicBlock**));

    for(u32 i = 0; i < headerCount; i++) {

        auto loop = (LoopInfo*)loops;
        *loop = {};
        loop->headerBlock = headers[i];
        GetLoopInfo(ctx, loop, (bool*)visited);
        loops += sizeof(LoopInfo) + loop->bodyCount * sizeof(SSABasicBlock*);
    }

    return loops;
}
byte* MakeCanonicalLoops(CompilerContext context, byte* loops, u32 *visited) {

    auto res = (SSABasicBlock**)loops;
    visited[0] = 0;
    u32 headerCount = FindLoopHeaders(context, context.fn->entry, context.fn->entry, res, visited);
    SSABasicBlock* headers[headerCount];
    memcpy(headers, res, headerCount * sizeof(SSABasicBlock**));

    for(u32 i = 0; i < headerCount; i++) {

        auto loop = (LoopInfo*)loops;
        *loop = {};
        loop->headerBlock = headers[i];
        CanonicalizeLoop(context, loop, (bool*)visited);
        loops += sizeof(LoopInfo) + loop->bodyCount * sizeof(SSABasicBlock*);
    }

    return loops;
}

u32 GetDefsFromBlock(byte* baseMem, SSABasicBlock *block, u32 *result) {

    u32 ret = 0;
    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while (it != 0) {
        u32 itPtr = (u64)it - (u64)baseMem;
        result[ret++] = itPtr;
        it = GetNextDef(baseMem, it->nextDef);
    }
    return ret;
}
bool CheckCFGEdge(Compiler* compiler, u32 predBlockPtr, u32 succBlockPtr) {

    ASSERT(predBlockPtr != ~u32(0));
    ASSERT(succBlockPtr != ~u32(0));
    byte *const baseMem = compiler->mem;
    SSABasicBlock* predMem = (SSABasicBlock*)(baseMem + predBlockPtr);
    SSABasicBlock* succMem = (SSABasicBlock*)(baseMem + succBlockPtr);

    for(u32 i = 0 ; i < predMem->successors.edgeCount; i++) {
        u32 w = Mem<u32>(baseMem + predMem->successors.edges + i * sizeof(u32));
        if(w == succBlockPtr) return true;
    }
    for(u32 i = 0 ; i < succMem->predecessors.edgeCount; i++) {
        u32 w = Mem<u32>(baseMem + predMem->predecessors.edges + i * sizeof(u32));
        if(w == predBlockPtr) return true;
    }
    return false;
}
bool CheckMemoryEdge(Compiler* compiler, u32 predMemPtr, u32 succMemPtr) {

    ASSERT(predMemPtr != ~u32(0));
    ASSERT(succMemPtr != ~u32(0));
    byte *const baseMem = compiler->mem;
    SSAMemoryDef* predMem = (SSAMemoryDef*)(baseMem + predMemPtr);
    SSAMemoryDef* succMem = (SSAMemoryDef*)(baseMem + succMemPtr);

    for(u32 i = 0 ; i < predMem->successors.edgeCount; i++) {
        u32 w = Mem<u32>(baseMem + predMem->successors.edges + i * sizeof(u32));
        if(w == succMemPtr) return true;
    }
    for(u32 i = 0 ; i < succMem->predecessors.edgeCount; i++) {
        u32 w = Mem<u32>(baseMem + predMem->predecessors.edges + i * sizeof(u32));
        if(w == predMemPtr) return true;
    }
    return false;
}

void HoistValuesBeforeBranch(CompilerContext context, u32 blockCount, BranchAnalysis branch, u32 valDefPtr0, u32 valDefPtr1, u32* mem, u32* visitMem) {

    byte *const baseMem = context.compiler->mem;
    SSADefinition* valDef0 = (SSADefinition*)(baseMem + valDefPtr0);
    SSADefinition* valDef1 = (SSADefinition*)(baseMem + valDefPtr1);
    SSABasicBlock* block0 = (SSABasicBlock*)(baseMem + valDef0->block);
    SSABasicBlock* block1 = (SSABasicBlock*)(baseMem + valDef1->block);

    ASSERT(
        valDef0->opr != SSA_CALL         &&
        valDef0->opr != SSA_MEMORY_STORE &&
        valDef1->opr != SSA_CALL         &&
        valDef1->opr != SSA_MEMORY_STORE
    );

    RemoveDef(context, valDefPtr1);

    memset(visitMem, 0, blockCount);
    RemoveDefHelper(context, block1, (bool*)visitMem, valDefPtr1);

    MoveDef(context, valDefPtr0, branch.condBlock, block0);
    memset(visitMem, 0, blockCount);
    RerouteDefs(context, branch.condBlock, (bool*)visitMem, valDefPtr1, valDefPtr0);
    for (u32 j = 0; j < branch.exitBlock->phiCount; j++) {
        u32 phiPtr = Mem<u32>(baseMem + branch.exitBlock->phis + j * sizeof(u32));
        SSADefinition *phi = (SSADefinition *)(baseMem + phiPtr);

        u32 *ptr = GetPhiOperandPtr(baseMem, phi);
        if (phi->operand0 && (ptr[0] == valDefPtr0 || ptr[0] == valDefPtr1)) {

            memset(visitMem, 0, blockCount);
            RerouteDefs(context, branch.exitBlock, (bool*)visitMem, phiPtr, valDefPtr0);
            RemovePhi(context, branch.exitBlock, j);
            break;
        }
    }
}



u32 GetLoadsFromMemory(byte* baseMem, SSABasicBlock *block, SSADefinition *first, u32 memory, u32 *result, u32 *visited) {

    u32 ret = 0;
    SSADefinition *it = first;
    while (it != 0) {

        if (it->opr == SSA_MEMORY_LOAD && it->extraPtr == memory) {
            result[ret++] = (u64)it - (u64)baseMem;
        }

        it = (it->nextDef == 0 ? nullptr : (SSADefinition *)(baseMem + it->nextDef));
    }

    for (u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succBlockPtr);

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == succBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        visited[(visited[0]++) + 1] = succBlockPtr;
        ret += GetLoadsFromMemory(baseMem, succBlock, GetPointer<SSADefinition>(baseMem, succBlock->firstDef), memory, result, visited);
    }
    return ret;
}
void RemoveCall(CompilerContext context, u32 callPtr, u32* mem) {

    byte* const baseMem = context.compiler->mem;
    SSADefinition* call = (SSADefinition*)(baseMem + callPtr);
    SSACallInfo* extra = (SSACallInfo*)(baseMem + call->extraPtr);

    SSABasicBlock* block = (SSABasicBlock*)(baseMem + call->block);

    for(u32 i = 0; i < extra->memoryDefCount; i++) {

        auto memoryPtr = extra->memoryDefs[i+extra->memoryUseCount];

        SSAMemoryDef* memory = (SSAMemoryDef*)(baseMem + memoryPtr);
        ASSERT(memory->predecessors.edgeCount == 1);
        u32 predMemPtr = Mem<u32>(baseMem + memory->predecessors.edges);

        u32* vis = mem+4*KILO_BYTE;
        vis[0] = 0;
        u32 loadCount = GetLoadsFromMemory(baseMem, block, call, memoryPtr, mem, vis);
        /*
        vis[0] = 0;
        u32 callCount = GetCallsFromMemory(baseMem, block, call, memoryPtr, mem+loadCount, vis);

        for(u32 i = 0; i < loadCount; i++) {
            SSADefinition* load = (SSADefinition*)(baseMem + mem[i]);
            load->extraPtr = predMemPtr;
        }
        for(u32 i = 0; i < callCount; i++) {
            SSADefinition* call = (SSADefinition*)(baseMem + mem[i+loadCount]);
            SSACallInfo* extra = (SSACallInfo*)(baseMem + call->extraPtr);
            for(u32 k = 0; k < extra->memoryUseCount; k++ ) {
                
                auto& memRef = extra->memoryUses[k];
                if(memRef == memoryPtr) {
                    memRef = predMemPtr;
                    break;
                }
            }
        }
        */

        RemoveMemory(context, memoryPtr);
    }
    RemoveDef(context, callPtr);
}

template <typename... Args>
using memory_pass_t = bool (*)(Compiler *, SSAMemoryDef *, u32 *, Args...);
template <typename... Args>
bool TraverseMemory(Compiler *compiler, SSAMemoryDef *currentMemory, u32 *visited, memory_pass_t<Args...> fn, Args... args) {

    bool ret = false;
    byte *const baseMem = compiler->mem;
    for (u32 i = 0; i < currentMemory->successors.edgeCount; i++) {
        u32 successorPtr = Mem<u32>(baseMem + currentMemory->successors.edges + i * sizeof(u32));
        SSAMemoryDef *successor = (SSAMemoryDef *)(baseMem + successorPtr);

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == successorPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        visited[(visited[0]++) + 1] = successorPtr;

        ret |= fn(compiler, successor, visited, args...);
    }
    return ret;
}
u32 GetDefsFromCFG(byte* baseMem, SSABasicBlock* entry, u32* result, u32* visited) {
    
    u32 ret = GetDefsFromBlock(baseMem, entry, result);
    for(u32 i = 0; i < entry->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + entry->predecessors.edges + i * sizeof(u32));
        SSABasicBlock* succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);

        bool blockVisited = false;
        for(u32 k = 1; k < visited[0]+1; k++) {
            if(visited[k] == succBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if(blockVisited) continue;
        visited[++visited[0]] = succBlockPtr;
        ret += GetDefsFromCFG(baseMem, succBlock, result+ret, visited);
    }
    return ret;
}

u32 GetFinalValUses(CompilerContext context, SSABasicBlock* block, u32 blockCount, u32 valDef, u32* result, byte* visited) {

    byte* const baseMem = context.compiler->mem;
    u32 ret = 0;
    memset(visited, 0, blockCount);
    u32 useCount = GetValUses(context, block, (bool*)visited, valDef, result);
    if(useCount == 0) {
        result[ret++] = valDef;
        return ret;
    }
    u32 local[useCount];
    memcpy(local, result, useCount*sizeof(u32));
    for(u32 i = 0; i < useCount; i++) {
        ret += GetFinalValUses(context, block, blockCount, local[i], result+ret, visited);
    }
    return ret;
}

u32 GetInductionCycleFromPhi(CompilerContext context, u32 blockCount, u32 phiPtr, u32* result, u32* visitMem) {

    byte* const baseMem = context.compiler->mem;
    SSADefinition* phi = (SSADefinition*)(baseMem + phiPtr);
    SSABasicBlock* headerBlock = (SSABasicBlock*)(baseMem + phi->block);

    ASSERT(phi->opr == SSA_PHI_NODE);
    u32 cycleCount = 1;
    result[0] = phiPtr;
    for(u32 k = 0; k < cycleCount; k++) {

        bool visited = false;
        for(u32 j = 0; j < cycleCount; j++) {
            if(j == k) continue;
            if(result[j] == result[k]) {
                visited = true;
                break;
            }
        }
        if(!visited) {

            memset(visitMem, 0, blockCount);
            cycleCount += GetValUses(context, headerBlock, (bool*)visitMem, result[k], result+cycleCount);
        }
    }
    ASSERT(cycleCount != 0);
    cycleCount--;
    return cycleCount;
}

// -------------------- symbolic value range solver ----------------------

u32 MakeSSADefintion(byte* base, byte* mem, u32 opr , u32 operand0 , u32 operand1) {

    static TypeExpr t;
    static u32 globalSSANames =  0;
    SSADefinition* def = (SSADefinition*)mem;
    *def = {};
    def->opr = opr;
    def->operand0 = operand0;
    def->operand1 = operand1;
    def->value = globalSSANames++;
    def->type = t;
    return mem - base;
}

bool SymbolicRangeComparable(byte* baseMem, u32 ptr) {

    SSADefinition* def = (SSADefinition*)(baseMem + ptr);
    if(def->opr == RANGE_EXPRESSION) {
        SSADefinition* lower = (SSADefinition*)(baseMem + def->operand0);
        SSADefinition* upper = (SSADefinition*)(baseMem + def->operand1);

        if(lower->opr == SSA_CONSTANT) {
            Value lowerBound = Mem<Value>(baseMem + lower->operand0);
            Value nonNeg;
            Mem<u64>(nonNeg.mem) = 0;
            nonNeg.type = TYPE_PRIMARY_INT64;
            Value res = EvalOpr(lowerBound, nonNeg, SSA_CMP_BIGGER_THAN_OR_EQ, 3);
            return Mem<bool>(res.mem);
        }
        if(upper->opr == SSA_CONSTANT) {

            Value upperBound = Mem<Value>(baseMem + lower->operand0);
            Value nonPos;
            Mem<u64>(nonPos.mem) = 0;
            nonPos.type = TYPE_PRIMARY_INT64;
            Value res = EvalOpr(upperBound, nonPos, SSA_CMP_LESS_THAN_OR_EQ, 3);
            return Mem<bool>(res.mem);
        }
    }

    return false;
}
u32 FinalRelation(byte* baseMem, u32 ptr) {

    SSADefinition* def = (SSADefinition*)(baseMem + ptr);
    if(def->opr == RANGE_EXPRESSION) {
        SSADefinition* lower = (SSADefinition*)(baseMem + def->operand0);
        SSADefinition* upper = (SSADefinition*)(baseMem + def->operand1);

        if(lower->opr == SSA_CONSTANT) {
            Value lowerBound = Mem<Value>(baseMem + lower->operand0);
            Value nonNeg;
            Mem<u64>(nonNeg.mem) = 0;
            nonNeg.type = TYPE_PRIMARY_INT64;
            Value bg = EvalOpr(lowerBound, nonNeg, SSA_CMP_BIGGER_THAN, 3);
            Value eq = EvalOpr(lowerBound, nonNeg, SSA_CMP_EQ, 3);

            bool lowerBGZero = Mem<bool>(bg.mem);
            bool lowerEQZero = Mem<bool>(eq.mem);

            if(upper->opr == SSA_CMP_BIGGER_THAN) {
                Value upperBound = Mem<Value>(baseMem + upper->operand0);
                Value nonPos;
                Mem<u64>(nonPos.mem) = 0;
                nonPos.type = TYPE_PRIMARY_INT64;
                Value ls = EvalOpr(upperBound, nonPos, SSA_CMP_LESS_THAN, 3);
                Value eq1 = EvalOpr(upperBound, nonPos, SSA_CMP_EQ, 3);

                bool upperEQZero = Mem<bool>(eq1.mem);

                if(upperEQZero && lowerEQZero) {
                    return SSA_CMP_EQ;
                }
            }
            if(lowerBGZero && !lowerEQZero) {
                return SSA_CMP_BIGGER_THAN;
            }
            if(lowerBGZero || lowerEQZero) {
                return SSA_CMP_BIGGER_THAN_OR_EQ;
            }
        }
        if(upper->opr == SSA_CONSTANT) {

            Value upperBound = Mem<Value>(baseMem + upper->operand0);
            Value nonPos;
            Mem<u64>(nonPos.mem) = 0;
            nonPos.type = TYPE_PRIMARY_INT64;
            Value ls = EvalOpr(upperBound, nonPos, SSA_CMP_LESS_THAN, 3);
            Value eq = EvalOpr(upperBound, nonPos, SSA_CMP_EQ, 3);

            bool upperLSZero = Mem<bool>(ls.mem);
            bool upperEQZero = Mem<bool>(eq.mem);

            if(upperLSZero && !upperEQZero) {
                return SSA_CMP_LESS_THAN;
            }
            if(upperLSZero || upperEQZero) {
                return SSA_CMP_LESS_THAN_OR_EQ;
            }
        }
    }
    return SSA_UN_INIT;
}
struct RDGEdgeList {
    u32 next;
    u32 index;
};
struct RDGVertex {
    u32 edges;
    u32 symbol;
    bool visited;
};
struct RDG {
    u32 vertices;
    u32 vertexCount;
};

u32 InsertRDGVertex(LinearAllocator* allocator, RDG* graph, u32 symbol) {

    auto tmp = (RDGVertex*)linear_allocate(allocator, (graph->vertexCount+1) * sizeof(RDGVertex));
    memcpy(tmp, allocator->base + graph->vertices, graph->vertexCount * sizeof(RDGVertex));
    RDGVertex* vertex = (RDGVertex*)(tmp + graph->vertexCount);
    vertex->edges = 0;
    vertex->symbol = symbol;
    vertex->visited = false;

    graph->vertices = (byte*)tmp - allocator->base;
    return graph->vertexCount++;
}
void InsertDependenceEdge(LinearAllocator* allocator, RDG* graph, u32 i, u32 k) {

    if(i == k) return;
    u32 max = Max(i,k)+1;

    if(graph->vertexCount < max) {

        auto tmp = (RDGVertex*)linear_allocate(allocator, max * sizeof(RDGVertex));
        memcpy(tmp, allocator->base + graph->vertices, graph->vertexCount * sizeof(RDGVertex));
        memset(tmp + graph->vertexCount, 0, (max - graph->vertexCount) * sizeof(RDGVertex));
        graph->vertices = (byte*)tmp - allocator->base;
        graph->vertexCount = max;
    }

    auto v = GetPointer<RDGEdgeList>(allocator->base, Mem<RDGVertex>(allocator->base + graph->vertices + i*sizeof(RDGVertex)).edges);
    for(; v ; v = GetPointer<RDGEdgeList>(allocator->base, v->next) );
        

    if(!v) {

        auto adj = (RDGEdgeList*)linear_allocate(allocator, sizeof(RDGEdgeList));
        *adj = {};
        adj->index = i;

        RDGVertex* vertexK = (RDGVertex*)(allocator->base + graph->vertices + k*sizeof(RDGVertex));
        adj->next = vertexK->edges;
        RDGVertex* vertexI = (RDGVertex*)(allocator->base + graph->vertices + i*sizeof(RDGVertex));
        vertexI->edges = (byte*)adj - allocator->base;
    }

    v = GetPointer<RDGEdgeList>(allocator->base, Mem<RDGVertex>(allocator->base + graph->vertices + k*sizeof(RDGVertex)).edges);
    for(; v ; v = GetPointer<RDGEdgeList>(allocator->base, v->next) );

    if(!v) {

        auto adj = (RDGEdgeList*)linear_allocate(allocator, sizeof(RDGEdgeList));
        *adj = {};
        adj->index = k;

        RDGVertex* vertexI = (RDGVertex*)(allocator->base + graph->vertices + i*sizeof(RDGVertex));
        adj->next = vertexI->edges;
        RDGVertex* vertexK = (RDGVertex*)(allocator->base + graph->vertices + k*sizeof(RDGVertex));
        vertexK->edges = (byte*)adj - allocator->base;
    }
}
u32 BuildSSADepGraph(LinearAllocator* allocator, RDG* graph, u32 symbol, DynamicBufferLocalMalloc<HashNode<u32,u32>>* rangeTable) {

    u32 index = ~u32(0);
    for(u32 i = 0; i < graph->vertexCount; i++) {
        auto vertex = Mem<RDGVertex>(allocator->base + graph->vertices + i * sizeof(RDGVertex));
        if(vertex.symbol == symbol) {
            if(vertex.visited) return i;
            index = i;
            break;
        }
    }
    if(index == ~u32(0)) {
        index = InsertRDGVertex(allocator, graph, symbol);
    }
    Mem<RDGVertex>(allocator->base + graph->vertices + index * sizeof(RDGVertex)).visited = true;

    SSADefinition* def = (SSADefinition*)(allocator->base + symbol);
    if(IsDefBinary(def)) {

        auto left = BuildSSADepGraph(allocator, graph, def->operand0, rangeTable);
        auto right = BuildSSADepGraph(allocator, graph, def->operand1, rangeTable);
        InsertDependenceEdge(allocator, graph, index,left);
        InsertDependenceEdge(allocator, graph, index,right);
        return index;
    }
    else {
        switch(def->opr) {
        case SSA_CONSTANT:
        case POSITIVE_INF:
        case NEGATIVE_INF:
            return index;
        case SSA_UN_INIT:
        case SSA_FN_PARAMETER:
            {
                u32 range = ~u32(0);
                for(u32 i = 0; i < rangeTable->size; i++) {
                    if((*rangeTable)[i].key == symbol) {
                        range = (*rangeTable)[i].value;
                        break;
                    }
                }
                if(range != ~u32(0)) {
                    SSADefinition* rangeDef = (SSADefinition*)(allocator->base + range);
                    auto left = BuildSSADepGraph(allocator, graph, rangeDef->operand0, rangeTable);
                    auto right = BuildSSADepGraph(allocator, graph, rangeDef->operand1, rangeTable);
                    InsertDependenceEdge(allocator, graph, index,left);
                    InsertDependenceEdge(allocator, graph, index,right);
                }
                return index;
            }
            break;
        case SSA_LOGICAL_NEG:
        case SSA_BITWISE_NEG:
        case SSA_MINUS:
            {
                auto left = BuildSSADepGraph(allocator, graph, def->operand0, rangeTable);
                InsertDependenceEdge(allocator, graph, index,left);
                return index;
            }
        }
    }
    ASSERT(false);
}
u32 TopologicalSCCGraphWalk(byte* base, RDG* graph, u32 vertex, u32* lowLinkBuffer, bool* visited, u32* seenStack, u32* result) {

    u32 sccCount = 0;
    lowLinkBuffer[vertex] = vertex;
    visited[vertex] = true;
    seenStack[++seenStack[0]] = vertex;

    auto v = GetPointer<RDGVertex>(base, graph->vertices + vertex * sizeof(RDGVertex));
    for(auto it = GetPointer<RDGEdgeList>(base, v->edges); it; it = GetPointer<RDGEdgeList>(base, it->next)) {

        if(!visited[it->index]) {
            sccCount += TopologicalSCCGraphWalk(base, graph, it->index, lowLinkBuffer, visited, seenStack, result+sccCount);
        }

        for(u32 i = 1; i < seenStack[0]+1; i++) {
            if(seenStack[i] == it->index) {
                lowLinkBuffer[vertex] = Min(lowLinkBuffer[vertex], lowLinkBuffer[it->index]);
                break;
            }
        }
    }

    if(lowLinkBuffer[vertex] == vertex) {
        i32 i = seenStack[0];
        for(; i > -1; i--) {
            
            auto seen = seenStack[i];
            visited[seen] = false;
            lowLinkBuffer[seen] = vertex;
            if(seen == vertex) break;
        }
        seenStack[0] = (u32)i;
        result[sccCount++] = vertex;
    }
    
    return sccCount;
}

template<typename T>
using StaticBufferLinearAlloc = StaticBufferLocal<T, LinearAllocator, linear_allocate, roll_back_linear_allocator>;
StaticBufferLinearAlloc<u32> SymbolReplacementOrder(u32 delta, DynamicBufferLocalMalloc<HashNode<u32,u32>>* rangeTable, LinearAllocator* allocator) {

    RDG graph{};
    auto save = linear_allocator_top(allocator);
    auto root = BuildSSADepGraph(allocator, &graph, delta, rangeTable);

    bool visited[graph.vertexCount]{};
    u32 lowLinkBuffer[graph.vertexCount];
    u32 SCCs[graph.vertexCount];

    Mem<u32>(linear_allocator_top(allocator)) = 0;
    u32 sccCount = TopologicalSCCGraphWalk(allocator->base, &graph, root, lowLinkBuffer, visited, (u32*)linear_allocator_top(allocator), SCCs);

    u32 scc[graph.vertexCount];
    u32 sccSize = 0;
    u32* sorted = (u32*)linear_allocator_top(allocator);

    for(u32 i = 0; i < sccCount; i++) {
        u32 vertex = SCCs[i];
        for(u32 k = 0; k < graph.vertexCount; k++) {
            if(lowLinkBuffer[k] == vertex) {
                scc[sccSize++] = k;
            }
        }

        if(sccSize == 1) {
            u32* node = (u32*)linear_allocate(allocator, sizeof(u32));
            *node = GetPointer<RDGVertex>(allocator->base, graph.vertices + scc[0]*sizeof(RDGVertex))->symbol;
        }
        else {
            u32* final = (u32*)linear_allocate(allocator, 2 * sccSize * sizeof(u32));
            for(u32 k = 0; k < sccSize; k++) {
                final[k] = GetPointer<RDGVertex>(allocator->base, graph.vertices + scc[k]*sizeof(RDGVertex))->symbol;
            }
            memcpy(final+sccSize, final, sccSize * sizeof(u32));
        }
        sccSize = 0;
    }

    StaticBufferLinearAlloc<u32> ret{};
    ret.memory = (u32*)save;
    ret.size = graph.vertexCount;
    ret.Fill(sorted, graph.vertexCount);
    return ret;
}

typedef u32 (*ssa_expr_match_replace_t)(LinearAllocator*, DynamicBufferLocalMalloc<HashNode<u32,u32>>*, u32, bool*);
u32 SimplifySymbolic(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 symbol, ssa_expr_match_replace_t* rewrites, u32 rewriteCount, bool* change) {
    
    u32 replaced = symbol;
    for(auto i = rewrites; i < rewrites+rewriteCount; i++) {
        auto rewrite = *i;
        replaced = rewrite(stack, table, replaced, change);
    }
    return replaced;
}
u32 Rule0(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 def, bool* change) {

    auto ssa = GetPointer<SSADefinition>(stack->base, def);
    if(ssa->opr == RANGE_EXPRESSION) {
        auto left = GetPointer<SSADefinition>(stack->base, ssa->operand0);
        auto right = GetPointer<SSADefinition>(stack->base, ssa->operand1);

        if(left->opr == RANGE_EXPRESSION) {
            *change |= true;
            auto leftLeft = GetPointer<SSADefinition>(stack->base, left->operand0);
            auto leftRight = GetPointer<SSADefinition>(stack->base, left->operand1);
            ssa->operand0 = left->operand0;
        }
    }

    return def;
}
u32 Rule1(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 def, bool* change) {

    auto ssa = GetPointer<SSADefinition>(stack->base, def);
    if(ssa->opr == RANGE_EXPRESSION) {
        auto left = GetPointer<SSADefinition>(stack->base, ssa->operand0);
        auto right = GetPointer<SSADefinition>(stack->base, ssa->operand1);

        if(right->opr == RANGE_EXPRESSION) {
            *change |= true;
            auto rightLeft = GetPointer<SSADefinition>(stack->base, right->operand0);
            auto rightRight = GetPointer<SSADefinition>(stack->base, right->operand1);
            ssa->operand1 = right->operand1;
        }
    }

    return def;
}
u32 Rule2(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 def, bool* change) {

    auto ssa = GetPointer<SSADefinition>(stack->base, def);
    if(ssa->opr == SSA_ADD) {
        auto left = GetPointer<SSADefinition>(stack->base, ssa->operand0);
        auto right = GetPointer<SSADefinition>(stack->base, ssa->operand1);

        if(left->opr == RANGE_EXPRESSION) {
            
            *change |= true;
            auto fresh = linear_allocate(stack, sizeof(SSADefinition));
            auto lowerBound = MakeSSADefintion(stack->base, (byte*)fresh, SSA_ADD, left->operand0, ssa->operand1);

            fresh = linear_allocate(stack, sizeof(SSADefinition));
            auto upperBound = MakeSSADefintion(stack->base, (byte*)fresh, SSA_ADD, left->operand1, ssa->operand1);

            fresh = linear_allocate(stack, sizeof(SSADefinition));
            return MakeSSADefintion(stack->base, (byte*)fresh, RANGE_EXPRESSION, lowerBound, upperBound);
        }
        else if(right->opr == RANGE_EXPRESSION) {

            *change |= true;
            auto fresh = linear_allocate(stack, sizeof(SSADefinition));
            auto lowerBound = MakeSSADefintion(stack->base, (byte*)fresh, SSA_ADD, right->operand0, ssa->operand0);

            fresh = linear_allocate(stack, sizeof(SSADefinition));
            auto upperBound = MakeSSADefintion(stack->base, (byte*)fresh, SSA_ADD, right->operand1, ssa->operand0);

            fresh = linear_allocate(stack, sizeof(SSADefinition));
            return MakeSSADefintion(stack->base, (byte*)fresh, RANGE_EXPRESSION, lowerBound, upperBound);
        }
    }
    return def;
}
u32 GetRelation(LinearAllocator* stack, u32 p, u32 q, DynamicBufferLocalMalloc<HashNode<u32, u32>>* rangeTable);
u32 Rule3Helper(LinearAllocator* stack, u32 a, u32 b, u32 c, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table) {

    auto save = *stack;
    auto mem = linear_allocate(stack, sizeof(SSADefinition));
    auto zero = MakeSSADefintion(stack->base, (byte*)mem, SSA_CONSTANT, stack->top, 0);
    Value* v = (Value*)linear_allocate(stack, sizeof(Value));
    Mem<u64>(v->mem) = 0;
    v->type = TYPE_PRIMARY_INT64;

    auto relation = GetRelation(stack, c, zero, table);
    *stack = save;

    if(relation == SSA_CMP_BIGGER_THAN_OR_EQ || relation == SSA_CMP_BIGGER_THAN || relation == SSA_CMP_EQ) {
        byte* mulMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto lower = MakeSSADefintion(stack->base, mulMem, TOKEN_ASTERISK, a,c);
        mulMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto upper = MakeSSADefintion(stack->base, mulMem, TOKEN_ASTERISK, b,c);

        byte* rangeMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        return MakeSSADefintion(stack->base, rangeMem, RANGE_EXPRESSION, lower,upper);
    }
    else if( relation == SSA_CMP_LESS_THAN_OR_EQ || relation == SSA_CMP_LESS_THAN || relation == SSA_CMP_EQ) {

        byte* mulMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto lower = MakeSSADefintion(stack->base, mulMem, TOKEN_ASTERISK, b,c);
        mulMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto upper = MakeSSADefintion(stack->base, mulMem, TOKEN_ASTERISK, a,c);

        byte* rangeMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        return MakeSSADefintion(stack->base, rangeMem, RANGE_EXPRESSION, lower,upper);
    }

    byte* mulMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
    auto lower = MakeSSADefintion(stack->base, mulMem, NEGATIVE_INF ,0,0);
    mulMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
    auto upper = MakeSSADefintion(stack->base, mulMem, POSITIVE_INF, 0,0);

    byte* rangeMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
    return MakeSSADefintion(stack->base, rangeMem, RANGE_EXPRESSION, lower,upper);
}
u32 Rule3(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 def, bool* change) {
    
    auto ssa = GetPointer<SSADefinition>(stack->base, def);
    if(ssa->opr == SSA_MUL) {
        auto left = GetPointer<SSADefinition>(stack->base, ssa->operand0);
        auto right = GetPointer<SSADefinition>(stack->base, ssa->operand1);

        if(left->opr == RANGE_EXPRESSION) {
            *change |= true;
            return Rule3Helper(stack, left->operand0, left->operand1, ssa->operand1, table);
        }
        else if(right->opr == RANGE_EXPRESSION) {
            *change |= true;
            return Rule3Helper(stack, right->operand0, right->operand1, ssa->operand0, table);
        }
    }
    return def;
}
u32 Rule4Helper(LinearAllocator* stack, u32 a, u32 b, u32 c, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table) {

    auto save = *stack;
    auto mem = linear_allocate(stack, sizeof(SSADefinition));
    auto zero = MakeSSADefintion(stack->base, (byte*)mem, SSA_CONSTANT, stack->top, 0);
    Value* v = (Value*)linear_allocate(stack, sizeof(Value));
    Mem<u64>(v->mem) = 0;
    v->type = TYPE_PRIMARY_INT64;

    auto relation = GetRelation(stack, c, zero, table);
    *stack = save;

    if(relation == SSA_CMP_BIGGER_THAN) {

        byte* divMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto lower = MakeSSADefintion(stack->base, divMem, SSA_DIV ,a,c);
        
        divMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto upper = MakeSSADefintion(stack->base, divMem, SSA_DIV ,b,c);

        auto rangeMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        return MakeSSADefintion(stack->base, rangeMem, RANGE_EXPRESSION ,lower,upper);
    }
    else if(relation == SSA_CMP_LESS_THAN) {

        byte* divMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto lower = MakeSSADefintion(stack->base, divMem, SSA_DIV ,b,c);
        
        divMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto upper = MakeSSADefintion(stack->base, divMem, SSA_DIV ,a,c);

        auto rangeMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        return MakeSSADefintion(stack->base, rangeMem, RANGE_EXPRESSION ,lower,upper);
    }

    byte* mulMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
    auto lower = MakeSSADefintion(stack->base, mulMem, NEGATIVE_INF ,0,0);
    mulMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
    auto upper = MakeSSADefintion(stack->base, mulMem, POSITIVE_INF, 0,0);

    byte* rangeMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
    return MakeSSADefintion(stack->base, rangeMem, RANGE_EXPRESSION, lower,upper);
}
u32 Rule4(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 def, bool* change) {

    auto ssa = GetPointer<SSADefinition>(stack->base, def);
    if(ssa->opr == SSA_DIV) {
        auto left = GetPointer<SSADefinition>(stack->base, ssa->operand0);
        auto right = GetPointer<SSADefinition>(stack->base, ssa->operand1);

        if(left->opr == RANGE_EXPRESSION && right->opr != RANGE_EXPRESSION) {
            *change |= true;
            return Rule4Helper(stack, left->operand0, left->operand1, ssa->operand1, table);
        }
    }
    return def;
}
u32 Rule5Helper(LinearAllocator* stack, u32 a, u32 b, u32 i, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, bool* change) {

    auto mem = linear_allocate(stack, sizeof(SSADefinition));
    auto zero = MakeSSADefintion(stack->base, (byte*)mem, SSA_CONSTANT, stack->top, 0);
    Value* v = (Value*)linear_allocate(stack, sizeof(Value));
    Mem<u64>(v->mem) = 0;
    v->type = TYPE_PRIMARY_INT64;

    auto save = *stack;
    auto relation = GetRelation(stack, a, zero, table);
    *stack = save;


    if(relation == SSA_CMP_BIGGER_THAN_OR_EQ || relation == SSA_CMP_BIGGER_THAN || relation == SSA_CMP_EQ) {

        *change |= true;
        byte* powerMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto lower = MakeSSADefintion(stack->base, powerMem, SSA_POWER ,a,i);
        
        powerMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto upper = MakeSSADefintion(stack->base, powerMem, SSA_POWER ,b,i);

        auto rangeMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        return MakeSSADefintion(stack->base, rangeMem, RANGE_EXPRESSION ,lower,upper);
    }

    zero = MakeSSADefintion(stack->base, (byte*)mem, SSA_CONSTANT, stack->top-sizeof(Value), 0);
    Mem<u64>(v->mem) = 0;
    v->type = TYPE_PRIMARY_INT64;

    relation = GetRelation(stack, b, zero, table);
    *stack = save;
    if(relation == SSA_CMP_LESS_THAN_OR_EQ || relation == SSA_CMP_LESS_THAN || relation == SSA_CMP_EQ) {

        *change |= true;
        byte* powerMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto lower = MakeSSADefintion(stack->base, powerMem, SSA_POWER ,b,i);
        
        powerMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        auto upper = MakeSSADefintion(stack->base, powerMem, SSA_POWER ,a,i);

        auto rangeMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        return MakeSSADefintion(stack->base, rangeMem, RANGE_EXPRESSION ,lower,upper);
    }

    return ~u32(0);
}
byte IsEven(byte* base, u32 def) {
    auto ssa = (SSADefinition*)(base + def);
    if(ssa->opr == SSA_CONSTANT) {
        Value v = Mem<Value>(base + ssa->operand0);
        if(IsUnsigned((TypeName)v.type)) {
            return (Mem<u64>(v.mem) % 2 == 0);
        }
        else {
            return (Mem<i64>(v.mem) % 2 == 0);
        }
    }
    return ~byte(0);
}
bool IsOdd(byte* base, u32 def) {

    auto ssa = (SSADefinition*)(base + def);
    if(ssa->opr == SSA_CONSTANT) {
        Value v = Mem<Value>(base + ssa->operand0);
        if(IsUnsigned((TypeName)v.type)) {
            return (Mem<u64>(v.mem) % 2 != 0);
        }
        else {
            return (Mem<i64>(v.mem) % 2 != 0);
        }
    }
    return ~byte(0);
}
bool IsTypeUnsigned(byte* base, TypeExpr type) {
    return IsUnsigned(GetLastType(base, type));
}
u32 Rule5(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 def, bool* change) {

    auto ssa = GetPointer<SSADefinition>(stack->base, def);
    if(ssa->opr == SSA_POWER) {
        auto left = GetPointer<SSADefinition>(stack->base, ssa->operand0);
        auto right = GetPointer<SSADefinition>(stack->base, ssa->operand1);

        if(left->opr == RANGE_EXPRESSION) {
            if(IsEven(stack->base, ssa->operand1) == 1) {
                auto ret = Rule5Helper(stack, left->operand0, left->operand1, ssa->operand1, table, change);
                if(ret != ~u32(0)) return ret;
            }
            else if(IsOdd(stack->base, ssa->operand1) == 1) {
                *change |= true;
                auto tmp = ssa->operand0;
                ssa->operand0 = ssa->operand1;
                ssa->operand1 = tmp;
            }
            else {
                *change |= true;
                auto infMem = linear_allocate(stack, sizeof(SSADefinition));
                auto upper = MakeSSADefintion(stack->base, (byte*)infMem, POSITIVE_INF,0,0);

                infMem = linear_allocate(stack, sizeof(SSADefinition));
                auto lower = MakeSSADefintion(stack->base, (byte*)infMem, NEGATIVE_INF,0,0);

                auto rangeMem = linear_allocate(stack, sizeof(SSADefinition));
                return MakeSSADefintion(stack->base, (byte*)rangeMem, RANGE_EXPRESSION, lower,upper);
            }
        }
    }
    return def;
}
u32 Rule6(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 def, bool* change) {

    auto ssa = GetPointer<SSADefinition>(stack->base, def);
    if(ssa->opr == SSA_MIN) {
        auto left = GetPointer<SSADefinition>(stack->base, ssa->operand0);
        auto right = GetPointer<SSADefinition>(stack->base, ssa->operand1);

        if(left->opr == RANGE_EXPRESSION && right->opr != RANGE_EXPRESSION) {

            *change |= true;
            auto minMem = linear_allocate(stack, sizeof(SSADefinition));
            auto lower = MakeSSADefintion(stack->base, (byte*)minMem, SSA_MIN, left->operand0, ssa->operand1);

            minMem = linear_allocate(stack, sizeof(SSADefinition));
            auto upper = MakeSSADefintion(stack->base, (byte*)minMem, SSA_MIN, left->operand1, ssa->operand1);

            minMem = linear_allocate(stack, sizeof(SSADefinition));
            return MakeSSADefintion(stack->base, (byte*)minMem, RANGE_EXPRESSION, lower, upper);
        }
    }
    return def;
}
u32 Rule7(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 def, bool* change) {

    auto ssa = GetPointer<SSADefinition>(stack->base, def);
    if(ssa->opr == SSA_MAX) {
        auto left = GetPointer<SSADefinition>(stack->base, ssa->operand0);
        auto right = GetPointer<SSADefinition>(stack->base, ssa->operand1);

        if(left->opr == RANGE_EXPRESSION && right->opr != RANGE_EXPRESSION) {

            *change |= true;
            auto minMem = linear_allocate(stack, sizeof(SSADefinition));
            auto lower = MakeSSADefintion(stack->base, (byte*)minMem, SSA_MAX, left->operand0, ssa->operand1);

            minMem = linear_allocate(stack, sizeof(SSADefinition));
            auto upper = MakeSSADefintion(stack->base, (byte*)minMem, SSA_MAX, left->operand1, ssa->operand1);

            minMem = linear_allocate(stack, sizeof(SSADefinition));
            return MakeSSADefintion(stack->base, (byte*)minMem, RANGE_EXPRESSION, lower, upper);
        }
    }
    return def;
}
void PrintSymbol(LinearAllocator* stack, u32 rootPtr) {

    SSADefinition* root = (SSADefinition*)(stack->base + rootPtr);
    if(IsDefBinary(root)) {
        PrintSymbol(stack, root->operand0);
        PrintSymbol(stack, root->operand1);
    }
    else if(!IsDefTerminator(root)) {
        PrintSymbol(stack, root->operand0);
    }
    PrintSSADef(stack->base, root);
    global_print("%\n");
    global_io_flush();
}

u32 FoldConstsSolver(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 def, bool* change) {

    SSADefinition* ssa = (SSADefinition*)(stack->base + def);
    if(!IsValueConst(stack->base, def) && IsValueConstRecursive(stack->base, def)) {
        
        *change |= true;
        auto vPtr = stack->top;
        Value* v = (Value*)linear_allocate(stack, sizeof(Value));
        *v = EvalConstDef(stack->base, ssa, (byte*)linear_allocator_top(stack));

        auto mem = linear_allocate(stack, sizeof(SSADefinition));
        return MakeSSADefintion(stack->base, (byte*)mem, SSA_CONSTANT, vPtr,0);
    }

    if(IsDefBinary(ssa)) {
        ssa->operand0 = FoldConstsSolver(stack, table, ssa->operand0, change);
        ssa->operand1 = FoldConstsSolver(stack, table, ssa->operand1, change);
    }
    else if(!IsDefTerminator(ssa)) {
        ssa->operand1 = FoldConstsSolver(stack, table, ssa->operand0, change);
    }

    return def;
}
u32 Identity0(LinearAllocator* stack, u32 ssaDef, bool* change) {

    byte* const baseMem = stack->base;
    SSADefinition* root = (SSADefinition*)(baseMem + ssaDef);
    SSADefinition* op0 = (SSADefinition*)(baseMem + root->operand0);
    SSADefinition* op1 = (SSADefinition*)(baseMem + root->operand1);
    if(root->opr == SSA_ADD) {
        if(op0->opr == SSA_CONSTANT) {
            auto v = (Value*)(baseMem + op0->operand0);
            if(IsImmZero(*v)) {
                *change |= true;
                return root->operand1;
            }
        }
        if(op1->opr == SSA_CONSTANT) {
            auto v = (Value*)(baseMem + op1->operand0);
            if(IsImmZero(*v)) {
                *change |= true;
                return root->operand0;
            }
        }
    }
    if(root->opr == SSA_SUB) {
        
        if(op1->opr == SSA_CONSTANT) {
            Value* v = (Value*)(baseMem + op1->operand0);
            if(IsImmZero(*v)) {
                *change |= true;
                return root->operand0;
            }
        }
        if(op0 == op1) {

            *change |= true;
            byte* immMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
            ssaDef = MakeSSADefintion(stack->base, immMem, SSA_CONSTANT, stack->top, 0);
            Value* v = (Value*)linear_allocate(stack, sizeof(Value));
            *v = MakeImm(GetLastType(baseMem, root->type), 0);
            return ssaDef;
        }
    }
    return ssaDef;
}
u32 Identity1(LinearAllocator* stack, u32 ssaDef, bool* change) {

    byte* const baseMem = stack->base;
    SSADefinition* root = (SSADefinition*)(baseMem + ssaDef);
    SSADefinition* op1 = (SSADefinition*)(baseMem + root->operand1);
    if(root->opr == SSA_MUL) {
        SSADefinition* op0 = (SSADefinition*)(baseMem + root->operand0);
        if(op0->opr == SSA_CONSTANT) {
            auto v = ((ImmediateExpr*)(baseMem + op0->operand0))->v;
            if(IsImmOne(v)) {
                *change |= true;
                return root->operand1;
            }
            if(IsImmZero(v)) {
                *change |= true;
                return root->operand0;                
            }
        }
        if(op1->opr == SSA_CONSTANT) {
            auto v = ((ImmediateExpr*)(baseMem + op0->operand1))->v;
            if(IsImmOne(v)) {
                *change |= true;
                return root->operand0;
            }
            if(IsImmZero(v)) {
                *change |= true;
                return root->operand1;
            }
        }
    }
    return ssaDef;
}
u32 Identity2(LinearAllocator* stack, u32 ssaDef, bool* change) {

    byte* const baseMem = stack->base;
    SSADefinition* root = (SSADefinition*)(baseMem + ssaDef);
    SSADefinition* op1 = (SSADefinition*)(baseMem + root->operand1);
    if(root->opr == SSA_DIV) {
        SSADefinition* op0 = (SSADefinition*)(baseMem + root->operand0);
        if(op0->opr == SSA_CONSTANT) {
            auto v = (Value*)(baseMem + op0->operand0);
            if(IsImmOne(*v)) {
                *change |= true;
                return root->operand1;
            }
            if(IsImmZero(*v)) {
                *change |= true;
                return root->operand0;
            }
        }
        if(op1->opr == SSA_CONSTANT) {
            auto v = (Value*)(baseMem + op0->operand1);
            if(IsImmOne(*v)) {
                *change |= true;
                return root->operand0;
            }
        }

        if(op0 == op1) {
            *change |= true;
            byte* immMem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
            ssaDef = MakeSSADefintion(stack->base, immMem, SSA_CONSTANT, stack->top, 0);
            Value* v = (Value*)linear_allocate(stack, sizeof(Value));
            *v = MakeImm(GetLastType(baseMem, root->type), 1);
            return ssaDef;
        }
    }

    return ssaDef;
}
u32 Identity3(LinearAllocator* stack, u32 def, bool* change) {

    byte* const baseMem = stack->base;
    SSADefinition* root = (SSADefinition*)(baseMem + def);
    if(root->opr == SSA_SUB) {

        root->opr = TOKEN_PLUS;
        auto right = stack->top;
        byte* mem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        MakeSSADefintion(stack->base, mem, SSA_MUL, stack->top, root->operand1);

        mem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
        MakeSSADefintion(stack->base, mem, SSA_CONSTANT, stack->top,0);

        Value* minusOne = (Value*)linear_allocate(stack, sizeof(Value));
        *minusOne = MakeImm(TYPE_PRIMARY_INT64, -1);

        bool dummy;
        root->operand1 = FoldConstsSolver(stack, nullptr, right, &dummy);
    }
    return def;
}
u32 Identity4(LinearAllocator* stack, u32 def, bool* change) {

    byte* const baseMem = stack->base;
    SSADefinition* root = (SSADefinition*)(baseMem + def);
    if(root->opr == SSA_MUL) {
        if(root->operand0 == root->operand1) {

            *change |= true;
            root->opr = SSA_POWER;
            root->operand1 = stack->top;
            byte* mem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
            MakeSSADefintion(stack->base, mem, SSA_CONSTANT, stack->top,0);
            Value* two = (Value*)linear_allocate(stack, sizeof(Value));
            Mem<i64>(two->mem) = 2;
            two->type = TYPE_PRIMARY_INT64;
        }
    }

    return def;
}
u32 Identity5(LinearAllocator* stack, u32 def, bool* change) {

    byte* const baseMem = stack->base;
    SSADefinition* root = (SSADefinition*)(baseMem + def);
    if(root->opr == SSA_MUL) {

        auto left = (SSADefinition*)(baseMem + root->operand0);
        auto right = (SSADefinition*)(baseMem + root->operand1);

        if(left->opr == SSA_POWER && left->operand0 == root->operand1) {
            *change = true;
            byte* mem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
            auto inc = MakeSSADefintion(stack->base, mem, SSA_ADD, left->operand1, stack->top);

            mem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
            MakeSSADefintion(stack->base, mem, SSA_CONSTANT, stack->top, 0);

            Value* one = (Value*)linear_allocate(stack, sizeof(Value));
            *one = MakeImm(TYPE_PRIMARY_INT64, 1);

            left->operand1 = FoldConstsSolver(stack, nullptr, inc, change);

            return root->operand0;
        }
        if(right->opr == SSA_POWER && right->operand0 == root->operand0) {
            *change = true;
            byte* mem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
            auto inc = MakeSSADefintion(stack->base, mem, SSA_ADD, right->operand1, stack->top);

            mem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
            MakeSSADefintion(stack->base, mem, SSA_CONSTANT, stack->top, 0);

            Value* one = (Value*)linear_allocate(stack, sizeof(Value));
            *one = MakeImm(TYPE_PRIMARY_INT64, 1);

            right->operand1 = FoldConstsSolver(stack, nullptr, inc, change);

            return root->operand1;
        }

        if(left->opr == SSA_POWER && right->opr == SSA_POWER && left->operand0 == right->operand0) {

            *change = true;
            byte* mem = (byte*)linear_allocate(stack, sizeof(SSADefinition));
            left->operand1 = MakeSSADefintion(stack->base, mem, SSA_ADD, left->operand1, right->operand1);
            return root->operand0;
        }
    }
    return def;
}
u32 Identity6(LinearAllocator* stack, u32 def, bool* change) {

    byte* const baseMem = stack->base;
    SSADefinition* root = (SSADefinition*)(baseMem + def);
    if(IsDefBinary(root)) {
        auto left = (SSADefinition*)(baseMem + root->operand0);
        auto right = (SSADefinition*)(baseMem + root->operand1);

        if(left->opr == POSITIVE_INF || left->opr == NEGATIVE_INF) {
            *change |= true;
            return root->operand0;
        }
    }
    return def;
}
u32 RewriteIdentities(LinearAllocator* stack, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table, u32 def, bool* change) {

    typedef u32(*ssa_rewrite_t)(LinearAllocator*, u32, bool*);
    ssa_rewrite_t ruleTable[] = {
        Identity0,
        Identity1,
        Identity2,
        Identity3,
        Identity4,
        Identity5,
        Identity6,
    };
    for(auto it = ruleTable; it < 1[&ruleTable]; it++) {
        def = (*it)(stack, def, change);
    }
    return def;
}

u32 SimplifyRec(LinearAllocator* stack, u32 rootPtr, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table) {
    
    SSADefinition* root = (SSADefinition*)(stack->base + rootPtr);
    if(IsDefBinary(root)) {
        root->operand0 = SimplifyRec(stack, root->operand0, table);
        root->operand1 = SimplifyRec(stack, root->operand1, table);
    }
    else if(!IsDefTerminator(root)) {
        root->operand0 = SimplifyRec(stack, root->operand0, table);
    }

    ssa_expr_match_replace_t rules[] = {
        FoldConstsSolver,
        RewriteIdentities,
        Rule0,
        Rule1,
        Rule2,
        Rule3,
        Rule4,
        Rule5,
        Rule6,
        Rule7,
        FoldConstsSolver,
        RewriteIdentities,
    };

    bool run = true;
    while(run) {
        run = false;
        rootPtr = SimplifySymbolic(stack, table, rootPtr, rules, 1[&rules] - rules, &run);
    }
    return rootPtr;
}
u32 ReplaceSymbol(LinearAllocator* stack, u32 rootPtr, u32 symbol, u32 range, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table) {

    SSADefinition* root = (SSADefinition*)(stack->base + rootPtr);
    if(IsDefBinary(root)) {
        root->operand0 = ReplaceSymbol(stack, root->operand0, symbol, range, table);
        root->operand1 = ReplaceSymbol(stack, root->operand1, symbol, range, table);
    }
    else if(!IsDefTerminator(root)) {
        root->operand0 = ReplaceSymbol(stack, root->operand0, symbol, range, table);
    }

    u32 ret = rootPtr;
    if(rootPtr == symbol) {
        ret = range;
    }

    ret = SimplifyRec(stack, ret, table);
}
u32 ReplaceAllSymbols(LinearAllocator* stack, u32 rootPtr, u32 range, DynamicBufferLocalMalloc<HashNode<u32,u32>>* table) {

    auto root = GetPointer<SSADefinition>(stack->base, rootPtr);

    u32 ret = rootPtr;
    if(!IsDefTerminator(root)) {
        u32 ret = range;
    }
    ret = SimplifyRec(stack, ret, table);
}

const char* GetRelationStr(u32 relation) {
    switch(relation) {
    case SSA_UN_INIT:return "?";
    case SSA_CMP_LESS_THAN: return "<";
    case SSA_CMP_LESS_THAN_OR_EQ: return "<=";
    case SSA_CMP_BIGGER_THAN: return ">";
    case SSA_CMP_BIGGER_THAN_OR_EQ: return ">=";
    case SSA_CMP_EQ: return "==";
    }
}
u32 GetRelation(LinearAllocator* stack, u32 p, u32 q, DynamicBufferLocalMalloc<HashNode<u32, u32>>* rangeTable) {

    auto deltaMem = linear_allocate(stack, sizeof(SSADefinition));
    auto minus = MakeSSADefintion(stack->base, (byte*)deltaMem, SSA_SUB, p, q);
    Mem<SSADefinition>(stack->base + minus).type = Mem<SSADefinition>(stack->base + p).type;

    minus = SimplifyRec(stack, minus, rangeTable);

    auto deltaRange = linear_allocate(stack, sizeof(SSADefinition));
    auto d = MakeSSADefintion(stack->base, (byte*)deltaRange, RANGE_EXPRESSION, minus,minus);

    auto orderS = SymbolReplacementOrder(minus, rangeTable, stack);
    u32 orderIndex = 0;

    while (!SymbolicRangeComparable(stack->base, d) && orderIndex < orderS.size) {

        u32 x = orderS[orderIndex++];
        u32 xSymbolicRange = ~u32(0);
        for(u32 i = 0; i < rangeTable->size; i++) {
            if((*rangeTable)[i].key == x) {
                xSymbolicRange = (*rangeTable)[i].value;
                break;
            }
        }
        if(xSymbolicRange != ~u32(0)) {
            d = ReplaceSymbol(stack, d, x, xSymbolicRange, rangeTable);
        }
    }

    d = SimplifyRec(stack, d, rangeTable);
    if(!SymbolicRangeComparable(stack->base, d)) {
        
        auto top = linear_allocate(stack, sizeof(SSADefinition));
        auto posInf = MakeSSADefintion(stack->base, (byte*)top, POSITIVE_INF, 0,0);

        top = linear_allocate(stack, sizeof(SSADefinition));
        auto negInf = MakeSSADefintion(stack->base, (byte*)top, NEGATIVE_INF, 0,0);

        top = linear_allocate(stack, sizeof(SSADefinition));
        auto rangeAll = MakeSSADefintion(stack->base, (byte*)top, RANGE_EXPRESSION, negInf, posInf);

        d = ReplaceAllSymbols(stack, d, rangeAll, rangeTable);
        d = SimplifyRec(stack, d, rangeTable);
    }

    return FinalRelation(stack->base, d);
}
// -------------------- symbolic value range solver END ----------------------

void MergeBasicBlocks(CompilerContext context, u32 predBlockPtr, u32 succBlockPtr) {

    byte *const baseMem = context.compiler->mem;
    SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
    SSABasicBlock* succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);

    ASSERT(predBlock->successors.edgeCount == 1);
    ASSERT(succBlock->predecessors.edgeCount == 1);

    if(succBlock->firstDef) {
        auto firstDefPtr = succBlock->firstDef;
        auto firstDef = (SSADefinition*)(baseMem + succBlock->firstDef);

        firstDef->prevDef = predBlock->lastDef ? predBlock->lastDef : 0;

        if(predBlock->lastDef) {
            Mem<SSADefinition>(baseMem + predBlock->lastDef).nextDef = firstDefPtr;
            predBlock->lastDef = succBlock->lastDef;
        }
        else {
            predBlock->firstDef = firstDefPtr;
            predBlock->lastDef = succBlock->lastDef;
        }
    }

    RemoveCFGEdge(context, predBlockPtr, succBlockPtr);
    auto nextBlock = succBlock->nextBlock;
    for(;succBlock->successors.edgeCount;) {
        u32 succSuccBlockPtr = Mem<u32>(baseMem + succBlock->successors.edges);
        RemoveCFGEdge(context, succBlockPtr, succSuccBlockPtr);
        InsertCFGEdge(context, predBlockPtr, succSuccBlockPtr);
    }
    predBlock->nextBlock = nextBlock;

    auto it = GetPointer<SSADefinition>(baseMem, predBlock->firstDef);
    while(it) {
        it->block = predBlockPtr;
        it = GetNextDef(baseMem, it->nextDef);
    }
}

bool FoldConstants(CompilerContext context, SSABasicBlock *currentBlock, bool* visited) {

    bool ret = false;
    auto it = GetPointer<SSADefinition>(context.compiler->mem, currentBlock->firstDef);
    while (it != 0) {

        u32 itPtr = (u64)it - (u64)context.compiler->mem;
        if (!IsValueConst(context.compiler->mem, itPtr) && IsValueConstRecursive(context.compiler->mem, itPtr)) {
            ret = true;

            Mem<u32>(visited+context.fn->maxBlockName) = 0;
            Value v = EvalConstDef(context.compiler->mem, it, (byte *)(visited + context.fn->maxBlockName));
            it->opr = SSA_CONSTANT;
            Value *val = AllocateSSA<Value>(context.compiler);
            it->operand0 = (byte*)val - context.compiler->mem;
            *val = v;
        }

        it = GetPointer<SSADefinition>(context.compiler->mem, it->nextDef);
    }

    return ret | TraverseCFG(context, currentBlock, visited, FoldConstants);
}
void PropogateValues(CompilerContext context, SSABasicBlock* block) {

    byte* const baseMem = context.compiler->mem;

    for(u32 k = 0; k < block->successors.edgeCount; k++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + k * sizeof(u32));
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);
        succBlock->values.Clear();
    }

    for(u32 i = 0; i < block->values.cap; i++) {
        auto v = block->values[i];
        if(v == ~u32(0)) continue;

        for(u32 k = 0; k < block->successors.edgeCount; k++) {
            u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + k * sizeof(u32));
            auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);
            
            u32 index = succBlock->values.Find(context.compiler, v);
            if(index == ~u32(0)) {
                succBlock->values.Insert(context.compiler, v);
            }
        }
    }
}
bool CommonSubExprElim(CompilerContext context, SSABasicBlock *currentBlock, bool *visited) {

    bool ret = false;
    byte *const baseMem = context.compiler->mem;
    u32 blockCount = context.fn->maxBlockName;

    auto it = GetPointer<SSADefinition>(baseMem, currentBlock->firstDef);
    while(it) {

        u32 itPtr = (u64)it - (u64)context.compiler->mem;
        u32 index = currentBlock->values.Find(context.compiler, itPtr);
        auto next = GetPointer<SSADefinition>(baseMem, it->nextDef);

        if (index == ~u32(0)) {
            currentBlock->values.Insert(context.compiler, itPtr);
        }
        else {
            u32 defPtr = currentBlock->values[index];
            auto *def = (SSADefinition *)(baseMem + defPtr);
            if (defPtr != itPtr && def->value < it->value) {
                ret = true;

                memset(visited+blockCount+1, 0, blockCount);
                Mem<bool>(visited + blockCount + 1 + currentBlock->name) = true;
                RerouteDefs(context, currentBlock, (bool*)(visited+blockCount+1), itPtr, defPtr);

                RemoveDefFromBlock(context, itPtr);
                memset(visited+blockCount+1, 0, blockCount);
                Mem<bool>(visited+blockCount+currentBlock->name+1) = true;
                RemoveDefHelper(context, currentBlock, (bool*)(visited+blockCount+1), itPtr);
            }
        }

        it = next;
    }
    PropogateValues(context, currentBlock);

    return ret | TraverseCFG(context, currentBlock, visited, CommonSubExprElim);
}
bool RemoveUneccesserayPhis(CompilerContext context, SSABasicBlock* entry, bool* visited, u32* mem) {

    bool ret = false;
    byte* const baseMem = context.compiler->mem;
    u32 blockCount = context.fn->maxBlockName;

    u32* phis = (u32*)(baseMem + entry->phis);
    for(u32 i = 0; i < entry->phiCount; i++) {
        u32 phiPtr = phis[i];
        SSADefinition* phi = (SSADefinition*)(baseMem + phiPtr);

        u32 removed = TryRemoveTrivialPhi(context, entry, phi, (bool*)(visited+blockCount), mem);
        ret |= (removed != phiPtr);
        if((removed == phiPtr)) {

            memset(visited+blockCount+1, 0, context.fn->maxBlockName);
            u32 useCount = GetValUses(context, entry, (bool*)(visited+blockCount+1), phiPtr, mem);
            if(useCount == 0) {
                RemovePhi(context, entry, i--);
                ret = true;
            }
        }
        else {
            i--;
        }

    }

    u32* memPhis = (u32*)(baseMem + entry->memoryPhis);
    for(u32 i = 0; i < entry->memoryPhiCount; i++) {
        auto memPhi = (SSAMemoryDef*)(baseMem + memPhis[i]);

        bool remove = TryRemoveTrivialMemoryPhi(context, entry, memPhi, mem, (u32*)(visited+blockCount));
        i -= remove;
        ret |= remove;
    }

    return ret | TraverseCFG(context, entry, visited, RemoveUneccesserayPhis, mem);
}
bool HoistBranchInvariants(CompilerContext ctx, BranchAnalysis branch, u32 *mem, u32* visitMem) {

    byte *const baseMem = ctx.compiler->mem;
    bool ret = false;

    u32 thenDefCount = GetDefsFromBlock(baseMem, branch.thenBlock, mem);
    u32 elseDefCount = GetDefsFromBlock(baseMem, branch.elseBlock, mem + thenDefCount);

    for (u32 i = 0; i < thenDefCount; i++) {
        auto *def0 = (SSADefinition *)(baseMem + mem[i]);
        if(def0->opr ==  SSA_MEMORY_STORE || def0->opr == SSA_CALL) continue;

        for (u32 k = thenDefCount; k < thenDefCount + elseDefCount; k++) {
            auto *def1 = (SSADefinition *)(baseMem + mem[k]);
            if(def1->opr ==  SSA_MEMORY_STORE || def1->opr == SSA_CALL) continue;

            mem[thenDefCount + elseDefCount] = 0;
            if(def0->opr == SSA_MEMORY_STORE && def1->opr == SSA_MEMORY_STORE) {
                //ret |= TryHoistStoresBeforeBranch(compiler, branch, mem[i], mem[k], mem + thenDefCount + elseDefCount, visitMem);
            }
            else if (EQSSA(baseMem, mem[i], mem[k], mem + thenDefCount + elseDefCount)) {

                HoistValuesBeforeBranch(ctx, ctx.fn->maxBlockName, branch, mem[i++], mem[k], mem + thenDefCount + elseDefCount, visitMem);
                def0 = (SSADefinition*)(baseMem + mem[i]);
            }
        }
    }

    return ret;
}


void MarkUsedValues(CompilerContext context, SSABasicBlock* currentBlock, u32* visited) {

    byte* const baseMem = context.compiler->mem;
    u32 maxBlockName = context.fn->maxBlockName;
    visited[currentBlock->name] = (byte*)currentBlock - baseMem;

    for(u32 i = 0; i < currentBlock->phiCount; i++) {
        u32 phiPtr = Mem<u32>(baseMem + currentBlock->phis + i * sizeof(u32));
        auto phi = (SSADefinition*)(baseMem + phiPtr);

        u32 count = GetPhiOperandCount(phi);
        u32* operands = GetPhiOperandPtr(baseMem, phi);
        for(u32 k = 0; k < count; k++) {
            auto opPtr = operands[k];
            auto op = (SSADefinition*)(baseMem + opPtr);
            auto val = Mem<SSADefinition>(baseMem + operands[k]).value;
            visited[maxBlockName + val] = 0;
        }
    }

    auto it = GetPointer<SSADefinition>(baseMem, currentBlock->firstDef);
    while(it) {

        if(visited[maxBlockName + it->value] == ~u32(0)) {
            visited[maxBlockName + it->value] = (byte*)it - baseMem;
        }

        if(DefOp2(it)) {
            auto op0 = (SSADefinition*)(baseMem + it->operand0);
            auto op1 = (SSADefinition*)(baseMem + it->operand1);

            visited[maxBlockName + op0->value] = 0;
            visited[maxBlockName + op1->value] = 0;
        }
        else if(DefOp1(it)) {
            auto op0 = (SSADefinition*)(baseMem + it->operand0);
            visited[maxBlockName + op0->value] = 0;
        }
        else if(it->opr == SSA_CALL){

            auto callee = Mem<SSADefinition>(baseMem + it->operand0).value;
            visited[maxBlockName + callee] = 0;
            auto args = GetCallArgs(baseMem, it);
            for(u32 i = 0; i < args.argCount; i++) {
                auto arg = (SSADefinition*)(baseMem + args.ptr[i]);
                visited[maxBlockName + arg->value] = 0;
            }
        }

        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    switch(currentBlock->nextBlock.opr) {
    case BRANCH:
        {
            auto val = Mem<SSADefinition>(baseMem + currentBlock->nextBlock.branch.conditionDef).value;
            visited[maxBlockName + val] = 0;
            break;
        }
    case RET:
        {
            if(currentBlock->nextBlock.ret.retDef != ~u32(0)) {
                auto val = Mem<SSADefinition>(baseMem + currentBlock->nextBlock.ret.retDef).value;
                visited[maxBlockName + val] = 0;
            }
            break;
        }
    }
    
    
    for(u32 i = 0; i < currentBlock->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + currentBlock->successors.edges + i * sizeof(u32));
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);

        if(visited[succBlock->name] != ~u32(0)) {
            continue;
        }
        MarkUsedValues(context, succBlock, visited);
    }
}
void nop_() {}
void PrintSSADefRec(byte* baseMem, SSADefinition* it) {

    global_print("%\n");
    PrintSSADef(baseMem, it);
    if(IsDefBinary(it)) {
        PrintSSADefRec(baseMem, (SSADefinition*)(baseMem + it->operand0));
        PrintSSADefRec(baseMem, (SSADefinition*)(baseMem + it->operand1));
    }
    else if(it->opr == SSA_PHI_NODE) {
        u32 count = GetPhiOperandCount(it);
        u32* operands = GetPhiOperandPtr(baseMem, it);
        for(u32 i = 0; i < count; i++) {
            PrintSSADefRec(baseMem, (SSADefinition*)(baseMem + operands[i]));
        }
    }
    else if(it->opr == SSA_CALL) {
        auto args = GetCallArgs(baseMem, it);
        for(u32 i = 0; i < args.argCount; i++) {
            PrintSSADefRec(baseMem, (SSADefinition*)(baseMem + args.ptr[i]));
        }
    }
    else if(!IsDefTerminator(it)) {
        PrintSSADefRec(baseMem, (SSADefinition*)(baseMem + it->operand0));
    }
}
void RenameSSACFG(CompilerContext context, SSABasicBlock* block, u32* visited) {

    
    byte* const baseMem = context.compiler->mem;
    block->name = context.fn->maxBlockName++;

    u32* phis = (u32*)(baseMem + block->phis);
    for(u32 i = 0; i < block->phiCount; i++) {
        auto phi = (SSADefinition*)(baseMem + phis[i]);
        phi->value = context.fn->maxSSAName++;
    }

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {

        it->value = context.fn->maxSSAName++;
        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    for(u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        
        bool seen = false;
        u32 vistiedCount = visited[0]+1;
        for(u32 k = 1;  k < vistiedCount; k++) {
            if(visited[k] == succBlockPtr) {
                seen = true;
                break;
            }
        }
        if(seen) {
            continue;
        }

        visited[++visited[0]] = succBlockPtr;
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);
        RenameSSACFG(context, succBlock, visited);
    }
}
u32 EnumaretBlocks(Compiler* compiler, SSABasicBlock* block, u32 blockCount, u32* visited) {

    u32 ret = block->name;
    byte* const baseMem = compiler->mem;
    visited[block->name] = (byte*)block - baseMem;

    for(u32 i = 0; i < block->successors.edgeCount; i++) {

        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);

        if(visited[succBlock->name] != 0) {
            continue;
        }
        ret = Max(ret, EnumaretBlocks(compiler, succBlock, blockCount, visited));
    }
    return ret;
}
void RemoveBlock(CompilerContext ctx, u32 block) {

    byte* const baseMem = ctx.compiler->mem;
    auto b = (SSABasicBlock*)(baseMem + block);

    while(b->successors.edgeCount) {
        u32 succBlockPtr = Mem<u32>(baseMem + b->successors.edges);
        RemoveCFGEdge(ctx, block, succBlockPtr);
    }

    b->values.Free();
    b->memoryPtrs.Free();
    pool_free(&ctx.compiler->ssaBlockPool, b);
}
bool DefHashSideEffects(CompilerContext ctx, SSADefinition* def) {
    if(def->opr == SSA_MEMORY_STORE) return true;
    if(def->opr == SSA_CALL) {
        auto callPtr = (byte*)def - ctx.compiler->mem;
        if(HasSideEffects(ctx.compiler, callPtr)) return true;
    }
    return false;
}
bool CollectUnusedValuesAndBlocks(CompilerContext context, SSABasicBlock* entry, u32* visited, u32* blockLookUp) {

    u32 maxSSAName = context.fn->maxSSAName;
    u32 maxBlockName = context.fn->maxBlockName;

    byte* const baseMem = context.compiler->mem;
    memset(visited, ~u32(0), sizeof(u32) * (maxSSAName + maxBlockName)*2);
    MarkUsedValues(context, entry, visited);

    bool ret = false;
    for(u32 i = 0; i < maxBlockName; i++) {
        if(visited[i] == ~u32(0)) {
            if(blockLookUp[i] != 0) {

                auto lut = blockLookUp[i];
                
                auto block = (SSABasicBlock*)(context.compiler->mem + blockLookUp[i]);
                ASSERT(block->name == i);

                RemoveBlock(context, blockLookUp[i]);
                blockLookUp[i] = 0;
                ret = true;
            }
        }
    }
    for(u32 i = 0; i < maxSSAName; i++) {
        auto v = visited[maxBlockName + i];
        if(v != 0 && v != ~u32(0)) {
            auto def = (SSADefinition*)(baseMem + v);
            if(DefHashSideEffects(context, def)) continue;
            RemoveDef(context, v);
            ret = true;
        }
    }

    context.fn->maxBlockName = 0;
    context.fn->maxSSAName = 0;
    visited[0] = 0;
    RenameSSACFG(context, context.fn->entry, visited);
    context.fn->maxBlockName++;
    context.fn->maxSSAName++;

    auto fn = context.fn;
    memset(visited, 0, fn->maxBlockName*sizeof(u32));
    fn->maxBlockName = EnumaretBlocks(context.compiler, fn->entry, fn->maxBlockName, visited)+1;
    memcpy(blockLookUp, visited, fn->maxBlockName*sizeof(u32));

    return ret;
}

bool IsBlockEmpty(SSABasicBlock* block) {
    return !block->firstDef && !block->lastDef && block->phiCount == 0;
}
bool IsBlockJump(SSABasicBlock* block) {
    return block->nextBlock.opr == JMP;
}
bool IsBlockUnreachable(SSABasicBlock* block) {
    return block->predecessors.edgeCount == 0;
}
bool IsRetBlock(Compiler* compiler, SSABasicBlock* block, SSAFunction* fn) {

    byte *const baseMem = compiler->mem;
    for(u32 i = 0; i < fn->outBlocks.edgeCount; i++) {
        u32 retBlockPtr = Mem<u32>(baseMem + fn->outBlocks.edges + i * sizeof(u32));
        auto retBlock = (SSABasicBlock*)(baseMem + retBlockPtr);
        if(retBlock == block) {
            return true;
        }
    }
    return false;
}
CFGEdges AddRetBlock(Compiler* compiler, CFGEdges outBlock, u32 block) {

    CFGEdges ret{compiler->miscAllocatorSSA, outBlock.edgeCount+1};
    u32* tmp = (u32*)AllocateSSA(compiler, (outBlock.edgeCount+1)*sizeof(u32));
    memcpy(tmp, compiler->mem + outBlock.edges, outBlock.edgeCount * sizeof(u32));
    tmp[outBlock.edgeCount] = block;
    return ret;
}
void DeDupe(CompilerContext ctx, SSABasicBlock* block) {

    byte* const baseMem = ctx.compiler->mem;
    for(u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlock = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        for(u32 k = 0; k < block->successors.edgeCount; k++) {
            if(k == i) continue;
            u32 succBlock2 = Mem<u32>(baseMem + block->successors.edges + k * sizeof(u32));
            if(succBlock == succBlock2) {
                u32 last = Mem<u32>(baseMem + block->successors.edges + (--block->successors.edgeCount) * sizeof(u32));
                Mem<u32>(baseMem + block->successors.edges + k * sizeof(u32)) = last;
                k--;
            }
        }
    }
}

bool IsLoopHeader(CompilerContext ctx, SSABasicBlock* block, u32* visitMem) {

    byte* const baseMem = ctx.compiler->mem;
    for(u32 i = 0; i < block->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
        auto predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);

        visitMem[0] = 0;
        if(Dominator(baseMem, ctx.fn->entry, block, predBlock, visitMem)) return true;
    }
    return false;
}

bool EliminateSuperflousControlFlow(CompilerContext context, SSABasicBlock *currentBlock, bool *visited) {

    bool ret = false;
    byte *const baseMem = context.compiler->mem;
    u32 currentBlockPtr = (u64)currentBlock - (u64)baseMem;

    for(u32 i = 0; i < currentBlock->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + currentBlock->predecessors.edges + i*sizeof(u32));
        auto predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
        ASSERT(predBlock->nextBlock.opr != RET);

        if(predBlock->nextBlock.opr == RET) {
            RemoveCFGEdge(context, predBlockPtr, currentBlockPtr);
            i--;
            ret = true;
        }
    }

    if(currentBlock->predecessors.edgeCount == 1) {
        u32 predBlockPtr = Mem<u32>(baseMem + currentBlock->predecessors.edges);
        auto predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
        if(IsBlockJump(predBlock) && predBlock->successors.edgeCount == 1) {
            ret = true;
            MergeBasicBlocks(context, predBlockPtr, currentBlockPtr);
            currentBlock = predBlock;
            currentBlockPtr = predBlockPtr;
        }
    }

    switch(currentBlock->nextBlock.opr) {
    case BRANCH:
        if(currentBlock->nextBlock.branch.thenBlock == currentBlock->nextBlock.branch.elseBlock) {
            ASSERT(currentBlock->successors.edgeCount == 1);
            auto target = currentBlock->nextBlock.branch.thenBlock;
            currentBlock->nextBlock.opr = JMP;
            currentBlock->nextBlock.jmp.targetBlock = target;
            ret = true;
        }
        break;
    case JMP:
        if(IsBlockEmpty(currentBlock)) {
            auto target = currentBlock->nextBlock.jmp.targetBlock;
            auto targetBlock = (SSABasicBlock*)(baseMem + target);
            for(u32 i = 0; i < currentBlock->predecessors.edgeCount; i++) {
                u32 predBlockPtr = Mem<u32>(baseMem + currentBlock->predecessors.edges + i * sizeof(u32));
                auto predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
                for(u32 k = 0; k < predBlock->successors.edgeCount; k++) {
                    u32 predBlockSuccPtr = Mem<u32>(baseMem + predBlock->successors.edges + k * sizeof(u32));
                    if(predBlockSuccPtr == currentBlockPtr) {
                        Mem<u32>(baseMem + predBlock->successors.edges + k * sizeof(u32)) = target;
                    }
                }
                for(u32 k = 0; k < targetBlock->predecessors.edgeCount; k++) {
                    u32 predBlockSuccPtr = Mem<u32>(baseMem + targetBlock->predecessors.edges + k * sizeof(u32));
                    if(predBlockSuccPtr == currentBlockPtr) {
                        Mem<u32>(baseMem + targetBlock->predecessors.edges + k * sizeof(u32)) = predBlockPtr;
                    }
                }

                RewireNextBlock(predBlock, currentBlockPtr, target);
                DeDupe(context, predBlock);
            }
            
            ret = true;
            currentBlock->predecessors.edgeCount = 0;
            currentBlock->successors.edgeCount = 0;
            if(currentBlock == context.fn->entry) {
                context.fn->entry = targetBlock;
            }
            currentBlock = targetBlock;
        }
        else {
            auto targetBlockPtr = currentBlock->nextBlock.jmp.targetBlock;
            auto targetBlock = (SSABasicBlock*)(baseMem + currentBlock->nextBlock.jmp.targetBlock);
            if(IsBlockEmpty(targetBlock) && targetBlock->nextBlock.opr == BRANCH && !IsLoopHeader(context, targetBlock, (u32*)(visited + context.fn->maxBlockName))) {

                ret = true;
                RemoveCFGEdge(context, currentBlockPtr, targetBlockPtr);
                if(IsBlockUnreachable(targetBlock)) {
                    RemoveCFGEdge(context, targetBlockPtr, targetBlock->nextBlock.branch.thenBlock);
                    RemoveCFGEdge(context, targetBlockPtr, targetBlock->nextBlock.branch.elseBlock);
                }

                InsertCFGEdge(context, currentBlockPtr, targetBlock->nextBlock.branch.thenBlock);
                InsertCFGEdge(context, currentBlockPtr, targetBlock->nextBlock.branch.elseBlock);
                currentBlock->nextBlock = targetBlock->nextBlock;
            }
        }
        break;
    }

    return ret | TraverseCFG(context, currentBlock, visited, EliminateSuperflousControlFlow);
}

bool DoesPhiFormCycle(byte* baseMem, u32 phiPtr, u32* visitMem) {

    SSADefinition* phi = (SSADefinition*)(baseMem + phiPtr);
    auto operands = GetPhiOperandPtr(baseMem, phi);
    for(u32 i = 0; i < phi->operand0; i++) {
        visitMem[0] = 0;
        if(DoesValueDependOn(baseMem, phiPtr, operands[i], visitMem)) return true;
    }
    return false;
}

bool RemoveUselessInductionCycles(CompilerContext context, const LoopInfo* loop, u32* visitMem, u32* mem) {

    
    byte* const baseMem = context.compiler->mem;
    bool ret = false;
    for(u32 i = 0; i < loop->headerBlock->phiCount; i++) {
        u32 phiPtr = Mem<u32>(baseMem + loop->headerBlock->phis + i * sizeof(u32));
        SSADefinition* phi = (SSADefinition*)(baseMem + phiPtr);

        if(!DoesPhiFormCycle(baseMem, phiPtr, visitMem)) continue;
        u32 cycleCount = GetInductionCycleFromPhi(context, context.fn->maxBlockName, phiPtr, mem, visitMem);
        bool used = false;
        for(u32 k = 0; k < cycleCount; k++) {

            visitMem[0] = 0;
            if(!DoesValueDependOn(baseMem, mem[k], phiPtr, visitMem)) {
                used = true;
                break;
            }
        }

        if(!used) {
            
            for(u32 k = 0; k < cycleCount; k++) {
                SSADefinition* cycleNode = (SSADefinition*)(baseMem + mem[k]);
                if(cycleNode != phi) {
                    RemoveDef(context, mem[k]);
                }
            }
            RemovePhi(context, loop->headerBlock, i);
            ret = true;
        }
    }

    return ret;
}

bool TryRemoveUselessLoop(CompilerContext context, SSABasicBlock* fnEntry, u32 blockCount, const LoopInfo* loop, u32* mem, u32* visitMem) {

    bool ret = false;
    byte* const baseMem = context.compiler->mem;
    if(loop->headerBlock->phiCount == 1) {

        u32 headerBlockPtr = (byte*)loop->headerBlock - baseMem;
        u32 loopOutBranchCount = 0;
        u32 loopOutBranches[loop->headerBlock->predecessors.edgeCount+1];
        u32 loopOutBlocks[loop->headerBlock->predecessors.edgeCount+1];

        u32 loopBackEdgesCount = 0;
        u32 loopBackEdges[loop->headerBlock->predecessors.edgeCount];
        if(loop->headerBlock->nextBlock.opr == BRANCH) {
            loopOutBranches[loopOutBranchCount] = ((byte*)(&loop->headerBlock->nextBlock)) - baseMem;;
            loopOutBlocks[loopOutBranchCount++] = headerBlockPtr;
        }

        for(u32 i = 0; i < loop->headerBlock->predecessors.edgeCount; i++) {
            u32 predBlockPtr = Mem<u32>(baseMem + loop->headerBlock->predecessors.edges + i * sizeof(u32));
            SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);

            visitMem[0] = 0;
            if(Dominator(baseMem, fnEntry, loop->headerBlock, predBlock, visitMem)) {
                loopBackEdges[loopBackEdgesCount++] = predBlockPtr;
                if(predBlock->nextBlock.opr == BRANCH) {
                    loopOutBranches[loopOutBranchCount] = ((byte*)(&predBlock->nextBlock)) - baseMem;
                    loopOutBlocks[loopOutBranchCount++] = predBlockPtr;
                }
            }
        }

        u32 phiPtr = Mem<u32>(baseMem + loop->headerBlock->phis);
        SSADefinition* phi = (SSADefinition*)(baseMem + phiPtr);
        u32 cycleCount = GetInductionCycleFromPhi(context, blockCount, phiPtr, mem, visitMem);

        bool onlyUsedByBranches = true;
        for(u32 i = 0; i < cycleCount; i++) {
            visitMem[0] = 0;
            if(!DoesValueDependOn(baseMem, mem[i], phiPtr, visitMem)) {

                NextBlock* component = (NextBlock*)(baseMem + mem[i]);
                if(component->opr != BRANCH) {
                    SSADefinition* comp = (SSADefinition*)(baseMem + mem[i]);
                    SSABasicBlock* compBlock = (SSABasicBlock*)(baseMem + comp->block);
                    visitMem[0] = 0;
                    u32 useCount = GetFinalValUses(context, compBlock, blockCount, mem[i], mem+cycleCount, (byte*)visitMem);
                    for(u32 k = 0; k < useCount; k++) {
                        u32 opr = Mem<u32>(baseMem + mem[cycleCount+k]);
                        if(opr != BRANCH) {
                            onlyUsedByBranches = false;
                            break;
                        }
                    }
                }
                else {
                    bool outBranch = false;
                    for(u32 k = 0; k < loopOutBranchCount; k++) {
                        if(loopOutBranches[k] == mem[i]) {
                            outBranch = true;
                            break;
                        }
                    }
                    if(!outBranch) {
                        onlyUsedByBranches = false;
                        break;
                    }
                }
            }
        }

        if(onlyUsedByBranches) {
            
            u32 outBlockPtr;
            visitMem[0] = 1;
            visitMem[1] = (byte*)loop->preHeader - baseMem;
            SSABasicBlock* outBranch = (SSABasicBlock*)(baseMem + loopOutBlocks[0]);
            auto outBlock = (SSABasicBlock*)(baseMem + outBranch->nextBlock.branch.thenBlock);
            if(!IsReachable(baseMem, outBlock, headerBlockPtr, visitMem)) {
                outBlockPtr = outBranch->nextBlock.branch.thenBlock;
            }
            else {
                outBlockPtr = outBranch->nextBlock.branch.elseBlock;
                outBlock = (SSABasicBlock*)(baseMem + outBranch->nextBlock.branch.elseBlock);
                visitMem[0] = 1;
                visitMem[1] = (byte*)loop->preHeader - baseMem;
                ASSERT(!IsReachable(baseMem, outBlock, headerBlockPtr, visitMem));
            }

            for(u32 i = 0; i < loopBackEdgesCount; i++) {
                RemoveCFGEdge(context, loopBackEdges[i], headerBlockPtr);
                RemoveCFGEdge(context, headerBlockPtr, loopBackEdges[i]);
            }
            for(u32 i = 0; i < loopOutBranchCount; i++) {
                RemoveCFGEdge(context, loopOutBlocks[i], outBlockPtr);
            }
            InsertCFGEdge(context, headerBlockPtr, outBlockPtr);

            loop->headerBlock->nextBlock.opr = JMP;
            loop->headerBlock->nextBlock.jmp.targetBlock = outBlockPtr;
            ret = true;
        }
    }
    return ret;
}

bool OperatorLshiftOrEq(u32 opr) {
    return opr == SSA_CMP_LESS_THAN || opr == SSA_CMP_LESS_THAN_OR_EQ || opr == SSA_CMP_EQ;
}
bool OperatorRshiftOrEq(u32 opr) {
    return opr == SSA_CMP_BIGGER_THAN || opr == SSA_CMP_BIGGER_THAN_OR_EQ || opr == SSA_CMP_EQ;
}
bool SymbolicRangeOverlap(LinearAllocator stack, SymbolicRange r0, SymbolicRange r1, DynamicBufferLocalMalloc<HashNode<u32, u32>>* rangeTable) {

    auto begin = (byte*)linear_allocator_top(&stack);
    auto relation0 = GetRelation(&stack, r0.lower, r1.lower, rangeTable);
    roll_back_linear_allocator(&stack, begin);

    auto relation1 = GetRelation(&stack, r0.lower, r1.upper, rangeTable);
    roll_back_linear_allocator(&stack, begin);

    if(OperatorLshiftOrEq(relation1) && OperatorRshiftOrEq(relation0)) {
        return true;
    }

    relation0 = GetRelation(&stack, r0.upper, r1.lower, rangeTable);
    roll_back_linear_allocator(&stack, begin);

    relation1 = GetRelation(&stack, r0.upper, r1.upper, rangeTable);
    roll_back_linear_allocator(&stack, begin);
    
    if(OperatorLshiftOrEq(relation1) && OperatorRshiftOrEq(relation0)) {
        return true;
    }

    return false;
}
bool SymbolicRangesOverlap(LinearAllocator stack, SymbolicRange* r0, SymbolicRange* r1, u32 r0Count, u32 r1Count, DynamicBufferLocalMalloc<HashNode<u32, u32>>* rangeTable) {

    for(u32 i = 0; i < r0Count; i++) {
        for(u32 k = 0; k < r1Count; k++) {
            if(SymbolicRangeOverlap(stack, r0[i], r1[k], rangeTable)) {
                return true;
            }
        }
    }

    return false;
}
u32 GetPossibleValuesFromLoad(Compiler* compiler, LinearAllocator* stack, SSADefinition* load, SSAMemoryDef* currentMemory, SymbolicRange range, u32* visitMem) {

    u32 ret = 0;
    byte *const baseMem = compiler->mem;
    SSAMemoryDef *it = currentMemory;
    while (it) {

        u32 itPtr = (u64)it - (u64)baseMem;
        switch (it->type) {
        case MEMORY_UNKOWN:
            {
                ASSERT(it->predecessors.edgeCount == 0);
                *(u32*)linear_allocate(stack, sizeof(u32)) = ~u32(0);
                ret++;
            }
            return ret;
        case MEMORY_DEF:
            {
                ASSERT(it->predecessors.edgeCount < 2);
                auto clobberedRange = (SymbolicRange*)(baseMem + it->addressRanges.edges);
                auto store = (SSADefinition*)(baseMem + it->ssaDef);

                if(store->opr == SSA_MEMORY_STORE) {

                    bool aligned = TypesEqual(baseMem, baseMem, baseMem, baseMem, load->type, store->type);
                    visitMem[0] = 0;
                    aligned &= EQSSA(compiler, load->operand0, store->operand0, visitMem);
                    if(aligned) {
                        *(u32*)linear_allocate(stack, sizeof(u32)) = store->operand1;
                        ret++;
                        return ret;
                    }

                    SymbolicRange locals[it->addressRanges.edgeCount];
                    auto save = *stack;
                    for(u32 i = 0; i < it->addressRanges.edgeCount; i++) {
                        locals[i].lower = CpySSAChain(compiler->mem, stack, clobberedRange[i].lower);
                        locals[i].upper = CpySSAChain(compiler->mem, stack, clobberedRange[i].upper);
                    }
                    if(SymbolicRangesOverlap(*stack, &range, locals, 1, it->addressRanges.edgeCount, &compiler->rangeTable)) {
                        *stack = save;
                        *(u32*)linear_allocate(stack, sizeof(u32)) = ~u32(0);
                        ret++;
                        return ret;
                    }
                    *stack = save;
                }

                u32 predMem = Mem<u32>(baseMem + it->predecessors.edges);
                it = GetPointer<SSAMemoryDef>(baseMem, predMem);
                break;
            }
        case MEMORY_PHI:
            {
                for(u32 i = 0; i < it->predecessors.edgeCount; i++) {
                    auto predMemPtr = Mem<u32>(baseMem + it->predecessors.edges + i * sizeof(u32));
                    auto predMem = (SSAMemoryDef*)(baseMem + predMemPtr);
                    ret += GetPossibleValuesFromLoad(compiler, stack, load, predMem, range, visitMem);
                }
                return ret;
            }
        }
    }

    return ret;
}
bool TryRemoveLoad(CompilerContext context, SSADefinition* load, u32 blockCount, u32* mem, u32* visitMem) {

    byte* const baseMem = context.compiler->mem;

    auto loadBlock = (SSABasicBlock*)(baseMem + load->block);
    u32 loadPtr = (byte*)load - baseMem;
    u32 loadWidth = GetTypeSize(baseMem, baseMem, load->type) - 1;

    LinearAllocator stack = make_linear_allocator(mem, 32*KILO_BYTE);
    auto ranges = (SymbolicRange*)linear_allocator_top(&stack);
    u32 rangeCount = GetAddressRanges(context, &stack, load->operand0, loadWidth, visitMem);

    auto info = (SSALoadInfo*)(baseMem + load->extraPtr);
    ASSERT(info->infoType == LOAD_INFO);

    u32* possibleValues = (u32*)linear_allocator_top(&stack);
    u32 possibleValueCount = 0;
    for(u32 i = 0; i < info->memoryUseCount; i++) {

        auto memoryDef = (SSAMemoryDef*)(baseMem + info->memoryUses[i]);
        possibleValueCount += GetPossibleValuesFromLoad(context.compiler, &stack, load, memoryDef, *ranges, visitMem);
    }

    for(u32 k = 0; k < possibleValueCount; k++) {
        if(possibleValues[k] == ~u32(0)) {
            return false;
        }
    }
    if(possibleValueCount == 1) {
        memset(visitMem, 0, blockCount);
        RerouteDefs(context, loadBlock, (bool*)visitMem, loadPtr, possibleValues[0]);
        RemoveDef(context, loadPtr);
        return true;
    }
    else if(possibleValueCount > 1) {

        SSABasicBlock* mergeBlock = (SSABasicBlock*)(baseMem + load->block);
        auto phi = AddPhiNode(context, mergeBlock, load->type);

        for(u32 i = 0; i < possibleValueCount; i++) {

            SSADefinition* from = (SSADefinition*)(baseMem + possibleValues[i]);
            SSABasicBlock* fromBlock = (SSABasicBlock*)(baseMem + from->block);
            if(!CheckCFGEdge(context.compiler, from->block, load->block)) {

                SSABasicBlock* selectedBlock = nullptr;
                for(u32 k = 0; k < mergeBlock->predecessors.edgeCount; k++) {
                    u32 predBlockPtr = Mem<u32>(baseMem + mergeBlock->predecessors.edges + k * sizeof(u32));
                    
                    possibleValues[possibleValueCount+1] = 0;
                    if(!IsReachable(baseMem, fromBlock, predBlockPtr, possibleValues+possibleValueCount+1)) continue;

                    bool used = false;
                    for(u32 j = 0; j < possibleValueCount; j++) {
                        SSADefinition* val = (SSADefinition*)(baseMem + possibleValues[j]);
                        if(val->block == predBlockPtr) {
                            used = true;
                            break;
                        }
                    }

                    if(!used) {
                        selectedBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
                        break;
                    }
                }

                auto copy = (SSADefinition*)pool_allocate(&context.compiler->ssaBlockPool);
                *copy = {};
                copy->opr = SSA_COPY;
                copy->type = load->type;
                copy->operand0 = possibleValues[i];
                possibleValues[i] = MakeDef(context, selectedBlock, (byte*)copy - baseMem);
            }
        }

        AddPhiNodeIncoming(context, phi, possibleValues, possibleValueCount);
        visitMem[0] = 0;
        memset(visitMem, 0, blockCount);
        RerouteDefs(context, loadBlock, (bool*)visitMem, loadPtr, (u64)phi - (u64)baseMem);
        return true;
    }

    return false;
}
bool IsDefParamaterDependent(Compiler *compiler, u32 defPtr, u32 *visited) {

    byte *const baseMem = compiler->mem;
    SSADefinition *def = (SSADefinition *)(baseMem + defPtr);

    if (def->opr == SSA_FN_PARAMETER) {
        return true;
    }

    u32 c = visited[0];
    bool defVisited = false;
    for (u32 i = 1; i < c + 1; i++) {
        if (visited[i] == defPtr) {
            return true;
        }
    }

    switch (def->opr) {
    case SSA_CONSTANT:
    case SSA_FUNCTION:
    case SSA_UN_INIT:
    case SSA_ALLOCA:
        return false;
    case SSA_PHI_NODE:
        {
            auto operands = GetPhiOperandPtr(baseMem, def);
            for(u32 i = 0; i < def->operand0; i++) {
                if(IsDefParamaterDependent(compiler, operands[i], visited)) return true;
            }
            return false;
        }
    case SSA_MEMORY_LOAD:
        return IsDefParamaterDependent(compiler, def->operand0, visited);
    case SSA_CALL: {
        bool dep = IsDefParamaterDependent(compiler, def->operand0, visited);
        auto args = GetCallArgs(baseMem, def);
        for (u32 i = 0; i < args.argCount; i++) {
            dep |= IsDefParamaterDependent(compiler, args.ptr[i], visited);
        }
        return dep;
    }
    case SSA_CONVERT:
        return IsDefParamaterDependent(compiler, def->operand0, visited);
    case SSA_MINUS:
    case SSA_LOGICAL_NEG:
    case SSA_BITWISE_NEG:
        return IsDefParamaterDependent(compiler, def->operand0, visited);
    case EXPRESSION_MEMORY_STORE:ASSERT(false);
    default:
        return IsDefParamaterDependent(compiler, def->operand0, visited) ||
               IsDefParamaterDependent(compiler, def->operand1, visited);
    }
}
bool IsMemoryUsed(CompilerContext context, LinearAllocator stack, SymbolicRange* range, u32 rangeCount, SSABasicBlock* block, u32 memoryPtr, u32* visitMem) {

    byte* const baseMem = context.compiler->mem;
    SSAMemoryDef* memory = (SSAMemoryDef*)(baseMem + memoryPtr);
    u32* loads = (u32*)linear_allocator_top(&stack);

    visitMem[0] = 0;
    u32 loadCount = GetLoadsFromMemory(baseMem, block, GetPointer<SSADefinition>(baseMem, block->firstDef), memoryPtr, loads, visitMem);
    auto observed = (SymbolicRange*)linear_allocator_top(&stack);

    for(u32 i = 0; i < loadCount; i++) {

        SSADefinition* load = (SSADefinition*)(baseMem + loads[i]);
        u32 loadSize = GetTypeSize(baseMem, baseMem, load->type) - 1;

        u32 observedCount = GetAddressRanges(context, &stack, load->operand0, loadSize, visitMem);
        if(SymbolicRangesOverlap(stack, observed, range, observedCount, rangeCount, &context.compiler->rangeTable)) {
            return true;
        }
        roll_back_linear_allocator(&stack, range);
    }

    return false;
}


bool IsMemoryObservableHelper(CompilerContext context, LinearAllocator stack, bool fnParamater, SymbolicRange* range, u32 rangeCount, SSAMemoryDef* currentMemory, u32* visitMem) {

    byte* baseMem = context.compiler->mem;
    u32 currentMemoryPtr = (byte*)currentMemory - baseMem;

    switch(currentMemory->type) {
    case MEMORY_DEF:
        {
            SSADefinition* store = (SSADefinition*)(baseMem + currentMemory->ssaDef);
            ASSERT(store->opr == EXPRESSION_MEMORY_STORE);
            ASSERT(currentMemory->predecessors.edgeCount < 2);
            SSABasicBlock* block = (SSABasicBlock*)(baseMem + store->block);
            if(IsMemoryUsed(context, stack, range, rangeCount, block, currentMemoryPtr, visitMem)) return true;

            for(u32 i = 0; i < currentMemory->successors.edgeCount; i++) {
                u32 succMemPtr = Mem<u32>(baseMem + currentMemory->successors.edges + i * sizeof(u32));
                SSAMemoryDef* succMem = (SSAMemoryDef*)(baseMem + succMemPtr);
                if(IsMemoryObservableHelper(context, stack, fnParamater, range, rangeCount, succMem, visitMem)) {
                    return true;
                }
            }
            return false;
        }
    case MEMORY_PHI:
        {
            SSABasicBlock* block = (SSABasicBlock*)(baseMem + currentMemory->ssaDef);
            if(IsMemoryUsed(context, stack, range, rangeCount, block, currentMemoryPtr, visitMem)) return true;

            for(u32 i = 0; i < currentMemory->successors.edgeCount; i++) {
                u32 succMemPtr = Mem<u32>(baseMem + currentMemory->successors.edges + i * sizeof(u32));
                SSAMemoryDef* succMem = (SSAMemoryDef*)(baseMem + succMemPtr);
                if(IsMemoryObservableHelper(context, stack, fnParamater, range, rangeCount, succMem, visitMem)) {
                    return true;
                }
            }
            return false;
        }
    }
}
bool IsMemoryObservable(CompilerContext context, bool fnParamater, SSAMemoryDef* memoryDef, u32* mem, u32* visitMem) {
    
    byte* const baseMem = context.compiler->mem;
    auto stack = make_linear_allocator(mem, 32*KILO_BYTE);

    auto defRange = (SymbolicRange*)(baseMem + memoryDef->addressRanges.edges);
    SymbolicRange localDefRange[memoryDef->addressRanges.edgeCount];
    for(u32 k = 0; k < memoryDef->addressRanges.edgeCount; k++) {
        localDefRange[k].lower = CpySSAChain(baseMem, &stack, defRange[k].lower);
        localDefRange[k].upper = CpySSAChain(baseMem, &stack, defRange[k].upper);
    }

    bool obserable = false;
    for(u32 i = 0; i < memoryDef->successors.edgeCount; i++) {
        u32 succMemPtr = Mem<u32>(baseMem + memoryDef->successors.edges + i * sizeof(u32));
        SSAMemoryDef* succMem = (SSAMemoryDef*)(baseMem + succMemPtr);

        obserable |= IsMemoryObservableHelper(context, stack, fnParamater, localDefRange, memoryDef->addressRanges.edgeCount, succMem, visitMem);
    }

    return obserable || (fnParamater && memoryDef->successors.edgeCount == 0);
}
bool TryRemoveStore(CompilerContext context, SSADefinition* store, u32* mem, u32* visitMem) {

    byte* const baseMem = context.compiler->mem;
    auto info = (SSAStoreInfo*)(baseMem + store->extraPtr);
    ASSERT(info->infoType == STORE_INFO);

    visitMem[0] = 0;
    bool param = IsDefParamaterDependent(context.compiler, store->operand0, visitMem);

    bool obserable = false;
    for(u32 i = 0; i < info->memoryDefCount; i++) {
        auto memoryDef = (SSAMemoryDef*)(baseMem + info->memoryDefs[i]);
        obserable |= IsMemoryObservable(context, param, memoryDef, mem, visitMem);
    }

    if(!obserable) {
        RemoveDef(context, (byte*)store - baseMem);
        for(u32 i = 0; i < info->memoryDefCount; i++) {
            RemoveMemory(context, info->memoryDefs[i]);
        }
        return true;
    }

    return false;
}
bool MemoryOpsToSSA(CompilerContext context, SSABasicBlock* entry, bool* visited, u32* mem) {

    bool ret = false;
    byte* const baseMem = context.compiler->mem;
    u32 blockCount = context.fn->maxBlockName;

    auto it = GetPointer<SSADefinition>(baseMem, entry->firstDef);
    while(it) {

        auto next = GetPointer<SSADefinition>(baseMem, it->nextDef);
        switch(it->opr) {
        case SSA_MEMORY_LOAD:
            ret |= TryRemoveLoad(context, it, blockCount, mem, (u32*)(visited+blockCount));
            break;
        case SSA_MEMORY_STORE:
            ret |= TryRemoveStore(context, it, mem, (u32*)(visited+blockCount));
            break;
        }

        it = next;
    }

    return ret | TraverseCFG(context, entry, visited, MemoryOpsToSSA, mem);
}
void SetRetBlocksFn(CompilerContext context, byte* mem, bool* traversalMem) {

    byte* const baseMem = context.compiler->mem;
    memset(traversalMem, 0, context.fn->maxBlockName);
    CFGEdges outBlocks;
    outBlocks.edgeCount = GetReturnBlocksFromFunction(baseMem, context.fn->entry, traversalMem, (u32*)mem);
    outBlocks.edges = context.compiler->miscAllocatorSSA;
    auto tmp = (u32*)AllocateSSA(context.compiler, outBlocks.edgeCount* sizeof(u32));
    memcpy(tmp, mem, outBlocks.edgeCount* sizeof(u32));
    context.fn->outBlocks = outBlocks;
}
u32 CpySSAChainToCFG(CompilerContext context, u32* name, u32 ssa, u32* blocks, u32* visited) {

    byte* const baseMem = context.compiler->mem;
    auto def = (SSADefinition*)(baseMem + ssa);

    if(visited[def->value] != ~u32(0)) return visited[def->value];
    auto cpy = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
    *cpy = *def;
    cpy->prevDef = 0;
    cpy->nextDef = 0;
    cpy->value = (*name)++;
    u32 cpyPtr = (byte*)cpy - baseMem;
    visited[def->value] = cpyPtr;

    u32 blockName = Mem<SSABasicBlock>(baseMem + def->block).name;
    u32 ptr = blocks[blockName];
    ASSERT(ptr != ~u32(0));

    switch(cpy->opr) {
    case SSA_CONSTANT:
        {
            cpy->operand0 = context.compiler->miscAllocatorSSA;
            auto v = AllocateSSA<Value>(context.compiler);
            *v = Mem<Value>(baseMem + def->operand0);
            break;
        }
    case SSA_FN_PARAMETER:
    case SSA_UN_INIT:
    case SSA_FUNCTION:
        break;
    case SSA_PHI_NODE:
        {
            auto cpyBlock = (SSABasicBlock*)(baseMem + ptr);
            AddPhiToBlock(context.compiler, cpyBlock, cpyPtr);
            cpy->operand0 = 0;
            u32 cpyOperands[def->operand0];
            u32* operands = GetPhiOperandPtr(baseMem, def);
            for(u32 i = 0; i < def->operand0; i++) {
                cpyOperands[i] = CpySSAChainToCFG(context, name, operands[i], blocks, visited);
            }
            AddPhiNodeIncoming(context, cpy, cpyOperands, def->operand0);
            return cpyPtr;
        }
    case SSA_ALLOCA:
    case SSA_COPY:
    case SSA_CONVERT:
    case SSA_MEMORY_LOAD:
    case SSA_LOGICAL_NEG:
    case SSA_BITWISE_NEG:
    case SSA_MINUS:
        cpy->operand0 = CpySSAChainToCFG(context, name, def->operand0, blocks, visited);
        break;
    case SSA_CALL:
        {
            cpy->operand0 = CpySSAChainToCFG(context, name, def->operand0, blocks, visited);
            auto args = GetCallArgs(baseMem, def);
            cpy->operand1 = context.compiler->miscAllocatorSSA;
            u32* cpyArgs = (u32*)AllocateSSA(context.compiler, (args.argCount+1)*sizeof(u32));
            for(u32 i = 0; i < args.argCount; i++) {
                cpyArgs[i] = CpySSAChainToCFG(context, name, args.ptr[i], blocks, visited);
            }
            cpyArgs[args.argCount] = 0;
            break;
        }
    case SSA_MEMORY_STORE:
    default:
        {
            cpy->operand0 = CpySSAChainToCFG(context, name, def->operand0, blocks, visited);
            cpy->operand1 = CpySSAChainToCFG(context, name, def->operand1, blocks, visited);
            break;
        }
    }

    auto cpyBlock = (SSABasicBlock*)(baseMem + ptr);

    auto defBlock = (SSABasicBlock*)(baseMem + def->block);
    auto prev = GetPointer<SSADefinition>(baseMem, def->prevDef);
    if(prev) {
        auto prevDefBlock = (SSABasicBlock*)(baseMem + prev->block);

        u32 prevCpy = visited[prev->value];
        if(prevCpy == ~u32(0)) {
            prevCpy = CpySSAChainToCFG(context, name, def->prevDef, blocks, visited);
        }
        cpy->prevDef = prevCpy;
        auto prevCpyDef = (SSADefinition*)(baseMem + prevCpy);
        prevCpyDef->nextDef = cpyPtr;
    }

    cpyBlock->values.Insert(context.compiler, cpyPtr);
    cpy->block = ptr;
    return cpyPtr;
}

void CpySSAFromCFGToCFG(CompilerContext ctx, SSABasicBlock* block, bool* visited, u32* valueNames, u32* blockPtrs, u32* ssaPtrs) {

    byte* const baseMem = ctx.compiler->mem;
    SSABasicBlock* cpy = (SSABasicBlock*)(baseMem + blockPtrs[block->name]);

    if(block->firstDef) {

        auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
        u32 itPtr = (byte*)it - baseMem;
        cpy->firstDef = CpySSAChainToCFG(ctx, valueNames, itPtr, blockPtrs, ssaPtrs);
        while(it) {
            u32 itPtr = (byte*)it - baseMem;
            cpy->lastDef = CpySSAChainToCFG(ctx, valueNames, itPtr, blockPtrs, ssaPtrs);
            it = GetPointer<SSADefinition>(baseMem, it->nextDef);
        }
    }
    cpy->nextBlock = block->nextBlock;
    switch(block->nextBlock.opr) {
    case BRANCH:
        {
            u32 value = Mem<SSADefinition>(baseMem + block->nextBlock.branch.conditionDef).value;
            if(ssaPtrs[value] == ~u32(0)) {
                ssaPtrs[value] = CpySSAChainToCFG(ctx, valueNames, block->nextBlock.branch.conditionDef, blockPtrs, ssaPtrs);
            }
            cpy->nextBlock.branch.conditionDef = ssaPtrs[value];

            auto thenName = Mem<SSABasicBlock>(baseMem + cpy->nextBlock.branch.thenBlock).name;
            cpy->nextBlock.branch.thenBlock = blockPtrs[thenName];
            auto elseName = Mem<SSABasicBlock>(baseMem + cpy->nextBlock.branch.elseBlock).name;
            cpy->nextBlock.branch.elseBlock = blockPtrs[elseName];
            break;
        }
    case RET:
        {
            if(block->nextBlock.ret.retDef != ~u32(0)) {
                u32 value = Mem<SSADefinition>(baseMem + block->nextBlock.ret.retDef).value;
                if(ssaPtrs[value] == ~u32(0)) {
                    ssaPtrs[value] = CpySSAChainToCFG(ctx, valueNames, block->nextBlock.ret.retDef, blockPtrs, ssaPtrs);
                }
                cpy->nextBlock.ret.retDef = ssaPtrs[value];
            }
            break;
        }
    case JMP:
        {
            auto targetName = Mem<SSABasicBlock>(baseMem + cpy->nextBlock.jmp.targetBlock).name;
            cpy->nextBlock.jmp.targetBlock = blockPtrs[targetName];
            break;
        }
    }

    TraverseCFG(ctx, block, visited, CpySSAFromCFGToCFG, valueNames, blockPtrs, ssaPtrs);
}
CFGEdges CpyCFG(CompilerContext context, u32* blockNames, CFGEdges cfg, u32* visited) {

    byte* const baseMem = context.compiler->mem;
    u32* blocks = (u32*)(baseMem + cfg.edges);

    CFGEdges ret{context.compiler->miscAllocatorSSA,cfg.edgeCount};
    u32* retBlocks = (u32*)AllocateSSA(context.compiler, sizeof(u32)*cfg.edgeCount);
    bool skip[cfg.edgeCount]{};

    for(u32 i = 0; i < cfg.edgeCount; i++) {

        auto block = (SSABasicBlock*)(baseMem + blocks[i]);
        if(visited[block->name] != ~u32(0)) {
            retBlocks[i] = visited[block->name];
            skip[i] = true;
            continue;
        }

        auto cpy = (SSABasicBlock*)pool_allocate(&context.compiler->ssaBlockPool);
        *cpy = {};
        cpy->hotness = 0.f;
        cpy->values.Init();
        cpy->firstDef = 0;
        cpy->lastDef = 0;
        cpy->firstMem = 0;
        cpy->lastMem = 0;
        cpy->phiCount = 0;
        cpy->memoryPhiCount = 0;
        cpy->name = (*blockNames)++;

        u32 cpyPtr = (byte*)cpy - baseMem;
        visited[block->name] = cpyPtr;
        retBlocks[i] = cpyPtr;

    }
    for(u32 i = 0; i < cfg.edgeCount; i++) {

        if(skip[i]) continue;
        auto block = (SSABasicBlock*)(baseMem + blocks[i]);
        auto cpy = (SSABasicBlock*)(baseMem + retBlocks[i]);

        CFGEdges successors = CpyCFG(context, blockNames, block->successors, visited);
        for(u32 k = 0; k < successors.edgeCount; k++) {
            u32 succBlock = Mem<u32>(baseMem + successors.edges + k * sizeof(u32));
            InsertCFGEdge(context, retBlocks[i], succBlock);
        }
    }

    return ret;
}

SSABasicBlock* MergeCF2(CompilerContext context, CFGEdges edges) {

    byte* const baseMem = context.compiler->mem;
    u32* blocks = (u32*)(baseMem + edges.edges);

    ASSERT(edges.edgeCount);
    if(edges.edgeCount == 1) {
        return (SSABasicBlock*)(baseMem + blocks[0]);
    }
    else {

        auto merge = (SSABasicBlock*)pool_allocate(&context.compiler->ssaBlockPool);
        u32 mergePtr = (byte*)merge - baseMem;
        *merge = {};
        merge->memoryPtrs.Init();
        merge->values.Init();

        for(u32 i = 0; i < edges.edgeCount; i++) {
            InsertCFGEdge(context, blocks[i], mergePtr);
        }

        CopyValues(context, merge);
        return merge;
    }

}

void InlineFunctionCall(CompilerContext context, SSADefinition* call, u32* mem, u32* visitMem) {

    auto caller = context.fn;
    byte* const baseMem = context.compiler->mem;
    auto fnDef = (SSADefinition*)(baseMem + call->operand0);
    if(fnDef->opr != SSA_FUNCTION) return;

    u32 callPtr = (byte*)call - baseMem;
    auto callBlock = (SSABasicBlock*)(baseMem + call->block);
    auto callee = (SSAFunction*)(baseMem + fnDef->operand0);

    CFGEdges calleeEntry{context.compiler->miscAllocatorSSA, 1};
    *AllocateSSA<u32>(context.compiler) = (byte*)callee->entry - baseMem;
    memset(visitMem, ~u32(0), sizeof(u32) * (callee->maxBlockName + callee->maxSSAName + caller->maxSSAName + caller->maxBlockName));
    u32 callerMaxBlockName = caller->maxBlockName;

    auto entryEdges = CpyCFG(context, &callerMaxBlockName, calleeEntry, visitMem);
    caller->maxBlockName = callerMaxBlockName;

    ASSERT(entryEdges.edgeCount == 1);
    auto entryBlock = (SSABasicBlock*)(baseMem + Mem<u32>(baseMem + entryEdges.edges));
    {
        bool vMem[callee->maxBlockName];
        memset(vMem, 0, callee->maxBlockName);
        vMem[callee->entry->name] = true;
      
        u32 callerMaxSSAName = caller->maxSSAName;
        CpySSAFromCFGToCFG(context, callee->entry, vMem, &callerMaxSSAName, visitMem, visitMem + callee->maxBlockName+1);
        caller->maxSSAName = callerMaxSSAName;
    }

    CFGEdges retBlocks{context.compiler->miscAllocatorSSA, callee->outBlocks.edgeCount};
    u32* outBlocks = (u32*)AllocateSSA(context.compiler, callee->outBlocks.edgeCount*sizeof(u32));
    for(u32 i = 0; i < callee->outBlocks.edgeCount; i++) {
        u32 retBlockPtr = Mem<u32>(baseMem + callee->outBlocks.edges + i * sizeof(u32));
        auto retBlock = (SSABasicBlock*)(baseMem + retBlockPtr);
        outBlocks[i] = visitMem[retBlock->name];
    }

    auto fnExpr = (FunctionExpr*)(baseMem + callee->funcExpr.index);
    auto merge = MergeCF2(context, retBlocks);
    merge->name = caller->maxBlockName++;

    auto it = GetPointer<SSADefinition>(baseMem, entryBlock->firstDef);
    u32 parameterCount = 0;
    while(it) {
        if(it->opr == SSA_FN_PARAMETER) {
            parameterCount++;
        }
        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }
    u32 parameters[parameterCount];
    parameterCount = 0;
    it = GetPointer<SSADefinition>(baseMem, entryBlock->firstDef);
    while(it) {

        if(it->opr == SSA_FN_PARAMETER) {
            parameters[parameterCount++] = (byte*)it - baseMem;
        }
        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }
    auto args = GetCallArgs(baseMem, call);
    for(u32 i = 0; i < parameterCount; i++) {
        memset(mem, 0, callee->maxBlockName);
        RerouteDefs(context, entryBlock, (bool*)mem, parameters[i], args.ptr[i]);
        RemoveDef(context, parameters[i]);
    }

    auto phi = AddPhiNode(context, merge, fnExpr->ret_t);
    u32 operandCount = 0;
    u32 phiOperands[retBlocks.edgeCount];
    for(u32 i = 0; i < retBlocks.edgeCount; i++) {
        u32 retBlockPtr = Mem<u32>(baseMem + retBlocks.edges + i * sizeof(u32));
        auto retBlock = (SSABasicBlock*)(baseMem + retBlockPtr);

        ASSERT(retBlock->nextBlock.opr == RET);
        if(retBlock->nextBlock.ret.retDef != ~u32(0)) {
            phiOperands[operandCount++] = retBlock->nextBlock.ret.retDef;
        }
        else {
            auto unInit = (SSADefinition*)pool_allocate(&context.compiler->ssaDefPool);
            *unInit = {};
            unInit->opr = SSA_UN_INIT;
            unInit->type = fnExpr->ret_t;
            u32 unInitPtr = (byte*)unInit - baseMem;
            unInitPtr = MakeDefWithSideEffects(context, retBlock, unInitPtr);
            phiOperands[operandCount++] = unInitPtr;
        }
    }

    for(u32 i = 0; i < retBlocks.edgeCount; i++) {
        u32 retBlockPtr = Mem<u32>(baseMem + retBlocks.edges + i * sizeof(u32));
        auto retBlock = (SSABasicBlock*)(baseMem + retBlockPtr);

        ASSERT(retBlock->nextBlock.opr == RET);
        retBlock->nextBlock.opr = JMP;
        retBlock->nextBlock.jmp.targetBlock = (byte*)merge - baseMem;
    }

    merge->nextBlock = callBlock->nextBlock;
    merge->successors = callBlock->successors;

    for(u32 i = 0; i < callBlock->successors.edgeCount; i++) {
        auto succBlockPtr = Mem<u32>(baseMem + callBlock->successors.edges + i * sizeof(u32));
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);
        for(u32 k = 0; k < succBlock->predecessors.edgeCount; k++) {
            u32 predBlockPtr = Mem<u32>(baseMem + succBlock->predecessors.edges + k * sizeof(u32));
            if(predBlockPtr == call->block) {
                Mem<u32>(baseMem + succBlock->predecessors.edges + k * sizeof(u32)) = (byte*)merge - baseMem;
            }
        }
    }

    callBlock->successors.edgeCount = 0;
    InsertCFGEdge(context, call->block, (byte*)entryBlock - baseMem);
    callBlock->nextBlock.opr = JMP;
    callBlock->nextBlock.jmp.targetBlock = (byte*)entryBlock - baseMem;

    if(call->nextDef) {

        auto callNext = (SSADefinition*)(baseMem + call->nextDef);
        if(merge->lastDef) {
            Mem<SSADefinition>(baseMem + merge->lastDef).nextDef = call->nextDef;
            merge->lastDef = call->nextDef;
        }
        else {
            ASSERT(!merge->firstDef);
            merge->firstDef = call->nextDef;
            Mem<SSADefinition>(baseMem + merge->firstDef).prevDef = 0;
        }

        auto it = GetPointer<SSADefinition>(baseMem, merge->firstDef);
        while(it) {
            it->block = (byte*)merge - baseMem;
            merge->lastDef = (byte*)it - baseMem;
            it = GetPointer<SSADefinition>(baseMem, it->nextDef);
        }
        ASSERT(merge->lastDef);
        Mem<SSADefinition>(baseMem + merge->lastDef).nextDef = 0;

        call->nextDef = 0;
        callBlock->lastDef = (byte*)call - baseMem;
    }

    callBlock->lastDef = call->prevDef;
    if(callBlock->lastDef) {
        Mem<SSADefinition>(baseMem + callBlock->lastDef).nextDef = 0;
    }
    RemoveCall(context, callPtr, visitMem);

    auto opr = (SSADefinition*)(baseMem + *phiOperands);
    AddPhiNodeIncoming(context, phi, phiOperands, operandCount);
    memset(visitMem, 0, caller->maxBlockName);
    RerouteDefs(context, callBlock, (bool*)visitMem, (byte*)call - baseMem, (byte*)phi - baseMem);
    TryRemoveTrivialPhi(context, merge, phi, (bool*)visitMem, mem);


    caller->maxBlockName = 0;
    caller->maxSSAName = 0;
    Mem<u32>(visitMem) = 0;
    RenameSSACFG(context, caller->entry, visitMem);
    caller->maxBlockName++;
    caller->maxSSAName++;

    memset(visitMem, 0, caller->maxBlockName);
    SetRetBlocksFn(context, (byte*)mem, (bool*)visitMem);

}


void GetFnCalls(CompilerContext context, SSABasicBlock* block, bool* visited, u32* result) {

    byte* const baseMem = context.compiler->mem;
    u32 callCount = result[0];

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {
        if(it->opr == SSA_CALL) {
            result[++callCount] = (byte*)it - baseMem;
        }
        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    result[0] = callCount;
    TraverseCFG(context, block, visited, GetFnCalls, result);
}



bool IsLoopInvariant(CompilerContext context, SSABasicBlock* headerBlock, u32 defPtr, bool* visited) {

    byte* const baseMem = context.compiler->mem;
    auto def = (SSADefinition*)(baseMem + defPtr);

    if( def->opr == SSA_COPY ||
        def->opr == SSA_CALL ||
        def->opr == SSA_MEMORY_LOAD ||
        def->opr == SSA_MEMORY_STORE) return false;
    if(visited[def->value]) {
        return false;
    }
    visited[def->value] = true;

    if(def->opr == SSA_PHI_NODE) {
        for(u32 i = 0; i < headerBlock->phiCount; i++) {
            u32 phiPtr = Mem<u32>(baseMem + headerBlock->phis + i * sizeof(u32));
            if(phiPtr == defPtr) {
                return false;
            }
        }
    }
    if(IsDefBinary(def)) {
        return  IsLoopInvariant(context, headerBlock, def->operand0, visited) &&
                IsLoopInvariant(context, headerBlock, def->operand1, visited);
    }
    if(IsDefUnary(def)) {
        return  IsLoopInvariant(context, headerBlock, def->operand0, visited);
    }

    return true;
}
bool HoistLoopInvariantsFromBlock(CompilerContext context, SSABasicBlock* block, bool* traversed, LoopInfo* loop) {

    byte* const baseMem = context.compiler->mem;
    bool ret = false;
    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {

        if(it->opr == SSA_PHI_NODE) continue;
        auto next = GetPointer<SSADefinition>(baseMem, it->nextDef);

        u32 itPtr = (byte*)it - baseMem;
        memset(traversed, 0, context.fn->maxSSAName);
        if(IsLoopInvariant(context, loop->headerBlock, itPtr, traversed)) {
            RemoveDefFromBlock(context, itPtr);
            PushBackDefIntoBlock(context, loop->preHeader, itPtr);
            ret = true;
        }
        it = next;
    }
    return ret;
}
bool HoistLoopInvariants(CompilerContext context, LoopInfo** loops, u32 loopsCount, u32 loopIndex, u32* mem, bool* traversalMem) {

    byte* const baseMem = context.compiler->mem;
    auto loop = loops[loopIndex];

    auto innerHeaders = (SSABasicBlock**)mem;
    Mem<u32>(traversalMem) = 1;
    Mem<u32>(traversalMem + sizeof(u32)) = (byte*)loop->exitBlock - baseMem;
    u32 headerCount = FindLoopHeaders(context, context.fn->entry, loop->headerBlock, innerHeaders, (u32*)traversalMem);

    memset(traversalMem, 0, context.fn->maxBlockName);
    for(u32 i = 1; i < headerCount; i++) {
        for(u32 k = 0; k < loopsCount; k++) {
            if(loopIndex == k) continue;
            if(loops[k]->headerBlock == innerHeaders[i]) {
                for(u32 j = 0; j < loops[k]->bodyCount; j++) {
                    traversalMem[loops[k]->bodyBlocks[j]->name] = true;
                }
            }
        }
    }

    bool ret = false;
    for(u32 i = 0; i < loop->bodyCount; i++) {
        if(!traversalMem[loop->bodyBlocks[i]->name]) {
            ret |= HoistLoopInvariantsFromBlock(context, loop->bodyBlocks[i], traversalMem + context.fn->maxBlockName, loop);
        }
    }
    return ret;
}

SSABasicBlock* MakeLoopEndBlock(CompilerContext context, SSABasicBlock* place) {

    byte* const baseMem = context.compiler->mem;
    auto exitBlock = (SSABasicBlock*)pool_allocate(&context.compiler->ssaBlockPool);
    *exitBlock = {};
    exitBlock->values.Init();
    exitBlock->memoryPtrs.Init();
    exitBlock->name = context.fn->maxBlockName++;

    u32 endBlockPtr = (byte*)exitBlock - baseMem;
    u32 placePtr = (byte*)place - baseMem;

    exitBlock->nextBlock.opr = JMP;
    exitBlock->nextBlock.jmp.targetBlock = placePtr;

    for(u32 i = 0; i < place->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + place->predecessors.edges + i * sizeof(u32));
        InsertCFGEdge(context, predBlockPtr, endBlockPtr);
        RemoveCFGEdge(context, predBlockPtr, placePtr);
    }
    InsertCFGEdge(context, endBlockPtr, placePtr);

    return exitBlock;
}

SSABasicBlock* FindFirstLoopHeader(CompilerContext ctx, SSABasicBlock* block, bool* traversed) {

    if(traversed[block->name]) {
        return nullptr;
    }
    traversed[block->name] = true;

    byte* const baseMem = ctx.compiler->mem;
    auto visitMem = (u32*)(traversed + ctx.fn->maxBlockName);

    for(u32 i = 0; i < block->predecessors.edgeCount; i++) {
        u32 nextBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
        auto nextBlock = (SSABasicBlock*)(baseMem + nextBlockPtr);

        visitMem[0] = 0;
        if(Dominator(baseMem, ctx.fn->entry, block, nextBlock, visitMem)) {
            return block;
        }
    }

    for(u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);
        auto r = FindFirstLoopHeader(ctx, succBlock, traversed);
        if(r) return r;
    }

    return nullptr;
}

bool OptLoopSimplifyPhase(CompilerContext context, byte* mem, bool* traversalMem) {

    Mem<u32>(traversalMem) = 0;
    byte* loopsBegin = (byte*)mem;
    auto loopsEnd = MakeCanonicalLoops(context, loopsBegin, (u32*)traversalMem);

    u32 loopCount = 0;
    auto loops = (LoopInfo**)loopsEnd;
    for(byte* it = loopsBegin; it < loopsEnd; ) {
        auto loop = (LoopInfo*)it;
        loops[loopCount++] = loop;
        it += sizeof(LoopInfo) + loop->bodyCount * sizeof(SSABasicBlock*);
    }

    for(u32 i = 0; i < loopCount; i++) {
        auto loop = loops[i];
        ASSERT(loop->exitBlock && loop->preHeader);
        for(bool loopChanged = true; loopChanged;) {
            loopChanged = false;
            loopChanged |= HoistLoopInvariants(context, loops, loopCount, i, (u32*)(loops+loopCount), traversalMem);
        }
    }
}
bool IsPhiOperandInit(CompilerContext ctx, SSABasicBlock* phiBlock, SSABasicBlock* operandBlock, u32* visitMem) {
    visitMem[0] = 0;
    return Dominator(ctx.compiler->mem, ctx.fn->entry, operandBlock, phiBlock, visitMem);
}

bool IsDefReachable(CompilerContext ctx, u32 from, u32 to, bool* visited) {

    if(from == to) return true;
    byte* const baseMem = ctx.compiler->mem;
    auto f = (SSADefinition*)(baseMem + from);
    auto t = (SSADefinition*)(baseMem + to);

    if(visited[f->value]) return false;
    visited[f->value] = true;

    if(IsDefBinary(f)) {
        return IsDefReachable(ctx, f->operand0, to, visited) || IsDefReachable(ctx, f->operand1, to, visited);
    }
    else if(IsDefUnary(f)) {
        return IsDefReachable(ctx, f->operand0, to, visited);
    }
    return false;
}

// ---------------------------------------- SCEV Begin --------------------------------------
u32 PartiallyCopyPhiUpdateEdge(CompilerContext ctx, LinearAllocator* mem, SSADefinition* phi, SSADefinition* update, u32* traversed) {

    byte* const baseMem = ctx.compiler->mem;
    if(phi == update) return (byte*)phi - baseMem;
    if(update->opr == SSA_PHI_NODE) return (byte*)update - baseMem;
    if(traversed[update->value] != ~u32(0)) return traversed[update->value];

    traversed[update->value] = (byte*)linear_allocator_top(mem) - baseMem;
    auto cpy = (SSADefinition*)linear_allocate(mem, sizeof(SSADefinition));
    *cpy = *update;
    cpy->block = ~u32(0);

    if(IsDefBinary(update)) {
        memset(traversed + ctx.fn->maxSSAName, 0, ctx.fn->maxSSAName);       
        if(IsDefReachable(ctx, update->operand0, (byte*)phi - baseMem, (bool*)traversed + ctx.fn->maxSSAName)) {
            cpy->operand0 = PartiallyCopyPhiUpdateEdge(ctx, mem, phi, (SSADefinition*)(baseMem + update->operand0), traversed);
            cpy->operand1 = update->operand1;
        }
        else {
            cpy->operand0 = update->operand0;
            cpy->operand1 = PartiallyCopyPhiUpdateEdge(ctx, mem, phi, (SSADefinition*)(baseMem + update->operand1), traversed);
        }
    }
    else if(IsDefUnary(update)) {
        cpy->operand0 = PartiallyCopyPhiUpdateEdge(ctx, mem, phi, (SSADefinition*)(baseMem + update->operand0), traversed);
    }

    return traversed[update->value];
}
u32 MakeSCEVUpdate(CompilerContext ctx, SSADefinition* phi, SSADefinition* update, LinearAllocator* mem, u32* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    auto minus = (SSADefinition*)linear_allocate(mem, sizeof(SSADefinition));
    *minus = {};
    minus->opr = SSA_SUB;
    minus->operand1 = (byte*)phi - baseMem;
    memset(traversalMem, ~u32(0), ctx.fn->maxSSAName);
    minus->operand0 = PartiallyCopyPhiUpdateEdge(ctx, mem, phi, update, traversalMem);

    return (byte*)minus - baseMem;
}

u32 GetSCEV(CompilerContext ctx, u32 defPtr, LinearAllocator* mem, u32* seen, LoopInfo** loops, u32 loopCount) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + defPtr);

    if(seen[def->value] != ~u32(0)) {
        return seen[def->value];
    }

    u32 ret = (byte*)linear_allocator_top(mem) - baseMem;;
    if(IsDefBinary(def)) {
        auto binOp = (SSADefinition*)linear_allocate(mem, sizeof(SSADefinition));
        *binOp = *def;
        binOp->operand0 = GetSCEV(ctx, def->operand0, mem, seen, loops, loopCount);
        binOp->operand1 = GetSCEV(ctx, def->operand1, mem, seen, loops, loopCount);
    }
    else if(IsDefUnary(def)) {
        ret = (byte*)linear_allocator_top(mem) - baseMem;
        auto unaryOp = (SSADefinition*)linear_allocate(mem, sizeof(SSADefinition));
        *unaryOp = *def;
        unaryOp->operand0 = GetSCEV(ctx, def->operand0, mem, seen, loops, loopCount);
    }
    else if(def->opr == SSA_PHI_NODE) {
        
        auto scev = (SSADefinition*)linear_allocate(mem, sizeof(SSADefinition));
        scev = {};
        scev->opr = SCEV_INFO;
        scev->block = def->block;
        scev->operand0 = (byte*)linear_allocator_top(mem) - baseMem;

        auto rec = (ScalarRecurrence*)linear_allocate(mem, sizeof(ScalarRecurrence));
        *rec = {};

        auto phiBlock = (SSABasicBlock*)(baseMem + def->block);
        for(u32 i = 0; i < loopCount; i++) {
            if(loops[i]->headerBlock == phiBlock) {
                rec->loop = loops[i] - loops[0];
                break;
            }
        }

        u32* visited = (u32*)(seen + ctx.fn->maxSSAName);
        visited[0] = 0;
        bool cycle = DoesPhiFormCycle(baseMem, defPtr, visited);

        u32 count = GetPhiOperandCount(def);
        u32* ops = GetPhiOperandPtr(baseMem, def);

        if(cycle) {
            u32 init;
            for(u32 i = 0; i < count; i++) {
                auto operandBlock = (SSABasicBlock*)(baseMem + Mem<SSADefinition>(baseMem + ops[i]).block);
                if(IsPhiOperandInit(ctx, phiBlock, operandBlock, (u32*)linear_allocator_top(mem))) {
                    rec->begin = GetSCEV(ctx, ops[i], mem, seen, loops, loopCount);
                    init = i;
                    break;
                }
            }
            for(u32 i = 0; i < count; i++) {
                if(i == init) continue;
                rec->opr = SSA_ADD;
                rec->step = MakeSCEVUpdate(ctx, def, (SSADefinition*)(baseMem + ops[i]), mem, (seen + ctx.fn->maxSSAName));
            }
        }
        else {
            // TODO() branch phi - merge scevs
        }
    }
    else {
        auto scev = (SSADefinition*)linear_allocate(mem, sizeof(SSADefinition));
        *scev = *def;
    }

    seen[def->value] = ret;
    return ret;
}



u32 SCEVrule0(CompilerContext ctx, u32 scev, LinearAllocator* alloc, byte* loops, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + scev);

    if(def->opr == SSA_ADD) {

        auto op0 = (SSADefinition*)(baseMem + def->operand0);
        auto op1 = (SSADefinition*)(baseMem + def->operand1);

        if(op0->opr == SCEV_INFO && op1->opr != SCEV_INFO) {
            auto rec = (ScalarRecurrence*)(baseMem + op0->operand0);
            if(rec->opr == SSA_ADD) {

                auto loopInfo = (LoopInfo*)(loops + rec->loop);
                memset(traversalMem, 0, ctx.fn->maxSSAName);
                if(IsLoopInvariant(ctx, loopInfo->headerBlock, def->operand1, traversalMem)) {
                    def->operand0 = rec->begin;
                    rec->begin = scev;
                    return (byte*)op0 - baseMem;
                }
            }
        }
        else if(op1->opr == SCEV_INFO && op0->opr != SCEV_INFO) {

            auto rec = (ScalarRecurrence*)(baseMem + op0->operand1);
            if(rec->opr == SSA_ADD) {

                auto loopInfo = (LoopInfo*)(loops + rec->loop);
                memset(traversalMem, 0, ctx.fn->maxSSAName);
                if(IsLoopInvariant(ctx, loopInfo->headerBlock, def->operand0, traversalMem)) {
                    def->operand1 = rec->begin;
                    rec->begin = scev;
                    return (byte*)op1 - baseMem;
                }
            }
        }
    }
    return scev;
}

u32 SCEVrule1n2Helper(CompilerContext ctx, SSADefinition* mul, LinearAllocator* alloc, byte* loops, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;

    auto op0 = (SSADefinition*)(baseMem + mul->operand0);
    auto op1 = (SSADefinition*)(baseMem + mul->operand1);

    bool cond = (op0->opr == SCEV_INFO && op1->opr != SCEV_INFO) || (op1->opr == SCEV_INFO && op0->opr != SCEV_INFO);
    if(cond) {

        auto scev = op0->opr == SCEV_INFO ? op0 : op1;
        auto rec = (ScalarRecurrence*)(baseMem + scev->operand0);

        if(rec->opr != SSA_ADD && rec->opr != SSA_MUL) return (byte*)mul - baseMem;

        auto E = op0->opr != SCEV_INFO ? op0 : op1;
        auto loopInfo = (LoopInfo*)(loops+ rec->loop);
        memset(traversalMem, 0, ctx.fn->maxSSAName);
        if(IsLoopInvariant(ctx, loopInfo->headerBlock, (byte*)E - baseMem, traversalMem)) {          

            if(rec->opr == SSA_ADD) {
                auto mul1 = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *mul1 = *mul;

                mul1->operand0 = rec->step;
                mul1->operand1 = (byte*)E - baseMem;
                rec->step = (byte*)mul1 - baseMem;
            }

            mul->operand0 = (byte*)E - baseMem;
            mul->operand1 = rec->begin;
            rec->begin = (byte*)mul - baseMem;

            return (byte*)scev - baseMem;
        }
    }

    return (byte*)mul - baseMem;
}
u32 SCEVrule1n2(CompilerContext ctx, u32 scev, LinearAllocator* alloc, byte* loops, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + scev);

    if(def->opr == SSA_MUL) {
        return SCEVrule1n2Helper(ctx, def, alloc, loops, traversalMem);
    }
    return scev;
}
u32 SCEVrule3n4(CompilerContext ctx, u32 scev, LinearAllocator* alloc, byte* loops, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + scev);

    if(def->opr == SSA_POWER) {
        
        auto op0 = (SSADefinition*)(baseMem + def->operand0);
        auto op1 = (SSADefinition*)(baseMem + def->operand1);

        if(op1->opr == SCEV_INFO) {

            auto rec = (ScalarRecurrence*)(baseMem + op1->operand0);
            if(rec->opr != SSA_ADD) return scev;

            auto loopInfo = (LoopInfo*)(loops + rec->loop);
            memset(traversalMem, 0, ctx.fn->maxSSAName);
            if(IsLoopInvariant(ctx, loopInfo->headerBlock, def->operand0, traversalMem)) {

                auto power = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *power = *def;
                power->operand0 = rec->step;
                power->operand1 = def->operand0;
                rec->step = (byte*)power - baseMem;

                rec->opr == SSA_MUL;
                def->operand1 = rec->begin;
                rec->begin = scev;
                return (byte*)op1 - baseMem;
            }
        }
        else if(op0->opr == SCEV_INFO) {

            auto rec = (ScalarRecurrence*)(baseMem + op0->operand0);
            if(rec->opr != SSA_MUL) return scev;

            auto loopInfo = (LoopInfo*)(loops + rec->loop);
            memset(traversalMem, 0, ctx.fn->maxSSAName);
            if(IsLoopInvariant(ctx, loopInfo->headerBlock, def->operand1, traversalMem)) {

                auto power = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *power = *def;
                power->operand0 = rec->step;
                power->operand1 = def->operand1;
                rec->step = (byte*)power - baseMem;

                def->operand0 = rec->begin;
                rec->begin = scev;
                return (byte*)op0 - baseMem;
            }
        }
    }
    return scev;
}
u32 SCEVrule5(CompilerContext ctx, u32 scev, LinearAllocator* alloc, byte* loops, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + scev);

    if(def->opr == SSA_ADD) {

        auto op0 = (SSADefinition*)(baseMem + def->operand0);
        auto op1 = (SSADefinition*)(baseMem + def->operand1);

        bool cond = op0->opr == SCEV_INFO && op1->opr == SCEV_INFO;
        if(cond) {

            auto rec0 = (ScalarRecurrence*)(baseMem + op0->operand0);
            auto rec1 = (ScalarRecurrence*)(baseMem + op0->operand0);

            if(rec0->loop == rec1->loop && rec0->opr == SSA_ADD && rec1->opr == SSA_ADD) {

                auto add = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *add = *def;
                add->operand0 = rec0->step;
                add->operand0 = rec1->step;

                rec0->step = (byte*)add - baseMem;

                def->operand0 = rec0->begin;
                def->operand1 = rec1->begin;

                rec0->begin = scev;

                return (byte*)op0 - baseMem;
            }
        }
    }
    return scev;
}

u32 SCEVrule6n7(CompilerContext ctx, u32 scev, LinearAllocator* alloc, byte* loops, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + scev);

    if(def->opr == SSA_MUL) {

        auto op0 = (SSADefinition*)(baseMem + def->operand0);
        auto op1 = (SSADefinition*)(baseMem + def->operand1);

        if(op0->opr == SCEV_INFO && op1->opr == SCEV_INFO) {

            auto rec0 = (ScalarRecurrence*)(baseMem + op0->operand0);
            auto rec1 = (ScalarRecurrence*)(baseMem + op1->operand0);

            if(rec0->loop == rec1->loop && rec0->opr == SSA_ADD && rec1->opr == SSA_ADD) {

                u32 begin0 = rec0->begin;
                u32 begin1 = rec1->begin;

                u32 step0 = rec0->step;
                u32 step1 = rec1->step;

                auto op2 = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *op2 = *op0;
                auto rec2 = (ScalarRecurrence*)linear_allocate(alloc, sizeof(ScalarRecurrence));
                op2->operand0 = (byte*)rec2 - baseMem;

                rec2->opr = SSA_ADD;
                rec2->loop = rec0->loop;
                rec2->begin = scev;

                def->operand0 = begin0;
                def->operand1 = begin1;

                auto rec0MulStep1 = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *rec0MulStep1 = *def;
                rec0MulStep1->opr = SSA_MUL;
                rec0MulStep1->operand0 = (byte*)op0 - baseMem;
                rec0MulStep1->operand1 = step1;

                auto rec1MulStep0 = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *rec1MulStep0 = *def;
                rec1MulStep0->opr = SSA_MUL;
                rec1MulStep0->operand0 = (byte*)op1 - baseMem;
                rec1MulStep0->operand1 = step0;


                auto step0MulStep1 = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *step0MulStep1 = *def;
                step0MulStep1->opr = SSA_MUL;
                step0MulStep1->operand0 = step0;
                step0MulStep1->operand1 = step1;

                auto add0 = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *add0 = *def;
                add0->opr == SSA_ADD;
                add0->operand0 = (byte*)rec0MulStep1 - baseMem;
                add0->operand1 = (byte*)rec1MulStep0 - baseMem;

                auto add1 = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *add1 = *def;
                add1->opr == SSA_ADD;
                add1->operand0 = (byte*)add0 - baseMem;
                add1->operand1 = (byte*)step0MulStep1 - baseMem;

                rec2->step = (byte*)add1 - baseMem;
                return (byte*)op2 - baseMem;
            }
            else if(rec0->loop == rec1->loop && rec0->opr == SSA_MUL && rec1->opr == SSA_MUL) {

                def->operand0 = rec0->begin;
                def->operand1 = rec1->begin;

                rec0->begin = scev;

                auto mul = (SSADefinition*)linear_allocate(alloc, sizeof(SSADefinition));
                *mul = *def;
                mul->operand0 = rec0->step;
                mul->operand1 = rec1->step;

                rec0->step = (byte*)mul - baseMem;

                return (byte*)op0 - baseMem;
            }
        }
    }

    return scev;
}
u32 SCEVrule11n12n13(CompilerContext ctx, u32 scev, LinearAllocator* alloc, byte* loops, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + scev);

    if(def->opr == SCEV_INFO) {

        auto rec = (ScalarRecurrence*)(baseMem + def->operand0);
        if(rec->opr == SSA_ADD) {
            auto step = (SSADefinition*)(baseMem + rec->step);
            if(step->opr == SSA_CONSTANT) {
                if(IsImmZero(Mem<Value>(baseMem + step->operand0))) {
                    return rec->begin;
                }
            }
        }
        else if(rec->opr == SSA_MUL) {

            auto begin = (SSADefinition*)(baseMem + rec->begin);
            auto step = (SSADefinition*)(baseMem + rec->step);

            bool c0 = begin->opr == SSA_CONSTANT;
            bool c1 = step->opr == SSA_CONSTANT;

            if(!c0 && c1) {
                if(IsImmOne(Mem<Value>(baseMem + step->operand0))) {
                    return rec->begin;
                }
            }
            if(c0 && !c1) {
                if(IsImmZero(Mem<Value>(baseMem + begin->operand0))) {
                    return rec->begin;
                }
            }
        }
    }
}
u32 SCEVConstFold(CompilerContext ctx, u32 scev, LinearAllocator* alloc, byte* loops, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + scev);

    if(IsDefBinary(def)) {

        def->operand0 = SCEVConstFold(ctx, def->operand0, alloc, nullptr, traversalMem);
        def->operand1 = SCEVConstFold(ctx, def->operand1, alloc, nullptr, traversalMem);
    }
    else if(IsDefUnary(def)) {
        def->operand0 = SCEVConstFold(ctx, def->operand0, alloc, nullptr, traversalMem);
    }
    
    if(IsValueConstRecursive(baseMem, scev)) {

        auto v = (Value*)linear_allocate(alloc, sizeof(Value));
        *v = EvalConstDef(baseMem, def, (byte*)traversalMem);

        def->opr = SSA_CONSTANT;
        def->operand0 = (byte*)v - baseMem;
    }
    return scev;
}

typedef u32 SCEVRewrite_t(CompilerContext ctx, u32 scev, LinearAllocator* alloc, byte* loops, bool* traversalMem);
u32 FoldSCEV(CompilerContext ctx, u32 scev, LinearAllocator* alloc, byte* loops, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + scev);

    SCEVRewrite_t* rewrites[] = {
        SCEVrule0, SCEVrule1n2, SCEVrule3n4, SCEVrule5, SCEVrule6n7, SCEVConstFold,
    };

    auto rewritten = scev;
    do {
        scev = rewritten;
        for(auto it = rewrites; it < 1[&rewrites]; it++) {
            rewritten = (*it)(ctx, rewritten, alloc, loops, traversalMem);
        }
    } while(rewritten != scev);
    return scev;
}

u32 SimplifySCEV(CompilerContext ctx, u32 scev, LinearAllocator* alloc, byte* loops, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + scev);
    
    if(IsDefBinary(def)) {
        def->operand0 = SimplifySCEV(ctx, def->operand0, alloc, loops, traversalMem);
        def->operand1 = SimplifySCEV(ctx, def->operand1, alloc, loops, traversalMem);
    }
    if(IsDefUnary(def)) {
        def->operand0 = SimplifySCEV(ctx, def->operand0, alloc, loops, traversalMem);
    }
    else if(def->opr == SCEV_INFO) {
        auto rec = (ScalarRecurrence*)(baseMem + def->operand0);
        rec->begin = SimplifySCEV(ctx, rec->begin, alloc, loops, traversalMem);
        rec->step = SimplifySCEV(ctx, rec->step, alloc, loops, traversalMem);
    }

    return FoldSCEV(ctx, scev, alloc, loops, traversalMem);
}

//--------------------------------- SCEV END --------------------------------

bool IsOprAssociative(u32 opr) {    
    return  opr == SSA_ADD ||
            opr == SSA_MUL ||
            opr == SSA_BITWISE_OR ||
            opr == SSA_LOGICAL_OR ||
            opr == SSA_BITWISE_AND ||
            opr == SSA_LOGICAL_AND;
            opr == SSA_BITWISE_XOR;
}

u32 GetSSAChainOps(CompilerContext ctx, u32 opr, u32 ssaChain, u32* result, u32* seen) {

    byte* const baseMem = ctx.compiler->mem;
    u32 ret = 0;
    auto ssa = (SSADefinition*)(baseMem + ssaChain);

    if(seen[ssa->value] != ~u32(0)) {
        return seen[ssa->value];
    }
    seen[ssa->value] = ssaChain;

    if(ssa->opr == (u32)opr) {
        result[ret++] = ssaChain;
        ret += GetSSAChainOps(ctx, opr, ssa->operand0, result+ret, seen);
        ret += GetSSAChainOps(ctx, opr, ssa->operand1, result+ret, seen);
    }

    return ret;
}


u32 ReassociateSSAChain2(CompilerContext ctx, u32 chain, byte* mem) {

    byte* const baseMem = ctx.compiler->mem;
    auto ssaRoot = (SSADefinition*)(baseMem + chain);

    u32* ops = (u32*)mem;
    u32* seen = ops + ctx.fn->maxSSAName;
    memset(seen, ~u32(0), sizeof(u32) * ctx.fn->maxSSAName);
    u32 opsCount = GetSSAChainOps(ctx, ssaRoot->opr, chain, ops, seen);

    for(u32 i = 0; i < opsCount; i++) {

        auto op = (SSADefinition*)(baseMem + ops[i]);
        if(op == ssaRoot) continue;
        auto opBlock = (SSABasicBlock*)(baseMem + op->block);
    }
}

struct SelectOps {
    u32 cond;
    u32 op0;
    u32 op1;
};
SelectOps GetSelectOps(CompilerContext ctx, SSADefinition* select) {
    ASSERT(select->opr == SSA_SELECT);
    u32* ops = (u32*)(ctx.compiler->mem + select->operand1);
    return {select->operand0, ops[0], ops[1]};
}

u32 ReassociateSSAChain(CompilerContext ctx, u32 chain, bool* mem) {

    byte* const baseMem = ctx.compiler->mem;
    auto it = (SSADefinition*)(baseMem + chain);

    u32* seen = (u32*)(mem);
    u32* ops = (u32*)(seen + ctx.fn->maxSSAName);
    memset(seen, ~u32(0), ctx.fn->maxSSAName * sizeof(u32));
    u32 opsCount = GetSSAChainOps(ctx, it->opr, chain, ops, seen);
    memset(seen, ~u32(0), ctx.fn->maxSSAName * sizeof(u32));

    for(u32 i = 0; i < opsCount; i++) {
        auto op = (SSADefinition*)(baseMem + ops[i]);
        auto opBlock = (SSABasicBlock*)(baseMem + op->block);

        if(op == it) continue;

        bool* t = (bool*)(ops + opsCount);
        u32* uses = (u32*)(t + ctx.fn->maxSSAName);
        memset(t, 0, ctx.fn->maxSSAName);
        u32 useCount = GetValUses(ctx, opBlock, t, ops[i], uses);

        bool ext = false;
        for(u32 k = 0; k < useCount; k++) {
            bool f = false;
            for(u32 j = 0; j < opsCount; j++) {
                if(uses[k] == ops[j]) {
                    f = true;
                    break;
                }
            }
            if(!f) {
                ext = true;
                break;
            }
        }

        if(ext) {
            seen[op->value] = true;
        }
    }

    opsCount = GetSSAChainOps(ctx, it->opr, chain, ops, seen);
    u32* leafs = (u32*)(ops + opsCount);
    u32 leafCount = 0;

    for(u32 i = 0; i < opsCount; i++) {
        auto op = (SSADefinition*)(baseMem + ops[i]);
        auto operand0 = (SSADefinition*)(baseMem + op->operand0);
        auto operand1 = (SSADefinition*)(baseMem + op->operand1);

        if(operand0->opr != op->opr) {
            leafs[leafCount++] = op->operand0;
        }
        if(operand1->opr != op->opr) {
            leafs[leafCount++] = op->operand1;
        }
    }

    u64 hashes[leafCount];
    i64 maxLeafName = -1;
    u32 lastLeaf = 0;

    for(u32 i = 0; i < leafCount; i++) {
        auto leaf = (SSADefinition*)(baseMem + leafs[i]);
        if(leaf->value > maxLeafName) {
            maxLeafName = leaf->value;
            lastLeaf = leafs[i];
        }

        hashes[i] = HashSSA(ctx.compiler, leafs[i]);
    }
    Qsort(hashes, 0, Max((i32)0, (i32)leafCount-1) );

    auto lastBlock = (SSABasicBlock*)(baseMem + Mem<SSADefinition>(baseMem + lastLeaf).block);
    u32 last = leafs[0];
    for(u32 i = 1; i < leafCount+1; i++) {
        if(!opsCount) break;
        u32 opPtr = ops[--opsCount];
        auto op = (SSADefinition*)(baseMem + opPtr);

        ASSERT(lastLeaf);
        RemoveDefFromBlock(ctx, opPtr);
        InsertAfterDef(ctx.compiler, lastBlock, lastLeaf, opPtr);

        op->operand0 = last;
        op->operand1 = leafs[i];
        last = opPtr;
    }

    return last;
}



void ReassociateSSAChains(CompilerContext ctx, SSABasicBlock* block, bool* traversed) {

    byte* const baseMem = ctx.compiler->mem;

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {
        auto next = GetPointer<SSADefinition>(baseMem, it->nextDef);
        if(IsOprCommutative(it->opr) && IsOprAssociative(it->opr) && IsIntegral(ctx.compiler->mem, it->type)) {

            u32 itPtr = (byte*)it - baseMem;
            u32 reassociated = ReassociateSSAChain(ctx, itPtr, traversed + ctx.fn->maxBlockName);
            if(reassociated != itPtr) {
                memset(traversed + ctx.fn->maxBlockName, 0, ctx.fn->maxBlockName);
                RerouteDefs(ctx, block, traversed + ctx.fn->maxBlockName, itPtr, reassociated);
            }
        }
        it = next;
    }

    TraverseCFG(ctx, block, traversed, ReassociateSSAChains);
}
void PrintSSAFunctionCFG(Compiler *compiler, SSABasicBlock* block, u32* visited);
bool OptSimplifyPhase(CompilerContext context, byte *mem, bool* traversalMem) {

    auto fn = context.fn;
    memset(traversalMem, 0, fn->maxBlockName*sizeof(u32));
    fn->maxBlockName = EnumaretBlocks(context.compiler, fn->entry, fn->maxBlockName, (u32*)traversalMem)+1;
    u32 blocks[context.fn->maxBlockName];
    memcpy(blocks, traversalMem, fn->maxBlockName*sizeof(u32));

    u32 i = 0;
    for (bool running = true; running;) {

        running = false;
        global_print("%s%i%\n", "simplifying iteration ", i++);

        memset(traversalMem, 0, fn->maxBlockName);
        running |= RemoveUneccesserayPhis(context, fn->entry, traversalMem, (u32*)mem);
        memset(traversalMem, 0, fn->maxBlockName);
        running |= FoldConstants(context, fn->entry, traversalMem);
        memset(traversalMem, 0, fn->maxBlockName);
        running |= RewriteSSAChains(context, fn->entry, traversalMem);
        fn->entry->values.Clear();
        memset(traversalMem, 0, fn->maxBlockName);
        running |= CommonSubExprElim(context, fn->entry, traversalMem);

        /*
        memset(traversalMem, 0, fn->maxBlockName);
        ReassociateSSAChains(context, fn->entry, traversalMem);
        memset(traversalMem, 0, context.fn->maxBlockName);
        running |= BalanceSSAChains(context, context.fn->entry, traversalMem);
        memset(traversalMem, 0, fn->maxBlockName);
        running |= MemoryOpsToSSA(context, fn->entry, traversalMem, (u32*)mem);
        */

        memset(traversalMem, 0, context.fn->maxBlockName);
        u32 branchCount = FindBranches(context, fn->entry, (BranchAnalysis *)mem, traversalMem);
        BranchAnalysis branches[branchCount];
        memcpy(branches, mem, branchCount * sizeof(BranchAnalysis));
        for (u32 i = 0; i < branchCount; i++) {
            bool inlined = InlineConstBranches(context, branches[i], (u32*)mem);
            running |= inlined;
            /*
            if(!inlined) {
                memset(traversalMem, 0, context.fn->maxBlockName);
                running |= HoistBranchInvariants(context, branches[i], (u32*)mem, (u32*)traversalMem);
            }
            */
        }

        memset(traversalMem, 0, fn->maxBlockName);
        running |= EliminateSuperflousControlFlow(context, fn->entry, traversalMem);
        running |= CollectUnusedValuesAndBlocks(context, fn->entry, (u32*)traversalMem, blocks); // gc
    }
}



u32 ApproximateFunctionComplexity(CompilerContext context, SSABasicBlock* block, bool* traversalMem) {

    u32 ret = 1;
    byte* const baseMem = context.compiler->mem;
    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {
        ret++;
        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    return ret + TraverseCFG(context, block, traversalMem, ApproximateFunctionComplexity);
}

struct InlineHeuristics {
    f32 constan0;
};

bool IsFunctionReachable(CallGraph graph, byte* graphBase, u32 fn, u32 currentFn, bool* traversed) {

    if(fn == currentFn) return true;
    if(traversed[currentFn]) return false;
    traversed[currentFn] = true;

    auto c = graph.vertices + currentFn;
    for(u32 i = 0; i < c->calls.edgeCount; i++) {
        auto v = (CallEdge*)(graphBase + c->calls.edges + i * sizeof(CallEdge));
        if(IsFunctionReachable(graph, graphBase, fn, v->callee, traversed)) return true;
    }

    return false;
}
bool IsCallRecursive(CallGraph graph, byte* graphBase, CallEdge edge, bool* traversed) {

    memset(traversed, 0, graph.vertexCount);
    return IsFunctionReachable(graph, graphBase, edge.caller, edge.callee, traversed);
}
bool IsFnRecursive(CallGraph graph, byte* graphBase, u32 fn, bool* traversed) {

    auto calls = graph.vertices[fn].calls;
    for(u32 i = 0; i < calls.edgeCount; i++) {
        auto e = (CallEdge*)(graphBase + calls.edges + i * sizeof(CallEdge));
        memset(traversed, 0, graph.vertexCount);
        if(IsFunctionReachable(graph, graphBase, fn, e->callee, traversed)) return true;
    }

    return false;
}

f32 InlineHeuristic(CompilerContext context, CallGraph* graph, byte* graphBase, CallEdge edge, InlineHeuristics heuristic, bool* mem) {

    byte* const baseMem = context.compiler->mem;
    auto called = (SSADefinition*)(baseMem + edge.call->operand0);
    if(called->opr != SSA_FUNCTION) {
        return 0;
    }

    auto fn = (SSAFunction*)(baseMem + called->operand0);
    f32 score = 0;

    CompilerContext ctx{context.compiler, fn};
    memset(mem, 0, fn->maxBlockName);
    u32 complexity = ApproximateFunctionComplexity(ctx, fn->entry, mem);
    f32 callSiteHotness = Mem<SSABasicBlock>(baseMem + edge.call->block).hotness;

    bool recursive = IsFnRecursive(*graph, graphBase, edge.callee, mem);

    graph->vertices[edge.caller];
    score = heuristic.constan0 / complexity;
    score *= callSiteHotness;
    score *= recursive ? 0.25f : 1.f;
    return score;
}

void InsertCallEdge(CallGraph* graph, LocalMallocState* mallocState, u32 caller, u32 callee, SSADefinition* call) {

    u32 max = Max(caller, callee)+1;
    ASSERT(graph->vertexCount >= max);
    byte* localHeapBase = get_local_malloc_base(*mallocState);

    {
        u32 callCount = graph->vertices[caller].calls.edgeCount;
        auto callTmp = (CallEdge*)local_malloc(mallocState, (callCount+1) * sizeof(CallEdge));
        auto calls = GetPointer<CallEdge>(localHeapBase, graph->vertices[caller].calls.edges);


        memcpy(callTmp, calls, callCount * sizeof(CallEdge));
        callTmp[callCount].call = call;
        callTmp[callCount].caller = caller;
        callTmp[callCount].callee = callee;

        local_free(mallocState, calls);
        graph->vertices[caller].calls.edges = (byte*)callTmp - localHeapBase;
        graph->vertices[caller].calls.edgeCount++;
    }

    {
        u32 callerCount = graph->vertices[callee].callers.edgeCount;
        auto callerTmp = (CallEdge*)local_malloc(mallocState, (callerCount+1) * sizeof(CallEdge));
        auto callerCalls = GetPointer<CallEdge>(localHeapBase, graph->vertices[callee].callers.edges);

        memcpy(callerTmp, callerCalls, callerCount * sizeof(CallEdge));
        callerTmp[callerCount].call = call;
        callerTmp[callerCount].caller = caller;
        callerTmp[callerCount].callee = callee;
    
        local_free(mallocState, callerCalls);
        graph->vertices[callee].callers.edges = (byte*)callerTmp - localHeapBase;
        graph->vertices[callee].callers.edgeCount++;
    }    
}
void RemoveCallEdge(CallGraph* graph, LocalMallocState* mallocState, CallEdge edge) {

    byte* localHeapBase = get_local_malloc_base(*mallocState);
    u32 size = graph->vertices[edge.callee].callers.edgeCount;
    for(u32 i = 0; i < size; i++) {
        auto e = (CallEdge*)(localHeapBase + graph->vertices[edge.callee].callers.edges + i * sizeof(CallEdge));
        if(e->call == edge.call) {
            auto last = (CallEdge*)(localHeapBase + graph->vertices[edge.callee].callers.edges + (size-1) * sizeof(CallEdge));
            *e = *last;
            graph->vertices[edge.callee].callers.edgeCount--;
            break;
        }
    }

    size = graph->vertices[edge.caller].calls.edgeCount;
    for(u32 i = 0; i < size; i++) {
        auto e = (CallEdge*)(localHeapBase + graph->vertices[edge.caller].calls.edges + i * sizeof(CallEdge));
        if(e->call == edge.call) {
            auto last = (CallEdge*)(localHeapBase + graph->vertices[edge.caller].calls.edges + (size-1) * sizeof(CallEdge));
            *e = *last;
            graph->vertices[edge.caller].calls.edgeCount--;
            break;
        }
    }
}


struct CallScore {
    CallEdge edge;
    f32 score;
};
CallScore BestInlineDecision(Compiler* compiler, byte* graphBase, CallGraph* graph, CallVertex* fn, bool* traversed) {
    
    u32 v = fn - graph->vertices;
    if(traversed[v]) {
        return {{nullptr, ~u32(0)}, 0.f};
    }
    traversed[v] = true;

    byte* const baseMem = compiler->mem;
    CallScore ret{{nullptr, ~u32(0)}, 0.f};

    for(u32 i = 0; i < fn->calls.edgeCount; i++) {

        auto edge = (CallEdge*)(graphBase + fn->calls.edges + i * sizeof(CallEdge));
        auto called = (SSAFunction*)(baseMem + graph->vertices[edge->callee].fn);
        auto call = edge->call;

        CompilerContext ctx{compiler, called};
        auto callScore = BestInlineDecision(compiler, graphBase, graph, graph->vertices + edge->callee, traversed);
        f32 score = InlineHeuristic(ctx, graph, graphBase, *edge, {10.f}, traversed + graph->vertexCount);

        if(callScore.score < score) {
            callScore.edge.call = edge->call;
            callScore.edge.caller = v;
            callScore.edge.callee;
            callScore.score = score;
        }
        if(callScore.score > ret.score) {
            ret = callScore;
        }
    }

    return ret;
}

CallGraph BuildCallGraph(Compiler* compiler, LocalMallocState* localHeap, SSAProgram* program, byte* mem, bool* traversalMem) {

    byte* localHeapBase = get_local_malloc_base(*localHeap);
    CallGraph graph{};
    graph.vertexCount = program->functionCount;
    graph.vertices = (CallVertex*)local_malloc(localHeap, graph.vertexCount * sizeof(CallVertex));
    memset(graph.vertices, 0, graph.vertexCount * sizeof(CallVertex));

    byte* const baseMem = compiler->mem;

    for(u32 i = 0; i < program->functionCount; i++) {

        graph.vertices[i].fn = (byte*)program->functions[i] - baseMem;
        auto fn = program->functions[i];

        Mem<u32>(mem) = 0;
        CompilerContext ctx{compiler, fn};
        memset(traversalMem, 0, fn->maxBlockName);
        GetFnCalls(ctx, program->functions[i]->entry, traversalMem, (u32*)mem);
        u32 callCount = Mem<u32>(mem);

        for(u32 k = 0; k < callCount; k++) {
            u32 callPtr = ((u32*)mem)[k+1];
            auto call = (SSADefinition*)(baseMem + callPtr);

            auto fnAddress = (SSADefinition*)(baseMem + call->operand0);
            auto add = baseMem + fnAddress->operand0;
            for(u32 j = 0; j < program->functionCount; j++) {
                if((byte*)program->functions[j] == add) {
                    InsertCallEdge(&graph, localHeap, i, j, call);
                    break;
                }
            }
        }
    }

    return graph;
}

CallEdge SelectInlineDecision(Compiler* compiler, byte* graphBase, CallGraph* graph, bool* traversalMem) {

    memset(traversalMem, 0, graph->vertexCount);
    CallScore decision{{nullptr, ~u32(0)}, 0.f};
    for(u32 i = 0; i < graph->vertexCount; i++) {
        if(traversalMem[i]) {
            continue;
        }
        auto k = BestInlineDecision(compiler, graphBase, graph, graph->vertices + i, traversalMem);
        if(k.score > decision.score) {
            decision = k;
        }
    }

    return decision.edge;
}
void PropogateHotness(CompilerContext ctx, SSABasicBlock* block, bool* traversed, LoopInfo** loops, u32 loopCount) {

    byte* const baseMem = ctx.compiler->mem;
    for(u32 i = 0; i < loopCount; i++) {
        auto loop = loops[i];

        if(loop->headerBlock == block) {
            block->hotness = loop->preHeader->hotness * 10.f;
            TraverseCFG(ctx, block, traversed, PropogateHotness, loops, loopCount);
            return;
        }
        if(loop->exitBlock == block) {
            block->hotness = loop->preHeader->hotness;
            TraverseCFG(ctx, block, traversed, PropogateHotness, loops, loopCount);
            return;
        }
    }

    f32 hotness = block->predecessors.edgeCount ? 0.f : 1.f;
    for(u32 i = 0; i < block->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
        auto predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);

        if(predBlock->successors.edgeCount == 2) {
            u32 predBlockSuccPtr0 = Mem<u32>(baseMem + predBlock->successors.edges + sizeof(u32)*0);
            u32 predBlockSuccPtr1 = Mem<u32>(baseMem + predBlock->successors.edges + sizeof(u32)*1);
            auto predBlockSucc0 = (SSABasicBlock*)(baseMem + predBlockSuccPtr0);
            auto predBlockSucc1 = (SSABasicBlock*)(baseMem + predBlockSuccPtr1);

            bool loopBlock = false;
            for(u32 k = 0; k < loopCount; k++) {
                auto loop = loops[k];
                if(loop->exitBlock == predBlockSucc0 || loop->exitBlock == predBlockSucc1) {
                    loopBlock = true;
                    break;
                }
            }
            if(loopBlock) {
                ASSERT(block->predecessors.edgeCount == 1);
                block->hotness = predBlock->hotness;
                TraverseCFG(ctx, block, traversed, PropogateHotness, loops, loopCount);
                return;
            }
        }

        hotness += predBlock->hotness / (f32)predBlock->successors.edgeCount;
    }
    block->hotness = hotness;
    TraverseCFG(ctx, block, traversed, PropogateHotness, loops, loopCount);
}

void AnalyzeSCEVs(CompilerContext ctx, SSABasicBlock* block, bool* traversed, LinearAllocator* mem, u32* scevs, LoopInfo** loops, u32 loopCount) {

    byte* const baseMem = ctx.compiler->mem;

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {
        u32 itPtr = (byte*)it - baseMem;
        GetSCEV(ctx, itPtr, mem, scevs, loops, loopCount);
        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    TraverseCFG(ctx, block, traversed, AnalyzeSCEVs, mem, scevs, loops, loopCount);
}
void OptimizeSSAFunction(CompilerContext context, byte *mem, bool* traversalMem) {

    /*
    Mem<u32>(traversalMem) = 0;
    auto loopsEnd = FindLoops(context, mem, (u32*)traversalMem);

    auto loops = (LoopInfo**)loopsEnd;
    u32 loopCount = 0;
    for(auto it = mem; it < loopsEnd;) {
        auto loop = (LoopInfo*)it;
        loops[loopCount++] = loop;
        it += sizeof(LoopInfo) + loop->bodyCount * sizeof(SSABasicBlock*);
    }

    u32 scevs[context.fn->maxSSAName];
    memset(scevs, ~u32(0), sizeof(u32) * context.fn->maxSSAName);
    memset(traversalMem, 0, context.fn->maxBlockName);
    AnalyzeSCEVs(context, context.fn->entry, traversalMem, &context.compiler->symbolicMem, scevs, loops, loopCount);
    for(u32 i = 0; i < context.fn->maxSSAName; i++) {
        scevs[i] = SimplifySCEV(context, scevs[i], &context.compiler->symbolicMem, mem, traversalMem);
    }
    */

    OptSimplifyPhase(context, mem, traversalMem);
    OptLoopSimplifyPhase(context, mem, traversalMem);
    OptSimplifyPhase(context, mem, traversalMem);
}
f32 OptInlinePhase(Compiler* compiler, f32 budget, SSAProgram* program, byte* const mem, bool* const traversalMem) {
    
    u32 initialProgramComplexity = 0;
    f32 growth = 1.f;

    for(;;) {

        byte* localMem = mem;

        auto localHeap = make_local_malloc(localMem, Kilobyte(16));
        byte* const localHeapBase = get_local_malloc_base(localHeap);
        localMem += Kilobyte(16);
        auto graph = BuildCallGraph(compiler, &localHeap, program, localMem, traversalMem);
        byte* const baseMem = compiler->mem;
        
        u32 programComplexity = 0;
        for(u32 i = 0; i < program->functionCount; i++) {

            Mem<u32>(localMem) = 0;
            Mem<u32>(traversalMem) = 0;
            CompilerContext ctx{compiler, program->functions[i]};
            auto loopsEnd = FindLoops(ctx, localMem, (u32*)traversalMem);
            u32 loopCount = 0;
            auto loops = (LoopInfo**)(loopsEnd);
            for(; localMem < loopsEnd;) {
                auto loop = (LoopInfo*)localMem;
                loops[loopCount++] = loop;
                localMem += sizeof(LoopInfo) + loop->bodyCount * sizeof(SSABasicBlock*);
            }

            memset(traversalMem, 0, ctx.fn->maxBlockName);
            ctx.fn->entry->hotness = 1.0f;
            PropogateHotness(ctx, ctx.fn->entry, traversalMem, loops, loopCount);
            memset(traversalMem, 0, ctx.fn->maxBlockName);
            programComplexity += ApproximateFunctionComplexity(ctx, ctx.fn->entry, traversalMem);
        }
        initialProgramComplexity = initialProgramComplexity ? initialProgramComplexity : programComplexity;

        auto decision = SelectInlineDecision(compiler, localHeapBase, &graph, traversalMem);
        if(!decision.call) break;

        auto inlinee = (SSAFunction*)(baseMem + graph.vertices[decision.callee].fn);
        memset(traversalMem, 0, inlinee->maxBlockName);
        programComplexity += ApproximateFunctionComplexity({compiler, inlinee}, inlinee->entry, traversalMem);

        f32 growthBefore = growth;
        growth = (f32)programComplexity / (f32)initialProgramComplexity;
        if(growth > budget) {
            growth = growthBefore;
            break;
        }

        auto fnAddress = (SSADefinition*)(baseMem + decision.call->operand0);
        auto called = (SSAFunction*)(baseMem + fnAddress->operand0);
        auto caller = (SSAFunction*)(baseMem + graph.vertices[decision.caller].fn);
        global_print("%s%i%s%s*%s%s*%\n", "inlining call(", decision.call->value, ") to ", called->name.text, called->name.lenght, " into ", caller->name.text, caller->name.lenght);

        CompilerContext ctx{compiler, caller};
        InlineFunctionCall(ctx, decision.call, (u32*)localMem, (u32*)traversalMem);

        OptimizeSSAFunction(ctx, localMem, traversalMem);
    }

    global_print("%s%f%\n", "growth: ", (f64)growth);
    return (budget - growth) + 1.f;
}

void ComputeSlotMapping(CompilerContext ctx, SSABasicBlock* block, bool* traversed, u32* mapping, u32* slotIndex) {

    block->lowerSlotIndex = *slotIndex;
    byte* const baseMem = ctx.compiler->mem;
    for(u32 i = 0; i < block->phiCount; i++) {
        u32 phiPtr = Mem<u32>(baseMem + block->phis + i * sizeof(u32));
        auto phi = (SSADefinition*)(baseMem + phiPtr);
        mapping[phi->value] = *slotIndex;
    }

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {
        mapping[it->value] = (*slotIndex)++;
        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }
    block->upperSlotIndex = (*slotIndex)++;

    
    TraverseCFG(ctx, block, traversed, ComputeSlotMapping, mapping, slotIndex);
}
void TraverseTrace(CompilerContext ctx, SSABasicBlock* defBlock, SSABasicBlock* block, SSABasicBlock* useBlock, LiveTrace* trace, bool* traversed) {

    byte* const baseMem = ctx.compiler->mem;
    u32 useB = (byte*)useBlock - baseMem;
    u32 blockPtr = (byte*)block - baseMem;

    if(block == useBlock) {
        trace->trace[trace->blockCount].block = blockPtr;
        trace->trace[trace->blockCount].lowerIndex = block->lowerSlotIndex;
        trace->blockCount += !(block == defBlock);
        return;
    }

    if(defBlock != block) {
        bool found = false;
        for(u32 i = 0; i < trace->blockCount; i++) {
            if(trace->trace[i].block == blockPtr) {
                found = true;
                break;
            }
        }
        if(!found) {
            trace->trace[trace->blockCount].lowerIndex = block->lowerSlotIndex;
            trace->trace[trace->blockCount++].upperIndex = block->upperSlotIndex;
        }
    }

    traversed[block->name] = true;
    for(u32 i = 0; i  < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        auto succBlock = (SSABasicBlock*)(baseMem + succBlockPtr);

        if(traversed[succBlock->name]) {
            continue;
        }
        auto visitMem = (u32*)(traversed + ctx.fn->maxBlockName);
        visitMem[0] = 0;
        if(!IsReachable(baseMem, succBlock, useB, visitMem)) {
            continue;
        }
        TraverseTrace(ctx, defBlock, succBlock, useBlock, trace, traversed);
    }
}
bool IsControlFlowOp(u32 op) {
    return op >= JMP && op <= EXIT;
}
byte* GetLiveTrace(CompilerContext ctx, u32 defPtr, bool* traversed, u32* slotMapping, u32* loopMapping, byte* loopsBegin, byte* result) {

    byte* const baseMem = ctx.compiler->mem;
    auto def = (SSADefinition*)(baseMem + defPtr);

    auto defBlock = (SSABasicBlock*)(baseMem + def->block);
    auto defLoop = loopMapping[defBlock->name];

    memset(traversed, 0, ctx.fn->maxBlockName);
    traversed[defBlock->name] = true;
    u32 useCount = GetValUses(ctx, defBlock, traversed, defPtr, (u32*)result);
    u32 uses[useCount];
    memcpy(uses, result, useCount * sizeof(u32));

    auto trace = (LiveTrace*)result;
    trace->def = defPtr;
    trace->blockCount = 1;

    trace->trace[0].block = def->block;
    trace->trace[0].lowerIndex = slotMapping[def->value];
    trace->trace[0].upperIndex = defBlock->upperSlotIndex;
    auto b = trace->trace;

    for(u32 i = 0; i < useCount; i++) {
        auto use = (SSADefinition*)(baseMem + uses[i]);
        SSABasicBlock* useBlock;
        bool cfOp = IsControlFlowOp(use->opr);
        if(cfOp) {
            constexpr auto offset = __builtin_offsetof(SSABasicBlock, nextBlock);
            useBlock = (SSABasicBlock*)(((byte*)use) - offset);
        }
        else {
            useBlock = (SSABasicBlock*)(baseMem + use->block);
        }

        if(defLoop != loopMapping[useBlock->name] && loopMapping[useBlock->name] != ~u32(0)) {
            auto loop = (LoopInfo*)(loopsBegin + loopMapping[useBlock->name]);
            for(u32 k = 0; k < loop->bodyCount; k++) {
                trace->trace[trace->blockCount].block = (byte*)loop->bodyBlocks[k] - baseMem;
                trace->trace[trace->blockCount].lowerIndex = loop->bodyBlocks[k]->lowerSlotIndex;
                trace->trace[trace->blockCount].upperIndex = loop->bodyBlocks[k]->upperSlotIndex;
                trace->blockCount++;
            }
        }
        else {
            memset(traversed, 0, ctx.fn->maxBlockName);
            TraverseTrace(ctx, defBlock, defBlock, useBlock, trace, traversed);
            auto up = cfOp ? useBlock->upperSlotIndex : slotMapping[use->value]-1;
            trace->trace[trace->blockCount-1].upperIndex = Max(up, trace->trace[trace->blockCount-1].lowerIndex);
        }
    }

    return (byte*)(trace->trace + trace->blockCount);
}

void ComputeLiveTraces(CompilerContext ctx, SSABasicBlock* block, bool* traversed, u32* slotMapping, u32* byteOffset, byte* result, u32* loopMapping, byte* loopsBegin) {

    byte* const baseMem = ctx.compiler->mem;
    auto trace = result;
    for(u32 i = 0; i < block->phiCount; i++) {
        u32 phiPtr = Mem<u32>(baseMem + block->phis + i * sizeof(u32));
        auto phi = (SSADefinition*)(baseMem + phiPtr);
        auto off = trace + (*byteOffset);
        *byteOffset += GetLiveTrace(ctx, phiPtr, traversed + ctx.fn->maxBlockName, slotMapping, loopMapping, loopsBegin, off) - off;
    }

    auto it = GetPointer<SSADefinition>(baseMem, block->firstDef);
    while(it) {
        u32 itPtr = (byte*)it - baseMem;
        auto off = trace + (*byteOffset);
        *byteOffset += GetLiveTrace(ctx, itPtr, traversed+ctx.fn->maxBlockName, slotMapping, loopMapping, loopsBegin, off) - off;
        it = GetPointer<SSADefinition>(baseMem, it->nextDef);
    }

    TraverseCFG(ctx, block, traversed, ComputeLiveTraces, slotMapping, byteOffset, trace, loopMapping, loopsBegin);
}


struct InterferenceEdge {
    u32 vertex;
    u32 next;
};
struct InterferenceVertex {
    InterferenceEdge* edges;
};
struct InterferenceGraph {
    u32 vertexCount;
    InterferenceVertex* vertices;
};
void InsertInterferenceEdge(InterferenceGraph* graph, LocalMallocState* localHeap, u32 i, u32 k) {

    u32 max = Max(i,k)+1;
    if(max > graph->vertexCount) {

        auto tmp = (InterferenceVertex*)local_malloc(localHeap, max * sizeof(InterferenceVertex));
        memcpy(tmp, graph->vertices, sizeof(InterferenceVertex) * graph->vertexCount);
        u32 diff = max - graph->vertexCount;
        memset(tmp+graph->vertexCount, 0, diff * sizeof(InterferenceVertex));

        local_free(localHeap, graph->vertices);
        graph->vertices = tmp;
        graph->vertexCount = max;
    }

    byte* localHeapBase = get_local_malloc_base(*localHeap);
    auto it0 = graph->vertices[i].edges;
    for(; it0; it0 = GetPointer<InterferenceEdge>(localHeapBase, it0->next));
    if(!it0) {
        auto v = (InterferenceEdge*)local_malloc(localHeap, sizeof(InterferenceEdge));
        v->vertex = k;
        v->next = graph->vertices[i].edges ? (byte*)graph->vertices[i].edges - localHeapBase : 0;
        graph->vertices[i].edges = v;
    }

    auto it1 = graph->vertices[k].edges;
    for(; it1; it1 = GetPointer<InterferenceEdge>(localHeapBase, it1->next));
    if(!it1) {
        auto v = (InterferenceEdge*)local_malloc(localHeap, sizeof(InterferenceEdge));
        v->vertex = i;
        v->next = graph->vertices[k].edges ? (byte*)graph->vertices[k].edges - localHeapBase : 0;
        graph->vertices[k].edges = v;
    }
}
bool CheckInterferenceEdge(InterferenceGraph* graph, byte* graphBase, u32 i, u32 k) {

    if(Max(i,k)+1 > graph->vertexCount) return false;
    for(auto it = graph->vertices[i].edges; it; it = GetPointer<InterferenceEdge>(graphBase, it->next) ) {
        if(it->vertex == k) return true;
    }
    for(auto it = graph->vertices[k].edges; it; it = GetPointer<InterferenceEdge>(graphBase, it->next) ) {
        if(it->vertex == i) return true;
    }
    return false;
}

template<typename T>
bool nInRange(T r0, T r1, T n) {
    return r0 <= n && n <= r1;
}

byte* ComputeLoopMapping(CompilerContext ctx, u32* mapping, byte* loopMem, bool* traversalMem) {

    byte* const baseMem = ctx.compiler->mem;
    byte* loopsBegin = loopMem;
    byte* loopsEnd = FindLoops(ctx, loopsBegin, (u32*)traversalMem);
    loopMem = loopsEnd;

    auto loops = (LoopInfo**)loopMem;
    u32 loopsCount = 0;
    for(auto it = loopsBegin; it < loopsEnd;) {
        auto loop = (LoopInfo*)it;
        loops[loopsCount++] = loop;
        it += sizeof(LoopInfo) + loop->bodyCount * sizeof(SSABasicBlock*);
    }

    memset(mapping, ~u32(0), ctx.fn->maxBlockName * sizeof(u32));
    for(u32 z = 0; z < loopsCount; z++) {

        auto loop = loops[z];
        for(u32 i = 0; i < loop->bodyCount; i++) {
            mapping[loop->bodyBlocks[i]->name] = (byte*)loop - loopsBegin;
        }

        auto innerHeaders = (SSABasicBlock**)(loops + loopsCount);
        Mem<u32>(traversalMem) = 1;
        Mem<u32>(traversalMem + sizeof(u32)) = (byte*)loop->exitBlock - baseMem;
        u32 headerCount = FindLoopHeaders(ctx, ctx.fn->entry, loop->headerBlock, innerHeaders, (u32*)traversalMem);

        memset(traversalMem, 0, ctx.fn->maxBlockName);
        for(u32 i = 1; i < headerCount; i++) {
            for(u32 k = 0; k < loopsCount; k++) {
                if(z == k) continue;
                if(loops[k]->headerBlock == innerHeaders[i]) {
                    for(u32 j = 0; j < loops[k]->bodyCount; j++) {
                        mapping[loops[k]->bodyBlocks[j]->name] = (byte*)loops[k] - loopsBegin;
                    }
                }
            }
        }
    }

    return loopsEnd;
}

void AllocateRegistersSSAFunction(CompilerContext ctx, byte* mem, bool* traversalMem, u32* allocation) {

    byte* const baseMem = ctx.compiler->mem;

    u32* loopMapping = (u32*)mem;
    byte* loopsBegin = (byte*)(loopMapping + ctx.fn->maxBlockName);
    mem = ComputeLoopMapping(ctx, loopMapping, loopsBegin, traversalMem);

    u32* slotMapping = (u32*)mem;
    u32 slot = 0;
    memset(traversalMem, 0, ctx.fn->maxBlockName);
    ComputeSlotMapping(ctx, ctx.fn->entry, traversalMem, slotMapping, &slot);
    mem = (byte*)(slotMapping + Max((i32)0, (i32)ctx.fn->maxSSAName-1));

    auto traces = (LiveTrace*)mem;
    memset(traversalMem, 0, ctx.fn->maxBlockName);
    u32 byteCount = 0;
    ComputeLiveTraces(ctx, ctx.fn->entry, traversalMem, slotMapping, &byteCount, mem, loopMapping, loopsBegin);
    mem += byteCount;

    auto tracePtrs = (LiveTrace**)mem;
    u32 tracePtrCount = 0;
    for(auto it = (byte*)traces; it < ((byte*)traces)+byteCount; ) {
        auto trace = (LiveTrace*)it;

        tracePtrs[tracePtrCount++] = trace;
        it += sizeof(LiveTrace) + trace->blockCount * sizeof(LiveInterval);
    }
    mem += sizeof(LiveTrace**) * tracePtrCount;

    auto localHeap = make_local_malloc(mem, Kilobyte(16));
    byte* const localHeapBase = get_local_malloc_base(localHeap);
    mem += Kilobyte(16);

    InterferenceGraph graph{};
    u32 ssaDefCount = Max(0, (i32)ctx.fn->maxSSAName-1);
    graph.vertices = (InterferenceVertex*)local_malloc(&localHeap, ssaDefCount * sizeof(InterferenceVertex));
    graph.vertexCount = ssaDefCount;
    memset(graph.vertices, 0, ssaDefCount * sizeof(InterferenceVertex));

    for(u32 i = 0; i < tracePtrCount; i++) {
        auto trace0 = tracePtrs[i];

        for(u32 k = 0; k < tracePtrCount; k++) {
            if(i == k || CheckInterferenceEdge(&graph, localHeapBase, i,k) ) continue;
            
            bool interference = false;
            auto trace1 = tracePtrs[k];
            for(u32 j = 0; j < trace0->blockCount && !interference; j++) {
                
                for(u32 z = 0; z < trace1->blockCount; z++) {
                    bool overlap = nInRange(trace1->trace[z].lowerIndex, trace1->trace[z].upperIndex, trace0->trace[j].lowerIndex) ||
                                   nInRange(trace1->trace[z].lowerIndex, trace1->trace[z].upperIndex, trace0->trace[j].upperIndex);
                    if(overlap) {
                        interference = true;
                        break;
                    }
                }
            }

            if(interference) {
                InsertInterferenceEdge(&graph, &localHeap, i,k);
            }
        }
    }

    u32 registerPressurePerSlot[ssaDefCount]{};
    bool incremented[ssaDefCount];
    for(u32 i = 0; i < tracePtrCount; i++) {
        memset(incremented, 0, ssaDefCount);
        auto trace0 = tracePtrs[i];
        auto def = (SSADefinition*)(baseMem + trace0->def);
        global_print("%i%\n", def->value);
        for(u32 k = 0; k < trace0->blockCount; k++) {
            global_print("%\t%i% %i%\n", trace0->trace[k].lowerIndex, trace0->trace[k].upperIndex);

            for(u32 j = trace0->trace[k].lowerIndex; j < trace0->trace[k].upperIndex+1; j++) {
                if(!incremented[j]) {
                    registerPressurePerSlot[j]++;
                    incremented[j] = true;
                }
            }
        }
    }

    bool available[graph.vertexCount];
    memset(available, true, graph.vertexCount);
    memset(allocation, ~u32(0), graph.vertexCount * sizeof(u32));
    for(u32 i = 0; i < graph.vertexCount; i++) {

        for(auto it = graph.vertices[i].edges; it; it = GetPointer<InterferenceEdge>(localHeapBase, it->next)) {
            if(allocation[it->vertex] != ~u32(0)) {
                available[allocation[it->vertex]] = false;
            }
        }
        for(u32 k = 0; k < graph.vertexCount; k++) {
            if(available[k]) {
                allocation[i] = k;
                break;
            }
        }
        memset(available, true, graph.vertexCount);
    }

    auto regs = ctx.compiler->machine->arhitecture->regDescriptorTable;
    global_print("%s*%\n", ctx.fn->name.text, ctx.fn->name.lenght);
    for(u32 k = 0; k < graph.vertexCount; k++) {
        global_print("%i% %s%\n", k, regs[allocation[k]].str);
    }
    global_print("%\n");

    u32 peakPresure = 0;
    for(u32 i = 0; i < ssaDefCount; i++) {
        peakPresure = Max(registerPressurePerSlot[i], peakPresure);
        global_print("%i% %i%\n", i, registerPressurePerSlot[i]);
    }
    global_print("%s%i%\n", "peak register pressure: ", peakPresure);
}

void OptSpecializePhase(Compiler* compiler, f32 budget, SSAProgram* program, byte* mem, bool* traversalMme) {

}
void OptimizeProgram(Compiler* compiler, SSAProgram* program, byte* mem, bool* traversalMem) {

    for(u32 i = 0; i < program->functionCount; i++) {
        CompilerContext ctx{compiler, program->functions[i]};
        OptimizeSSAFunction(ctx, mem, traversalMem);
    }
    f32 remainingBudget = OptInlinePhase(compiler, 1.2f, program, mem, traversalMem);
    OptSpecializePhase(compiler, remainingBudget, program, mem, traversalMem);
}

void PrintSSAMemoryCFG(Compiler *compiler, SSABasicBlock *block, u32* visited) {

    byte* const baseMem = compiler->mem;
    u32 blockPtr = (u64)block - (u64)baseMem;
    u32 visitedCount = visited[0];
    for (u32 i = 1; i < visitedCount+1; i++) {
        if ( visited[i] == blockPtr) {
            return;
        }
    }
    visited[++visited[0]] = blockPtr; 

    global_print("%s%i", "Block: ", block->name);
    if (block->predecessors.edgeCount)
        global_print("%s", " predecessors: ");
    for (u32 i = 0; i < block->predecessors.edgeCount; i++) {
        u32 blockPtr = Mem<u32>(compiler->mem + block->predecessors.edges + i * sizeof(u32));
        SSABasicBlock *pred = (SSABasicBlock *)(compiler->mem + blockPtr);
        global_print("%s%i", " ", pred->name);
    }
    global_print("%c", '\n');

    for(u32 i = 0; i < block->memoryPhiCount; i++) {
        u32 memPhiPtr = Mem<u32>(baseMem + block->memoryPhis + i * sizeof(u32));
        global_print("%c", '\t');
        PrintMemory(compiler, memPhiPtr, (u32*)(visited+visited[0]+1));
        global_print("%\n");
    }

    auto it = GetPointer<SSAMemoryDef>(baseMem, block->firstMem);
    while(it) {

        global_print("%c", '\t');
        PrintMemory(compiler, (u64)it - (u64)baseMem, (u32*)(visited+visited[0]+1));
        global_print("%\n");
        it = GetNextMem(baseMem, it->next);
    }

    for (u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succPtr = Mem<u32>(compiler->mem + block->successors.edges + i * sizeof(u32));
        SSABasicBlock* succBlock = (SSABasicBlock *)(compiler->mem + succPtr);
        PrintSSAMemoryCFG(compiler, succBlock, visited);
    }
}

void PrintSSAFunctionCFG(Compiler *compiler, SSABasicBlock* block, u32* visited) {

    byte* const baseMem = compiler->mem;
    u32 blockPtr = (u64)block - (u64)baseMem;
    u32 visitedCount = visited[0];
    for (u32 i = 1; i < visitedCount+1; i++) {
        if ( visited[i] == blockPtr) {
            return;
        }
    }
    visited[++visited[0]] = blockPtr; 

    global_print("%s%i", "Block: ", block->name);
    if (block->predecessors.edgeCount)
        global_print("%\n%s", "predecessors: ");
    for (u32 i = 0; i < block->predecessors.edgeCount; i++) {
        u32 blockPtr = Mem<u32>(compiler->mem + block->predecessors.edges + i * sizeof(u32));
        SSABasicBlock *pred = (SSABasicBlock *)(compiler->mem + blockPtr);
        global_print("%c%i", ' ', pred->name);
    }
    global_print("%\n%s", "successors: ");
    for (u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 blockPtr = Mem<u32>(compiler->mem + block->successors.edges + i * sizeof(u32));
        SSABasicBlock *succ = (SSABasicBlock *)(compiler->mem + blockPtr);
        global_print("%c%i", ' ', succ->name);
    }
    global_print("%\n%s%f%c%\n", "block info: {", block->hotness, '}');

    for (u32 i = 0; i < block->phiCount; i++) {
        u32 phiPtr = Mem<u32>(compiler->mem + block->phis + i * sizeof(u32));
        SSADefinition *phi = (SSADefinition *)(compiler->mem + phiPtr);

        u32* ptr = GetPhiOperandPtr(baseMem, phi);
        global_print("%c%i%s", '\t', phi->value, " PHI ");
        for (u32 k = 0; k < phi->operand0; k++) {
            SSADefinition *inc = (SSADefinition *)(compiler->mem + ptr[k]);
            global_print("%i%c", inc->value, ' ');
        }

        global_print("%\n");
    }

    auto def = GetPointer<SSADefinition>(baseMem, block->firstDef);
    if(def) {
        ASSERT(def->prevDef == 0);
    }
    if(block->lastDef) {
        ASSERT(Mem<SSADefinition>(baseMem + block->lastDef).nextDef == 0);
    }
    while (def) {

        global_print("%c", '\t');
        PrintSSADef(baseMem, def);
        global_print("%\n");

        def = (def->nextDef == 0 ? nullptr : (SSADefinition *)(compiler->mem + def->nextDef));
    }

    switch (block->nextBlock.opr) {
    case BRANCH:
        {
            SSABasicBlock *thenBlock = (SSABasicBlock *)(compiler->mem + block->nextBlock.branch.thenBlock);
            SSABasicBlock *elseBlock = (SSABasicBlock *)(compiler->mem + block->nextBlock.branch.elseBlock);

            u32 condPtr = block->nextBlock.branch.conditionDef;
            if (condPtr != ~u32(0)) {
                SSADefinition *cond = (SSADefinition *)(compiler->mem + block->nextBlock.branch.conditionDef);
                global_print("%s%i%s%i%s%i%\n", "\tBR (", cond->value, ") then: ", thenBlock->name, " else: ", elseBlock->name);
            }
            else {
                global_print("%s%i%s%i%\n", "\tBR (SSA_UN_INIT) then: ", thenBlock->name, " else: ", elseBlock->name);
            }
            break;
        }
    case JMP:
        {
            ASSERT(block->nextBlock.jmp.targetBlock != ~u32(0));
            SSABasicBlock *target = (SSABasicBlock*)(compiler->mem + block->nextBlock.jmp.targetBlock);
            global_print("%s%i%\n", "\tJMP ", target->name);
            break;
        }
    case RET:
        {
            global_print("%s", "\tRET ");
            if(block->nextBlock.ret.retDef != ~u32(0)) {
                SSADefinition* retDef = (SSADefinition*)(compiler->mem + block->nextBlock.ret.retDef);
                global_print("%i", retDef->value);
            }
            global_print("%\n");
            break;
        }
    }

    global_print("%\n");
    for (u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succPtr = Mem<u32>(compiler->mem + block->successors.edges + i * sizeof(u32));
        SSABasicBlock* succBlock = (SSABasicBlock *)(compiler->mem + succPtr);
        PrintSSAFunctionCFG(compiler, succBlock, visited);
    }
}


Compiler MakeCompiler(byte* memory, u32 size) {

    ASSERT(memory && size >= Kilobyte(512));
    u64 memory_page_size = sysconf(_SC_PAGESIZE);
    memset(memory, ~byte(0), size);

    {
        auto s = memory;
        memory = (byte*)align_pointer(memory, Max(memory_page_size, CACHE_LINE_SIZE));
        size -= (memory - s);
    }

    Compiler compiler;
    memset(&compiler, 0, sizeof(Compiler));

    compiler.totalMem = size;
    compiler.mem                = memory;
    compiler.localHeap          = make_local_malloc(memory, size/2);
    compiler.scratchMem         = (memory + size/2) - Kilobyte(128);
    compiler.cfgTraversalMemory = (compiler.scratchMem + Kilobyte(64));
    compiler.symbolicMem        = make_linear_allocator((byte*)local_malloc(&compiler.localHeap, Kilobyte(128)), Kilobyte(128));

    compiler.exprAllocator    = (size/2) + 64;
    compiler.StmtAllocator    = (size/2) + (size/8);

    compiler.ssaBlockPool     = make_memory_pool<sizeof(SSABasicBlock)>(memory + (size/2) + (size/4) + (size / 16) * 0, (size / 16) );
    compiler.ssaDefPool       = make_memory_pool<sizeof(SSADefinition)>(memory + (size/2) + (size/4) + (size / 16) * 1, (size / 16) );
    compiler.memoryDefPool    = make_memory_pool<sizeof(SSAMemoryDef)>( memory + (size/2) + (size/4) + (size / 16) * 2, (size / 16) );
    compiler.miscAllocatorSSA = (size / 2) + (size/4) + (size / 16) * 3;

    for(TypeName i = TYPE_PRIMARY_BOOL; i <= TYPE_PRIMARY_F64; i = (TypeName)(i + 1)) {
        compiler.basicTypes[i] = {compiler.exprAllocator};
        *AllocateExpr<TypeName>(&compiler) = i;
        *AllocateExpr<TypeName>(&compiler) = TYPE_NON;
    }
    
    compiler.parser.tokenBuffer.Init();
    compiler.parser.structs.Init();
    compiler.symbolTable.Init(&compiler.localHeap);
    compiler.basicBlockVarDefs.Init(&compiler.localHeap);
    compiler.incompleteMemoryOps.Init(&compiler.localHeap);
    compiler.rangeTable.Init(&compiler.localHeap);

    return compiler;
}

void DumpSSAIR(Compiler* compiler, SSAProgram* program, byte* visitMem) {

    for(u32 i = 0; i < program->functionCount; i++) {

        auto fn = program->functions[i];
        global_print("%\n");
        u32 index = FindSymbol(&compiler->symbolTable, fn->name);
        PrintTypeExpression(compiler->mem, compiler->mem, compiler->symbolTable[index].type);
        global_print("%s*%\n", fn->name.text, fn->name.lenght);

        Mem<u32>(visitMem) = 0;
        PrintSSAFunctionCFG(compiler, fn->entry, (u32*)visitMem);
    }
    global_io_flush();
}

SSAProgram* CompileInternal(Compiler* compiler, CompileConfig config) {

    compiler->machine = config.machine;
    compiler->optimize = config.optimize;
    
    Stmt programAST{compiler->StmtAllocator};
    while(ParseStatement(compiler));
    if (compiler->error || compiler->parser.error) {
        return nullptr;
    }
    *AllocateStmt<StatementType>(compiler) = STATEMENT_NON;
    compiler->parser.tokenBuffer.Free();

    auto programSSA = (SSAProgram*)compiler->scratchMem;
    programSSA->functionCount = 0;
    while(Mem<StatementType>(compiler->mem + programAST.index) != STATEMENT_NON) {

        compiler->memoryName = 0;
        compiler->uniqueMemoryName = 0;

        auto fn = AllocateSSA<SSAFunction>(compiler);
        programSSA->functions[programSSA->functionCount++] = fn;
        CompilerContext context{compiler, fn};

        byte *const scratchMem = (byte*)(programSSA->functions + programSSA->functionCount);
        Mem<u64>(scratchMem) = 0;
        CreateCDFG(context, &programAST, CFGEdges{}, false, scratchMem, (u32*)compiler->cfgTraversalMemory);
        fn->maxBlockName = compiler->basicBlockVarDefs.size;

        for(u32 i = 0; i < compiler->basicBlockVarDefs.size; i++) {
            LOG(global_free_debug(compiler->basicBlockVarDefs[i].key));
        }
        compiler->basicBlockVarDefs.Clear();

#ifdef DEBUG_BUILD
        check_all_memory(nullptr);
#endif
    }
    auto ret = (SSAProgram*)AllocateSSA(compiler, sizeof(SSAProgram) + sizeof(SSAFunction*) * programSSA->functionCount);
    memcpy(ret, programSSA, sizeof(SSAProgram) + sizeof(SSAFunction*) * programSSA->functionCount);

    compiler->StmtAllocator = 0;
    compiler->exprAllocator = 0;

    if(compiler->optimize) {
        OptimizeProgram(compiler, ret, compiler->scratchMem, (bool*)compiler->cfgTraversalMemory);
    }

    if(config.dumpIR) {
        DumpSSAIR(compiler, ret, compiler->cfgTraversalMemory);
    }

    return ret;
}

u32 GetSSADefOperandCount(byte* baseMem, SSADefinition* def) {

    if(IsDefBinary(def)) return 2;
    if(IsDefUnary(def)) return 1;
    if(def->opr == SSA_CALL) return GetPhiOperandCount(def);
    if(def->opr == SSA_PHI_NODE) return GetCallArgs(baseMem, def).argCount;
    ASSERT(0);
    return 1;
}

bool CheckSSAConstraint(SSADefinition* def, SSAPatternConstraint constraint) {
}

u32 GetSSAOperands(CompilerContext ctx, SSADefinition* def, u32* operands) {

}

bool MatchSSAPattern(CompilerContext ctx, u32 block, SSADefinition* def, SSAPattern* pattern, bool* traversed, u32* leaves) {

    byte* const baseMem = ctx.compiler->mem;
    if(pattern->implements == SSA_ANY) {

        for(u32 i = 0; i < pattern->constraintCount; i++) {
            if(!CheckSSAConstraint(def, pattern->constraints[i])) return false;
        }
        leaves[++leaves[0]] = (byte*)def - baseMem;
        return true;
    }
    else if(def->block == block && def->opr == pattern->implements) {

        for(u32 i = 0; i < pattern->constraintCount; i++) {
            if(!CheckSSAConstraint(def, pattern->constraints[i])) return false;
        }

        auto ssaOperands = leaves + leaves[0] + 1;
        u32 ssaOperandCount = GetSSAOperands(ctx, def, ssaOperands);

        if(pattern->operandCount != ssaOperandCount) return false;
        for(u32 i = 0; i < pattern->operandCount; i++) {
            auto operand = (SSADefinition*)(baseMem + ssaOperands[i]);
            if(!MatchSSAPattern(ctx, block, operand, pattern->operands[i], traversed, leaves)) return false;
        }

        return true;
    }

    ASSERT(false);
}

OpMIR* CpyOpMIRChain(Compiler* compiler, OpMIR* op, FunctionMIR* fnMIR) {

    auto cpy = (OpMIR*)pool_allocate(&fnMIR->opPool);
    *cpy = *op;

    auto operands = (OpMIR**)local_malloc(&compiler->localHeap, sizeof(OpMIR*) * op->operandCount);
    for(u32 i = 0 ; i < op->operandCount; i++) {
        operands[i] = CpyOpMIRChain(compiler, ((OpMIR**)op->operands)[i], fnMIR);
    }
    cpy->operands = operands;
    
    return cpy;
}
InstMIR* CpyInstMIR(Compiler* compiler, InstMIR* inst, FunctionMIR* fnMIR) {

    auto instCpy = (InstMIR*)pool_allocate(&fnMIR->instPool);
    instCpy->chainCount = inst->chainCount;
    auto chain = (OpMIR**)local_malloc(&compiler->localHeap, instCpy->chainCount * sizeof(OpMIR*));
    instCpy->opChains = chain;

    for(u32 i = 0; i < instCpy->chainCount; i++) {
        ((OpMIR**)instCpy->opChains)[i] = CpyOpMIRChain(compiler, ((OpMIR**)inst->opChains)[i], fnMIR);
    }

    return instCpy;
}
OpMIR* EmitMIRMacro(Compiler* compiler, BlockMIR* block, SSADefinition* def, MIRMacro* macro, FunctionMIR* fnMIR) {
}

OpMIR* MunchSSA(CompilerContext ctx, BlockMIR* block, SSADefinition* root, FunctionMIR* fnMIR, bool* traversed, BlockMIR** mirBlockTable) {

    u32 select = ~u32(0);
    i64 cost = 0;
    u32* leaves = (u32*)(traversed + ctx.fn->maxSSAName);
    for(u32 i = 0; i < ctx.compiler->machine->arhitecture->instCount; i++) {
        auto macro = &ctx.compiler->machine->arhitecture->instDescriptorTable[i].macro;
        leaves[0] = 0;
        for(u32 k = 0; k < macro->patternCount; k++) {
            if(MatchSSAPattern(ctx, root->block, root, macro->patterns[k], traversed, leaves) && cost > macro->cost) {
                cost = macro->cost;
                select = i;
                break;
            }
        }
    }

    byte* const baseMem = ctx.compiler->mem;
    u32 leafCount = leaves[0];
    u32 local_leaves[leafCount];
    memcpy(local_leaves, leaves, sizeof(u32) * leafCount);

    for(u32 i = 0; i < leafCount; i++) {
        auto ssaRoot = (SSADefinition*)(baseMem + local_leaves[i]);
        auto ssaBlock = (SSABasicBlock*)(baseMem + ssaRoot->block);
        auto mirBlock = mirBlockTable[ssaBlock->name];

        MunchSSA(ctx, mirBlock, ssaRoot, fnMIR, traversed, mirBlockTable);
    }

    auto macro = &ctx.compiler->machine->arhitecture->instDescriptorTable[select].macro;
    return EmitMIRMacro(ctx.compiler, block, root, macro, fnMIR);
}

void PrintOpChain(OpMIR* chain) {

    switch(chain->opr) {
    case MIR_READ:
        global_print("%s", "READ(");
        break;
    case MIR_WRITE:
        global_print("%s", "WRITE(");
        break;
    case MIR_MEM:
        global_print("%s", "MEM(");
        break;
    case MIR_VREG:
        global_print("%s", "VREG(");
        break;
    case MIR_PREG:
        global_print("%s", "PREG(");
        break;
    case MIR_CONSTANT:
        global_print("%s", "CONSTANT(");
        break;
    case MIR_ADD:
        global_print("%s", "ADD(");
        break;
    case MIR_SUB:
        global_print("%s", "SUB(");
        break;
    case MIR_MUL:
        global_print("%s", "MUL(");
        break;
    case MIR_SDIV:
        global_print("%s", "SDIV(");
        break;
    case MIR_UDIV:
        global_print("%s", "UDIV(");
        break;
    case MIR_SREM:
        global_print("%s", "SREM(");
        break;
    case MIR_UREM:
        global_print("%s", "UREM(");
        break;
    case MIR_AND:
        global_print("%s", "AND(");
        break;
    case MIR_OR:
        global_print("%s", "OR(");
        break;
    case MIR_XOR:
        global_print("%s", "XOR(");
        break;
    case MIR_NEG:
        global_print("%s", "NEG(");
        break;
    case MIR_SHL:
        global_print("%s", "SHL(");
        break;
    case MIR_SHR:
        global_print("%s", "SHR(");
        break;
    case MIR_SAR:
        global_print("%s", "SAR(");
        break;
    case MIR_SAL:
        global_print("%s", "SAL(");
        break;
    case MIR_ROR:
        global_print("%s", "ROR(");
        break;
    case MIR_ROL:
        global_print("%s", "ROL(");
        break;
    case MIR_CALL:
        global_print("%s", "CALL(");
        break;
    case MIR_JMP:
        global_print("%s", "JMP(");
        break;
    case MIR_CJMP:
        global_print("%s", "CJMP(");
        break;
    case MIR_RET:
        global_print("%s", "RET(");
        break;
    case MIR_BLOCK:
        global_print("%s", "BLOCK(");
        break;
    case MIR_FALLTHROUGH:
        global_print("%s", "FALLTHROUGH(");
        break;
    }

    if(chain->operandCount == 1) {
        PrintOpChain((OpMIR*)chain->operands);
    }
    else {
        for(u32 i = 0; i < chain->operandCount; i++) {
            PrintOpChain(((OpMIR**)chain->operands)[i]);
            global_print("%s", ", ");
        }
    }
    global_print("%c", ')');
}
void PrintInstMIR(InstMIR* inst) {

    for(u32 i = 0; i < inst->chainCount; i++) {
        PrintOpChain(((OpMIR**)inst->opChains)[i]);
    }
}


FunctionMIR* CreateMIR(CompilerContext ctx, byte* scratch, byte* traversal, byte* result) {

    auto ret = (FunctionMIR*)(result);
    result += sizeof(FunctionMIR);
    memset(traversal, 0, sizeof(void*) * (ctx.fn->maxBlockName + ctx.fn->maxSSAName));
    auto seen = (OpMIR**)(traversal + sizeof(void*) * ctx.fn->maxBlockName);

    ret->maxBlockName = ctx.fn->maxBlockName;
    ret->maxOpName = ctx.fn->maxSSAName;

    ret->blockPool = make_memory_pool<sizeof(BlockMIR)>(result, Kilobyte(8));
    result += Kilobyte(8);
    ret->instPool = make_memory_pool<sizeof(InstMIR)>(result, Kilobyte(8));
    result += Kilobyte(8);
    ret->opPool = make_memory_pool<sizeof(OpMIR)>(result, Kilobyte(8));
    result += Kilobyte(8);

    return ret;
}
SSAProgram* CompileFile(Compiler* compiler, CompileConfig config, const char* filePath) {

    ASSERT(compiler && filePath);
    
    u32 size;
    compiler->parser.source = (char *)ReadFileTerminated(filePath, compiler->mem + compiler->exprAllocator, &size);
    compiler->exprAllocator += size;
    compiler->parser.tokenizer = MakeTokenizer(compiler->parser.source);

    return CompileInternal(compiler, config);
}
SSAProgram* CompileString(Compiler* compiler, CompileConfig config, const char* str) {

    ASSERT(compiler && str);
    compiler->optimize = config.optimize;

    compiler->parser.source = (char*)compiler->mem + compiler->exprAllocator;
    u32 size = (byte*)str_cpy(compiler->parser.source, str) - compiler->mem;
    compiler->exprAllocator += size;
    compiler->parser.tokenizer = MakeTokenizer(compiler->parser.source);

    return CompileInternal(compiler, config);
}

CompileConfig ParseArgs(i32 argc, const char** args) {

    RUNTIME_CHECK(args);
    CompileConfig config{false,false,1.1f, {}};
    for(u32 i = 1; i < argc; i++) {
        RUNTIME_CHECK(args[i]);

        if(str_cmp(args[i], "-O1")) {
            config.optimize = true;
        }
        else if(str_cmp(args[i], "-O0")) {
            config.optimize = false;
        }
        else if(str_cmp(args[i], "--dump-IR")) {
            config.dumpIR = true;
        }
        else if(str_cmp(args[i], "--inline-limit")) {
            f32 limit = str_to_f64(args[++i]);
            config.growthBudget = limit;
        }
        else {
            global_print("%s%s%\n", "unrecognized command-line argument: ", args[i]);
        }
    }

    return config;
}
void DumpCompilerMemory(Compiler* compiler, const char* msg) {
    ASSERT(msg);

    auto file = fopen("./compiler_dump.bin", "w");
    fwrite(msg, str_len(msg), 1, file);
    fwrite(compiler, sizeof(Compiler), 1, file);
    fwrite(compiler->mem, compiler->totalMem, 1, file);

    fclose(file);
}

template<typename T>
T* RestorePointer(byte* baseMem, T* ptr) {
    if(!ptr) return nullptr;
    auto offset = (byte*)ptr - baseMem;
    return (T*)(baseMem + offset);
}
void RestoreList(byte* baseMem, FreeListState* freeList) {

    freeList->head = RestorePointer(baseMem, freeList->head);
    for(auto it = freeList->head; it; it = it->next) {
        it->next = RestorePointer(baseMem, it->next);
    }
}
void RestoreCompilerMemory(Compiler* compiler, const char* file) {

    u32 size;
    byte* dump = ReadFileTerminated(file, nullptr, &size);

    u32 len = str_len((char*)dump);
    global_print("%s%\n", (char*)dump);
    memcpy(compiler, dump + len, sizeof(Compiler));

    compiler->mem = (byte*)global_malloc_debug(compiler->totalMem);
    memcpy(compiler->mem, dump + len + sizeof(Compiler), compiler->totalMem);
    global_free_debug(dump);

    byte* baseMem = compiler->mem;
    compiler->localHeap.headBlock = RestorePointer(baseMem, compiler->localHeap.headBlock);
    compiler->cfgTraversalMemory = RestorePointer(baseMem, compiler->cfgTraversalMemory);
    compiler->scratchMem = RestorePointer(baseMem, compiler->scratchMem);
    compiler->symbolicMem.base = RestorePointer(baseMem, compiler->symbolicMem.base);

    compiler->memoryDefPool.base = RestorePointer(baseMem, compiler->memoryDefPool.base);
    RestoreList(baseMem, &compiler->memoryDefPool.list);
    compiler->ssaBlockPool.base = RestorePointer(baseMem, compiler->ssaBlockPool.base);
    RestoreList(baseMem, &compiler->ssaBlockPool.list);
    compiler->ssaDefPool.base = RestorePointer(baseMem, compiler->ssaDefPool.base);
    RestoreList(baseMem, &compiler->ssaDefPool.list);

    compiler->parser.source = RestorePointer(baseMem, compiler->parser.source);
    compiler->parser.structs.mem = RestorePointer(baseMem, compiler->parser.structs.mem);
    compiler->parser.tokenBuffer.mem = RestorePointer(baseMem, compiler->parser.tokenBuffer.mem);
    compiler->parser.tokenizer.at = RestorePointer(baseMem, compiler->parser.tokenizer.at);


    compiler->rangeTable.mem = RestorePointer(baseMem, compiler->rangeTable.mem);
    compiler->incompleteMemoryOps.mem = RestorePointer(baseMem, compiler->incompleteMemoryOps.mem);
    compiler->basicBlockVarDefs.mem = RestorePointer(baseMem, compiler->basicBlockVarDefs.mem);
    compiler->symbolTable.mem = RestorePointer(baseMem, compiler->symbolTable.mem);

    for(u32 i = 0; i < compiler->basicBlockVarDefs.size; i++) {
        compiler->basicBlockVarDefs[i].key = RestorePointer(baseMem, compiler->basicBlockVarDefs[i].key);
    }
    for(u32 i = 0; i < compiler->parser.tokenBuffer.size; i++) {
        compiler->parser.tokenBuffer[i].text = RestorePointer(baseMem, compiler->parser.tokenBuffer[i].text);
    }
    for(u32 i = 0; i < compiler->parser.structs.size; i++) {
        compiler->parser.structs[i].name.text = RestorePointer(baseMem, compiler->parser.structs[i].name.text);
    }
}

byte* LoadArchitectureDescritor(const char* file, byte* result) {

    u32 size;
    ReadFile(file, result, &size);
    auto arch = (ArhitectureDescriptor*)result;

    return result + size;
}



/*
const char* LookupReg(Compiler* compiler, u32 name, u32* allocation) {
    u32 reg = allocation[name];
    return compiler->machine->arhitecture->regDescriptorTable[reg].str;
}
byte* SelectInstX86_64(CompilerContext ctx, SSADefinition* def, byte* result, u32* regs) {

    auto regDescriptorTable = ctx.compiler->machine->arhitecture->regDescriptorTable;
    auto dst = regs[def->value];
    auto cmp = ctx.compiler;
    byte* const baseMem = cmp->mem;

    switch(def->opr) {
        
    case SSA_MEMORY_LOAD:
        {
            u32 address = Mem<SSADefinition>(baseMem + def->operand0).value;
            global_print("%s%s%c%s%c%\n", "mov ", LookupReg(cmp, dst, regs), '[', LookupReg(cmp, address, regs), ']');
            break;
        }
    case SSA_MEMORY_STORE:
        {
            u32 address = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 val = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%s%s%\n", "mov [", LookupReg(cmp, address, regs), "] ", LookupReg(cmp, val, regs));
            break;
        }
    case SSA_CMP_EQ:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "cmp ", LookupReg(cmp, op0, regs), ' ', LookupReg(cmp, op1, regs));
            global_print("%s%s%\n", "sete ", LookupReg(cmp, dst, regs));
            break;
        }
    case SSA_CMP_NOT_EQ:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "cmp ", LookupReg(cmp, op0, regs), ' ', LookupReg(cmp, op1, regs));
            global_print("%s%s%\n", "setne ", LookupReg(cmp, dst, regs));
            break;
        }
    case SSA_CMP_LESS_THAN:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "cmp ", LookupReg(cmp, op0, regs), ' ', LookupReg(cmp, op1, regs));
            if(IsTypeUnsigned(baseMem, def->type)) {
                global_print("%s%s%\n", "setb ", LookupReg(cmp, dst, regs));
            }
            else {
                global_print("%s%s%\n", "setl ", LookupReg(cmp, dst, regs));
            }
            break;
        }
    case SSA_CMP_BIGGER_THAN:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "cmp ", LookupReg(cmp, op0, regs), ' ', LookupReg(cmp, op1, regs));
            if(IsTypeUnsigned(baseMem, def->type)) {
                global_print("%s%s%\n", "seta ", LookupReg(cmp, dst, regs));
            }
            else {
                global_print("%s%s%\n", "setg ", LookupReg(cmp, dst, regs));
            }
            break;
        }
    case SSA_CMP_LESS_THAN_OR_EQ:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "cmp ", LookupReg(cmp, op0, regs), ' ', LookupReg(cmp, op1, regs));
            if(IsTypeUnsigned(baseMem, def->type)) {
                global_print("%s%s%\n", "seta ", LookupReg(cmp, dst, regs));
            }
            else {
                global_print("%s%s%\n", "setle ", LookupReg(cmp, dst, regs));
            }
            break;
        }
    case SSA_CMP_BIGGER_THAN_OR_EQ:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "cmp ", LookupReg(cmp, op0, regs), ' ', LookupReg(cmp, op1, regs));
            if(IsTypeUnsigned(baseMem, def->type)) {
                global_print("%s%s%\n", "setae ", LookupReg(cmp, dst, regs));
            }
            else {
                global_print("%s%s%\n", "setge ", LookupReg(cmp, dst, regs));
            }
            break;
        }
    case SSA_LOGICAL_AND:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%s%\n", "and ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            break;
        }
    case SSA_LOGICAL_OR:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%s%\n", "or ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            break;
        }
    case SSA_BITWISE_AND:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%s%\n", "and ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            break;
        }
    case SSA_BITWISE_OR:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%s%\n", "or ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            break;
        }
    case SSA_BITWISE_XOR:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%s%\n", "xor ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            break;
        }
    case SSA_BITWISE_NEG:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%s%\n", "not ", LookupReg(cmp, dst, regs));
            break;
        }
    case SSA_MUL:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            if(IsTypeUnsigned(baseMem, def->type)) {
                global_print("%s%\n", "push rax");
                global_print("%s%\n", "push rdx");
                
                global_print("%s%s%c%s%\n", "mov rax ", LookupReg(cmp, op0, regs));
                global_print("%s%s%c%s%\n", "mul ", LookupReg(cmp, op1, regs));
                global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), " rax");

                global_print("%s%\n", "pop rdx");
                global_print("%s%\n", "pop rax");
            }
            else {
                global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
                global_print("%s%s%c%s%\n", "imul ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            }
            break;
        }
    case SSA_DIV:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%\n", "push rax");
            global_print("%s%\n", "push rdx");
            
            global_print("%s%s%c%s%\n", "mov rax ", LookupReg(cmp, op0, regs));
            if(IsTypeUnsigned(baseMem, def->type)) {
                global_print("%s%s%c%s%\n", "div ", LookupReg(cmp, op1, regs));
            }
            else {
                global_print("%s%s%c%s%\n", "idiv ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            }
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), " rax");

            global_print("%s%\n", "pop rdx");
            global_print("%s%\n", "pop rax");
            break;
        }
    case SSA_ADD:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%s%\n", "add ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            break;
        }
    case SSA_SUB:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%s%\n", "sub ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            break;
        }
    case SSA_LEFT_BIT_SHIFT:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%s%\n", "shl ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            break;
        }
    case SSA_RIGHT_BIT_SHIFT:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            u32 op1 = Mem<SSADefinition>(baseMem + def->operand1).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%s%\n", "shr ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op1, regs));
            break;
        }
    case SSA_MINUS:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%\n", "neg ", LookupReg(cmp, dst, regs));
            break;
        }
    case SSA_LOGICAL_NEG:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            global_print("%s%s%c%\n", "not ", LookupReg(cmp, dst, regs));
            break;
        }
    case SSA_CONSTANT:
        {
            auto v = Mem<Value>(baseMem + def->operand0);
            global_print("%s%s%c", "mov ", LookupReg(cmp, dst, regs), ' ');
            PrintValue(v);
            global_print("%\n");
            break;
        }
    case SSA_CALL:
        {
            for(u32 i = 0; i < cmp->machine->arhitecture->abi.callerSavedRegCount; i++) {
                u32 saveReg = cmp->machine->arhitecture->abi.calleeSavedRegs[i];
                auto name = cmp->machine->arhitecture->regDescriptorTable[saveReg].str;
                global_print("%s%s%\n", "push " ,name);
            }

            auto calledFn = (SSAFunction*)(baseMem + Mem<SSADefinition>(baseMem + def->operand0).operand0);
            auto args = GetCallArgs(baseMem, def);

            ASSERT(cmp->machine->arhitecture->abi.paramRegCount >= args.argCount);

            for(u32 i = 0; i < args.argCount; i++) {
                u32 paramVal = Mem<SSADefinition>(baseMem + args.ptr[i]).value;
                u32 paramReg = cmp->machine->arhitecture->abi.paramRegs[i];
                auto name = cmp->machine->arhitecture->regDescriptorTable[paramReg].str;
                global_print("%s%s%\n", "push ", name);
                global_print("%s%s%c%s%\n", "mov ", name, ' ', LookupReg(cmp, paramVal, regs));
            }
            global_print("%s%s%\n", "push rax");
            global_print("%s%i%\n", "call ", ~u32(0));
            global_print("%s%s%\n", "mov ", LookupReg(cmp, def->value, regs), " rax");
            global_print("%s%s%\n", "pop rax");

            for(u32 i = 0; i < args.argCount; i++) {
                u32 paramVal = Mem<SSADefinition>(baseMem + args.ptr[i]).value;
                u32 paramReg = cmp->machine->arhitecture->abi.paramRegs[i];
                auto name = cmp->machine->arhitecture->regDescriptorTable[paramReg].str;
                global_print("%s%s%\n", "pop ", name);
            }

            for(i32 i = cmp->machine->arhitecture->abi.callerSavedRegCount-1; i > -1; i--) {
                u32 saveReg = cmp->machine->arhitecture->abi.calleeSavedRegs[i];
                auto name = cmp->machine->arhitecture->regDescriptorTable[saveReg].str;
                global_print("%s%s%\n", "pop " ,name);
            }
            break;
        }
    case SSA_CONVERT:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            global_print("%s%s%c%s%\n", "movz ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            break;
        }
    case SSA_PHI_NODE:
    case SSA_FN_PARAMETER:
    case SSA_UN_INIT:
    case SSA_FUNCTION:
            break;
    case SSA_ALLOCA:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            global_print("%s%s%\n", "sub rsp ", LookupReg(cmp, op0, regs));
            global_print("%s%s%s%\n", "mov ", LookupReg(cmp, dst, regs), " rsp");
            break;
        }
    case SSA_COPY:
        {
            u32 op0 = Mem<SSADefinition>(baseMem + def->operand0).value;
            global_print("%s%s%c%s%\n", "mov ", LookupReg(cmp, dst, regs), ' ', LookupReg(cmp, op0, regs));
            break;
        }
    }

}
byte* SelectCFInstX86_64(CompilerContext ctx, NextBlock* op, u32* regs, byte* result) {
    
    byte* const baseMem = ctx.compiler->mem;
    switch(op->opr) {
    case JMP:
        {
            auto name = Mem<SSABasicBlock>(baseMem + op->jmp.targetBlock).name;
            global_print("%s%i%\n", "jmp label", name);
            break;
        }
    case BRANCH:
        {
            u32 cond = Mem<SSADefinition>(baseMem + op->branch.conditionDef).value;
            auto thenBlock = Mem<SSABasicBlock>(baseMem + op->branch.thenBlock).name;
            auto elseBlock = Mem<SSABasicBlock>(baseMem + op->branch.elseBlock).name;
            global_print("%s%s%c%s%\n", "test ", LookupReg(ctx.compiler, cond, regs), ' ', LookupReg(ctx.compiler, cond, regs));
            global_print("%s%i%\n", "jnz block", thenBlock);
            global_print("%s%i%\n", "jmp block", elseBlock);
            break;
        }
    case RET:
    case EXIT:
        {
            global_print("%s%\n", "ret");
            break;
        }
    }
}
*/

byte* jit_memory[8*MEGA_BYTE];
i32 main(i32 argc, const char** args) {

    auto compilerMemory = init_global_state(Megabyte(48), Megabyte(16));

    auto config = ParseArgs(argc, args);
    config.machine = &piledriver;

    auto compiler = MakeCompiler(compilerMemory, Megabyte(16));
    auto IR = CompileFile(&compiler, config, "parse_test");

    global_io_flush();
}