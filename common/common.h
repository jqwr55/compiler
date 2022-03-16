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

#define SIZE_OF_ARRAY(arr) sizeof(arr) / sizeof(arr[0])

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
            global_print("c, s, s, s, c, s, c, i, c", '(', #x, ") ", y, ' ', __FILE__, ' ', __LINE__, 'c'); \
            global_io_flush();                                                                   \
            _TRAP;                                                                               \
        }
    #define ASSERT(x)                                                                                           \
        if (!(x)) {                                                                                             \
            global_print("c, s, s, s, c, i, c", '(', #x, ") triggered builtin trap in: ", __FILE__, ' ', __LINE__, '\n'); \
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
void Swap(T* t0, T* t1) {
    auto tmp = *t0;
    *t0 = *t1;
    *t1 = tmp;
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
    fresh_block->right_ptr = right_block ? (byte*)right_block - base : 0;
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
        
        while(*format == ' ') format++;
        format += *format == ' ';

        switch(*format++) {
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
    global_print("s", "out of memory\n");
    global_io_flush();
    runtime_panic(__FILE__, __LINE__);
}
byte* init_global_state(u32 heapSize, u32 miscMemoySize, u32 ioBufferSize) {

    auto memory = mmap(nullptr, heapSize + miscMemoySize + ioBufferSize, PROT_WRITE | PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
    if(!memory) {
        size_t size = sizeof("initial memory request failed\n");
        write(STDOUT_FILENO, "initial memory request failed\n", size);
    }

    init_global_malloc(memory, heapSize, print_out_of_memory);
    auto io_base = (byte*)memory + heapSize;
    init_global_print( make_linear_allocator((byte*)io_base, KILO_BYTE) );

    return (byte*)memory + heapSize + ioBufferSize;
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

/*
_FORCE_INLINE u64 xy_to_morton_ (u64 x, u64 y) {

    __m128i a = _mm_set_epi64x(0,x);
    a = _mm_clmulepi64_si128(a,a,0);

    __m128i b = _mm_set_epi64x(0,y);
    b = _mm_clmulepi64_si128(b,b,0);

    u64 l = _mm_extract_epi64(a, 0);
    u64 r = _mm_extract_epi64(b, 0);
    return l | (r << 1);
}
*/