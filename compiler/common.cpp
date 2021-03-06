#include <common.h>
#include <cstddef>
#include <memory>

byte *global_malloc_base;
malloc_handler_t global_out_of_memory_handler;
LinearAllocator io;

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

u64 align(u64 n, u64 alignment) {
    
    auto mod = n % alignment;
    auto rem = alignment - mod;
    auto aligned = n + rem * (mod != 0);

    return aligned;
}
void* align_pointer(void* n, u64 alignment) {

    auto mod = (uintptr_t)n % alignment;
    auto rem = alignment - mod;
    auto aligned = (byte*)n + rem * (mod != 0);

    return aligned;
}


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


LinearAllocator make_linear_allocator(void* base, u32 size) {
    LinearAllocator stack;
    stack.base = (byte*)base;
    stack.cap = size;
    stack.top = 0;
    return stack;
}
void* linear_aligned_allocate(LinearAllocator* stack, u32 size, u32 alignment) {

    auto mem = (byte*)align_pointer(stack->base + stack->top, alignment);

    auto rem = (uintptr_t)mem % alignment;
    auto rem2 = (mem - (stack->base + stack->top));

    ASSERT((uintptr_t)mem % alignment == 0 && ((mem - (stack->base + stack->top)) < alignment));

    stack->top = (mem + size) - stack->base;

    LOG_ASSERT(stack->top <= stack->cap, "stack overflow");
    return mem;
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

CoalescingLinearAllocator make_coalescing_linear_allocator(void* mem, u32 size) {

    CoalescingLinearAllocator allocator{};
    allocator.base = (byte*)mem;
    allocator.cap = size;
    return allocator;
}
void* linear_top(CoalescingLinearAllocator* ctx) {
    return ctx->base + ctx->top;
}
struct SizedMem {
    void* mem;
    u32 size;
};
SizedMem linear_max(CoalescingLinearAllocator* ctx) {
    
    SizedMem ret{linear_top(ctx), ctx->cap - ctx->top};
    auto it = ctx->freed;

    while(it) {
        ret.size = Max(ret.size, it->size);
        ret.mem = it;
        it = it->next;
    }
    return ret;
}
void* linear_alloc(CoalescingLinearAllocator* ctx, u32 size) {

    ASSERT(size >= sizeof(SizedFreeList));
   
    auto prev = (SizedFreeList*)ctx->freed;
    auto it = ctx->freed;
    while(it) {
        if(AbsDiff(size, it->size) > sizeof(SizedFreeList)) {

            SizedFreeList* next = it->next;
            if(it->size < size) {
                auto fresh_block = (SizedFreeList*)((byte*)it + size);
                fresh_block->size = size - it->size;
                fresh_block->next = it->next;
                next = fresh_block;
            }

            prev->next = next;
            return it;
        }

        prev = it;
        it = it->next;
    }

    auto block = ctx->base + ctx->top;
    ctx->top += size;
    LOG_ASSERT(ctx->top <= ctx->cap, "out of memory");

    return block;
}
void linear_free(CoalescingLinearAllocator* ctx, void* mem, u32 size) {

    ASSERT((ctx->base <= mem) && (mem <= (ctx->base + ctx->cap)));
    LOG_ASSERT(ctx->top >= size, "allocator underflow");
    auto free = (SizedFreeList*)mem;
    free->next = nullptr;
    free->size = size;

    if(!ctx->freed) {
        ctx->freed = free;
    }
    auto it = ctx->freed;

    SizedFreeList dummy;
    SizedFreeList* prev = &dummy;
    SizedFreeList* prev_prev = &dummy;

    byte* top = ctx->base + ctx->top;
    while(it) {

        if((byte*)it + it->size == (byte*)prev) {
            it->size += prev->size;
            prev_prev->next = it;
            prev = prev_prev;
        }
        if( ((byte*)it + it->size) == (ctx->base + ctx->top) ) {
            top = (byte*)it;
            ctx->freed = it->next;
        }
        if(it < free) {
            
            free->next = it;
            prev->next = free;
            it = free;
            free = nullptr;
        }

        prev_prev = prev;
        prev = it;
        it = it->next;
    }

    if(prev < free) {
        prev->next = free;
    }

    ctx->top = (top - ctx->base);
}

void* free_list_allocate(FreeList** list) {
    auto tmp = (*list)->next;
    *list = (*list)->next->next;
    return tmp;
}

void free_list_free(FreeList** list, void* memory) {
    Mem<FreeList>(memory).next = (*list)->next;
    *list = (FreeList*)memory;
}


CircularAllocator make_circular_allocator(void* mem, u32 size) {
    
    CircularAllocator ret;
    ret.base = (byte*)mem;
    ret.cap = size;
    ret.head = 0;

    return ret;
}
void* circular_allocate(CircularAllocator* alloc, u32 size) {

    ASSERT(size <= alloc->cap);
    bool wrap = (alloc->head + size) > alloc->cap;
    alloc->head += size;
    alloc->head *= !wrap;

    return alloc->base + alloc->head;
}

RingBuffer make_ring_buffer(void* mem, u32 size) {
    
    RingBuffer ret{};
    ret.base = (byte*)mem;
    ret.cap = size;

    return ret;
}

void circular_advance_write(RingBuffer* buffer, u32 size) {

    u32 head = buffer->head + size;
    buffer->head = head * (head <= buffer->cap);
}
void* circular_get_write_ptr(RingBuffer* buffer, u32 size) {

    ASSERT(size <= buffer->cap);

    u32 head = buffer->head;
    u32 tail = buffer->tail;

    u32 tailDst = (tail - head) - 1;
    u32 capDst = buffer->cap - head;
    u32 dst = tailDst > capDst ? capDst : tailDst;

    if(dst < size && tail < size) return nullptr;
    if(capDst >= size) {
        return buffer->base + head;
    }

    return buffer->base;
}
u32 circular_read_size(RingBuffer* buffer, u32 size) {

    u32 tail = buffer->tail;
    u32 head = buffer->head;

    if(head >= tail) {
        return head - tail;
    }

    if(buffer->cap - tail < size) {
        return buffer->head;
    }
    return buffer->cap - tail;
}
void* circular_get_read_ptr(RingBuffer* buffer, u32 size) {

    u32 tail = buffer->tail;
    return buffer->base + (tail * (tail + size <= buffer->cap));
}
void circular_advance_read(RingBuffer* buffer, u32 size) {

    buffer->tail += size;
    if(buffer->tail > buffer->cap) {
        buffer->tail = 0;
    }
}

_NO_INLINE
void runtime_panic(const char* file, u32 line) {

    global_io_flush();
    global_print("%s%s%s%i%\n", "runtime panic in file: ", file, " at line: ", line);
    global_io_flush();
    exit(1);
}


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
    auto free_block_size = get_size_in_block(free_block);

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
void print_local_heap_info(LocalMallocState state) {
    MemoryBlockHeader *block = state.headBlock;
    
    u32 total = 0;
    u32 totalBlockCount = 0;
    u32 allocatedTotal = 0;
    u32 allocatedTotalBlockCount = 0;
    u32 freeTotal = 0;
    u32 freeBlockCount = 0;
    u32 fragmented = 0;
    u32 maxFreeBlock = 0;

    auto base = get_local_malloc_base(state);
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

        global_print("ucscuc", (u64)block, ' ', extract_free_bit(block->size) ? "free" : "used", ' ', get_size_in_block(block), '\n');
        block = get_block_ptr(base, block->right_ptr);
    }


    global_print("suc", "total: "               , total, '\n');
    global_print("suc", "total block count: "   , totalBlockCount, '\n');
    global_print("suc", "in use: "              , allocatedTotal, '\n');
    global_print("suc", "in use block count: "  , allocatedTotalBlockCount, '\n');
    global_print("suc", "free: "                , freeTotal, '\n');
    global_print("suc", "free block count: "    , freeBlockCount, '\n');
    global_print("sfs", "fragmentation: "     , ((f64)(freeTotal - maxFreeBlock) / (f64)freeTotal) * 100.0, "%\n" );
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
u32 u64_to_string_hex(char* buffer, u32 buffer_size, u64 n) {

    static_assert(sizeof(char) == 1);
    // buffer too small
    if(buffer_size < 3) return 0;

    // can't divide by 0 early out
    buffer[0] = '0';
    buffer[1] = 'x';
    if (n == 0) {
        buffer[2] = '0';
        return 3;
    }

    i64 i = 0;
    u64 m = n;
    while (m != 0) {

        // compiler should optimize division by constant
        m /= 16;
        if(i != buffer_size) {
            // count number of digits to write upto the remaining buffer size
            i++;
        }
        else {
            // otherwise shift (n) one digit at a time
            n /= 16;
        }
    }

    const char digits[] = {
        '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'
    };

    u32 size = --i;
    // stop at i == 0
    for (; i > -1; i--) {
        // write digit
        buffer[i + 2] = digits[ n % 16 ];
        n /= 16;
    }
    return size + 3;
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
        case 'u':// u64
            {
                u64 arg = va_arg(args, u64);
                buffer += u64_to_string((char*)buffer, buffer_end - buffer, arg);
                break;
            }
        case 'p': // void*
            {
                u64 arg = va_arg(args, u64);
                buffer += u64_to_string_hex((char*)buffer, buffer_end - buffer, arg);
                break;
            }
        case 'f': // floating-point
            {
                f64 arg = va_arg(args, f64);
                buffer += f32_to_string((char*)buffer, buffer_end - buffer, (f32)arg, 7);
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
                    len = str_len(arg) - 1;
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
    init_global_print( make_linear_allocator((byte*)io_base, ioBufferSize) );

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


u64 ReadFile(const char* fileName, byte* buffer) {

    FILE* file = fopen(fileName, "r");
    u64 size = ~u64(0);
    if(file) {

        fseek(file, 0, SEEK_END);
        size = ftell(file);
        fseek(file, 0, SEEK_SET);

        fread(buffer, size, 1, file);
        fclose(file);
    }

    return size;
}
u64 ReadFileTerminated(const char* fileName, byte* buffer) {

    FILE* file = fopen(fileName, "r");
    u64 size = ~u64(0);
    if(file) {

        fseek(file, 0, SEEK_END);
        size = ftell(file);
        fseek(file, 0, SEEK_SET);

        fread(buffer, size, 1, file);
        buffer[size++] = 0;
        fclose(file);
    }

    return size;
}
byte* ReadFileTerminated(const char* fileName, byte* buffer, u32* size_) {

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

u32 u32_log2(u32 n) {
    return 31 - __builtin_clz(n);
}
f32 f32_log(f32 n, f32 base) {
    return (f32)u32_log2((u32)n) / (f32)u32_log2((u32)base);
}
void local_free_wrapper(LocalMallocState* state, void* mem, u32 size) {
    local_free(state, mem);
}

ImageDescriptor LoadBMP(const char* path, void* memory) {

    FILE* ImgHandle;
    ImgHandle = fopen(path, "r");
    if(!ImgHandle) {
        global_print("ssc", "file could not be opened ", path, '\n');
        return ImageDescriptor{};
    }

    fseek(ImgHandle, 0, SEEK_END);
    u32 sizeofBmp = ftell(ImgHandle);
    fseek(ImgHandle, 0, SEEK_SET);
    fread(memory + sizeofBmp, sizeofBmp, 1, ImgHandle);
    fclose(ImgHandle);

    byte* bmp = (byte*)memory + sizeofBmp;
    if (Mem<char>(bmp) != 'B' || Mem<char>(bmp + 1) != 'M') {
        global_print("s", "Not a bmp file\n");
        return ImageDescriptor{};
    }

    u32 compression = Mem<u32>(bmp + 30);
    if(compression != 0) {
        global_print("s", "Error image compressed\n");
        return ImageDescriptor{};
    }
    u32 imageOffSet = Mem<u32>(bmp + 10);
    i32 imageWidth  = Mem<i32>(bmp + 18);
    i32 imageHeight = Mem<i32>(bmp + 22);

    ImageDescriptor descriptor{};
    descriptor.img = (Pixel*)memory;
    descriptor.height = imageHeight;
    descriptor.width = imageWidth;
    u32 rowOffset = 0;

    if (imageWidth * 3 % 4 != 0) {
        rowOffset = 4 - (imageWidth * 3 % 4);
    }

    byte* img = bmp + imageOffSet;
    for (i32 i = 0; i < imageHeight; i++) {
        for (i32 k = 0; k < imageWidth; k++) {
            descriptor.img[i * imageWidth + k].b = img[(imageWidth * 3 + rowOffset) * i + k * 3 + 0];
            descriptor.img[i * imageWidth + k].g = img[(imageWidth * 3 + rowOffset) * i + k * 3 + 1];
            descriptor.img[i * imageWidth + k].r = img[(imageWidth * 3 + rowOffset) * i + k * 3 + 2];
            descriptor.img[i * imageWidth + k].a = 0;
            descriptor.img[i * imageWidth + k].a = (descriptor.img[i * imageWidth + k].mem == 0 ? 0 : 255);
        }
    }

    sizeof(std::shared_ptr<i32>);

    return descriptor;
}


struct PNGChunk {
    u32 length;
    u32 type;
    byte payload[];
};

struct __attribute__ ((packed)) IHDRPayload {
    u32 width;
    u32 height;
    u8 bitDepth;
    u8 colorType;
    u8 compressionMethod;
    u8 filterMethod;
    u8 interlaceMethod;
};

bool VerifyPngSignature(byte* mem) {

    bool ret = true;
    u8 signature[] = {137, 80, 78, 71, 13, 10, 26, 10};
    for(u32 i = 0; i < SIZE_OF_ARRAY(signature); i++) {
        ret &= (mem[i] == signature[i]);
    }
    return ret;
}

constexpr u32 PNG_TYPE_STR(char c0, char c1, char c2, char c3) {
    return ((u32)c0 << 24) | ((u32)c1 << 16) | ((u32)c2 << 8) | (u32)c3;
}
void ParsePNGMemory(byte* mem) {

    LOG_ASSERT(VerifyPngSignature(mem), "not png signature");
    mem += 8;

    bool ihdrFound = false;
    u32 chunkCounter = 0;
    
    IHDRPayload info;

    for(;;) {

        auto chunk = (PNGChunk*)mem;
        LOG_ASSERT(chunkCounter == 0 && chunk->type != PNG_TYPE_STR('I', 'H', 'D', 'R'), "IHDR is not the first chunk");
        switch(chunk->type) {
        case PNG_TYPE_STR('I', 'H', 'D', 'R'):
            {
                LOG_ASSERT(!ihdrFound, "multiple IHDR chunks");
                LOG_ASSERT(chunkCounter == 0, "IHDR is not the first chunk");
                ihdrFound = true;
                chunkCounter++;
                info = Mem<IHDRPayload>(chunk->payload);
                break;
            }
        case PNG_TYPE_STR('I', 'D', 'A', 'T'):
            {
                break;
            }
        case PNG_TYPE_STR('P', 'L', 'T', 'E'):
            {
                break;
            }
        }

        chunkCounter++;
        mem += chunk->length + sizeof(PNGChunk) + 4;
    }
}