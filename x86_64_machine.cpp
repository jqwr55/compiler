#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <alloca.h>
#include <memory.h>

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

enum SSAOpr : u32 {
    SSA_NONE,
    SSA_ANY,
    
    // ssa ops
    SSA_MEMORY_LOAD = 8,
    SSA_MEMORY_STORE = 9,
    SSA_CMP_EQ = 34,
    SSA_CMP_NOT_EQ = 35,
    SSA_CMP_LESS_THAN = 49,      // <
    SSA_CMP_BIGGER_THAN = 50,    // >
    SSA_CMP_LESS_THAN_OR_EQ = 51,
    SSA_CMP_BIGGER_THAN_OR_EQ = 52,
    SSA_LOGICAL_AND = 36,
    SSA_LOGICAL_OR,
    SSA_BITWISE_NOT,
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

    SSA_LOGICAL_NEG = 61,
    SSA_CONSTANT = 1,
    SSA_CALL = 14,
    SSA_CONVERT = 15,
    SSA_PHI_NODE = 68,
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
};
SSAOpr MIR2SSAopr[] = {
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,

    SSA_CONSTANT,
    SSA_MUL,
    SSA_DIV,
    SSA_DIV,

    SSA_NONE,
    SSA_NONE,
    SSA_MINUS,

    SSA_ADD,
    SSA_SUB,
    SSA_BITWISE_AND,
    SSA_BITWISE_OR,
    SSA_BITWISE_XOR,
    SSA_BITWISE_NOT,
    SSA_LEFT_BIT_SHIFT,
    SSA_RIGHT_BIT_SHIFT,

    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,
    SSA_NONE,

    SSA_CALL,
    JMP,
    BRANCH,
    RET,

};
enum MachineIROp : u32 {
    MIR_NONE,

    MIR_READ,
    MIR_WRITE,
    MIR_MEM,
    MIR_VREG, // virtual
    MIR_PREG, // physical
    MIR_CONSTANT,

    MIR_MUL,
    MIR_SDIV,
    MIR_UDIV,
    MIR_SREM,
    MIR_UREM,
    MIR_NEG,

    MIR_ADD,
    MIR_SUB,
    MIR_AND,
    MIR_OR,
    MIR_XOR,
    MIR_NOT,

    MIR_SHL,
    MIR_SHR,
    MIR_SAR,
    MIR_SAL,
    MIR_ROR,
    MIR_ROL,

    MIR_SETBITS,
    MIR_READBIT,

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
    RelativePointer<void> opChains;
    InstMIR* prev;
    InstMIR* next;
};
struct TypeExpr {
    u32 index;
};
struct OpMIR {
    u16 opr;
    u16 operandCount;
    TypeExpr type;
    u32 name;
    RelativePointer<void> operands;
};

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

    RelativePointer<u32> returnRegs;
    RelativePointer<u32> paramRegs;
    RelativePointer<u32> callerSavedRegs;
    RelativePointer<u32> calleeSavedRegs;
};
struct RegisterDescriptor {
    RelativePointer<char> str;
    RelativePointer<u32> clobbersOnWrite;
    bit_mask64 properties;
    u32 clobberCount;
    u32 width; // bit width
};
struct RegisterClassD {
    u32 registerCount;
    u32* regs;
};
struct RegisterClass {
    u32 registerCount;
    RelativePointer<u32> regs;
};

enum SSAPatternConstraintEnum : u32 {

    CONSTRAINT_SINGLE_USE,
    CONSTRAINT_NON_COMMUMATIVE,
    CONSTRAINT_MAX_CONSTANT_WIDTH,
    CONSTRAINT_NON_CONSTANT,
    CONSTRAINT_SPECIFIC_CONSANT,

    CONSTRAINT_COUNT,
};
struct SSAPatternConstraint {
    SSAPatternConstraintEnum type;
};
struct Constant {
    byte mem[8];
    u8 type;
};
struct SSAPatternConst : SSAPatternConstraint {
    Constant constant;
};

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

struct SSAPattern {
    u16 implements;
    u16 name;
    u16 constraintCount;
    u16 operandCount;
    RelativePointer<RelativePointer<SSAPattern>> operands;
    RelativePointer<SSAPatternConstraint> constraints;
    RelativePointer<OpMIR> link;
};
struct MIRMacro {
    u32 instCount;
    u32 patternCount;
    i64 cost;
    RelativePointer<RelativePointer<InstMIR>> insts;
    RelativePointer<RelativePointer<SSAPattern>> patterns;
};
struct VirtualRegisterMIR {
    u32 name;
    u32 regClass;
};
struct BitsMIR {
    u32 bit;
    RelativePointer<OpMIR> def;
};
struct PhysicalRegisterMIR {
    u32 reg;
};
struct InstDescriptor {
    MIRMacro macro;
    RelativePointer<char> str;
};

template<typename T>
struct RelativePointer {
    u64 off;
    RelativePointer() : off(0) {}
    RelativePointer(byte* base, T* ptr) {
        off = (byte*)ptr - base;
    }
    T* GetPtr(byte* base) {
        return (T*)(base + off);
    }
    void SetPtr(byte* base, T* ptr) {
        off = (byte*)ptr - base;
    }
};

struct ArhitectureDescriptor {
    RelativePointer<char> arhitecture;
    u32 wordSize;
    u32 registerCount;
    u32 instCount;
    u32 regClassCount;
    RelativePointer<RegisterDescriptor> regDescriptorTable;
    RelativePointer<RegisterClass> regClasses;
    RelativePointer<InstDescriptor> instDescriptorTable;

    ABI abi;
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

static InstDescriptor x86_64_instructions[] = {};
static ABI x86_64_linux_abi = {
    128,
    16,
    1,
    6,
    6,
    9,
    RelativePointer<u32>(nullptr, &x86_64_ret_reg),
    RelativePointer<u32>(nullptr, x86_64_param_regs),
    RelativePointer<u32>(nullptr, x86_64_caller_saved_regs),
    RelativePointer<u32>(nullptr, x86_64_callee_save_regs),
};

u32 x86_64_gpr_all[] = {
    0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
    16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
    32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
    48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,
};
u32 x86_64_gpr8[] = {
    48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,
};
u32 x86_64_gpr16[] = {
    32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
};
u32 x86_64_gpr32[] = {
    16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
};
u32 x86_64_gpr64[] = {
    0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
};
u32 x86_64_stack_registers[] = {
    7,23,39,55
};
u32 x86_64_xmm_n[] = {
    64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79
};
u32 x86_64_rflags_class[] = {
    80
};

RegisterClassD x86_64_reg_classes[] = {
    {64, x86_64_gpr_all},
    {16, x86_64_gpr64},
    {16, x86_64_gpr32},
    {16, x86_64_gpr16},
    {16, x86_64_gpr8},
    {4, x86_64_stack_registers},
    {16, x86_64_xmm_n},
    {1, x86_64_rflags_class},
};
const char* x86_64_register_str[] = {
    "rax",
    "rbx",
    "rcx",
    "rdx",
    "rsi",
    "rdi",
    "rbp",
    "rsp",
    "r8",
    "r9",
    "r10",
    "r11",
    "r12",
    "r13",
    "r14",
    "r15",
    "eax",
    "ebx",
    "ecx",
    "edx",
    "esi",
    "edi",
    "ebp",
    "esp",
    "r8d",
    "r9d",
    "r10d",
    "r11d",
    "r12d",
    "r13d",
    "r14d",
    "r15d",
    "ax",
    "bx",
    "cx",
    "dx",
    "si",
    "di",
    "bp",
    "sp",
    "r8w",
    "r9w",
    "r10w",
    "r11w",
    "r12w",
    "r13w",
    "r14w",
    "r15w",
    "al",
    "bl",
    "cl",
    "dl",
    "sil",
    "dil",
    "bpl",
    "spl",
    "r8b",
    "r9b",
    "r10b",
    "r11b",
    "r12b",
    "r13b",
    "r14b",
    "r15b",
    "xmm0",
    "xmm1",
    "xmm2",
    "xmm3",
    "xmm4",
    "xmm5",
    "xmm6",
    "xmm7",
    "xmm8",
    "xmm9",
    "xmm10",
    "xmm11",
    "xmm12",
    "xmm13",
    "xmm14",
    "xmm15",
    "rflag",
};
u32 x86_64_reg_width[] = {
    64,
    64,
    64,
    64,
    64,
    64,
    64,
    64,
    64,
    64,
    64,
    64,
    64,
    64,
    64,
    64,

    32,
    32,
    32,
    32,
    32,
    32,
    32,
    32,
    32,
    32,
    32,
    32,
    32,
    32,
    32,
    32,

    16,
    16,
    16,
    16,
    16,
    16,
    16,
    16,
    16,
    16,
    16,
    16,
    16,
    16,
    16,
    16,

    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,

    128,
    128,
    128,
    128,
    128,
    128,
    128,
    128,
    128,
    128,
    128,
    128,
    128,
    128,
    128,
    128,

    64,
};


u32 global_mir_names = 0;
OpMIR* MakeBinaryOpMIR(LinearAllocator* allocator, MachineIROp op, OpMIR* operand0, OpMIR* operand1) {

    auto operation = (OpMIR*)linear_allocate(allocator, sizeof(OpMIR));
    operation->name = global_mir_names++;
    operation->operandCount = 2;

    auto operands = (OpMIR**)linear_allocate(allocator, sizeof(OpMIR*) * 2);
    operands[0] = operand0;
    operands[1] = operand1;
    operation->operands.SetPtr(allocator->base, operands);
    operation->opr = op;

    return operation;
}
OpMIR* MakeUnaryOpMIR(LinearAllocator* allocator, MachineIROp op, void* operand0) {

    auto operation = (OpMIR*)linear_allocate(allocator, sizeof(OpMIR));
    operation->name = global_mir_names++;
    operation->operandCount = 1;
    operation->operands.SetPtr(allocator->base, operand0);
    operation->opr = op;

    return operation;
}
OpMIR* MakeConstantMIR(LinearAllocator* allocator) {

    auto c = (OpMIR*)linear_allocate(allocator, sizeof(OpMIR));
    c->name = global_mir_names++;
    c->operandCount = 1;
    c->opr = MIR_CONSTANT;

    return c;
}

u32 global_vreg_names = 0;
VirtualRegisterMIR* MakeVregMIR(LinearAllocator* allocator, u32 vregClass) {
    auto vreg = (VirtualRegisterMIR*)linear_allocate(allocator, sizeof(VirtualRegisterMIR));
    vreg->regClass = vregClass;
    return vreg;
}

// OpMIR* make_x86_64_
OpMIR* make_x86_64_vreg_write(LinearAllocator* allocator, VirtualRegisterMIR* vreg, OpMIR* def) {

    return MakeBinaryOpMIR(allocator, MIR_WRITE,
        MakeUnaryOpMIR(allocator, MIR_VREG, vreg),
        def
    );
}
OpMIR* make_x86_64_vreg_def(LinearAllocator* allocator, OpMIR* vreg, OpMIR* def) {
    return MakeBinaryOpMIR(allocator, MIR_WRITE, vreg, def);
}
OpMIR* make_x86_64_vreg_read(LinearAllocator* allocator, OpMIR* vreg) {
    return MakeUnaryOpMIR(allocator, MIR_READ, vreg);
}
OpMIR* make_x86_64_vreg_use(LinearAllocator* allocator, VirtualRegisterMIR* use) {
    return MakeUnaryOpMIR(allocator, MIR_READ, MakeUnaryOpMIR(allocator, MIR_VREG, use));
}
OpMIR* make_x86_64_preg_use(LinearAllocator* allocator, PhysicalRegisterMIR* use) {
    return MakeUnaryOpMIR(allocator, MIR_READ, MakeUnaryOpMIR(allocator, MIR_PREG, use));
}
OpMIR* make_x86_64_mem_read(LinearAllocator* allocator, OpMIR* add) {
    return MakeUnaryOpMIR(allocator, MIR_READ, MakeUnaryOpMIR(allocator, MIR_MEM, add));
}
OpMIR* make_x86_64_mem_write(LinearAllocator* allocator, OpMIR* add, OpMIR* vreg) {
    return MakeBinaryOpMIR(allocator, MIR_WRITE, 
        MakeUnaryOpMIR(allocator, MIR_MEM, add),
        vreg
    );
}
OpMIR* make_x86_64_base_plus_disp(LinearAllocator* allocator, OpMIR* base, OpMIR* disp) {
    return MakeBinaryOpMIR(allocator, MIR_ADD, base, disp);
}
OpMIR* make_x86_64_base_plus_index(LinearAllocator* allocator, OpMIR* base, OpMIR* index) {
    return MakeBinaryOpMIR(allocator, MIR_ADD, base, index);
}
OpMIR* make_x86_64_base_plus_index_plus_disp(LinearAllocator* allocator, OpMIR* base, OpMIR* index, OpMIR* disp) {
    // (base + index) + disp
    return 
        MakeBinaryOpMIR(allocator, MIR_ADD,
            MakeBinaryOpMIR(allocator, MIR_ADD, base, index),
            disp
    );
}
OpMIR* make_x86_64_base_plus_index_mul_scale(LinearAllocator* allocator, OpMIR* base, OpMIR* index, OpMIR* scale) {
    // base + (index * scale)
    return 
        MakeBinaryOpMIR(allocator, MIR_ADD,
            base,
            MakeBinaryOpMIR(allocator, MIR_SHL, index, scale)
    );
}
OpMIR* make_x86_64_index_mul_scale_plus_disp(LinearAllocator* allocator, OpMIR* index, OpMIR* scale, OpMIR* disp) {
    
    // (index * scale) + disp
    return 
        MakeBinaryOpMIR(allocator, MIR_ADD,
            MakeBinaryOpMIR(allocator, MIR_SHL, index, scale),
            disp
    );
}
OpMIR* make_x86_64_base_plus_index_mul_scale_plus_disp(LinearAllocator* allocator,  OpMIR* base, OpMIR* index, OpMIR* scale, OpMIR* disp) {
    
    // base + ( (index * scale) + disp )
    return 
        MakeBinaryOpMIR(allocator, MIR_ADD, base,
            MakeBinaryOpMIR(allocator, MIR_ADD,
                MakeBinaryOpMIR(allocator, MIR_SHL, index, scale),
                disp
            )
    );
}


OpMIR* make_x86_64_setbits(LinearAllocator* ctx, u32 bitCount, OpMIR* result, u32* bits, OpMIR** resultBits) {

    auto def = (OpMIR*)linear_allocate(ctx, sizeof(OpMIR));
    def->name = global_mir_names++;
    def->opr = MIR_SETBITS;
    def->operandCount = bitCount + 1;

    auto bitsOverwriten = (BitsMIR*)linear_allocate(ctx, sizeof(OpMIR*) + sizeof(BitsMIR) * bitCount);
    auto operands = (void*)bitsOverwriten++;
    Mem<OpMIR*>(operands) = result;
    for(u32 i = 0; i < bitCount; i++) {
        bitsOverwriten[i].bit = bits[i];
        bitsOverwriten[i].def.SetPtr(ctx->base, resultBits[i]);
    }

    def->operands.SetPtr(ctx->base, operands);

    return def;
}

enum Flags : u32 {

    FLAG_CARRY_FLAG,
    FLAG_PARITY_FLAG,
    FLAG_ADJUT_FLAG,
    FLAG_ZERO_FLAG,
    FLAG_SIGN_FLAG,
    FLAG_OVERFLOW_FLAG,

    FLAG_COUNT
};
u32 x86_64_rflag_bits[] = {
    0,
    2,
    4,
    6,
    7,
    11,
};

enum x86_64_register_enums : u32{

    reg_rax,
    reg_rbx,
    reg_rcx,
    reg_rdx,
    reg_rsi,
    reg_rdi,
    reg_rbp,
    reg_rsp,
    reg_r8,
    reg_r9,
    reg_r10,
    reg_r11,
    reg_r12,
    reg_r13,
    reg_r14,
    reg_r15,
    reg_eax,
    reg_ebx,
    reg_ecx,
    reg_edx,
    reg_esi,
    reg_edi,
    reg_ebp,
    reg_esp,
    reg_r8d,
    reg_r9d,
    reg_r10d,
    reg_r11d,
    reg_r12d,
    reg_r13d,
    reg_r14d,
    reg_r15d,
    reg_ax,
    reg_bx,
    reg_cx,
    reg_dx,
    reg_si,
    reg_di,
    reg_bp,
    reg_sp,
    reg_r8w,
    reg_r9w,
    reg_r10w,
    reg_r11w,
    reg_r12w,
    reg_r13w,
    reg_r14w,
    reg_r15w,
    reg_al,
    reg_bl,
    reg_cl,
    reg_dl,
    reg_sil,
    reg_dil,
    reg_bpl,
    reg_spl,
    reg_r8b,
    reg_r9b,
    reg_r10b,
    reg_r11b,
    reg_r12b,
    reg_r13b,
    reg_r14b,
    reg_r15b,
    reg_xmm0,
    reg_xmm1,
    reg_xmm2,
    reg_xmm3,
    reg_xmm4,
    reg_xmm5,
    reg_xmm6,
    reg_xmm7,
    reg_xmm8,
    reg_xmm9,
    reg_xmm10,
    reg_xmm11,
    reg_xmm12,
    reg_xmm13,
    reg_xmm14,
    reg_xmm15,
    reg_rflag,
};

OpMIR* make_x86_64_rflags_setbits(LinearAllocator* allocator, u32 bitCount, BitsMIR** bits) {

    auto rflags = (OpMIR*)linear_allocate(allocator, sizeof(OpMIR));
    rflags->opr = MIR_PREG;
    rflags->name = global_mir_names++;
    rflags->operandCount = 1;
    
    auto preg = (PhysicalRegisterMIR*)linear_allocate(allocator, sizeof(PhysicalRegisterMIR));
    preg->reg = reg_rflag;
    rflags->operands.SetPtr(allocator->base, preg);

    auto def = (OpMIR*)linear_allocate(allocator, sizeof(OpMIR));
    def->name = global_mir_names++;

    def->opr = MIR_SETBITS;
    auto operands = (OpMIR**)linear_allocate(allocator, sizeof(OpMIR) * (bitCount+1));
    operands[0] = rflags;
    memcpy(operands + 1, bits, sizeof(BitsMIR**) * bitCount);

    def->operandCount = bitCount + 1;
    def->operands.SetPtr(allocator->base, operands);
    
    return def;
}

SSAPattern* MakePatternBasePlusDisp(LinearAllocator* ctx, OpMIR* base_plus_disp) {

    auto base = ((RelativePointer<OpMIR>*)base_plus_disp->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base);
    auto disp = ((RelativePointer<OpMIR>*)base_plus_disp->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);

    SSAPatternConstraint noConst[1] = {
        CONSTRAINT_NON_CONSTANT,
    };
    SSAPatternConstraint noReuse[1] = {
        CONSTRAINT_SINGLE_USE
    };

    auto patternBase = MakePatternAny(ctx, base, 1, noConst); // no constant
    auto patternConst = MakePattern(ctx, SSA_CONSTANT, disp, 1, noReuse); // no reuse
    auto plus = MakeBinaryPattern(ctx, base_plus_disp, SSA_ADD, patternBase, patternConst, 1, noReuse); // no reuse, comm

    return plus;
}
SSAPattern* MakePatternBasePlusIndex(LinearAllocator* ctx, OpMIR* base_plus_index) {

    auto base = ((RelativePointer<OpMIR>*)base_plus_index->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base);
    auto index = ((RelativePointer<OpMIR>*)base_plus_index->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);

    SSAPatternConstraint noConst[1] = {
        CONSTRAINT_NON_CONSTANT,
    };
    SSAPatternConstraint noReuse[1] = {
        CONSTRAINT_SINGLE_USE
    };

    auto patternBase = MakePatternAny(ctx, base, 1, noConst); // no constant, no reuse
    auto patternConst = MakePatternAny(ctx, index, 1, noConst); // no reuse
    auto plus = MakeBinaryPattern(ctx, base_plus_index, SSA_ADD, patternBase, patternConst, 1, noReuse); // no reuse, comm

    return plus;
}
void MakePatternBasePlusIndexPlusDisp(LinearAllocator* ctx, OpMIR* base_plus_index_plus_disp, SSAPattern** result) {

    auto disp = ((RelativePointer<OpMIR>*)base_plus_index_plus_disp->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base); // disp
    auto basePlusIndex = ((RelativePointer<OpMIR>*)base_plus_index_plus_disp->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);
    auto base = ((RelativePointer<OpMIR>*)basePlusIndex->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base);
    auto index = ((RelativePointer<OpMIR>*)basePlusIndex->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);

    // base + (index + disp)
    // (base + index) + disp
    // (base + disp) + index

    SSAPatternConstraint noConst[1] = {
        CONSTRAINT_NON_CONSTANT,
    };
    SSAPatternConstraint noReuse[1] = {
        CONSTRAINT_SINGLE_USE
    };

    auto patternBase = MakePatternAny(ctx, base, 1, noConst); // no constant, no reuse
    auto patternIndex = MakePatternAny(ctx, index, 1, noConst); // no constant, no reuse
    auto patternConst = MakePatternAny(ctx, index, 1, noReuse); // no reuse

    // base + (index + disp)
    auto patternIndexPlusDisp = MakeBinaryPattern(ctx, base_plus_index_plus_disp, SSA_ADD, patternBase, patternConst, 1, noReuse); // no reuse, comm
    auto patternBasePlus = MakeBinaryPattern(ctx, base_plus_index_plus_disp, SSA_ADD, patternIndex, patternConst, 1, noReuse); // no reuse, comm

    // (base + index) + disp
    auto patternBasePlusIndex = MakeBinaryPattern(ctx, base_plus_index_plus_disp, SSA_ADD, patternBase, patternIndex, 1, noReuse); // no reuse, comm
    auto patternDispPlus = MakeBinaryPattern(ctx, base_plus_index_plus_disp, SSA_ADD, patternConst, patternBasePlusIndex, 1, noReuse); // no reuse, comm

    // (base + disp) + index
    auto patternBasePlusDisp = MakeBinaryPattern(ctx, base_plus_index_plus_disp, SSA_ADD, patternBase, patternConst, 1, noReuse); // no reuse, comm
    auto patternIndexPlus = MakeBinaryPattern(ctx, base_plus_index_plus_disp, SSA_ADD, patternIndex, patternBasePlusDisp, 1, noReuse); // no reuse, comm

    result[0] = patternBasePlus;
    result[1] = patternDispPlus;
    result[2] = patternIndexPlus;
}
SSAPattern* MakePatternBasePlusIndexShiftScale(LinearAllocator* ctx, OpMIR* base_plus_index_shift_scale) {

    // base + (index * scale)
    auto base = ((RelativePointer<OpMIR>*)base_plus_index_shift_scale->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base);
    auto index_shift_scale = ((RelativePointer<OpMIR>*)base_plus_index_shift_scale->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);
    auto index = ((RelativePointer<OpMIR>*)index_shift_scale->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base);
    auto scale = ((RelativePointer<OpMIR>*)index_shift_scale->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);

    SSAPatternConstraint noConst[1] = {
        CONSTRAINT_NON_CONSTANT,
    };
    SSAPatternConstraint noReuse[1] = {
        CONSTRAINT_SINGLE_USE
    };

    auto patternBase = MakePatternAny(ctx, base, 1, noConst);
    auto patternIndex = MakePatternAny(ctx, index, 1, noConst);
    auto patternScale = MakePattern(ctx, SSA_CONSTANT, scale, 1, noReuse);

    auto patternShift = MakeBinaryPattern(ctx, index_shift_scale, SSA_LEFT_BIT_SHIFT, patternIndex, patternScale, 1, noReuse);
    auto patternAdd = MakeBinaryPattern(ctx, index_shift_scale, SSA_ADD, patternBase, patternShift, 1, noReuse);
    return patternAdd;
}
SSAPattern* MakePatternDispPlusIndexShiftScale(LinearAllocator* ctx, OpMIR* disp_plus_index_shift_scale) {

    // (index * scale) + disp
    auto index_shift_scale = ((RelativePointer<OpMIR>*)disp_plus_index_shift_scale->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base);
    auto disp = ((RelativePointer<OpMIR>*)disp_plus_index_shift_scale->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);
    auto index = ((RelativePointer<OpMIR>*)index_shift_scale->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base);
    auto scale = ((RelativePointer<OpMIR>*)index_shift_scale->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);

    SSAPatternConstraint noConst[1] = {
        CONSTRAINT_NON_CONSTANT,
    };
    SSAPatternConstraint noReuse[1] = {
        CONSTRAINT_SINGLE_USE
    };

    auto patternDisp = MakePattern(ctx, SSA_CONSTANT, disp, 1, noReuse);;
    auto patternIndex = MakePatternAny(ctx, index, 1, noConst);
    auto patternScale = MakePattern(ctx, SSA_CONSTANT, scale, 1, noReuse);

    auto patternShift = MakeBinaryPattern(ctx, index_shift_scale, SSA_LEFT_BIT_SHIFT, patternIndex, patternScale, 1, noReuse);
    auto patternAdd = MakeBinaryPattern(ctx, index_shift_scale, SSA_ADD, patternDisp, patternShift, 1, noReuse);
    return patternAdd;
}
void MakePatternBasePlusIndexShiftScalePlusDisp(LinearAllocator* ctx, OpMIR* base_plus_index_shift_scale_plus_disp, SSAPattern** result) {

    // base + ( (index * scale) + disp )
    auto base = ((RelativePointer<OpMIR>*)base_plus_index_shift_scale_plus_disp->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base);
    auto index_shift_scale_plus_disp = ((RelativePointer<OpMIR>*)base_plus_index_shift_scale_plus_disp->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);
    auto index_shift_scale = ((RelativePointer<OpMIR>*)index_shift_scale_plus_disp->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base);
    auto index = ((RelativePointer<OpMIR>*)index_shift_scale->operands.GetPtr(ctx->base))[0].GetPtr(ctx->base);
    auto scale = ((RelativePointer<OpMIR>*)index_shift_scale->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);
    auto disp = ((RelativePointer<OpMIR>*)index_shift_scale_plus_disp->operands.GetPtr(ctx->base))[1].GetPtr(ctx->base);

    SSAPatternConstraint noConst[1] = {
        CONSTRAINT_NON_CONSTANT,
    };
    SSAPatternConstraint noReuse[1] = {
        CONSTRAINT_SINGLE_USE
    };

    // base + ( index * scale + disp )
    // disp + ( index * scale + base )
    // (index * scale) + (base + disp)

    auto patternBase = MakePattern(ctx, SSA_CONSTANT, base, 1, noConst);
    auto patternDisp = MakePattern(ctx, SSA_CONSTANT, disp, 1, noReuse);
    auto patternIndex = MakePatternAny(ctx, index, 1, noConst);
    auto patternScale = MakePattern(ctx, SSA_CONSTANT, scale, 1, noReuse);

    auto patternShift = MakeBinaryPattern(ctx, index_shift_scale, SSA_LEFT_BIT_SHIFT, patternIndex, patternScale, 1, noReuse);
    auto patternShiftPlusDisp = MakeBinaryPattern(ctx, base_plus_index_shift_scale_plus_disp, SSA_ADD, patternShift, patternDisp, 1, noReuse);
    auto patternBasePlus = MakeBinaryPattern(ctx, index_shift_scale, SSA_ADD, patternBase, patternShiftPlusDisp, 1, noReuse);

    auto patternShiftPlusBase = MakeBinaryPattern(ctx, base_plus_index_shift_scale_plus_disp, SSA_ADD, patternShift, patternBase, 1, noReuse);
    auto patternDispPlus = MakeBinaryPattern(ctx, index_shift_scale, SSA_ADD, patternBase, patternShiftPlusBase, 1, noReuse);

    auto patternBasePlusDisp = MakeBinaryPattern(ctx, base_plus_index_shift_scale_plus_disp, SSA_ADD, patternBase, patternDisp, 1, noReuse);
    auto patternShiftPlus = MakeBinaryPattern(ctx, index_shift_scale, SSA_ADD, patternShift, patternBasePlusDisp, 1, noReuse);

    result[0] = patternBasePlus;
    result[1] = patternDispPlus;
    result[2] = patternShiftPlus;
}

RelativePointer<void> Helper(LinearAllocator* ctx, OpMIR* op, OpMIR* bin, u32 flagsCount, Flags* flagsAffected) {

    auto opChains = (RelativePointer<OpMIR>*)linear_allocate(ctx, sizeof(RelativePointer<OpMIR>) * 2);
    opChains[0].SetPtr(ctx->base, op);
    opChains[1].SetPtr(ctx->base, MakeSideEffectsChain(ctx, bin, flagsCount, flagsAffected));

    RelativePointer<void> ret;
    ret.SetPtr(ctx->base, opChains);
    return ret;
}

void MakeAllAddressingModes(LinearAllocator* ctx, MachineIROp baseOperation, bool commutative, u32 flagsCount, Flags* flagsAffected, MIRMacro* macros) {

    auto ssaOpr = MIR2SSAopr[baseOperation];

    auto nonCommutativeNoReuse = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(u32) * 2);
    nonCommutativeNoReuse[0].type = CONSTRAINT_NON_COMMUMATIVE;
    nonCommutativeNoReuse[1].type = CONSTRAINT_SINGLE_USE;

    auto nonCommutative = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(u32));
    nonCommutative->type = CONSTRAINT_NON_COMMUMATIVE;
    auto noReuse = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(u32));
    noReuse->type = CONSTRAINT_SINGLE_USE;
    auto noConstNoReuse = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(u32) * 2);
    noConstNoReuse[0].type = CONSTRAINT_NON_CONSTANT;
    noConstNoReuse[1].type = CONSTRAINT_SINGLE_USE;
    auto noConst = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(u32));
    noConst->type = CONSTRAINT_NON_CONSTANT;

    SSAPatternConstraint* binConstraint = nullptr;
    u32 binConstraintCount = 0;
    if(!commutative) {
        binConstraintCount = 1;
        binConstraint = nonCommutative;
    }

    for(u32 i = 0; i < 10; i++) {
        macros[i].instCount = 1;
        macros[i].cost = i+1;
    }

    // mov
    // r op r
    MakeSimplestMacro(ctx, baseOperation, commutative, flagsCount, flagsAffected, macros);
    {
        // r op r
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, vreg1_read);
        
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, make_x86_64_vreg_def(ctx, vreg0, bin), bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[1].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
       
        auto pattern = MakeBinaryPattern(ctx, bin, ssaOpr,
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakePatternAny(ctx, vreg1, 1, noConst),
            binConstraintCount, binConstraint
        );
        patterns[0].SetPtr(ctx->base, pattern);
        macros[1].patternCount = 1;
        macros[1].patterns.SetPtr(ctx->base, patterns);

    }
    {
        // r op imm0
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, imm0);
        
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, make_x86_64_vreg_def(ctx, vreg0, bin), bin, flagsCount, flagsAffected);
        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[2].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto pattern = MakeBinaryPattern(ctx, bin, ssaOpr, 
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakePattern(ctx, SSA_CONSTANT, imm0, 1, noReuse),
            binConstraintCount, binConstraint
        );
        patterns[0].SetPtr(ctx->base, pattern);
        macros[2].patternCount = 1;
        macros[2].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // r op [disp]
        auto mem_read = make_x86_64_mem_read(ctx, disp);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, mem_read);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, make_x86_64_vreg_def(ctx, vreg0, bin));
        inst->opChains = Helper(ctx, make_x86_64_vreg_def(ctx, vreg0, bin), bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[3].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto pattern = MakeBinaryPattern(ctx, bin, ssaOpr,
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD,
                MakePattern(ctx, SSA_CONSTANT, imm0, 1, noReuse),
                1, noReuse),
            binConstraintCount, binConstraint
        );
        patterns[0].SetPtr(ctx->base, pattern);
        macros[3].patternCount = 1;
        macros[3].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // r op [r]
        auto mem_read = make_x86_64_mem_read(ctx, base_read);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, mem_read);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, make_x86_64_vreg_def(ctx, vreg0, bin), bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[4].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto pattern = MakeBinaryPattern(ctx, bin, ssaOpr,
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD,
                MakePatternAny(ctx, vreg0, 0, nullptr),
                1, noReuse),
            binConstraintCount, binConstraint
        );
        patterns[0].SetPtr(ctx->base, pattern);
        macros[4].patternCount = 1;
        macros[4].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // r op [base + disp]
        auto mem_read = make_x86_64_mem_read(ctx, base_plus_disp);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, mem_read);
        
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, make_x86_64_vreg_def(ctx, vreg0, bin), bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[5].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto pattern = MakeBinaryPattern(ctx, bin, ssaOpr,
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD,
                MakePatternBasePlusDisp(ctx, base_plus_disp),
                1, noReuse),
            binConstraintCount, binConstraint
        );
        patterns[0].SetPtr(ctx->base, pattern);
        macros[5].patternCount = 1;
        macros[5].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // r op [base + index]
        auto mem_read = make_x86_64_mem_read(ctx, base_plus_index);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, mem_read);
        
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, make_x86_64_vreg_def(ctx, vreg0, bin), bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[6].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto pattern = MakeBinaryPattern(ctx, bin, ssaOpr,
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD,
                MakePatternBasePlusIndex(ctx, base_plus_index),
                1, noReuse),
            binConstraintCount, binConstraint
        );
        patterns[0].SetPtr(ctx->base, pattern);
        macros[6].patternCount = 1;
        macros[6].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // r op [base + index + disp]
        auto mem_read = make_x86_64_mem_read(ctx, base_plus_index_plus_disp);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, mem_read);
        
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, make_x86_64_vreg_def(ctx, vreg0, bin), bin, flagsCount, flagsAffected);
        
        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[7].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>) * 3);
        SSAPattern* memPattern[3];
        MakePatternBasePlusIndexPlusDisp(ctx, base_plus_index_plus_disp, memPattern);

        auto pattern0 = MakeBinaryPattern(ctx, bin, ssaOpr, 
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memPattern[0], 1 , noReuse),
            1, noReuse);
        auto pattern1 = MakeBinaryPattern(ctx, bin, ssaOpr, 
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memPattern[1], 1 , noReuse),
            1, noReuse);
        auto pattern2 = MakeBinaryPattern(ctx, bin, ssaOpr, 
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memPattern[2], 1 , noReuse),
            1, noReuse);
        
        patterns[0].SetPtr(ctx->base, pattern0);
        patterns[1].SetPtr(ctx->base, pattern1);
        patterns[2].SetPtr(ctx->base, pattern2);
        
        macros[7].patternCount = 3;
        macros[7].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // r op [base + (index * scale)]
        auto mem_read = make_x86_64_mem_read(ctx, base_plus_index_mul_scale);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, mem_read);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, make_x86_64_vreg_def(ctx, vreg0, bin), bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[8].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto pattern = MakeBinaryPattern(ctx, bin, ssaOpr,
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD,
                MakePatternBasePlusIndexShiftScale(ctx, base_plus_index_mul_scale),
                1, noReuse),
            binConstraintCount, binConstraint
        );
        patterns[0].SetPtr(ctx->base, pattern);
        macros[8].patternCount = 1;
        macros[8].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // r op [(index * scale) + disp]
        auto mem_read = make_x86_64_mem_read(ctx, index_mul_scale_plus_disp);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, mem_read);
        
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, make_x86_64_vreg_def(ctx, vreg0, bin), bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[9].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto pattern = MakeBinaryPattern(ctx, bin, ssaOpr,
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD,
                MakePatternDispPlusIndexShiftScale(ctx, index_mul_scale_plus_disp),
                1, noReuse),
            binConstraintCount, binConstraint
        );
        patterns[0].SetPtr(ctx->base, pattern);
        macros[9].patternCount = 1;
        macros[9].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // r op [base + (index * scale) + disp]
        auto mem_read = make_x86_64_mem_read(ctx, base_plus_index_mul_scale_plus_disp);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, mem_read);
        
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, make_x86_64_vreg_def(ctx, vreg0, bin), bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[10].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>) * 3);
        SSAPattern* memPattern[3];
        MakePatternBasePlusIndexShiftScalePlusDisp(ctx, base_plus_index_mul_scale_plus_disp, memPattern);

        auto pattern0 = MakeBinaryPattern(ctx, bin, ssaOpr, 
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memPattern[0], 1 , noReuse),
            1, noReuse);
        auto pattern1 = MakeBinaryPattern(ctx, bin, ssaOpr, 
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memPattern[1], 1 , noReuse),
            1, noReuse);
        auto pattern2 = MakeBinaryPattern(ctx, bin, ssaOpr, 
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memPattern[2], 1 , noReuse),
            1, noReuse);
        
        patterns[0].SetPtr(ctx->base, pattern0);
        patterns[1].SetPtr(ctx->base, pattern1);
        patterns[2].SetPtr(ctx->base, pattern2);
        
        macros[10].patternCount = 3;
        macros[10].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // [base] op r
        auto mem_read = make_x86_64_mem_read(ctx, base_read);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, mem_read, vreg1_read);
        auto mem_write = make_x86_64_mem_write(ctx, base_read, bin);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, mem_write, bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[11].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto patternAddress = MakePatternAny(ctx, vreg0, 1, noConst);
        auto pattern = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE, 
            patternAddress,
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, patternAddress, 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );

        patterns[0].SetPtr(ctx->base, pattern);
        macros[11].patternCount = 1;
        macros[11].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // [disp] op r
        auto mem_read = make_x86_64_mem_read(ctx, disp);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, mem_read, vreg1_read);
        auto mem_write = make_x86_64_mem_write(ctx, disp, bin);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, mem_write, bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[11].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto patternAddress = MakePatternAny(ctx, disp, 1, noReuse);
        auto pattern = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE, 
            patternAddress,
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, patternAddress, 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );

        patterns[0].SetPtr(ctx->base, pattern);
        macros[11].patternCount = 1;
        macros[11].patterns.SetPtr(ctx->base, patterns);
    }
    {
        
        // [base + disp] opr r
        auto mem_read = make_x86_64_mem_read(ctx, base_plus_disp);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, mem_read, vreg1_read);
        auto mem_write = make_x86_64_mem_write(ctx, base_plus_disp, bin);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, mem_write, bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[12].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto patternAddress = MakePatternBasePlusDisp(ctx, base_plus_disp);
        auto pattern = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE, 
            patternAddress,
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, patternAddress, 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );

        patterns[0].SetPtr(ctx->base, pattern);
        macros[12].patternCount = 1;
        macros[12].patterns.SetPtr(ctx->base, patterns);
    }
    {
        //[base + index] opr r
        auto mem_read = make_x86_64_mem_read(ctx, base_plus_index);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, mem_read, vreg1_read);
        auto mem_write = make_x86_64_mem_write(ctx, base_plus_index, bin);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, mem_write, bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[13].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto patternAddress = MakePatternBasePlusIndex(ctx, base_plus_index);
        auto pattern = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE, 
            patternAddress,
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, patternAddress, 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );

        patterns[0].SetPtr(ctx->base, pattern);
        macros[13].patternCount = 1;
        macros[13].patterns.SetPtr(ctx->base, patterns);
    }
    {

        // [base + index + disp] op r
        auto mem_read = make_x86_64_mem_read(ctx, base_plus_index_plus_disp);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, mem_read, vreg1_read);
        auto mem_write = make_x86_64_mem_write(ctx, base_plus_index_plus_disp, bin);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, mem_write, bin, flagsCount, flagsAffected);
        
        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[14].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>) * 3);
        SSAPattern* memAddresses[3];
        MakePatternBasePlusIndexPlusDisp(ctx, base_plus_index_plus_disp, memAddresses);
        auto pattern0 = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE, 
            memAddresses[0],
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memAddresses[0], 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );
        auto pattern1 = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE, 
            memAddresses[1],
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memAddresses[1], 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );
        auto pattern2 = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE, 
            memAddresses[2],
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memAddresses[2], 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );

        patterns[0].SetPtr(ctx->base, pattern0);
        patterns[1].SetPtr(ctx->base, pattern1);
        patterns[2].SetPtr(ctx->base, pattern2);

        macros[14].patternCount = 3;
        macros[14].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // [base + (index * scale)] op r
        auto mem_read = make_x86_64_mem_read(ctx, base_plus_index_mul_scale);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, mem_read, vreg1_read);
        auto mem_write = make_x86_64_mem_write(ctx, base_plus_index_mul_scale, bin);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, mem_write, bin, flagsCount, flagsAffected);
        
        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[15].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto patternAddress = MakePatternBasePlusIndexShiftScale(ctx, base_plus_index_mul_scale);
        auto pattern = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE, 
            patternAddress,
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, patternAddress, 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );

        patterns[0].SetPtr(ctx->base, pattern);
        macros[15].patternCount = 1;
        macros[15].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // [(index * scale) + disp] op r
        auto mem_read = make_x86_64_mem_read(ctx, index_mul_scale_plus_disp);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, mem_read, vreg1_read);
        auto mem_write = make_x86_64_mem_write(ctx, index_mul_scale_plus_disp, bin);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, mem_write, bin, flagsCount, flagsAffected);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[16].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
        auto patternAddress = MakePatternDispPlusIndexShiftScale(ctx, index_mul_scale_plus_disp);
        auto pattern = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE,
            patternAddress,
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, patternAddress, 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );

        patterns[0].SetPtr(ctx->base, pattern);
        macros[16].patternCount = 1;
        macros[16].patterns.SetPtr(ctx->base, patterns);
    }
    {
        // [base + (index * scale) + disp] op r
        auto mem_read = make_x86_64_mem_read(ctx, base_plus_index_mul_scale_plus_disp);
        auto bin = MakeBinaryOpMIR(ctx, baseOperation, mem_read, vreg1_read);
        auto mem_write = make_x86_64_mem_write(ctx, base_plus_index_mul_scale_plus_disp, bin);

        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains = Helper(ctx, mem_write, bin, flagsCount, flagsAffected);
        
        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr[0].SetPtr(ctx->base, inst);
        macros[17].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>) * 3);

        SSAPattern* memAddresses[3];
        MakePatternBasePlusIndexShiftScalePlusDisp(ctx, base_plus_index_mul_scale_plus_disp, memAddresses);
        auto pattern0 = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE,
            memAddresses[0],
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memAddresses[0], 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );
        auto pattern1 = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE,
            memAddresses[1],
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memAddresses[1], 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );
        auto pattern2 = 
        MakeBinaryPattern(ctx, mem_write, SSA_MEMORY_STORE,
            memAddresses[2],
            MakeBinaryPattern(ctx, bin, ssaOpr,
                MakeUnaryPattern(ctx, mem_read, SSA_MEMORY_LOAD, memAddresses[2], 1, noReuse),
                MakePatternAny(ctx, vreg1, 1, noConst),
                binConstraintCount, binConstraint
            ),
            1, nonCommutative
        );

        patterns[0].SetPtr(ctx->base, pattern0);
        patterns[1].SetPtr(ctx->base, pattern1);
        patterns[2].SetPtr(ctx->base, pattern2);

        macros[17].patternCount = 3;
        macros[17].patterns.SetPtr(ctx->base, patterns);
    }
}

OpMIR* MakeSideEffectsChain(LinearAllocator* ctx, OpMIR* op, u32 flagsCount, Flags* flagsAffected) {

    OpMIR** chains = (OpMIR**)linear_allocate(ctx, sizeof(OpMIR*) * 2);
    auto bits = (BitsMIR*)linear_allocate(ctx, sizeof(BitsMIR) * flagsCount);
    auto bitsPtr = (BitsMIR**)linear_allocate(ctx, sizeof(BitsMIR*) * flagsCount);
    for(u32 i = 0; i < flagsCount; i++) {
        bitsPtr[i] = bits + i;
        bits[i].bit = x86_64_rflag_bits[flagsAffected[i]];
        bits[i].def.SetPtr(ctx->base, MakeUnaryOpMIR(ctx, (MachineIROp)(flagsAffected[i] + MIR_CARRY_FLAG), op));
    }

    return make_x86_64_rflags_setbits(ctx, flagsCount, bitsPtr);
}
InstMIR* make_x86_64_mov_vreg(LinearAllocator* ctx, OpMIR* dest, OpMIR* source) {

    auto ret = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
    ret->chainCount = 1;

    auto chain = (RelativePointer<RelativePointer<OpMIR>>*)linear_allocate(ctx, sizeof(RelativePointer<RelativePointer<OpMIR>>));
    ret->opChains.SetPtr(ctx->base, chain);

    auto mov = MakeBinaryOpMIR(ctx, MIR_WRITE, dest, MakeUnaryOpMIR(ctx, MIR_READ, source));
    chain->GetPtr(ctx->base)->SetPtr(ctx->base, mov);

    return ret;
}
InstMIR* make_x86_64_mov_preg_vreg(LinearAllocator* ctx, u32 dst, OpMIR* source) {

    auto ret = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
    ret->chainCount = 1;

    auto chain = (RelativePointer<RelativePointer<OpMIR>>*)linear_allocate(ctx, sizeof(RelativePointer<RelativePointer<OpMIR>>));
    ret->opChains.SetPtr(ctx->base, chain);

    auto preg = (PhysicalRegisterMIR*)linear_allocate(ctx, sizeof(PhysicalRegisterMIR));
    preg->reg = dst;

    auto mov = MakeBinaryOpMIR(ctx, MIR_WRITE, MakeUnaryOpMIR(ctx, MIR_PREG, preg), MakeUnaryOpMIR(ctx, MIR_READ, source));
    chain->GetPtr(ctx->base)->SetPtr(ctx->base, mov);

    return ret;
}

SSAPattern* MakeBinaryPattern(LinearAllocator* ctx, OpMIR* link, SSAOpr opr, SSAPattern* operand0, SSAPattern* operand1, u32 constraintCount, SSAPatternConstraint* constraints) {

    auto bin = (SSAPattern*)linear_allocate(ctx, sizeof(SSAPattern));
    bin->implements = opr;
    bin->link.SetPtr(ctx->base, link);
    bin->operandCount = 2;

    auto operands = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>) * 2);
    bin->operands.SetPtr(ctx->base, operands);

    operands[0].SetPtr(ctx->base, operand0);
    operands[1].SetPtr(ctx->base, operand1);

    bin->constraintCount = constraintCount;
    if(constraintCount) {
        bin->constraints.SetPtr(ctx->base, (SSAPatternConstraint*)linear_allocate(ctx, sizeof(SSAPatternConstraint) * constraintCount));
        memcpy(bin->constraints.GetPtr(ctx->base), constraints, sizeof(SSAPatternConstraint) * constraintCount);
    }

    return bin;
}
SSAPattern* MakeUnaryPattern(LinearAllocator* ctx, OpMIR* link, SSAOpr opr, SSAPattern* operand0, u32 constraintCount, SSAPatternConstraint* constraints) {

    auto unary = (SSAPattern*)linear_allocate(ctx, sizeof(SSAPattern));
    unary->implements = opr;
    unary->link.SetPtr(ctx->base, link);

    unary->operandCount = 1;

    auto operands = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>));
    unary->operands.SetPtr(ctx->base, operands);

    operands[0].SetPtr(ctx->base, operand0);

    unary->constraintCount = constraintCount;
    if(constraintCount) {
        unary->constraints.SetPtr(ctx->base, (SSAPatternConstraint*)linear_allocate(ctx, sizeof(SSAPatternConstraint) * constraintCount));
        memcpy(unary->constraints.GetPtr(ctx->base), constraints, sizeof(SSAPatternConstraint) * constraintCount);
    }

    return unary;
}

SSAPattern* MakePatternAny(LinearAllocator* ctx, OpMIR* link, u32 constraintCount, SSAPatternConstraint* constraints) {

    auto ret = (SSAPattern*)linear_allocate(ctx, sizeof(SSAPattern));
    ret->link.SetPtr(ctx->base, link);
    ret->operandCount = 0;
    ret->implements = SSA_ANY;

    ret->constraintCount = constraintCount;
    if(constraintCount) {
        ret->constraints.SetPtr(ctx->base, (SSAPatternConstraint*)linear_allocate(ctx, sizeof(SSAPatternConstraint) * constraintCount));
        memcpy(ret->constraints.GetPtr(ctx->base), constraints, sizeof(SSAPatternConstraint) * constraintCount);
    }

    return ret;
}
SSAPattern* MakePattern(LinearAllocator* ctx, SSAOpr implements, OpMIR* link, u32 constraintCount, SSAPatternConstraint* constraints) {

    auto ret = (SSAPattern*)linear_allocate(ctx, sizeof(SSAPattern));
    ret->link.SetPtr(ctx->base, link);
    ret->operandCount = 0;
    ret->implements = implements;

    ret->constraintCount = constraintCount;
    if(constraintCount) {
        ret->constraints.SetPtr(ctx->base, (SSAPatternConstraint*)linear_allocate(ctx, sizeof(SSAPatternConstraint) * constraintCount));
        memcpy(ret->constraints.GetPtr(ctx->base), constraints, sizeof(SSAPatternConstraint) * constraintCount);
    }

    return ret;
}

void MakeSimplestMacro(LinearAllocator* ctx, MachineIROp baseOperation, bool commutative, u32 flagsCount, Flags* flagsAffected, MIRMacro* result) {

    auto vreg0 = MakeVregMIR(ctx, 0);
    auto vreg1 = MakeVregMIR(ctx, 0);

    auto use0 = MakeUnaryOpMIR(ctx, MIR_VREG, vreg0);
    auto use1 = MakeUnaryOpMIR(ctx, MIR_VREG, vreg1);

    auto bin = MakeBinaryOpMIR(ctx, baseOperation, use0, use1);
    auto mainChain =  make_x86_64_vreg_def(ctx, use0, bin);

    SSAPatternConstraint* constraint = nullptr;
    u32 constraintCount = 0;
    if(!commutative) {
        constraintCount = 1;
        auto constraint = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(u32));
        constraint->type = CONSTRAINT_NON_COMMUMATIVE;
    }

    auto pattern = 
        MakeBinaryPattern(ctx, bin, MIR2SSAopr[baseOperation], 
            MakePatternAny(ctx, use0, 0, nullptr),
            MakePatternAny(ctx, use1, 0, nullptr),
            constraintCount, constraint
    );

    auto movInst = make_x86_64_mov_vreg(ctx, use0, use1);

    auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
    inst->chainCount = 2;
    inst->opChains.SetPtr(ctx->base, (RelativePointer<OpMIR>*)linear_allocate(ctx, sizeof(RelativePointer<OpMIR>) * 2));
    ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))[0].SetPtr(ctx->base, mainChain);
    ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))[1].SetPtr(ctx->base, MakeSideEffectsChain(ctx, bin, flagsCount, flagsAffected));
    
    result->patternCount = 1;
    result->patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>) ));
    result->patterns.GetPtr(ctx->base)->SetPtr(ctx->base, pattern);

    result->instCount = 1;
    result->insts.SetPtr(ctx->base, (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>) * 2));
    result->insts.GetPtr(ctx->base)[0].SetPtr(ctx->base, movInst);
    result->insts.GetPtr(ctx->base)[1].SetPtr(ctx->base, inst);

    result->cost = 1;
}

u32 GetRegister(const char* name) {

    for(u32 i = 0; i < 81; i++) {
        if(str_cmp(name, x86_64_register_str[i])) return i;
    }
    ASSERT(false);
}
OpMIR* MakePreg(LinearAllocator* ctx, const char* name) {

    auto preg = (PhysicalRegisterMIR*)linear_allocate(ctx, sizeof(PhysicalRegisterMIR));
    preg->reg = GetRegister(name);

    return MakeUnaryOpMIR(ctx, MIR_PREG, preg);
}

void MakeShiftMacros(LinearAllocator* ctx, MachineIROp baseOperation, MIRMacro* result) {

    auto ssaOpr = MIR2SSAopr[baseOperation];

    {
        // op r, 1
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, imm0);
        auto mainChain = make_x86_64_vreg_def(ctx, vreg0, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[0].insts.SetPtr(ctx->base, instPtr);

        auto pattern = MakeBinaryPattern(ctx, shl, ssaOpr,
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakePattern(ctx, SSA_CONSTANT, imm0, 2, noReuseConstOne),
            1, nonCommutative
        );

        result[0].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[0].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, pattern);
        result[0].patternCount = 1;
        result[0].instCount = 1;
    }
    {
        // op r, imm8
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, imm0);
        auto mainChain = make_x86_64_vreg_def(ctx, vreg0, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[1].insts.SetPtr(ctx->base, instPtr);

        auto pattern = MakeBinaryPattern(ctx, shl, ssaOpr,
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakePattern(ctx, SSA_CONSTANT, imm0, 1, noReuse),
            1, nonCommutative
        );

        result[1].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[1].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, pattern);
        result[1].patternCount = 1;
        result[1].instCount = 1;
    }
    {
        // mov cl, vreg1
        // op r, cl

        auto clReg = MakePreg(ctx, "cl");

        make_x86_64_mov_preg_vreg(ctx, GetRegister("cl"), vreg1);
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, vreg0_read, mem_disp);
        auto mainChain = make_x86_64_vreg_def(ctx, vreg0, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[2].insts.SetPtr(ctx->base, instPtr);

        auto pattern = MakeBinaryPattern(ctx, shl, ssaOpr,
            MakePatternAny(ctx, vreg0, 1, noConst),
            MakePatternAny(ctx, vreg1, 1, noConst),
            1, nonCommutative
        );

        result[2].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[2].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, pattern);
        result[2].instCount = 1;
        result[2].patternCount = 1;
    }
    {
        // op [disp], imm8
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, mem_disp, imm0);
        auto mainChain = make_x86_64_mem_write(ctx, disp, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[3].insts.SetPtr(ctx->base, instPtr);

        auto patternDisp = MakePattern(ctx, SSA_CONSTANT, disp, 1,noReuse);
        auto pattern = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternDisp,
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_disp, SSA_MEMORY_LOAD, patternDisp, 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );

        result[3].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[3].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, pattern);
        result[3].instCount = 1;
        result[3].patternCount = 1;
    }
    {
        // op [base], imm8
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, mem_base, imm0);
        auto mainChain = make_x86_64_mem_write(ctx, disp, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[4].insts.SetPtr(ctx->base, instPtr);

        auto patternBase = MakePatternAny(ctx, base, 1, noConst);
        auto pattern = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternBase,
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base, SSA_MEMORY_LOAD, patternBase, 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );

        result[4].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[4].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, pattern);
        result[4].instCount = 1;
        result[4].patternCount = 1;
    }
    {
        // op [base + disp], imm8
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, mem_base_plus_disp, imm0);
        auto mainChain = make_x86_64_mem_write(ctx, base_plus_disp, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[5].insts.SetPtr(ctx->base, instPtr);

        auto patternBasePlusDisp = MakePatternBasePlusDisp(ctx, base_plus_disp);
        auto pattern = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternBasePlusDisp,
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base_plus_disp, SSA_MEMORY_LOAD, patternBasePlusDisp, 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );

        result[5].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[5].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, pattern);
        result[5].instCount = 1;
        result[5].patternCount = 1;
    }
    {
        // op [base + index], imm8       
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, mem_base_plus_index, imm0);
        auto mainChain = make_x86_64_mem_write(ctx, base_plus_index, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[6].insts.SetPtr(ctx->base, instPtr);

        auto patternBasePlusIndex = MakePatternBasePlusIndex(ctx, base_plus_index);
        auto pattern = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternBasePlusIndex,
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base_plus_index, SSA_MEMORY_LOAD, patternBasePlusIndex, 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );

        result[6].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[6].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, pattern);
        result[6].instCount = 1;
        result[6].patternCount = 1;
    }
    {
        // op [base + index + disp], imm8
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, mem_base_plus_index_plus_disp, imm0);
        auto mainChain = make_x86_64_mem_write(ctx, base_plus_index_plus_disp, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[7].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>) * 3);
        SSAPattern* patternAddresses[3];
        MakePatternBasePlusIndexPlusDisp(ctx, base_plus_index_plus_disp, patternAddresses);
        
        auto pattern0 = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternAddresses[0],
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base_plus_index_plus_disp, SSA_MEMORY_LOAD, patternAddresses[0], 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );
        auto pattern1 = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternAddresses[1],
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base_plus_index_plus_disp, SSA_MEMORY_LOAD, patternAddresses[1], 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );
        auto pattern2 = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternAddresses[2],
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base_plus_index_plus_disp, SSA_MEMORY_LOAD, patternAddresses[2], 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );
        
        patterns[0].SetPtr(ctx->base, pattern0);
        patterns[1].SetPtr(ctx->base, pattern1);
        patterns[2].SetPtr(ctx->base, pattern2);

        result[7].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[7].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, (SSAPattern*)patterns);
        result[7].instCount = 1;
        result[7].patternCount = 3;
    }
    {
        // op [base + index * scale], imm8
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, mem_base_plus_index_mul_scale, imm0);
        auto mainChain = make_x86_64_mem_write(ctx, base_plus_index_mul_scale, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[8].insts.SetPtr(ctx->base, instPtr);

        SSAPattern* patternAddress = MakePatternBasePlusIndexShiftScale(ctx, base_plus_index_mul_scale);
        
        auto pattern0 = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternAddress,
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base, SSA_MEMORY_LOAD, patternAddress, 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );
        
        result[8].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[8].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, patternAddress);
        result[8].instCount = 1;
        result[8].patternCount = 1;
    }
    {
        // op [index * scale + disp], imm8
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, mem_index_mul_scale_plus_disp, imm0);
        auto mainChain = make_x86_64_mem_write(ctx, index_mul_scale_plus_disp, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[9].insts.SetPtr(ctx->base, instPtr);

        SSAPattern* patternAddress = MakePatternDispPlusIndexShiftScale(ctx, index_mul_scale_plus_disp);
        auto pattern0 = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternAddress,
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base, SSA_MEMORY_LOAD, patternAddress, 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );
        
        result[9].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[9].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, patternAddress);
        result[9].instCount = 1;
        result[9].patternCount = 1;
    }
    {
        // op [base + index * scale + disp], imm8
        auto shl = MakeBinaryOpMIR(ctx, baseOperation, mem_base_plus_index_mul_scale_plus_disp, imm0);
        auto mainChain = make_x86_64_mem_write(ctx, base_plus_index_mul_scale_plus_disp, shl);
        auto inst = (InstMIR*)linear_allocate(ctx, sizeof(InstMIR));
        inst->opChains.SetPtr(ctx->base, linear_allocate(ctx, sizeof(RelativePointer<OpMIR>)));
        ((RelativePointer<OpMIR>*)inst->opChains.GetPtr(ctx->base))->SetPtr(ctx->base, mainChain);

        auto instPtr = (RelativePointer<InstMIR>*)linear_allocate(ctx, sizeof(RelativePointer<InstMIR>));
        instPtr->SetPtr(ctx->base, inst);
        result[10].insts.SetPtr(ctx->base, instPtr);

        auto patterns = (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>) * 3);
        SSAPattern* patternAddresses[3];
        MakePatternBasePlusIndexShiftScalePlusDisp(ctx, base_plus_index_mul_scale_plus_disp, patternAddresses);

        auto pattern0 = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternAddresses[0],
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base, SSA_MEMORY_LOAD, patternAddresses[0], 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );
        auto pattern1 = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternAddresses[1],
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base, SSA_MEMORY_LOAD, patternAddresses[1], 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );
        auto pattern2 = MakeBinaryPattern(ctx, mainChain, SSA_MEMORY_STORE,
            patternAddresses[2],
            MakeBinaryPattern(ctx, shl, ssaOpr,
                MakeUnaryPattern(ctx, mem_base, SSA_MEMORY_LOAD, patternAddresses[2], 1, noReuse),
                MakePattern(ctx, SSA_CONSTANT, imm0, 1,noReuse),
                1, nonCommutative
            ),
            1, nonCommutative
        );

        patterns[0].SetPtr(ctx->base, pattern0);
        patterns[1].SetPtr(ctx->base, pattern1);
        patterns[2].SetPtr(ctx->base, pattern2);
        
        result[10].patterns.SetPtr(ctx->base, (RelativePointer<SSAPattern>*)linear_allocate(ctx, sizeof(RelativePointer<SSAPattern>)));
        result[10].patterns.GetPtr(ctx->base)->SetPtr(ctx->base, (SSAPattern*)patterns);
        result[10].instCount = 1;
        result[10].patternCount = 1;
    }
}
// mov cl, vreg1
// op [base + disp], cl
// disp <= 32bit

ABI make_x86_64_linux_abi(LinearAllocator* ctx) {

    ABI ret = x86_64_linux_abi;

    auto ret_reg = (u32*)linear_allocate(ctx, sizeof(x86_64_ret_reg));
    auto param_regs = (u32*)linear_allocate(ctx, sizeof(x86_64_param_regs));
    auto caller_saved = (u32*)linear_allocate(ctx, sizeof(x86_64_caller_saved_regs));
    auto callee_save = (u32*)linear_allocate(ctx, sizeof(x86_64_callee_save_regs));

    memcpy(ret_reg, &x86_64_ret_reg, sizeof(x86_64_ret_reg));
    memcpy(param_regs, x86_64_param_regs, sizeof(x86_64_param_regs));
    memcpy(caller_saved, x86_64_caller_saved_regs, sizeof(x86_64_caller_saved_regs));
    memcpy(callee_save, x86_64_callee_save_regs, sizeof(x86_64_callee_save_regs));

    ret.returnRegs.SetPtr(ctx->base, ret_reg);
    ret.callerSavedRegs.SetPtr(ctx->base, caller_saved);
    ret.calleeSavedRegs.SetPtr(ctx->base, callee_save);
    ret.paramRegs.SetPtr(ctx->base, param_regs);

    return ret;
}

OpMIR* vreg0;
OpMIR* vreg1;
OpMIR* base;
OpMIR* index;

OpMIR* base_read;
OpMIR* index_read;
OpMIR* vreg0_read;
OpMIR* vreg1_read;

OpMIR* imm0;
OpMIR* disp;
OpMIR* scale;

OpMIR* base_plus_disp;
OpMIR* base_plus_index;
OpMIR* base_plus_index_plus_disp;
OpMIR* base_plus_index_mul_scale;
OpMIR* index_mul_scale_plus_disp;
OpMIR* base_plus_index_mul_scale_plus_disp;

OpMIR* mem_base;
OpMIR* mem_disp;
OpMIR* mem_base_plus_disp;
OpMIR* mem_base_plus_index;
OpMIR* mem_base_plus_index_plus_disp;
OpMIR* mem_base_plus_index_mul_scale;
OpMIR* mem_index_mul_scale_plus_disp;
OpMIR* mem_base_plus_index_mul_scale_plus_disp;

SSAPatternConstraint* nonCommutative;
SSAPatternConstraint* noReuse;
SSAPatternConstraint* noConst;
SSAPatternConstraint* noConstNoReuse;
SSAPatternConstraint* noReuseConstOne;

void InitGlobals(LinearAllocator* ctx) {

    nonCommutative = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(SSAPatternConstraint));
    noReuse = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(SSAPatternConstraint));
    noConst = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(SSAPatternConstraint));
    noConstNoReuse = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(SSAPatternConstraint) * 2);
    noReuseConstOne = (SSAPatternConstraint*)linear_allocate(ctx, sizeof(SSAPatternConstraint) + sizeof(SSAPatternConst));

    nonCommutative[0].type = CONSTRAINT_NON_COMMUMATIVE;
    noReuse[0].type = CONSTRAINT_SINGLE_USE;
    noConst[0].type = CONSTRAINT_NON_CONSTANT;
    noConstNoReuse[0].type = CONSTRAINT_NON_CONSTANT;
    noConstNoReuse[1].type = CONSTRAINT_SINGLE_USE;
    Mem<SSAPatternConstraint>(noReuseConstOne).type = CONSTRAINT_SINGLE_USE;
    Mem<SSAPatternConst>(noReuseConstOne + 1).type = CONSTRAINT_SPECIFIC_CONSANT;
    Mem<SSAPatternConst>(noReuseConstOne + 1).constant.type = TYPE_PRIMARY_INT64;
    Mem<i64>(Mem<SSAPatternConst>(noReuseConstOne + 1).constant.mem) = 1;
    
    vreg0 = MakeUnaryOpMIR(ctx, MIR_VREG, MakeVregMIR(ctx, 0));
    vreg1 = MakeUnaryOpMIR(ctx, MIR_VREG, MakeVregMIR(ctx, 0));
    base = MakeUnaryOpMIR(ctx, MIR_VREG, MakeVregMIR(ctx, 0));
    index = MakeUnaryOpMIR(ctx, MIR_VREG, MakeVregMIR(ctx, 0));

    base_read = make_x86_64_vreg_read(ctx, base);
    index_read = make_x86_64_vreg_read(ctx, index);
    vreg0_read = make_x86_64_vreg_read(ctx, vreg0);
    vreg1_read = make_x86_64_vreg_read(ctx, vreg1);
    
    imm0 = MakeConstantMIR(ctx);
    disp = MakeConstantMIR(ctx);
    scale = MakeConstantMIR(ctx);

    base_plus_disp = make_x86_64_base_plus_disp(ctx, base_read, disp);
    base_plus_index = make_x86_64_base_plus_index(ctx, base_read, index_read);
    base_plus_index_plus_disp = make_x86_64_base_plus_index_plus_disp(ctx, base_read, index_read, disp);
    base_plus_index_mul_scale = make_x86_64_base_plus_index_mul_scale(ctx, base_read, index_read, scale);
    index_mul_scale_plus_disp = make_x86_64_index_mul_scale_plus_disp(ctx, index_read, scale, disp);
    base_plus_index_mul_scale_plus_disp = make_x86_64_base_plus_index_mul_scale_plus_disp(ctx, base_read, index_read, scale, disp);

    mem_disp = make_x86_64_mem_read(ctx, disp);
    mem_base = make_x86_64_mem_read(ctx, base);
    mem_base_plus_disp = MakeUnaryOpMIR(ctx, MIR_MEM, base_plus_disp);
    mem_base_plus_index = MakeUnaryOpMIR(ctx, MIR_MEM, base_plus_index);
    mem_base_plus_index_plus_disp = MakeUnaryOpMIR(ctx, MIR_MEM, base_plus_index_plus_disp);
    mem_base_plus_index_mul_scale = MakeUnaryOpMIR(ctx, MIR_MEM, base_plus_index_mul_scale);
    mem_index_mul_scale_plus_disp = MakeUnaryOpMIR(ctx, MIR_MEM, index_mul_scale_plus_disp);
    mem_base_plus_index_mul_scale_plus_disp = MakeUnaryOpMIR(ctx, MIR_MEM, base_plus_index_mul_scale_plus_disp);
}

i32 main() {

    auto ctx = make_linear_allocator(alloca(Megabyte(4)), Megabyte(4));
    auto arch = (ArhitectureDescriptor*)linear_allocate(&ctx, sizeof(ArhitectureDescriptor));

    *arch = {
        {},
        2,
        81,
        sizeof(x86_64_instructions) / sizeof(InstDescriptor),
        4,
        {},
        {},
        {},
        make_x86_64_linux_abi(&ctx),
    };
    auto archName = (char*)linear_allocate(&ctx, sizeof("x86_64"));
    memcpy((char*)archName, "x86_64", sizeof("x86_64"));
    arch->arhitecture.SetPtr(ctx.base, archName);

    auto regs = (RegisterDescriptor*)linear_allocate(&ctx, arch->registerCount * sizeof(RegisterDescriptor));
    arch->regDescriptorTable.SetPtr(ctx.base, regs);

    for(u32 i = 0; i < 81; i++) {
        
        auto len = str_len(x86_64_register_str[i]);
        auto name = (char*)linear_allocate(&ctx, len);
        memcpy(name, x86_64_register_str[i], len);
        regs[i].str.SetPtr(ctx.base, name);

        regs[i].width = x86_64_reg_width[i];

        if(i < 4 * 16) { // no. gpr
            auto aliases = (u32*)linear_allocate(&ctx, sizeof(x86_64_gpr_alias_classes[i]));
            memcpy(aliases, x86_64_gpr_alias_classes[i], sizeof(x86_64_gpr_alias_classes[i]));
            regs[i].clobberCount = sizeof(x86_64_gpr_alias_classes[i]) / sizeof(u32);
            regs[i].clobbersOnWrite.SetPtr(ctx.base, aliases);
        }
        else {
            regs[i].clobberCount = 0;
        }
    }

    // gpr8, gpr16, gpr32, gpr64, all_gpr, stack_reg, rflags, xmm_n
    arch->regClassCount = sizeof(x86_64_reg_classes) / sizeof(RegisterClass);
    auto classes = (RegisterClass*)linear_allocate(&ctx, sizeof(x86_64_reg_classes));
    arch->regClasses.SetPtr(ctx.base, classes);
    for(u32 i = 0; i < arch->regClassCount; i++) {
        classes[i].registerCount = x86_64_reg_classes[i].registerCount;
        auto regs = (u32*)linear_allocate(&ctx, classes[i].registerCount * sizeof(u32));
        memcpy(regs, x86_64_reg_classes[i].regs, classes[i].registerCount * sizeof(u32));
        classes[i].regs.SetPtr(ctx.base, regs);
    }

    auto insts = (InstDescriptor*)linear_allocate(&ctx, sizeof(InstDescriptor) * MIR_COUNT * 18);
    arch->instDescriptorTable.SetPtr(ctx.base, insts);

    Flags flagsAffected[FLAG_COUNT] = {
        FLAG_CARRY_FLAG,
        FLAG_PARITY_FLAG,
        FLAG_ADJUT_FLAG,
        FLAG_ZERO_FLAG,
        FLAG_SIGN_FLAG,
        FLAG_OVERFLOW_FLAG,
    };


    InitGlobals(&ctx);
    for(u32 it = MIR_ADD; it <= MIR_NEG; it++) {
        auto macro = &insts[it * 18].macro;
        MakeAllAddressingModes(&ctx, (MachineIROp)it, false, FLAG_COUNT, flagsAffected, macro);
    }

    auto file = fopen("x86_64_machine_descriptor", "w");
    fwrite(ctx.base, ctx.top, 1, file);
    fclose(file);

    return 0;
}