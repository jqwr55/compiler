#include <iostream>
#include <stdio.h>
#include <stdlib.h>

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef float f32;
typedef double f64;

static_assert(sizeof(f32) == 4);
static_assert(sizeof(f64) == 8);

typedef u8 byte;

constexpr u64 KILO_BYTE = 1000;
constexpr u64 MEGA_BYTE = 1000 * KILO_BYTE;
constexpr u64 GIGA_BYTE = 1000 * MEGA_BYTE;
constexpr u64 TERA_BYTE = 1000 * GIGA_BYTE;

constexpr u32 MICRO_SEC = 1000;
constexpr u32 MILI_SEC = 1000 * MICRO_SEC;

#define LOGASSERT(x , y ) if( !(x) ) {std::cout << #x << " " << y << " " << __FILE__ << " " << __LINE__ << std::endl; __builtin_trap(); }
#define ASSERT(x) if( !(x) ) {std::cout << #x << " triggered builtin trap in: " << __FILE__ << " " << __LINE__ << std::endl; __builtin_trap(); }

#ifdef __GNUC__
    #define _FORCE_INLINE __attribute__((always_inline))
#else
    #ifdef _MSC_VER
        #define _FORCE_INLINE __builtin_trap();
    #endif
#endif

template<typename T> _FORCE_INLINE T Min(T t0 , T t1) {
    return t0 > t1 ? t1 : t0;
}
template<typename T> _FORCE_INLINE T Max(T t0 , T t1) {
    return t0 < t1 ? t1 : t0;
}

template<typename T> _FORCE_INLINE T& Cast(void* mem) {
    return *((T*)mem);
}

template<typename K , typename V> struct HashNode {
    K key;
    V value;
};
template<typename K , typename V> struct HashTable {
    HashNode<K,V>* array = nullptr;
    u32 cap = 0;
    u32 occupancy = 0;
    static constexpr f32 loadFactor = 0.5;

    void Init() {
        cap = 2;
        array = (HashNode<K,V>*)malloc( sizeof(HashNode<K,V>) * 2);
        array[0].key = (~u32(0));
        array[1].key = (~u32(0));
    }

    u32 Find(K key) {
    
        u32 hash = key & (cap - 1);
        u32 ogHash = hash;
        for(;;) {
            if(array[hash].key == key) {
                return hash;
            }
            hash++;

            if(hash == cap) {
                return (~u32(0));
            }
        }
    }

    void Delete(K key) {
        occupancy--;
        array[Find(key)].key = (~u32(0));
    }

    void Insert(u32 key , V val) {

        if( cap * loadFactor < (occupancy + 1) ) {

            HashNode<K,V>* tmp = (HashNode<K,V>*)malloc(sizeof(HashNode<K,V>) * cap * 2);

            u32 hash;
            u32 newCap = cap * 2;
            for(u32 i = 0; i < newCap ; i++) {
                tmp[i].key = (~u32(0));
            }

            for(u32 i = 0; i < cap ; i++) {
                if( array[i].key != (~u32(0)) ) {
                    hash = array[i].key & (newCap - 1);
                    
                    for(;;) {
                        if(tmp[hash].key == (~u32(0))) {
                            tmp[hash].key = array[i].key;
                            tmp[hash].value = array[i].value;
                            break;
                        }

                        hash++;

                        if(hash == newCap) {
                            ASSERT(false);
                        }
                    }
                }
            }

            free(array);
            array = tmp;
            cap = newCap;
        }
        occupancy++;

        u32 hash = key & (cap - 1);
        for(;;) {
            if( array[hash].key == (~u32(0)) ) {
                array[hash].key = key;
                array[hash].value = val;
                return;
            }

            hash++;
            ASSERT(hash != cap);
        }
    }

    

    void Free() {
        free(array);
        array = nullptr;
    }
};
template<typename T> struct DynamicBufferSimple {
    T*      mem = nullptr;
    u32     cap = 0;
    u32     size = 0;

    void Init() {
        mem = nullptr;
        cap = 0;
        size = 0;
    }

    T& Nth(u32 n) {
        ASSERT(mem != nullptr && size != 0);
        return mem[(size-1)-n];
    }
    T& Back() {
        ASSERT(mem != nullptr && size != 0);
        return mem[size-1];
    }
    T& Front(){
        ASSERT(mem != nullptr && size != 0);
        return mem[0];
    }

    void SetCapacity(u32 capacity) {
        mem = (T*)malloc(capacity * sizeof(T));
        cap = capacity;
    }
    u32 PushBack(T e) {

        if( cap < size + 1) {
            T* tmp = (T*)malloc( sizeof(T) * (size + 1) * 2 );
            for(u32 i = 0; i < size * sizeof(T) ; i++) {
                ((byte*)tmp)[i] = ((byte*)mem)[i];
            }
            free(mem);
            mem = tmp;

            cap = (size + 1) * 2;
        }

        mem[size] = e;
        return size++;
    }
    u32 Find(T e) {
        for(u32 i = 0; i < size ; i++) {
            if(mem[i] == e) return i;
        }
        return ~u32(0);
    }
    void Clear() {
        size = 0;
    }
    u32 PopBack() {
        return size--;
    }
    u32 Shrink() {
        if(size < cap) {
            T* tmp = (T*)malloc( sizeof(T) * (size + 1) * 2 );
            for(u32 i = 0; i < size * sizeof(T) ; i++) {
                ((byte*)tmp)[i] = ((byte*)mem)[i];
            }
            free(mem);
            mem = tmp;
            cap = size;
        }
        return size;
    }
    u32 ShrinkNoBranch() {
        T* tmp = (T*)malloc( sizeof(T) * (size + 1) * 2 );
        for(u32 i = 0; i < size * sizeof(T) ; i++) {
            ((byte*)tmp)[i] = ((byte*)mem)[i];
        }
        free(mem);
        mem = tmp;
        cap = size;
        return size;
    }

    _FORCE_INLINE T& operator[] (u32 i) {
        return mem[i];
    }
    void Free() {
        free(mem);
        mem = nullptr;
        cap = 0;
        size = 0;
    }

    void Delete(u32 i) {
        for(; i < size-1 ; i++) {
            mem[i] = mem[i+1];
        }
    }
    void Remove(u32 i) {
        mem[i] = Back();
        PopBack();
    }
};

enum Opcode : byte {

    NO_OP,          // nop
    OP_MOV_IM8,     // mov register,im8
    OP_MOV_IM16,    // mov register,im16
    OP_MOV_IM32,    // mov register,im32
    OP_MOV_IM64,    // mov register,im64

    OP_MOV8,        // mov8     register , register
    OP_MOV16,       // mov16    register , register
    OP_MOV32,       // mov32    register , register
    OP_MOV64,       // mov64    register , register

    OP_OR,          // or       register , register
    OP_AND,         // and      register , register
    OP_NOT,         // not      register , register
    OP_XOR,         // xor     register , register

    OP_RSHIFT,      // rshift reg , reg
    OP_LSHIFT,      // rshift reg , reg

    OP_PUSH,        // push     reg , [0,1,2,3] -> [8,16,32,64]
    OP_POP,         // pop      reg , [0,1,2,3] -> [8,16,32,64]

    OP_ADD,         // add      reg,reg
    OP_SUB,         // sub      reg,reg
    OP_MUL,         // mul      [0,1,2,3]->[8,16,32,64], reg,reg
    OP_DIV,         // div      [0,1,2,3]->[8,16,32,64], reg,reg

    OP_IMUL,        // imul      [0,1,2,3]->[8,16,32,64], reg,reg
    OP_IDIV,        // idiv      [0,1,2,3]->[8,16,32,64], reg,reg

    OP_ADD_S,       // add_s    reg,reg
    OP_SUB_S,       // add_s    reg,reg
    OP_MUL_S,       // add_s    reg,reg
    OP_DIV_S,       // add_s    reg,reg

    OP_ADD_D,       // add_d    reg,reg
    OP_SUB_D,       // add_d    reg,reg
    OP_MUL_D,       // add_d    reg,reg
    OP_DIV_D,       // add_d    reg,reg

    OP_CJMP,        // cjmp     reg0,[reg1]     if(reg0 != 0) jmp [reg1]
    OP_JMP,         // jmp      [reg]
    OP_JMP_IM,      // jmp      im64

    OP_LOAD,        // load     reg,[reg],[0,1,2,3]->[8,16,32,64]
    OP_STORE,       // store    [reg],reg,[0,1,2,3]->[8,16,32,64]

    OP_INTERUPT,    // int      R0 type function    R1... parameters R12 return val
                    // printINT       0,R1 0  R2 int
                    // printUINT      0,R1 1  R2 uint
                    // printF32       0,R1 2  R2 f32
                    // printF64       0,R1 3  R2 f64
                    // printFCHAR     0,R1 4  R2 char
                    // printStr       0,R1 5  R2 str,R3 lenght

                    // abort          1
    
    OP_CSFS,        // convert  reg,reg     i32<-f32
    OP_CSFU,        // convert  reg,reg     u32<-f32
    OP_CSSF,        // convert  reg,reg     f32<-i32
    OP_CUSF,        // convert  reg,reg     f32<-u32
    OP_CDFS,        // convert  reg,reg     i64<-f64
    OP_CDFU,        // convert  reg,reg     u64<-f64
    OP_CSDF,        // convert  reg,reg     f64<-i64
    OP_CUDF,        // convert  reg,reg     f64<-u64
    OP_CSFDF,       // convert  reg,reg     f64<-f32
    OP_CDFSF,       // convert  reg,reg     f32<-f64

    OP_CMP_I_LT,      // cmp      reg0,reg1, reg2   reg2 = reg0 < reg1
    OP_CMP_I_GT,      // cmp      reg0,reg1, reg2   reg2 = reg0 > reg1
    OP_CMP_I_E,       // cmp      reg0,reg1, reg2   reg2 = reg0 = reg1
    OP_CMP_U_LT,      // cmp      reg0,reg1, reg2   reg2 = reg0 < reg1
    OP_CMP_U_GT,      // cmp      reg0,reg1, reg2   reg2 = reg0 > reg1
    OP_CMP_SF_LT,     // cmp      reg0,reg1, reg2   reg2 = reg0 < reg1
    OP_CMP_SF_GT,     // cmp      reg0,reg1, reg2   reg2 = reg0 > reg1
    OP_CMP_SF_E,      // cmp      reg0,reg1, reg2   reg2 = reg0 = reg1
    OP_CMP_DF_LT,     // cmp      reg0,reg1, reg2   reg2 = reg0 < reg1
    OP_CMP_DF_GT,     // cmp      reg0,reg1, reg2   reg2 = reg0 > reg1
    OP_CMP_DF_E,      // cmp      reg0,reg1, reg2   reg2 = reg0 = reg1

    OP_LOAD_REL_RSP,    // load_stack  reg,[0,1,2,3]->[8,16,32,64],[rsp+im16]
    OP_STORE_REL_RSP,   // store_stack reg,[0,1,2,3]->[8,16,32,64],[rsp+im16]
    OP_LOAD_REL_RBP,    // load_base   reg,[0,1,2,3]->[8,16,32,64],[rsp+im16]
    OP_STORE_REL_RBP,   // store_base  reg,[0,1,2,3]->[8,16,32,64],[rsp+im16]
    OP_LEA,             // lea         reg,[reg+reg<<sh+im16]

    OP_GET_PTR,         // mov r , im32
    OP_GET_STACK_PTR,   // get_stack_ptr reg
    OP_GET_BASE_PTR,    // get_base_ptr reg

    OP_CALL_R,        //  r target
    OP_CALL,          //  im32 target
    OP_RET,

    OP_EXIT,

    OP_COUNT,
};
enum RegisterName : byte {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    RRV,    // return values
    RSP,    // stack ptr
    RBP,    // base ptr
    RPC,    // program counter
    REGISTER_COUNT,
};
struct ExecutableHeader {
    u64 size                    = 0;
    u64 code_start_offset       = 0;
    u64 code_end_offset         = 0;
    u64 read_only_start_offset  = 0;
    u64 read_only_end_offset    = 0;
    u64 globals_start_offset    = 0;
    u64 globals_end_offset      = 0;
    char name[256]{0};
};
struct Executable {
    ExecutableHeader header;
};
struct alignas(8) Reg64 {
    byte mem[8]{0};
};
struct alignas(4)  Reg32 {
    byte mem[4]{0};
};
struct alignas(64) RegisterFile {
    Reg64 gpr[17]{0};
};

struct VM;
typedef void(*interupt_handler_t)(VM* vm);
struct InteruptTable {
    interupt_handler_t* handlers;
    u32 size;
};

struct RegisterTracker {
    RegisterFile                           val;
    DynamicBufferSimple<HashNode<u32,u32>> history[REGISTER_COUNT];
    DynamicBufferSimple<HashNode<u32,u32>> labels;
    HashTable<u32,u32>                     opOffsets;
    bool                                   known[REGISTER_COUNT];
};

struct VM {
    RegisterTracker tracker;
    RegisterFile    reg;
    InteruptTable   interupts;
    Executable*     loadedProgram   = nullptr;
    byte*           stack           = nullptr;
    byte*           stackEnd        = nullptr;
    bool            running         = true;
    bool            abort           = false;
};

enum VmStatus {
    VM_OK,
    VM_ERROR,
    VM_PROGRAM_EXIT,
    VM_PROGRAM_ABORT,
    VM_PROGRAM_YIELD,
};

typedef VmStatus(*jit_compiled_t)();

void WriteRegister(byte* mem, u32* index, RegisterName r, u32 size);
void WriteOP(byte* mem, u32* index, const char* str, u32 size);
jit_compiled_t jit_compile(VM* vm, byte* mem, void* stack, byte* program, u32 size);
u8 ToFrom(RegisterName to , RegisterName from);
u32 GetLow4bits(byte b);
u32 GetHigh4bits(byte b);
byte* DisassembleInstruction(byte* pc);
void MemSet(void* dst, u8 v, u32 size);
void MemCpy(void* dst, const void* const src, u32 size);
char* ReadFileTerminated(const char* fileName, u32* size_);
const char* GetVmStatusStr(VmStatus stat);