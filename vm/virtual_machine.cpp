#include <vm_common.h>

char* ReadFileTerminated(const char* fileName, u32* size_) {

    char* sourceString = nullptr;
    FILE* file = fopen(fileName ,"r");
    if(file) {

        fseek(file , 0, SEEK_END);
        u32 size = ftell(file);
        fseek(file ,0, SEEK_SET);
        *size_ = size;

        sourceString = (char*)malloc(size + 1);
        fread(sourceString , size , 1 , file);
        sourceString[size] = 0;

    }
    fclose(file);

    return sourceString;
}

void MemSet(void* dst, u8 v, u32 size) {
    for(u32 i = 0; i < size ; i++) {
        ((byte*)dst)[i] = v;
    }
}
void MemCpy(void* dst, const void* const src, u32 size) {
    for(u32 i = 0; i < size ; i++) {
        ((byte*)dst)[i] = ((byte*)src)[i];
    }
}

struct MemoryBlockHeader {
    MemoryBlockHeader* left;
    MemoryBlockHeader* right;
    u32 size;
    bool is_free;
};
byte* base;
MemoryBlockHeader* search_free_block(u32 size) {

    MemoryBlockHeader* block = (MemoryBlockHeader*)(base);
    while(block) {
        if(block->is_free && block->size >= size) return block;
        block = block->right;
    }

    return nullptr;
}

void init_my_malloc(void* base_, u32 size) {
    base = (byte*)base_;
    MemoryBlockHeader* first_block = (MemoryBlockHeader*)base;
    first_block->is_free = true;
    first_block->left = 0;
    first_block->right = 0;
    first_block->size = size;
}
void* my_malloc(u32 size) {

    if(!size) return nullptr;

    MemoryBlockHeader* free_block = search_free_block(size);
    if(free_block) {
        free_block->is_free = false;
        if(free_block->size - size > sizeof(MemoryBlockHeader)) {

            byte* free_block_end = ((byte*)(free_block + 1)) + size;
            MemoryBlockHeader* new_free_block = (MemoryBlockHeader*)free_block_end;

            new_free_block->is_free = true;
            new_free_block->size = (free_block->size - size) - sizeof(MemoryBlockHeader);
            new_free_block->right = free_block->right;
            new_free_block->left = free_block;

            free_block->right = new_free_block;
            free_block->size = size;
        }
        return free_block + 1;
    }


    std::cout << "out of memory" << std::endl;
    return nullptr;
}


void my_free(void* block) {
    if(!block) return;

    MemoryBlockHeader* header = ((MemoryBlockHeader*)block) - 1;
    ASSERT(!header->is_free);
    header->is_free = true;

    MemoryBlockHeader* next_block = header->right;
    MemoryBlockHeader* previous_block = header->left;

    while(next_block) {
        if(!next_block->is_free) break;

        header->size += next_block->size + sizeof(MemoryBlockHeader);
        header->right = next_block->right;
        if(header->right) header->right->left = header;

        next_block = header->right;
    }
    while(previous_block) {
        if(!previous_block->is_free) break;

        previous_block->size += header->size + sizeof(MemoryBlockHeader);;
        previous_block->right = header->right;

        if(previous_block->right) previous_block->right->left = previous_block;

        header = previous_block;
        previous_block = previous_block->left;
    }
}

void PrintBlocks() {
    MemoryBlockHeader* block = (MemoryBlockHeader*)(base);
    while(block) {
        std::cout << (u64)block << " " << block->is_free << " " << block->size << std::endl;
        block = block->right;
    }
}

void* my_malloc_debug(u32 size) {
    byte* mem = (byte*)my_malloc(size + 128);
    *((u32*)mem) = size;
    MemSet(mem + sizeof(u32), 255, 60);
    MemSet(mem + size + 64, 255, 64);
    return mem + 64;
}
void my_free_debug(void* mem) {
    if(!mem) return;
    u32 size = *((u32*)((byte*)mem - 64));
    byte* back_guard = ((byte*)mem) - 60;
    byte* front_guard = ((byte*)mem) + size;

    for(u32 i = 0; i < 60; i++) {
        ASSERT(back_guard[i] == 255);
    }
    for(u32 i = 0; i < 64; i++) {
        ASSERT(front_guard[i] == 255);
    }

    my_free(back_guard-4);
}
bool check_memory_integrity(void* mem) {
    if(!mem) return true;
    u32 size = *((u32*)((byte*)mem - 64));
    byte* back_guard = ((byte*)mem) - 60;
    byte* front_guard = ((byte*)mem) + size;

    bool corrupt = false;
    for(u32 i = 0; i < 60; i++) {
        //std::cout << (u32)back_guard[i] << " ";
        corrupt |= back_guard[i] != 255;
    }
    //std::cout << std::endl;
    for(u32 i = 0; i < 64; i++) {
        //std::cout << (u32)front_guard[i] << " ";
        corrupt |= front_guard[i] != 255;
    }

    if(corrupt) {

        std::cout << std::endl;
        for(u32 i = 0; i < 60; i++) {
            std::cout << (u32)back_guard[i] << " ";
        }
        std::cout << std::endl;
        for(u32 i = 0; i < 64; i++) {
            std::cout << (u32)front_guard[i] << " ";
        }
        std::cout << std::endl;
    }

    ASSERT(!corrupt);
    return corrupt;
}

_FORCE_INLINE u32 GetLow4bits(byte b) {
    return b & (0x0F);
}
 _FORCE_INLINE u32 GetHigh4bits(byte b) {
    return b >> 4;
}
byte* SimpleInstruction(const char* name, byte* pc) {
    std::cout << name << std::endl;
    return pc + 1;
}
byte* SimpleInstructionImmediate8(const char* name, byte* pc) {
    std::cout << name << " " << std::hex << std::showbase << (u32)*(pc+1) << std::endl;
    return pc + 3;
}
byte* SimpleInstructionImmediate16(const char* name, byte* pc) {
    std::cout << name << " " << std::hex << std::showbase << (u32)*(pc+1) << " " << (u32)*(pc+2) << std::endl;
    return pc + 4;
}
byte* SimpleInstructionImmediate32(const char* name, byte* pc) {
    std::cout << name << " " << std::hex << std::showbase << (u32)*(pc+1) << " " << (u32)*(pc+2) << " " << (u32)*(pc+3) << " " << (u32)*(pc+4) << std::endl;
    return pc + 6;
}
byte* SimpleInstructionImmediate64(const char* name, byte* pc) {
    std::cout << name << " " << std::hex << std::showbase << (u32)*(pc+1) << " " << (u32)*(pc+2) << " " << (u32)*(pc+3) << " " << (u32)*(pc+4) << " "
    << (u32)*(pc+5) << " " << (u32)*(pc+6) << " " << (u32)*(pc+7) << " "<< (u32)*(pc+8) << std::endl;
    return pc + 10;
}
byte* DisassembleInstruction(byte* pc) {
    switch (*pc) {
        case NO_OP:
            return SimpleInstruction("NO_OP",pc);
            break;
        case OP_EXIT:
            return SimpleInstruction("OP_EXIT",pc);
            break;
        case OP_MOV_IM8:
            return SimpleInstructionImmediate8("OP_MOV_IM8",pc);
            break;
        case OP_MOV_IM16:
            return SimpleInstructionImmediate16("OP_MOV_IM16",pc);
            break;
        case OP_MOV_IM32:
            return SimpleInstructionImmediate32("OP_MOV_IM32",pc);
            break;
        case OP_MOV_IM64:
            return SimpleInstructionImmediate64("OP_MOV_IM64",pc);
            break;
        case OP_MOV8:
            std::cout <<  std::dec << std::noshowbase << "OP_MOV8 r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
        case OP_MOV16:
            std::cout << std::dec << std::noshowbase << "OP_MOV16 r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
        case OP_MOV32:
            std::cout << std::dec << std::noshowbase << "OP_MOV32 r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
        case OP_MOV64:
            std::cout << std::dec << std::noshowbase << "OP_MOV64 r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_OR:
            std::cout << std::dec << std::noshowbase << "OP_OR r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
        case OP_AND:
            std::cout << std::dec << std::noshowbase << "OP_AND r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
        case OP_NOT:
            std::cout << std::dec << std::noshowbase << "OP_NOT r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
        case OP_XOR:
            std::cout << std::dec << std::noshowbase << "OP_XOR r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_RSHIFT:
            std::cout << std::dec << std::noshowbase << "OP_RSHIFT r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_LSHIFT:
            std::cout << std::dec << std::noshowbase << "OP_LSHIFT r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;

        case OP_PUSH:
            std::cout << std::dec << std::noshowbase << "OP_PUSH r" << GetHigh4bits(pc[1]) << " , " << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_POP:
            std::cout << std::dec << std::noshowbase << "OP_POP r" << GetHigh4bits(pc[1]) << " , " << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_ADD:
            std::cout << std::dec << std::noshowbase << "OP_ADD r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_SUB:
            std::cout << std::dec << std::noshowbase << "OP_SUB r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_MUL:
            std::cout << std::dec << std::noshowbase << "OP_MUL [" << GetHigh4bits(pc[1]) << "] , r" << GetLow4bits(pc[1]) << " , r" << (i32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_DIV:
            std::cout << std::dec << std::noshowbase << "OP_DIV [" << GetHigh4bits(pc[1]) << "] , r" << GetLow4bits(pc[1]) << " , r" << (i32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_IMUL:
            std::cout << std::dec << std::noshowbase << "OP_IMUL [" << GetHigh4bits(pc[1]) << "] , r" << GetLow4bits(pc[1]) << " , r" << (i32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_IDIV:
            std::cout << std::dec << std::noshowbase << "OP_IDIV [" << GetHigh4bits(pc[1]) << "] , r" << GetLow4bits(pc[1]) << " , r" << (i32)pc[2] << std::endl;
            return pc + 3;
            break;
        
        case OP_ADD_S:
            std::cout << std::dec << std::noshowbase << "OP_ADD_S r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_SUB_S:
            std::cout << std::dec << std::noshowbase << "OP_SUB_S r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_MUL_S:
            std::cout << std::dec << std::noshowbase << "OP_MUL_S r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_DIV_S:
            std::cout << std::dec << std::noshowbase << "OP_DIV_S r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_ADD_D:
            std::cout << std::dec << std::noshowbase << "OP_ADD_D r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_SUB_D:
            std::cout << std::dec << std::noshowbase << "OP_SUB_D r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_MUL_D:
            std::cout << std::dec << std::noshowbase << "OP_MUL_D r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_DIV_D:
            std::cout << std::dec << std::noshowbase << "OP_DIV_D r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_CJMP:
            std::cout << std::dec << std::noshowbase << "OP_CJMP r" << GetHigh4bits(pc[1] ) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_JMP_IM:
            std::cout << std::dec << std::noshowbase << "OP_JMP_IM [" << std::hex << std::showbase << Cast<u64>(pc + 1) - 320 << std::dec << std::noshowbase  << "]" << std::endl;
            return pc + 9;
            break;
        case OP_JMP:
            std::cout << std::dec << std::noshowbase << "OP_JMP r" << (u16)pc[1] << std::endl;
            return pc + 2;
            break;
        case OP_LOAD:
            std::cout << std::dec << std::noshowbase << "OP_LOAD r" << GetHigh4bits(pc[1] ) << " , [r" << GetLow4bits(pc[1]) << "] , " << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_STORE:
            std::cout << std::dec << std::noshowbase << "OP_STORE [r" << GetHigh4bits(pc[1] ) << "] , r" << GetLow4bits(pc[1]) << " , " << (u32)pc[2]  << std::endl;
            return pc + 3;
            break;
        case OP_INTERUPT:
            std::cout << std::dec << std::noshowbase << "OP_INTERUPT" << std::endl;
            return pc + 1;
            break;

        case OP_CSFS:
            std::cout << std::dec << std::noshowbase << "OP_CSFS r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_CSFU:
            std::cout << std::dec << std::noshowbase << "OP_CSFU r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_CSSF:
            std::cout << std::dec << std::noshowbase << "OP_CSSF r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_CUSF:
            std::cout << std::dec << std::noshowbase << "OP_CUSF r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_CDFS:
            std::cout << std::dec << std::noshowbase << "OP_CDFS r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_CDFU:
            std::cout << std::dec << std::noshowbase << "OP_CDFU r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_CSDF:
            std::cout << std::dec << std::noshowbase << "OP_CSDF r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_CUDF:
            std::cout << std::dec << std::noshowbase << "OP_CUDF r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_CSFDF:
            std::cout << std::dec << std::noshowbase << "OP_CSFD r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;
        case OP_CDFSF:
            std::cout << std::dec << std::noshowbase << "OP_CDFS r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << std::endl;
            return pc + 2;
            break;

        case OP_CMP_I_LT:
            std::cout << std::dec << std::noshowbase << "OP_CMP_I_LT r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_CMP_I_GT:
            std::cout << std::dec << std::noshowbase << "OP_CMP_I_GT r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_CMP_I_E:
            std::cout << std::dec << std::noshowbase << "OP_CMP_I_E r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_CMP_SF_LT:
            std::cout << std::dec << std::noshowbase << "OP_CMP_SF_L r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_CMP_SF_GT:
            std::cout << std::dec << std::noshowbase << "OP_CMP_SF_G r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_CMP_SF_E:
            std::cout << std::dec << std::noshowbase << "OP_CMP_SF_E r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_CMP_DF_LT:
            std::cout << std::dec << std::noshowbase << "OP_CMP_DF_L r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_CMP_DF_GT:
            std::cout << std::dec << std::noshowbase << "OP_CMP_DF_G r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_CMP_DF_E:
            std::cout << std::dec << std::noshowbase << "OP_CMP_DF_E r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        
        case OP_LOAD_REL_RSP:
            std::cout << std::dec << std::noshowbase << "OP_LOAD_REL_RSP r "  << GetHigh4bits(pc[1]) << " , [" << GetLow4bits(pc[1]) << "] , " << Cast<u16>(pc + 2) << std::endl;
            return pc + 4;
            break;
        case OP_STORE_REL_RSP:
            std::cout << std::dec << std::noshowbase << "OP_STORE_REL_RSP r "  << GetHigh4bits(pc[1]) << " , [" << GetLow4bits(pc[1]) << "] , " << Cast<u16>(pc + 2) << std::endl;
            return pc + 4;
            break;

        case OP_LOAD_REL_RBP:
            std::cout << std::dec << std::noshowbase << "OP_LOAD_REL_RBP r" << GetHigh4bits(pc[1]) << " , [" << GetLow4bits(pc[1]) << "] , " << Cast<u16>(pc + 2) << std::endl;
            return pc + 4;
            break;
        case OP_STORE_REL_RBP:
            std::cout << std::dec << std::noshowbase << "OP_STORE_REL_RBP r" << GetHigh4bits(pc[1]) << " , [" << GetLow4bits(pc[1]) << "] , " << Cast<u16>(pc + 2) << std::endl;
            return pc + 4;
            break;
        case OP_GET_STACK_PTR:
            std::cout << std::dec << std::noshowbase << "OP_GET_STACK_PTR r" << (u16)pc[1] << std::endl;
            return pc + 2;
            break;
        case OP_GET_BASE_PTR:
            std::cout << std::dec << std::noshowbase << "OP_GET_BASE_PTR r" << (u16)pc[1] << std::endl;
            return pc + 2;
            break;
        default:
            std::cout << "Warning: unknown op code: " << std::hex << std::showbase << (u32)*pc << std::endl;
            return pc + 1;
            break;
    }

    ASSERT(false);
    return 0;
}
void InteruptPrintF32(VM* vm) {
    f32 c = Cast<f32>(vm->reg.gpr[R5].mem);
    (std::cout << c).flush();
}
void InteruptPrintF64(VM* vm) {
    f64 c = Cast<f64>(vm->reg.gpr[R5].mem);
    (std::cout << c).flush();
}
void InteruptPrintI64(VM* vm) {
    i64 c = Cast<i64>(vm->reg.gpr[R5].mem);
    (std::cout << c).flush();
}
void InteruptPrintU64(VM* vm) {
    u64 c = Cast<u64>(vm->reg.gpr[R5].mem);
    (std::cout << c).flush();
}
void InteruptPrintString(VM* vm) {
    u64 ptr = Cast<u64>(vm->reg.gpr[R5].mem);
    const char* str = (char*)vm->loadedProgram + ptr;
    u64 l = Cast<u64>(vm->reg.gpr[R4].mem);
    std::cout.write(str,l).flush();
}
void InteruptAbort(VM* vm) {
    vm->running = false;
}
void InteruptYield(VM* vm) {
    vm->running = false;
}
void InteruptAlloc(VM* vm) {

    u64 size = Cast<u64>(vm->reg.gpr[R5].mem);
    Cast<u64>(vm->reg.gpr[R5].mem) = (u64)my_malloc_debug(size) - (u64)vm->loadedProgram;
}
void InteruptFree(VM* vm) {
    u64 ptr = Cast<u64>(vm->reg.gpr[R5].mem);
    my_free_debug(vm->loadedProgram + ptr);
}

void AddInteruptHandler(VM* vm, interupt_handler_t fn, u32 slot) {

    u32 oldSize = vm->interupts.size;
    vm->interupts.size = Max<u32>(vm->interupts.size,slot+1);

    if(vm->interupts.size != oldSize) {

        interupt_handler_t* tmp = (interupt_handler_t*)malloc(vm->interupts.size * sizeof(interupt_handler_t) );
        MemCpy(tmp, vm->interupts.handlers, oldSize * sizeof(interupt_handler_t) );
        free(vm->interupts.handlers);
        vm->interupts.handlers = tmp;
    }

    vm->interupts.handlers[slot] = fn;
}
void AddInteruptHandlers(VM* vm, interupt_handler_t* fns, u32* slots, u32 size) {

    u32 oldSize = vm->interupts.size;
    for(u32 i = 0; i < size; i++) {
        vm->interupts.size = Max<u32>(vm->interupts.size,slots[i]+1);
    }
    if(vm->interupts.size != oldSize) {

        interupt_handler_t* tmp = (interupt_handler_t*)malloc(vm->interupts.size * sizeof(interupt_handler_t) );
        MemCpy(tmp, vm->interupts.handlers, oldSize * sizeof(interupt_handler_t) );
        free(vm->interupts.handlers);
        vm->interupts.handlers = tmp;
    }

    for(u32 i = 0; i < size; i++) {
        vm->interupts.handlers[slots[i]] = fns[i];
    }

}
void InitVM(VM* vm) {
    *vm = VM{};
    interupt_handler_t handlers[] = {
        InteruptPrintF32,
        InteruptPrintF64,
        InteruptPrintI64,
        InteruptPrintU64,
        InteruptPrintString,
        InteruptAbort,
        InteruptYield,
        InteruptAlloc,
        InteruptFree
    };
    u32 slots[] = {8,7,6,5,4,3,2,1,0};
    AddInteruptHandlers(vm, handlers, slots, 9);
}
void ShutDownVM(VM* vm) {
    free(vm->loadedProgram);
    free(vm->interupts.handlers);
    *vm = VM{};
}
void LoadProgramVM(VM* vm, Executable* program) {
    ASSERT(vm->loadedProgram == nullptr);

    vm->loadedProgram = (Executable*)aligned_alloc(64, program->header.size + 16 * MEGA_BYTE);
    MemSet(vm->loadedProgram, 0, program->header.size + 16 * MEGA_BYTE);
    MemCpy(vm->loadedProgram, program, program->header.size);

    vm->stack = (byte*)vm->loadedProgram + program->header.size;
    vm->stack += (64 - ((u64)vm->stack % 64)) * ( (u64)vm->stack % 64 != 0);
    vm->stackEnd = (byte*)vm->loadedProgram + (program->header.size + 8 * MEGA_BYTE);

    init_my_malloc(vm->stackEnd, 8 * MEGA_BYTE);
    
    Cast<byte*>(vm->reg.gpr[RSP].mem) = vm->stackEnd;
    Cast<byte*>(vm->reg.gpr[RPC].mem) = (byte*)vm->loadedProgram + program->header.code_start_offset;

    vm->running = true;
}

void DumpRegisterFileVM(VM* vm) {
    std::cout << "registers" << std::endl;
    for(u32 i = 0; i < 16 ; i++) {
        std::cout << std::dec << std::noshowbase << "r" <<  i << " ";
        for(u32 k = 0;  k < 8 ; k++) {
            std::cout << std::hex << std::showbase << (i32(vm->reg.gpr[i].mem[k]) & 0xFF) << " ";
        }
        std::cout << std::endl;
    }
}


void DumpStack(VM* vm);
constexpr u64 masks[4] = {0x00000000000000FF,0x000000000000FFFF,0x00000000FFFFFFFF,0xFFFFFFFFFFFFFFFF};
VmStatus ExecuteVM(VM* vm) {

    void* dispatch_table[OP_COUNT] = {
        &&NO_OP,
        &&OP_MOV_IM8,
        &&OP_MOV_IM16,
        &&OP_MOV_IM32,
        &&OP_MOV_IM64,
        &&OP_MOV8,
        &&OP_MOV16,
        &&OP_MOV32,
        &&OP_MOV64,
        &&OP_OR,
        &&OP_AND,
        &&OP_NOT,
        &&OP_XOR,
        &&OP_RSHIFT,
        &&OP_LSHIFT,
        &&OP_PUSH,
        &&OP_POP,
        &&OP_ADD,
        &&OP_SUB,
        &&OP_MUL,
        &&OP_DIV,
        &&OP_IMUL,
        &&OP_IDIV,
        &&OP_ADD_S,
        &&OP_SUB_S,
        &&OP_MUL_S,
        &&OP_DIV_S,
        &&OP_ADD_D,
        &&OP_SUB_D,
        &&OP_MUL_D,
        &&OP_DIV_D,
        &&OP_CJMP,
        &&OP_JMP,
        &&OP_JMP_IM,
        &&OP_LOAD,
        &&OP_STORE,
        &&OP_INTERUPT,
        &&OP_CSFS,
        &&OP_CSFU,
        &&OP_CSSF,
        &&OP_CUSF,
        &&OP_CDFS,
        &&OP_CDFU,
        &&OP_CSDF,
        &&OP_CUDF,
        &&OP_CSFDF,
        &&OP_CDFSF,
        &&OP_CMP_I_LT,
        &&OP_CMP_I_GT,
        &&OP_CMP_I_E,
        &&OP_CMP_U_LT,
        &&OP_CMP_U_GT,
        &&OP_CMP_SF_LT,
        &&OP_CMP_SF_GT,
        &&OP_CMP_SF_E,
        &&OP_CMP_DF_LT,
        &&OP_CMP_DF_GT,
        &&OP_CMP_DF_E,
        &&OP_LOAD_REL_RSP,
        &&OP_STORE_REL_RSP,
        &&OP_LOAD_REL_RBP,
        &&OP_STORE_REL_RBP,
        &&OP_LEA,
        &&OP_MOV_IM32,
        &&OP_GET_STACK_PTR,
        &&OP_GET_BASE_PTR,
        &&OP_CALL_R,
        &&OP_CALL,
        &&OP_RET,

        &&OP_EXIT,
    };

    byte* codeEnd = (byte*)vm->loadedProgram + vm->loadedProgram->header.code_end_offset;
    byte* codeStart = (byte*)vm->loadedProgram + vm->loadedProgram->header.code_start_offset-1;

    u64 readOnlyEnd = vm->loadedProgram->header.read_only_end_offset;
    u64 readOnlyStart = vm->loadedProgram->header.read_only_start_offset-1;

    u64 RWStart = vm->loadedProgram->header.globals_start_offset-1;
    u64 RWEnd = vm->loadedProgram->header.globals_start_offset-1;
    u64 stackEnd = (u64)vm->stackEnd - (u64)vm->loadedProgram;
    vm->running = true;

    goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];

    NO_OP:
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 1;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_MOV_IM8:
        Cast<u64>(vm->reg.gpr[ Cast<byte*>(vm->reg.gpr[RPC].mem)[1] ].mem) = Cast<u8>(Cast<byte*>(vm->reg.gpr[RPC].mem) + 2);
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_MOV_IM16:
        Cast<u64>(vm->reg.gpr[ Cast<byte*>(vm->reg.gpr[RPC].mem)[1] ].mem) = Cast<u16>(Cast<byte*>(vm->reg.gpr[RPC].mem) + 2);
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 4;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_MOV_IM32:
        Cast<u64>(vm->reg.gpr[ Cast<byte*>(vm->reg.gpr[RPC].mem)[1] ].mem) = Cast<u32>(Cast<byte*>(vm->reg.gpr[RPC].mem) + 2);
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 6;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_MOV_IM64:
        Cast<u64>(vm->reg.gpr[ Cast<byte*>(vm->reg.gpr[RPC].mem)[1] ].mem) = Cast<u64>(Cast<byte*>(vm->reg.gpr[RPC].mem) + 2);
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 10;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];

    OP_MOV8:
        Cast<u64>( vm->reg.gpr + GetHigh4bits( Cast<byte*>(vm->reg.gpr[RPC].mem)[1] )) = Cast<u8>( vm->reg.gpr + GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]));
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_MOV16:
        Cast<u64>( vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) ) = Cast<u16>( vm->reg.gpr + GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) );
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_MOV32:
        Cast<u64>( vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) ) = Cast<u32>( vm->reg.gpr + GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) );
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_MOV64:
        Cast<u64>( vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) ) = Cast<u64>( vm->reg.gpr + GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) );
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];


    OP_OR:
        Cast<u64>( vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) ) |= Cast<u64>( vm->reg.gpr + GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) );
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_AND:
        Cast<u64>( vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) ) &= Cast<u64>( vm->reg.gpr + GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) );
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_NOT:
        {
            Cast<u64>( vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) ) = ~Cast<u64>( vm->reg.gpr + GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) );
            auto s = Cast<u64>( vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) );
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_XOR:
        Cast<u64>( vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) ) ^= Cast<u64>( vm->reg.gpr + GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) );
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_RSHIFT:
        Cast<u64>( vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) ) >>= Cast<u8>( vm->reg.gpr + GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) );
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_LSHIFT:
        Cast<u64>( vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) ) <<= Cast<u8>( vm->reg.gpr + GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]) );
        Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];


    OP_PUSH:
        {
            u32 index = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            
            Cast<byte*>(vm->reg.gpr[RSP].mem) -= 1 << index;
            u64* p = Cast<u64*>(vm->reg.gpr[RSP].mem);
            switch(index) {
            case 0:
                *Cast<u8*>(vm->reg.gpr[RSP].mem) = (Cast<u8>(vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1])));
                break;
            case 1:
                *Cast<u16*>(vm->reg.gpr[RSP].mem) = (Cast<u16>(vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1])));
                break;
            case 2:
                *Cast<u32*>(vm->reg.gpr[RSP].mem) = (Cast<u32>(vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1])));
                break;
            case 3:
                *Cast<u64*>(vm->reg.gpr[RSP].mem) = (Cast<u64>(vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1])));
                break;
            }

            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_POP:
        {
            u32 index = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u64 l;
            switch(index) {
            case 0:
                l = *Cast<u8*>(vm->reg.gpr[RSP].mem) & masks[index];
                break;
            case 1:
                l = *Cast<u16*>(vm->reg.gpr[RSP].mem) & masks[index];
                break;
            case 2:
                l = *Cast<u32*>(vm->reg.gpr[RSP].mem) & masks[index];
                break;
            case 3:
                l = *Cast<u64*>(vm->reg.gpr[RSP].mem) & masks[index];
                break;
            }
            Cast<byte*>(vm->reg.gpr[RSP].mem) += 1 << index;
            Cast<u64>(vm->reg.gpr + GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1])) = l;
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;

        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];

    OP_ADD:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<u64>(vm->reg.gpr[to].mem) += Cast<u64>(vm->reg.gpr[from].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_SUB:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<u64>(vm->reg.gpr[to].mem) -= Cast<u64>(vm->reg.gpr[from].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];


    OP_MUL:
        {
            u32 index = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 to = GetLow4bits( Cast<byte*>(vm->reg.gpr[RPC].mem)[1] );
            u32 from = (Cast<byte*>(vm->reg.gpr[RPC].mem)[2]);

            switch (index) {
                case 0:
                Cast<u8>(vm->reg.gpr[to].mem) *= Cast<u8>(vm->reg.gpr[from].mem);
                break;

                case 1:
                Cast<u16>(vm->reg.gpr[to].mem) *= Cast<u16>(vm->reg.gpr[from].mem);
                break;
                
                case 2:
                Cast<u32>(vm->reg.gpr[to].mem) *= Cast<u32>(vm->reg.gpr[from].mem);
                break;

                case 3:
                Cast<u64>(vm->reg.gpr[to].mem) *= Cast<u64>(vm->reg.gpr[from].mem);
                break;
            
            }

            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_DIV:
        {
            u32 index = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 to = GetLow4bits( Cast<byte*>(vm->reg.gpr[RPC].mem)[1] );
            u32 from = (Cast<byte*>(vm->reg.gpr[RPC].mem)[2]);

            switch (index) {
                case 0:
                Cast<u8>(vm->reg.gpr[to].mem) /= Cast<u8>(vm->reg.gpr[from].mem);
                break;

                case 1:
                Cast<u16>(vm->reg.gpr[to].mem) /= Cast<u16>(vm->reg.gpr[from].mem);
                break;
                
                case 2:
                Cast<u32>(vm->reg.gpr[to].mem) /= Cast<u32>(vm->reg.gpr[from].mem);
                break;

                case 3:
                Cast<u64>(vm->reg.gpr[to].mem) /= Cast<u64>(vm->reg.gpr[from].mem);
                break;
            
            }

            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_IMUL:
        {
            u32 index = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 to = GetLow4bits( Cast<byte*>(vm->reg.gpr[RPC].mem)[1] );
            u32 from = (Cast<byte*>(vm->reg.gpr[RPC].mem)[2]);

            switch (index) {
                case 0:
                Cast<i8>(vm->reg.gpr[to].mem) *= Cast<i8>(vm->reg.gpr[from].mem);
                break;

                case 1:
                Cast<i16>(vm->reg.gpr[to].mem) *= Cast<i16>(vm->reg.gpr[from].mem);
                break;
                
                case 2:
                Cast<i32>(vm->reg.gpr[to].mem) *= Cast<i32>(vm->reg.gpr[from].mem);
                break;

                case 3:
                Cast<i64>(vm->reg.gpr[to].mem) *= Cast<i64>(vm->reg.gpr[from].mem);
                break;
            
            }
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_IDIV:
        {
            u32 index = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 to = GetLow4bits( Cast<byte*>(vm->reg.gpr[RPC].mem)[1] );
            u32 from = (Cast<byte*>(vm->reg.gpr[RPC].mem)[2]);

            switch (index) {
                case 0:
                Cast<i8>(vm->reg.gpr[to].mem) /= Cast<i8>(vm->reg.gpr[from].mem);
                break;

                case 1:
                Cast<i16>(vm->reg.gpr[to].mem) /= Cast<i16>(vm->reg.gpr[from].mem);
                break;
                
                case 2:
                Cast<i32>(vm->reg.gpr[to].mem) /= Cast<i32>(vm->reg.gpr[from].mem);
                break;

                case 3:
                Cast<i64>(vm->reg.gpr[to].mem) /= Cast<i64>(vm->reg.gpr[from].mem);
                break;
            
            }
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];



    OP_ADD_S:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f32>(vm->reg.gpr[to].mem) += Cast<f32>(vm->reg.gpr[from].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_SUB_S:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f32>(vm->reg.gpr[to].mem) -= Cast<f32>(vm->reg.gpr[from].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_MUL_S:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f32>(vm->reg.gpr[to].mem) *= Cast<f32>(vm->reg.gpr[from].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_DIV_S:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f32>(vm->reg.gpr[to].mem) /= Cast<f32>(vm->reg.gpr[from].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_ADD_D:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f64>(vm->reg.gpr[to].mem) += Cast<f64>(vm->reg.gpr[from].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_SUB_D:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f64>(vm->reg.gpr[to].mem) -= Cast<f64>(vm->reg.gpr[from].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_MUL_D:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f64>(vm->reg.gpr[to].mem) *= Cast<f64>(vm->reg.gpr[from].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_DIV_D:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f64>(vm->reg.gpr[to].mem) /= Cast<f64>(vm->reg.gpr[from].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];


    OP_CJMP:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;

            if( Cast<u64>(vm->reg.gpr[to].mem) != 0 ) {
                Cast<byte*>(vm->reg.gpr[RPC].mem) = (byte*)vm->loadedProgram + Cast<u64>(vm->reg.gpr[from].mem);
            }
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_JMP:
        {
            u32 to = (Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u64 jmpTarget = Cast<u64>(vm->reg.gpr[to].mem);
            Cast<byte*>(vm->reg.gpr[RPC].mem) = (byte*)vm->loadedProgram + Cast<u64>(vm->reg.gpr[to].mem);
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_JMP_IM:
        {
            u64 to = Cast<u64>(Cast<byte*>(vm->reg.gpr[RPC].mem) + 1);
            Cast<byte*>(vm->reg.gpr[RPC].mem) = (byte*)vm->loadedProgram + to;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_LOAD:
        {
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            bool not_out_of_bounds = (Cast<u64>(vm->reg.gpr + from) < stackEnd ) & (Cast<u64>(vm->reg.gpr + from) > readOnlyStart);

            auto add = Cast<u64>(vm->reg.gpr + from);
            if(!not_out_of_bounds) {
                return VM_ERROR;
            }
            
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u64 mask = masks[Cast<byte*>(vm->reg.gpr[RPC].mem)[2]];


            Cast<u64>(vm->reg.gpr + to) = Cast<u64>((byte*)vm->loadedProgram + Cast<u64>(vm->reg.gpr + from)) & mask;
            //std::cout << "load " << Cast<u64>(vm->reg.gpr + to) << " [" << Cast<u64>(vm->reg.gpr + from) << "]" << std::endl;
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_STORE:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            bool not_out_of_bounds = (Cast<u64>(vm->reg.gpr + to) > RWStart ) & (Cast<u64>(vm->reg.gpr + to) < stackEnd);
            if(!not_out_of_bounds) {
                return VM_ERROR;
            }


            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            switch (Cast<byte*>(vm->reg.gpr[RPC].mem)[2]) {
                case 0:
                    Cast<u8>((byte*)vm->loadedProgram + Cast<u64>(vm->reg.gpr + to)) = Cast<u8>(vm->reg.gpr + from);
                    break;
                case 1:
                    Cast<u16>((byte*)vm->loadedProgram + Cast<u64>(vm->reg.gpr + to)) = Cast<u16>(vm->reg.gpr + from);
                    break;
                case 2:
                    Cast<u32>((byte*)vm->loadedProgram + Cast<u64>(vm->reg.gpr + to)) = Cast<u32>(vm->reg.gpr + from);
                    break;
                case 3:
                    Cast<u64>((byte*)vm->loadedProgram + Cast<u64>(vm->reg.gpr + to)) = Cast<u64>(vm->reg.gpr + from);
                    break;
            }

            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_INTERUPT:
        {
            u64 index = Cast<u64>(vm->reg.gpr[R0].mem);
            ASSERT(index < vm->interupts.size);
            vm->interupts.handlers[index](vm);
            if(vm->abort) {
                return VM_PROGRAM_ABORT;
            }
            else if(vm->running == false) {
                return VM_PROGRAM_YIELD;
            }
            else {
                return VM_PROGRAM_EXIT;
            }
            Cast<byte*>(vm->reg.gpr[RPC].mem)++;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];

    OP_CSFS:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<i32>(vm->reg.gpr + to) = Cast<f32>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CSFU:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<u32>(vm->reg.gpr + to) = Cast<f32>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CSSF:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f32>(vm->reg.gpr + to) = Cast<i32>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CUSF:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f32>(vm->reg.gpr + to) = Cast<u32>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CDFS:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<i64>(vm->reg.gpr + to) = Cast<f64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CDFU:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<u64>(vm->reg.gpr + to) = Cast<f64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CSDF:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f64>(vm->reg.gpr + to) = Cast<i64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CUDF:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f64>(vm->reg.gpr + to) = Cast<u64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CSFDF:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f64>(vm->reg.gpr + to) = Cast<f32>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CDFSF:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<f32>(vm->reg.gpr + to) = Cast<f64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_I_LT:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = Cast<i64>(vm->reg.gpr + to) < Cast<i64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_I_GT:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = Cast<i64>(vm->reg.gpr + to) > Cast<i64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_I_E:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = Cast<i64>(vm->reg.gpr + to) == Cast<i64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_U_LT:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = Cast<u64>(vm->reg.gpr + to) < Cast<u64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_U_GT:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = Cast<u64>(vm->reg.gpr + to) > Cast<u64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_SF_LT:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = Cast<f32>(vm->reg.gpr + to) < Cast<f32>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_SF_GT:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = Cast<f32>(vm->reg.gpr + to) > Cast<f32>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_SF_E:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = Cast<f32>(vm->reg.gpr + to) == Cast<f32>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_DF_LT:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = Cast<f64>(vm->reg.gpr + to) < Cast<f64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_DF_GT:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = Cast<f64>(vm->reg.gpr + to) > Cast<f64>(vm->reg.gpr + from);
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CMP_DF_E:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 from = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 dst = Cast<byte*>(vm->reg.gpr[RPC].mem)[2];

            Cast<u64>(vm->reg.gpr + dst) = (Cast<f64>(vm->reg.gpr + to) == Cast<f64>(vm->reg.gpr + from));
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 3;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    
    OP_LOAD_REL_RSP:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 mode = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 memOffset = Cast<u16>((Cast<byte*>(vm->reg.gpr[RPC].mem) + 2));

            switch(mode) {
            case 0:
                Cast<u64>(vm->reg.gpr + to) = Cast<u8>(Cast<byte*>(vm->reg.gpr[RSP].mem) + memOffset);
                break;
            case 1:
                Cast<u64>(vm->reg.gpr + to) = Cast<u16>(Cast<byte*>(vm->reg.gpr[RSP].mem) + memOffset);
                break;
            case 2:
                Cast<u64>(vm->reg.gpr + to) = Cast<u32>(Cast<byte*>(vm->reg.gpr[RSP].mem) + memOffset);
                break;
            case 3:
                Cast<u64>(vm->reg.gpr + to) = Cast<u64>(Cast<byte*>(vm->reg.gpr[RSP].mem) + memOffset);
                break;
            }
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 4;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_STORE_REL_RSP:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 mode = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 memOffset = Cast<u16>((Cast<byte*>(vm->reg.gpr[RPC].mem) + 2));
            switch(mode) {
            case 0:
                Cast<u8>(Cast<byte*>(vm->reg.gpr[RSP].mem) + memOffset) = Cast<u8>(vm->reg.gpr + to);
                break;
            case 1:
                Cast<u16>(Cast<byte*>(vm->reg.gpr[RSP].mem) + memOffset) = Cast<u16>(vm->reg.gpr + to);
                break;
            case 2:
                Cast<u32>(Cast<byte*>(vm->reg.gpr[RSP].mem) + memOffset) = Cast<u32>(vm->reg.gpr + to);
                break;
            case 3:
                Cast<u64>(Cast<byte*>(vm->reg.gpr[RSP].mem) + memOffset) = Cast<u64>(vm->reg.gpr + to);
                break;
            }
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 4;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_LOAD_REL_RBP:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 mode = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 memOffset = Cast<u16>((Cast<byte*>(vm->reg.gpr[RPC].mem) + 2));

            switch(mode) {
            case 0:
                Cast<u64>(vm->reg.gpr + to) = Cast<u8>(Cast<byte*>(vm->reg.gpr[RBP].mem) + memOffset);
                break;
            case 1:
                Cast<u64>(vm->reg.gpr + to) = Cast<u16>(Cast<byte*>(vm->reg.gpr[RBP].mem) + memOffset);
                break;
            case 2:
                Cast<u64>(vm->reg.gpr + to) = Cast<u32>(Cast<byte*>(vm->reg.gpr[RBP].mem) + memOffset);
                break;
            case 3:
                Cast<u64>(vm->reg.gpr + to) = Cast<u64>(Cast<byte*>(vm->reg.gpr[RBP].mem) + memOffset);
                break;
            }
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 4;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_STORE_REL_RBP:
        {
            u32 to = GetHigh4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 mode = GetLow4bits(Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            u32 memOffset = Cast<u16>((Cast<byte*>(vm->reg.gpr[RPC].mem) + 2));
            switch(mode) {
            case 0:
                Cast<u8>(Cast<byte*>(vm->reg.gpr[RBP].mem) + memOffset) = Cast<u8>(vm->reg.gpr + to);
                break;
            case 1:
                Cast<u16>(Cast<byte*>(vm->reg.gpr[RBP].mem) + memOffset) = Cast<u16>(vm->reg.gpr + to);
                break;
            case 2:
                Cast<u32>(Cast<byte*>(vm->reg.gpr[RBP].mem) + memOffset) = Cast<u32>(vm->reg.gpr + to);
                break;
            case 3:
                Cast<u64>(Cast<byte*>(vm->reg.gpr[RBP].mem) + memOffset) = Cast<u64>(vm->reg.gpr + to);
                break;
            }
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 4;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_LEA:
        {
            u16 h = *((u16*)(Cast<byte*>(vm->reg.gpr[RPC].mem) + 1));
            u32 dst = (h >> 12) & 0xF;
            u32 base = (h >> 8) & 0xF;
            u32 offset = (h >> 4) & 0xF;
            u32 shift = (h) & 0xF;
            i16 im = *((i16*)(Cast<byte*>(vm->reg.gpr[RPC].mem)+3));
            byte extra = Cast<byte*>(vm->reg.gpr[RPC].mem)[5];

            Cast<u64>(vm->reg.gpr[dst].mem) = 0;
            if(extra & 1) {
                if(base == RSP || base == RBP) {
                    Cast<u64>(vm->reg.gpr[dst].mem) += Cast<u64>(vm->reg.gpr[base].mem) - (u64)vm->loadedProgram;
                }
                else {
                    Cast<u64>(vm->reg.gpr[dst].mem) += Cast<u64>(vm->reg.gpr[base].mem);
                }
            }
            if(extra & 2) {
                if(base == RSP || base == RBP) {
                    Cast<u64>(vm->reg.gpr[dst].mem) += (Cast<u64>(vm->reg.gpr[offset].mem) << shift) - (u64)vm->loadedProgram;;
                }
                else {
                    Cast<u64>(vm->reg.gpr[dst].mem) += (Cast<u64>(vm->reg.gpr[offset].mem) << shift);
                }
            }
            Cast<u64>(vm->reg.gpr[dst].mem) += im;
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 6;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_GET_STACK_PTR:
        {
            u32 to = (Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<u64>(vm->reg.gpr + to) = Cast<u64>(vm->reg.gpr[RSP].mem) - (u64)vm->loadedProgram;
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];

    OP_GET_BASE_PTR:
        {
            u32 to = (Cast<byte*>(vm->reg.gpr[RPC].mem)[1]);
            Cast<u64>(vm->reg.gpr + to) = Cast<u64>(vm->reg.gpr[RBP].mem) - (u64)vm->loadedProgram;
            Cast<byte*>(vm->reg.gpr[RPC].mem) += 2;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CALL_R:
        {
            RegisterName reg = (RegisterName)(Cast<byte*>(vm->reg.gpr[RPC].mem))[1];
            u64 ret = Cast<u64>(vm->reg.gpr[RPC].mem) - (u64)vm->loadedProgram;
            Cast<byte*>(vm->reg.gpr[RSP].mem) -= 8;
            *Cast<u64*>(vm->reg.gpr[RSP].mem) = ret + 2;
            
            Cast<byte*>(vm->reg.gpr[RPC].mem) = (byte*)vm->loadedProgram + Cast<u64>(vm->reg.gpr[reg].mem);
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_CALL:
        {
            byte* add = Cast<byte*>(vm->reg.gpr[RPC].mem);
            u32 target = *((u32*)(add + 1));
            u64 ret = Cast<u64>(vm->reg.gpr[RPC].mem) - (u64)vm->loadedProgram;
            
            Cast<byte*>(vm->reg.gpr[RSP].mem) -= 8;
            *Cast<u64*>(vm->reg.gpr[RSP].mem) = ret + 5;
            
            Cast<byte*>(vm->reg.gpr[RPC].mem) = (byte*)vm->loadedProgram + target;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_RET:
        {
            u32 target = *Cast<u64*>(vm->reg.gpr[RSP].mem);
            Cast<byte*>(vm->reg.gpr[RSP].mem) += 8;
            Cast<byte*>(vm->reg.gpr[RPC].mem) = (byte*)vm->loadedProgram + target;
        }
        goto *dispatch_table[*Cast<byte*>(vm->reg.gpr[RPC].mem)];
    OP_EXIT:
        return VM_PROGRAM_EXIT;

    return VM_OK;
}

void DumpStack(VM* vm) {
    std::cout << "stack" << std::endl;
    for(byte* i = vm->stackEnd ; i > Cast<byte*>(vm->reg.gpr[RSP].mem); i-- ) {
        std::cout << (void*)i << " " << (i32)(*i) << std::endl;
    }
}
void DumpGlobals(VM* vm) {
    std::cout << "globals" << std::endl;
    for(u32 i = vm->loadedProgram->header.globals_start_offset; i < vm->loadedProgram->header.globals_end_offset; i++) {
        std::cout << (((u32)((byte*)vm->loadedProgram)[i]) & 0xFF) << std::endl;
    }
}

u8 ToFrom(RegisterName to , RegisterName from) {
    return (to << 4) | (from & 0x0F);
}

struct VMArgs {
    const char* input;
    bool dump_stack;
    bool dump_register_file;
    bool dump_globals;
    bool show_vm_status;
    bool jit_compile;
};

bool StrCmp(const char* str0, const char* str1) {
    ASSERT(str0 != nullptr && str1 != nullptr);
    while( *str0 == *str1 && *str0 != 0) {str0++;str1++;}
    return *str0 == *str1;
}

VMArgs ParseVMArgs(i32 argc, char** args) {

    VMArgs ret{};
    ret.jit_compile = false;
    if(argc < 2) {
        std::cout << "ERROR: no arguments please run with --help" << std::endl;
        exit(1);
    }
    for(u32 i = 1; i < argc; i++) {

        if(StrCmp(args[i], "--help")) {
            std::cout << "--help                    prints this message" << std::endl;
            std::cout << "--in                      specifies binary to execute" << std::endl;
            std::cout << "--jit-compile             compiles to x64 before execution" << std::endl;
            std::cout << "--dump-stack              dump the stack after execution" << std::endl;
            std::cout << "--dump-register-file      dump the register file after execution" << std::endl;
            std::cout << "--dump-globals            dump globals after execution" << std::endl;
            std::cout << "--show-vm-status          shows vm status after execution" << std::endl;
        }
        else if(StrCmp(args[i], "--in")) {
            if(argc-1 <= i) {
                std::cout << "ERROR: missing arg" << std::endl;
                exit(1);
            }
            ret.input = args[i+1];
            i += 1;
        }
        else if(StrCmp(args[i], "--dump-stack")) {
            ret.dump_stack = true;
        }
        else if(StrCmp(args[i], "--jit-compile")) {
            ret.jit_compile = true;
        }
        else if(StrCmp(args[i], "--dump-register-file")) {
            ret.dump_register_file = true;
        }
        else if(StrCmp(args[i], "--dump-globals")) {
            ret.dump_globals = true;
        }
        else if(StrCmp(args[i], "--show-vm-status")) {
            ret.show_vm_status = true;
        }
        else {
            std::cout << "ERROR: unknown argument please run with --help" << std::endl;
        }
    }

    return ret;
}

const char* GetVmStatusStr(VmStatus status) {
    constexpr const char* strings[4] = { "VM_OK","VM_ERROR","VM_PROGRAM_EXIT","VM_PROGRAM_ABORT"};
    ASSERT(status < 4);
    return strings[status];
}


i32 main(i32 argc , char** argv) {
 
    VMArgs args = ParseVMArgs(argc, argv);

    if(args.input) {

        u32 size;
        Executable* bin = (Executable*)ReadFileTerminated(args.input, &size);
        if(!bin) {
            std::cout << "input file doesn't exist" << std::endl;
            exit(1);
        }
        VM virtual_machine;
        InitVM(&virtual_machine);
        LoadProgramVM(&virtual_machine , bin);

        VmStatus status;
        if(args.jit_compile) {
            char* jitMem = (char*)malloc(32 * KILO_BYTE);
            byte* code = ((byte*)virtual_machine.loadedProgram + virtual_machine.loadedProgram->header.code_start_offset);
            u32 codeSize = virtual_machine.loadedProgram->header.code_end_offset - virtual_machine.loadedProgram->header.code_start_offset;

            void* stack = malloc(8 * MEGA_BYTE);
            jit_compiled_t jit_code = jit_compile(&virtual_machine, (byte*)jitMem, stack, code, codeSize);
            free(jitMem);
            status = jit_code();
        }
        else {
            status = ExecuteVM(&virtual_machine);
        }
        std::cout << std::endl;
        if(args.show_vm_status) {
            std::cout << std::endl << std::dec << std::noshowbase << (args.jit_compile ? "just-in-time: " : "byte-code interpreted: ") << GetVmStatusStr(status) << std::endl;
        }

        if(args.dump_stack) {
            DumpStack(&virtual_machine);
        }
        if(args.dump_globals) {
            DumpGlobals(&virtual_machine);
        }
        if(args.dump_register_file) {
            DumpRegisterFileVM(&virtual_machine);
        }

        ShutDownVM(&virtual_machine);
    }
    return 0;
}