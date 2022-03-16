#include <vm_common.h>
#include <sys/mman.h>

u32 StrLen(const char* str) {
    u32 r = 0;
    while( str[r] != 0 ) r++;
    return r;
}
u32 U64ToString(char* str, u64 n) {

    if(n == 0) {
        str[0] = (n % 10 + '0');
        return 1;
    }

    i32 i = 0;
    u64 m = n;
    while(m != 0) {
        m /= 10;
        i++;
    }
    
    u32 size = i--;
    for(; i > -1; i--) {
        str[i] = (n % 10 + '0');
        n /= 10;
    }
    return size;
}


void WriteRegister(byte* mem, u32* index, RegisterName r, u32 size) {

    switch (r) {
    case R0:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "rax";
                size = 3;
                break;
            case 2:
                str = "eax";
                size = 3;
                break;
            case 1: 
                str = "ax";
                size = 2;
                break;
            case 0:
                str = "al";
                size = 2;
                break;
            }

            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R1:
        {   
            const char* str;
            switch(size) {
            case 3:
                str = "rcx";
                size = 3;
                break;
            case 2:
                str = "ecx";
                size = 3;
                break;
            case 1: 
                str = "cx";
                size = 2;
                break;
            case 0:
                str = "cl";
                size = 2;
                break;
            }

            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R2:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "rdx";
                size = 3;
                break;
            case 2:
                str = "edx";
                size = 3;
                break;
            case 1: 
                str = "dx";
                size = 2;
                break;
            case 0:
                str = "dl";
                size = 2;
                break;
            }

            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R3:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "rbx";
                size = 3;
                break;
            case 2:
                str = "ebx";
                size = 3;
                break;
            case 1: 
                str = "bx";
                size = 2;
                break;
            case 0:
                str = "bl";
                size = 2;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R4:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "rsi";
                size = 3;
                break;
            case 2:
                str = "esi";
                size = 3;
                break;
            case 1: 
                str = "sx";
                size = 2;
                break;
            case 0:
                str = "sil";
                size = 3;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R5:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "rdi";
                size = 3;
                break;
            case 2:
                str = "edi";
                size = 3;
                break;
            case 1: 
                str = "di";
                size = 2;
                break;
            case 0:
                str = "dil";
                size = 3;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R6:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "r8";
                size = 2;
                break;
            case 2:
                str = "r8d";
                size = 3;
                break;
            case 1: 
                str = "r8w";
                size = 3;
                break;
            case 0:
                str = "r8b";
                size = 3;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R7:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "r9";
                size = 2;
                break;
            case 2:
                str = "r9d";
                size = 3;
                break;
            case 1: 
                str = "r9w";
                size = 3;
                break;
            case 0:
                str = "r9b";
                size = 3;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R8:
        {
            char* str;
            switch(size) {
            case 3:
                str = "r10";
                size = 3;
                break;
            case 2:
                str = "r10d";
                size = 3;
                break;
            case 1: 
                str = "r10w";
                size = 3;
                break;
            case 0:
                str = "r10b";
                size = 3;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R9:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "r11";
                size = 3;
                break;
            case 2:
                str = "r11d";
                size = 4;
                break;
            case 1: 
                str = "r11w";
                size = 4;
                break;
            case 0:
                str = "r11b";
                size = 4;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R10:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "r12";
                size = 3;
                break;
            case 2:
                str = "r12d";
                size = 4;
                break;
            case 1: 
                str = "r12w";
                size = 4;
                break;
            case 0:
                str = "r12b";
                size = 4;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R11:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "r13";
                size = 3;
                break;
            case 2:
                str = "r13d";
                size = 4;
                break;
            case 1: 
                str = "r13w";
                size = 4;
                break;
            case 0:
                str = "r13b";
                size = 4;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case R12:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "r14";
                size = 3;
                break;
            case 2:
                str = "r14d";
                size = 4;
                break;
            case 1: 
                str = "r14w";
                size = 4;
                break;
            case 0:
                str = "r14b";
                size = 4;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case RRV:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "r15";
                size = 3;
                break;
            case 2:
                str = "r15d";
                size = 4;
                break;
            case 1: 
                str = "r15w";
                size = 4;
                break;
            case 0:
                str = "r15b";
                size = 4;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case RPC:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "rip";
                size = 3;
                break;
            case 2:
                str = "eip";
                size = 3;
                break;
            case 1: 
                str = "ip";
                size = 2;
                break;
            case 0:
                str = "ipl";
                size = 3;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case RSP:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "rsp";
                size = 3;
                break;
            case 2:
                str = "esp";
                size = 3;
                break;
            case 1: 
                str = "sp";
                size = 2;
                break;
            case 0:
                str = "spl";
                size = 3;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    case RBP:
        {
            const char* str;
            switch(size) {
            case 3:
                str = "rbp";
                size = 3;
                break;
            case 2:
                str = "ebp";
                size = 3;
                break;
            case 1: 
                str = "bp";
                size = 2;
                break;
            case 0:
                str = "bpl";
                size = 3;
                break;
            }
            for(u32 i = 0;i < size; (*index)++,i++) {
                mem[*index] = str[i];
            }
            break;
        }
    }
}
void WriteOP(byte* mem, u32* index, const char* str, u32 size) {
    for(u32 i = 0; i < size; i++,(*index)++) {
        mem[*index] = str[i];
    }
}
void WriteOP(byte* mem, u32* index, const char* str) {

    u32 size = StrLen(str);
    for(u32 i = 0; i < size; i++,(*index)++) {
        mem[*index] = str[i];
    }
}


void WriteMulDivInst(byte* mem, u32* index, RegisterName l, RegisterName r, u32 size, const char* op) {


    if(l != R0 && l != R2 && r != R0 && r != R2) {
        WriteOP(mem , index, "push rax\n");
        WriteOP(mem , index, "push rdx\n");

        WriteOP(mem , index, "mov rax , ");
        WriteRegister(mem, index, l, size);
        WriteOP(mem , index, "\n");

        WriteOP(mem , index, op);
        WriteOP(mem , index, " ");
        WriteRegister(mem, index, r, size);
        WriteOP(mem , index, "\n");
        WriteOP(mem , index, "mov ");
        WriteRegister(mem, index,l, size);
        WriteOP(mem , index, " , rax\n");

        WriteOP(mem , index, "pop rdx\n");
        WriteOP(mem , index, "pop rax\n");
    }
    else if(l == R0 && r != R0 && r != R2) {
        WriteOP(mem , index, "push rdx\n");

        WriteOP(mem , index, op);
        WriteOP(mem , index, " ");
        WriteRegister(mem ,index, r, size);
        WriteOP(mem , index, "\n");

        WriteOP(mem , index, "pop rdx\n");
    }
    else if(l == R2 && r != R0 && r != R2) {
        WriteOP(mem , index, "push rax\n");

        WriteOP(mem , index, "mov rax , rdx\n");
        WriteOP(mem , index, op);
        WriteOP(mem , index, " ");
        WriteRegister(mem ,index, r, size);
        WriteOP(mem , index, "\n");
        WriteOP(mem , index, "mov rdx , rax\n");

        WriteOP(mem , index, "pop rax\n");
    }
    else if(r == R0 && l != R0 && l != R2) {

        WriteOP(mem , index, "push rax\n");
        WriteOP(mem , index, "push rdx\n");

        WriteOP(mem , index, op);
        WriteOP(mem , index, " ");
        WriteRegister(mem ,index, l, size);
        WriteOP(mem , index, "\n");
        WriteOP(mem , index, "mov ");
        WriteRegister(mem ,index, l, size);
        WriteOP(mem , index, " , rax\n");

        WriteOP(mem , index, "pop rdx\n");
        WriteOP(mem , index, "pop rax\n");
    }
    else if(r == R2 && l != R0 && l != R2) {
        WriteOP(mem , index, "push rax\n");
        WriteOP(mem , index, "push rdx\n");

        WriteOP(mem , index, "mov rax , rdx\n");
        WriteOP(mem , index, op);
        WriteOP(mem , index, " ");
        WriteRegister(mem ,index, l, size);
        WriteOP(mem , index, "\n");
        WriteOP(mem , index, "mov ");
        WriteRegister(mem ,index, l, size);
        WriteOP(mem , index, " , rax\n");

        WriteOP(mem , index, "pop rdx\n");
        WriteOP(mem , index, "pop rax\n");
    }
    else if(l == R0 && r == R2) {

        WriteOP(mem , index, "push rcx\n");
        WriteOP(mem , index, "push rdx\n");
        
        WriteOP(mem , index, "mov rcx , rdx\n");
        WriteOP(mem , index, op);
        WriteOP(mem , index, " rcx\n");

        WriteOP(mem , index, "pop rdx\n");
        WriteOP(mem , index, "pop rcx\n");
    }
    else if(l == R2 && r == R0) {

        WriteOP(mem , index, "push rax\n");
        WriteOP(mem , index, "push rcx\n");
        WriteOP(mem , index, "mov rcx , rdx\n");
        WriteOP(mem , index, op);
        WriteOP(mem , index, " rcx\n");
        WriteOP(mem , index, "mov rdx , rax\n");
        WriteOP(mem , index, "pop rcx\n");
        WriteOP(mem , index, "pop rax\n");
    }
    else {ASSERT(false)}
}

void DeleteInstruction(byte* mem, u32 index, char terminator) {
    
    for(; ((char*)mem)[index] != terminator; index++) {
        ((char*)mem)[index] = ' ';
    }
}

void InsertStr(byte* scratchMem, byte* mem, u32* end, u32 at, const char* str, u32 size) {

    u32 cpySize = (*end) - at;
    MemCpy(scratchMem, mem + at, cpySize);
    WriteOP(mem, &at, str, size);
    MemCpy(mem + at, scratchMem, cpySize);
    (*end) += size;
}


void PrintU64(u64 n) {
    (std::cout << n).flush();
}
void PrintI64(i64 n) {
    (std::cout << n).flush();
}
void PrintF64(f64 n) {
    (std::cout << n).flush();
}
void PrintF32(f32 n) {
    (std::cout << n).flush();
}
void PrintString(const char* str, u32 size) {
    (std::cout.write(str,size)).flush();
}

jit_compiled_t jit_compile(VM* vm, byte* mem, void* stack, byte* program, u32 codeSize) {

    void* compiled = mmap(nullptr, 4*KILO_BYTE, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_ANONYMOUS | MAP_PRIVATE ,0,0);
    
    void* scratch = malloc(4 * KILO_BYTE);
    u32 labelCount = 0;

    vm->tracker.opOffsets.Init();
    for(u32 i = 0; i < REGISTER_COUNT; i++) {
        vm->tracker.history[i].Init();
    }

    u32 k = 0;
    char str[256];

    WriteOP(mem, &k, "[bits 64]\n", sizeof("[bits 64]\n")-1);
    WriteOP(mem, &k, "\n");
    WriteOP(mem, &k, "push rax\n");
    WriteOP(mem, &k, "push rcx\n");
    WriteOP(mem, &k, "push rdx\n");
    WriteOP(mem, &k, "push rbx\n");
    WriteOP(mem, &k, "push rsi\n");
    WriteOP(mem, &k, "push rdi\n");
    WriteOP(mem, &k, "push r8\n");
    WriteOP(mem, &k, "push r9\n");
    WriteOP(mem, &k, "push r10\n");
    WriteOP(mem, &k, "push r11\n");
    WriteOP(mem, &k, "push r12\n");
    WriteOP(mem, &k, "push r13\n");
    WriteOP(mem, &k, "push r14\n");
    WriteOP(mem, &k, "push r15\n");

    WriteOP(mem, &k, "push rsp\n");
    WriteOP(mem, &k, "push rbp\n");

    WriteOP(mem, &k, "mov rax, rsp\n");
    WriteOP(mem, &k, "mov rsp, ");
    u32 size = U64ToString(str, ((u64)stack) + 8 * MEGA_BYTE);
    WriteOP(mem, &k, str, size);
    WriteOP(mem, &k, "\n");
    WriteOP(mem, &k, "push rax\n");
    WriteOP(mem, &k, "mov rbp, rsp\n");

    WriteOP(mem, &k, "\n");

    for(u32 i = 0; i < codeSize; i++) {

        vm->tracker.opOffsets.Insert(i,k);
        switch(program[i]) {
        case NO_OP:
            break;
        case OP_MOV_IM8:
            {
                RegisterName r = (RegisterName)(program[++i]);
                auto hh = program[i];
                vm->tracker.history[r].PushBack({i-1,k});
                vm->tracker.known[r] = true;
                u64 val = (u32)(program[++i]);
                Cast<u64>(vm->tracker.val.gpr[r].mem) = val;

                if(val == 0) {
                    WriteOP(mem, &k, "xor ", sizeof("xor ")-1);
                    WriteRegister(mem, &k, r, 2);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteRegister(mem, &k, r, 2);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                else {
                    u32 size = sprintf(str, "%d", val);
                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    WriteRegister(mem, &k, r, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteOP(mem, &k, str, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                break;
            }
        case OP_MOV_IM16:
            {
                RegisterName r = (RegisterName)(program[++i]);
                vm->tracker.history[r].PushBack({i-1,k});
                vm->tracker.known[r] = true;
                i++;
                u16 im = *((u16*)(program + i));
                i++;

                Cast<u64>(vm->tracker.val.gpr[r].mem) = im;
                if(im == 0) {
                    WriteOP(mem, &k, "xor ", sizeof("xor ")-1);
                    WriteRegister(mem, &k, r, 2);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteRegister(mem, &k, r, 2);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                else {

                    u32 size = sprintf(str, "%d", im);
                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    WriteRegister(mem, &k, r, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteOP(mem, &k, str, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                break;
            }
        case OP_MOV_IM32:
            {
                RegisterName r = (RegisterName)(program[++i]);
                vm->tracker.history[r].PushBack({i-1,k});
                vm->tracker.known[r] = true;
                i++;
                u32 im = *((u32*)(program + i));
                i += 3;
                Cast<u64>(vm->tracker.val.gpr[r].mem) = im;

                if(im == 0) {
                    WriteOP(mem, &k, "xor ", sizeof("xor ")-1);
                    WriteRegister(mem, &k, r, 2);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteRegister(mem, &k, r, 2);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                else {

                    u32 size = sprintf(str, "%d", im);
                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    WriteRegister(mem, &k, r, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteOP(mem, &k, str, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                break;
            }
        case OP_MOV_IM64:
            {
                RegisterName r = (RegisterName)(program[++i]);
                vm->tracker.history[r].PushBack({i-1,k});
                vm->tracker.known[r] = true;
                i++;
                u64 im = *((u64*)(program + i));
                i += 7;
                Cast<u64>(vm->tracker.val.gpr[r].mem) = im;
                if(im == 0) {
                    WriteOP(mem, &k, "xor ", sizeof("xor ")-1);
                    WriteRegister(mem, &k, r, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteRegister(mem, &k, r, 3);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                else {

                    u32 size = sprintf(str, "%d", im);
                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    WriteRegister(mem, &k, r, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteOP(mem, &k, str, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                break;
            }
        case OP_MOV8:
        case OP_MOV16:
        case OP_MOV32:
        case OP_MOV64:
            {
                RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                RegisterName l = (RegisterName)GetLow4bits(program[i]);
                if(l == h) {
                    break;
                }

                vm->tracker.history[h].PushBack({i-1,k});
                vm->tracker.known[h] = vm->tracker.known[l];
                if(vm->tracker.known[l]) {
                    Cast<u64>(vm->tracker.val.gpr[h].mem) = Cast<u64>(vm->tracker.val.gpr[l].mem);
                    DeleteInstruction(mem,vm->tracker.history[l].Back().value,'\n');

                    u32 val = Cast<u64>(vm->tracker.val.gpr[l].mem);
                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    WriteRegister(mem, &k, h, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    u32 size = U64ToString(str, val);
                    WriteOP(mem, &k, str, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                else {

                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    WriteRegister(mem, &k, h, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteRegister(mem, &k, l, 3);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }

                break;
            }
        case OP_OR:
            {
                RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                RegisterName l = (RegisterName)GetLow4bits(program[i]);

                vm->tracker.history[h].PushBack({i-1,k});
                vm->tracker.known[h] = vm->tracker.known[l];
                if(vm->tracker.known[l]) {
                    Cast<u64>(vm->tracker.val.gpr[h].mem) |= Cast<u64>(vm->tracker.val.gpr[l].mem);
                    DeleteInstruction(mem,vm->tracker.history[l].Back().value,'\n');

                    u32 val = Cast<u64>(vm->tracker.val.gpr[l].mem);
                    WriteOP(mem, &k, "or ", sizeof("or ")-1);
                    WriteRegister(mem, &k, h, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    u32 size = U64ToString(str, val);
                    WriteOP(mem, &k, str, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                else {

                    WriteOP(mem, &k, "or ", sizeof("or ")-1);
                    WriteRegister(mem, &k, h, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteRegister(mem, &k, l, 3);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                break;
            }
        case OP_AND:
            {
                RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                RegisterName l = (RegisterName)GetLow4bits(program[i]);

                vm->tracker.history[h].PushBack({i-1,k});
                vm->tracker.known[h] = vm->tracker.known[l];

                if(vm->tracker.known[l]) {
                    Cast<u64>(vm->tracker.val.gpr[h].mem) &= Cast<u64>(vm->tracker.val.gpr[l].mem);
                    DeleteInstruction(mem,vm->tracker.history[l].Back().value,'\n');

                    u32 val = Cast<u64>(vm->tracker.val.gpr[l].mem);
                    WriteOP(mem, &k, "and ", sizeof("and ")-1);
                    WriteRegister(mem, &k, h, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    u32 size = U64ToString(str, val);
                    WriteOP(mem, &k, str, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                else {

                    WriteOP(mem, &k, "and ", sizeof("and ")-1);
                    WriteRegister(mem, &k, h, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteRegister(mem, &k, l, 3);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                break;
            }
        case OP_NOT:
            {
                RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                RegisterName l = (RegisterName)GetLow4bits(program[i]);

                vm->tracker.history[h].PushBack({i-1,k});
                vm->tracker.known[h] = vm->tracker.known[l];
                if(vm->tracker.known[l]) {
                    Cast<u64>(vm->tracker.val.gpr[h].mem) = ~Cast<u64>(vm->tracker.val.gpr[l].mem);
                }

                ASSERT(h == l);
                WriteOP(mem, &k, "not ", sizeof("not ")-1);
                WriteRegister(mem, &k, h, 3);
                WriteOP(mem, &k, "\n", sizeof("\n")-1);
                break;
            }
        case OP_XOR:
            {
                RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                RegisterName l = (RegisterName)GetLow4bits(program[i]);

                vm->tracker.history[h].PushBack({i-1,k});
                vm->tracker.known[h] &= vm->tracker.known[l];
                if(vm->tracker.known[l] && (l != h)) {
                    Cast<u64>(vm->tracker.val.gpr[h].mem) ^= Cast<u64>(vm->tracker.val.gpr[l].mem);
                    DeleteInstruction(mem,vm->tracker.history[l].Back().value,'\n');

                    u32 val = Cast<u64>(vm->tracker.val.gpr[l].mem);
                    WriteOP(mem, &k, "xor ", sizeof("xor ")-1);
                    WriteRegister(mem, &k, h, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    u32 size = U64ToString(str, val);
                    WriteOP(mem, &k, str, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                else {
                    vm->tracker.known[h] = false;
                    WriteOP(mem, &k, "xor ", sizeof("xor ")-1);
                    WriteRegister(mem, &k, h, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteRegister(mem, &k, l, 3);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                break;
            }
        case OP_RSHIFT:
            {
                RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                RegisterName l = (RegisterName)GetLow4bits(program[i]);

                vm->tracker.history[h].PushBack({i-1,k});
                vm->tracker.known[h] = vm->tracker.known[l];
                if(vm->tracker.known[l]) {
                    Cast<u64>(vm->tracker.val.gpr[h].mem) >>= Cast<u64>(vm->tracker.val.gpr[l].mem);
                }

                WriteOP(mem, &k, "shr ", sizeof("shr ")-1);
                WriteRegister(mem, &k, h, 3);
                WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                WriteRegister(mem, &k, l, 0);
                WriteOP(mem, &k, "\n", sizeof("\n")-1);
                break;                   
            }
        case OP_LSHIFT:
            {
                RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                RegisterName l = (RegisterName)GetLow4bits(program[i]);

                vm->tracker.history[h].PushBack({i-1,k});
                vm->tracker.known[h] = vm->tracker.known[l];
                if(vm->tracker.known[l]) {
                    Cast<u64>(vm->tracker.val.gpr[h].mem) <<= Cast<u64>(vm->tracker.val.gpr[l].mem);
                }


                WriteOP(mem, &k, "shl ", sizeof("shl ")-1);
                WriteRegister(mem, &k, h, 3);
                WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                WriteRegister(mem, &k, l, 0);
                WriteOP(mem, &k, "\n", sizeof("\n")-1);
                break;
            }
        case OP_PUSH:
            {
                RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                RegisterName size = (RegisterName)GetLow4bits(program[i]);

                if(size == 3) {
                    WriteOP(mem, &k, "push ", sizeof("push ")-1);
                    WriteRegister(mem, &k, h, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                else if(size == 0) {
                    WriteOP(mem, &k, "dec rsp\n", sizeof("dec rsp\n")-1);
                    WriteOP(mem, &k, "mov [rsp] , ", sizeof("mov [rsp] , ")-1);
                    WriteRegister(mem, &k, h, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                }
                break;
            }
        case OP_POP:
                {
                    RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName size = (RegisterName)GetLow4bits(program[i]);

                    vm->tracker.known[h] = false;
                    vm->tracker.history->PushBack({i-1,k});

                    if(size == 3) {
                        WriteOP(mem, &k, "pop ", sizeof("pop ")-1);
                        WriteRegister(mem, &k, h, size);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }
                    else if(size == 0) {
                        WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                        WriteRegister(mem, &k, h, size);
                        WriteOP(mem, &k, " , [rsp]", sizeof(" , [rsp]")-1);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                        WriteOP(mem, &k, "inc rsp\n", sizeof("inc rsp\n")-1);
                    }
                    break;
                }
        case OP_ADD:
                {
                    RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName l = (RegisterName)GetLow4bits(program[i]);

                    vm->tracker.history[h].PushBack({i-1,k});
                    vm->tracker.known[h] &= vm->tracker.known[l];
                    if(vm->tracker.known[l]) {
                        Cast<u64>(vm->tracker.val.gpr[h].mem) += Cast<u64>(vm->tracker.val.gpr[l].mem);
                        DeleteInstruction(mem,vm->tracker.history[l].Back().value,'\n');

                        u32 val = Cast<u64>(vm->tracker.val.gpr[l].mem);
                        if(val == 0) {
                            break;
                        }
                        if(val == 1) {
                            WriteOP(mem, &k, "inc ", sizeof("inc ")-1);
                            WriteRegister(mem, &k, h, 3);
                            WriteOP(mem, &k, "\n", sizeof("\n")-1);
                            break;
                        }

                        WriteOP(mem, &k, "add ", sizeof("add ")-1);
                        WriteRegister(mem, &k, h, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        u32 size = U64ToString(str, val);
                        WriteOP(mem, &k, str, size);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }
                    else {

                        WriteOP(mem, &k, "add ", sizeof("add ")-1);
                        WriteRegister(mem, &k, h, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }
                    break;
                }
        case OP_SUB:
                {
                    RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName l = (RegisterName)GetLow4bits(program[i]);

                    vm->tracker.history[h].PushBack({i-1,k});
                    vm->tracker.known[h] &= vm->tracker.known[l];
                    if(vm->tracker.known[l]) {
                        Cast<u64>(vm->tracker.val.gpr[h].mem) -= Cast<u64>(vm->tracker.val.gpr[l].mem);
                        DeleteInstruction(mem,vm->tracker.history[l].Back().value,'\n');

                        u32 val = Cast<u64>(vm->tracker.val.gpr[l].mem);
                        if(val == 1) {
                            WriteOP(mem, &k, "dec ", sizeof("dec ")-1);
                            WriteRegister(mem, &k, h, 3);
                            WriteOP(mem, &k, "\n", sizeof("\n")-1);
                            break;
                        }

                        WriteOP(mem, &k, "sub ", sizeof("sub ")-1);
                        WriteRegister(mem, &k, h, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        u32 size = U64ToString(str, val);
                        WriteOP(mem, &k, str, size);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }
                    else {

                        WriteOP(mem, &k, "sub ", sizeof("sub ")-1);
                        WriteRegister(mem, &k, h, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }
                    break;
                }
        case OP_MUL:
                {                   
                    RegisterName size = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName h = (RegisterName)GetLow4bits(program[i]);
                    RegisterName l = (RegisterName)(program[++i]);

                    vm->tracker.history[h].PushBack({i-1,k});
                    vm->tracker.known[h] = vm->tracker.known[h] & vm->tracker.known[l];

                    bool hKnown = vm->tracker.known[h];
                    bool lKnown = vm->tracker.known[l];
                    u64 hV = Cast<u64>(vm->tracker.val.gpr[h].mem);
                    u64 lV = Cast<u64>(vm->tracker.val.gpr[l].mem);

                    if(hKnown && lKnown) {
                        Cast<u64>(vm->tracker.val.gpr[h].mem) *= Cast<u64>(vm->tracker.val.gpr[l].mem);
                    }

                    if(lKnown) {
                        if(lV == 0) {
                            vm->tracker.known[h] = true;
                            Cast<u64>(vm->tracker.val.gpr[h].mem) = 0;
                            DeleteInstruction(mem, vm->tracker.history[h].Nth(1).value, '\n');
                            DeleteInstruction(mem, vm->tracker.history[l].Back().value, '\n');
                            WriteOP(mem, &k, "xor ");
                            WriteRegister(mem, &k, h, 3);
                            WriteOP(mem, &k, " , ");
                            WriteRegister(mem, &k, h, 3);
                            WriteOP(mem, &k, "\n");
                            break;
                        }
                        else if((lV & (8 - lV)) == 0 && lV != 0) {
                            WriteOP(mem, &k, "shl ", sizeof("shl ")-1);
                            WriteRegister(mem, &k, h, 3);
                            WriteOP(mem, &k, " , ");
                            u32 strSize = U64ToString(str, lV);
                            WriteOP(mem, &k, str, strSize);
                            WriteOP(mem, &k, "\n");
                            break;
                        }
                    }

                    WriteMulDivInst(mem, &k, h,l, size, "mul");
                    break;
                }
        case OP_DIV:
                {
                    RegisterName size = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName h = (RegisterName)GetLow4bits(program[i]);
                    RegisterName l = (RegisterName)(program[++i]);

                    vm->tracker.history[h].PushBack({i-1,k});
                    vm->tracker.known[h] = vm->tracker.known[l];
                    if(vm->tracker.known[l]) {
                        Cast<u64>(vm->tracker.val.gpr[h].mem) /= Cast<u64>(vm->tracker.val.gpr[l].mem);
                    }

                    WriteMulDivInst(mem, &k, h,l, size, "div");
                    break;
                }
        case OP_IMUL:
                {
                    RegisterName size = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName h = (RegisterName)GetLow4bits(program[i]);
                    RegisterName l = (RegisterName)(program[++i]);

                    vm->tracker.history[h].PushBack({i-1,k});
                    vm->tracker.known[h]  =  vm->tracker.known[h] & vm->tracker.known[l];
                    if(vm->tracker.known[l] & vm->tracker.known[h]) {
                        Cast<u64>(vm->tracker.val.gpr[h].mem) *= Cast<u64>(vm->tracker.val.gpr[l].mem);
                    }

                    WriteMulDivInst(mem, &k, h,l, size, "imul");
                    break;
                }
        case OP_IDIV:
                {
                    RegisterName size = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName h = (RegisterName)GetLow4bits(program[i]);
                    RegisterName l = (RegisterName)(program[++i]);

                    vm->tracker.history[h].PushBack({i-1,k});
                    vm->tracker.known[h] = vm->tracker.known[l];
                    if(vm->tracker.known[l]) {
                        Cast<i64>(vm->tracker.val.gpr[h].mem) /= Cast<i64>(vm->tracker.val.gpr[l].mem);
                    }
                    WriteMulDivInst(mem, &k, h,l, size, "idiv");
                    break;
                }
        case OP_ADD_S:
                {
                    
                    break;
                }
        case OP_SUB_S:
                {
                    
                    break;
                }
        case OP_MUL_S:
                {
                    
                    break;
                }
        case OP_DIV_S:
                {
                    
                    break;
                }
        case OP_ADD_D:
                {
                    
                    break;
                }
        case OP_SUB_D:
                {
                    
                    break;
                }
        case OP_MUL_D:
                {
                    
                    break;
                }
        case OP_DIV_D:
                {
                    
                    break;
                }
        case OP_CJMP:
                {
                    RegisterName h = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName l = (RegisterName)GetLow4bits(program[i]);

                    ASSERT(vm->tracker.known[l]);
                    DeleteInstruction(mem, vm->tracker.history[l].Back().value, '\n');
                    u64 jmpOffset = Cast<u64>(vm->tracker.val.gpr[l].mem);
                    u32 label = labelCount;
                    HashNode<u32,u32> n;
                    n.key = jmpOffset-320;
                    n.value = label;

                    bool exist = false;
                    for(u32 k = 0; k < vm->tracker.labels.size; k++) {
                        if(vm->tracker.labels[k].key == n.key) {
                            exist = true;
                            label = vm->tracker.labels[k].value;
                            break;
                        }
                    }
                    if(!exist) {
                        n.value = labelCount++;
                        vm->tracker.labels.PushBack(n);
                    }

                    MemSet(vm->tracker.val.gpr, 0, sizeof(vm->tracker.val));
                    MemSet(vm->tracker.known, false, sizeof(vm->tracker.known));

                    WriteOP(mem, &k, "test ", sizeof("test ")-1);
                    WriteRegister(mem, &k, h, 0);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteRegister(mem, &k, h, 0);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);

                    WriteOP(mem, &k, "jnz ", sizeof("jnz ")-1);

                    u32 size = sprintf(str, "label%d", label);
                    str[size] = '\n';
                    WriteOP(mem, &k, str, size + 1);
                    break;
                }
        case OP_JMP:
                {
                    RegisterName l = (RegisterName)(program[++i]);

                    ASSERT(vm->tracker.known[l]);
                    DeleteInstruction(mem, vm->tracker.history[l].Back().value, '\n');
                    u64 jmpOffset = Cast<u64>(vm->tracker.val.gpr[l].mem);
                    u32 label = labelCount;
                    HashNode<u32,u32> n;
                    n.key = jmpOffset-320;
                    n.value = label;

                    bool exist = false;
                    for(u32 k = 0; k < vm->tracker.labels.size; k++) {
                        if(vm->tracker.labels[k].key == n.key) {
                            exist = true;
                            label = vm->tracker.labels[k].value;
                            break;
                        }
                    }
                    if(!exist) {
                        n.value = labelCount++;
                        vm->tracker.labels.PushBack(n);
                    }
                    MemSet(vm->tracker.val.gpr, 0, sizeof(vm->tracker.val));
                    MemSet(vm->tracker.known, false, sizeof(vm->tracker.known));

                    WriteOP(mem, &k, "jmp ");
                    u32 size = sprintf(str, "label%d", label);
                    str[size] = '\n';
                    WriteOP(mem, &k, str, size + 1);
                    break;
                }
        case OP_JMP_IM:
                {
                    i++;
                    u64 jmpOffset = *((u64*)(program + i));
                    i += 7;

                    if(jmpOffset-320 == i+1) {
                        break;
                    }

                    u32 in = vm->tracker.opOffsets.Find(jmpOffset-320);

                    u32 label = labelCount;
                    HashNode<u32,u32> n;
                    n.key = jmpOffset-320;
                    n.value = label;

                    bool exist = false;
                    for(u32 k = 0; k < vm->tracker.labels.size; k++) {
                        if(vm->tracker.labels[k].key == n.key) {
                            exist = true;
                            label = vm->tracker.labels[k].value;
                            break;
                        }
                    }
                    if(!exist) {
                        n.value = labelCount++;
                        vm->tracker.labels.PushBack(n);
                    }

                    MemSet(vm->tracker.val.gpr, 0, sizeof(vm->tracker.val));
                    MemSet(vm->tracker.known, false, sizeof(vm->tracker.known));

                    u32 size = sprintf(str, "label%d", label);
                    str[size] = '\n';

                    WriteOP(mem, &k, "jmp ", sizeof("jmp ")-1);
                    WriteOP(mem, &k, str, size+1);
                    break;
                }
        case OP_LOAD:
                {
                    RegisterName dst = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName add = (RegisterName)GetLow4bits(program[i]);
                    RegisterName size = (RegisterName)program[++i];

                    u32 op = vm->tracker.history[add].Back().key;
                    if(program[op] == OP_ADD) {
                        RegisterName addDst = (RegisterName)GetHigh4bits(program[op+1]);
                        RegisterName addSrc = (RegisterName)GetLow4bits(program[op+1]);


                        u32 movop = vm->tracker.history[addSrc].Nth(0).key;
                        Opcode opc = (Opcode)program[movop];
                        if(program[movop] >= OP_MOV_IM8 && program[movop] <= OP_MOV_IM64) {
                            u64 s;
                            switch (program[movop]) {
                            case OP_MOV_IM8: s = *((u8*)(program + movop + 2));break;
                            case OP_MOV_IM16: s = *((u16*)(program + movop + 2));break;
                            case OP_MOV_IM32: s = *((u32*)(program + movop + 2));break;
                            case OP_MOV_IM64: s = *((u64*)(program + movop + 2));break;
                            }
                            
                            WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                            WriteRegister(mem, &k, dst, size);
                            WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                            if(size == 0) WriteOP(mem, &k, "BYTE ");
                            WriteOP(mem, &k, "[", sizeof("[")-1);
                            WriteRegister(mem, &k, addDst, R3);

                            if(s != 0) {
                                DeleteInstruction(mem, vm->tracker.history[add].Back().value, '\n');
                                WriteOP(mem, &k, "+", sizeof("+")-1);
                                u32 size = U64ToString(str, s);
                                WriteOP(mem, &k, str, size);
                                WriteOP(mem, &k, "]\n", sizeof("]\n")-1);
                            }
                            WriteOP(mem, &k, "]\n", sizeof("]\n")-1);
                            break;
                        }
                    }
                    else if(program[op] == OP_SUB) {
                        RegisterName addDst = (RegisterName)GetHigh4bits(program[op+1]);
                        RegisterName addSrc = (RegisterName)GetLow4bits(program[op+1]);

                        u32 movop = vm->tracker.history[addSrc].Back().key;
                        if(program[movop] >= OP_MOV_IM8 && program[movop] <= OP_MOV_IM64) {
                            u64 s;
                            switch (program[movop]) {
                            case OP_MOV_IM8: s = *((u8*)(program + movop + 1));break;
                            case OP_MOV_IM16: s = *((u16*)(program + movop + 1));break;
                            case OP_MOV_IM32: s = *((u32*)(program + movop + 1));break;
                            case OP_MOV_IM64: s = *((u64*)(program + movop + 1));break;
                            }
                            
                            DeleteInstruction(mem, vm->tracker.history[add].Back().value, '\n');
                            WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                            WriteRegister(mem, &k, dst, size);
                            WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                            if(size == 0) WriteOP(mem, &k, "BYTE ");
                            WriteOP(mem, &k, "[", sizeof("[")-1);
                            WriteRegister(mem, &k, addDst, R3);
                            WriteOP(mem, &k, "-", sizeof("-")-1);
                            u32 size = U64ToString(str, s);
                            WriteOP(mem, &k, str, size);
                            WriteOP(mem, &k, "]\n", sizeof("]\n")-1);
                            break;
                        }
                    }

                    vm->tracker.history[dst].PushBack({i-1,k});
                    vm->tracker.known[dst] = false;
                    
                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    WriteRegister(mem, &k, dst, size);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    if(size == 0) WriteOP(mem, &k, "BYTE ");
                    WriteOP(mem, &k, "[", sizeof("[")-1);
                    WriteRegister(mem, &k, add, R3);
                    WriteOP(mem, &k, "]\n", sizeof("]\n")-1);
                    break;
                }
        case OP_STORE:
                {
                    RegisterName add = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName val = (RegisterName)GetLow4bits(program[i]);
                    RegisterName size = (RegisterName)program[++i];


                    if(vm->tracker.known[val]) {
                        u32 opI = vm->tracker.history[val].Back().key;
                        Opcode op = (Opcode)program[opI];
                        if(op >= OP_MOV_IM8 && op <= OP_MOV_IM64) {
                            DeleteInstruction(mem, vm->tracker.history[val].Back().value, '\n');

                            WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                            if(size == 3) WriteOP(mem, &k, "qword "); else ASSERT(false);
                            WriteOP(mem, &k, "[");
                            WriteRegister(mem, &k, add, R3);
                            WriteOP(mem, &k, "] , ", sizeof("] , ")-1);
                            u64 im = Cast<u64>(vm->tracker.val.gpr[val].mem);
                            u32 size = U64ToString(str, im);
                            WriteOP(mem, &k, str, size);
                            WriteOP(mem, &k, "\n", sizeof("\n")-1);
                            break;
                        }
                    }

                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    if(size == 0) WriteOP(mem, &k, "BYTE ");
                    WriteOP(mem, &k, "[");
                    WriteRegister(mem, &k, add, R3);
                    WriteOP(mem, &k, "] , ", sizeof("] , ")-1);
                    WriteRegister(mem, &k, val, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    
                    break;
                }
        case OP_INTERUPT:
                {

                    u32 s = 0;
                    if(vm->tracker.known[R0]) {
                        u64 val = Cast<u64>(vm->tracker.val.gpr[R0].mem);

                        switch(val) {
                        case 0:
                            {// abort

                            break;
                            }
                        case 1: // print string
                            WriteOP(mem, &k, "mov rax , ");
                            s = U64ToString(str, (u64)PrintString);
                            WriteOP(mem, &k, str, s);
                            WriteOP(mem, &k, "\n");
                            WriteOP(mem, &k, "call rax\n");
                            break;
                        case 2: // print u64
                            WriteOP(mem, &k, "mov rax , ");
                            s = U64ToString(str, (u64)PrintU64);
                            WriteOP(mem, &k, str, s);
                            WriteOP(mem, &k, "\n");
                            WriteOP(mem, &k, "call rax\n");
                            break;
                        case 3: // print i64
                            WriteOP(mem, &k, "mov rax , ");
                            s = U64ToString(str, (u64)PrintI64);
                            WriteOP(mem, &k, str, s);
                            WriteOP(mem, &k, "\n");
                            WriteOP(mem, &k, "call rax\n");
                            break;
                        case 4: // print f64
                            WriteOP(mem, &k, "mov rax , ");
                            s = U64ToString(str, (u64)PrintF64);
                            WriteOP(mem, &k, str, s);
                            WriteOP(mem, &k, "\n");
                            WriteOP(mem, &k, "call rax\n");
                            break;
                        case 5: // print f32
                            WriteOP(mem, &k, "mov rax , ");
                            s = U64ToString(str, (u64)PrintF32);
                            WriteOP(mem, &k, str, s);
                            WriteOP(mem, &k, "\n");
                            WriteOP(mem, &k, "call rax\n");
                            break;
                        }
                    }
                    else{
                        std::cout << "syscall unkown" << std::endl;
                        ASSERT(false);
                    }
                    break;
                }
        case OP_CSFS:
                {
                    
                    break;
                }
        case OP_CSFU:
                {
                    
                    break;
                }
        case OP_CSSF:
                {
                    
                    break;
                }
        case OP_CUSF:
                {
                    
                    break;
                }
        case OP_CDFS:
                {
                    
                    break;
                }
        case OP_CDFU:
                {
                    
                    break;
                }
        case OP_CSDF:
                {
                    
                    break;
                }
        case OP_CUDF:
                {
                    
                    break;
                }
        case OP_CSFDF:
                {
                    
                    break;
                }
        case OP_CDFSF:
                {
                    
                    break;
                }
        case OP_CMP_I_LT:
                {
                    RegisterName l = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName r = (RegisterName)GetLow4bits(program[i]);
                    RegisterName dst = (RegisterName)program[++i];


                    if(vm->tracker.known[r]) {
                        auto val = Cast<i64>(vm->tracker.val.gpr[r].mem);
                        DeleteInstruction(mem,vm->tracker.history[r].Back().value,'\n');

                        WriteOP(mem, &k, "cmp ", sizeof("cmp ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        u32 size = U64ToString(str, val);
                        WriteOP(mem, &k, str, size);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }
                    else {
                        WriteOP(mem, &k, "cmp ", sizeof("cmp ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        WriteRegister(mem, &k, r, 3);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }

                    WriteOP(mem, &k, "setl ", sizeof("setl ")-1);
                    WriteRegister(mem, &k, dst, 0);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    vm->tracker.known[dst] = false;
                    break;
                }
        case OP_CMP_I_GT:
                {
                    RegisterName l = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName r = (RegisterName)GetLow4bits(program[i]);
                    RegisterName dst = (RegisterName)program[++i];

                    if(vm->tracker.known[r]) {
                        auto val = Cast<i64>(vm->tracker.val.gpr[r].mem);
                        DeleteInstruction(mem,vm->tracker.history[r].Back().value,'\n');

                        WriteOP(mem, &k, "cmp ", sizeof("cmp ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        u32 size = U64ToString(str, val);
                        WriteOP(mem, &k, str, size);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }
                    else {
                        WriteOP(mem, &k, "cmp ", sizeof("cmp ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        WriteRegister(mem, &k, r, 3);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }

                    WriteOP(mem, &k, "setg ", sizeof("setg ")-1);
                    WriteRegister(mem, &k, dst, 0);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    
                    vm->tracker.known[dst] = false;
                    break;
                }
        case OP_CMP_I_E:
                {
                    RegisterName l = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName r = (RegisterName)GetLow4bits(program[i]);
                    RegisterName dst = (RegisterName)program[++i];

                    if(vm->tracker.known[r]) {
                        auto val = Cast<i64>(vm->tracker.val.gpr[r].mem);
                        DeleteInstruction(mem,vm->tracker.history[r].Back().value,'\n');

                        WriteOP(mem, &k, "cmp ", sizeof("cmp ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        u32 size = U64ToString(str, val);
                        WriteOP(mem, &k, str, size);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }
                    else {

                        WriteOP(mem, &k, "cmp ", sizeof("cmp ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        WriteRegister(mem, &k, r, 3);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }

                    WriteOP(mem, &k, "sete ", sizeof("sete ")-1);
                    WriteRegister(mem, &k, dst, 0);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    vm->tracker.known[dst] = false;
                    break;
                }
        case OP_CMP_U_LT:
                {
                    RegisterName l = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName r = (RegisterName)GetLow4bits(program[i]);
                    RegisterName dst = (RegisterName)program[++i];

                    if(vm->tracker.known[r]) {
                        auto val = Cast<i64>(vm->tracker.val.gpr[r].mem);
                        DeleteInstruction(mem,vm->tracker.history[r].Back().value,'\n');

                        WriteOP(mem, &k, "cmp ", sizeof("cmp ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        u32 size = U64ToString(str, val);
                        WriteOP(mem, &k, str, size);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }
                    else {
                        WriteOP(mem, &k, "cmp ", sizeof("cmp ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        WriteRegister(mem, &k, r, 3);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }

                    WriteOP(mem, &k, "setb ", sizeof("setb ")-1);
                    WriteRegister(mem, &k, dst, 0);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    vm->tracker.known[dst] = false;
                    break;
                }
        case OP_CMP_U_GT:
                {
                    RegisterName l = (RegisterName)GetHigh4bits(program[++i]);
                    RegisterName r = (RegisterName)GetLow4bits(program[i]);
                    RegisterName dst = (RegisterName)program[++i];

                    if(vm->tracker.known[r]) {
                        auto val = Cast<i64>(vm->tracker.val.gpr[r].mem);
                        DeleteInstruction(mem,vm->tracker.history[r].Back().value,'\n');

                        WriteOP(mem, &k, "cmp ", sizeof("cmp ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        u32 size = U64ToString(str, val);
                        WriteOP(mem, &k, str, size);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }
                    else {
                        WriteOP(mem, &k, "cmp ", sizeof("cmp ")-1);
                        WriteRegister(mem, &k, l, 3);
                        WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                        WriteRegister(mem, &k, r, 3);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }

                    WriteOP(mem, &k, "seta ", sizeof("seta ")-1);
                    WriteRegister(mem, &k, dst, 0);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    vm->tracker.known[dst] = false;

                    break;
                }
        case OP_CMP_SF_LT:
                {
                    
                    break;
                }
        case OP_CMP_SF_GT:
                {
                    
                    break;
                }
        case OP_CMP_SF_E:
                {
                    
                    break;
                }
        case OP_CMP_DF_LT:
                {
                    
                    break;
                }
        case OP_CMP_DF_GT:
                {
                    
                    break;
                }
        case OP_CMP_DF_E:
                {
                    
                    break;
                }
        case OP_LOAD_REL_RSP:
                {
                    
                    break;
                }
        case OP_STORE_REL_RSP:
                {
                    
                    break;
                }
        case OP_LOAD_REL_RBP:
                {
                    
                    break;
                }
        case OP_STORE_REL_RBP:
                {
                    
                    break;
                }
        case OP_LEA:
                {
                    i++;
                    u16 h = *((u16*)(program+i));
                    u32 dst = (h >> 12) & 0xF;
                    u32 base = (h >> 8) & 0xF;
                    vm->tracker.history[dst].PushBack({i-1,k});

                    u32 offset = (h >> 4) & 0xF;
                    u32 shift = (h) & 0xF;
                    i16 im = *((i16*)(program + i + 2));
                    byte extra = program[i+4];
                    i += 4;

                    vm->tracker.known[dst] = false;

                    WriteOP(mem, &k, "lea ");
                    WriteRegister(mem, &k, (RegisterName)dst, 3);
                    WriteOP(mem, &k, " , [");

                    if(extra & 1) {
                        WriteRegister(mem, &k, (RegisterName)base, 3);
                    }
                    if(extra & 2) {
                        if(extra & 1) WriteOP(mem, &k, "+");
                        WriteRegister(mem, &k, (RegisterName)offset, 3);
                        WriteOP(mem, &k, "*");
                        U64ToString(str, shift << 3);
                        WriteOP(mem, &k, str, 1);
                    }
                    if(im != 0) {
                        u32 size = U64ToString(str, abs(im));
                        WriteOP(mem, &k, im > 0 ? "+" : "-");
                        WriteOP(mem, &k, str, size);
                    }
                    
                    WriteOP(mem, &k, "]\n");
                    break;
                }
        case OP_GET_PTR:
                {
                    RegisterName r = (RegisterName)(program[++i]);
                    vm->tracker.history[r].PushBack({i-1,k});
                    vm->tracker.known[r] = true;
                    i++;
                    u32 target = *((u32*)(program + i));
                    i += 3;
                    Cast<u64>(vm->tracker.val.gpr[r].mem) = target;

                    u32 label = labelCount;
                    HashNode<u32,u32> n;
                    n.key = target-320;
                    n.value = label;

                    bool exist = false;
                    for(u32 k = 0; k < vm->tracker.labels.size; k++) {
                        if(vm->tracker.labels[k].key == n.key) {
                            exist = true;
                            label = vm->tracker.labels[k].value;
                            break;
                        }
                    }
                    if(!exist) {
                        n.value = labelCount++;
                        vm->tracker.labels.PushBack(n);
                    }
                    u32 size = sprintf(str, "label%d + ", label);

                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    WriteRegister(mem, &k, r, 3);
                    WriteOP(mem, &k, " , ", sizeof(" , ")-1);
                    WriteOP(mem, &k, str, size);
                    size = U64ToString(str, (u64)compiled);
                    WriteOP(mem, &k, str, size);
                    WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    break;
                }
        case OP_GET_STACK_PTR:
                {
                    vm->tracker.history->PushBack({i-1,k});
                    RegisterName dst = (RegisterName)program[++i];
                    vm->tracker.known[dst] = false;
                    
                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    WriteRegister(mem, &k, dst, 3);
                    WriteOP(mem, &k, " , rsp\n", sizeof(" , rsp\n")-1);
                    break;
                }
        case OP_GET_BASE_PTR:
                {
                    vm->tracker.history->PushBack({i-1,k});
                    RegisterName dst = (RegisterName)program[++i];
                    vm->tracker.known[dst] = false;

                    WriteOP(mem, &k, "mov ", sizeof("mov ")-1);
                    WriteRegister(mem, &k, dst, 3);
                    WriteOP(mem, &k, " , rbp\n", sizeof(" , rbp\n")-1);
                    break;
                }
        case OP_CALL_R:
                {
                    RegisterName target = (RegisterName)program[++i];

                    if(vm->tracker.known[target]) {

                        DeleteInstruction(mem, vm->tracker.history[target].Back().value, '\n');
                        u32 label = labelCount;
                        HashNode<u32,u32> n;
                        n.key = Cast<u64>(vm->tracker.val.gpr[target].mem)-320;
                        n.value = label;

                        bool exist = false;
                        for(u32 k = 0; k < vm->tracker.labels.size; k++) {
                            if(vm->tracker.labels[k].key == n.key) {
                                exist = true;
                                label = vm->tracker.labels[k].value;
                                break;
                            }
                        }
                        if(!exist) {
                            n.value = labelCount++;
                            vm->tracker.labels.PushBack(n);
                        }

                        MemSet(vm->tracker.val.gpr, 0, sizeof(vm->tracker.val));
                        MemSet(vm->tracker.known, false, sizeof(vm->tracker.known));

                        u32 size = sprintf(str, "label%d", label);
                        str[size] = '\n';
                        WriteOP(mem, &k, "call ", sizeof("call ")-1);
                        WriteOP(mem, &k, str, size+1);
                    }
                    else {

                        WriteOP(mem, &k, "call ", sizeof("call ")-1);
                        WriteRegister(mem, &k, target, 3);
                        WriteOP(mem, &k, "\n", sizeof("\n")-1);
                    }

                    break;
                }
        case OP_CALL:
                {
                    MemSet(vm->tracker.val.gpr, 0, sizeof(vm->tracker.val));
                    MemSet(vm->tracker.known, false, sizeof(vm->tracker.known));
                    u32 target = *((u32*)(program + i + 1));
                    i += 4;

                    u32 label = labelCount;
                    HashNode<u32,u32> n;
                    n.key = target-320;
                    n.value = label;

                    bool exist = false;
                    for(u32 k = 0; k < vm->tracker.labels.size; k++) {
                        if(vm->tracker.labels[k].key == n.key) {
                            exist = true;
                            label = vm->tracker.labels[k].value;
                            break;
                        }
                    }
                    if(!exist) {
                        n.value = labelCount++;
                        vm->tracker.labels.PushBack(n);
                    }

                    MemSet(vm->tracker.val.gpr, 0, sizeof(vm->tracker.val));
                    MemSet(vm->tracker.known, false, sizeof(vm->tracker.known));

                    u32 size = sprintf(str, "label%d", label);
                    str[size] = '\n';
                    WriteOP(mem, &k, "call ", sizeof("call ")-1);
                    WriteOP(mem, &k, str, size+1);
                    break;
                }
        case OP_RET:
                {
                    MemSet(vm->tracker.val.gpr, 0, sizeof(vm->tracker.val));
                    MemSet(vm->tracker.known, false, sizeof(vm->tracker.known));
                    WriteOP(mem, &k, "ret\n", sizeof("ret\n")-1);
                    break;
                }
        case OP_EXIT:
                {
                    MemSet(vm->tracker.val.gpr, 0, sizeof(vm->tracker.val));
                    MemSet(vm->tracker.known, false, sizeof(vm->tracker.known));
                    WriteOP(mem, &k, "\n");

                    WriteOP(mem, &k, "pop rax\n");
                    WriteOP(mem, &k, "mov rsp, rax\n");

                    WriteOP(mem, &k, "pop rbp\n");
                    WriteOP(mem, &k, "pop rsp\n");
                    WriteOP(mem, &k, "pop r15\n");
                    WriteOP(mem, &k, "pop r14\n");
                    WriteOP(mem, &k, "pop r13\n");
                    WriteOP(mem, &k, "pop r12\n");
                    WriteOP(mem, &k, "pop r11\n");
                    WriteOP(mem, &k, "pop r10\n");
                    WriteOP(mem, &k, "pop r9\n");
                    WriteOP(mem, &k, "pop r8\n");
                    WriteOP(mem, &k, "pop rdi\n");
                    WriteOP(mem, &k, "pop rsi\n");
                    WriteOP(mem, &k, "pop rbx\n");
                    WriteOP(mem, &k, "pop rdx\n");
                    WriteOP(mem, &k, "pop rcx\n");
                    WriteOP(mem, &k, "pop rax\n");
                    WriteOP(mem, &k, "\n");


                    WriteOP(mem, &k, "mov rax , ");
                    u32 size = U64ToString(str, VM_PROGRAM_EXIT);
                    WriteOP(mem, &k, str, size);
                    WriteOP(mem, &k, "\n");
                    WriteOP(mem, &k, "ret\n", sizeof("ret\n")-1);
                    break;
                }
        }
    }

    for(u32 i = 0; i < vm->tracker.labels.size; i++) {

        u32 min = ~u32(0);
        u32 minIndex;
        for(u32 k = i; k < vm->tracker.labels.size; k++) {
            if(vm->tracker.labels[k].key < min) {
                min = vm->tracker.labels[k].key;
                minIndex = k;
            }
        }

        auto tmp = vm->tracker.labels[i];
        vm->tracker.labels[i] = vm->tracker.labels[minIndex];
        vm->tracker.labels[minIndex] = tmp;
    }

    u32 keyOffset = 0;
    for(u32 i = 0; i < vm->tracker.labels.size; i++) {
        u32 key = vm->tracker.labels[i].key;
        u32 label = vm->tracker.labels[i].value;
        u32 size = sprintf(str, "label%d:\n", label);

        u32 strOffset = vm->tracker.opOffsets.array[vm->tracker.opOffsets.Find(key)].value;

        InsertStr((byte*)scratch, mem, &k, strOffset + keyOffset, str, size);
        keyOffset += size;
    }

    WriteOP(mem, &k, "\0", 1);

    u32 asmSize = StrLen((char*)mem);
    FILE* out = fopen("jit_asm.asm" ,"w");
    fwrite(mem , 1 , asmSize , out);
    fclose(out);

    system("nasm jit_asm.asm -f bin");
    u32 code_size;
    char* jited_code = ReadFileTerminated("jit_asm", &code_size);
    MemCpy(compiled, jited_code, code_size);

    free(scratch);

    VmStatus(*jit_code)() = (VmStatus(*)())compiled;
    return jit_code;
}