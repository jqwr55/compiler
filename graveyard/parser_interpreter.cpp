#define INTERPRETER

#include <chrono>
#include <iostream>
#include <sys/mman.h> 

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

#ifdef __GNUC__
    #define _FORCE_INLINE __attribute__((always_inline))
    #define _TRAP __builtin_trap()
#else
    #ifdef _MSC_VER
        #define _FORCE_INLINE __builtin_trap();
    #endif
#endif

#define LOGASSERT(x , y ) if( !(x) ) {std::cout << #x << " " << y << " " << __FILE__ << " " << __LINE__ << std::endl; _TRAP; }
#define ASSERT(x) if( !(x) ) {std::cout << #x << " triggered builtin trap in: " << __FILE__ << " " << __LINE__ << std::endl; _TRAP; }
//#define LOG(x) x; std::cout << #x << " " << __FILE__ << " " << __LINE__ << std::endl;
#define LOG(x) x;

template<typename T> T& Cast(void* mem) {
    return *((T*)mem);
}
template<typename T> T Min(T t0 , T t1) {
    return t0 > t1 ? t1 : t0;
}
template<typename T> T Max(T t0 , T t1) {
    return t0 > t1 ? t0 : t1;
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

u32 StringHash(const char* str , u32 c) {
    u32 hash = 7;
    for(u32 i = 0; i < c ; i++) {
        hash = hash * 31 + str[i];
    }
    return hash;
}

struct MemoryBlockHeader {
    MemoryBlockHeader* left;
    MemoryBlockHeader* right;
    u32 size;
    u16 allocation_name;
    bool is_free;
};
byte* base;

MemoryBlockHeader* search_free_block(u32 size) {

    MemoryBlockHeader* block = (MemoryBlockHeader*)(base);
    while(block) {

        if(block->left) {
            ASSERT(block->left->right == block);
        }
        if(block->right) {
            ASSERT(block->right->left == block);
        }
        
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

u32 global_allocation_name = 0;
u32 check_live_mem(void* block);
void* my_malloc(u32 size) {

    if(!size) return nullptr;

    MemoryBlockHeader* free_block = search_free_block(size);
    if(free_block->right) {
        ASSERT(free_block->right->left == free_block);
        ASSERT(free_block->right->size != 0);
    }
    if(free_block->left) {
        ASSERT(free_block->left->right == free_block);
        ASSERT(free_block->left->size != 0);
    }

    ASSERT(free_block->is_free);
    if(free_block) {
        free_block->is_free = false;
        if(free_block->size - size > sizeof(MemoryBlockHeader)) {

            byte* free_block_end = ((byte*)(free_block + 1)) + size;
            MemoryBlockHeader* new_free_block = (MemoryBlockHeader*)free_block_end;

            new_free_block->is_free = true;
            new_free_block->size = (free_block->size - size) - sizeof(MemoryBlockHeader);
            new_free_block->right = free_block->right;
            new_free_block->left = free_block;
            if(free_block->right) {
                free_block->right->left = new_free_block;
            }

            free_block->right = new_free_block;
            free_block->size = size;
        }

        free_block->allocation_name = global_allocation_name++;
        return free_block + 1;
    }


    std::cout << "out of memory" << std::endl;
    return nullptr;
}

u32 check_live_mem(void* block) {

    if(!block) return ~u32(0);
    MemoryBlockHeader* header = ((MemoryBlockHeader*)block) - 1;
    ASSERT(!header->is_free);

    MemoryBlockHeader* next_block = header->right;
    MemoryBlockHeader* previous_block = header->left;

    if(next_block) {
        ASSERT(next_block->left == header);
        ASSERT(next_block->size != 0);
    }
    if(previous_block) {
        ASSERT(previous_block->right == header);
        ASSERT(previous_block->size != 0);
    }

    return header->size;
}
void my_free(void* block) {
    if(!block) return;

    MemoryBlockHeader* header = ((MemoryBlockHeader*)block) - 1;
    ASSERT(!header->is_free);
    header->is_free = true;

    MemoryBlockHeader* next_block = header->right;
    MemoryBlockHeader* previous_block = header->left;

    if(next_block) {
        ASSERT(next_block->left == header);
        ASSERT(next_block->size != 0);
    }
    if(previous_block) {
        ASSERT(previous_block->right == header);
        ASSERT(previous_block->size != 0);
    }

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


bool check_memory_integrity(void* mem) {

    if(!mem) return true;
    u32 size = *((u32*)((byte*)mem - 64));
    byte* back_guard = ((byte*)mem) - 60;
    byte* front_guard = ((byte*)mem) + size;

    ASSERT( (check_live_mem(back_guard-4) - (size + 128)) < sizeof(MemoryBlockHeader));

    bool corrupt = false;
    for(u32 i = 0; i < 60; i++) {
        corrupt |= back_guard[i] != 255;
    }
    for(u32 i = 0; i < 64; i++) {
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
void check_all_memory(void* check) {
    if(check != nullptr) {
        ASSERT(!check_memory_integrity(check));
    }

    bool found = false;
    const MemoryBlockHeader* block = (MemoryBlockHeader*)(base);
    while(block) {
        byte* mem = (byte*)block;
        if(!block->is_free) {
            check_memory_integrity(mem+64+sizeof(MemoryBlockHeader));
        }
        if(check != nullptr && check == (mem+64+sizeof(MemoryBlockHeader))) {
            found = true;
        }
        block = block->right;
    }
    
    if(check != nullptr) {
        ASSERT(found);
    }
}
void* my_malloc_debug(u32 size) {

    byte* mem = (byte*)my_malloc(size+128);
    *((u32*)mem) = size;
    MemSet(mem + sizeof(u32), 255, 60+64+size);
    check_all_memory(mem + 64);
    ASSERT( (check_live_mem(mem) - (size + 128)) < sizeof(MemoryBlockHeader));
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
    check_all_memory(mem);
    ASSERT( (check_live_mem(back_guard-4) - (size + 128)) < sizeof(MemoryBlockHeader));
    my_free(back_guard-4);
}
template<typename K , typename V> struct HashNode {
    K key;
    V value;
};
template<typename K, typename V, u64 (*HASH_FUNCTION)(void*,K), bool (*EQ_FUNCTION)(void*,K,K), K INVALID_KEY> struct HashTable {
    HashNode<K,V>* array = nullptr;
    void* user;
    u32 cap = 0;
    u32 occupancy = 0;
    static constexpr f32 loadFactor = 0.5;

    void Init(void* user_) {
        user = user_;
        cap = 2;
        array = (HashNode<K,V>*)LOG(my_malloc_debug( sizeof(HashNode<K,V>) * 2));
        array[0].key = INVALID_KEY;
        array[1].key = INVALID_KEY;
        check_memory_integrity(array);
    }
    void CopyInit(HashTable<K,V,HASH_FUNCTION,EQ_FUNCTION,INVALID_KEY>* other) {

        MemCpy(this, other, sizeof(*this));
        array = (HashNode<K,V>*)LOG(my_malloc_debug( sizeof(HashNode<K,V>) * other->cap));
        MemCpy(array, other->array, sizeof(HashNode<K,V>) * other->cap);
        check_memory_integrity(array);
    }

    u32 Find(K key) {
    
        u32 index = HASH_FUNCTION(user, key) & (cap - 1);
        for(;;) {
            
            if(EQ_FUNCTION(user, array[index].key, key)) {
                return index;
            }
            index++;
            if(index == cap) {
                return ~u32(0);
            }
        }
    }

    void Delete(K key) {
        occupancy--;
        array[Find(key)].key = INVALID_KEY;
    }
    void Remove(u32 index) {
        occupancy--;
        array[index].key = INVALID_KEY;
    }

    void Insert(K key , V val) {

        if( cap * loadFactor < (occupancy + 1) ) {
            GrowAndReHash();
        }

        occupancy++;
        u32 index = HASH_FUNCTION(user, key) & (cap - 1);
        for(;;) {
            if(EQ_FUNCTION(user, array[index].key, INVALID_KEY)) {
                array[index].key = key;
                array[index].value = val;
                return;
            }

            index++;
            if(index == cap) {
                GrowAndReHash();
                index = HASH_FUNCTION(user, key) & (cap - 1);
            }
        }
    }

    void GrowAndReHash() {

        u32 newCap = cap * 2;
        Begin:
        check_memory_integrity(array);
        HashNode<K,V>* tmp = (HashNode<K,V>*)LOG(my_malloc_debug(sizeof(HashNode<K,V>) * newCap));
        for(u32 i = 0; i < newCap; i++) {
            tmp[i].key = INVALID_KEY;
        }
        check_memory_integrity(array);
        check_memory_integrity(tmp);

        for(u32 i = 0; i < cap; i++) {

            if(!EQ_FUNCTION(user, array[i].key, INVALID_KEY)) {

                u32 index = HASH_FUNCTION(user, array[i].key) & (newCap - 1);
                for(;;) {
                    if(EQ_FUNCTION(user, tmp[index].key, INVALID_KEY)) {
                        tmp[index].key = array[i].key;
                        tmp[index].value = array[i].value;
                        break;
                    }
                    index++;
                    if(index == newCap) {
                        newCap *= 2;
                        LOG(my_free_debug(tmp));
                        goto Begin;
                    }
                }
            }
        }

        LOG(my_free_debug(array));
        array = tmp;
        cap = newCap;
    }
    void Free() {
        LOG(my_free_debug(array));
        array = nullptr;
    }
};
template<typename T> struct DynamicBuffer {
    T*      mem = nullptr;
    u32     cap = 0;
    u32     size = 0;

    void Init() {
        mem = (T*)LOG(my_malloc_debug(sizeof(T)));
        cap = 1;
        size = 0;
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
        mem = (T*)LOG(my_malloc_debug(capacity * sizeof(T)));
        cap = capacity;
    }
    u32 PushBack(T e) {
        
        if( cap < size + 1) {
            
            check_all_memory(nullptr);
            T* tmp = (T*)LOG(my_malloc_debug( sizeof(T) * cap * 2 ));
            check_all_memory(nullptr);
            ASSERT(cap * 2 >= size + 1);

            MemCpy(tmp, mem, size * sizeof(T));
            LOG(my_free_debug(mem));
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
        return size--;
    }
    u32 Shrink() {
        if(size < cap) {
            T* tmp = (T*)LOG(my_malloc_debug( sizeof(T) * (size + 1) * 2 ));
            for(u32 i = 0; i < size * sizeof(T) ; i++) {
                ((byte*)tmp)[i] = ((byte*)mem)[i];
            }
            LOG(my_free_debug(mem));
            mem = tmp;
            cap = size;
        }
        return size;
    }
    u32 ShrinkNoBranch() {
        T* tmp = (T*)LOG(my_malloc_debug( sizeof(T) * (size + 1) * 2));
        for(u32 i = 0; i < size * sizeof(T) ; i++) {
            ((byte*)tmp)[i] = ((byte*)mem)[i];
        }
        LOG(my_free_debug(mem));
        mem = tmp;
        cap = size;
        return size;
    }

    _FORCE_INLINE T& operator[] (u32 i) {
        return mem[i];
    }
    void Free() {
        LOG(my_free_debug(mem));
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

enum TokenType {
    TOKEN_EOF,
    TOKEN_IDENTIFIER,

    TOKEN_KEYWORD_PRINT,
    TOKEN_KEYWORD_IF,
    TOKEN_KEYWORD_ELSE,
    TOKEN_KEYWORD_FOR,
    TOKEN_KEYWORD_FN,
    TOKEN_KEYWORD_RETURN,
    TOKEN_KEYWORD_MAIN,
    TOKEN_KEYWORD_STRUCT,
    TOKEN_KEYWORD_ARROW,
    TOKEN_KEYWORD_STRING,
    TOKEN_KEYWORD_AUTO,
    TOKEN_KEYWORD_VAR,

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
    TOKEN_LSHIFT_EQUALS,
    TOKEN_RSHIFT_EQUALS,
    TOKEN_TILDE_EQUALS,
    TOKEN_EXCLAMATION_EQUALS,

    TOKEN_AMPERSAND_AMPERSAND,
    TOKEN_VERTICAL_BAR_VERTICAL_BAR,
    TOKEN_EQUALS_EQUALS, 

    TOKEN_VERTICAL_BAR,
    TOKEN_CIRCUMFLEX,
    TOKEN_DOT,
    TOKEN_COMMA,
    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_ASTERISK,
    TOKEN_SLASH,
    TOKEN_AMPERSAND,
    TOKEN_TILDE,
    TOKEN_EXCLAMATION_MARK,
    TOKEN_PLUS,
    TOKEN_PLUS_PLUS,
    TOKEN_MINUS,
    TOKEN_MINUS_MINUS,
    TOKEN_EQUAL_SIGN,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_LSHIFT_LSHIFT,
    TOKEN_RSHIFT_RSHIFT,
    TOKEN_SUB_SCRIPT_OPR,

    TOKEN_CHAR_LITERAL,
    TOKEN_STRING_LITERAL,
    TOKEN_NUMBER_LITERAL,
    TOKEN_BOOL_LITERAL,
    TOKEN_NULL_LITERAL,
    
    TOKEN_UNKNOWN,
    TOKEN_COUNT,
};


struct Token {
    char*       text;
    u32         lenght;
    TokenType   type;
};

struct Tokenizer {
    char*   at;
    u32     line = 1;
};
const char* GetTokenStr(TokenType t) {
    switch (t) {
        case TOKEN_KEYWORD_PRINT:                   return "TOKEN_KEYWORD_PRINT";
        case TOKEN_KEYWORD_IF:                      return "TOKEN_KEYWORD_IF";
        case TOKEN_KEYWORD_ELSE:                    return "TOKEN_KEYWORD_ELSE";
        case TOKEN_KEYWORD_FN:                      return "TOKEN_KEYWORD_FN";
        case TOKEN_KEYWORD_ARROW:                   return "TOKEN_KEYWORD_ARROW,";
        case TOKEN_KEYWORD_STRING:                  return "TOKEN_KEYWORD_STRING,";
        case TOKEN_KEYWORD_RETURN:                  return "TOKEN_KEYWORD_RETURN";
        case TOKEN_KEYWORD_MAIN:                    return "TOKEN_KEYWORD_MAIN";
        case TOKEN_KEYWORD_VAR:                     return "TOKEN_KEYWORD_VAR";
        case TOKEN_IDENTIFIER:                      return "TOKEN_IDENTIFIER";
        case TOKEN_OPEN_PAREN:                      return "TOKEN_OPEN_PAREN";
        case TOKEN_CLOSE_PAREN:                     return "TOKEN_CLOSE_PAREN";
        case TOKEN_OPEN_BRACKET:                    return "TOKEN_OPEN_BRACKET";
        case TOKEN_CLOSE_BRACKET:                   return "TOKEN_CLOSE_BRACKET";
        case TOKEN_OPEN_BRACES:                     return "TOKEN_OPEN_BRACES";
        case TOKEN_CLOSE_BRACES:                    return "TOKEN_CLOSE_BRACES";
        case TOKEN_PLUS_EQUALS:                     return "TOKEN_PLUS_EQUALS";
        case TOKEN_MINUS_EQUALS:                    return "TOKEN_MINUS_EQUALS";
        case TOKEN_ASTERISTK_EQUALS:                return "TOKEN_ASTERISTK_EQUALS";
        case TOKEN_SLASH_EQUALS:                    return "TOKEN_SLASH_EQUALS";
        case TOKEN_AMPERSAND_EQUALS:                return "TOKEN_AMPERSAND_EQUALS";
        case TOKEN_VERTICAL_BAR_EQUALS:             return "TOKEN_VERTICAL_BAR_EQUALS";
        case TOKEN_CIRCUMFLEX_EQUALS:               return "TOKEN_CIRCUMFLEX_EQUALS";
        case TOKEN_LSHIFT_EQUALS:                   return "TOKEN_LSHIFT_EQUALS";
        case TOKEN_RSHIFT_EQUALS:                   return "TOKEN_RSHIFT_EQUALS";
        case TOKEN_SUB_SCRIPT_OPR:                  return "TOKEN_SUB_SCRIPT_OPR";
        case TOKEN_EXCLAMATION_EQUALS:              return "TOKEN_EXCLAMATION_EQUALS";
        case TOKEN_TILDE_EQUALS:                    return "TOKEN_TILDE_EQUALS";
        case TOKEN_CIRCUMFLEX:                      return "TOKEN_CIRCUMFLEX";
        case TOKEN_DOT:                             return "TOKEN_DOT";
        case TOKEN_COMMA:                           return "TOKEN_COMA";
        case TOKEN_COLON:                           return "TOKEN_COLON";
        case TOKEN_SEMICOLON:                       return "TOKEN_SEMICOLON";
        case TOKEN_ASTERISK:                        return "TOKEN_ASTERISK";
        case TOKEN_AMPERSAND:                       return "TOKEN_AMPERSAND";
        case TOKEN_TILDE:                           return "TOKEN_TILDE";
        case TOKEN_EXCLAMATION_MARK:                return "TOKEN_EXLAMATION_MARK";
        case TOKEN_PLUS:                            return "TOKEN_PLUS";
        case TOKEN_PLUS_PLUS:                       return "TOKEN_PLUS_PLUS";
        case TOKEN_MINUS:                           return "TOKEN_MINUS";
        case TOKEN_MINUS_MINUS:                     return "TOKEN_MINUS_MINUS";
        case TOKEN_SLASH:                           return "TOKEN_SLASH";
        case TOKEN_EQUAL_SIGN:                      return "TOKEN_EQUAL_SIGN";
        case TOKEN_LSHIFT:                          return "TOKEN_LSHIFT";
        case TOKEN_RSHIFT:                          return "TOKEN_RSHIFT";
        case TOKEN_LSHIFT_LSHIFT:                   return "TOKEN_LSHIFT_LSHIFT";
        case TOKEN_RSHIFT_RSHIFT:                   return "TOKEN_RSHIFT_RSHIFT";
        case TOKEN_STRING_LITERAL:                  return "TOKEN_STRING_LITERAL";
        case TOKEN_NUMBER_LITERAL:                  return "TOKEN_NUMBER_LITERAL";
        case TOKEN_UNKNOWN:                         return "TOKEN_UNKNOWN";
        case TOKEN_EOF:                             return "TOKEN_EOF";
        case TOKEN_AMPERSAND_AMPERSAND:             return "TOKEN_AMPERSAND_AMPERSAND";
        case TOKEN_VERTICAL_BAR_VERTICAL_BAR:       return "TOKEN_VERTICAL_BAR_VERTICAL_BAR";
        case TOKEN_EQUALS_EQUALS:                   return "TOKEN_EQUALS_EQUALS";
        case TOKEN_BOOL_LITERAL:                    return "TOKEN_BOOL_LITERAL";
        case TOKEN_NULL_LITERAL:                    return "TOKEN_NULL_LITERAL";
        default:
            return nullptr;
            break;
    }
}
void PrintToken(Token t) {
    std::cout.write(t.text , t.lenght) << " ";
}


i64 GetI64(Token t) {
    char str[t.lenght+1]{0};
    for(u32 i = 0; i < t.lenght ; i++) {
        str[i] = t.text[i];
    }
    return atoi(str);
}
u64 GetU64(Token t) {
    char str[t.lenght+1]{0};
    for(u32 i = 0; i < t.lenght ; i++) {
        str[i] = t.text[i];
    }
    return strtoul(t.text , nullptr, 10);
}

f64 GetF64(Token t) {
    char str[t.lenght+1]{0};
    for(u32 i = 0; i < t.lenght ; i++) {
        str[i] = t.text[i];
    }
    return atof(str);
}

bool IsWhiteSpace(char c) {
    return  (c == ' ') ||
            (c == '\n') ||
            (c == '\t') ||
            (c == '\r');
}

u32 GetLineNumber(char* source ,char* at) {
    u32 c = 1;
    while( source != at ) {
        if( *source == '\n' ) c++;
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
bool TokenEquals(Token t , const char* match) {

    const char* m = match;
    for(u32 i = 0; i < t.lenght ; i++) {

        if(*m == 0 || t.text[i] != *m ) {
            return false;
        }

        m++;
    }
    return (*m == 0);
}
bool TokensEquals(Token t0 , Token t1) {

    if(t0.text == t1.text) {return true;}
    if(t0.lenght != t1.lenght) {return false;}

    for(u32 i = 0; i < t0.lenght ; i++) {
        if( t0.text[i] != t1.text[i] ) {
            return false;
        }
    }

    return true;
}
void EatMacors(Tokenizer* tokenizer) {

    bool eat = false;

    while(tokenizer->at[0]) {

        tokenizer->line += tokenizer->at[0] == '\n';
        if( tokenizer->at[0] == '\\' && tokenizer->at[1] == '\\' ) {
            eat = true;
            tokenizer->at++;
        }
        else if( tokenizer->at[0] == '\n') {
            if(eat) eat = false;
        }
        else {break;}

        ++tokenizer->at;
    }
}

void EatWhiteSpace(Tokenizer* tokenizer) {

    while(tokenizer->at[0]) {
        if( IsWhiteSpace(tokenizer->at[0])  ) {
            tokenizer->line += (tokenizer->at[0] == '\n');
            tokenizer->at++;
        }
        else if( tokenizer->at[0] == '/' && tokenizer->at[1] == '/' ) {
            tokenizer->at += 2;
            while( tokenizer->at[0] && !(tokenizer->at[0] == '\n' )) ++tokenizer->at;
            tokenizer->line++;
            tokenizer->at += 1;
        }
        else if( tokenizer->at[0] == '/' && tokenizer->at[1] == '*' ) {
            tokenizer->at += 2;
            while( tokenizer->at[0] && !(tokenizer->at[0] == '*' && tokenizer->at[1] == '/') ) tokenizer->line += (tokenizer->at++)[0] == '\n';
            tokenizer->at += 2;
        }
        else if( tokenizer->at[0] == '#') {
            EatMacors(tokenizer);
        }
        else {
            break;
        }
    }

}

Token GetToken(Tokenizer* tokenizer) {

    EatWhiteSpace(tokenizer);

    Token token{};
    token.lenght = 1;

    char c = tokenizer->at[0];
    token.text = tokenizer->at++; 

    switch (c) {
        case '\0'   :token.type = TOKEN_EOF;                break;
        case '('    :token.type = TOKEN_OPEN_PAREN;         break;
        case ')'    :token.type = TOKEN_CLOSE_PAREN;        break;
        case '['    :token.type = TOKEN_OPEN_BRACKET;       break;
        case ']'    :token.type = TOKEN_CLOSE_BRACKET;      break;
        case '{'    :token.type = TOKEN_OPEN_BRACES;        break;
        case '}'    :token.type = TOKEN_CLOSE_BRACES;       break;
        case ','    :token.type = TOKEN_COMMA;              break;
        case ':'    :token.type = TOKEN_COLON;              break;
        case ';'    :token.type = TOKEN_SEMICOLON;          break;
        case '='    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_EQUALS_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else {
                        token.type = TOKEN_EQUAL_SIGN;
                    }
                    break;
        case '*'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_ASTERISTK_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else {
                        token.type = TOKEN_ASTERISK;
                    }
                    break;
        case '&'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_AMPERSAND_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else if(tokenizer->at[0] == '&') {
                        token.type = TOKEN_AMPERSAND_AMPERSAND;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else {
                        token.type = TOKEN_AMPERSAND;
                    }
                    break;
        case '!'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_EXCLAMATION_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else {
                        token.type = TOKEN_EXCLAMATION_MARK;
                    }
                    break;
        case '+'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_PLUS_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else if(tokenizer->at[0] == '+') {
                        token.lenght++;
                        tokenizer->at++;
                        token.type = TOKEN_PLUS_PLUS;
                    }
                    else {
                        token.type = TOKEN_PLUS;
                    }
                    break;
        case '~'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_TILDE_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else {
                        token.type = TOKEN_TILDE;
                    }
                    break;
        case '-'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_MINUS_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else if(tokenizer->at[0] == '>') {
                        token.lenght++;
                        tokenizer->at++;
                        token.type = TOKEN_KEYWORD_ARROW;
                    }
                    else if(tokenizer->at[0] == '-') {
                        token.lenght++;
                        tokenizer->at++;
                        token.type = TOKEN_MINUS_MINUS;
                    }
                    else {
                        token.type = TOKEN_MINUS;
                    }
                    break;
        case '/'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_SLASH_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else {
                        token.type = TOKEN_SLASH;
                    }
                    break;
        case '>'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_RSHIFT_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else if(tokenizer->at[0] == '>') {
                        token.type = TOKEN_RSHIFT_RSHIFT;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else {
                        token.type = TOKEN_RSHIFT;
                    }
                    break;
        case '<'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_LSHIFT_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else if(tokenizer->at[0] == '<') {
                        token.type = TOKEN_LSHIFT_LSHIFT;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else {
                        token.type = TOKEN_LSHIFT;
                    }
                    break;
        case '|'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_VERTICAL_BAR_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else if(tokenizer->at[0] == '|') {
                        token.type = TOKEN_VERTICAL_BAR_VERTICAL_BAR;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else {
                        token.type = TOKEN_VERTICAL_BAR;
                    }
                    break;
        case '^'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_CIRCUMFLEX_EQUALS;
                        token.lenght++;
                        tokenizer->at++;
                    }
                    else {
                        token.type = TOKEN_CIRCUMFLEX;
                    }
                    break;
        case '\'':
            {
                token.type = TOKEN_CHAR_LITERAL;
                token.text = tokenizer->at;
                token.lenght = 0;
                if( token.text[0] == '\\' && token.text[1] == 'n') {
                    tokenizer->at += 2;
                    token.lenght += 2;
                } 
                else {
                    tokenizer->at++;
                    token.lenght++;
                }

                if(tokenizer->at[0] != '\'') {
                    std::cout << "ERROR: ilformed character literal at line: " << tokenizer->line << std::endl;
                    token.type = TOKEN_UNKNOWN;
                }
                tokenizer->at++;
            }
            break;
                    
        case '"'    :
            {
                token.text = tokenizer->at;
                token.type = TOKEN_STRING_LITERAL;

                while(  tokenizer->at[0] && tokenizer->at[0] != '"') {
                    
                    if(tokenizer->at[0] == '\\' && tokenizer->at[1] ) {
                        ++tokenizer->at;
                    }
                    ++tokenizer->at;
                }
                token.lenght = tokenizer->at - token.text;
                if( tokenizer->at[0] == '"' ) ++tokenizer->at;
                break;
            }

        case '.':
            if(!IsNumeric(tokenizer->at[0])) {token.type = TOKEN_DOT; break;}
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            {
                bool e = true;
                bool point = (c == '.');
                token.type = TOKEN_NUMBER_LITERAL;
                while(IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '.') {

                    if(tokenizer->at[0] == '.' && !point) {
                        point = true;
                    }
                    else if( tokenizer->at[0] == '.' && point && e) {
                        e = false;
                        std::cout << "ERROR: ilformed number literal at line: " << tokenizer->line << std::endl;
                        token.type = TOKEN_UNKNOWN;
                    }

                    tokenizer->at++;
                }
                if(tokenizer->at[0] == 'f') {
                    tokenizer->at++;

                    if( IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '.' ) {
                        std::cout << "ERROR: ilformed number literal at line: " << tokenizer->line << std::endl;
                        token.type = TOKEN_UNKNOWN;
                    }
                }

                token.lenght = tokenizer->at - token.text;
                break;
            }
    
        
        default:
            if( IsAlpha(c) ) {
                token.type = TOKEN_IDENTIFIER;
                while( IsAlpha(tokenizer->at[0]) || IsNumeric(tokenizer->at[0]) || tokenizer->at[0] == '_' ) tokenizer->at++;

                token.lenght = tokenizer->at - token.text;

                if( TokenEquals(token , "true") || TokenEquals(token , "false")) {
                    token.type = TOKEN_BOOL_LITERAL;
                }
                else if( TokenEquals(token , "null") ) {
                    token.type = TOKEN_NULL_LITERAL;
                }
                else if( TokenEquals(token , "var") ) {
                    token.type = TOKEN_KEYWORD_VAR;
                }
                else if(TokenEquals(token , "print")) {
                    token.type = TOKEN_KEYWORD_PRINT;
                }
                else if(TokenEquals(token , "if")) {
                    token.type = TOKEN_KEYWORD_IF;
                }
                else if(TokenEquals(token , "else")) {
                    token.type = TOKEN_KEYWORD_ELSE;
                }
                else if(TokenEquals(token , "for")) {
                    token.type = TOKEN_KEYWORD_FOR;
                }
                else if(TokenEquals(token , "fn")) {
                    token.type = TOKEN_KEYWORD_FN;
                }
                else if(TokenEquals(token , "return")) {
                    token.type = TOKEN_KEYWORD_RETURN;
                }
                else if(TokenEquals(token , "main")) {
                    token.type = TOKEN_KEYWORD_MAIN;
                }
                else if(TokenEquals(token , "string")) {
                    token.type = TOKEN_KEYWORD_STRING;
                }
                else if(TokenEquals(token, "auto")) {
                    token.type = TOKEN_KEYWORD_AUTO;
                }
                else if(TokenEquals(token, "struct")) {
                    token.type = TOKEN_KEYWORD_STRUCT;
                }
            

            }
            else {
                token.type = TOKEN_UNKNOWN;
                std::cout << "Error: unknown token at line : " << tokenizer->line << std::endl;
            }
            break;
    }

    return token;
}


byte* ReadFileTerminated(const char* fileName, byte* buffer,u32* size_) {

    byte* sourceString = nullptr;
    FILE* file = fopen(fileName ,"r");
    if(file) {

        fseek(file , 0, SEEK_END);
        u32 size = ftell(file);
        fseek(file ,0, SEEK_SET);
        *size_ = size;

        sourceString = buffer;
        if(buffer == nullptr) {
            sourceString = (byte*)LOG(my_malloc_debug(size + 1));
        }
        check_memory_integrity(sourceString);
        fread(sourceString , size , 1 , file);
        check_memory_integrity(sourceString);
        sourceString[size] = 0;

        fclose(file);
    }

    return sourceString;
}   
enum ValueTypes { 
    VALUE_STRING_LITERAL = TOKEN_STRING_LITERAL,
    VALUE_BOOL_LITERAL = TOKEN_BOOL_LITERAL,
    VALUE_NUMBER_LITERAL = TOKEN_NUMBER_LITERAL,
    VALUE_NULL_LITERAL = TOKEN_NULL_LITERAL,

    VALUE_VARIABLE,
    VALUE_NATIVE_FUNCTION,
    VALUE_NON_NATIVE_FUNCTION,
};

struct Value {
    ValueTypes type;
    byte mem[8]{0};
};
struct Symbol {
    Value v;
    u32 scope;
};
enum ExprType {
    EXPRESSION_NULL,

    EXPRESSION_BINARY,
    EXPRESSION_UNARY,
    EXPRESSION_LITERAL,
    EXPRESSION_GROUPING,
    EXPRESSION_VARIABLE,
    EXPRESSION_ASSIGNMENT,
    EXPRESSION_CALL,

    EXPRESSION_COUNT,
};

void PrintValue(Value v) {
    switch (v.type) {
        case TOKEN_STRING_LITERAL:
        
            PrintToken( *Cast<Token*>(v.mem) );
            break;
        case TOKEN_BOOL_LITERAL:
            std::cout << (Cast<bool>(v.mem) ? "true" : "false");
            break;
        case TOKEN_NUMBER_LITERAL:
            std::cout << Cast<double>(v.mem);
            break;
        case TOKEN_NULL_LITERAL:
            std::cout << "null";
            break;
        
        default:
            break;
    }

    std::cout << std::endl;
}

struct Expr {
    u32 index;
};
struct BinaryExpr : Expr {
    Token opr;
    Expr left;
    Expr right;
};
struct UnaryExpr : Expr {
    Token opr;
    Expr right;
};
struct LiteralExpr : Expr {
    Token literal;
};
struct GroupingExpr : Expr {
    Expr expr;
};
struct VariableExpr : Expr {
    Token name;
};
struct AssignExpr : Expr {
    Expr value;
    Token name;
};
struct CallExpr : Expr {
    Expr callee;
    Expr args;
};


enum StatementType {
    STATEMENT_NULL,

    STATEMENT_PRINT,
    STATEMENT_EXPRESSION,
    STATEMENT_VAR_DECLARATION,
    STATEMENT_FN_DECLARATION,
    STATEMENT_SCOPE_ENTER,
    STATEMENT_SCOPE_EXIT,
    STATEMENT_IF,
    STATEMENT_JMP,
    STATEMENT_RETURN,

    STATEMENT_COUNT,
};

struct Stmt {
    u32     index;
};
struct ExprStmt : Stmt {
    Expr    expr;
};
struct PrintStmt : Stmt {
    Expr    expr;
};
struct ScopeStmt : Stmt {
    Stmt    statements;
};
struct DeclVarStmt : Stmt {
    Token   name;
    Expr    expr;
};
struct DeclFnStmt : Stmt {
    Token   name;
    u32     paramIndex;
    Stmt    body;
};
struct JmpStmt : Stmt {
    Stmt    Target;
};
struct BranchStmt : Stmt {
    Expr    condition;
    Stmt    thenBranch;
    Stmt    elseBranch;
};
struct DeclFnStmtReversed {
    Token           name;
    Stmt            body;
    u32             paramIndex;
    StatementType   type;
};
struct ReturnStmt : Stmt {
    Expr returnValue;
};

struct Parser {
    Tokenizer tokenizer;
    DynamicBuffer<Token>  tokenBuffer;
    char*                       source = nullptr;

    byte*                       mem = nullptr;
    u32                         memCap = 512 * KILO_BYTE;
    u32                         exprAllocator = 64;
    u32                         stmtAllocator = 256 * KILO_BYTE;
    Stmt                        entryPoint;
};

u64 hash(void* user, Token symbol) {
    return StringHash(symbol.text, symbol.lenght);
}
bool eq(void* user, Token key0, Token key1) {
    return TokensEquals(key0,key1);
}

typedef Value(*native_function_t)(Value* args);
struct InterpreterState {

    HashTable<Token,Symbol, hash, eq, Token{}>    symbolTable;
    byte*                   stack = nullptr;
    byte*                   stackPtr;
    byte*                   returnAddress;
    Stmt                    program;
    u32                     scope;
};

template<typename T> void Push(InterpreterState* interpreter, T t) {
    Cast<T>(interpreter->stackPtr) = t;
    interpreter->stackPtr += sizeof(T);
}
template<typename T> T Pop(InterpreterState* interpreter) {
    interpreter->stackPtr -= sizeof(T);
    return Cast<T>(interpreter->stackPtr);
}

Token PreviousToken(Parser* parser) {
    return parser->tokenBuffer.mem[parser->tokenBuffer.size - 1];
}
Token NextToken(Parser* parser) {
    u32 i = parser->tokenBuffer.PushBack( GetToken(&parser->tokenizer) );
    return parser->tokenBuffer[i];
}

bool Check(Parser* parser , TokenType type) {
    if(!parser->tokenizer.at[0]) return false;

    Tokenizer peek = parser->tokenizer;
    Token nextToken = GetToken(&peek);
    return (nextToken.type == type);
}

bool Match(Parser* parser , TokenType* types , u32 count) {
    for(u32 i = 0; i < count ;i++) {
        if(Check(parser , types[i])) {
            NextToken(parser);
            return true;
        }
    }
    return false;
}

void ExpectToken(Parser* parser , TokenType e) {

    Token token = NextToken(parser);

    if(token.type != e) {
        std::cout << "ERROR: exptected: " << GetTokenStr(e);
        std::cout << " got: ";
        PrintToken(token);
        std::cout << " " << token.lenght << std::endl;
    }
}



Expr Expression(Parser* parser);
Expr Primary(Parser* parser) {

    TokenType m[4]{TOKEN_BOOL_LITERAL , TOKEN_NULL_LITERAL , TOKEN_NUMBER_LITERAL , TOKEN_STRING_LITERAL };
    TokenType identifier = TOKEN_IDENTIFIER;
    TokenType openParen = TOKEN_OPEN_PAREN;

    if(Match(parser , &openParen, 1)) {
        Expr expr = Expression(parser);

        ExpectToken(parser , TOKEN_CLOSE_PAREN);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(GroupingExpr);
        ((GroupingExpr*)(parser->mem + index))->index = EXPRESSION_GROUPING;
        ((GroupingExpr*)(parser->mem + index))->expr = expr;

        return Expr{index};
    }
    else if(Match(parser , m , 4)) {
        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(LiteralExpr);
        ((LiteralExpr*)(parser->mem + index))->index = EXPRESSION_LITERAL;
        ((LiteralExpr*)(parser->mem + index))->literal = PreviousToken(parser);
        return Expr{index};
    }
    else if(Match(parser , &identifier, 1)) {
        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(VariableExpr);
        VariableExpr* var = &Cast<VariableExpr>(parser->mem + index);
        var->index = EXPRESSION_VARIABLE;
        var->name = PreviousToken(parser);

        return Expr{index};
    }

    ASSERT(false);

    return Expr{0};
}

Expr RestOfCall(Parser* parser, Expr callee) {

    u32 index = parser->exprAllocator;
    CallExpr& call = Cast<CallExpr>(parser->mem + index);
    parser->exprAllocator += sizeof(CallExpr);

    call.index = EXPRESSION_CALL;
    call.callee = callee;

    u32 argCount = 0;
    Tokenizer peek = parser->tokenizer;
    if(!Check(parser , TOKEN_CLOSE_PAREN)) {
        do {
            argCount++;
            GetToken(&peek);
        } while(GetToken(&peek).type == TOKEN_COMMA);
    }

    Expr args{parser->exprAllocator};
    parser->exprAllocator += sizeof(Expr) * argCount;
    Cast<Expr>(parser->mem + parser->exprAllocator).index = EXPRESSION_NULL;
    parser->exprAllocator += sizeof(Expr);

    
    u32 i = args.index;
    if(!Check(parser , TOKEN_CLOSE_PAREN)) {
        do {
            Cast<Expr>(parser->mem + i) = Expression(parser);
            i += sizeof(Expr);
        } while(NextToken(parser).type == TOKEN_COMMA);

        TokenType t = parser->tokenBuffer.Back().type;
        if( t != TOKEN_CLOSE_PAREN) {
            std::cout << "ERROR: expected ) at line: " << parser->tokenizer.line << std::endl;
        }
    }
    else {
        NextToken(parser);
    }

    call.args = args;
    return Expr{index};
}
Expr Call(Parser* parser) {

    Expr expr = Primary(parser);

    for(;;) {
        
        if(Check(parser,TOKEN_OPEN_PAREN)) {
            NextToken(parser);
            expr = RestOfCall(parser, expr);
        }
        else {
            break;
        }
    }
    return expr;
}

Expr Unary(Parser* parser) {

    TokenType tokens[2]{TOKEN_EXCLAMATION_MARK,TOKEN_MINUS};
    while(Match(parser , tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Unary(parser);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(UnaryExpr);
        ((UnaryExpr*)(parser->mem + index))->index = EXPRESSION_UNARY;
        ((UnaryExpr*)(parser->mem + index))->opr = opr;
        ((UnaryExpr*)(parser->mem + index))->right = right;

        return Expr{index};
    }
    
    return Call(parser);
}
Expr Factor(Parser* parser) {

    Expr expr = Unary(parser);

    TokenType tokens[2]{TOKEN_ASTERISK , TOKEN_SLASH};
    while(Match(parser, tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Unary(parser);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(BinaryExpr);
        ((BinaryExpr*)(parser->mem + index))->index = EXPRESSION_BINARY;
        ((BinaryExpr*)(parser->mem + index))->left = expr;
        ((BinaryExpr*)(parser->mem + index))->opr = opr;
        ((BinaryExpr*)(parser->mem + index))->right = right;

        expr.index = index;
    }

    return expr;
}
Expr Term(Parser* parser) {
    Expr expr = Factor(parser);

    TokenType tokens[2]{TOKEN_PLUS,TOKEN_MINUS};

    while(Match(parser, tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Factor(parser);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(BinaryExpr);
        ((BinaryExpr*)(parser->mem + index))->index = EXPRESSION_BINARY;
        ((BinaryExpr*)(parser->mem + index))->left = expr;
        ((BinaryExpr*)(parser->mem + index))->opr = opr;
        ((BinaryExpr*)(parser->mem + index))->right = right;

        expr.index = index;
    }

    return expr;
}
Expr Comparison(Parser* parser) {

    Expr expr = Term(parser);

    TokenType tokens[4]{TOKEN_RSHIFT_EQUALS , TOKEN_LSHIFT_EQUALS , TOKEN_LSHIFT , TOKEN_RSHIFT};

    while(Match(parser, tokens , 4)) {
        Token opr = PreviousToken(parser);
        Expr right = Term(parser);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(BinaryExpr);
        ((BinaryExpr*)(parser->mem + index))->index = EXPRESSION_BINARY;
        ((BinaryExpr*)(parser->mem + index))->left = expr;
        ((BinaryExpr*)(parser->mem + index))->opr = opr;
        ((BinaryExpr*)(parser->mem + index))->right = right;

        expr.index = index;
    }

    return expr;
}
Expr Equality(Parser* parser) {

    Expr expr = Comparison(parser);

    TokenType tokens[2]{TOKEN_EXCLAMATION_EQUALS,TOKEN_EQUALS_EQUALS};
    while(Match(parser, tokens , 2)) {
        Token opr = PreviousToken(parser);
        Expr right = Comparison(parser);

        u32 index = parser->exprAllocator;
        parser->exprAllocator += sizeof(BinaryExpr);
        ((BinaryExpr*)(parser->mem + index))->index = EXPRESSION_BINARY;
        ((BinaryExpr*)(parser->mem + index))->left = expr;
        ((BinaryExpr*)(parser->mem + index))->opr = opr;
        ((BinaryExpr*)(parser->mem + index))->right = right;

        expr.index = index;
    }

    return expr;
}
Expr Assignment(Parser* parser) {

    Expr expr = Equality(parser);

    TokenType e = TOKEN_EQUAL_SIGN;
    if(Match(parser , &e , 1)) {

        Expr value = Assignment(parser);
        ExprType expresionType = Cast<ExprType>(parser->mem + expr.index);

        if(expresionType == EXPRESSION_VARIABLE) {
            Token name = Cast<VariableExpr>(parser->mem + expr.index).name;

            u32 index = parser->exprAllocator;
            parser->exprAllocator += sizeof(AssignExpr);
            AssignExpr* assign = &Cast<AssignExpr>(parser->mem + index);
            assign->index = EXPRESSION_ASSIGNMENT;
            assign->name = name;
            assign->value = value;

            return Expr{index};
        }

        std::cout << "ERROR: expression must be a modifiable l-value at line: " << parser->tokenizer.line << std::endl;
    }

    return expr;
}
Expr Expression(Parser* parser) {
    return Assignment(parser);
}

Stmt Statement(Parser* parser) {

    bool expect = true;
    Stmt ret{0};

    if(Check(parser,TOKEN_KEYWORD_PRINT )) {

        NextToken(parser);

        u32 index = parser->stmtAllocator;
        PrintStmt* stmt = (PrintStmt*)(parser->mem + index);
        parser->stmtAllocator += sizeof(PrintStmt);

        stmt->index = STATEMENT_PRINT;
        stmt->expr = Expression(parser);

        ret.index = index;
    }
    else if(Check(parser , TOKEN_KEYWORD_VAR)) {
        NextToken(parser);

        u32 index = parser->stmtAllocator;
        DeclVarStmt& declaration = Cast<DeclVarStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(DeclVarStmt);

        declaration.index = STATEMENT_VAR_DECLARATION;
        ExpectToken(parser , TOKEN_IDENTIFIER);
        declaration.name = parser->tokenBuffer.Back();
        declaration.expr.index = 0;
        if(!Check(parser , TOKEN_SEMICOLON)) {
            ExpectToken(parser , TOKEN_EQUAL_SIGN);
            declaration.expr = Expression(parser);
        }

        ret.index = index;
    } 
    else if(Check(parser , TOKEN_KEYWORD_FN)) {
        NextToken(parser);

        u32 index = parser->stmtAllocator;
        DeclFnStmt& declaration = Cast<DeclFnStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(DeclFnStmt);


        declaration.index = STATEMENT_FN_DECLARATION;

        bool main = false;
        if(Check(parser , TOKEN_IDENTIFIER) || Check(parser , TOKEN_KEYWORD_MAIN)) {

            Token name = NextToken(parser);
            if(TokenEquals(name , "main")) {
                main = true;
            }
        }
        else {
            std::cout << "ERROR: exptected identifier at line: " << parser->tokenizer.line << std::endl;
        }

        declaration.name = parser->tokenBuffer.Back();
        ExpectToken(parser , TOKEN_OPEN_PAREN);

        TokenType match = TOKEN_COMMA;
        declaration.paramIndex = parser->stmtAllocator;
        if(!Check(parser, TOKEN_CLOSE_PAREN)) {
            do {
                Cast<Token>(parser->mem + parser->stmtAllocator) = NextToken(parser);
                parser->stmtAllocator += sizeof(Token);
            } while(Match(parser, &match , 1));
        }

        Token null;
        null.type = TOKEN_EOF;
        Cast<Token>(parser->mem + parser->stmtAllocator) = null;
        parser->stmtAllocator += sizeof(Token);

        u32 jmpIndex = parser->stmtAllocator;
        parser->stmtAllocator += sizeof(JmpStmt);
        Cast<JmpStmt>(parser->mem + jmpIndex).index = STATEMENT_JMP;

        ExpectToken(parser , TOKEN_CLOSE_PAREN);
        declaration.body = Statement(parser);
        if(main) {
            parser->entryPoint = declaration.body;
        }

        expect = (Cast<StatementType>(parser->mem + declaration.body.index) != STATEMENT_SCOPE_ENTER);

        Cast<JmpStmt>(parser->mem + jmpIndex).Target = Stmt{parser->stmtAllocator};

        ret.index = index;
    } 
    else if(Check(parser , TOKEN_OPEN_BRACES)) {
        NextToken(parser);
        expect = false;
        
        u32 index = parser->stmtAllocator;
        ScopeStmt& scope = Cast<ScopeStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(ScopeStmt);

        scope.index = STATEMENT_SCOPE_ENTER;
        scope.statements = Stmt{parser->stmtAllocator};

        while( Cast<StatementType>(parser->mem + Statement(parser).index) != STATEMENT_SCOPE_EXIT);

        ret.index = index;
    }
    else if(Check(parser , TOKEN_CLOSE_BRACES)) {
        NextToken(parser);
        expect = false;

        u32 index = parser->stmtAllocator;
        Stmt& scopeEnd = Cast<ScopeStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(ScopeStmt);

        scopeEnd.index = STATEMENT_SCOPE_EXIT;

        ret.index = index;
    }
    else if(Check(parser , TOKEN_KEYWORD_IF)) {

        NextToken(parser);
        u32 index = parser->stmtAllocator;
        BranchStmt& branch = Cast<BranchStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(BranchStmt);

        ExpectToken(parser , TOKEN_OPEN_PAREN);
        branch.condition = Expression(parser);
        ExpectToken(parser , TOKEN_CLOSE_PAREN);

        branch.index = STATEMENT_IF;
        branch.thenBranch = Statement(parser);
        branch.elseBranch = Stmt{parser->stmtAllocator};

        if(Check(parser , TOKEN_KEYWORD_ELSE)) {
            NextToken(parser);
            
            expect = !(Check(parser , TOKEN_OPEN_BRACES));

            u32 index = parser->stmtAllocator;
            JmpStmt& jmp = Cast<JmpStmt>(parser->mem + index);
            parser->stmtAllocator += sizeof(JmpStmt);

            branch.elseBranch = Statement(parser);

            jmp.index = STATEMENT_JMP;
            jmp.Target = Stmt{parser->stmtAllocator};
        }
        else {
            expect = !(Check(parser , TOKEN_OPEN_BRACES));
        }
        ret.index = index;
    }
    else if(Check(parser , TOKEN_KEYWORD_RETURN)) {
        NextToken(parser);

        u32 index = parser->stmtAllocator;
        ReturnStmt& stmt = Cast<ReturnStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(ReturnStmt);

        stmt.index = STATEMENT_RETURN;
        stmt.returnValue = Expr{EXPRESSION_NULL};
        if(!Check(parser ,TOKEN_SEMICOLON)) {
            stmt.returnValue = Expression(parser);
        }


    }
    else if(Check(parser , TOKEN_KEYWORD_FOR)) {
        NextToken(parser);
        ExpectToken(parser , TOKEN_OPEN_PAREN);

        u32 index = parser->stmtAllocator;
        ScopeStmt& scopeOuterBegin = Cast<ScopeStmt>(parser->mem + index);
        parser->stmtAllocator += sizeof(ScopeStmt);
        scopeOuterBegin.index = STATEMENT_SCOPE_ENTER;

        Stmt init = Statement(parser);
        StatementType initT = Cast<StatementType>(parser->mem + init.index);
        if( !(initT == STATEMENT_EXPRESSION || initT == STATEMENT_VAR_DECLARATION) ) {
            std::cout << "ERROR: for loop initalizer must be either an expression statement or a declaration at line: " << parser->tokenizer.line << std::endl;
        }

        u32 branchIndex = parser->stmtAllocator;
        BranchStmt& branch = Cast<BranchStmt>(parser->mem + branchIndex);
        parser->stmtAllocator += sizeof(BranchStmt);
        branch.index = STATEMENT_IF;
        branch.condition = Expression(parser);
        ExpectToken(parser, TOKEN_SEMICOLON);
        
        Stmt incSmtm = Stmt{parser->stmtAllocator};
        Cast<ExprStmt>(parser->mem + incSmtm.index).index = STATEMENT_EXPRESSION;
        Cast<ExprStmt>(parser->mem + incSmtm.index).expr = Expression(parser);

        if(Cast<StatementType>(parser->mem + incSmtm.index) != STATEMENT_EXPRESSION) {
            std::cout << "ERROR: for loop increment must be an expression statement at line: " << parser->tokenizer.line << std::endl;
        }
        ExprStmt incStmt = Cast<ExprStmt>(parser->mem + incSmtm.index);

        ExpectToken(parser, TOKEN_CLOSE_PAREN);

        Stmt body;
        if(Check(parser , TOKEN_OPEN_BRACES)) {
            body = Statement(parser);

            expect = false;
        }
        else {
            expect = Check(parser , TOKEN_SEMICOLON);

            ScopeStmt& scopeInnerBegin = Cast<ScopeStmt>(parser->mem + parser->stmtAllocator);
            parser->stmtAllocator += sizeof(ScopeStmt);
            scopeInnerBegin.index = STATEMENT_SCOPE_ENTER;

            body = Statement(parser);

            ScopeStmt& scopeInnerEnd = Cast<ScopeStmt>(parser->mem + parser->stmtAllocator);
            parser->stmtAllocator += sizeof(ScopeStmt);
            scopeInnerEnd.index = STATEMENT_SCOPE_EXIT;
        }
        branch.thenBranch = body;

        Cast<ExprStmt>(parser->mem + parser->stmtAllocator) = incStmt;
        parser->stmtAllocator += sizeof(ExprStmt);
       
        JmpStmt& jmp = Cast<JmpStmt>(parser->mem + parser->stmtAllocator);
        parser->stmtAllocator += sizeof(JmpStmt);
        jmp.index = STATEMENT_JMP;
        jmp.Target = Stmt{branchIndex};

        branch.elseBranch = Stmt{parser->stmtAllocator};
        ScopeStmt& scopeOuterEnd = Cast<ScopeStmt>(parser->mem + parser->stmtAllocator);
        parser->stmtAllocator += sizeof(ScopeStmt);
        scopeOuterEnd.index = STATEMENT_SCOPE_EXIT;
        

        ret.index = index;
    }
    else {
        u32 index = parser->stmtAllocator;
        ExprStmt* exprStmt = (ExprStmt*)(parser->mem + index);
        parser->stmtAllocator += sizeof(ExprStmt);

        exprStmt->index = STATEMENT_EXPRESSION;
        exprStmt->expr = Expression(parser);

        ret.index = index;
    }

    if(expect) {
        ExpectToken(parser , TOKEN_SEMICOLON);
    }

    return ret;
}

Stmt Parse(Parser* parser) {

    Stmt program;
    program.index = parser->stmtAllocator;
    parser->entryPoint.index = STATEMENT_NULL;


    for(;;) {

        Tokenizer peek = parser->tokenizer;
        if( GetToken(&peek).type != TOKEN_EOF ) {
            Statement(parser);
        }
        else {
            break;
        }
    }

    u32 index = parser->stmtAllocator;
    Stmt* end = (Stmt*)(parser->mem + index);
    end->index = STATEMENT_NULL;

    if(parser->entryPoint.index != STATEMENT_NULL) {
        program = parser->entryPoint;
    }

    return program;
}


double GetNumber(LiteralExpr l) {    
    char str[l.literal.lenght+1]{0};
    for(u32 i = 0; i < l.literal.lenght ; i++) {
        str[i] = l.literal.text[i];
    }
    return strtod(str,nullptr);
}
bool GetBool(LiteralExpr l) {
    return TokenEquals(l.literal , "true");
}



void UnwindStack(InterpreterState* interpreter) {
    while(interpreter->stackPtr != interpreter->returnAddress ) {

        StatementType type = Cast<StatementType>(interpreter->stackPtr-8);

        switch (type) {
            case STATEMENT_VAR_DECLARATION:
                {
                    DeclVarStmt stmt = Pop<DeclVarStmt>(interpreter);
                    u32 index = interpreter->symbolTable.Find(stmt.name);
                    ASSERT(index != ~u32(0));
                    if(interpreter->symbolTable.array[index].value.scope != 0) {
                        interpreter->symbolTable.Remove(index);
                    }

                }
                break;
            case STATEMENT_FN_DECLARATION:
                {
                    DeclFnStmt stmt = Pop<DeclFnStmt>(interpreter);
                    u32 index = interpreter->symbolTable.Find(stmt.name);
                    ASSERT(index != ~u32(0));
                    interpreter->symbolTable.array[index];
                    if(interpreter->symbolTable.array[index].value.scope != 0) {
                        interpreter->symbolTable.Remove(index);
                    }
                }
                break;

            default:
                return;
                break;
        }
    }

    byte* returnAddress = Pop<byte*>(interpreter);
    interpreter->returnAddress = returnAddress;
}

void DefineVar(InterpreterState* interpreter, Symbol s , Token name) {

    u32 index = interpreter->symbolTable.Find(name);
    if(index == ~(u32(0))) {
        interpreter->symbolTable.Insert(name, s);
    }
}
void UnDefineVar(InterpreterState* interpreter, Token name) {

    u32 index = interpreter->symbolTable.Find(name);
    if(index != ~(u32(0))) {
        interpreter->symbolTable.Delete(name);
    }
}
Value Evaluate(InterpreterState* interpreter, Parser* parser, Expr expr);

void Execute(InterpreterState* interpreter ,Parser* parser, Stmt program) {

    interpreter->program = program;

    bool running = true;
    while(running) {
        switch ( Cast<StatementType>( parser->mem + interpreter->program.index ) ) {
            case STATEMENT_EXPRESSION:
                {
                    ExprStmt& stmt = Cast<ExprStmt>( parser->mem + interpreter->program.index );
                    Evaluate(interpreter, parser , stmt.expr);
                    interpreter->program.index += sizeof(ExprStmt);
                }
                break;
            case STATEMENT_PRINT:
                {
                    PrintStmt& stmt = Cast<PrintStmt>( parser->mem + interpreter->program.index );
                    Value val = Evaluate(interpreter ,parser , stmt.expr);
                    interpreter->program.index += sizeof(PrintStmt);
                    PrintValue(val);
                }
                break;
            case STATEMENT_VAR_DECLARATION:
                {
                    DeclVarStmt& stmt = Cast<DeclVarStmt>( parser->mem + interpreter->program.index );

                    if(TokenEquals(stmt.name, "returnVal")) {
                        std::cout << "ERROR: returnVal is a reserved variable name at line: " << GetLineNumber(parser->source , stmt.name.text) << std::endl;
                    }

                    Symbol symbol;
                    symbol.scope = interpreter->scope;
                    Cast<u64>(symbol.v.mem) = 0;
                    symbol.v.type = VALUE_NULL_LITERAL;
                    if( stmt.expr.index != 0 ) {
                        symbol.v = Evaluate(interpreter ,parser , stmt.expr);
                    }
                    u32 index = interpreter->symbolTable.Find(stmt.name);
                    if(index != ~(u32(0))) {
                        std::cout << "ERROR: variable redecleration ";
                        PrintToken(stmt.name);
                        std::cout << " at line: " << GetLineNumber(parser->source , stmt.name.text) << std::endl;
                    }
                    else {
                        interpreter->symbolTable.Insert(stmt.name, symbol);
                        DeclVarStmt s = stmt;
                        s.expr.index = STATEMENT_VAR_DECLARATION;
                        Push<DeclVarStmt>(interpreter, s);
                    }

                    interpreter->program.index += sizeof(DeclVarStmt);
                }
                break;
            case STATEMENT_FN_DECLARATION:
                {
                    DeclFnStmt& decl = Cast<DeclFnStmt>(parser->mem + interpreter->program.index);
                    if(TokenEquals(decl.name, "returnVal")) {
                        std::cout << "ERROR: returnVal is a reserved variable name at line: " << GetLineNumber(parser->source , decl.name.text) << std::endl;
                    }

                    u32 index = interpreter->symbolTable.Find(decl.name);
                    if(index != ~(u32(0))) {
                        std::cout << "ERROR: function redecleration ";
                        PrintToken(decl.name);
                        std::cout << " at line: " << parser->tokenizer.line << std::endl;
                    }
                    else {
                        
                        Symbol function;
                        function.scope = interpreter->scope;
                        function.v.type = VALUE_NON_NATIVE_FUNCTION;
                        Cast<Stmt>(function.v.mem) = interpreter->program;

                        DeclFnStmtReversed s;
                        s.body = decl.body;
                        s.name = decl.name;
                        s.paramIndex = decl.paramIndex;
                        s.type = STATEMENT_FN_DECLARATION;
                        interpreter->symbolTable.Insert(decl.name, function);

                        Push<DeclFnStmtReversed>(interpreter, s);
                    }

                    u32 i = decl.paramIndex;
                    while( Cast<Token>(parser->mem + i).type != TOKEN_EOF ) {
                        i += sizeof(Token);
                    } 
                    i += sizeof(Token);

                    interpreter->program.index = i;
                }
                break;
            case STATEMENT_SCOPE_ENTER:
                {
                    Push<byte*>(interpreter , interpreter->returnAddress);
                    interpreter->returnAddress = interpreter->stackPtr;
                    interpreter->program.index += sizeof(ScopeStmt);
                    interpreter->scope++;
                    Execute(interpreter, parser, interpreter->program );
                }
                break;
            case STATEMENT_SCOPE_EXIT:
                {
                    UnwindStack(interpreter);
                    interpreter->scope--;
                    interpreter->program.index += sizeof(ScopeStmt);
                    return;
                }
                break;
            case STATEMENT_RETURN:
                {
                    ReturnStmt& stmt = Cast<ReturnStmt>(parser->mem + interpreter->program.index);
                    
                    if(stmt.returnValue.index != EXPRESSION_NULL) {
                        Value ret = Evaluate(interpreter , parser ,stmt.returnValue);

                        Token t = {"returnValue",11,TOKEN_IDENTIFIER};
                        u32 index = interpreter->symbolTable.Find(t);
                        if(index != ~(u32(0))) {
                            interpreter->symbolTable.array[index].value.v = ret;
                        }
                        else {
                            std::cout << "ERROR: returnValue is only defined inside function bodies at line: " << std::endl;
                        }
                    }

                    UnwindStack(interpreter);
                    interpreter->program.index += sizeof(ReturnStmt);
                    return;
                }
                break;
            case STATEMENT_JMP:
                {
                    JmpStmt& jmp = Cast<JmpStmt>(parser->mem + interpreter->program.index);
                    interpreter->program = jmp.Target;
                }
                break;

            case STATEMENT_IF:
                {
                    BranchStmt& branch = Cast<BranchStmt>( parser->mem + interpreter->program.index );
                    Value result = Evaluate(interpreter ,parser, branch.condition );
                    if(result.type == TOKEN_BOOL_LITERAL) {
                        if( Cast<bool>(result.mem) ) {
                            interpreter->program = branch.thenBranch;
                        }
                        else {
                            interpreter->program = branch.elseBranch;
                        }
                    }
                    else {
                        std::cout << "ERROR: conditions must evaluate to boolean expressions at line :" << parser->tokenizer.line << std::endl;
                    }
                }
                break;
            
            case STATEMENT_NULL:
                running = false;
                break;
            
            default:
                break;
        }
    }
}

Value Evaluate(InterpreterState* interpreter, Parser* parser, Expr expr) {

    ExprType type =  *((ExprType*)(parser->mem + expr.index));

    switch(type) {
        case EXPRESSION_BINARY:
            {
                Value l = Evaluate(interpreter, parser ,((BinaryExpr*)(parser->mem + expr.index))->left);
                Value r = Evaluate(interpreter, parser ,((BinaryExpr*)(parser->mem + expr.index))->right);

                Value ret;
                BinaryExpr* bex = ((BinaryExpr*)(parser->mem + expr.index));

                if( bex->opr.type == TOKEN_PLUS ) {
                    ret.type = VALUE_NUMBER_LITERAL;
                    if( l.type != VALUE_NUMBER_LITERAL || r.type != VALUE_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for + must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                    Cast<double>(ret.mem) = Cast<double>(l.mem) + Cast<double>(r.mem);
                }
                else if( bex->opr.type == TOKEN_MINUS ) {
                    ret.type = VALUE_NUMBER_LITERAL;
                    if( l.type != VALUE_NUMBER_LITERAL || r.type != VALUE_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for - must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                    Cast<double>(ret.mem) = Cast<double>(l.mem) - Cast<double>(r.mem);
                }
                else if( bex->opr.type == TOKEN_ASTERISK ) {
                    ret.type = VALUE_NUMBER_LITERAL;
                    if( l.type != VALUE_NUMBER_LITERAL || r.type != VALUE_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for * must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                    Cast<double>(ret.mem) = Cast<double>(l.mem) * Cast<double>(r.mem);
                }
                else if( bex->opr.type == TOKEN_SLASH ) {
                    ret.type = VALUE_NUMBER_LITERAL;
                    if( l.type != VALUE_NUMBER_LITERAL || r.type != VALUE_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for / must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                    Cast<double>(ret.mem) = Cast<double>(l.mem) / Cast<double>(r.mem);
                }
                else if(bex->opr.type == TOKEN_EQUALS_EQUALS) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_BOOL_LITERAL && r.type == VALUE_BOOL_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<bool>(l.mem) == Cast<bool>(r.mem);
                    }
                    else if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) == Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for == must be numeric or boolean expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_EXCLAMATION_EQUALS) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_BOOL_LITERAL && r.type == VALUE_BOOL_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<bool>(l.mem) != Cast<bool>(r.mem);
                    }
                    else if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) != Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for != must be numeric or boolean expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_RSHIFT_EQUALS) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) >= Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for >= must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_LSHIFT_EQUALS) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) <= Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for <= must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_RSHIFT) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) > Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for > must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }
                else if(bex->opr.type == TOKEN_LSHIFT) {
                    ret.type = VALUE_BOOL_LITERAL;
                    if( l.type == VALUE_NUMBER_LITERAL && r.type == VALUE_NUMBER_LITERAL ) {
                        Cast<bool>(ret.mem) = Cast<double>(l.mem) < Cast<double>(r.mem);
                    }
                    else {
                        std::cout << "ERROR: operands for < must be numeric expressions at line : " << parser->tokenizer.line << std::endl;
                    }
                }


                return ret;
            }

            break;
        case EXPRESSION_UNARY:

            {
                UnaryExpr* ex = ((UnaryExpr*)(parser->mem + expr.index));
                Value r = Evaluate(interpreter ,parser , ex->right);

                if( ex->opr.type == TOKEN_EXCLAMATION_MARK ) {
                    if( r.type != VALUE_BOOL_LITERAL ) {
                        std::cout << "ERROR: operand for ! must be a boolean expression at line : " << GetLineNumber(parser->source, ex->opr.text ) << std::endl;
                    }
                    else {
                        Cast<bool>(r.mem) = !Cast<bool>(r.mem);
                    }
                }
                else if( ex->opr.type == TOKEN_MINUS ) {
                    if( r.type != VALUE_NUMBER_LITERAL ) {
                        std::cout << "ERROR: operands for - be numeric expressions at line : " << GetLineNumber(parser->source, ex->opr.text ) << std::endl;
                    }
                    Cast<double>(r.mem) = - Cast<double>(r.mem);
                }
                

                return r;
            }
            break;
        case EXPRESSION_VARIABLE:
            {
                Token name = Cast<VariableExpr>(parser->mem + expr.index).name;
                u32 index = interpreter->symbolTable.Find(name);
                if(index == ~(u32(0)) ) {
                    std::cout << "ERROR: unknown indentifier ";
                    PrintToken(name);
                    std::cout << " used in expression at line " << GetLineNumber(parser->source , name.text) << std::endl;

                    Value null;
                    Cast<u64>(null.mem) = 0;
                    null.type = VALUE_NULL_LITERAL;
                    return null;
                }
                return interpreter->symbolTable.array[index].value.v;
            }
            break;
        case EXPRESSION_LITERAL:
            {
                LiteralExpr* literal = ((LiteralExpr*)(parser->mem + expr.index));
                Value r;
                r.type = (ValueTypes)literal->literal.type;

                if( r.type == VALUE_NUMBER_LITERAL ) {
                    Cast<double>(r.mem) = GetNumber( *literal );
                }
                else if( r.type == VALUE_STRING_LITERAL ) {
                    Cast<Token*>(r.mem) = &literal->literal;
                }
                else if(r.type == VALUE_BOOL_LITERAL) {
                    Cast<bool>(r.mem) = GetBool( *literal );
                }
                
                return r;
            }

        case EXPRESSION_ASSIGNMENT:
            {

                AssignExpr* assign = &Cast<AssignExpr>(parser->mem + expr.index);
                u32 index = interpreter->symbolTable.Find(assign->name);
                if(index != ~(u32(0))) {
                    Value val = Evaluate(interpreter,parser,assign->value);

                    if( val.type == interpreter->symbolTable.array[index].value.v.type || interpreter->symbolTable.array[index].value.v.type == VALUE_NULL_LITERAL) {
                        interpreter->symbolTable.array[index].value.v = val;
                        return val;
                    }
                    else {
                        std::cout << "ERROR: invalid assignment at line: " << parser->tokenizer.line << std::endl;
                    }
                }
                else {
                    std::cout << "ERROR: undefined variable at line: " << parser->tokenizer.line << std::endl;
                }
            }
            break;

            break;
        case EXPRESSION_GROUPING:
            return Evaluate( interpreter, parser , ((GroupingExpr*)(parser->mem + expr.index))->expr );
            break;
        case EXPRESSION_CALL:
            {
                CallExpr& call = Cast<CallExpr>(parser->mem + expr.index);
                Value callee = Evaluate(interpreter , parser , call.callee);

                u32 i = call.args.index;
                u32 argCount = 0;
                while( Cast<Expr>(parser->mem + i).index != EXPRESSION_NULL ) {
                    argCount++;
                    i += sizeof(Expr);
                }
                Value args[Max<u32>(1,argCount)];

                u32 index = 0;
                i = call.args.index;
                while( Cast<Expr>(parser->mem + i).index != EXPRESSION_NULL ) {
                    Expr arg = Cast<Expr>(parser->mem + i);
                    args[index] = Evaluate(interpreter , parser , arg);
                    index++;
                    i += sizeof(Expr);
                }

                u32 calleeType = callee.type;
                if( calleeType == VALUE_NATIVE_FUNCTION ) {
                    return (Cast<native_function_t>(callee.mem))(args);
                }
                else if(callee.type == VALUE_NON_NATIVE_FUNCTION ) {

                    Stmt declIndex = Cast<Stmt>(callee.mem);
                    DeclFnStmt& declfn = Cast<DeclFnStmt>(parser->mem + declIndex.index);

                    Token params[argCount];
                    
                    i = 0;
                    u32 ptr = declfn.paramIndex;
                    while( Cast<Token>(parser->mem + ptr).type != TOKEN_EOF ) {
                        params[i] = Cast<Token>(parser->mem + ptr);
                        i++;
                        ptr += sizeof(Token);
                    }

                    Symbol returnValue;
                    returnValue.scope = 0;
                    returnValue.v.type = VALUE_NULL_LITERAL;
                    Token returnToken;
                    returnToken.text = "returnValue";
                    returnToken.lenght = 11;

                    auto vars = interpreter->symbolTable;
                    interpreter->symbolTable = {};
                    interpreter->symbolTable.Init(nullptr);
                    interpreter->symbolTable.CopyInit(&vars);
                    for(u32 i = 0; i < interpreter->symbolTable.cap; i++) {
                        if(interpreter->symbolTable.array[i].key.text) {
                            interpreter->symbolTable.array[i].value.scope != 0;
                            interpreter->symbolTable.array[i].key = Token{};
                        }
                    }

                    DefineVar(interpreter , returnValue ,returnToken );
                    for(u32 i = 0; i < argCount ; i++) {
                        Symbol arg;
                        arg.scope = 0;
                        arg.v = args[i];
                        DefineVar(interpreter, arg, params[i]);
                    }

                    Push<byte*>(interpreter , interpreter->returnAddress);
                    interpreter->returnAddress = interpreter->stackPtr;

                    Stmt pc = interpreter->program;
                    auto scope = interpreter->scope;
                    Execute(interpreter , parser, Stmt{(declfn.body.index + sizeof(ScopeStmt))} );
                    interpreter->program = pc;
                    interpreter->scope = scope;

                    u32 returnIndex = interpreter->symbolTable.Find(returnToken);
                    returnValue = interpreter->symbolTable.array[returnIndex].value;

                    interpreter->symbolTable.Free();
                    interpreter->symbolTable = vars;

                    return returnValue.v;
                }
                else {
                    std::cout << "ERROR: expressions must evaluate to functions to be called " << std::endl;
                }
            }
            break;
        
        default:
            break;
    }
}

void RegisterNativeFunction(InterpreterState* interpreter , native_function_t f , char* name) {

    u32 c = 0;
    while( name[c] != 0) c++;
    Token nameT{name, c, TOKEN_IDENTIFIER};
    Symbol func;
    func.scope = 0;
    func.v.type = VALUE_NATIVE_FUNCTION;
    Cast<native_function_t>(func.v.mem) = f;
    interpreter->symbolTable.Insert(nameT , func);
}

Value NativeFunction_f(Value* value) {
    Value ret;    
    Cast<double>(ret.mem) = Cast<double>(value[0].mem) + Cast<double>(value[1].mem);
    ret.type = VALUE_NUMBER_LITERAL;

    return ret;
}

Value NativeClock(Value* args) {
    auto timer = std::chrono::high_resolution_clock::now();
    auto t = std::chrono::time_point_cast<std::chrono::microseconds>(timer).time_since_epoch();
    Value ret;
    ret.type = VALUE_NUMBER_LITERAL;
    Cast<double>(ret.mem) =  t.count();
    return ret;
}



int main() {

    base = (byte*)mmap(nullptr, 2*MEGA_BYTE , PROT_WRITE | PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, 0,0);
    init_my_malloc(base, MEGA_BYTE);

    Parser parser;
    u32 s;
    parser.tokenBuffer.Init();
    parser.source = (char*)ReadFileTerminated("./test_h", nullptr, &s);
    parser.tokenizer.at = parser.source;
    parser.tokenizer.line = 0;
    parser.mem = base + MEGA_BYTE;

    InterpreterState interpreter;
    interpreter.symbolTable.Init(nullptr);
    interpreter.stack = (byte*)my_malloc_debug( 64 * KILO_BYTE );
    interpreter.stackPtr = interpreter.stack;
    interpreter.returnAddress = interpreter.stack;
    interpreter.scope = 0;
    
    Stmt program = Parse(&parser);

    RegisterNativeFunction(&interpreter , NativeFunction_f , "native_add");
    RegisterNativeFunction(&interpreter , NativeClock , "native_clock");

    Execute(&interpreter , &parser , program);
}