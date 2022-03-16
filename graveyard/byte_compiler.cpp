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

#define LOGASSERT(x , y ) if( !(x) ) {std::cout << #x << " " << y << " " << __FILE__ << " " << __LINE__ << std::endl; __builtin_trap(); }
#define ASSERT(x) if( !(x) ) {std::cout << #x << " triggered builtin trap in: " << __FILE__ << " " << __LINE__ << std::endl; __builtin_trap(); }

#ifdef __GNUC__
    #define _FORCE_INLINE __attribute__((always_inline))
#else
    #ifdef _MSC_VER
        #define _FORCE_INLINE __builtin_trap();
    #endif
#endif

template<typename T> T& Cast(void* mem) {
    return *((T*)mem);
}
template<typename T> T min(T t0 , T t1) {
    return t0 > t1 ? t1 : t0;
}
template<typename T> T max(T t0 , T t1) {
    return t0 > t1 ? t0 : t1;
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
            new_free_block->size = free_block->size - size - sizeof(MemoryBlockHeader);
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



byte* ReadFileTerminated(const char* fileName, byte* buffer,u32* size_) {

    byte* sourceString;
    FILE* file = nullptr;
    file = fopen(fileName ,"r");
    if(file) {

        fseek(file , 0, SEEK_END);
        u32 size = ftell(file);
        fseek(file ,0, SEEK_SET);
        *size_ = size;

        sourceString = buffer;
        if(buffer == nullptr) {
            sourceString = (byte*)my_malloc(size + 1);
        }
        fread(sourceString , size , 1 , file);
        sourceString[size] = 0;

        fclose(file);
    }

    return sourceString;
}   

u32 StringHash(const char* str , u32 c) {
    u32 hash = 7;
    for(u32 i = 0; i < c ; i++) {
        hash = hash * 31 + str[i];
    }
    return hash;
}

u32 StrLen(const char* str) {
    u32 r = 0;
    while( str[r] != 0 ) r++;
    return r;
}
bool StrCmp(const char* str0, const char* str1) {
    ASSERT(str0 != nullptr && str1 != nullptr);
    while( *str0 == *str1 && *str0 != 0 && *str1 != 0 ) {str0++;str1++;}
    return *str0 == *str1;
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
        array = (HashNode<K,V>*)my_malloc( sizeof(HashNode<K,V>) * 2);
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

            HashNode<K,V>* tmp = (HashNode<K,V>*)my_malloc(sizeof(HashNode<K,V>) * cap * 2);

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

            my_free(array);
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
        my_free(array);
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

    T& Back() {
        ASSERT(mem != nullptr && size != 0);
        return mem[size-1];
    }
    T& Front(){
        ASSERT(mem != nullptr && size != 0);
        return mem[0];
    }

    void SetCapacity(u32 capacity) {
        mem = (T*)my_malloc(capacity * sizeof(T));
        cap = capacity;
    }
    u32 PushBack(T e) {

        if( cap < size + 1) {
            T* tmp = (T*)my_malloc( sizeof(T) * (size + 1) * 2 );
            for(u32 i = 0; i < size * sizeof(T) ; i++) {
                ((byte*)tmp)[i] = ((byte*)mem)[i];
            }
            my_free(mem);
            mem = tmp;

            cap = (size + 1) * 2;
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
            T* tmp = (T*)my_malloc( sizeof(T) * (size + 1) * 2 );
            for(u32 i = 0; i < size * sizeof(T) ; i++) {
                ((byte*)tmp)[i] = ((byte*)mem)[i];
            }
            my_free(mem);
            mem = tmp;
            cap = size;
        }
        return size;
    }
    u32 ShrinkNoBranch() {
        T* tmp = (T*)my_malloc( sizeof(T) * (size + 1) * 2 );
        for(u32 i = 0; i < size * sizeof(T) ; i++) {
            ((byte*)tmp)[i] = ((byte*)mem)[i];
        }
        my_free(mem);
        mem = tmp;
        cap = size;
        return size;
    }

    _FORCE_INLINE T& operator[] (u32 i) {
        return mem[i];
    }
    void Free() {
        my_free(mem);
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
    TOKEN_EXCLAMATION_EQUALS,
    TOKEN_TILDE_EQUALS,

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
    TOKEN_SUB_SCRIPT_OPR,

    TOKEN_CHAR_LITERAL,
    TOKEN_STRING_LITERAL,
    TOKEN_NUMBER_LITERAL,
    TOKEN_BOOL_LITERAL,
    TOKEN_NULL_LITERAL,
    

    TOKEN_UNKNOWN,
    TOKEN_EOF,
    TOKEN_COUNT,
};


struct Token {
    char*       text;
    u64         lenght;
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
        case TOKEN_STRING_LITERAL:                  return "TOKEN_STRING_LITERAL";
        case TOKEN_NUMBER_LITERAL:                  return "TOKEN_NUMBER_LITERAL";
        case TOKEN_UNKNOWN:                         return "TOKEN_UNKNOWN";
        case TOKEN_EOF:                             return "TOKEN_EOF";
        case TOKEN_AMPERSAND_AMPERSAND:             return "TOKEN_AMPERSAND_AMPERSAND";
        case TOKEN_VERTICAL_BAR_VERTICAL_BAR:       return "TOKEN_VERTICAL_BAR_VERTICAL_BAR";
        case TOKEN_EQUALS_EQUALS:                   return "TOKEN_EQUALS_EQUALS";
        case TOKEN_BOOL_LITERAL:                    return "TOKEN_BOOL_LITERAL";
        case TOKEN_NULL_LITERAL:                    return "TOKEN_NULL_LITERAL";
        default:ASSERT(false);
            return nullptr;
            break;
    }
    ASSERT(false);
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

    if(t0.lenght != t1.lenght) {
        return false;
    }

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
            tokenizer->line += tokenizer->at[0] == '\n';
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
            while( tokenizer->at[0] && !(tokenizer->at[0] == '*' && tokenizer->at[1] == '/') ) ++tokenizer->at;
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
                    else {
                        token.type = TOKEN_RSHIFT;
                    }
                    break;
        case '<'    :if(tokenizer->at[0] == '=') {
                        token.type = TOKEN_LSHIFT_EQUALS;
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
    OP_XOR,         // xor      register , register

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
                    // printINT       0,R1 0  R2 num
                    // printUINT      0,R1 1  R2 num
                    // printF32       0,R1 2  R2 num
                    // printF64       0,R1 3  R2 num
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

    OP_CMP_I_LT,      // cmp      reg0,reg1, reg2   reg2 = (reg0 < reg1)
    OP_CMP_I_GT,      // cmp      reg0,reg1, reg2   reg2 = (reg0 > reg1)
    OP_CMP_I_E,       // cmp      reg0,reg1, reg2   reg2 = (reg0 == reg1)
    OP_CMP_U_LT,      // cmp      reg0,reg1, reg2   reg2 = (reg0 < reg1)
    OP_CMP_U_GT,      // cmp      reg0,reg1, reg2   reg2 = (reg0 > reg1)
    OP_CMP_SF_LT,     // cmp      reg0,reg1, reg2   reg2 = (reg0 < reg1)
    OP_CMP_SF_GT,     // cmp      reg0,reg1, reg2   reg2 = (reg0 > reg1)
    OP_CMP_SF_E,      // cmp      reg0,reg1, reg2   reg2 = (reg0 == reg1)
    OP_CMP_DF_LT,     // cmp      reg0,reg1, reg2   reg2 = (reg0 < reg1)
    OP_CMP_DF_GT,     // cmp      reg0,reg1, reg2   reg2 = (reg0 > reg1)
    OP_CMP_DF_E,      // cmp      reg0,reg1, reg2   reg2 = (reg0 == reg1)

    OP_LOAD_REL_RSP,    // load   reg , [0,1,2,3]->[8,16,32,64] , [rsp+im16]
    OP_STORE_REL_RSP,   // store  reg , [0,1,2,3]->[8,16,32,64] , [rsp+im16]
    OP_LOAD_REL_RBP,    // load   reg , [0,1,2,3]->[8,16,32,64] , [rbp+im16]
    OP_STORE_REL_RBP,   // store  reg , [0,1,2,3]->[8,16,32,64] , [rbp+im16]
    OP_LEA,             // lea    reg , [reg+reg<<sh+im16]

    OP_GET_PTR,         // mov r , im32
    OP_GET_STACK_PTR,   // get_stack_ptr  reg
    OP_GET_BASE_PTR,    // get_base_ptr   reg

    OP_CALL_R,          //  r   target
    OP_CALL,            //  im32 target
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
    REGISTER_COUNT,
};

enum RelatvieType : byte {
    RELATIVE_TO_CODE_INIT,
    RELATIVE_TO_CODE_MAIN,
    RELATIVE_TO_NUMBER_CONSTANTS,
    RELATIVE_TO_STRING_CONSTANTS,
    RELATIVE_TO_GLOBALS,
    RELATIVE_TO_STACK_BASE,

    RELATIIVE_COUNT,
};

enum TypeName {
    TYPE_NON,

    TYPE_STRUCTURE,
    TYPE_STRUCTURE_MEMBER,

    TYPE_PRIMARY_AUTO,
    TYPE_PRIMARY_VOID,
    TYPE_PRIMARY_CHAR,
    TYPE_PRIMARY_BOOL,
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

    TYPE_MODIFIER_POINTER,
    TYPE_MODIFIER_ARRAY,

    TYPE_COUNT,
};
struct StructName {
    u32     ptr;
    Token   name;
};
struct Parser {
    DynamicBufferSimple<Token>      tokenBuffer;
    DynamicBufferSimple<StructName> structs;
    Tokenizer                       tokenizer;
    char*                           source = nullptr;
    byte*                           mem = nullptr;
    u32                             exprAllocator = 64;
    bool                            error = false;
    u32                             block_name_count = 0;
};

struct ParserState {
    Tokenizer   tokenizer;
    u32         tokenBufferSize;
    u32         allocator;
    bool        error;
};
struct TypeExpr {
    u32 index;
};
struct FnTypeExpr : TypeExpr {
    u32         param_count = 0;
    u32         params = 0;
    TypeExpr    ret_t;
    TypeExpr    modifier;
};
struct ArrayTypeExpr : TypeExpr {
    u32 array_size;
};
struct StructMemberTypeExpr : TypeExpr {
    Token       name;
    TypeExpr    type;
};
struct StructTypeExpr : TypeExpr {
    Token       name;
    TypeExpr    members;
};


enum ExprType {
    EXPRESSION_NULL,

    EXPRESSION_IMMEDIATE,
    EXPRESSION_LITERAL,
    EXPRESSION_GROUP,
    EXPRESSION_UNARY,
    EXPRESSION_BINARY,

    EXPRESSION_VARIABLE_LOAD,
    EXPRESSION_MEMORY_LOAD,
    EXPRESSION_VARIABLE_ASSIGNMENT,
    EXPRESSION_MEMORY_ASSIGNMENT,

    EXPRESSION_MEMORY_DEREF,
    EXPRESSION_ADDRESS_OF,

    EXPRESSION_GET,
    EXPRESSION_PEEL_TYPE,
    EXPRESSION_MEM_COPY,

    EXPRESSION_CALL,
    EXPRESSION_CONVERSION,
    
    EXPRESSION_LOAD,
    EXPRESSION_STORE,
    
    EXPRESSION_GET_STACK_BASE,

    EXPRESSION_COUNT,
};

struct Value {
    byte mem[8];
    u8 type;
};
struct Expr {
    u32 index;
};
struct Variable {
    Token           name;
    TypeExpr        type;
    u32             address;
    u32             scope;
    u32             g_index = ~u32(0);
    Expr            val;
};
struct LiteralExpr : Expr {
    Token   literal;
    u32     g_index;
    u32     block_name;
};
struct GetStackBase : Expr {
    u32 g_index;
};
struct ImmediateExpr : Expr {
    Value   v;
    u32     g_index;
    u32     block_name;
};
struct GroupExpr : Expr {
    Expr expr;
};
struct UnaryExpr : Expr {
    TokenType   opr;
    Expr        primaryExpr;
    u32         g_index;
    u32         block_name;
};
struct BinaryExpr : Expr {
    Expr        left;
    Expr        right;
    TokenType   opr;
    u32         g_index;
    u32         block_name;
};
struct PeelTypeExpr : Expr {
    Expr expr;
};
struct VariableGetExpr : Expr {
    Token   name;
    Expr    prev;
    u32     g_index;
    u32     g_index_copy;
    u32     block_name;
};
struct VariableLoadExpr : Expr {
    Variable    var;
    byte        relative;
    u32         g_index;
    u32         block_name;
};
struct VariableAssignmentExpr : Expr {
    Variable    var;
    Expr        value;
    u32         g_index;
    u32         block_name;
};
struct MemDeRefExpr : Expr {
    Expr address;
};
struct MemoryLoadExpr : Expr {
    Expr    address;
    u32     g_index;
    u32     g_index2;
    bool    de_ref;
    u32     block_name;
};
struct MemoryStoreExpr : Expr {
    Expr    address;
    Expr    value;
    u32     g_index;
    u32     g_index2;
    bool    de_ref;
    u32     block_name;
};
struct MemCopyExpr : Expr {
    Expr    src;
    Expr    dst;
    u32     size;
    u32     g_index_i;
    u32     g_index_res;
    u32     g_index_cmp;
    u32     block_name;
};

struct CallExpr : Expr {
    Expr    calleeExpr;
    Expr    args;
    u32*    liveRegisters;
    u32     g_index;
    u32     block_name;
};
struct ConversionExpr : Expr {
    Expr        from;
    TypeExpr    type;
    u32         g_index;
    u32         block_name;
};

struct StoreExprLiteral : Expr {
    Expr    expr;
    u32     size;
    u32     address;
    u32     block_name;
    byte    relative;
};
struct LoadExprLiteral : Expr {
    u32      g_index;
    u32      size;
    u32      address;
    u32      block_name;
    TypeExpr type;
    byte    relative;
};

struct RegisterSymbol {
    Token   name;
    u32     g_index;
    u32     possibleCFpaths;
};

struct ExprList {
    Expr      expr     = Expr{0};
    ExprList* next     = nullptr;
};
struct VirtualRegister {
    ExprList*   list;
    u32         name;
    u32         firstRef;
    u32         LastRef;
    u32         usageCount = 0;
    bool        var;
    bool        parameter;
};
struct AdjacencyList {
    u32             index = 0;
    AdjacencyList*  next = nullptr;
};
struct AdjacencyGraph {
    AdjacencyList** adj = nullptr;
    u32             vertexCount = 0;
};
struct RegisterPair {
    u32 g_index;
    RegisterName reg;
};
struct RegisterAllocation {
    u32*                                    colors = nullptr;
    DynamicBufferSimple<RegisterPair>       preAssigned;
    DynamicBufferSimple<VirtualRegister>    virtualRegisters;
    DynamicBufferSimple<Variable>           params;
    AdjacencyGraph                          graph;
    u32                                     virtual_register_count = 0;
};

struct Function {
    Token              name;
    u32                address = 0;
    u32                frameSize = 0;
    u32                currentFrameSize = 0;
    u32                scratchSize = 0;
    u32                scratchAddress = 0;
    u32                paramCount = 0;
    u32                params;
    u32                retJmpAddress;
    TypeExpr           ret_t;
    RegisterAllocation reg;
};
enum StatementType {
    STATEMENT_NON,

    STATEMENT_EXPRESSION,
    STATEMENT_PRINT,
    STATEMENT_ENTRY_POINT,
    STATEMENT_FUNCTION_ENTRY,
    STATEMENT_FUNCTION_RET,
    STATEMENT_BRANCH,
    STATEMENT_FOR_LOOP,
    STATEMENT_REPEAT,
    STATEMENT_RRV_ASSIGN,

    STATEMENT_VAR_DECL,
    STATEMENT_ABORT,
    STATEMENT_SKIP,

    STATEMENT_COUNT,
};

struct Stmt {
    u32 index;
};
struct ExprStmt : Stmt {
    Expr expr;
};
struct PrintStmt : Stmt {
    // null terminated Expr string
};
struct FuncFrame : Stmt {
    u32 function;
    u32 g_index;
    u32 expressions;
};
struct SkipStmt : Stmt {
    Stmt target;
    Stmt end;
};
struct BranchStmt : Stmt {
    Expr    cond;
    Stmt    thenBranch;
    Stmt    elseBranch;
    Stmt    end;
    u32     g_index;
};
struct ForStmt : Stmt {
    Expr    cond;
    Stmt    init;
    Stmt    inc;
    Stmt    body;
    Stmt    end;
    u32     g_index;
    u32     g_index2;
};
struct RetAssignStmt : Stmt {
    Expr retExpr;
};
struct VarDeclStm : Stmt {
    bool init = false;
    bool param = false;
    bool preColor = false;
    bool hidden = false;
    RegisterName reg;
    Variable var;
};

struct RelativeReference {
    RelativeReference(RelatvieType type_, u32 codeOffset_, u64 value_, byte count_);
    RelatvieType    type;
    u32             codeOffset;
    byte            value[8] = {0,0,0,0,0,0,0,0};
    byte            count;
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

struct CodeSegment {
    DynamicBufferSimple<byte> buffer;
    DynamicBufferSimple<RelativeReference> refs;
};

struct Chunk {
    ExecutableHeader header;
    DynamicBufferSimple<byte> numberConstants;
    DynamicBufferSimple<char> stringConstants;
    DynamicBufferSimple<byte> globals;

    u64 entryPoint = ~(u64(0));
    CodeSegment main;
    CodeSegment init;
};

struct FunctionCall {
    Expr call;
    u32 i;
};

struct CompileConfig {
    const char* output_file;
    const char* input_file;
    
    u64 mem_cap;
    u64 parser_mem;
    u64 comp_mem;
    u64 comp_scratch_mem;

    u64 expr_allocator;
    u64 comp_allocator;
    u64 comp_fn_allocator;
    u64 comp_misc_allocator;

    bool dump_dissassembly;
    bool optimization_dead_code_elimination;
    bool optimization_constant_propogation;
};
struct Compiler {
    DynamicBufferSimple<Variable>       stack;
    DynamicBufferSimple<u32>            hiddenParams;
    DynamicBufferSimple<RegisterSymbol> registerVariable;
    DynamicBufferSimple<FunctionCall>   calls;
    
    u64                                 falseStrConstantOffset;
    u64                                 trueStrConstantOffset;

    CompileConfig*                      config;
    byte*                               mem;
    byte*                               scratchMem; 
    u32                                 allocator;
    u32                                 fnAllocator;
    u32                                 miscAllocator;
    u32                                 currentI;
    Stmt                                entryPoint;

    u32                                 scope;
    u32                                 currentFunction;
    bool                                panic;
    bool                                error;
};

template<typename T> _FORCE_INLINE T* Allocate(Parser* parser) {
    u32 i = parser->exprAllocator;
    parser->exprAllocator += sizeof(T);
    return (T*)(parser->mem + i);
}
template<typename T> _FORCE_INLINE T* AllocateF(Compiler* compiler) {
    u32 i = compiler->fnAllocator;
    compiler->fnAllocator += sizeof(T);
    return (T*)(compiler->mem + i);
}
template<typename T> _FORCE_INLINE T* Allocate(Compiler* compiler) {
    u32 i = compiler->allocator;
    compiler->allocator += sizeof(T);
    return (T*)(compiler->mem + i);
}
template<typename T> _FORCE_INLINE T* AllocateMisc(Compiler* compiler) {
    u32 i = compiler->miscAllocator;
    compiler->miscAllocator += sizeof(T);
    return (T*)(compiler->mem + i);
}
template<typename T> T* Get(Parser* parser , u32 index) {
    return (T*)(parser->mem + index);
}
template<typename T> T* Get(Compiler* compiler , u32 index) {
    return (T*)(compiler->mem + index);
}

ParserState SaveParserState(Parser* parser) {
    return  ParserState{parser->tokenizer, parser->tokenBuffer.size, parser->exprAllocator, parser->error};
}
void RestoreParserState(Parser* parser, ParserState state) {
    parser->tokenizer = state.tokenizer;
    parser->tokenBuffer.size = state.tokenBufferSize;
    parser->exprAllocator = state.allocator;
    parser->error = state.error;
}

f64 GetF64FromToken(Token l) {
    char str[l.lenght+1]{0};
    for(u32 i = 0; i < l.lenght ; i++) {
        str[i] = l.text[i];
    }
    return strtod(str,nullptr);
}
f32 GetF32FromToken(Token t) {
    char str[t.lenght+1]{0};
    for(u32 i = 0; i < t.lenght ; i++) {
        str[i] = t.text[i];
    }
    return (f32)strtod(str,nullptr);
}
bool GetBoolFromToken(Token t) {
    return TokenEquals(t , "true");
}
i64 GetIntFromToken(Token t) {
    i64 r = 0;
    u32 e = 1;
    for(i32 i = t.lenght-1; i > -1 ; i--) {
        r += (t.text[i] - '0') * e;
        e *= 10;
    }
    return r;
}
u64 GetUIntFromToken(Token t) {
    u64 r = 0;
    u32 e = 1;
    for(i32 i = t.lenght-1; i > -1 ; i--) {
        r += (t.text[i] - '0') * e;
        e *= 10;
    }
    return r;
}

RelativeReference::RelativeReference(RelatvieType type_, u32 codeOffset_, u64 value_, byte count_) :type(type_) ,codeOffset(codeOffset_),count(count_) {
    for(u32 i = 0; i < count ;i++) {
        value[i] = ((byte*)&value_)[i];
    }
}

const char* GetExprTypeStr(ExprType t) {
    switch(t) {
        case EXPRESSION_NULL:               return "EXPRESSION_NULL";
        case EXPRESSION_LITERAL:            return "EXPRESSION_LITERAL";
        case EXPRESSION_GROUP:              return "EXPRESSION_GROUP";
        case EXPRESSION_UNARY:              return "EXPRESSION_UNARY";
        case EXPRESSION_BINARY:             return "EXPRESSION_BINARY";
        case EXPRESSION_VARIABLE_LOAD:      return "EXPRESSION_VARIABLE_LOAD";
        case EXPRESSION_MEMORY_LOAD:        return "EXPRESSION_MEMORY_LOAD";
        case EXPRESSION_VARIABLE_ASSIGNMENT:return "EXPRESSION_VARIABLE_ASSIGNMENT";
        case EXPRESSION_MEMORY_ASSIGNMENT:  return "EXPRESSION_MEMORY_ASSIGNMENT";
        case EXPRESSION_MEMORY_DEREF:       return "EXPRESSION_MEMORY_DEREF";
        case EXPRESSION_ADDRESS_OF:         return "EXPRESSION_ADDRESS_OF";
        case EXPRESSION_GET:                return "EXPRESSION_GET";
        case EXPRESSION_PEEL_TYPE:          return "EXPRESSION_PEEL_TYPE";
        case EXPRESSION_MEM_COPY:           return "EXPRESSION_MEM_COPY";
        case EXPRESSION_CALL:               return "EXPRESSION_CALL";
        case EXPRESSION_CONVERSION:         return "EXPRESSION_CONVERSION";
        case EXPRESSION_LOAD:               return "EXPRESSION_LOAD";
        case EXPRESSION_STORE:              return "EXPRESSION_STORE";
        default:{ASSERT(false);             return "UNKOWN EXPRESSION";}
    }
    ASSERT(false);
    return nullptr;
}
Expr Convert(Parser* parser , void* node) {
    return Expr {u32(  (u64)node - (u64)parser->mem)};
}

void InsertExpr(Compiler* compiler,u32 function, u32 g_index, Expr expr) {
    
    Function* f = Get<Function>(compiler, function);

    ExprList* i = nullptr;
    ExprList* k = nullptr;

    if( f->reg.virtualRegisters[g_index].list != nullptr ) {
        for(i = f->reg.virtualRegisters[g_index].list,k = i; i != nullptr ; i = i->next) {
            k = i;
        }
        k->next = (ExprList*)my_malloc( sizeof(ExprList) );
        k->next->expr = expr;
        k->next->next = nullptr;
    }
    else {
        f->reg.virtualRegisters[g_index].list = (ExprList*)my_malloc( sizeof(ExprList) );
        f->reg.virtualRegisters[g_index].list->expr = expr;
        f->reg.virtualRegisters[g_index].list->next = nullptr;
    }
}

u32* GreedyGraphColor(AdjacencyGraph* g, DynamicBufferSimple<RegisterPair>* preColored) {

    static_assert( sizeof(bool) == 1);
    u32* colors = (u32*)my_malloc(g->vertexCount * sizeof(u32));
    MemSet(colors, ~u32(0), g->vertexCount * sizeof(u32));

    bool available[g->vertexCount];
    MemSet(available, true, g->vertexCount);
    for(u32 i = 0; i < (*preColored).size ; i++) {
        colors[ (*preColored)[i].g_index ] = (*preColored)[i].reg;
    }

    /*for(u32 i = 0; i < g->vertexCount ; i++) {
        std::cout << colors[i] << std::endl;
    }*/

    ASSERT(g->adj != nullptr);
    for(u32 i = 0; i < g->vertexCount; i++) {

        bool preAssigned = false;
        for(u32 k = 0; k < preColored->size; k++) {
            if((*preColored)[k].g_index == i) {
                preAssigned = true;
                break;
            }
        }
        if(preAssigned) {
            continue;
        }
        
        
        for(AdjacencyList* it = g->adj[i]; it != nullptr; it = it->next) {
            if(colors[it->index] != ~u32(0)) {
                available[colors[it->index]] = false;
            }
        }
        
        /*for(u32 k = 0; k < g->vertexCount; k++) {
            std::cout << available[k] << " ";
        }
        std::cout << std::endl;*/

        for(u32 k = 0; k < g->vertexCount; k++) {
            if(available[k]) {
                colors[i] = k;
                break;
            }
        }

        MemSet(available, true, g->vertexCount);
    }

    return colors;
}
u32* GreedyColoring(AdjacencyGraph* g, DynamicBufferSimple<RegisterPair>* preColored) {
    AdjacencyList* l = nullptr;
    
    u32* colors = (u32*)my_malloc(g->vertexCount * sizeof(u32) );
    MemSet(colors, ~u8(0) , g->vertexCount * sizeof(u32));

    static_assert( sizeof(bool) == 1);
    bool avail[g->vertexCount]{};
    MemSet(avail, true, g->vertexCount);


    for(u32 j = 0; j < (*preColored).size ; j++) {
        colors[ (*preColored)[j].g_index ] = (*preColored)[j].reg;
    }
    for(AdjacencyList* it = g->adj[0]; it != nullptr; it = it->next) {
        bool found = false;
        for(u32 i = 0; i < preColored->size; i++) {
            if( (*preColored)[i].g_index == it->index ) {
                found = true;
                break;
            }
        }
        if(found) {
            avail[(*preColored)[it->index].reg] = false;
        }
    }

    for(u32 i = 0; i < g->vertexCount; i++) {
        if(avail[i]) {
            colors[0] = i;
            break;
        }
    }

    u32 k = 0;
    for(u32 i = 1; i < g->vertexCount; i++) {

        bool found = false;
        for(u32 j = 0; j < preColored->size; j++) {
            if( (*preColored)[j].g_index == i) {
                found = true;
                break;
            }
        }
        if(found) {
            continue;
        }

        for(AdjacencyList* it = g->adj[i]; it != nullptr; it = it->next) {
            found = false;
            for(u32 j = 0; j < preColored->size; j++) {
                if( (*preColored)[j].g_index == it->index ) {
                    found = true;
                    break;
                }
            }
            if(found) {
                avail[(*preColored)[it->index].reg] = false;
            }
        }
        l = g->adj[i];
        while(l != nullptr) {
            k = l->index;
            if(colors[k] != ~u32(0)) {
                avail[colors[k]] = 0;
            }
            l = l->next;
        }

        {
            u32 j = 0;
            for(; j < g->vertexCount; j++) {
                if(avail[j] == 1) {
                    colors[i] = j;
                    break;
                }
            }
            colors[i] = j;
        }

        for(u32 j = 0; j < g->vertexCount; j++) {
            avail[j] = 1;
        }
    }

    return colors;
}

void PreAssignRegister(RegisterAllocation* reg, u32 g_index, RegisterName r) {
    reg->preAssigned.PushBack(RegisterPair{g_index,r});
}

void InsertVertex(AdjacencyGraph* graph, u32 z) {

    if(graph->vertexCount < z+1) {

        AdjacencyList** tmp = (AdjacencyList**)my_malloc( (graph->vertexCount + 1) * sizeof(AdjacencyList*) );
        MemCpy(tmp, graph->adj, sizeof(AdjacencyList*) * graph->vertexCount);
        
        my_free(graph->adj);
        graph->adj = tmp;
        graph->vertexCount++;
        graph->adj[z] = (AdjacencyList*)my_malloc(sizeof(AdjacencyList));
        graph->adj[z]->index = z;
        graph->adj[z]->next = nullptr;
    }
}

u32 AllocateVirtualRegister(Compiler* compiler, u32 function,  u32 name, bool var, bool parameter) {
    
    Function* f = Get<Function>(compiler, function);
    
    for(u32 i = 0; i < f->reg.virtualRegisters.size ; i++) {
        if( name == f->reg.virtualRegisters[i].name ) {
            return i;
        }
    }
    VirtualRegister reg;
    reg.name = name;
    reg.firstRef = compiler->currentI++;
    reg.LastRef = reg.firstRef;
    reg.var = var;
    reg.parameter = parameter;
    reg.list = nullptr;

    u32 g_index = f->reg.virtualRegisters.PushBack(reg);
    InsertVertex(&f->reg.graph, g_index);

    return g_index;
}


void InsertEdge(AdjacencyGraph* graph, u32 z , u32 w) {

    if(z == w) return;
    AdjacencyList* l = nullptr;

    u32 max = z > w ? z : w;
    max++;

    if(graph->vertexCount == 0) {
        graph->adj = (AdjacencyList**)my_malloc( max * sizeof(AdjacencyList*) );
        graph->vertexCount = max;
        MemSet(graph->adj, 0, max * sizeof(AdjacencyList*));
    }
    else if(graph->vertexCount < max ) {

        AdjacencyList** tmp = (AdjacencyList**)my_malloc( max * sizeof(AdjacencyList*) );
        MemSet(tmp, 0, max * sizeof(AdjacencyList*));
        for(u32 i = 0; i < graph->vertexCount; i++) {
            tmp[i] = graph->adj[i];
        
        }
        MemSet(graph->adj, 0, graph->vertexCount * sizeof(AdjacencyList*));
        my_free(graph->adj);
        graph->adj = tmp;
        graph->vertexCount = max;
    }

    l = graph->adj[z];
    while( l != nullptr && (l->index != w) ) {
        l = l->next;
    }

    if(l == nullptr) {
        l = (AdjacencyList*)my_malloc( sizeof(AdjacencyList) );
        *l = {};
        l->index = w;
        l->next = graph->adj[z];
        graph->adj[z] = l;
    }

    l = graph->adj[w];

    while( l != nullptr && l->index != z) {
        l = l->next;
    }

    if( l == nullptr) {
        l = (AdjacencyList*)my_malloc( sizeof(AdjacencyList) );
        *l = {};
        l->index = z;
        l->next = graph->adj[w];
        graph->adj[w] = l;
    }
}

void PrintGraph(AdjacencyGraph* g) {

    for(u32 i = 0; i < g->vertexCount ; i++) {

        std::cout << "vertex " <<  i << "    ";
        for(AdjacencyList* k = g->adj[i]; k != nullptr; k = k->next) {
            std::cout << k->index << " ";
        }
        std::cout << std::endl;
    }
}


u32 FindVariable(DynamicBufferSimple<Variable>* vars, Token name) {
    for(i32 i = vars->size-1; i > -1; i--) {
        if( TokensEquals( (*vars)[i].name, name)) {
            return i;
        }
    }
    return ~(u32(0));
}

Token PreviousToken(Parser* parser) {
    return parser->tokenBuffer.mem[parser->tokenBuffer.size - 1];
}
Token NextToken(Parser* parser) {
    u32 i = parser->tokenBuffer.PushBack( GetToken(&parser->tokenizer) );
    return parser->tokenBuffer[i];
}

Token PeekToken(Tokenizer peek) {
    return GetToken(&peek);
}

bool Check(Parser* parser , TokenType type) {
    if(!parser->tokenizer.at[0]) return type == TOKEN_EOF;

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
bool CheckPeek(Tokenizer tokenizer , TokenType type) {
    if(!tokenizer.at[0]) return type == TOKEN_EOF;

    Token nextToken = GetToken(&tokenizer);
    return (nextToken.type == type);
}
bool MatchPeek(Tokenizer* tokenizer, TokenType* types , u32 count) {
    for(u32 i = 0; i < count ;i++) {
        if(CheckPeek(*tokenizer , types[i])) {
            GetToken(tokenizer);
            return true;
        }
    }
    return false;
}

void ExpectToken(Parser* parser , TokenType e) {

    Token token = NextToken(parser);

    if(token.type != e) {
        std::cout << "ERROR: expected: " << GetTokenStr(e);
        (std::cout << " got: ").write(token.text, token.lenght) << " at line: " << parser->tokenizer.line << std::endl;
        parser->error = true;
    }
}
void Expect(Compiler* compiler, Parser* parser, bool cond , const char* errMsg) {
    if(!cond) {
        std::cout << "ERROR: " << errMsg << " at line: " << parser->tokenizer.line << std::endl;
        compiler->error = true;
    }
}


void PrintTypeModifierExpression(byte* mem, TypeExpr modifier) {

    
    while( Cast<TypeName>(mem + modifier.index) != TYPE_NON ) {

        switch( Cast<TypeName>(mem + modifier.index) ) {
        case TYPE_MODIFIER_POINTER:
            std::cout << "*";
            modifier.index += sizeof(TypeName);
            break;
        case TYPE_MODIFIER_ARRAY:
            std::cout << "[" << Cast<ArrayTypeExpr>(mem + modifier.index).array_size << "]";
            modifier.index += sizeof(ArrayTypeExpr);
            break;
        default:
            {
                std::cout << "unkonw type modifier" << std::endl;
                ASSERT(false);
            }
        }
    }
}
void PrintTypeExpression(byte* strucures, byte* mem, TypeExpr expr) {

    TypeName n = Cast<TypeName>(mem + expr.index);
    switch(n) {
    case TYPE_PRIMARY_UINT8:
        std::cout << "u8 ";
        break;
    case TYPE_PRIMARY_UINT16:
        std::cout << "u16 ";
        break;
    case TYPE_PRIMARY_UINT32:
        std::cout << "u32 ";
        break;
    case TYPE_PRIMARY_UINT64:
        std::cout << "u64 ";
        break;

    case TYPE_PRIMARY_INT8:
        std::cout << "i8 ";
        break;
    case TYPE_PRIMARY_INT16:
        std::cout << "i16 ";
        break;
    case TYPE_PRIMARY_INT32:
        std::cout << "i32 ";
        break;
    case TYPE_PRIMARY_INT64:
        std::cout << "i64 ";
        break;

    case TYPE_PRIMARY_F32:
        std::cout << "f32 ";
        break;
    case TYPE_PRIMARY_F64:
        std::cout << "f64 ";
        break;

    case TYPE_PRIMARY_CHAR:
        std::cout << "char ";
        break;
    case TYPE_PRIMARY_VOID:
        std::cout << "void ";
        break;
    case TYPE_PRIMARY_BOOL:
        std::cout << "bool ";
        break;

    case TYPE_PRIMARY_NATIVE_FN:
    case TYPE_PRIMARY_FN:
        {
            FnTypeExpr fn = Cast<FnTypeExpr>(mem + expr.index);
            if(fn.modifier.index != TYPE_NON) {
                std::cout << "(fn(";
                for(u32 i = 0 ; i < fn.param_count; i++) {
                    PrintTypeExpression(strucures, mem, Cast<TypeExpr>(mem + fn.params + i * sizeof(TypeExpr) ) );
                    if(i != fn.param_count-1) {
                        std::cout << ", ";
                    }
                }

                std::cout << ") -> ";
                PrintTypeExpression(strucures, mem, fn.ret_t);
                std::cout << ")";
                PrintTypeModifierExpression(mem, fn.modifier);
            }
            else {
                std::cout << "fn(";
                for(u32 i = 0 ; i < fn.param_count; i++) {
                    PrintTypeExpression(strucures, mem, Cast<TypeExpr>(mem + fn.params + i  * sizeof(TypeExpr)) );
                    if(i != fn.param_count-1) {
                        std::cout << ", ";
                    }
                }

                std::cout << ") -> ";
                PrintTypeExpression(strucures, mem, fn.ret_t);
            }
            return;
        }
        break;
    default:
        {
            StructTypeExpr st = Cast<StructTypeExpr>(strucures + n);
            std::cout.write(st.name.text, st.name.lenght);
        }
        break;
    }

    expr.index += sizeof(TypeName);
    PrintTypeModifierExpression(mem, expr);
}

void PrintStruct(byte* structures,byte* mem, TypeExpr ptr) {

    char* name = Cast<StructTypeExpr>(mem + ptr.index).name.text;
    u32 len = Cast<StructTypeExpr>(mem + ptr.index).name.lenght;
    (std::cout << "struct ").write(name, len) << " {" << std::endl;

    TypeExpr members = Cast<StructTypeExpr>(mem + ptr.index).members;

    while(Cast<TypeExpr>(mem + members.index).index != 0) {

        TypeExpr member = Cast<TypeExpr>(mem + members.index);
        TypeExpr type = Cast<StructMemberTypeExpr>(mem + member.index).type;
        Token name = Cast<StructMemberTypeExpr>(mem + member.index).name;
        std::cout << "\t";
        PrintTypeExpression(structures, mem, type);
        (std::cout << " ").write(name.text, name.lenght) << ";" << std::endl;
        members.index += sizeof(TypeExpr);
    }

    std::cout << "}" << std::endl;
}

TypeName GetLastType(byte* mem, TypeExpr expr) {

    TypeName last = Cast<TypeName>(mem + expr.index);
    while( Cast<TypeName>(mem + expr.index) != TYPE_NON ) {

        last = Cast<TypeName>(mem + expr.index);

        switch( Cast<TypeName>(mem + expr.index) ) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            expr = Cast<FnTypeExpr>(mem + expr.index).modifier;
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
TypeExpr GetNthType(byte* mem, TypeExpr expr, u32 nth) {

    TypeExpr ptrs[nth+1]{};

    while( Cast<TypeName>(mem + expr.index) != TYPE_NON ) {

        for(i32 i = nth; i > 0 ; i--) {
            ptrs[i] = ptrs[i-1];
        }
        ptrs[0] = expr;


        TypeName type = Cast<TypeName>(mem + expr.index);
        switch(type) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            expr = Cast<FnTypeExpr>(mem + expr.index).modifier;
            break;
        case TYPE_MODIFIER_ARRAY:
            expr.index += sizeof(ArrayTypeExpr);
            break;
        default:
            expr.index += sizeof(TypeName);
            break;
        }

    }

    ASSERT(Cast<TypeName>(mem + ptrs[nth].index) != TYPE_NON);
    return ptrs[nth];
}

bool IsType(Parser* parser, Tokenizer peek) {

    Token t = GetToken(&peek);

    switch(t.type) {
    case TOKEN_IDENTIFIER:
        if( TokenEquals(t, "u8") ) {
            return true;
        }
        else if( TokenEquals(t, "u16") ) {
            return true;
        }
        else if( TokenEquals(t, "u32") ) {
            return true;
        }
        else if( TokenEquals(t, "u64") ) {
            return true;
        }
        else if( TokenEquals(t, "i8") ) {
            return true;
        }
        else if( TokenEquals(t, "i16") ) {
            return true;
        }
        else if( TokenEquals(t, "i32") ) {
            return true;
        }
        else if( TokenEquals(t, "i64") ) {
            return true;
        }
        else if( TokenEquals(t, "void") ) {
            return true;
        }
        else if( TokenEquals(t, "bool") ) {
            return true;
        }
        else if( TokenEquals(t, "char") ) {
            return true;
        }
        else if( TokenEquals(t, "f32") ) {
            return true;
        }
        else if( TokenEquals(t, "f64") ) {
            return true;
        }
        else {
            for(u32 i = 0; i < parser->structs.size; i++) {
                if(TokensEquals(t, parser->structs[i].name)) {
                    return true;
                }
            }
        }
        break;
    case TOKEN_KEYWORD_FN:
        return true;
        break;
    case TOKEN_OPEN_PAREN:
        return IsType(parser, peek);
        break;
    case TOKEN_KEYWORD_AUTO:return true;
    default: return false;
    }

    return false;
}
TypeName GetNumberType(Token t) {

    if(t.text[t.lenght-1] == 'f') return TYPE_PRIMARY_F32;

    for(u32 i = 0; i < t.lenght ; i++) {
        if(t.text[i] == '.' ) {
            return TYPE_PRIMARY_F64;
        }
    }

    return TYPE_PRIMARY_INT64;
}

u32 GetTypeExprSize(void* mem, TypeExpr expr) {

    u32 begin = expr.index;
    while( Cast<TypeName>((byte*)mem + expr.index) != TYPE_NON ) {
        switch( Cast<TypeName>((byte*)mem + expr.index) ) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            expr = Cast<FnTypeExpr>((byte*)mem + expr.index).modifier;
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

TypeExpr CpyTypeExpr(byte* dst, TypeExpr dstExpr, byte* src, TypeExpr srcExpr) {

    TypeName t = Cast<TypeName>(src + srcExpr.index);
    while( t != TYPE_NON) {

        switch (t){
        case TYPE_PRIMARY_FN:
            {
                FnTypeExpr* node = (FnTypeExpr*)(src + srcExpr.index);
                FnTypeExpr* dstNode = (FnTypeExpr*)(dst + dstExpr.index);
                MemCpy(dst + dstExpr.index, src + srcExpr.index, sizeof(FnTypeExpr));
                dstExpr.index += sizeof(FnTypeExpr);

                if(node->param_count != 0) {
                    ASSERT(node->params != 0);

                    dstNode->params = dstExpr.index;
                    dstExpr.index += node->param_count * sizeof(TypeExpr);
                    for(u32 i = 0; i < node->param_count; i++) {
                        Cast<TypeExpr>(dst + dstNode->params + i) = dstExpr;
                        dstExpr = CpyTypeExpr(dst, dstExpr, src, Cast<TypeExpr>(src + node->params + i));
                    }
                }

                dstNode->ret_t = dstExpr;
                dstExpr = CpyTypeExpr(dst, dstExpr, src, node->ret_t);

                dstNode->modifier = dstExpr;
                srcExpr = node->modifier;

            }break;
        case TYPE_MODIFIER_ARRAY:
            MemCpy(dst + dstExpr.index, src + srcExpr.index, sizeof(ArrayTypeExpr));
            srcExpr.index += sizeof(ArrayTypeExpr);
            dstExpr.index += sizeof(ArrayTypeExpr);
            break;
        default:
            MemCpy(dst + dstExpr.index, src + srcExpr.index, sizeof(TypeName));
            srcExpr.index += sizeof(TypeName);
            dstExpr.index += sizeof(TypeName);
            break;
        }
        t = Cast<TypeName>(src + srcExpr.index);
    }
    
    Cast<TypeName>(dst + dstExpr.index) = TYPE_NON;
    dstExpr.index += sizeof(TypeName);
    return dstExpr;
}

/*
TypeExpr CpyTypeExpr(byte* dst, TypeExpr dstExpr, byte* src, TypeExpr srcExpr) {

    u32 size = GetTypeExprSize(src, srcExpr);
    MemCpy(dst + dstExpr.index , src + srcExpr.index , size);

    while( Cast<TypeName>(src + srcExpr.index) != TYPE_NON ) {
        switch( Cast<TypeName>(src + srcExpr.index) ) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            {
                u32 srcParam = Cast<FnTypeExpr>(src + srcExpr.index).params;
                u32 offsetParam = Cast<FnTypeExpr>(src + srcExpr.index).params - srcExpr.index;
                
                Cast<FnTypeExpr>(dst + dstExpr.index).params = Cast<FnTypeExpr>(src + srcExpr.index).params == 0 ? 0 : dstExpr.index + offsetParam;;
                u32 dstParam = Cast<FnTypeExpr>(dst + dstExpr.index).params;
                for(u32 i = 0; i < Cast<FnTypeExpr>(src + srcExpr.index).param_count ; i++ ) {
                    Cast<TypeExpr>(dst + dstParam + i * sizeof(TypeExpr) ).index = (Cast<TypeExpr>(src + srcParam + i * sizeof(TypeExpr) ).index - srcExpr.index) + dstExpr.index;
                }
                u32 offsetRet = Cast<FnTypeExpr>(src + srcExpr.index).ret_t.index - srcExpr.index;
                u32 offsetMod = Cast<FnTypeExpr>(src + srcExpr.index).modifier.index - srcExpr.index;
                

                Cast<FnTypeExpr>(dst + dstExpr.index).ret_t.index = dstExpr.index + offsetRet;
                Cast<FnTypeExpr>(dst + dstExpr.index).modifier.index = dstExpr.index + offsetMod;

                dstExpr = Cast<FnTypeExpr>(dst + dstExpr.index).modifier;
                srcExpr = Cast<FnTypeExpr>(src + srcExpr.index).modifier;
            }
            break;
        case TYPE_MODIFIER_ARRAY:
            srcExpr.index += sizeof(ArrayTypeExpr);
            dstExpr.index += sizeof(ArrayTypeExpr);
            break;
        default:
            srcExpr.index += sizeof(TypeName);
            dstExpr.index += sizeof(TypeName);
            break;
        }
    }

    dstExpr.index += sizeof(TypeName);
    return dstExpr;
}
*/

_FORCE_INLINE bool IsIntegral(byte* mem, TypeExpr expr) {
    TypeName t = GetLastType(mem , expr);
    return t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_UINT64;
}
bool TypesEqual(byte* structMem0, byte* structMem1, byte* mem0, byte* mem1, TypeExpr e0, TypeExpr e1) {

    TypeName t0 = Cast<TypeName>(mem0 + e0.index);
    TypeName t1 = Cast<TypeName>(mem1 + e1.index);
    if(t0 != t1) return false;

    while( Cast<TypeName>(mem0 + e0.index) != TYPE_NON && Cast<TypeName>(mem1 + e1.index) != TYPE_NON && Cast<TypeName>(mem0 + e0.index) == Cast<TypeName>(mem1 + e1.index) ) {

        switch( Cast<TypeName>(mem0 + e0.index) ) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            {

                FnTypeExpr fn0 = Cast<FnTypeExpr>(mem0 + e0.index);
                FnTypeExpr fn1 = Cast<FnTypeExpr>(mem1 + e1.index);
                if( fn0.param_count != fn1.param_count ) return false;

                for(u32 i = 0; i < fn0.param_count; i++) {
                    TypeExpr p0 = Cast<TypeExpr>(mem0 + fn0.params + i * sizeof(TypeExpr));
                    TypeExpr p1 = Cast<TypeExpr>(mem1 + fn1.params + i * sizeof(TypeExpr));
                    if(!TypesEqual(structMem0, structMem1, mem0, mem1, p0,p1)) return false;
                }
                if(!TypesEqual(structMem0, structMem1, mem0, mem1, fn0.ret_t, fn1.ret_t)) return false;

                e0 = Cast<FnTypeExpr>(mem0 + e0.index).modifier;
                e1 = Cast<FnTypeExpr>(mem1 + e1.index).modifier;
            }
            break;
        case TYPE_MODIFIER_ARRAY:
            {
                ArrayTypeExpr fn0 = Cast<ArrayTypeExpr>(mem0 + e0.index);
                ArrayTypeExpr fn1 = Cast<ArrayTypeExpr>(mem1 + e1.index);
                if(fn0.array_size != fn1.array_size) return false;
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

    return Cast<TypeName>(mem0 + e0.index) == Cast<TypeName>(mem1 + e1.index);
}

ExprType GetLastExprType(Parser* parser, Expr* expr) {
    
    ExprType t = *Get<ExprType>(parser, expr->index);
    bool searching = true;
    while(searching) {
        switch(t) {
        case EXPRESSION_GROUP:
            {
                GroupExpr g = *Get<GroupExpr>(parser, expr->index);
                *expr = g.expr;
                t = *Get<ExprType>(parser, expr->index);
            }
            break;
        default:
            t = *Get<ExprType>(parser, expr->index);
            searching = false;
            break;
        }
    }
    return t;
}

ExprType FirstNotGroupExpr(Parser* parser, Expr* expr) {
    
    ExprType t = *Get<ExprType>(parser, expr->index);
    bool searching = true;
    while(searching) {
        switch(t) {
        case EXPRESSION_GROUP:
            {
                GroupExpr g = *Get<GroupExpr>(parser, expr->index);
                *expr = g.expr;
                t = *Get<ExprType>(parser, expr->index);
            }
            break;
        case EXPRESSION_CONVERSION:
            {  
                ConversionExpr g = *Get<ConversionExpr>(parser, expr->index);
                *expr = g.from;
                t = *Get<ExprType>(parser, expr->index);
            }
            break;
        case EXPRESSION_PEEL_TYPE:
            {
                PeelTypeExpr g = *Get<PeelTypeExpr>(parser, expr->index);
                *expr = g.expr;
                t = *Get<ExprType>(parser, expr->index);
                break;
            }
        default:
            t = *Get<ExprType>(parser, expr->index);
            searching = false;
            break;
        }
    }
    return t;
}
u32 GetTypeNameSize(TypeName type) {
    
    switch(type) {
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
        return 8;
    default: return 0;
    }

    return 0;
}

u32 GetTypeSize(byte* structmem, byte* mem, TypeExpr type);
u32 GetTypeSizeHelper(byte* structmem, byte* mem, i32* types, u32 i) {

    TypeName type = Cast<TypeName>(mem + types[i]);
    switch(type) {
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
        return 8;
    case TYPE_MODIFIER_ARRAY:
        return Cast<ArrayTypeExpr>(mem + types[i]).array_size * GetTypeSizeHelper(structmem, mem, types, i-1);
    default:
        {
            TypeExpr members = Cast<StructTypeExpr>(structmem + type).members;
            u32 size = 0;
            while( Cast<TypeExpr>(structmem + members.index).index != 0) {
                TypeExpr member = Cast<TypeExpr>(structmem + members.index);
                TypeExpr memberType = Cast<StructMemberTypeExpr>(structmem + member.index).type;
                size += GetTypeSize(structmem, structmem, memberType);
                members.index += sizeof(TypeExpr);
            }
            return size;
        }
    }
    ASSERT(false);
}

u32 GetTypeSize(byte* structmem, byte* mem, TypeExpr type) {

    u32 i = type.index;
    u32 size = 0;
    while(Cast<TypeName>(mem + i)) {
        size++;
        switch(Cast<TypeName>(mem + i)) {
        case TYPE_MODIFIER_ARRAY:
            {
                i += sizeof(ArrayTypeExpr);
            }
            break;
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            {  
                FnTypeExpr* node = (FnTypeExpr*)(mem + i);
                i = node->modifier.index;
            }
            break;
        default:
            i += sizeof(TypeName);
            break;
        }
    }

    i32 types[size];
    i = type.index;
    size = 0;
    while(Cast<TypeName>(mem + i)) {
        switch(Cast<TypeName>(mem + i)) {
        case TYPE_MODIFIER_ARRAY:
            {
                types[size++] = i;
                i += sizeof(ArrayTypeExpr);
            }
            break;
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            {  
                types[size++] = i;
                FnTypeExpr* node = (FnTypeExpr*)(mem + i);
                i = node->modifier.index;
            }
            break;
        default:
            types[size++] = i;
            i += sizeof(TypeName);
            break;
        }
    }

    return GetTypeSizeHelper(structmem, mem, types, size-1);
}

TypeExpr GetTypeExpr(Compiler* compiler, Parser* parser, Expr expr, void* mem, TypeExpr alloc) {

    switch(*Get<ExprType>(parser, expr.index)) {
    case EXPRESSION_NULL:return {~u32(0)};
    case EXPRESSION_IMMEDIATE:
        {
            ImmediateExpr* node = Get<ImmediateExpr>(parser, expr.index);
            Cast<TypeName>((byte*)mem + alloc.index) = (TypeName)node->v.type;
            Cast<TypeName>((byte*)mem + alloc.index + sizeof(TypeName) ) = TYPE_NON;
            alloc.index += sizeof(TypeName) * 2;
            return alloc;
        }
    case EXPRESSION_LITERAL:
        {

            LiteralExpr* node = Get<LiteralExpr>(parser, expr.index);
            switch(node->literal.type) {
            case TOKEN_NUMBER_LITERAL:
                Cast<TypeName>((byte*)mem + alloc.index) = GetNumberType(node->literal);
                break;
            case TOKEN_BOOL_LITERAL:
                Cast<TypeName>((byte*)mem + alloc.index) = TYPE_PRIMARY_BOOL;
                break;
            case TOKEN_NULL_LITERAL:
                Cast<TypeName>((byte*)mem + alloc.index) = TYPE_PRIMARY_VOID;
                break;
            case TOKEN_CHAR_LITERAL:
                Cast<TypeName>((byte*)mem + alloc.index) = TYPE_PRIMARY_CHAR;
                break;
            case TOKEN_STRING_LITERAL:
                Cast<TypeName>((byte*)mem + alloc.index) = TYPE_PRIMARY_CHAR;
                alloc.index += sizeof(TypeName);
                Cast<TypeName>((byte*)mem + alloc.index) = TYPE_MODIFIER_POINTER;
                alloc.index += sizeof(TypeName);
                Cast<TypeName>((byte*)mem + alloc.index) = TYPE_NON;
                alloc.index += sizeof(TypeName);
                return alloc;
                break;
            default:LOGASSERT(false,"Unkown literal type");break;
            }
            Cast<TypeName>((byte*)mem + alloc.index + sizeof(TypeName) ) = TYPE_NON;
            alloc.index += sizeof(TypeName) * 2;
            return alloc;
        }
        break;
    case EXPRESSION_UNARY:
        {
            UnaryExpr* node = Get<UnaryExpr>(parser, expr.index);
            TypeExpr ex = GetTypeExpr(compiler, parser, node->primaryExpr, mem , alloc);
            TypeName t = GetLastType( (byte*)mem, alloc);

            switch(node->opr) {
            case TOKEN_EXCLAMATION_MARK:
                Expect(compiler, parser, t == TYPE_PRIMARY_BOOL , "unary (!) operator expects boolean expressions" );
                break;
            case TOKEN_MINUS:
                Expect(compiler, parser, (t <= TYPE_PRIMARY_INT64 && t >= TYPE_PRIMARY_INT8) || t == TYPE_PRIMARY_F32 || t == TYPE_PRIMARY_F64 , "unary (-) operator expects signed integral or floating point types" );
                break;
            case TOKEN_PLUS_PLUS:
            case TOKEN_MINUS_MINUS:
                Expect(compiler, parser, Cast<ExprType>(parser->mem + node->primaryExpr.index) == EXPRESSION_VARIABLE_LOAD ||
                    Cast<ExprType>(parser->mem + node->primaryExpr.index) == EXPRESSION_VARIABLE_ASSIGNMENT, "operand for(++/--) must be a modifiable l-value");
                Expect(compiler, parser, (t <= TYPE_PRIMARY_UINT64 && t >= TYPE_PRIMARY_INT8) || t == TYPE_MODIFIER_POINTER || t == TYPE_PRIMARY_F32 || t == TYPE_PRIMARY_F64 , "unary (++/--) operator expects integral or floating point types" );
                break;
            default:LOGASSERT(false, "Unkown unary operation");break;
            }

            return ex;
        }
        break;
    case EXPRESSION_BINARY:
        {
            BinaryExpr* node = Get<BinaryExpr>(parser, expr.index);

            TypeExpr left = GetTypeExpr(compiler, parser, node->left, mem , alloc);
            GetTypeExpr(compiler, parser, node->right, mem , left);

            TypeName left_t = GetLastType((byte*)mem, alloc);
            TypeName right_t = GetLastType((byte*)mem, left);

            bool cond = (left_t == right_t)
                        || (left_t == TYPE_MODIFIER_POINTER && IsIntegral((byte*)mem, left))
                        || (right_t == TYPE_MODIFIER_POINTER && IsIntegral((byte*)mem, alloc))
                        || node->opr == TOKEN_SUB_SCRIPT_OPR;

            Expect(compiler, parser, cond, "left and right operands have incompatible types");
            switch(node->opr) {
            case TOKEN_SUB_SCRIPT_OPR:
                cond = (left_t == TYPE_MODIFIER_POINTER || left_t == TYPE_MODIFIER_ARRAY) && IsIntegral((byte*)mem, left);
                Expect(compiler, parser, cond, "operands are incompatible with operator");
                if(left_t == TYPE_MODIFIER_ARRAY) {
                    left.index -= sizeof(ArrayTypeExpr) + sizeof(TypeName);
                    Cast<TypeName>((byte*)mem + left.index) = TYPE_NON;
                }
                else if(left_t == TYPE_MODIFIER_POINTER) {
                    left.index -= sizeof(TypeName) + sizeof(TypeName);
                    Cast<TypeName>((byte*)mem + left.index) = TYPE_NON;
                }
                left.index += sizeof(TypeName);
                return left;
            case TOKEN_PLUS:
            case TOKEN_MINUS:
                cond = IsIntegral((byte*)mem, alloc) || left_t == TYPE_PRIMARY_F32 || left_t == TYPE_PRIMARY_F64 ||
                    (left_t == TYPE_MODIFIER_POINTER && IsIntegral((byte*)mem, left)) || (right_t == TYPE_MODIFIER_POINTER && IsIntegral((byte*)mem, alloc));
                Expect(compiler, parser, cond, "operands are incompatible with operator");
                return left;
            case TOKEN_ASTERISK:
            case TOKEN_SLASH:
                cond = IsIntegral((byte*)mem, alloc) || left_t == TYPE_PRIMARY_F32 || left_t == TYPE_PRIMARY_F64;
                Expect(compiler, parser, cond, "operands are incompatible with operator");
                return left;
                break;
            case TOKEN_EQUALS_EQUALS:
            case TOKEN_EXCLAMATION_EQUALS:
                Expect(compiler, parser, left_t != TYPE_PRIMARY_VOID && left_t != TYPE_MODIFIER_ARRAY, "operands are incompatible with operator");
                Cast<TypeName>((byte*)mem + alloc.index) = TYPE_PRIMARY_BOOL;
                Cast<TypeName>((byte*)mem + alloc.index + sizeof(TypeName)) = TYPE_NON;
                alloc.index += sizeof(TypeName) * 2;
                return alloc;
                break;
            case TOKEN_RSHIFT:
            case TOKEN_RSHIFT_EQUALS:
            case TOKEN_LSHIFT:
            case TOKEN_LSHIFT_EQUALS:
                cond = IsIntegral((byte*)mem, alloc) || left_t == TYPE_PRIMARY_F32 || left_t == TYPE_PRIMARY_F64 || (left_t == TYPE_MODIFIER_POINTER && right_t == TYPE_MODIFIER_POINTER) ||
                    (left_t == TYPE_MODIFIER_POINTER && IsIntegral((byte*)mem, left)) || (right_t == TYPE_MODIFIER_POINTER && IsIntegral((byte*)mem, alloc));
                Expect(compiler, parser, cond, "operands for (>/>=/</<=) must be integral or float types");
                Cast<TypeName>((byte*)mem + alloc.index) = TYPE_PRIMARY_BOOL;
                Cast<TypeName>((byte*)mem + alloc.index + sizeof(TypeName)) = TYPE_NON;
                alloc.index += sizeof(TypeName) * 2;
                return alloc;
                break;
            case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
            case TOKEN_AMPERSAND_AMPERSAND:
            case TOKEN_CIRCUMFLEX:
                Expect(compiler, parser, (left_t = TYPE_PRIMARY_BOOL) , "operands for (|| && ^) must be boolean or integral types");
                Cast<TypeName>((byte*)mem + alloc.index) = TYPE_PRIMARY_BOOL;
                Cast<TypeName>((byte*)mem + alloc.index + sizeof(TypeName)) = TYPE_NON;
                alloc.index += sizeof(TypeName) * 2;
                return alloc;
                break;
            default:LOGASSERT(false, "Unkown binary operation");break;
            }
        }
        break;
    case EXPRESSION_CONVERSION:
        {
            ConversionExpr* node = Get<ConversionExpr>(parser, expr.index);
            GetTypeExpr(compiler, parser, node->from, mem , alloc);
            TypeName from = GetLastType((byte*)mem, alloc);
            TypeName to = GetLastType(parser->mem, node->type);

            Expect(compiler, parser, !(from == TYPE_PRIMARY_AUTO && to == TYPE_PRIMARY_AUTO), "cannot deduce type" );
            Expect(compiler, parser, from != TYPE_PRIMARY_VOID, "conversion from void is invalid" );
            Expect(compiler, parser, to != TYPE_PRIMARY_VOID, "conversion to void is invalid" );
            Expect(compiler, parser, to != TYPE_MODIFIER_ARRAY, "conversion to array is invalid" );
            Expect(compiler, parser, !(to == TYPE_MODIFIER_POINTER && (from == TYPE_PRIMARY_F32 || from == TYPE_PRIMARY_F64)) , "conversion from floating point to pointer" );
            Expect(compiler, parser, !(from == TYPE_MODIFIER_POINTER && (to == TYPE_PRIMARY_F32 || to == TYPE_PRIMARY_F64)) , "conversion from pointer to floating point" );

            if(to == TYPE_PRIMARY_AUTO) {
                Cast<TypeName>((byte*)mem + alloc.index) = TYPE_PRIMARY_AUTO;
                Cast<TypeName>((byte*)mem + alloc.index + sizeof(TypeName)) = TYPE_NON;
                return alloc;
            }
            return CpyTypeExpr( (byte*)mem, alloc, parser->mem, node->type);
        }
        break;
    case EXPRESSION_GROUP:
        {
            GroupExpr* node = Get<GroupExpr>(parser, expr.index);
            return GetTypeExpr(compiler, parser, node->expr ,mem , alloc);
        }
        break;
    case EXPRESSION_VARIABLE_LOAD:
        {
            VariableLoadExpr* node = Get<VariableLoadExpr>(parser, expr.index);
            return CpyTypeExpr( (byte*)mem, alloc, parser->mem, node->var.type);
        }
        break;
    case EXPRESSION_MEMORY_LOAD:
        {
            MemoryLoadExpr* node = Get<MemoryLoadExpr>(parser, expr.index);
            TypeExpr f = GetTypeExpr(compiler, parser, node->address, mem, alloc);

            
            if(node->de_ref) {
                ASSERT(false);
                TypeName l = GetLastType((byte*)mem, alloc);
                Expect(compiler, parser, l == TYPE_MODIFIER_POINTER || l == TYPE_MODIFIER_ARRAY, "operand for (*,[]) must be a pointer or array epxression");
                f.index -= sizeof(TypeName) * 2;
                Cast<TypeName>((byte*)mem + f.index) = TYPE_NON;
                f.index += sizeof(TypeName);
            }


            return f;
        }
        break;
    case EXPRESSION_MEMORY_ASSIGNMENT:
        {
            MemoryStoreExpr* node = Get<MemoryStoreExpr>(parser, expr.index);
            TypeExpr right = GetTypeExpr(compiler, parser, node->address, mem, alloc);

            if(node->de_ref) {
                ASSERT(false);
                TypeName l = GetLastType((byte*)mem, alloc);
                Expect(compiler, parser, l == TYPE_MODIFIER_POINTER || l == TYPE_MODIFIER_ARRAY, "operand for (*,[]) must be a pointer or array epxression");
                right.index -= sizeof(TypeName) * 2;
                Cast<TypeName>((byte*)mem + right.index) = TYPE_NON;
                right.index += sizeof(TypeName);
            }

            GetTypeExpr(compiler, parser, node->value, mem, right);

            bool eq = TypesEqual(parser->mem, parser->mem, (byte*)mem, (byte*)mem, alloc, right);
            Expect(compiler, parser, eq, "assignment of incompatible types");
            return alloc;
        }
        break;
    case EXPRESSION_VARIABLE_ASSIGNMENT:
        {
            VariableAssignmentExpr* node = Get<VariableAssignmentExpr>(parser, expr.index);

            byte* lvalMem = parser->mem;
            TypeExpr lval_t = node->var.type;
            Expect(compiler, parser, GetLastType(lvalMem, lval_t) != TYPE_MODIFIER_ARRAY, "assignment to array");

            byte* rvalMem = (byte*)mem;
            TypeExpr rval_t = GetTypeExpr(compiler, parser, node->value, mem, alloc);
            TypeExpr l = rval_t;
            rval_t = alloc;
            TypeName last = GetLastType((byte*)mem , alloc);

            if(last == TYPE_PRIMARY_AUTO) {

                u32 i = 0;
                while( Cast<ExprType>(parser->mem + node->value.index + i) == EXPRESSION_GROUP) {
                    i += sizeof(ExprType);
                }
                ASSERT( Cast<ExprType>(parser->mem + node->value.index + i) == EXPRESSION_CONVERSION );
                ConversionExpr* conv = Get<ConversionExpr>(parser, node->value.index + i);
                
                conv->type = node->var.type;
                rval_t = alloc;
                l = GetTypeExpr(compiler, parser, node->value, mem, alloc);
                last = GetLastType((byte*)mem , alloc);
            }
            if(last == TYPE_PRIMARY_FN) {

                Cast<TypeName>((byte*)mem + l.index) = TYPE_MODIFIER_POINTER;
                Cast<TypeName>((byte*)mem + l.index + sizeof(TypeName)) = TYPE_NON;
                Cast<FnTypeExpr>((byte*)mem + alloc.index).modifier = l;
            }

            bool eq = TypesEqual(parser->mem, parser->mem, lvalMem, rvalMem , lval_t, rval_t);
            Expect(compiler, parser, eq , "assignment of incompatible types");
            return alloc;
        }
        break;
    case EXPRESSION_CALL:
        {
            CallExpr* node = Get<CallExpr>(parser, expr.index);
            TypeExpr callee = GetTypeExpr(compiler, parser, node->calleeExpr, mem, alloc);

            TypeName last = GetLastType((byte*)mem , alloc);
            Expect(compiler, parser, last == TYPE_MODIFIER_POINTER || last == TYPE_PRIMARY_FN, "callee expression must have pointer to function type");

            TypeExpr fn;
            if( last == TYPE_MODIFIER_POINTER ) {
                fn = GetNthType((byte*)mem , alloc, 1);
                Expect(compiler, parser, Cast<TypeName>((byte*)mem + fn.index ) == TYPE_PRIMARY_FN, "callee expression must have pointer to function type");
            }
            else {
                fn = GetNthType((byte*)mem , alloc, 0);
            }

            u32 paramI = 0;
            FnTypeExpr fn_t = Cast<FnTypeExpr>((byte*)mem + fn.index);
            for(u32 i = 0,k = 0; i < fn_t.param_count ;i++,k += sizeof(TypeName),paramI += sizeof(TypeName)) {

                if( Cast<Expr>(parser->mem + node->args.index + k).index == EXPRESSION_NULL) {
                    break;
                }
                GetTypeExpr(compiler, parser, Cast<Expr>(parser->mem + node->args.index + k), mem, callee);
                TypeExpr param = Cast<TypeExpr>((byte*)mem + fn_t.params + k);
                Expect(compiler, parser, TypesEqual(parser->mem, parser->mem, (byte*)mem, (byte*)mem , callee, param) , "parameter argument type mismatch");
            }

            ExprType last_t = Cast<ExprType>(parser->mem + node->args.index + paramI);
            Expect(compiler, parser, last_t == EXPRESSION_NULL && fn_t.param_count*4 == paramI , "parameter argument type mismatch");

            last_t = Cast<ExprType>(parser->mem + node->args.index);
            last_t = Cast<ExprType>(parser->mem + last_t);

            return CpyTypeExpr( (byte*)mem, alloc, (byte*)mem, fn_t.ret_t);
        }
        break;
    case EXPRESSION_MEM_COPY:
        {
            MemCopyExpr* node = Get<MemCopyExpr>(parser, expr.index);
            
            TypeExpr src = GetTypeExpr(compiler, parser, node->dst, compiler->scratchMem, alloc);
            GetTypeExpr(compiler, parser, node->src, compiler->scratchMem, src);

            bool eq = TypesEqual(parser->mem, parser->mem, compiler->scratchMem, compiler->scratchMem, alloc, src);
            Expect(compiler, parser, eq , "assignment of incompatible types");

            return src;
        }
        break;
    case EXPRESSION_PEEL_TYPE:
        {
            PeelTypeExpr* node = Get<PeelTypeExpr>(parser, expr.index);
            GetTypeExpr(compiler, parser, node->expr, mem, alloc);
            TypeExpr last = GetNthType((byte*)mem, alloc, 0);
            Cast<TypeName>((byte*)mem + last.index) = TYPE_NON;
            last.index += sizeof(TypeName);

            return last;
        }
        break;
    case EXPRESSION_GET:
        {
            VariableGetExpr* node = Get<VariableGetExpr>(parser, expr.index);
            TypeExpr ret = GetTypeExpr(compiler, parser, node->prev, mem, alloc);

            TypeName l = GetLastType(compiler->scratchMem, alloc);
            Token name = Cast<StructTypeExpr>(parser->mem + l).name;

            bool found = false;
            for(u32 i = 0; i < parser->structs.size; i++) {
                if(TokensEquals( parser->structs[i].name, name)) {
                    TypeExpr members = Cast<StructTypeExpr>(parser->mem + l).members;
                    while( Cast<TypeExpr>(parser->mem + members.index).index != 0 ) {
                        TypeExpr member = Cast<TypeExpr>(parser->mem + members.index);
                        Token stName = Cast<StructMemberTypeExpr>(parser->mem + member.index).name;
                        TypeExpr memberType = Cast<StructMemberTypeExpr>(parser->mem + member.index).type;

                        if(TokensEquals(stName, node->name)) {
                            return CpyTypeExpr((byte*)mem, alloc, parser->mem, memberType);
                        }
                        members.index += sizeof(TypeExpr);
                    }
                    break;
                }
            }

            Expect(compiler, parser, found, "not a member");

            return ret;
        }
        break;
    case EXPRESSION_LOAD:
        {
            LoadExprLiteral* node = Get<LoadExprLiteral>(parser, expr.index);
            return CpyTypeExpr( (byte*)mem, alloc, parser->mem, node->type);
        }
        break;
    case EXPRESSION_STORE:
        {
            StoreExprLiteral* node = Get<StoreExprLiteral>(parser, expr.index);
            return GetTypeExpr(compiler, parser, node->expr, mem, alloc);
        }
        break;
        default:LOGASSERT(false, "Unkown expression type");break;
    };

    ASSERT(false);
    return alloc;
}


TypeExpr ParseTypeExpression(Parser* parser);
TypeExpr PrimaryTypeExpression(Parser* parser) {

    TypeExpr ret{parser->exprAllocator};

    Token base = NextToken(parser);
    if(TokenEquals(base, "u8")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_UINT8;
    }
    else if(TokenEquals(base, "u16")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_UINT16;
    }
    else if(TokenEquals(base, "u32")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_UINT32;
    }
    else if(TokenEquals(base, "u64")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_UINT64;
    }
    else if(TokenEquals(base, "i8")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_INT8;
    }
    else if(TokenEquals(base, "i16")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_INT16;
    }
    else if(TokenEquals(base, "i32")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_INT32;
    }
    else if(TokenEquals(base, "i64")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_INT64;
    }
    else if(TokenEquals(base, "f32")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_F32;
    }
    else if(TokenEquals(base, "f64")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_F64;
    }
    else if(TokenEquals(base, "char")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_CHAR;
    }
    else if(TokenEquals(base, "bool")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_BOOL;
    }
    else if(TokenEquals(base, "void")) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_VOID;
    }
    else if(base.type == TOKEN_KEYWORD_AUTO) {
        *Allocate<TypeName>(parser) = TYPE_PRIMARY_AUTO;
    }
    else if(base.type == TOKEN_KEYWORD_FN) {

        FnTypeExpr* node = Allocate<FnTypeExpr>(parser);
        ExpectToken(parser, TOKEN_OPEN_PAREN);
        node->index = TYPE_PRIMARY_FN;
        node->param_count = 0;
        node->params = 0;

        TokenType close = TOKEN_CLOSE_PAREN;
        TokenType comma = TOKEN_COMMA;
        Tokenizer peek = parser->tokenizer;
        if(!Match(parser, &close, 1)) {
            do {
                node->param_count++;
                Token q;
                while( (q = PeekToken(peek)).type != TOKEN_COMMA && q.type != TOKEN_CLOSE_PAREN) GetToken(&peek);
            } while( MatchPeek(&peek, &comma ,1 ) );

            node->params = parser->exprAllocator;
            for(u32 i = 0; i < node->param_count; i++) Allocate<TypeExpr>(parser);

            node->param_count = 0;
            do {
                Cast<TypeExpr>(parser->mem + node->params + node->param_count * sizeof(TypeExpr)) = ParseTypeExpression(parser);
                node->param_count++;
            } while( Match(parser, &comma ,1 ) );


            ExpectToken(parser, TOKEN_CLOSE_PAREN);
        }


        TokenType arrow = TOKEN_KEYWORD_ARROW;
        if(Match(parser, &arrow, 1 )) {
            node->ret_t = ParseTypeExpression(parser);
        }
        else {
            node->ret_t.index = parser->exprAllocator;
            *Allocate<TypeName>(parser) = TYPE_PRIMARY_VOID;
        }
    }
    else if(base.type == TOKEN_OPEN_PAREN) {
        PrimaryTypeExpression(parser);
        ExpectToken(parser, TOKEN_CLOSE_PAREN);
    }
    else {
        for(u32 i = 0; i < parser->structs.size; i++) {
            if(TokensEquals(base, parser->structs[i].name)) {
                Allocate<TypeExpr>(parser)->index = parser->structs[i].ptr;
                return ret;
            }
        }

        parser->error = true;
        std::cout << "ERROR: unknown type identifier used in type expression at line: " << parser->tokenizer.line << std::endl;
    }

    return ret;
}

TypeExpr ParseTypeModifierExpression(Parser* parser) {

    TypeExpr expr{parser->exprAllocator};

    Token peek = PeekToken(parser->tokenizer);
    if(peek.type == TOKEN_OPEN_PAREN) {
        ExpectToken(parser, TOKEN_OPEN_PAREN);
        ParseTypeModifierExpression(parser);
        ExpectToken(parser, TOKEN_CLOSE_PAREN);
    }

    while( (peek = PeekToken(parser->tokenizer)).type == TOKEN_ASTERISK || peek.type == TOKEN_OPEN_BRACKET || peek.type == TOKEN_OPEN_PAREN) {

        switch(peek.type) {
        case TOKEN_ASTERISK:
            {
                NextToken(parser);
                *Allocate<TypeName>(parser) = TYPE_MODIFIER_POINTER;
            }
            break;
        case TOKEN_OPEN_BRACKET:
            {
                NextToken(parser);
                Token arrCount = NextToken(parser);
                TypeName t = GetNumberType( arrCount );
                if(t != TYPE_PRIMARY_INT64) {
                    std::cout << "ERROR: array size must be of integral type at line: " << parser->tokenizer.line << std::endl;
                }
                u64 size = GetIntFromToken(arrCount);
                ArrayTypeExpr* node = Allocate<ArrayTypeExpr>(parser);
                node->index = TYPE_MODIFIER_ARRAY;
                node->array_size = size;

                ExpectToken(parser, TOKEN_CLOSE_BRACKET);
            }
            break;
        case TOKEN_OPEN_PAREN:
            ParseTypeModifierExpression(parser);
            ExpectToken(parser, TOKEN_CLOSE_PAREN);
            break;
        default:LOGASSERT(false, "uknown type modifier in expression");break;
        }
    }

    return expr;
}
TypeExpr ParseTypeExpression(Parser* parser) {

    TypeExpr expr = PrimaryTypeExpression(parser);
    if( *Get<TypeName>(parser, expr.index) == TYPE_PRIMARY_FN) {
        Get<FnTypeExpr>(parser, expr.index)->modifier = ParseTypeModifierExpression(parser);
    }
    else {
        ParseTypeModifierExpression(parser);
    }

    *Allocate<TypeName>(parser) = TYPE_NON;
    return expr;
}

u32 MemberOffset(Compiler* compiler, Parser* parser, Token memberName, Expr prev) {

    u32 size = 0;
    while(true) {

        GetTypeExpr(compiler, parser, prev, compiler->scratchMem, TypeExpr{0});
        TypeName ptr = GetLastType(compiler->scratchMem, TypeExpr{0});
        StructTypeExpr str = Cast<StructTypeExpr>(parser->mem + ptr);
        TypeExpr members = str.members;

        while( Cast<TypeExpr>(parser->mem + members.index).index != 0 ) {
            TypeExpr memberPtr = Cast<TypeExpr>(parser->mem + members.index);
            StructMemberTypeExpr member = Cast<StructMemberTypeExpr>(parser->mem + memberPtr.index);
            

            if(TokensEquals(member.name, memberName) ) break;
            size += GetTypeSize(parser->mem, parser->mem, member.type);
            members.index += sizeof(TypeExpr);
        }

        switch(Cast<ExprType>(parser->mem + prev.index)) {
        case EXPRESSION_GET:
            {
                memberName = Get<VariableGetExpr>(parser, prev.index)->name;
                prev = Get<VariableGetExpr>(parser, prev.index)->prev;
            }
            break;
        default: return size;
        }
    }
}

bool IsMemberOf(byte* structures, byte* mem, TypeExpr st, Token name) {
    
    TypeExpr members = Cast<StructTypeExpr>(structures + Cast<TypeExpr>(mem + st.index).index).members;

    while( Cast<TypeExpr>(structures + members.index).index != 0 ) {
        TypeExpr member = Cast<TypeExpr>(structures + members.index);
        Token t = Cast<StructMemberTypeExpr>(structures + member.index).name;

        if(TokensEquals(t, name)) {
            return true;
        }
        members.index += sizeof(TypeExpr);
    }

    return false;
}

u32 AllocateBlockName(Parser* parser) {
    u32 ret = parser->block_name_count++;
    return ret;
}

Expr Expression(Parser* parser, Compiler* compiler);
Expr PrimaryExpression(Parser* parser, Compiler* compiler) {

    Expr expr{EXPRESSION_NULL};

    TokenType literals[5] = {TOKEN_BOOL_LITERAL , TOKEN_STRING_LITERAL , TOKEN_NULL_LITERAL ,TOKEN_NUMBER_LITERAL, TOKEN_CHAR_LITERAL};
    TokenType open = TOKEN_OPEN_PAREN;
    TokenType cast = TOKEN_LSHIFT;
    TokenType identifier = TOKEN_IDENTIFIER;
    TokenType asterisk = TOKEN_ASTERISK;

    if( Match(parser , literals ,5) ) {
        
        LiteralExpr* node = Allocate<LiteralExpr>(parser);
        node->index = EXPRESSION_LITERAL;
        node->literal = PreviousToken(parser);
        node->block_name = AllocateBlockName(parser);
        expr = Convert(parser, node);
    }
    else if(Match(parser ,&cast ,1)) {
        
        ConversionExpr* node = Allocate<ConversionExpr>(parser);
        node->index = EXPRESSION_CONVERSION;
        node->type = ParseTypeExpression(parser);
        ExpectToken(parser ,TOKEN_RSHIFT);
        node->from = PrimaryExpression(parser, compiler);
        node->block_name = AllocateBlockName(parser);
        expr = Convert(parser, node);
    }
    else if(Match(parser ,&open ,1)) {

        GroupExpr* node = Allocate<GroupExpr>(parser);
        node->index = EXPRESSION_GROUP;
        node->expr = Expression(parser, compiler);
        ExpectToken(parser ,TOKEN_CLOSE_PAREN);
        expr = Convert(parser, node);
    }
    else if(Match(parser , &identifier, 1)) {

        VariableLoadExpr* node = Allocate<VariableLoadExpr>(parser);
        node->index = EXPRESSION_VARIABLE_LOAD;
        node->block_name = AllocateBlockName(parser);
        node->var.name = PreviousToken(parser);

        u32 index = FindVariable(&compiler->stack, node->var.name);
        if(index == ~u32(0)) (std::cout << "ERROR: unknown variable(").write(node->var.name.text, node->var.name.lenght) << ")referenced in expression" << std::endl;
        Expect(compiler, parser, index != ~u32(0), "");
        node->var = compiler->stack[index];

        expr = Convert(parser, node);
    }
    else if(Match(parser, &asterisk, 1)) {

        MemoryLoadExpr* node = Allocate<MemoryLoadExpr>(parser);
        node->index = EXPRESSION_MEMORY_LOAD;
        node->address.index = parser->exprAllocator;
        node->de_ref = false;
        node->block_name = AllocateBlockName(parser);

        PeelTypeExpr* peel = Allocate<PeelTypeExpr>(parser);
        peel->index = EXPRESSION_PEEL_TYPE;
        peel->expr = PrimaryExpression(parser, compiler);

        expr = Convert(parser, node);
        GetTypeExpr(compiler, parser, node->address, compiler->scratchMem, TypeExpr{0});
        TypeName last = GetLastType(compiler->scratchMem, TypeExpr{0});
        if(last > TYPE_COUNT) {
            expr = Convert(parser, peel);
        }
    }

    return expr;
}
Expr RestOfCall(Parser* parser, Compiler* compiler, Expr callee) {

    CallExpr* call = Allocate<CallExpr>(parser);
    call->index = EXPRESSION_CALL;
    call->calleeExpr = callee;
    call->liveRegisters = nullptr;
    call->block_name = AllocateBlockName(parser);


    u32 argCount = 0;
    ParserState save = SaveParserState(parser);
    if(!Check(parser , TOKEN_CLOSE_PAREN)) {
        do {
            argCount++;
            Expression(parser, compiler);
        } while(NextToken(parser).type == TOKEN_COMMA);
    }
    RestoreParserState(parser, save);

    Expr args{parser->exprAllocator + (u32)sizeof(Expr)};
    parser->exprAllocator += sizeof(Expr) * (argCount + 1);
    Allocate<Expr>(parser)->index = EXPRESSION_NULL;
    
    u32 i = args.index;
    if(!Check(parser , TOKEN_CLOSE_PAREN)) {
        do {
            Cast<Expr>(parser->mem + i) = Expression(parser, compiler);
            i += sizeof(Expr);
        } while(NextToken(parser).type == TOKEN_COMMA);

        TokenType t = parser->tokenBuffer.Back().type;
        Expect(compiler, parser , t == TOKEN_CLOSE_PAREN, "expected )");
    }
    else {
        NextToken(parser);
    }
    call->args = args;
    {
        GetTypeExpr(compiler, parser, call->calleeExpr, compiler->scratchMem, TypeExpr{0});
        TypeExpr fnExpr = GetNthType(compiler->scratchMem, TypeExpr{0}, 0);

        FnTypeExpr* fn = (FnTypeExpr*)(compiler->scratchMem + fnExpr.index);
        TypeName retT = GetLastType(compiler->scratchMem, fn->ret_t);
        if(retT == TYPE_MODIFIER_ARRAY || retT > TYPE_COUNT) {
            
            call->args.index -= sizeof(Expr);
            Cast<Expr>(parser->mem + call->args.index).index = parser->exprAllocator;
            
            ConversionExpr* conv = Allocate<ConversionExpr>(parser);
            conv->block_name = AllocateBlockName(parser);
            conv->from.index = parser->exprAllocator;
            compiler->hiddenParams.PushBack(parser->exprAllocator);
            VariableLoadExpr* hidden = Allocate<VariableLoadExpr>(parser);
            hidden->block_name = AllocateBlockName(parser);

            conv->index = EXPRESSION_CONVERSION;
            conv->type.index = parser->exprAllocator;
            TypeExpr end = CpyTypeExpr(parser->mem, conv->type, compiler->scratchMem, fn->ret_t);
            Cast<TypeName>(parser->mem + end.index - sizeof(TypeName)) = TYPE_MODIFIER_POINTER;
            Cast<TypeName>(parser->mem + end.index) = TYPE_NON;
            parser->exprAllocator = end.index + sizeof(TypeName);

            hidden->index = EXPRESSION_VARIABLE_LOAD;
            hidden->var.type.index = parser->exprAllocator;
            end = CpyTypeExpr(parser->mem, hidden->var.type, compiler->scratchMem, fn->ret_t);
            parser->exprAllocator = end.index + sizeof(TypeName);
            
        }
    }

    return Convert(parser, call);
}

Expr EqualityExpression(Parser* parser, Compiler* compiler);
Expr MemoryExpression(Parser* parser, Compiler* compiler) {

    Expr expr = PrimaryExpression(parser, compiler);

    Token peek = PeekToken(parser->tokenizer);
    if(peek.type == TOKEN_DOT || peek.type == TOKEN_OPEN_BRACKET) {
        TokenType bracket = TOKEN_OPEN_BRACKET;
        TokenType dot = TOKEN_DOT;

        MemoryLoadExpr* load;
        Expr address = expr;

        Expr i = expr;
        ExprType lval_t = FirstNotGroupExpr(parser, &i);

        if(lval_t == EXPRESSION_MEMORY_LOAD) {

            load = (MemoryLoadExpr*)(parser->mem + i.index);
            load->de_ref = false;
            address = load->address;

            i = address;
            lval_t = FirstNotGroupExpr(parser, &i);
            if(lval_t == EXPRESSION_GET) {
                MemoryLoadExpr* load2 = Allocate<MemoryLoadExpr>(parser);
                load2->block_name = AllocateBlockName(parser);
                load2->index = EXPRESSION_MEMORY_LOAD;
                load2->de_ref = false;
                address = expr;
                load = load2;
            }
        }
        else {

            load = Allocate<MemoryLoadExpr>(parser);
            load->block_name = AllocateBlockName(parser);
            load->index = EXPRESSION_MEMORY_LOAD;
            load->de_ref = false;
        }

        while(peek.type == TOKEN_DOT || peek.type == TOKEN_OPEN_BRACKET) {
            if(Match(parser, &dot, 1)) {
                Expr current{parser->exprAllocator};
                VariableGetExpr* get = Allocate<VariableGetExpr>(parser);
                get->block_name = AllocateBlockName(parser);
                get->index = EXPRESSION_GET;
                get->name = NextToken(parser);

                GetTypeExpr(compiler, parser, address, compiler->scratchMem, TypeExpr{0});
                get->prev = address;
                address = current;
            }
            else if(Match(parser, &bracket, 1)) {

                Expr current{parser->exprAllocator};

                BinaryExpr* binary = Allocate<BinaryExpr>(parser);
                binary->block_name = AllocateBlockName(parser);
                binary->index = EXPRESSION_BINARY;
                binary->opr = TOKEN_SUB_SCRIPT_OPR;
                binary->left = address;
                binary->right = EqualityExpression(parser, compiler);

                ExpectToken(parser, TOKEN_CLOSE_BRACKET);

                address = current;
            }
            peek = PeekToken(parser->tokenizer);
        }

        load->address = address;
        expr = Convert(parser, load);

        GetTypeExpr(compiler, parser, load->address, compiler->scratchMem, TypeExpr{0});
        TypeName last = GetLastType(compiler->scratchMem, TypeExpr{0});
        if(last > TYPE_COUNT || last == TYPE_MODIFIER_ARRAY) {
            expr = load->address;
        }

    }

    return expr;
}


Expr CallExpression(Parser* parser, Compiler* compiler) {

    Expr expr = MemoryExpression(parser, compiler);
    for(;;) {
        
        if(Check(parser,TOKEN_OPEN_PAREN)) {
            NextToken(parser);
            expr = RestOfCall(parser, compiler, expr);
        }
        else {
            break;
        }
    }
    return expr;
}
Expr UnaryExpression(Parser* parser, Compiler* compiler) {

    TokenType unaries[4] = {TOKEN_EXCLAMATION_MARK, TOKEN_MINUS, TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS};
    while(Match(parser ,unaries ,4)) {

        UnaryExpr* node = Allocate<UnaryExpr>(parser);
        node->block_name = AllocateBlockName(parser);
        node->index = EXPRESSION_UNARY;
        node->opr = PreviousToken(parser).type;
        node->primaryExpr = CallExpression(parser, compiler);

        return Expr{Convert(parser , node)};
    }

    return CallExpression(parser, compiler);
}

Expr FactorExpression(Parser* parser, Compiler* compiler) {

    Expr expr = UnaryExpression(parser, compiler);

    TokenType binarie[2] = {TOKEN_ASTERISK,TOKEN_SLASH};
    while(Match(parser ,binarie,2 )) {

        BinaryExpr* node = Allocate<BinaryExpr>(parser);
        node->block_name = AllocateBlockName(parser);
        node->index = EXPRESSION_BINARY;
        node->opr = PreviousToken(parser).type;
        node->left = expr;
        node->right = UnaryExpression(parser, compiler);

        expr = Convert(parser, node);
    }

    return expr;
}
Expr TermExpression(Parser* parser, Compiler* compiler) {
    
    Expr expr = FactorExpression(parser, compiler);

    TokenType binaries[5] = {TOKEN_PLUS,TOKEN_MINUS,TOKEN_AMPERSAND_AMPERSAND,TOKEN_VERTICAL_BAR_VERTICAL_BAR,TOKEN_CIRCUMFLEX};
    while(Match(parser, binaries ,5)) {
        
        BinaryExpr* node = Allocate<BinaryExpr>(parser);
        node->block_name = AllocateBlockName(parser);
        node->index = EXPRESSION_BINARY;
        node->opr = PreviousToken(parser).type;
        node->left = expr;
        node->right = FactorExpression(parser, compiler);

        expr = Convert(parser, node);
    }

    return expr;
}

Expr ComparisonExpression(Parser* parser, Compiler* compiler) {

    Expr expr = TermExpression(parser, compiler);

    TokenType comp[4] = {TOKEN_RSHIFT,TOKEN_LSHIFT,TOKEN_RSHIFT_EQUALS,TOKEN_LSHIFT_EQUALS};
    while(Match(parser ,comp, 4 )) {

        BinaryExpr* node = Allocate<BinaryExpr>(parser);
        node->block_name = AllocateBlockName(parser);
        node->index = EXPRESSION_BINARY;
        node->opr = PreviousToken(parser).type;
        node->left = expr;
        node->right = TermExpression(parser, compiler);

        expr = Convert(parser, node);
    }
    
    return expr;
}

Expr EqualityExpression(Parser* parser, Compiler* compiler) {

    Expr expr = ComparisonExpression(parser, compiler);
    TokenType eq[2]{TOKEN_EXCLAMATION_EQUALS,TOKEN_EQUALS_EQUALS};

    while(Match(parser ,eq, 4 )) {

        BinaryExpr* node = Allocate<BinaryExpr>(parser);
        node->block_name = AllocateBlockName(parser);
        node->index = EXPRESSION_BINARY;
        node->opr = PreviousToken(parser).type;
        node->left = expr;
        node->right = ComparisonExpression(parser, compiler);

        expr = Convert(parser, node);
    }

    return expr;
}

Expr AssignmentExpression(Parser* parser, Compiler* compiler) {

    Expr lval = EqualityExpression(parser, compiler);

    TokenType eq = TOKEN_EQUAL_SIGN;
    if(Match(parser , &eq , 1)) {

        Expr rval = AssignmentExpression(parser, compiler);

        Expr i = rval;
        if(FirstNotGroupExpr(parser, &i) == EXPRESSION_CALL) {

            CallExpr* call = Get<CallExpr>(parser, i.index);
            TypeExpr end = GetTypeExpr(compiler, parser, call->calleeExpr, compiler->scratchMem, TypeExpr{0});
            TypeExpr fnExpr = GetNthType(compiler->scratchMem, TypeExpr{0}, 0);
            FnTypeExpr* fn = (FnTypeExpr*)(compiler->scratchMem + fnExpr.index);
            TypeName retT = GetLastType(compiler->scratchMem, fn->ret_t);

            GetTypeExpr(compiler, parser, lval, compiler->scratchMem, end);
            Expect(compiler, parser, TypesEqual(parser->mem, parser->mem, compiler->scratchMem, compiler->scratchMem, fn->ret_t, end), "assignment of incompatible tpyes");

            if(retT == TYPE_MODIFIER_ARRAY || retT > TYPE_COUNT) {
                
                Expr paramExpr = Cast<Expr>(parser->mem + call->args.index);
                ConversionExpr* lvalAdd = (ConversionExpr*)(parser->mem + paramExpr.index);
                for(u32 i = 0; i < compiler->hiddenParams.size ; i++) {

                    if( compiler->hiddenParams[i] == lvalAdd->from.index) {
                        compiler->hiddenParams[i] = compiler->hiddenParams.Back();
                        compiler->hiddenParams.PopBack();
                        break;
                    }
                }
                lvalAdd->from = lval;

                return Convert(parser, call);
            }

        }

        i = lval;
        ExprType expressionType = FirstNotGroupExpr(parser, &i);
        Expect(compiler, parser, (expressionType == EXPRESSION_MEMORY_LOAD) || (expressionType == EXPRESSION_VARIABLE_LOAD), "expression must be a modifiable l-value");
        if(expressionType == EXPRESSION_MEMORY_LOAD) {

            MemoryLoadExpr* l = Get<MemoryLoadExpr>(parser, i.index);

            GetTypeExpr(compiler, parser, lval, compiler->scratchMem, TypeExpr{0});
            TypeName lval_type = GetLastType(compiler->scratchMem, TypeExpr{0});
            if(lval_type > TYPE_COUNT || lval_type == TYPE_MODIFIER_ARRAY) {
                MemCopyExpr* node = Allocate<MemCopyExpr>(parser);
                node->block_name = AllocateBlockName(parser);
                node->index = EXPRESSION_MEM_COPY;
                node->dst = l->address;
                node->src = rval;
                node->size = GetTypeSize(parser->mem, compiler->scratchMem, TypeExpr{0});

                return Convert(parser, node);
            }

            MemoryStoreExpr* node = Allocate<MemoryStoreExpr>(parser);
            node->block_name = AllocateBlockName(parser);

            node->index = EXPRESSION_MEMORY_ASSIGNMENT;
            node->address = l->address;
            node->de_ref = l->de_ref;
            node->value = rval;

            return Convert(parser, node);
        }
        else if(expressionType == EXPRESSION_VARIABLE_LOAD) {

            GetTypeExpr(compiler, parser, lval, compiler->scratchMem, TypeExpr{0});
            TypeName lval_type = GetLastType(compiler->scratchMem, TypeExpr{0});
            if(lval_type > TYPE_COUNT || lval_type == TYPE_MODIFIER_ARRAY) {
                MemCopyExpr* node = Allocate<MemCopyExpr>(parser);
                node->block_name = AllocateBlockName(parser);
                node->index = EXPRESSION_MEM_COPY;
                node->dst = lval;
                node->src = rval;
                node->size = GetTypeSize(parser->mem, compiler->scratchMem, TypeExpr{0});

                return Convert(parser, node);
            }

            
            VariableLoadExpr* l = Get<VariableLoadExpr>(parser, i.index);
            VariableAssignmentExpr* node = Allocate<VariableAssignmentExpr>(parser);
            node->block_name = AllocateBlockName(parser);
            node->index = EXPRESSION_VARIABLE_ASSIGNMENT;
            node->var = l->var;
            node->value = rval;

            return Convert(parser, node);
        }
    }

    return lval;
}

Expr Expression(Parser* parser, Compiler* compiler) {
    return AssignmentExpression(parser , compiler);
}

double GetNumber(LiteralExpr l) {
    char str[l.literal.lenght+1]{0};
    for(u32 i = 0; i < l.literal.lenght ; i++) {
        str[i] = l.literal.text[i];
    }
    return strtod(str,nullptr);
}

//-------------------------------------------------------------------------------------------------


void InitChunk(Chunk* chunk, u32 init_size_init_code, u32 init_size_code, u32 init_size_num, u32 init_size_str, u32 init_size_globals) {

    chunk->main.buffer.SetCapacity(init_size_code);
    chunk->init.buffer.SetCapacity(init_size_init_code);
    chunk->stringConstants.SetCapacity(init_size_str);
    chunk->numberConstants.SetCapacity(init_size_num);
    chunk->globals.SetCapacity(init_size_globals);
}
void FreeChunk(Chunk* chunk) {

    chunk->stringConstants.Free(),
    chunk->numberConstants.Free();
    chunk->main.buffer.Free();
    chunk->main.refs.Free();
    chunk->init.buffer.Free();
    chunk->init.refs.Free();
    chunk->globals.Free();
}
u32 GetOpSize(u32 size) {
    switch(size) {
    case 1:return 0;
    case 2:return 1;
    case 4:return 2;
    case 8:return 3;
    }
    ASSERT(false);
    return 0;
}
u8 ToFrom(RegisterName to , RegisterName from) {
    return (to << 4) | (from & 0x0F);
}
struct EncodedLea {
    u16     high;
    i16     low;
    byte    extra;
};
EncodedLea EncodeLea(RegisterName dst, RegisterName base, RegisterName offset, u8 shift, i16 im16) {

    EncodedLea ret;

    ret.high = ( u16(ToFrom(dst,base) << 8) & 0xFF00) | ((ToFrom(offset,(RegisterName)shift)) & 0x00FF);
    ret.low = im16;

    ret.extra = 0;
    ret.extra |= (base == 255) << 1;
    ret.extra |= (offset == 255);

    return ret;
}
void PrintLea(EncodedLea lea) {
    u32 dst = (lea.high >> 12) & 0xF;
    u32 base = (lea.high >> 8) & 0xF;
    u32 offset = (lea.high >> 4) & 0xF;
    u32 shift = (lea.high) & 0xF;
    i32 im = lea.low;

    bool baseE = (lea.extra & 1);
    bool offsetE = (lea.extra & 2) >> 1;

    std::cout << "OP_LEA r" << (u32)dst << " , [";
    if(baseE) {
        std::cout << "r" << (u32)base << "+";
    }
    if(offsetE) {
        std::cout << "r" << offset << "*" << (1 << (shift)) <<  (im > 0 ? "+" : "");
    }
    std::cout  << im << "]" << std::endl;
}
void WriteChunkOpCode(CodeSegment* seg, Opcode op) {
    seg->buffer.PushBack(op);
}
void WriteChunkIM8(CodeSegment* seg , u8 im) {
    seg->buffer.PushBack(im);
}
void WriteChunkRegister(CodeSegment* seg , RegisterName r) {
    seg->buffer.PushBack(r);
}
void WriteChunkDataNum(Chunk* chunk , byte d) {
    chunk->numberConstants.PushBack(d);
}
void WriteChunkDataStr(Chunk* chunk , char d) {
    chunk->numberConstants.PushBack(d);
}
void WriteChunkGlobal(Chunk* chunk , byte d) {
    chunk->globals.PushBack(d);
}

void WriteChunkIM16(CodeSegment* seg, u16 v) {
    seg->buffer.PushBack( ((byte*)&v)[0] );
    seg->buffer.PushBack( ((byte*)&v)[1] );
}

void AnchorRefAtCurrent(CodeSegment* seg, u32 ref) {
    seg->refs[ref].codeOffset = seg->buffer.size;
}

constexpr u32 code_padding = (64 - (sizeof(ExecutableHeader) - ((sizeof(ExecutableHeader) / 64) * 64))) * ((sizeof(ExecutableHeader) - ((sizeof(ExecutableHeader) / 64) * 64)) % 64 != 0);
void WriteChunkHeader(Chunk* chunk, byte* mem) {
    
    for(u32 i = 0; i < sizeof(ExecutableHeader) ; i++) {
        mem[i] = 0;
    }

    *(ExecutableHeader*)mem = chunk->header;
    ((ExecutableHeader*)mem)->code_start_offset = sizeof(ExecutableHeader) + code_padding;
    ((ExecutableHeader*)mem)->code_end_offset = ((ExecutableHeader*)mem)->code_start_offset + chunk->main.buffer.size + chunk->init.buffer.size;


    const u32 code_end = ((ExecutableHeader*)mem)->code_end_offset;

    const u32 data_padding = (64 - (code_end % 64)) * ((code_end % 64) != 0);
    ((ExecutableHeader*)mem)->read_only_start_offset = code_end + data_padding;
    ((ExecutableHeader*)mem)->read_only_end_offset = ((ExecutableHeader*)mem)->read_only_start_offset + chunk->numberConstants.size + chunk->stringConstants.size;

    const u32 read_only_end = ((ExecutableHeader*)mem)->read_only_end_offset;
    const u32 global_padding = (64 - (read_only_end % 64)) * ((read_only_end % 64) != 0);

    ((ExecutableHeader*)mem)->globals_start_offset = read_only_end + global_padding;
    ((ExecutableHeader*)mem)->globals_end_offset = ((ExecutableHeader*)mem)->globals_start_offset + chunk->globals.size;
}

byte* DisassembleInstruction(byte* pc);

Executable* CreateExecutable(Chunk* chunk) {
    {
        u64 main = chunk->entryPoint;
        u32 mainRef = chunk->init.refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, 0,main,4));
        
        WriteChunkOpCode(&chunk->init, OP_PUSH);
        WriteChunkIM8(&chunk->init, ToFrom(RBP,R3));
        WriteChunkOpCode(&chunk->init, OP_MOV64);
        WriteChunkIM8(&chunk->init, ToFrom(RBP,RSP));

        WriteChunkOpCode(&chunk->init, OP_CALL);
        AnchorRefAtCurrent(&chunk->init, mainRef);
        for(u32 i = 0; i < 4 ; i++) WriteChunkIM8(&chunk->init, 0xFF);

        WriteChunkOpCode(&chunk->init, OP_MOV64);
        WriteChunkIM8(&chunk->init, ToFrom(RSP,RBP));
        WriteChunkOpCode(&chunk->init, OP_POP);
        WriteChunkIM8(&chunk->init, ToFrom(RBP,R3));
        WriteChunkOpCode(&chunk->init, OP_EXIT);
    }

    u64 allocSize = chunk->main.buffer.size + chunk->numberConstants.size + chunk->stringConstants.size + chunk->globals.size + chunk->init.buffer.size + sizeof(ExecutableHeader) + 512;
    Executable* exe = (Executable*)my_malloc(allocSize);
    WriteChunkHeader(chunk,(byte*)exe);
    u32 exeOffset = exe->header.code_start_offset;

    const u32 mainOffset = exeOffset + chunk->init.buffer.size;
    {
        for(i64 codeOffset = 0; codeOffset < chunk->init.buffer.size ; exeOffset++,codeOffset++ ) {
            ((byte*)exe)[exeOffset] = chunk->init.buffer[codeOffset];
        }
        for(i64 codeOffset = 0; codeOffset < chunk->main.buffer.size ; exeOffset++,codeOffset++ ) {
            ((byte*)exe)[exeOffset] = chunk->main.buffer[codeOffset];
        }
    }
    const u32 numberConstOff = exe->header.read_only_start_offset;
    {
        exeOffset = exe->header.read_only_start_offset;
        i64 dataOffset = 0;
        for(;dataOffset < chunk->numberConstants.size ; exeOffset++,dataOffset++ ) {
            ((byte*)exe)[exeOffset] = chunk->numberConstants[dataOffset];
        }
    }
    const u32 stringConstOff = exeOffset;
    {
        i64 dataOffset = 0;
        for(;dataOffset < chunk->stringConstants.size ; exeOffset++,dataOffset++ ) {
            ((byte*)exe)[exeOffset] = chunk->stringConstants[dataOffset];
        }

    }
    const u32 globalOff = exe->header.globals_start_offset;
    {
        exeOffset = globalOff;
        i64 globalOffset = 0;
        for(;globalOffset < chunk->globals.size ; exeOffset++,globalOffset++) {
            ((byte*)exe)[exeOffset] = chunk->globals[globalOffset];
        }
    }
    exe->header.size = exeOffset;

    {
        u32 stackBegin = exe->header.size;
        stackBegin += (64 - ((u64)stackBegin % 64)) * ( (u64)stackBegin % 64 != 0);
        const u64 offsets[6] = { exe->header.code_start_offset, mainOffset, numberConstOff, stringConstOff, globalOff, stackBegin};

        for(u32 i = 0; i < chunk->init.refs.size ; i++) {
            
            const u32 off = exe->header.code_start_offset + chunk->init.refs[i].codeOffset;
            const u64 ptr = Cast<u64>(chunk->init.refs[i].value) + offsets[ chunk->init.refs[i].type ];

            for(u32 k = 0 ; k < chunk->init.refs[i].count ; k++) {
                ((byte*)exe)[off + k] = ((byte*)&ptr)[k];
            }
        }
        for(u32 i = 0; i < chunk->main.refs.size ; i++) {
            
            const u32 off = mainOffset + chunk->main.refs[i].codeOffset;
            const u64 ptr = Cast<u64>(chunk->main.refs[i].value) + offsets[ chunk->main.refs[i].type ];

            for(u32 k = 0 ; k < chunk->main.refs[i].count ; k++) {
                ((byte*)exe)[off + k] = ((byte*)&ptr)[k];
            }
        }
    }

    return exe;
}
byte* SimpleInstruction(const char* name, byte* pc) {
    std::cout << name << std::endl;
    return pc + 1;
}
_FORCE_INLINE u32 GetLow4bits(byte b) {
    return b & (0x0F);
}
_FORCE_INLINE u32 GetHigh4bits(byte b) {
    return b >> 4;
}
byte* SimpleInstructionImmediate8(const char* name, byte* pc) {
    std::cout << std::dec << std::noshowbase << name << " r" << (u32)pc[1] << " , " << std::hex << std::showbase << (u32)*(pc+2) << std::endl;
    return pc + 3;
}
byte* SimpleInstructionImmediate16(const char* name, byte* pc) {
    std::cout << std::dec << std::noshowbase << name << " r" << (u32)pc[1] << " , " << std::hex << std::showbase << (u32)*(pc+2) << " " << (u32)*(pc+3) << std::endl;
    return pc + 4;
}
byte* SimpleInstructionImmediate32(const char* name, byte* pc) {
    std::cout << std::dec << std::noshowbase << name << " r" << (u32)pc[1] << " , " << std::hex << std::showbase << (u32)*(pc+2) << " " << (u32)*(pc+3) << " " << (u32)*(pc+4) << " " << (u32)*(pc+5) << std::endl;
    return pc + 6;
}
byte* SimpleInstructionImmediate64(const char* name, byte* pc) {
    std::cout << std::dec << std::noshowbase << name << " r" << (u32)pc[1] << " , " << std::hex << std::showbase << (u32)*(pc+2) << " " << (u32)*(pc+3) << " " << (u32)*(pc+4) << " " << (u32)*(pc+5) << " "
    << (u32)*(pc+6) << " " << (u32)*(pc+7) << " " << (u32)*(pc+8) << " "<< (u32)*(pc+9) << std::endl;
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
        case OP_CMP_U_GT:
            std::cout << std::dec << std::noshowbase << "OP_CMP_U_GT r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
            return pc + 3;
            break;
        case OP_CMP_U_LT:
            std::cout << std::dec << std::noshowbase << "OP_CMP_U_LT r" << GetHigh4bits(pc[1]) << " , r" << GetLow4bits(pc[1]) << " , r" << (u32)pc[2] << std::endl;
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
        case OP_LEA:
            std::cout << std::dec << std::noshowbase;
            PrintLea(* ((EncodedLea*)(pc + 1)) );
            return pc + 6;
            break;
        case OP_GET_PTR:
            return SimpleInstructionImmediate32("OP_GET_PTR",pc);
            break;
        case OP_GET_STACK_PTR:
            std::cout << std::dec << std::noshowbase << "OP_GET_STACK_PTR r" << (u16)pc[1] << std::endl;
            return pc + 2;
            break;
        case OP_GET_BASE_PTR:
            std::cout << std::dec << std::noshowbase << "OP_GET_BASE_PTR r" << (u16)pc[1] << std::endl;
            return pc + 2;
            break;
        case OP_CALL_R:
            std::cout << std::dec << std::noshowbase << "OP_CALL_R [" << std::dec << std::noshowbase << (u32)pc[1] << "]" << std::endl;
            return pc + 2;
        case OP_CALL:
            std::cout << std::dec << std::noshowbase << "OP_CALL [" << std::hex << std::showbase << *((u32*)(pc+1)) -320 << "]" << std::endl;
            return pc + 5;
        case OP_RET:
            std::cout << std::dec << std::noshowbase << "OP_RET" << std::endl;
            return pc + 1;
        default:
            std::cout << "Warning: unknown op code: " << std::hex << std::showbase << (u32)*pc << std::endl;
            return pc + 1;
            break;
    }

    ASSERT(false);
    return 0;
}

void DumpExecutable(Executable* program) {
    std::cout << "== " << program->header.name << " exe ==" << std::endl;

    byte* codeStart = (byte*)program + program->header.code_start_offset;
    byte* codeEnd = (byte*)program + program->header.code_end_offset;
    byte* pc = codeStart;

    while(pc < codeEnd) {
        byte* offset = (byte*)(pc - codeStart);
        std::cout << (void*)offset << " ";
        pc = DisassembleInstruction(pc);
    }


    byte* dataStart = (byte*)program + program->header.read_only_start_offset;
    byte* dataEnd = (byte*)program + program->header.read_only_end_offset;
    byte* ptr = dataStart;

    std::cout << "== data ==" << std::endl;
    while(ptr < dataEnd) {
        std::cout << std::hex << std::showbase << (((i32)*ptr) & 0xFF) << " ";

        ptr++;
        if( (u64(ptr) & 7) == 0 ) {
            std::cout << "|"<< std::endl;
        }
    }
    std::cout << std::endl;
}

template<typename T> void WriteChunkBytes(CodeSegment* seg, byte* mem) {
    for(u32 i = 0; i < sizeof(T) ; i++) {
        WriteChunkIM8(seg,mem[i]);
    }
}
u32 WriteChunkDataString(Chunk* chunk, const char* str, u32 c) {
    u32 r = chunk->stringConstants.size;
    for(u32 i = 0; i < c ; i++) {
        chunk->stringConstants.PushBack(str[i]);
    }
    return r;
}
void SavePrintStrRegs(Compiler* compiler, CodeSegment* seg, u32 I) {

    bool live[REGISTER_COUNT]{};
    Function* fn = Get<Function>(compiler, compiler->currentFunction);
    for(u32 i = 0; i < fn->reg.virtualRegisters.size; i++) {
        if(I >= fn->reg.virtualRegisters[i].firstRef && I <= fn->reg.virtualRegisters[i].LastRef) {
            live[fn->reg.colors[i]] = true;
        }
    }
    for(u32 i = 0; i < REGISTER_COUNT; i++) {
        if(!live[i]) continue;
        WriteChunkOpCode(seg, OP_PUSH);
        WriteChunkIM8(seg, ToFrom((RegisterName)i, R3));
    }
}
void RestorePrintStrRegs(Compiler* compiler, CodeSegment* seg, u32 I) {

    bool live[REGISTER_COUNT]{};
    Function* fn = Get<Function>(compiler, compiler->currentFunction);
    for(u32 i = 0; i < fn->reg.virtualRegisters.size; i++) {
        if(I >= fn->reg.virtualRegisters[i].firstRef && I <= fn->reg.virtualRegisters[i].LastRef) {
            live[fn->reg.colors[i]] = true;
        }
    }
    for(i32 i = REGISTER_COUNT-1; i > -1; i--) {
        if(!live[i]) continue;
        WriteChunkOpCode(seg, OP_POP);
        WriteChunkIM8(seg, ToFrom((RegisterName)i, R3));
    }
}

TypeName GetLiteralType(Token literal) {
    switch(literal.type) {
    case TOKEN_NUMBER_LITERAL:return GetNumberType(literal);
    case TOKEN_BOOL_LITERAL:return TYPE_PRIMARY_BOOL;
    case TOKEN_NULL_LITERAL:return TYPE_PRIMARY_VOID;
    case TOKEN_STRING_LITERAL:return TYPE_MODIFIER_POINTER;
    case TOKEN_CHAR_LITERAL:return TYPE_PRIMARY_CHAR;
    default:LOGASSERT(false, "Unkown literal type") break;
    }

    ASSERT(false);
}

RegisterName CompileExpression(Compiler* compiler, Parser* parser, CodeSegment* seg, Expr expr) {

    Expr* e = Get<Expr>(parser , expr.index);
    Function* f = Get<Function>(compiler, compiler->currentFunction);

    switch (e->index) {
        case EXPRESSION_NULL:return (RegisterName)255;
        case EXPRESSION_IMMEDIATE:
            {
                ImmediateExpr* node = Get<ImmediateExpr>(parser, expr.index);
                RegisterName r = (RegisterName)f->reg.colors[node->g_index];

                if(f->reg.virtualRegisters[node->g_index].usageCount == 0) return (RegisterName)255;

                u32 size = GetTypeNameSize((TypeName)node->v.type);
                WriteChunkOpCode(seg, (Opcode)(OP_MOV_IM8 + GetOpSize(size)) );
                WriteChunkRegister(seg, r);
                for(u32 i = 0; i < size ; i++) WriteChunkIM8(seg, node->v.mem[i] );
                return r;
            }
        case EXPRESSION_LITERAL:
            {
                LiteralExpr* node = Get<LiteralExpr>(parser, expr.index);
                TypeName t = GetLiteralType(node->literal);
                
                RegisterName r = (RegisterName)f->reg.colors[node->g_index];
                switch(t) {
                case TYPE_PRIMARY_INT64:
                    {   
                        i64 n = GetIntFromToken(node->literal);
                        WriteChunkOpCode(seg, OP_MOV_IM64);
                        WriteChunkRegister(seg, r);
                        for(u32 i = 0; i < 8 ; i++) WriteChunkIM8(seg, ((byte*)&n)[i] );
                    }
                    break;
                case TYPE_PRIMARY_F32:
                    {
                        f32 n = GetF32FromToken(node->literal);
                        WriteChunkOpCode(seg, OP_MOV_IM32);
                        WriteChunkRegister(seg, r);
                        for(u32 i = 0; i < 4 ; i++) WriteChunkIM8(seg, ((byte*)&n)[i] );
                    }
                    break;
                case TYPE_PRIMARY_F64:
                    {
                        f64 n = GetF64FromToken(node->literal);
                        WriteChunkOpCode(seg, OP_MOV_IM64);
                        WriteChunkRegister(seg, r);
                        for(u32 i = 0; i < 8 ; i++) WriteChunkIM8(seg, ((byte*)&n)[i] );
                    }
                    break;
                case TYPE_PRIMARY_BOOL:
                    {
                        bool n = GetBoolFromToken(node->literal);
                        WriteChunkOpCode(seg, OP_MOV_IM8);
                        WriteChunkRegister(seg, r);
                        WriteChunkIM8(seg, n);
                    }
                    break;
                case TYPE_PRIMARY_CHAR:
                    {
                        char c = node->literal.text[0];
                        if( node->literal.text[0] == '\\' && node->literal.text[1] == 'n') c = '\n';
                        WriteChunkOpCode(seg, OP_MOV_IM8);
                        WriteChunkRegister(seg, r);
                        WriteChunkIM8(seg, *((byte*)&c) );
                    }
                    break;
                case TYPE_MODIFIER_POINTER: 
                    {
                        u16 size = 0;
                        for(u32 i = 0; i < node->literal.lenght; i++,size++) {
                            char c;
                            if( node->literal.text[i] == '\\' && node->literal.text[i+1] == 'n' ) {
                                c = '\n';
                                i += 2;
                            }
                            else {
                                c = node->literal.text[i];
                            }
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r);
                            WriteChunkIM8(seg, c);
                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, ToFrom(r,R0));
                        }
                        WriteChunkOpCode(seg, OP_MOV_IM16);
                        WriteChunkIM8(seg, r);
                        WriteChunkIM16(seg, size);

                        WriteChunkOpCode(seg, OP_SUB);
                        WriteChunkIM8(seg, ToFrom(RSP,r));
                        WriteChunkOpCode(seg, OP_GET_STACK_PTR);
                        WriteChunkIM8(seg, r);
                    }
                    break;
                    default:LOGASSERT(false, "Unkown literal type") break;
                }
                return r;
            }
        case EXPRESSION_UNARY:
            {
                UnaryExpr* node = Get<UnaryExpr>(parser, expr.index);
                RegisterName r0 = CompileExpression(compiler, parser, seg, node->primaryExpr);
                
                GetTypeExpr(compiler, parser, node->primaryExpr,compiler->scratchMem , TypeExpr{0} );
                TypeName t = GetLastType(compiler->scratchMem , TypeExpr{0});

                RegisterName r1 = (RegisterName)f->reg.colors[node->g_index];
                switch(node->opr) {
                default:LOGASSERT(false, "Unkown unary operation") break;
                case TOKEN_EXCLAMATION_MARK:
                    {
                        WriteChunkOpCode(seg , OP_NOT);
                        WriteChunkIM8(seg, ToFrom(r0,r0));
                        WriteChunkOpCode(seg , OP_MOV_IM8);
                        WriteChunkIM8(seg, r1);
                        WriteChunkIM8(seg, 1);
                        WriteChunkOpCode(seg , OP_AND);
                        WriteChunkIM8(seg, ToFrom(r1,r0) );
                        return r1;
                    }
                    break;
                case TOKEN_MINUS:
                    {
                        WriteChunkOpCode(seg , OP_XOR);
                        WriteChunkIM8(seg, ToFrom(r1,r1));
                        
                        if(t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_UINT64) {
                            WriteChunkOpCode(seg , OP_SUB);
                        }
                        else if(t == TYPE_PRIMARY_F32 ) {
                            WriteChunkOpCode(seg , OP_SUB_S);
                        }
                        else if(t == TYPE_PRIMARY_F64 ) {
                            WriteChunkOpCode(seg , OP_SUB_D);
                        }
                        WriteChunkIM8(seg, ToFrom(r1,r0) );
                        return r1;
                    }
                    break;
                case TOKEN_MINUS_MINUS:
                case TOKEN_PLUS_PLUS:
                    {
                        Opcode opr;
                        if(t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_UINT64) {
                            WriteChunkOpCode(seg , OP_MOV_IM8);
                            WriteChunkRegister(seg, r1);
                            WriteChunkIM8(seg, 1);
                            
                            opr = (node->opr == TOKEN_PLUS_PLUS ? OP_ADD : OP_SUB);
                        }
                        else if(t == TYPE_PRIMARY_F32 ) {
                            WriteChunkOpCode(seg , OP_MOV_IM32);
                            WriteChunkRegister(seg, r1);
                            f32 n = 1.0f;
                            for(u32 i = 0; i < 4 ; i++) WriteChunkIM8(seg, ((byte*)&n)[i] );

                            opr = (node->opr == TOKEN_PLUS_PLUS ? OP_ADD_S : OP_SUB_S);
                        }
                        else if(t == TYPE_PRIMARY_F64 ) {
                            WriteChunkOpCode(seg , OP_MOV_IM64);
                            WriteChunkRegister(seg, r1);
                            f64 n = 1.0f;
                            for(u32 i = 0; i < 8 ; i++) WriteChunkIM8(seg, ((byte*)&n)[i] );

                            opr = node->opr == TOKEN_PLUS_PLUS ? OP_ADD_D : OP_SUB_D;
                        }

                        WriteChunkOpCode(seg , opr);
                        WriteChunkIM8(seg, ToFrom(r0,r1));

                        return r0;
                    }
                    break;
                }
            }
            break;
        case EXPRESSION_GROUP:
            {
                GroupExpr* u = Get<GroupExpr>(parser, expr.index);
                return CompileExpression(compiler,parser, seg, u->expr);
            }
            break;
        case EXPRESSION_PEEL_TYPE:
            {
                PeelTypeExpr* node = Get<PeelTypeExpr>(parser, expr.index);
                return CompileExpression(compiler, parser ,seg, node->expr);
            }
            break;
        case EXPRESSION_BINARY:
            {
                BinaryExpr* node = Get<BinaryExpr>(parser, expr.index);
                RegisterName r0;
                RegisterName r1;
                
                {
                    Expr l = node->left,r = node->right;
                    ExprType leftT = FirstNotGroupExpr(parser, &l);
                    ExprType rightT = FirstNotGroupExpr(parser, &r);
                    if( (leftT == EXPRESSION_VARIABLE_LOAD || leftT == EXPRESSION_LITERAL) && !(rightT == EXPRESSION_VARIABLE_LOAD || rightT == EXPRESSION_LITERAL) ) {
                        r1 = CompileExpression(compiler, parser, seg, node->right);
                        r0 = CompileExpression(compiler, parser, seg, node->left);
                    }
                    else if( rightT == EXPRESSION_VARIABLE_LOAD || rightT == EXPRESSION_LITERAL ) {
                        r0 = CompileExpression(compiler, parser, seg, node->left);
                        r1 = CompileExpression(compiler, parser, seg, node->right);
                    }
                    else {
                        r0 = CompileExpression(compiler, parser, seg, node->left);
                        if(r0 == RRV && rightT == EXPRESSION_CALL) {
                            r0 = (RegisterName)f->reg.colors[node->g_index];
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r0,RRV) );
                        }
                        r1 = CompileExpression(compiler, parser, seg, node->right);
                    }
                }

                RegisterName r2;
                if(node->g_index != ~u32(0)) {
                    r2 = (RegisterName)f->reg.colors[node->g_index];
                }

                GetTypeExpr(compiler, parser, node->left,compiler->scratchMem , TypeExpr{0} );
                TypeName t = GetLastType(compiler->scratchMem , TypeExpr{0});
                u32 size = 0;
                if(t == TYPE_MODIFIER_POINTER) {
                    
                    TypeExpr last = GetNthType(compiler->scratchMem, TypeExpr{0}, 0);
                    TypeExpr nth = GetNthType(compiler->scratchMem, TypeExpr{0}, 1);

                    Cast<TypeName>(compiler->scratchMem + last.index) = TYPE_NON;
                    size = GetTypeSize(parser->mem, compiler->scratchMem, nth);
                }
                else if(t == TYPE_MODIFIER_ARRAY) {
                    size = GetTypeSize(parser->mem, compiler->scratchMem, TypeExpr{0});
                    TypeExpr last = GetNthType(compiler->scratchMem, TypeExpr{0}, 0);
                    size /= Cast<ArrayTypeExpr>(compiler->scratchMem + last.index).array_size;
                }


                Expr leftCpy = node->left;
                Expr rightCpy = node->right;
                ExprType leftExpr_t = FirstNotGroupExpr(parser, &leftCpy);
                ExprType rightExpr_t = FirstNotGroupExpr(parser, &rightCpy);

                if( leftExpr_t == EXPRESSION_VARIABLE_LOAD && rightExpr_t == EXPRESSION_VARIABLE_LOAD) {
                    switch(t) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_F32:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2, r0));
                            WriteChunkOpCode(seg, OP_ADD_S);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2, r0));
                            WriteChunkOpCode(seg, OP_SUB_S);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2, r0));
                            WriteChunkOpCode(seg, OP_MUL_S);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2, r0));
                            WriteChunkOpCode(seg, OP_DIV_S);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r2,r2));
                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r0);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, r0);
                            return r2;
                            break;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_SF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, r1);
                            return r2;
                            break;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, r1);
                            return r2;
                            break;
                        }
                        break;
                    case TYPE_PRIMARY_F64:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2, r0));
                            WriteChunkOpCode(seg, OP_ADD_D);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2, r0));
                            WriteChunkOpCode(seg, OP_SUB_D);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2, r0));
                            WriteChunkOpCode(seg, OP_MUL_D);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2, r0));
                            WriteChunkOpCode(seg, OP_DIV_D);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r2,r2));
                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r0);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, r0);
                            return r2;
                            break;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_DF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_DF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, r1);
                            return r2;
                            break;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, r1);
                            return r2;
                            break;
                        }
                        break;
                    case TYPE_MODIFIER_ARRAY:
                    case TYPE_MODIFIER_POINTER:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_SUB_SCRIPT_OPR:
                        case TOKEN_PLUS:
                            
                            WriteChunkOpCode(seg, OP_MOV_IM16);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM16(seg, size);
                            WriteChunkOpCode(seg, OP_MUL);
                            WriteChunkIM8(seg, ToFrom(R3,r2));
                            WriteChunkIM8(seg, r1);

                            WriteChunkOpCode(seg, OP_ADD);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            return r2;
                            break;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, ToFrom(r1,R3));

                            WriteChunkOpCode(seg, OP_MOV_IM16);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, size);
                            WriteChunkOpCode(seg, OP_MUL);
                            WriteChunkIM8(seg, ToFrom(R3,r1));
                            WriteChunkIM8(seg, r2);

                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));

                            WriteChunkOpCode(seg, OP_SUB);
                            WriteChunkIM8(seg, ToFrom(r2,r1));

                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, ToFrom(r1,R3));
                            return r2;
                            break;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);

                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, ToFrom(r0,R3));

                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r2,r2));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r0);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            
                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, ToFrom(r0,R3));
                            return r2;
                            break;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_U_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_U_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_U_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);

                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, ToFrom(r0,R3));

                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);

                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r2,r0));

                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, ToFrom(r0,R3));
                            return r2;
                            break;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_U_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);

                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, ToFrom(r0,R3));

                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);

                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r2,r0));

                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, ToFrom(r0,R3));
                            return r2;
                            break;
                        }

                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_ADD);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_SUB);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_IMUL : OP_MUL));
                            WriteChunkIM8(seg, ToFrom( (RegisterName)GetOpSize(GetTypeNameSize(t)) ,r2));
                            WriteChunkIM8(seg, r1);
                            return r2;
                            break;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_IDIV : OP_DIV));
                            WriteChunkIM8(seg, ToFrom( (RegisterName)GetOpSize(GetTypeNameSize(t)) ,r2));
                            WriteChunkIM8(seg, r1);
                            return r2;
                            break;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r2,r2));
                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r0);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, r0);
                            return r2;
                            break;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_GT : OP_CMP_U_GT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_LT : OP_CMP_U_LT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_GT : OP_CMP_U_GT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);

                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, ToFrom(r0,R3));

                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);

                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r2,r0));

                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, ToFrom(r0,R3));
                            return r2;
                            break;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_LT : OP_CMP_U_LT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);

                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, ToFrom(r0,R3));

                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);

                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r2,r0));

                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, ToFrom(r0,R3));
                            return r2;
                            break;
                        }
                        break;
                    case TYPE_PRIMARY_BOOL:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_AMPERSAND_AMPERSAND:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_CIRCUMFLEX:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_XOR);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r2;
                            break;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r2,r2));
                            WriteChunkOpCode(seg, OP_PUSH);
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r0);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_POP);
                            WriteChunkIM8(seg, r0);
                            return r2;
                            break;
                        }
                        break;
                    }
                }
                else if(leftExpr_t != EXPRESSION_VARIABLE_LOAD && rightExpr_t == EXPRESSION_VARIABLE_LOAD) {
                    switch(t) {
                    default:LOGASSERT(false, "Unkown binary operation") break;
                    case TYPE_PRIMARY_F32:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_ADD_S);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_SUB_S);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, OP_MUL_S);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, OP_DIV_S);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r0,r0));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_SF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        }
                        break;
                    case TYPE_PRIMARY_F64:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_ADD_D);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_SUB_D);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, OP_MUL_D);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, OP_DIV_D);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r0,r0));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_DF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_DF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        }
                        break;
                    case TYPE_MODIFIER_ARRAY:
                    case TYPE_MODIFIER_POINTER:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_SUB_SCRIPT_OPR:
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_MOV_IM16);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM16(seg, size);
                            WriteChunkOpCode(seg, OP_MUL);
                            WriteChunkIM8(seg, ToFrom(R3,r2));
                            WriteChunkIM8(seg, r1);

                            WriteChunkOpCode(seg, OP_ADD);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                            break;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_MOV_IM16);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM16(seg, size);
                            WriteChunkOpCode(seg, OP_MUL);
                            WriteChunkIM8(seg, ToFrom(R3,r2));
                            WriteChunkIM8(seg, r1);

                            WriteChunkOpCode(seg, OP_SUB);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                            break;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                            break;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r0,r0));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                            break;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_U_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                            break;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_U_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                            break;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_U_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                            break;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_U_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                            break;
                        }
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_ADD);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_SUB);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_IMUL : OP_MUL));
                            WriteChunkIM8(seg, ToFrom( (RegisterName)GetOpSize(GetTypeNameSize(t)) ,r0));
                            WriteChunkIM8(seg, r1);
                            return r0;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_IDIV : OP_DIV));
                            WriteChunkIM8(seg, ToFrom( (RegisterName)GetOpSize(GetTypeNameSize(t)) ,r0));
                            WriteChunkIM8(seg, r1);
                            return r0;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r0,r0));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_GT : OP_CMP_U_GT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_LT : OP_CMP_U_LT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_GT : OP_CMP_U_GT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_LT : OP_CMP_U_LT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        }
                        break;
                    case TYPE_PRIMARY_BOOL:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_AMPERSAND_AMPERSAND:
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_CIRCUMFLEX:
                            WriteChunkOpCode(seg, OP_XOR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r0,r0));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        }
                        break;
                    }

                }
                else if(leftExpr_t == EXPRESSION_VARIABLE_LOAD && rightExpr_t != EXPRESSION_VARIABLE_LOAD) {
                    switch(t) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_F32:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_ADD_S);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_SUB_S);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_MUL_S);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_DIV_S);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r1,r1));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_SF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                            break;
                        }
                    case TYPE_PRIMARY_F64:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_ADD_D);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r1;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_SUB_D);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r1;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_MUL_D);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r1;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_DIV_D);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r1;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            return r1;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r1,r1));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_DF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_DF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                            break;
                        }
                    case TYPE_MODIFIER_POINTER:
                    case TYPE_MODIFIER_ARRAY:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_SUB_SCRIPT_OPR:
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_MOV_IM16);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM16(seg, size);
                            WriteChunkOpCode(seg, OP_MUL);
                            WriteChunkIM8(seg, ToFrom(R3, r2));
                            WriteChunkIM8(seg, r1);

                            WriteChunkOpCode(seg, OP_ADD);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            return r2;
                            break;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_MOV_IM16);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM16(seg, size);
                            WriteChunkOpCode(seg, OP_MUL);
                            WriteChunkIM8(seg, ToFrom(R3, r1));
                            WriteChunkIM8(seg, r2);

                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));

                            WriteChunkOpCode(seg, OP_SUB);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                            break;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);

                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r1,r1));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                            break;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_U_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                            break;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_U_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                            break;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_U_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                            break;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_U_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                            break;
                        }
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:case TYPE_PRIMARY_CHAR:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_ADD);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, OP_SUB);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_IMUL : OP_MUL));
                            WriteChunkIM8(seg, ToFrom( (RegisterName)GetOpSize(GetTypeNameSize(t)) ,r2));
                            WriteChunkIM8(seg, r1);
                            return r2;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, OP_MOV64);
                            WriteChunkIM8(seg, ToFrom(r2,r0));
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_IDIV : OP_DIV));
                            WriteChunkIM8(seg, ToFrom( (RegisterName)GetOpSize(GetTypeNameSize(t)) ,r2));
                            WriteChunkIM8(seg, r1);
                            return r2;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r1,r1));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_GT : OP_CMP_U_GT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_LT : OP_CMP_U_LT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_LT : OP_CMP_U_LT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_LT : OP_CMP_U_LT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r2,r1));
                            return r2;
                            break;
                        }
                    case TYPE_PRIMARY_BOOL:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_AMPERSAND_AMPERSAND:
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r1,r0));
                            return r1;
                        case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r1,r0));
                            return r1;
                        case TOKEN_CIRCUMFLEX:
                            WriteChunkOpCode(seg, OP_XOR);
                            WriteChunkIM8(seg, ToFrom(r1,r0));
                            return r1;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            return r1;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r1);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r1,r1));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r1,r2));
                            return r1;
                            break;
                        }
                        break;
                    }
                }
                else {
                    switch(t) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_F32:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_ADD_S);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_SUB_S);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, OP_MUL_S);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, OP_DIV_S);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r1);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r0,r0));
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_SF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_SF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_SF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        }
                        break;
                    case TYPE_PRIMARY_F64:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_ADD_D);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_SUB_D);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, OP_MUL_D);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, OP_DIV_D);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r0,r0));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r1);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_DF_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_DF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_DF_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_DF_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        }
                        break;
                    case TYPE_MODIFIER_ARRAY:
                    case TYPE_MODIFIER_POINTER:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_SUB_SCRIPT_OPR:
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_MOV_IM16);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM16(seg, size);
                            WriteChunkOpCode(seg, OP_MUL);
                            WriteChunkIM8(seg, ToFrom(R3,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_ADD);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                            break;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_MOV_IM16);
                            WriteChunkIM8(seg, r2);
                            WriteChunkIM16(seg, size);
                            WriteChunkOpCode(seg, OP_MUL);
                            WriteChunkIM8(seg, ToFrom(R3,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_SUB);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                            break;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                            break;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r0,r0));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r1);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                            break;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_U_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                            break;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, OP_CMP_U_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                            break;
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_U_GT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                            break;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_U_LT);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                            break;
                        }
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_PLUS:
                            WriteChunkOpCode(seg, OP_ADD);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_MINUS:
                            WriteChunkOpCode(seg, OP_SUB);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_ASTERISK:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_IMUL : OP_MUL));
                            WriteChunkIM8(seg, ToFrom( (RegisterName)GetOpSize(GetTypeNameSize(t)) ,r0));
                            WriteChunkIM8(seg, r1);
                            return r0;
                        case TOKEN_SLASH:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_IDIV : OP_DIV));
                            WriteChunkIM8(seg, ToFrom( (RegisterName)GetOpSize(GetTypeNameSize(t)) ,r0));
                            WriteChunkIM8(seg, r1);
                            return r0;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkIM8(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r0,r0));
                            WriteChunkIM8(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r1);
                            WriteChunkIM8(seg, 1);
                            return r0;
                        case TOKEN_RSHIFT:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_GT : OP_CMP_U_GT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_LSHIFT:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_LT : OP_CMP_U_LT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                        case TOKEN_RSHIFT_EQUALS:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_LT : OP_CMP_U_LT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        case TOKEN_LSHIFT_EQUALS:
                            WriteChunkOpCode(seg, (t >= TYPE_PRIMARY_INT8 && t <= TYPE_PRIMARY_INT64 ? OP_CMP_I_LT : OP_CMP_U_LT));
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r2);
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r2));
                            return r0;
                        }
                        break;
                    case TYPE_PRIMARY_BOOL:
                        switch(node->opr) {
                        default:LOGASSERT(false, "Unkown binary operation") break;
                        case TOKEN_AMPERSAND_AMPERSAND:
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
                            WriteChunkOpCode(seg, OP_OR);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        case TOKEN_EQUALS_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            return r0;
                        case TOKEN_EXCLAMATION_EQUALS:
                            WriteChunkOpCode(seg, OP_CMP_I_E);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            WriteChunkIM8(seg, r0);
                            WriteChunkOpCode(seg, OP_NOT);
                            WriteChunkIM8(seg, ToFrom(r0,r0));
                            WriteChunkOpCode(seg, OP_MOV_IM8);
                            WriteChunkIM8(seg, r1);
                            WriteChunkIM8(seg, 1);
                            WriteChunkOpCode(seg, OP_AND);
                            WriteChunkIM8(seg, ToFrom(r0,r1));
                            return r0;
                        }
                        break;
                    }
                }

            }
            break;
        case EXPRESSION_CONVERSION:
            {
                ConversionExpr* node = Get<ConversionExpr>(parser, expr.index);
                
                TypeName to = GetLastType(parser->mem , node->type);
                GetTypeExpr(compiler, parser, node->from, compiler->scratchMem, TypeExpr{0});
                TypeName from = GetLastType(compiler->scratchMem, TypeExpr{0});

                RegisterName Rto;
                RegisterName r;
                r = CompileExpression(compiler, parser, seg, node->from);
                RegisterName Rfrom = r;

                if(Cast<ExprType>(parser->mem + node->from.index) == EXPRESSION_VARIABLE_LOAD ) {
                    Rto = (RegisterName)f->reg.colors[node->g_index];
                }
                else {
                    Rto = r;
                }

                if( to >= TYPE_PRIMARY_INT8 && to <= TYPE_PRIMARY_INT64) {
                    switch(from) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                    case TYPE_PRIMARY_BOOL:
                    case TYPE_PRIMARY_CHAR:
                    case TYPE_MODIFIER_POINTER:
                        return Rfrom;
                        break;

                    case TYPE_PRIMARY_F32:
                        WriteChunkOpCode(seg, OP_CSFS);
                        break;
                    case TYPE_PRIMARY_F64:
                        WriteChunkOpCode(seg, OP_CDFS);
                        break;
                    }
                }
                else if( (to >= TYPE_PRIMARY_UINT8 && to <= TYPE_PRIMARY_UINT64) || (to == TYPE_MODIFIER_POINTER)) {
                    switch(from) {
                    default:
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                    case TYPE_PRIMARY_BOOL:
                    case TYPE_PRIMARY_CHAR:
                    case TYPE_MODIFIER_POINTER:
                    case TYPE_MODIFIER_ARRAY:
                        return Rfrom;
                        break;

                    case TYPE_PRIMARY_F32:
                        WriteChunkOpCode(seg, OP_CSFU);
                        break;
                    case TYPE_PRIMARY_F64:
                        WriteChunkOpCode(seg, OP_CDFU);
                        break;
                    }
                }
                else if( to == TYPE_PRIMARY_CHAR ) {
                    switch(from) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                    case TYPE_PRIMARY_BOOL:
                    case TYPE_PRIMARY_CHAR:
                        return Rfrom;
                        break; 
                    }
                }
                else if( to == TYPE_PRIMARY_F32 ) {
                    switch(from) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:
                        WriteChunkOpCode(seg, OP_CSSF);
                        break;
                    case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                    case TYPE_PRIMARY_BOOL:
                    case TYPE_PRIMARY_CHAR:
                        WriteChunkOpCode(seg, OP_CUSF);
                        break;
                    case TYPE_PRIMARY_F32:
                        return Rfrom;
                        break;
                    case TYPE_PRIMARY_F64:
                        WriteChunkOpCode(seg, OP_CDFSF);
                        break;
                    }
                }
                else if( to == TYPE_PRIMARY_F64 ) {
                    switch(from) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:
                        WriteChunkOpCode(seg, OP_CSDF);
                        break;
                    case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                    case TYPE_PRIMARY_BOOL:
                    case TYPE_PRIMARY_CHAR:
                        WriteChunkOpCode(seg, OP_CUDF);
                        break;
                    case TYPE_PRIMARY_F32:
                        WriteChunkOpCode(seg, OP_CSFDF);
                        break;
                    case TYPE_PRIMARY_F64:
                        return Rfrom;
                        break;
                    }
                }
                WriteChunkIM8(seg, ToFrom(Rto,Rfrom));
                return Rto;
            }
            break;
        case EXPRESSION_MEM_COPY:
            {
                MemCopyExpr* node = Get<MemCopyExpr>(parser, expr.index);

                u8 size = node->size % 8 == 0 ? 3 : (node->size % 4 == 0 ? 2 : (node->size % 2 == 0 ? 1 : 0));

                RegisterName dstPtr = CompileExpression(compiler, parser, seg, node->dst);
                RegisterName srcPtr = CompileExpression(compiler, parser, seg, node->src);

                RegisterName i = (RegisterName)f->reg.colors[node->g_index_i];
                RegisterName res = (RegisterName)f->reg.colors[node->g_index_res];
                RegisterName cmp = (RegisterName)f->reg.colors[node->g_index_cmp];

                i32 w = (node->size >> size) - 1;
                if(w != 0) {

                    WriteChunkOpCode(seg, OP_XOR);
                    WriteChunkIM8(seg, ToFrom(i,i));

                    u32 ref = seg->refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, 0, seg->buffer.size, 8));
                    WriteChunkOpCode(seg, OP_MOV_IM16);
                    WriteChunkIM8(seg, cmp);
                    WriteChunkIM16(seg, max<i32>(w,0) );

                    WriteChunkOpCode(seg, OP_CMP_U_LT);
                    WriteChunkIM8(seg, ToFrom(cmp,i));
                    WriteChunkIM8(seg, res);

                    WriteChunkOpCode(seg, OP_MOV_IM64);
                    WriteChunkIM8(seg, cmp);
                    u32 ref2 = seg->refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, seg->buffer.size, 0, 8));
                    for(u32 i = 0; i < 8 ; i++) WriteChunkIM8(seg, 0);

                    WriteChunkOpCode(seg, OP_CJMP);
                    WriteChunkIM8(seg, ToFrom(res,cmp));

                    WriteChunkOpCode(seg, OP_LOAD);
                    WriteChunkIM8(seg, ToFrom(res,srcPtr));
                    WriteChunkIM8(seg, size);
                    WriteChunkOpCode(seg, OP_STORE);
                    WriteChunkIM8(seg, ToFrom(dstPtr,res));
                    WriteChunkIM8(seg, size);

                    WriteChunkOpCode(seg, OP_MOV_IM8);
                    WriteChunkIM8(seg, res);
                    WriteChunkIM8(seg, 1);
                    WriteChunkOpCode(seg, OP_ADD);
                    WriteChunkIM8(seg, ToFrom(i,res));

                    WriteChunkOpCode(seg, OP_MOV_IM8);
                    WriteChunkIM8(seg, res);
                    WriteChunkIM8(seg, 1 << size);
                    WriteChunkOpCode(seg, OP_ADD);
                    WriteChunkIM8(seg, ToFrom(dstPtr,res));
                    WriteChunkOpCode(seg, OP_ADD);
                    WriteChunkIM8(seg, ToFrom(srcPtr,res));

                    WriteChunkOpCode(seg, OP_JMP_IM);
                    AnchorRefAtCurrent(seg, ref);
                    for(u32 i = 0; i < 8 ; i++) WriteChunkIM8(seg, 0);
                    Cast<u64>(seg->refs[ref2].value) = seg->buffer.size;
                }
                else {
                    WriteChunkOpCode(seg, OP_LOAD);
                    WriteChunkIM8(seg, ToFrom(res,srcPtr));
                    WriteChunkIM8(seg, size);
                    WriteChunkOpCode(seg, OP_STORE);
                    WriteChunkIM8(seg, ToFrom(dstPtr,res));
                    WriteChunkIM8(seg, size);
                }

                return RRV;
            }
        case EXPRESSION_VARIABLE_LOAD:
            {
                VariableLoadExpr* node = Get<VariableLoadExpr>(parser, expr.index);

                RegisterName r = (RegisterName)f->reg.colors[node->var.g_index];
                TypeName t = GetLastType(parser->mem, node->var.type);
                
                if( t == TYPE_PRIMARY_FN ) {
                    Function* f = Get<Function>(compiler, node->var.address);
                    WriteChunkOpCode(seg, OP_GET_PTR);
                    WriteChunkIM8(seg, r);

                    u32 ref = seg->refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, 0, f->address , 4));
                    AnchorRefAtCurrent(seg, ref);
                    for(u32 i = 0; i < 4 ; i++) WriteChunkIM8(seg, 0xFF );
                }
                else if(t == TYPE_MODIFIER_ARRAY || t > TYPE_COUNT) {
                    Function* f = Get<Function>(compiler, compiler->currentFunction);
                    RegisterName r1 = (RegisterName)f->reg.colors[node->g_index];

                    EncodedLea enc = EncodeLea(r, (RegisterName)node->relative, (RegisterName)255,0, -i32(node->var.address));
                    WriteChunkOpCode(seg, OP_LEA);
                    for(u32 i = 0; i < 5; i++) WriteChunkIM8(seg, ((byte*)&enc)[i] );

                    /*
                    WriteChunkOpCode(seg, node->relative == RBP ? OP_GET_BASE_PTR : OP_GET_STACK_PTR);
                    WriteChunkIM8(seg, r);
                    
                    WriteChunkOpCode(seg, OP_MOV_IM16);
                    WriteChunkIM8(seg, r1);
                    WriteChunkIM16(seg, node->var.address);

                    WriteChunkOpCode(seg, OP_SUB);
                    WriteChunkIM8(seg, ToFrom(r,r1));
                    */
                }

                return r;
            }
            break;
        case EXPRESSION_VARIABLE_ASSIGNMENT:
            {
                VariableAssignmentExpr* node = Get<VariableAssignmentExpr>(parser, expr.index);
                
                RegisterName valueReg = CompileExpression(compiler, parser, seg, node->value);
                RegisterName varReg = (RegisterName)f->reg.colors[node->var.g_index];
                if(valueReg != varReg) {
                    WriteChunkOpCode(seg, OP_MOV64);
                    WriteChunkIM8(seg, ToFrom(varReg, valueReg));
                }
                return varReg;
            }
            break;
        case EXPRESSION_MEMORY_LOAD:
            {
                MemoryLoadExpr* node = Get<MemoryLoadExpr>(parser, expr.index);
                RegisterName add = CompileExpression(compiler, parser, seg, node->address);
                RegisterName r = (RegisterName)f->reg.colors[node->g_index];

                Expr ex = node->address;
                ExprType t = FirstNotGroupExpr(parser, &ex);

                GetTypeExpr(compiler, parser, node->address, compiler->scratchMem, TypeExpr{0});
                TypeName type = GetLastType(compiler->scratchMem, TypeExpr{0});

                if(type == TYPE_MODIFIER_POINTER) {
                    TypeName arr = Cast<TypeName>(compiler->scratchMem + GetNthType(compiler->scratchMem, TypeExpr{0}, 1).index);
                    if(arr == TYPE_MODIFIER_ARRAY) {
                        return r;
                    }
                }

                if(type == TYPE_MODIFIER_ARRAY && t == EXPRESSION_VARIABLE_LOAD) {

                    RegisterName add2 = (RegisterName)f->reg.colors[node->g_index2];
                    WriteChunkOpCode(seg, OP_MOV64);
                    WriteChunkIM8(seg, ToFrom(add2,add));
                    add = add2;
                }
                
                u32 size = GetTypeSize(parser->mem, compiler->scratchMem, TypeExpr{0});
                u8 loadSize = GetOpSize(size);

                WriteChunkOpCode(seg, OP_LOAD);
                WriteChunkIM8(seg, ToFrom(r,add));
                WriteChunkIM8(seg, loadSize);
                return r;
            }
            break;
        case EXPRESSION_MEMORY_ASSIGNMENT:
            {
                MemoryStoreExpr* node = Get<MemoryStoreExpr>(parser, expr.index);

                RegisterName add = CompileExpression(compiler, parser, seg, node->address);
                Expr ex = node->address;
                ExprType t = FirstNotGroupExpr(parser, &ex);
                RegisterName r = (RegisterName)f->reg.colors[node->g_index];
                
                GetTypeExpr(compiler, parser, node->address, compiler->scratchMem, TypeExpr{0});
                TypeName type = GetLastType(compiler->scratchMem, TypeExpr{0});

                if(type == TYPE_MODIFIER_POINTER) {
                    TypeName arr = Cast<TypeName>(compiler->scratchMem + GetNthType(compiler->scratchMem, TypeExpr{0}, 1).index);
                    if(arr == TYPE_MODIFIER_ARRAY) {
                        return r;
                    }
                }

                if(type == TYPE_MODIFIER_ARRAY) {
                    if(t == EXPRESSION_VARIABLE_LOAD) {
                        RegisterName add2 = (RegisterName)f->reg.colors[node->g_index2];
                        WriteChunkOpCode(seg, OP_MOV64);
                        WriteChunkIM8(seg, ToFrom(add2,add));
                        add = add2;
                    }
                }

                r = CompileExpression(compiler, parser, seg, node->value);
                GetTypeExpr(compiler, parser, node->value, compiler->scratchMem, TypeExpr{0});
                u32 size = GetTypeSize(parser->mem, compiler->scratchMem, TypeExpr{0});
                u8 storeSize = GetOpSize(size);

                WriteChunkOpCode(seg, OP_STORE);
                WriteChunkIM8(seg, ToFrom(add,r));
                WriteChunkIM8(seg, storeSize);
                return r;
            }
            break;
        case EXPRESSION_GET:
            {
                VariableGetExpr* node = Get<VariableGetExpr>(parser, expr.index);
                u32 offset = MemberOffset(compiler, parser, node->name, node->prev);

                RegisterName add = (RegisterName)f->reg.colors[node->g_index];

                Expr i = node->prev;

                RegisterName r = CompileExpression(compiler, parser, seg, node->prev);
                ExprType t = FirstNotGroupExpr(parser, &i);
                if(t == EXPRESSION_VARIABLE_LOAD) {
                    RegisterName cpy = (RegisterName)f->reg.colors[node->g_index_copy];
                    WriteChunkOpCode(seg, OP_MOV64);
                    WriteChunkIM8(seg, ToFrom(cpy, r));
                    r = cpy;
                }

                WriteChunkOpCode(seg, OP_MOV_IM16);
                WriteChunkIM8(seg, add);
                WriteChunkIM16(seg, offset);

                WriteChunkOpCode(seg, OP_ADD);
                WriteChunkIM8(seg, ToFrom(r,add));

                return r;
            }
            break;
        case EXPRESSION_LOAD:
            {
                LoadExprLiteral* node = Get<LoadExprLiteral>(parser, expr.index);
                RegisterName r = (RegisterName)f->reg.colors[node->g_index];
                WriteChunkOpCode(seg, OP_LOAD_REL_RBP);
                WriteChunkIM8(seg, ToFrom(r, (RegisterName)node->size ));
                WriteChunkIM16(seg, node->address);
                return r;
            }
            break;
        case EXPRESSION_STORE:
            {
                StoreExprLiteral* node = Get<StoreExprLiteral>(parser, expr.index);
                RegisterName r = CompileExpression(compiler, parser, seg, node->expr);
                WriteChunkOpCode(seg, node->relative == RBP ? OP_STORE_REL_RBP : OP_STORE_REL_RSP);
                WriteChunkIM8(seg, ToFrom(r, (RegisterName)node->size ));
                WriteChunkIM16(seg, node->address);
                return r;
            }
            break;
        case EXPRESSION_CALL:
            {
                static_assert(sizeof(ExprType) == 4);
                CallExpr* node = Get<CallExpr>(parser, expr.index);
                RegisterName r = (RegisterName)f->reg.colors[node->g_index];

                for(u32 size = node->liveRegisters[0]+1,i = 1; i < size; i++) {
                    RegisterName reg = (RegisterName)node->liveRegisters[i];
                    WriteChunkOpCode(seg, OP_PUSH);
                    WriteChunkIM8(seg, ToFrom( (RegisterName)reg,R3));
                }
                
                u32 index = node->args.index;
                while( Cast<ExprType>(parser->mem + index) != EXPRESSION_NULL ) {

                    RegisterName p = CompileExpression(compiler, parser, seg, Cast<Expr>(parser->mem + index));
                    u32 i = (index - node->args.index) >> 2;
                    if( i < R4 && p != (RegisterName)i) {
                        WriteChunkOpCode(seg, OP_MOV64);
                        WriteChunkIM8(seg, ToFrom(RegisterName(i),p));
                    }
                    index += sizeof(ExprType);
                }

                r = CompileExpression(compiler, parser, seg, node->calleeExpr);
                WriteChunkOpCode(seg, OP_CALL_R);
                WriteChunkIM8(seg, r);

                for(i32 i = node->liveRegisters[0]; i > 0; i--) {
                    RegisterName reg = (RegisterName)node->liveRegisters[i];
                    WriteChunkOpCode(seg, OP_POP);
                    WriteChunkIM8(seg, ToFrom( (RegisterName)reg,R3));
                }

                return RRV;
            }
            break;
    }


    ASSERT(false);
    return R0;
}

void Sync(Compiler* compiler,Parser* parser) {
    compiler->panic = false;

    while(parser->tokenBuffer.Back().type != TOKEN_EOF) {

        if(parser->tokenBuffer.Back().type == TOKEN_SEMICOLON) return;

        switch(parser->tokenBuffer.Back().type) {
        case TOKEN_KEYWORD_PRINT:
        case TOKEN_KEYWORD_FN:
        case TOKEN_KEYWORD_IF:
        case TOKEN_KEYWORD_FOR:
            return;
        default:
            break;
        }

        NextToken(parser);
    }
}

void PopVariables(Parser* parser, Compiler* compiler) {

    Function* f = Get<Function>(compiler, compiler->currentFunction);

    f->frameSize = max<u32>(f->currentFrameSize, f->frameSize);

    while(compiler->stack.size != 0 && compiler->stack.Back().scope > compiler->scope) {
        
        TypeExpr varType = compiler->stack.Back().type;
        TypeName t = GetLastType(parser->mem, varType);
        if(t > TYPE_COUNT || t == TYPE_MODIFIER_ARRAY) {
            f->currentFrameSize -= GetTypeSize(parser->mem, parser->mem, varType);
        }

        compiler->stack.PopBack();
    }
}

void RegInterference(Compiler* compiler, Parser* parser, Expr expr) {
    Expr* e = Get<Expr>(parser , expr.index);
    Function* f = Get<Function>(compiler , compiler->currentFunction);

    switch(e->index) {
    case EXPRESSION_LITERAL:
        {
            //LiteralExpr* node = Get<LiteralExpr>(parser , expr.index);
            //f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
        }
        break;
    case EXPRESSION_UNARY:
        {
            UnaryExpr* node = Get<UnaryExpr>(parser , expr.index);
            if(node->g_index != ~u32(0)) f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
            RegInterference(compiler, parser, node->primaryExpr);
            //f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
        }
        break;
    case EXPRESSION_BINARY:
        {
            BinaryExpr* node = Get<BinaryExpr>(parser , expr.index);
            RegInterference(compiler, parser, node->left);
            RegInterference(compiler, parser, node->right);
            //f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
        }
        break;
    case EXPRESSION_GROUP:
        {
            GroupExpr* node = Get<GroupExpr>(parser , expr.index);
            RegInterference(compiler, parser, node->expr);
        }
        break;
    case EXPRESSION_VARIABLE_ASSIGNMENT:
        {
            VariableAssignmentExpr* node = Get<VariableAssignmentExpr>(parser , expr.index);
            RegInterference(compiler, parser, node->value);
            //f->reg.virtualRegisters[node->var.g_index].LastRef = compiler->currentI;
        }
        break;
    case EXPRESSION_VARIABLE_LOAD:
        {
            VariableLoadExpr* node = Get<VariableLoadExpr>(parser , expr.index);
            f->reg.virtualRegisters[node->var.g_index].LastRef = compiler->currentI;
        }
        break;
    case EXPRESSION_STORE:
        {
            StoreExprLiteral* node = Get<StoreExprLiteral>(parser , expr.index);
            RegInterference(compiler, parser, node->expr);
        }
        break;
    case EXPRESSION_LOAD:
        {
            //LoadExprLiteral* node = Get<LoadExprLiteral>(parser , expr.index);
            //f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
        }
        break;
    case EXPRESSION_CONVERSION:
        {
            ConversionExpr* node = Get<ConversionExpr>(parser , expr.index);
            RegInterference(compiler, parser, node->from);
            //f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
        }
        break;
    case EXPRESSION_CALL:
        {
            CallExpr* node = Get<CallExpr>(parser , expr.index);
            RegInterference(compiler, parser, node->calleeExpr);
            u32 i = node->args.index;
            while( Cast<ExprType>(parser->mem + i) != EXPRESSION_NULL) {
                RegInterference(compiler, parser, Cast<Expr>(parser->mem + i));
            }
            //f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
        }
        break;
    }
}
bool RE(Compiler* compiler, Parser* parser, Stmt* program) {
    
    StatementType t = *Get<StatementType>(compiler, program->index);
    //Function* fn = Get<Function>(compiler, compiler->currentFunction);
    
    switch (t) {
    default:LOGASSERT(false, "Uknown statement");break;
    case STATEMENT_SKIP:
        {
            SkipStmt* stmt = Get<SkipStmt>(compiler, program->index);
            *program = stmt->target;
            RE(compiler, parser, program);
            if(stmt->end.index != 0) {
                *program = stmt->end;
            }
            break;
        }
    case STATEMENT_NON:
        return false;
        break;
    case STATEMENT_ABORT:
        program->index += sizeof(Stmt);
        break;
    case STATEMENT_REPEAT:
        program->index += sizeof(Stmt);
        while(RE(compiler, parser, program));
        program->index += sizeof(Stmt);
        break;
    case STATEMENT_EXPRESSION:
        {
            ExprStmt* stmt = Get<ExprStmt>(compiler, program->index);
            RegInterference(compiler, parser, stmt->expr);
            program->index += sizeof(ExprStmt);
        }
        break;
    case STATEMENT_PRINT:
        {
            //PrintStmt* stmt = Get<PrintStmt>(compiler, program->index);
            program->index += sizeof(PrintStmt);
            while( Cast<ExprType>(compiler->mem + program->index) != EXPRESSION_NULL) {
                RegInterference(compiler, parser, Cast<Expr>(compiler->mem + program->index));
                program->index += sizeof(Expr);
            }
            program->index += sizeof(Expr);
        }
        break;
    case STATEMENT_ENTRY_POINT:
        program->index += sizeof(Stmt);
        break;
    case STATEMENT_FUNCTION_ENTRY:
        program->index += sizeof(FuncFrame);
        break;
    case STATEMENT_FUNCTION_RET:
        program->index += sizeof(FuncFrame);
        break;
    case STATEMENT_BRANCH:
        {
            BranchStmt* stmt = Get<BranchStmt>(compiler, program->index);
            RegInterference(compiler, parser, stmt->cond);

            *program = stmt->thenBranch;
            RE(compiler, parser, program);
            *program = stmt->elseBranch;
            RE(compiler, parser, program);
            //fn->reg.virtualRegisters[stmt->g_index].LastRef = compiler->currentI;
        }
        break;
    case STATEMENT_FOR_LOOP:
        {
            ForStmt* stmt = Get<ForStmt>(compiler, program->index);
            RegInterference(compiler, parser, stmt->cond);
            //fn->reg.virtualRegisters[stmt->g_index].LastRef = compiler->currentI;
            //fn->reg.virtualRegisters[stmt->g_index2].LastRef = compiler->currentI;

            *program = stmt->init;
            RE(compiler, parser, program);
            *program = stmt->body;
            RE(compiler, parser, program);
            *program = stmt->inc;
            RE(compiler, parser, program);
        }
        break;
    case STATEMENT_RRV_ASSIGN:
        break;
    case STATEMENT_VAR_DECL:
        {
            VarDeclStm* stmt = Get<VarDeclStm>(compiler, program->index);
            program->index += sizeof(VarDeclStm);
            if(stmt->init) {
                program->index += sizeof(ExprStmt);
            }
        }
        break;
    }

    return true;
}
u32 AllocateVirtualRegisters(Compiler* compiler, Parser* parser, Expr expr, u32 resultVReg);
bool AllocateRegisters(Compiler* compiler, Parser* parser, Stmt* program, u32 funcPtr) {

    StatementType t = *Get<StatementType>(compiler, program->index);
    Function* fn = Get<Function>(compiler, funcPtr);
    switch(t) {
    default:LOGASSERT(false, "Uknown statement");break;
    case STATEMENT_NON:
        return false;
    case STATEMENT_ABORT:
        program->index += sizeof(Stmt);
        break;
    case STATEMENT_SKIP:
        {
            SkipStmt* stmt = Get<SkipStmt>(compiler, program->index);
            *program = stmt->target;
            AllocateRegisters(compiler, parser, program, funcPtr);
            if(stmt->end.index != 0) {
                *program = stmt->end;
            }
            break;
        }
    case STATEMENT_EXPRESSION:
        {
            ExprStmt* stmt = Get<ExprStmt>(compiler, program->index);
            AllocateVirtualRegisters(compiler, parser, stmt->expr, fn->reg.virtual_register_count++);
            program->index += sizeof(ExprStmt);
            break;
        }
    case STATEMENT_PRINT:
        {
            program->index += sizeof(PrintStmt);
            Expr e = Cast<Expr>(compiler->mem + program->index);
            while(e.index != 0) {
                Cast<u32>(compiler->mem + program->index + 4) = compiler->currentI;
                AllocateVirtualRegisters(compiler, parser, e, fn->reg.virtual_register_count++);
                program->index += sizeof(Expr) * 2;
                e = Cast<Expr>(compiler->mem + program->index);
            }
            program->index += sizeof(Expr);
            break;
        }
    case STATEMENT_ENTRY_POINT:
        {
            program->index += sizeof(FuncFrame);
            break;
        }
    case STATEMENT_FUNCTION_ENTRY:
        {
            FuncFrame* stmt = Get<FuncFrame>(compiler, program->index);
            compiler->currentFunction = stmt->function;
            stmt->g_index = AllocateVirtualRegister(compiler, funcPtr, fn->reg.virtual_register_count++, false, false);
            compiler->currentI++;
            program->index += sizeof(FuncFrame);
            break;
        }
    case STATEMENT_FUNCTION_RET:
        {
            FuncFrame* stmt = Get<FuncFrame>(compiler, program->index);
            stmt->g_index = AllocateVirtualRegister(compiler, funcPtr, fn->reg.virtual_register_count++, false, false);
            program->index += sizeof(FuncFrame);
            break;
        }
    case STATEMENT_BRANCH:
        {
            BranchStmt* stmt = Get<BranchStmt>(compiler, program->index);
            Stmt thenB = stmt->thenBranch;
            Stmt elseB = stmt->elseBranch;
            stmt->g_index = AllocateVirtualRegister(compiler, funcPtr, fn->reg.virtual_register_count++, false,false);
            u32 g_index = AllocateVirtualRegisters(compiler, parser, stmt->cond, fn->reg.virtual_register_count++);
            InsertEdge(&fn->reg.graph, g_index, stmt->g_index);
            InsertEdge(&fn->reg.graph, stmt->g_index, g_index);

            for(u32 i = 0; i < compiler->registerVariable.size ; i++) {
                compiler->registerVariable[i].possibleCFpaths++;
            }
            u32 size = compiler->registerVariable.size;
            if(elseB.index != 0) {

                AllocateRegisters(compiler, parser, &thenB, funcPtr);
                AllocateRegisters(compiler, parser, &elseB, funcPtr);
            }
            else {
                AllocateRegisters(compiler, parser, &thenB, funcPtr);
            }
            compiler->registerVariable.size = size;
            for(u32 i = 0; i < compiler->registerVariable.size ; i++) {
                compiler->registerVariable[i].possibleCFpaths--;
            }
            *program = stmt->end;
            break;
        }
    case STATEMENT_FOR_LOOP:
        {
            ForStmt* stmt = Get<ForStmt>(compiler, program->index);

            u32 vRegSize = fn->reg.virtualRegisters.size;
            u32 loopBeginI = compiler->currentI;

            Stmt s = stmt->init;
            u32 size = compiler->registerVariable.size;
            AllocateRegisters(compiler, parser, &s, funcPtr);
            u32 g_index = AllocateVirtualRegisters(compiler, parser, stmt->cond, fn->reg.virtual_register_count++);
            stmt->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, fn->reg.virtual_register_count++, false, false);
                
            for(u32 i = 0; i < compiler->registerVariable.size ; i++) {
                compiler->registerVariable[i].possibleCFpaths++;
            }
            Expr expr = stmt->cond;
            ExprType cond_t = FirstNotGroupExpr(parser, &expr);
            if(cond_t == EXPRESSION_VARIABLE_LOAD) {
                stmt->g_index2 = AllocateVirtualRegister(compiler, compiler->currentFunction, fn->reg.virtual_register_count++, false, false);
                InsertEdge(&fn->reg.graph, stmt->g_index2, g_index);
                InsertEdge(&fn->reg.graph, g_index, stmt->g_index2);

                InsertEdge(&fn->reg.graph, stmt->g_index2, stmt->g_index);
                InsertEdge(&fn->reg.graph, stmt->g_index, stmt->g_index2);
            }

            InsertEdge(&fn->reg.graph, stmt->g_index, g_index); 
            InsertEdge(&fn->reg.graph, g_index, stmt->g_index);

            s = stmt->body;
            AllocateRegisters(compiler, parser, &s, funcPtr);
            Stmt end{s};
            s = stmt->inc;
            AllocateRegisters(compiler, parser, &s, funcPtr);

            s = stmt->inc;
            RE(compiler, parser, &s);
            RegInterference(compiler, parser, stmt->cond);
            for(u32 i = 0; i < compiler->registerVariable.size ; i++) {
                compiler->registerVariable[i].possibleCFpaths--;
            }
            /*
            for(u32 i = 0; i < vRegSize; i++) {
                if(fn->reg.virtualRegisters[i].LastRef >= loopBeginI) {
                    fn->reg.virtualRegisters[i].LastRef = compiler->currentI;
                }
            }
            */

            compiler->registerVariable.size = size;
            *program = end;
            break;
        }
    case STATEMENT_REPEAT:
        {
            program->index += sizeof(Stmt);
            while(AllocateRegisters(compiler, parser, program, funcPtr));
            program->index += sizeof(Stmt);
            break;
        }
    case STATEMENT_RRV_ASSIGN:
        {
            RetAssignStmt* stmt = Get<RetAssignStmt>(compiler, program->index);
            AllocateVirtualRegisters(compiler, parser, stmt->retExpr, fn->reg.virtual_register_count++);
            program->index += sizeof(RetAssignStmt);
            break;
        }
    case STATEMENT_VAR_DECL:
        {
            Function* fn = Get<Function>(compiler, compiler->currentFunction);
            VarDeclStm* stmt = Get<VarDeclStm>(compiler, program->index);

            if(stmt->param) {
                u32 g_index = AllocateVirtualRegister(compiler, funcPtr, fn->reg.virtual_register_count++, true,true);
                compiler->registerVariable.PushBack(RegisterSymbol{stmt->var.name,g_index,1});
                if(stmt->preColor) {
                    PreAssignRegister(&fn->reg, g_index, stmt->reg);
                }
            }
            program->index += sizeof(VarDeclStm);
        }
    }

    return true;
}
void RegisterInterference(Compiler* compiler, u32 function) {

    Function* fn = Get<Function>(compiler, function);

    for(u32 i = 0; i < fn->reg.virtualRegisters.size ; i++) {
        u32 first = fn->reg.virtualRegisters[i].firstRef;
        u32 last = fn->reg.virtualRegisters[i].LastRef;
        
        for(u32 k = 0; k < fn->reg.virtualRegisters.size ; k++) {
            if(k == i) continue;

            if( (fn->reg.virtualRegisters[k].firstRef >= first && fn->reg.virtualRegisters[k].firstRef <= last) ||
                (fn->reg.virtualRegisters[k].LastRef >= first && fn->reg.virtualRegisters[k].LastRef <= last)) {
                    InsertEdge(&fn->reg.graph, i,k);
            }
        }
    }
}

struct Pair {
    u32 g_index;
    ExprList* list;
};
void AllocateLocalStorage(Compiler* compiler, Parser* parser, u32 function) {

    Function* fn = Get<Function>(compiler, function);

    fn->scratchAddress = fn->frameSize;
    fn->currentFrameSize += fn->scratchSize;
    fn->frameSize += fn->scratchSize;
    for(u32 i = 0; i < compiler->hiddenParams.size; i++) {
        VariableLoadExpr* param = (VariableLoadExpr*)(parser->mem + compiler->hiddenParams[i]);
        param->var.address = fn->scratchAddress;
    }
    compiler->hiddenParams.Clear();
}
u32 PickSpill(RegisterAllocation* alloc, DynamicBufferSimple<Pair>* spills) {

    u32 ret = 0;
    for(u32 i = 0; i < alloc->virtualRegisters.size ; i++) {

        bool found = false;
        for(u32 k = 0; k < alloc->preAssigned.size; k++) {
            if(i == alloc->preAssigned[k].g_index) {
                found = true;
                break;
            }
        }
        for(u32 k = 0; k < spills->size ; k++) {
            if(i == (*spills)[k].g_index) {
                found = true;
                break;
            }
        }
        if( !found && alloc->virtualRegisters[i].var) {
            ret = i;
            break;
        }
    }
    return ret;
}
void AssignHWRegisters(Compiler* compiler, Parser* parser, Stmt functionEntry, u32 funcPtr) {

    compiler->registerVariable.Clear();
    bool running = true;
    while(running) {
        running = AllocateRegisters(compiler, parser, &functionEntry, funcPtr);
    }

    Function* fn = Get<Function>(compiler, funcPtr);
    DynamicBufferSimple<Pair> spills;
    spills.Init();

    bool not_colorable = true;
    while(not_colorable) {

        
        //std::cout.write( fn->name.text, fn->name.lenght) << std::endl;
        RegisterInterference(compiler, funcPtr);

#if 1
        fn->reg.colors = GreedyGraphColor(&fn->reg.graph, &fn->reg.preAssigned);
#else
        fn->reg.colors = GreedyColoring(&fn->reg.graph, &fn->reg.preAssigned);
#endif
        /*
        PrintGraph(&fn->reg.graph);
        std::cout << "pre assigned" << std::endl;
        for(u32 i = 0; i < fn->reg.preAssigned.size; i++) {
            std::cout << fn->reg.preAssigned[i].g_index << " " << (u32)fn->reg.preAssigned[i].reg << std::endl;
        }
        std::cout << std::endl;
        for(u32 i = 0; i < fn->reg.virtualRegisters.size ; i++) {
            std::cout << i << " " << fn->reg.colors[i] << std::endl;
        }
        std::cout << std::endl;
        for(u32 i = 0; i < fn->reg.virtualRegisters.size ; i++) {
            std::cout << i << " " << fn->reg.virtualRegisters[i].name << " " << (fn->reg.virtualRegisters[i].var ? "true" : "false") << " " << fn->reg.virtualRegisters[i].firstRef << " " << fn->reg.virtualRegisters[i].LastRef << std::endl;
        }
        */

        not_colorable = false;
        for(u32 i = 0; i < fn->reg.virtualRegisters.size ; i++) {
            if(fn->reg.colors[i] > R11) {

                bool f = false;
                for(u32 k = 0; k < fn->reg.preAssigned.size ; k++) {
                    if(i == fn->reg.preAssigned[k].g_index) {
                        f = true;
                        break;
                    }
                }

                if(!f) {
                    not_colorable = true;
                    break;
                }
            }
        }
        if(!not_colorable) {
            break;
        }

        u32 vReg = PickSpill(&fn->reg, &spills);
        std::cout << vReg << " spilled" << std::endl;
        spills.PushBack( Pair{vReg, fn->reg.virtualRegisters[vReg].list} );

        for(u32 i = 0; i < fn->reg.virtualRegisters.size; i++) {

            u32 count = 0;
            for(AdjacencyList* k = fn->reg.graph.adj[i]; k != nullptr; k = k->next,count++);
            AdjacencyList* list[count];
            count = 0;
            for(AdjacencyList* k = fn->reg.graph.adj[i]; k != nullptr; k = k->next,count++) list[count] = k;
            for(u32 k = 0; k < count ; k++) my_free(list[k]);
            fn->reg.graph.adj[i] = nullptr;
        }
        my_free(fn->reg.graph.adj);
        fn->reg.graph.adj = nullptr;
        fn->reg.graph.vertexCount = 0;

        for(ExprList* i = fn->reg.virtualRegisters[vReg].list; i != nullptr ; i = i->next) {
            switch(Cast<ExprType>(parser->mem + i->expr.index) ) {
            default:LOGASSERT(false, "wrong expression");break;
            case EXPRESSION_VARIABLE_LOAD:
                {
                    VariableLoadExpr* node = Get<VariableLoadExpr>(parser, i->expr.index);
                    LoadExprLiteral* q = Get<LoadExprLiteral>(parser, i->expr.index);
                    q->index = EXPRESSION_LOAD;
                    q->address = 0;
                    q->size = 3;
                    q->type = node->var.type;
                }
                break;
            case EXPRESSION_VARIABLE_ASSIGNMENT:
                {
                    VariableAssignmentExpr* node = Get<VariableAssignmentExpr>(parser, i->expr.index);
                    Expr val = node->value;
                    StoreExprLiteral* q = Get<StoreExprLiteral>(parser, i->expr.index);
                    q->index = EXPRESSION_STORE;
                    q->address = 0;
                    q->size = 3;
                    q->expr = val;
                }
                break;
            }
        }

        for(u32 i = 0; i < spills.size ; i++) {
            if(spills[i].g_index == vReg) {
                spills[i].list = fn->reg.virtualRegisters[vReg].list;
                break;
            }
        }

        compiler->currentI = 0;
        my_free(fn->reg.colors);
        for(u32 i = 0; i < fn->reg.virtualRegisters.size; i++) {
            fn->reg.virtualRegisters[i].list = nullptr;
        }
        fn->reg.virtualRegisters.Clear();
        fn->reg.params.Clear();
        fn->reg.preAssigned.Clear();

        fn->reg.virtual_register_count = 0;
        while(AllocateRegisters(compiler, parser, &functionEntry, funcPtr));

        
    }

    FuncFrame* f = Get<FuncFrame>(compiler, functionEntry.index);
    u32 scratchAllocator = compiler->miscAllocator;
    if(f->expressions == 0) {
        f->expressions = compiler->miscAllocator;
    }
    else {
        u32 it = f->expressions;
        while( Cast<ExprType>(compiler->mem + it) != EXPRESSION_NULL) {
            *AllocateMisc<Expr>(compiler) = Cast<Expr>(compiler->mem + it);
            it += sizeof(Expr);
        }
        Cast<ExprType>(compiler->mem + compiler->miscAllocator) = EXPRESSION_NULL;
        f->expressions = scratchAllocator;
    }

    /*
    bool terminate = false;
    bool skip[spills.size]{};
    for(u32 i = 0; i < fn->reg.params.size ; i++) {

        bool f = true;
        u32 k = 0;
        for(; k < spills.size ; k++) {
            if( spills[k].g_index == fn->reg.params[i].g_index) {
                f = false;
                skip[k] = true;
                break;
            }
        }

        if(f) {
            terminate = true;
            AllocateMisc<Expr>(compiler)->index = parser->exprAllocator;
            LoadExprLiteral* load = Allocate<LoadExprLiteral>(parser);
            load->index = EXPRESSION_LOAD;
            load->size = 3;
            load->address = fn->frameSize;
            load->g_index = fn->reg.params[i].g_index;
        }
        else {
            for(ExprList* i = spills[k].list; i != nullptr ; i = i->next) {
                ExprType t = Cast<ExprType>(parser->mem + i->expr.index);
                switch(t) {
                case EXPRESSION_LOAD:
                    {
                        LoadExprLiteral* q = Get<LoadExprLiteral>(parser, i->expr.index);
                        q->address = fn->frameSize;
                    }
                    break;
                case EXPRESSION_VARIABLE_ASSIGNMENT:
                    {
                        StoreExprLiteral* q = Get<StoreExprLiteral>(parser, i->expr.index);
                        q->address = fn->frameSize;
                    }
                    break;
                }
            }
        }
        fn->frameSize += 8;
    }
    if(terminate) {
        *AllocateMisc<ExprType>(compiler) = EXPRESSION_NULL;
    }
    else {
        f->expressions = 0; 
    }

    for(u32 k = 0; k < spills.size ; k++) {
        if(skip[k]) continue;
        for(ExprList* i = spills[k].list; i != nullptr ; i = i->next) {
            switch(Cast<ExprType>(parser->mem + i->expr.index) ) {
            case EXPRESSION_LOAD:
                {
                    LoadExprLiteral* q = Get<LoadExprLiteral>(parser, i->expr.index);
                    q->address = fn->frameSize;
                }
                break;
            case EXPRESSION_VARIABLE_ASSIGNMENT:
                {
                    StoreExprLiteral* q = Get<StoreExprLiteral>(parser, i->expr.index);
                    q->address = fn->frameSize;
                }
                break;
            }
        }
        fn->frameSize += 8;
    }
    */

    for(u32 i = 0; i < compiler->calls.size; i++) {
        CallExpr* call = Get<CallExpr>(parser, compiler->calls[i].call.index);
        
        bool liveRegs[REGISTER_COUNT]{};
        u32 j = compiler->calls[i].i;
        u32 count = 0;
        for(u32 k = 0; k < fn->reg.virtualRegisters.size; k++) {
            if(j >= fn->reg.virtualRegisters[k].firstRef && j <= fn->reg.virtualRegisters[k].LastRef) {
                count++;
                liveRegs[fn->reg.colors[k]] = true;
            }
        }

        call->liveRegisters = (u32*)my_malloc( sizeof(u32) * (count+1) );
        call->liveRegisters[0] = count;
        count = 1;

        for(u32 k = 0; k < REGISTER_COUNT; k++) {
            if(!liveRegs[k]) continue;
            call->liveRegisters[count++] = k;
        }
    }
    compiler->calls.Clear();

    AllocateLocalStorage(compiler, parser, funcPtr);
}



u32 AllocateVirtualRegisters(Compiler* compiler, Parser* parser, Expr expr, u32 resultVReg) {
    
    Expr* e = Get<Expr>(parser , expr.index);
    Function* f = Get<Function>(compiler , compiler->currentFunction);

    switch(e->index) {
    case EXPRESSION_NULL:return ~u32(0);
    case EXPRESSION_IMMEDIATE:
        {
            ImmediateExpr* node = Get<ImmediateExpr>(parser, expr.index);
            node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, false, false);
            return node->g_index;
        }
    case EXPRESSION_LITERAL:
        {
            LiteralExpr* node = Get<LiteralExpr>(parser , expr.index);
            node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, false, false);
            return node->g_index;
        }
        break;
    case EXPRESSION_UNARY:
        {
            UnaryExpr* node = Get<UnaryExpr>(parser , expr.index);

            switch(node->opr) {
            default:LOGASSERT(false, "Uknown unary operation");break;
            case TOKEN_EXCLAMATION_MARK:
            case TOKEN_MINUS:
                {
                    node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, false, false);
                    u32 r0 = AllocateVirtualRegisters(compiler, parser, node->primaryExpr, f->reg.virtual_register_count++);

                    f->reg.virtualRegisters[r0].LastRef = compiler->currentI++;
                    f->reg.virtualRegisters[node->g_index].LastRef = f->reg.virtualRegisters[r0].LastRef;

                    InsertEdge(&f->reg.graph, r0,node->g_index);
                    return node->g_index;
                }
                break;
            case TOKEN_MINUS_MINUS:
            case TOKEN_PLUS_PLUS:
                {
                    u32 r0 = AllocateVirtualRegisters(compiler, parser, node->primaryExpr, resultVReg);
                    node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);
                    
                    f->reg.virtualRegisters[r0].LastRef = compiler->currentI++;
                    f->reg.virtualRegisters[node->g_index].LastRef = f->reg.virtualRegisters[r0].LastRef;

                    InsertEdge(&f->reg.graph, node->g_index,r0);
                    return r0;
                }
                break;
            }
        }
        break;
    case EXPRESSION_BINARY:
        {
            BinaryExpr* node = Get<BinaryExpr>(parser, expr.index);
            node->g_index = ~u32(0);

            Expr ex = node->left;
            ExprType left_t = FirstNotGroupExpr(parser, &ex);
            ex = node->right;
            ExprType right_t = FirstNotGroupExpr(parser, &ex);

            TypeExpr right = GetTypeExpr(compiler, parser, node->left, compiler->scratchMem, TypeExpr{0});
            GetTypeExpr(compiler, parser, node->right, compiler->scratchMem, right);

            TypeName leftType = GetLastType(compiler->scratchMem, TypeExpr{0});
            TypeName rightType = GetLastType(compiler->scratchMem, right);

            switch(node->opr) {
            default:LOGASSERT(false, "Uknown binary operation");break;
            case TOKEN_SUB_SCRIPT_OPR:
            case TOKEN_PLUS:
            case TOKEN_MINUS:
            case TOKEN_ASTERISK:
            case TOKEN_SLASH:
            case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
            case TOKEN_AMPERSAND_AMPERSAND:
            case TOKEN_CIRCUMFLEX:
                {
                    if( left_t == EXPRESSION_VARIABLE_LOAD) {

                        u32 r0 = AllocateVirtualRegisters(compiler, parser, node->left , f->reg.virtual_register_count++);
                        u32 r1 = AllocateVirtualRegisters(compiler, parser, node->right , f->reg.virtual_register_count++);
                        node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, false, false);

                        f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r0].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r1].LastRef = compiler->currentI;

                        InsertEdge(&f->reg.graph, r1,r0);
                        InsertEdge(&f->reg.graph, node->g_index,r1);
                        InsertEdge(&f->reg.graph, r0,node->g_index);

                        return node->g_index;
                    }
                    else {
                        u32 r0 = AllocateVirtualRegisters(compiler, parser, node->left , resultVReg);
                        if( (left_t == EXPRESSION_CALL && right_t == EXPRESSION_CALL) || node->opr == TOKEN_SUB_SCRIPT_OPR || (leftType == TYPE_MODIFIER_POINTER || rightType == TYPE_MODIFIER_POINTER)) {
                            node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);
                        }
                        u32 r1 = AllocateVirtualRegisters(compiler, parser, node->right , f->reg.virtual_register_count++);

                        if( (left_t == EXPRESSION_CALL && right_t == EXPRESSION_CALL) || node->opr == TOKEN_SUB_SCRIPT_OPR || (leftType == TYPE_MODIFIER_POINTER || rightType == TYPE_MODIFIER_POINTER)) {
                            f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
                        }
                        f->reg.virtualRegisters[r0].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r1].LastRef = compiler->currentI;
                        InsertEdge(&f->reg.graph, r0,r1);
                        return r0;
                    }
                }
                break;
            case TOKEN_EXCLAMATION_EQUALS:
            case TOKEN_LSHIFT_EQUALS:
            case TOKEN_RSHIFT_EQUALS:
                {
                    if( left_t == EXPRESSION_VARIABLE_LOAD && right_t == EXPRESSION_VARIABLE_LOAD) {

                        node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, false, false);
                        u32 r0 = AllocateVirtualRegisters(compiler, parser, node->left, f->reg.virtual_register_count++);
                        u32 r1 = AllocateVirtualRegisters(compiler, parser, node->right, f->reg.virtual_register_count++);
                        f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r0].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r1].LastRef = compiler->currentI;

                        InsertEdge(&f->reg.graph, r0,r1);
                        InsertEdge(&f->reg.graph, r0,node->g_index);
                        InsertEdge(&f->reg.graph, r1,node->g_index);

                    }
                    else {
                        node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction , resultVReg, false, false);
                        u32 r0 = AllocateVirtualRegisters(compiler, parser, node->left, f->reg.virtual_register_count++);
                        u32 r1 = AllocateVirtualRegisters(compiler, parser, node->right, f->reg.virtual_register_count++);
                        f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r0].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r1].LastRef = compiler->currentI;

                        InsertEdge(&f->reg.graph, r0,r1);
                        InsertEdge(&f->reg.graph, r0,node->g_index);
                        InsertEdge(&f->reg.graph, r1,node->g_index);
                    }

                    return node->g_index;
                }
                break;
            case TOKEN_EQUALS_EQUALS:
            case TOKEN_RSHIFT:
            case TOKEN_LSHIFT:
                {
                    if( left_t == EXPRESSION_VARIABLE_LOAD && right_t == EXPRESSION_VARIABLE_LOAD) {
                        u32 r0 = AllocateVirtualRegisters(compiler, parser, node->left, f->reg.virtual_register_count++);
                        u32 r1 = AllocateVirtualRegisters(compiler, parser, node->right, f->reg.virtual_register_count++);
                        node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, false, false);
                        f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r0].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r1].LastRef = compiler->currentI;
                        InsertEdge(&f->reg.graph, r0,r1);
                        InsertEdge(&f->reg.graph, r0,node->g_index);
                        InsertEdge(&f->reg.graph, r1,node->g_index);
                        return node->g_index;
                    }
                    else if( left_t == EXPRESSION_VARIABLE_LOAD ) {
                        u32 r0 = AllocateVirtualRegisters(compiler, parser, node->left, f->reg.virtual_register_count++);
                        u32 r1 = AllocateVirtualRegisters(compiler, parser, node->right, resultVReg);
                        f->reg.virtualRegisters[r0].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r1].LastRef = compiler->currentI;
                        InsertEdge(&f->reg.graph, r0,r1);
                        return r1;
                    }
                    else {
                        u32 r0 = AllocateVirtualRegisters(compiler, parser, node->left , f->reg.virtual_register_count++);
                        if( left_t == EXPRESSION_CALL && right_t == EXPRESSION_CALL ) {
                            node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);
                        }
                        u32 r1 = AllocateVirtualRegisters(compiler, parser, node->right , f->reg.virtual_register_count++);
                    
                        if( left_t == EXPRESSION_CALL && right_t == EXPRESSION_CALL ) {
                            f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;
                        }
                        f->reg.virtualRegisters[r0].LastRef = compiler->currentI;
                        f->reg.virtualRegisters[r1].LastRef = compiler->currentI;

                        InsertEdge(&f->reg.graph, r1,r0);

                        return node->g_index;
                    }
                }
                break;
            }
        }
        break;
    case EXPRESSION_GROUP:
        {
            GroupExpr* node = Get<GroupExpr>(parser, expr.index);
            return AllocateVirtualRegisters(compiler, parser ,node->expr, resultVReg);
        }
        break;
    case EXPRESSION_PEEL_TYPE:
        {
            PeelTypeExpr* node = Get<PeelTypeExpr>(parser, expr.index);
            return AllocateVirtualRegisters(compiler, parser ,node->expr, resultVReg);
        }
        break;
    case EXPRESSION_MEM_COPY:
        {
            MemCopyExpr* node = Get<MemCopyExpr>(parser, expr.index);
            u32 vregDst = AllocateVirtualRegisters(compiler, parser, node->dst, f->reg.virtual_register_count++);
            u32 vregSrc = AllocateVirtualRegisters(compiler, parser, node->src, f->reg.virtual_register_count++);

            node->g_index_i = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false,false);
            node->g_index_res = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false,false);
            node->g_index_cmp = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false,false);

            InsertEdge(&f->reg.graph, vregDst, vregSrc);
            InsertEdge(&f->reg.graph, vregDst, node->g_index_i);
            InsertEdge(&f->reg.graph, vregDst, node->g_index_res);
            InsertEdge(&f->reg.graph, vregDst, node->g_index_cmp);

            InsertEdge(&f->reg.graph, vregSrc, node->g_index_i);
            InsertEdge(&f->reg.graph, vregSrc, node->g_index_res);
            InsertEdge(&f->reg.graph, vregSrc, node->g_index_cmp);

            InsertEdge(&f->reg.graph, node->g_index_i, node->g_index_cmp);
            InsertEdge(&f->reg.graph, node->g_index_i, node->g_index_res);

            InsertEdge(&f->reg.graph, node->g_index_cmp, node->g_index_res);

            f->reg.virtualRegisters[vregSrc].LastRef = compiler->currentI;
            f->reg.virtualRegisters[vregDst].LastRef = compiler->currentI;
            f->reg.virtualRegisters[node->g_index_i].LastRef = compiler->currentI;
            f->reg.virtualRegisters[node->g_index_cmp].LastRef = compiler->currentI;
            f->reg.virtualRegisters[node->g_index_res].LastRef = compiler->currentI;

            return vregSrc;
        }
        break;
    case EXPRESSION_VARIABLE_LOAD:
        {
            VariableLoadExpr* node = Get<VariableLoadExpr>(parser, expr.index);

            for(u32 i = 0; i < compiler->registerVariable.size ; i++) {
                if(TokensEquals(node->var.name, compiler->registerVariable[i].name)) {
                    node->var.g_index = compiler->registerVariable[i].g_index;
                    break;
                }
            }

            TypeName t = GetLastType(parser->mem, node->var.type);
            if( t == TYPE_PRIMARY_FN ) {
                node->var.g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, false, false);
            }
            else if(t == TYPE_MODIFIER_ARRAY || t > TYPE_COUNT) {
                Function* f = Get<Function>(compiler, compiler->currentFunction);
                node->var.g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);
                node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, false, false);
                InsertEdge(&f->reg.graph, node->g_index, node->var.g_index);
                node->relative = RBP;
            }
            else {
                if(node->var.g_index == ~u32(0)) node->var.g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);
                f->reg.virtualRegisters[node->var.g_index].LastRef = compiler->currentI++;
                f->reg.virtualRegisters[node->var.g_index].usageCount++;
                //InsertExpr(compiler, compiler->currentFunction, node->var.g_index, expr);
            }

            return node->var.g_index;
        }
        break;
    case EXPRESSION_VARIABLE_ASSIGNMENT:
        {
            VariableAssignmentExpr* node = Get<VariableAssignmentExpr>(parser , expr.index);
            bool found = false;
            for(u32 i = 0; i < compiler->registerVariable.size ; i++) {
                if(TokensEquals(node->var.name, compiler->registerVariable[i].name)) {
                    if(compiler->registerVariable[i].possibleCFpaths == 1) {
                        compiler->registerVariable[i].g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, true, false);
                    }
                    node->var.g_index = compiler->registerVariable[i].g_index;
                    node->g_index = node->var.g_index;
                    found = true;
                    break;
                }
            }
            if(!found) {
                RegisterSymbol symbol;
                symbol.possibleCFpaths = 1;
                symbol.name = node->var.name;
                symbol.g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, true, false);;
                node->var.g_index = symbol.g_index;
                node->g_index = node->var.g_index;
                compiler->registerVariable.PushBack(symbol);
            }

            AllocateVirtualRegisters(compiler, parser, node->value, resultVReg);
            f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI++;
            //InsertExpr(compiler, compiler->currentFunction, node->g_index, expr);
            return node->g_index;
        }
        break;
    case EXPRESSION_MEMORY_ASSIGNMENT:
        {
            MemoryStoreExpr* node = Get<MemoryStoreExpr>(parser , expr.index);
            Expr ex = node->address;
            ExprType t = FirstNotGroupExpr(parser, &ex);

            u32 addReg = AllocateVirtualRegisters(compiler, parser, node->address, f->reg.virtual_register_count++);

            GetTypeExpr(compiler, parser, node->address, compiler->scratchMem, TypeExpr{0});
            if(GetLastType(compiler->scratchMem, TypeExpr{0}) == TYPE_MODIFIER_POINTER) {
                TypeName arr = Cast<TypeName>(compiler->scratchMem + GetNthType(compiler->scratchMem, TypeExpr{0}, 1).index);
                if(arr == TYPE_MODIFIER_ARRAY) {
                    return addReg;
                }
            }

            u32 valReg = AllocateVirtualRegisters(compiler, parser, node->value, resultVReg);
            if(t == EXPRESSION_VARIABLE_LOAD) {
                node->g_index2 = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);
                
                InsertEdge(&f->reg.graph, node->g_index2, valReg);
                InsertEdge(&f->reg.graph, node->g_index2, addReg);
            }
            f->reg.virtualRegisters[addReg].LastRef = compiler->currentI;
            f->reg.virtualRegisters[valReg].LastRef = compiler->currentI;

            InsertEdge(&f->reg.graph, addReg, valReg);

            return valReg;
        }
        break;
    case EXPRESSION_MEMORY_LOAD:
        {
            MemoryLoadExpr* node = Get<MemoryLoadExpr>(parser, expr.index);
            Expr ex = node->address;
            ExprType t = FirstNotGroupExpr(parser, &ex);
            
            u32 addReg = AllocateVirtualRegisters(compiler, parser, node->address, f->reg.virtual_register_count++);

            GetTypeExpr(compiler, parser, node->address, compiler->scratchMem, TypeExpr{0});
            if(GetLastType(compiler->scratchMem, TypeExpr{0}) == TYPE_MODIFIER_POINTER) {
                TypeName arr = Cast<TypeName>(compiler->scratchMem + GetNthType(compiler->scratchMem, TypeExpr{0}, 1).index);
                if(arr == TYPE_MODIFIER_ARRAY) {
                    return addReg;
                }
            }
            node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, false, false);
            if(t == EXPRESSION_VARIABLE_LOAD) {
                node->g_index2 = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);
                
                InsertEdge(&f->reg.graph, node->g_index, node->g_index2);

                InsertEdge(&f->reg.graph, addReg, node->g_index2);
            }

            InsertEdge(&f->reg.graph, node->g_index, addReg);

            return node->g_index;
        }
        break;
    case EXPRESSION_CONVERSION:
        {
            ConversionExpr* node = Get<ConversionExpr>(parser, expr.index);
            
            GetTypeExpr(compiler, parser, node->from, compiler->scratchMem, TypeExpr{0});
            TypeName t = GetLastType(compiler->scratchMem, TypeExpr{0});

            if(Cast<ExprType>(parser->mem + node->from.index) == EXPRESSION_VARIABLE_LOAD && t != TYPE_MODIFIER_ARRAY) {
                
                u32 r0 = AllocateVirtualRegisters(compiler, parser, node->from, 0);
                node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, resultVReg, false, false);
                f->reg.virtualRegisters[r0].LastRef = compiler->currentI;
                InsertEdge(&f->reg.graph, node->g_index, r0);

                return node->g_index;
            } else {
                return AllocateVirtualRegisters(compiler, parser, node->from, resultVReg);
            }
        }
        break;
    case EXPRESSION_GET:
        {
            VariableGetExpr* node = Get<VariableGetExpr>(parser, expr.index);
            node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);

            Expr i = node->prev;
            ExprType l = FirstNotGroupExpr(parser, &i);
            u32 reg = AllocateVirtualRegisters(compiler, parser, i, resultVReg);
            InsertEdge(&f->reg.graph, node->g_index, reg);
            if(l == EXPRESSION_VARIABLE_LOAD) {
                node->g_index_copy = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);

                InsertEdge(&f->reg.graph, node->g_index_copy, reg);
                InsertEdge(&f->reg.graph, node->g_index, node->g_index_copy);


                reg = node->g_index_copy;
            }
            return reg;
        }
        break;
    case EXPRESSION_LOAD:
        {
            LoadExprLiteral* node = Get<LoadExprLiteral>(parser, expr.index);
            node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);
            return node->g_index;
        }
        break;
    case EXPRESSION_STORE:
        {
            StoreExprLiteral* node = Get<StoreExprLiteral>(parser, expr.index);
            return AllocateVirtualRegisters(compiler, parser, node->expr, resultVReg);
        }
        break;
    case EXPRESSION_CALL:
        {
            CallExpr* node = Get<CallExpr>(parser, expr.index);
            compiler->calls.PushBack(FunctionCall{expr,compiler->currentI});

            u32 i = 0;
            u32 size = 0;
            u32 offset = 0;

            for(u32 j = 0; j < compiler->stack.size ; j++) {
                if( compiler->stack[j].scope == 1 ) {
                    offset = j;
                    break;
                }
            }

            u32 address = 8;
            GetTypeExpr(compiler, parser, node->calleeExpr, compiler->scratchMem, TypeExpr{0});
            if(GetLastType(compiler->scratchMem, TypeExpr{0}) == TYPE_PRIMARY_FN) {
                TypeExpr fnExprPtr = GetNthType(compiler->scratchMem, TypeExpr{0}, 0);
                FnTypeExpr fnExpr = Cast<FnTypeExpr>(compiler->scratchMem + fnExprPtr.index);
                
                TypeName returnType = GetLastType(compiler->scratchMem, fnExpr.ret_t);
                if(returnType == TYPE_MODIFIER_ARRAY || returnType > TYPE_COUNT) {
                    f->scratchSize = max<u32>(f->scratchSize, GetTypeSize(parser->mem, compiler->scratchMem, fnExpr.ret_t));
                }
            }


            while( Cast<ExprType>(parser->mem + node->args.index + i * sizeof(ExprType)) != EXPRESSION_NULL) {

                if( i < R4) {

                    ((u32*)compiler->scratchMem)[i] = AllocateVirtualRegisters(compiler, parser, Cast<Expr>(parser->mem + node->args.index + i * sizeof(ExprType)), f->reg.virtual_register_count++);
                    Expr paramExpr = Cast<Expr>(parser->mem + node->args.index + i * sizeof(ExprType));
                    ExprType paramExpr_t = GetLastExprType(parser, &paramExpr);
                   
                    u32 vReg = compiler->stack[i+offset].g_index;
                    if(vReg != ~u32(0)) {
                        if( f->reg.virtualRegisters[vReg].parameter && (f->reg.virtualRegisters[vReg].LastRef <= compiler->currentI) ) {
                            size++;
                            *AllocateMisc<u32>(compiler) = i;
                        }
                    }

                    if( paramExpr_t == EXPRESSION_VARIABLE_LOAD) {

                        VariableLoadExpr* varExpr = Get<VariableLoadExpr>(parser, paramExpr.index);

                        GetTypeExpr(compiler, parser, paramExpr, compiler->scratchMem, TypeExpr{16});
                        TypeName last = GetLastType(compiler->scratchMem, TypeExpr{16});
                        if(last > TYPE_COUNT || last == TYPE_MODIFIER_ARRAY) {
                            Expr newParam{parser->exprAllocator};
                            MemCopyExpr* memCopy = Allocate<MemCopyExpr>(parser);
                            memCopy->block_name = AllocateBlockName(parser);
                            memCopy->index = EXPRESSION_MEM_COPY;
                            memCopy->size = GetTypeSize(parser->mem, compiler->scratchMem, TypeExpr{16});
                            memCopy->src = paramExpr;
                            memCopy->dst.index = parser->exprAllocator;

                            VariableLoadExpr* var = Allocate<VariableLoadExpr>(parser);
                            var->block_name = AllocateBlockName(parser);
                            var->index = EXPRESSION_VARIABLE_LOAD;
                            var->var.address = address;
                            var->var.type = varExpr->var.type;
                            address += memCopy->size;

                            AllocateVirtualRegisters(compiler, parser, newParam, f->reg.virtual_register_count++);
                            var->relative = RSP;
                            Cast<Expr>(parser->mem + node->args.index + i * sizeof(ExprType)) = newParam;
                        }
                        else {

                            Expr valEx = Cast<Expr>(parser->mem + node->args.index + i * sizeof(ExprType));
                            u32 alloc = parser->exprAllocator;

                            VariableAssignmentExpr* assign = Allocate<VariableAssignmentExpr>(parser);
                            assign->block_name = AllocateBlockName(parser);
                            *assign = {};
                            assign->index = EXPRESSION_VARIABLE_ASSIGNMENT;
                            assign->value = valEx;

                            assign->var = varExpr->var;
                            assign->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, true);;

                            PreAssignRegister(&f->reg, assign->var.g_index, (RegisterName)i);

                            Cast<Expr>(parser->mem + node->args.index + i * sizeof(ExprType)).index = alloc;
                            ((u32*)compiler->scratchMem)[i] = assign->var.g_index;
                        }
                    }
                    else {
                        PreAssignRegister(&f->reg, ((u32*)compiler->scratchMem)[i], (RegisterName)i);
                    }
                }
                else {
                    Expr st{parser->exprAllocator};
                    StoreExprLiteral* stNode = Allocate<StoreExprLiteral>(parser);
                    stNode->block_name = AllocateBlockName(parser);
                    stNode->index = EXPRESSION_STORE;
                    stNode->size = 3;
                    stNode->address = address;
                    address += 8;
                    stNode->expr = Cast<Expr>(parser->mem + node->args.index + i * sizeof(Expr));
                    stNode->relative = RSP;
                    Cast<Expr>(parser->mem + node->args.index + i * sizeof(Expr)) = st;
                    AllocateVirtualRegisters(compiler, parser, st, f->reg.virtual_register_count++);
                }
                i++;
            }
            node->g_index = AllocateVirtualRegister(compiler, compiler->currentFunction, f->reg.virtual_register_count++, false, false);
            for(u32 k = 0; k < min<u32>(i,4); k++) {
                u32 g_index = ((u32*)compiler->scratchMem)[k];
                f->reg.virtualRegisters[g_index].LastRef = compiler->currentI;
            }
            u32 callee = AllocateVirtualRegisters(compiler, parser, node->calleeExpr, f->reg.virtual_register_count++);
            f->reg.virtualRegisters[node->g_index].LastRef = compiler->currentI;

            InsertEdge(&f->reg.graph, callee, node->g_index);

            return RRV;
        }
        break;
    }

    ASSERT(false);
    return 0;
}

bool ExprEq(byte* structMem0, byte* structMem1, byte* mem0, byte* mem1, Expr expr0, Expr expr1, bool byValue) {

    Token token0;
    Token token1;
    ExprType t0 = Cast<ExprType>(mem0 + expr0.index);
    ExprType t1 = Cast<ExprType>(mem1 + expr1.index);
    bool eq = true;
    while(t0 == t1 && eq) {
        
        switch(t0) {
        case EXPRESSION_IMMEDIATE:
            if(byValue) {
                eq  = Cast<ImmediateExpr>(mem0 + expr0.index).v.type == Cast<ImmediateExpr>(mem1 + expr1.index).v.type;
                eq &= Cast<u64>(Cast<ImmediateExpr>(mem0 + expr0.index).v.mem) == Cast<u64>(Cast<ImmediateExpr>(mem1 + expr1.index).v.mem);
                return eq;
            }
            return true;
        case EXPRESSION_LITERAL:
            if(byValue) {
                token0 = Cast<LiteralExpr>(mem0 + expr0.index).literal;
                token1 = Cast<LiteralExpr>(mem1 + expr1.index).literal;
                eq = TokensEquals(token0, token1);
                return eq;
            }
            return true;
        case EXPRESSION_GROUP:
            expr0 = Cast<GroupExpr>(mem0 + expr0.index).expr;
            expr1 = Cast<GroupExpr>(mem1 + expr1.index).expr;
            break;
        case EXPRESSION_UNARY:
            eq    = Cast<UnaryExpr>(mem0 + expr0.index).opr == Cast<UnaryExpr>(mem1 + expr1.index).opr;
            expr0 = Cast<UnaryExpr>(mem0 + expr0.index).primaryExpr;
            expr1 = Cast<UnaryExpr>(mem1 + expr1.index).primaryExpr;
            return eq;
            break;
        case EXPRESSION_BINARY:
            eq = Cast<BinaryExpr>(mem0 + expr0.index).opr == Cast<BinaryExpr>(mem1 + expr1.index).opr;
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Cast<BinaryExpr>(mem0 + expr0.index).left, Cast<BinaryExpr>(mem1 + expr1.index).left, byValue);
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Cast<BinaryExpr>(mem0 + expr0.index).right, Cast<BinaryExpr>(mem1 + expr1.index).right, byValue);
            return eq;
            break;
        case EXPRESSION_VARIABLE_LOAD:
            token0 = Cast<VariableLoadExpr>(mem0 + expr0.index).var.name;
            token1 = Cast<VariableLoadExpr>(mem1 + expr1.index).var.name;
            eq = TokensEquals(token0, token1);
            return eq;
            break;
        case EXPRESSION_MEMORY_LOAD:
            eq = ExprEq(structMem0, structMem1, mem0, mem1, Cast<MemoryLoadExpr>(mem0 + expr0.index).address, Cast<MemoryLoadExpr>(mem1 + expr1.index).address, byValue);
            return eq;
            break;
        case EXPRESSION_VARIABLE_ASSIGNMENT:
            eq = ExprEq(structMem0, structMem1, mem0, mem1, Cast<VariableAssignmentExpr>(mem0 + expr0.index).value, Cast<VariableAssignmentExpr>(mem1 + expr1.index).value, byValue);
            token0 = Cast<VariableAssignmentExpr>(mem0 + expr0.index).var.name;
            token1 = Cast<VariableAssignmentExpr>(mem1 + expr1.index).var.name;
            eq &= TokensEquals(token0, token1);
            return eq;
            break;
        case EXPRESSION_MEMORY_ASSIGNMENT:
            eq  = ExprEq(structMem0, structMem1, mem0, mem1, Cast<MemoryStoreExpr>(mem0 + expr0.index).value, Cast<MemoryStoreExpr>(mem1 + expr1.index).value, byValue);
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Cast<MemoryStoreExpr>(mem0 + expr0.index).address, Cast<MemoryStoreExpr>(mem1 + expr1.index).address, byValue);
            return eq;
            break;
        case EXPRESSION_ADDRESS_OF:
            ASSERT(false);
            break;
        case EXPRESSION_GET:
            token0 = Cast<VariableGetExpr>(mem0 + expr0.index).name;
            token1 = Cast<VariableGetExpr>(mem1 + expr1.index).name;
            eq = TokensEquals(token0, token1);
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Cast<VariableGetExpr>(mem0 + expr0.index).prev, Cast<VariableGetExpr>(mem1 + expr1.index).prev, byValue);
            return eq;
            break;
        case EXPRESSION_PEEL_TYPE:
            expr0 = Cast<PeelTypeExpr>(mem0 + expr0.index).expr;
            expr1 = Cast<PeelTypeExpr>(mem1 + expr1.index).expr;
            break;
        case EXPRESSION_MEM_COPY:
            eq = Cast<MemCopyExpr>(mem0 + expr0.index).size == Cast<MemCopyExpr>(mem1 + expr1.index).size;
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Cast<MemCopyExpr>(mem0 + expr0.index).dst, Cast<MemCopyExpr>(mem1 + expr1.index).dst, byValue);
            eq &= ExprEq(structMem0, structMem1, mem0, mem1, Cast<MemCopyExpr>(mem0 + expr0.index).src, Cast<MemCopyExpr>(mem1 + expr1.index).src, byValue);
            return eq;
            break;
        case EXPRESSION_CALL:
            {
                eq = ExprEq(structMem0, structMem1, mem0, mem1, Cast<CallExpr>(mem0 + expr0.index).calleeExpr, Cast<CallExpr>(mem1 + expr1.index).calleeExpr, byValue);
                u32 i0 = Cast<CallExpr>(mem0 + expr0.index).args.index;
                u32 i1 = Cast<CallExpr>(mem1 + expr1.index).args.index;
                while(Cast<Expr>(mem0 + i0).index != 0 && Cast<Expr>(mem1 + i1).index != 0) {
                    
                    Expr p0 = Cast<Expr>(mem0 + i0);
                    Expr p1 = Cast<Expr>(mem1 + i1);
                    if(!ExprEq(structMem0, structMem1, mem0, mem1, p0, p1, byValue)) return false;
                    i0 += sizeof(Expr);
                    i1 += sizeof(Expr);
                }
                eq &= Cast<Expr>(mem0 + i0).index == Cast<Expr>(mem1 + i1).index;
                return eq;
            }
            break;
        case EXPRESSION_CONVERSION:
            eq = TypesEqual(structMem0, structMem1, mem0, mem1, Cast<ConversionExpr>(mem0 + expr0.index).type, Cast<ConversionExpr>(mem1 + expr1.index).type);
            if(!eq) return false;
            expr0 = Cast<ConversionExpr>(mem0 + expr0.index).from;
            expr1 = Cast<ConversionExpr>(mem1 + expr1.index).from;
            break;
        case EXPRESSION_LOAD:break;
        case EXPRESSION_STORE:break;
        }

        t0 = Cast<ExprType>(mem0 + expr0.index);
        t1 = Cast<ExprType>(mem1 + expr1.index);
    }

    return t0 == t1;
}

void PrintValue(Value v) {
    switch (v.type) {
    case TYPE_MODIFIER_POINTER:       
    std::cout << Cast<char*>(v.mem);break;
    case TYPE_PRIMARY_BOOL:
    std::cout << (Cast<bool>(v.mem) ? "true" : "false");break;
    case TYPE_PRIMARY_INT8:
    std::cout << Cast<i8>(v.mem);break;
    case TYPE_PRIMARY_INT16:
    std::cout << Cast<i16>(v.mem);break;
    case TYPE_PRIMARY_INT32:
    std::cout << Cast<i32>(v.mem);break;
    case TYPE_PRIMARY_INT64:
    std::cout << Cast<i64>(v.mem);break;
    case TYPE_PRIMARY_UINT8:
    std::cout << Cast<u8>(v.mem);break;
    case TYPE_PRIMARY_UINT16:
    std::cout << Cast<u16>(v.mem);break;
    case TYPE_PRIMARY_UINT32:
    std::cout << Cast<u32>(v.mem);break;
    case TYPE_PRIMARY_UINT64:
    std::cout << Cast<u64>(v.mem);break;
    case TYPE_PRIMARY_F32:
    std::cout << Cast<f32>(v.mem);break;
    case TYPE_PRIMARY_F64:
    std::cout << Cast<f64>(v.mem);break;
    }
}
Value EvaluateConstExpression(Compiler* compiler, Parser* parser, Expr expr) {

    ExprType e = Cast<ExprType>(parser->mem + expr.index);
    switch (e) {
        case EXPRESSION_IMMEDIATE:
            {
                ImmediateExpr* node = Get<ImmediateExpr>(parser, expr.index);
                return node->v;
            }
        case EXPRESSION_LITERAL:
            {
                Value ret;
                LiteralExpr* node = Get<LiteralExpr>(parser, expr.index);
                TypeName t = GetLiteralType(node->literal);
                switch(t) {
                case TYPE_PRIMARY_INT64:
                    {   
                        Cast<i64>(ret.mem) = GetIntFromToken(node->literal);
                        ret.type = TYPE_PRIMARY_INT64;
                    }
                    break;
                case TYPE_PRIMARY_F32:
                    {
                        Cast<f32>(ret.mem) =  GetF32FromToken(node->literal);
                        ret.type = TYPE_PRIMARY_F32;
                    }
                    break;
                case TYPE_PRIMARY_F64:
                    {
                        Cast<f64>(ret.mem) =  GetF64FromToken(node->literal);
                        ret.type = TYPE_PRIMARY_F64;
                    }
                    break;
                case TYPE_PRIMARY_BOOL:
                    {
                        Cast<bool>(ret.mem) =  GetBoolFromToken(node->literal);
                        ret.type = TYPE_PRIMARY_BOOL;
                    }
                    break;
                case TYPE_PRIMARY_CHAR:
                    {
                        char c = node->literal.text[0];
                        if( node->literal.text[0] == '\\' && node->literal.text[1] == 'n') c = '\n';
                        Cast<char>(ret.mem) = c;
                        ret.type = TYPE_PRIMARY_CHAR;
                    }
                    break;
                case TYPE_MODIFIER_POINTER: 
                    {
                        u16 size = 0;
                        char str[node->literal.lenght+1]{};
                        for(u32 i = 0; i < node->literal.lenght; i++,size++) {
                            char c;
                            if( node->literal.text[i] == '\\' && node->literal.text[i+1] == 'n' ) {
                                c = '\n';
                                i += 2;
                            }
                            else {
                                c = node->literal.text[i];
                            }
                            str[size] = c;
                        }                   
                        str[size] = 0;
                        size++;
                        Cast<char*>(ret.mem) = (char*)(compiler->mem + compiler->miscAllocator);
                        compiler->miscAllocator += size;
                        MemCpy(Cast<char*>(ret.mem), str, size);
                        ret.type = TYPE_MODIFIER_POINTER;
                    }
                    break;
                    default:LOGASSERT(false, "Unkown literal type") break;
                }
                return ret;
                
            }
        case EXPRESSION_GROUP:
            {
                GroupExpr* node = Get<GroupExpr>(parser, expr.index);
                return EvaluateConstExpression(compiler, parser, node->expr);
            }
        case EXPRESSION_UNARY:
            {

                UnaryExpr* node = Get<UnaryExpr>(parser, expr.index);
                Value primary = EvaluateConstExpression(compiler, parser, node->primaryExpr);
                switch (node->opr) {
                    case TOKEN_MINUS_MINUS:
                        switch(primary.type) {
                        case TYPE_PRIMARY_INT8:
                            Cast<i8>(primary.mem)--;break;
                        case TYPE_PRIMARY_INT16:
                            Cast<i16>(primary.mem)--;break;
                        case TYPE_PRIMARY_INT32:
                            Cast<i32>(primary.mem)--;break;
                        case TYPE_PRIMARY_INT64:
                            Cast<i64>(primary.mem)--;break;
                        case TYPE_PRIMARY_UINT8:
                            Cast<u8>(primary.mem)--;break;
                        case TYPE_PRIMARY_UINT16:
                            Cast<u16>(primary.mem)--;break;
                        case TYPE_PRIMARY_UINT32:
                            Cast<u32>(primary.mem)--;break;
                        case TYPE_PRIMARY_UINT64:
                            Cast<u64>(primary.mem)--;break;
                        case TYPE_PRIMARY_F32:
                            Cast<f32>(primary.mem)--;break;
                        case TYPE_PRIMARY_F64:
                            Cast<f64>(primary.mem)--;break;
                        }
                        break;
                    case TOKEN_PLUS_PLUS:
                        switch(primary.type) {
                        case TYPE_PRIMARY_INT8:
                            Cast<i8>(primary.mem)++;break;
                        case TYPE_PRIMARY_INT16:
                            Cast<i16>(primary.mem)++;break;
                        case TYPE_PRIMARY_INT32:
                            Cast<i32>(primary.mem)++;break;
                        case TYPE_PRIMARY_INT64:
                            Cast<i64>(primary.mem)++;break;
                        case TYPE_PRIMARY_UINT8:
                            Cast<u8>(primary.mem)++;break;
                        case TYPE_PRIMARY_UINT16:
                            Cast<u16>(primary.mem)++;break;
                        case TYPE_PRIMARY_UINT32:
                            Cast<u32>(primary.mem)++;break;
                        case TYPE_PRIMARY_UINT64:
                            Cast<u64>(primary.mem)++;break;
                        case TYPE_PRIMARY_F32:
                            Cast<f32>(primary.mem)++;break;
                        case TYPE_PRIMARY_F64:
                            Cast<f64>(primary.mem)++;break;
                        }
                        break;

                    case TOKEN_EXCLAMATION_MARK:
                        Cast<bool>(primary.mem) = !Cast<bool>(primary.mem);break;
                        break;

                    case TOKEN_MINUS:
                        switch(primary.type) {
                        case TYPE_PRIMARY_INT8:
                        Cast<i8>(primary.mem) = -Cast<i8>(primary.mem);break;
                        case TYPE_PRIMARY_INT16:
                        Cast<i16>(primary.mem) = -Cast<i16>(primary.mem);break;
                        case TYPE_PRIMARY_INT32:
                        Cast<i32>(primary.mem) = -Cast<i32>(primary.mem);break;
                        case TYPE_PRIMARY_INT64:
                        Cast<i64>(primary.mem) = -Cast<i64>(primary.mem);break;
                        case TYPE_PRIMARY_UINT8:
                        Cast<u8>(primary.mem) = -Cast<u8>(primary.mem);break;
                        case TYPE_PRIMARY_UINT16:
                        Cast<u16>(primary.mem) = -Cast<u16>(primary.mem);break;
                        case TYPE_PRIMARY_UINT32:
                        Cast<u32>(primary.mem) = -Cast<u32>(primary.mem);break;
                        case TYPE_PRIMARY_UINT64:case TYPE_MODIFIER_POINTER:
                        Cast<u64>(primary.mem) = -Cast<u64>(primary.mem);break;
                        case TYPE_PRIMARY_F32:
                        Cast<f32>(primary.mem) = -Cast<f32>(primary.mem);break;
                        case TYPE_PRIMARY_F64:
                        Cast<f64>(primary.mem) = -Cast<f64>(primary.mem);break;
                        }
                        break;
                }
                return primary;
            }
        case EXPRESSION_BINARY:
            {
                BinaryExpr* node = Get<BinaryExpr>(parser, expr.index);
                Value left = EvaluateConstExpression(compiler, parser, node->left);
                Value right = EvaluateConstExpression(compiler, parser, node->right);

                GetTypeExpr(compiler, parser, node->left, compiler->scratchMem ,TypeExpr{0});
                TypeName t = GetLastType(compiler->scratchMem , TypeExpr{0});
                u32 size = 0;
                if(t == TYPE_MODIFIER_POINTER) {
                    
                    TypeExpr last = GetNthType(compiler->scratchMem, TypeExpr{0}, 0);
                    TypeExpr nth = GetNthType(compiler->scratchMem, TypeExpr{0}, 1);

                    Cast<TypeName>(compiler->scratchMem + last.index) = TYPE_NON;
                    size = GetTypeSize(parser->mem, compiler->scratchMem, nth);
                }
                else if(t == TYPE_MODIFIER_ARRAY) {
                    size = GetTypeSize(parser->mem, compiler->scratchMem, TypeExpr{0});
                    TypeExpr last = GetNthType(compiler->scratchMem, TypeExpr{0}, 0);
                    size /= Cast<ArrayTypeExpr>(compiler->scratchMem + last.index).array_size;
                }
                    
                switch(node->opr) {
                case TOKEN_SUB_SCRIPT_OPR:
                    switch(right.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<u64>(left.mem) += Cast<i8>(left.mem) * size;break;
                    case TYPE_PRIMARY_INT16:
                    Cast<u64>(left.mem) += Cast<i16>(left.mem) * size;break;
                    case TYPE_PRIMARY_INT32:
                    Cast<u64>(left.mem) += Cast<i32>(left.mem) * size;break;
                    case TYPE_PRIMARY_INT64:
                    Cast<u64>(left.mem) += Cast<i64>(left.mem) * size;break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<u64>(left.mem) += Cast<u8>(left.mem) * size;break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<u64>(left.mem) += Cast<u16>(left.mem) * size;break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<u64>(left.mem) += Cast<u32>(left.mem) * size;break;
                    case TYPE_PRIMARY_UINT64:
                    Cast<u64>(left.mem) += Cast<u64>(left.mem) * size;break;
                    }break;
                case TOKEN_LSHIFT_EQUALS:
                    switch(left.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<bool>(left.mem) = Cast<i8>(left.mem) <= Cast<i8>(right.mem);break;
                    case TYPE_PRIMARY_INT16:
                    Cast<bool>(left.mem) = Cast<i16>(left.mem) <= Cast<i16>(right.mem);break;
                    case TYPE_PRIMARY_INT32:
                    Cast<bool>(left.mem) = Cast<i32>(left.mem) <= Cast<i32>(right.mem);break;
                    case TYPE_PRIMARY_INT64:
                    Cast<bool>(left.mem) = Cast<i64>(left.mem) <= Cast<i64>(right.mem);break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<bool>(left.mem) = Cast<u8>(left.mem) <= Cast<u8>(right.mem);break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<bool>(left.mem) = Cast<u16>(left.mem) <= Cast<u16>(right.mem);break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<bool>(left.mem) = Cast<u32>(left.mem) <= Cast<u32>(right.mem);break;
                    case TYPE_MODIFIER_POINTER:
                    case TYPE_PRIMARY_UINT64:
                    Cast<bool>(left.mem) = Cast<u64>(left.mem) <= Cast<u64>(right.mem);break;
                    case TYPE_PRIMARY_F32:
                    Cast<bool>(left.mem) = Cast<f32>(left.mem) <= Cast<f32>(right.mem);break;
                    case TYPE_PRIMARY_F64:
                    Cast<bool>(left.mem) = Cast<f64>(left.mem) <= Cast<f64>(right.mem);break;
                    }break;
                case TOKEN_RSHIFT_EQUALS:
                    switch(left.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<bool>(left.mem) = Cast<i8>(left.mem) >= Cast<i8>(right.mem);break;
                    case TYPE_PRIMARY_INT16:
                    Cast<bool>(left.mem) = Cast<i16>(left.mem) >= Cast<i16>(right.mem);break;
                    case TYPE_PRIMARY_INT32:
                    Cast<bool>(left.mem) = Cast<i32>(left.mem) >= Cast<i32>(right.mem);break;
                    case TYPE_PRIMARY_INT64:
                    Cast<bool>(left.mem) = Cast<i64>(left.mem) >= Cast<i64>(right.mem);break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<bool>(left.mem) = Cast<u8>(left.mem) >= Cast<u8>(right.mem);break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<bool>(left.mem) = Cast<u16>(left.mem) >= Cast<u16>(right.mem);break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<bool>(left.mem) = Cast<u32>(left.mem) >= Cast<u32>(right.mem);break;
                    case TYPE_MODIFIER_POINTER:
                    case TYPE_PRIMARY_UINT64:
                    Cast<bool>(left.mem) = Cast<u64>(left.mem) >= Cast<u64>(right.mem);break;
                    case TYPE_PRIMARY_F32:
                    Cast<bool>(left.mem) = Cast<f32>(left.mem) >= Cast<f32>(right.mem);break;
                    case TYPE_PRIMARY_F64:
                    Cast<bool>(left.mem) = Cast<f64>(left.mem) >= Cast<f64>(right.mem);break;
                    }break;
                case TOKEN_EXCLAMATION_EQUALS:
                    switch(left.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<bool>(left.mem) = Cast<i8>(left.mem) != Cast<i8>(right.mem);break;
                    case TYPE_PRIMARY_INT16:
                    Cast<bool>(left.mem) = Cast<i16>(left.mem) != Cast<i16>(right.mem);break;
                    case TYPE_PRIMARY_INT32:
                    Cast<bool>(left.mem) = Cast<i32>(left.mem) != Cast<i32>(right.mem);break;
                    case TYPE_PRIMARY_INT64:
                    Cast<bool>(left.mem) = Cast<i64>(left.mem) != Cast<i64>(right.mem);break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<bool>(left.mem) = Cast<u8>(left.mem) != Cast<u8>(right.mem);break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<bool>(left.mem) = Cast<u16>(left.mem) != Cast<u16>(right.mem);break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<bool>(left.mem) = Cast<u32>(left.mem) != Cast<u32>(right.mem);break;
                    case TYPE_MODIFIER_POINTER:
                    case TYPE_PRIMARY_UINT64:
                    Cast<bool>(left.mem) = Cast<u64>(left.mem) != Cast<u64>(right.mem);break;
                    case TYPE_PRIMARY_F32:
                    Cast<bool>(left.mem) = Cast<f32>(left.mem) != Cast<f32>(right.mem);break;
                    case TYPE_PRIMARY_F64:
                    Cast<bool>(left.mem) = Cast<f64>(left.mem) != Cast<f64>(right.mem);break;
                    }break;
                case TOKEN_LSHIFT:
                    switch(left.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<bool>(left.mem) = Cast<i8>(left.mem) < Cast<i8>(right.mem);break;
                    case TYPE_PRIMARY_INT16:
                    Cast<bool>(left.mem) = Cast<i16>(left.mem) < Cast<i16>(right.mem);break;
                    case TYPE_PRIMARY_INT32:
                    Cast<bool>(left.mem) = Cast<i32>(left.mem) < Cast<i32>(right.mem);break;
                    case TYPE_PRIMARY_INT64:
                    Cast<bool>(left.mem) = Cast<i64>(left.mem) < Cast<i64>(right.mem);break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<bool>(left.mem) = Cast<u8>(left.mem) < Cast<u8>(right.mem);break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<bool>(left.mem) = Cast<u16>(left.mem) < Cast<u16>(right.mem);break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<bool>(left.mem) = Cast<u32>(left.mem) < Cast<u32>(right.mem);break;
                    case TYPE_MODIFIER_POINTER:
                    case TYPE_PRIMARY_UINT64:
                    Cast<bool>(left.mem) = Cast<u64>(left.mem) < Cast<u64>(right.mem);break;
                    case TYPE_PRIMARY_F32:
                    Cast<bool>(left.mem) = Cast<f32>(left.mem) < Cast<f32>(right.mem);break;
                    case TYPE_PRIMARY_F64:
                    Cast<bool>(left.mem) = Cast<f64>(left.mem) < Cast<f64>(right.mem);break;
                    }break;
                case TOKEN_RSHIFT:
                    switch(left.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<bool>(left.mem) = Cast<i8>(left.mem) > Cast<i8>(right.mem);break;
                    case TYPE_PRIMARY_INT16:
                    Cast<bool>(left.mem) = Cast<i16>(left.mem) > Cast<i16>(right.mem);break;
                    case TYPE_PRIMARY_INT32:
                    Cast<bool>(left.mem) = Cast<i32>(left.mem) > Cast<i32>(right.mem);break;
                    case TYPE_PRIMARY_INT64:
                    Cast<bool>(left.mem) = Cast<i64>(left.mem) > Cast<i64>(right.mem);break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<bool>(left.mem) = Cast<u8>(left.mem) > Cast<u8>(right.mem);break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<bool>(left.mem) = Cast<u16>(left.mem) > Cast<u16>(right.mem);break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<bool>(left.mem) = Cast<u32>(left.mem) > Cast<u32>(right.mem);break;
                    case TYPE_MODIFIER_POINTER:
                    case TYPE_PRIMARY_UINT64:
                    Cast<bool>(left.mem) = Cast<u64>(left.mem) > Cast<u64>(right.mem);break;
                    case TYPE_PRIMARY_F32:
                    Cast<bool>(left.mem) = Cast<f32>(left.mem) > Cast<f32>(right.mem);break;
                    case TYPE_PRIMARY_F64:
                    Cast<bool>(left.mem) = Cast<f64>(left.mem) > Cast<f64>(right.mem);break;
                    }break;
                case TOKEN_AMPERSAND_AMPERSAND:
                    Cast<bool>(left.mem) = Cast<bool>(left.mem) && Cast<bool>(right.mem);break;
                case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
                    Cast<bool>(left.mem) = Cast<bool>(left.mem) || Cast<bool>(right.mem);break;
                case TOKEN_EQUALS_EQUALS:
                    switch(left.type) {
                    case TYPE_PRIMARY_BOOL:
                    Cast<bool>(left.mem) = Cast<bool>(left.mem) == Cast<bool>(right.mem);break;
                    case TYPE_PRIMARY_INT8:
                    Cast<bool>(left.mem) = Cast<i8>(left.mem) == Cast<i8>(right.mem);break;
                    case TYPE_PRIMARY_INT16:
                    Cast<bool>(left.mem) = Cast<i16>(left.mem) == Cast<i16>(right.mem);break;
                    case TYPE_PRIMARY_INT32:
                    Cast<bool>(left.mem) = Cast<i32>(left.mem) == Cast<i32>(right.mem);break;
                    case TYPE_PRIMARY_INT64:
                    Cast<bool>(left.mem) = Cast<i64>(left.mem) == Cast<i64>(right.mem);break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<bool>(left.mem) = Cast<u8>(left.mem) == Cast<u8>(right.mem);break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<bool>(left.mem) = Cast<u16>(left.mem) == Cast<u16>(right.mem);break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<bool>(left.mem) = Cast<u32>(left.mem) == Cast<u32>(right.mem);break;
                    case TYPE_MODIFIER_POINTER:
                    case TYPE_PRIMARY_UINT64:
                    Cast<bool>(left.mem) = Cast<u64>(left.mem) == Cast<u64>(right.mem);break;
                    case TYPE_PRIMARY_F32:
                    Cast<bool>(left.mem) = Cast<f32>(left.mem) == Cast<f32>(right.mem);break;
                    case TYPE_PRIMARY_F64:
                    Cast<bool>(left.mem) = Cast<f64>(left.mem) == Cast<f64>(right.mem);break;
                    }break;
                case TOKEN_VERTICAL_BAR:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) | Cast<u64>(right.mem);break;
                case TOKEN_CIRCUMFLEX:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) ^ Cast<u64>(right.mem);break;
                case TOKEN_AMPERSAND:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) & Cast<u64>(right.mem);break;
                case TOKEN_ASTERISK:
                    switch(left.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<i8>(left.mem) = Cast<i8>(left.mem) * Cast<i8>(right.mem);break;
                    case TYPE_PRIMARY_INT16:
                    Cast<i16>(left.mem) = Cast<i16>(left.mem) * Cast<i16>(right.mem);break;
                    case TYPE_PRIMARY_INT32:
                    Cast<i32>(left.mem) = Cast<i32>(left.mem) * Cast<i32>(right.mem);break;
                    case TYPE_PRIMARY_INT64:
                    Cast<i64>(left.mem) = Cast<i64>(left.mem) * Cast<i64>(right.mem);break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<u8>(left.mem) = Cast<u8>(left.mem) * Cast<u8>(right.mem);break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<u16>(left.mem) = Cast<u16>(left.mem) * Cast<u16>(right.mem);break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<u32>(left.mem) = Cast<u32>(left.mem) * Cast<u32>(right.mem);break;
                    case TYPE_PRIMARY_UINT64:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) * Cast<u64>(right.mem);break;
                    case TYPE_PRIMARY_F32:
                    Cast<f32>(left.mem) = Cast<f32>(left.mem) * Cast<f32>(right.mem);break;
                    case TYPE_PRIMARY_F64:
                    Cast<f64>(left.mem) = Cast<f64>(left.mem) * Cast<f64>(right.mem);break;
                    }break;
                case TOKEN_SLASH:
                    switch(left.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<i8>(left.mem) = Cast<i8>(left.mem) / Cast<i8>(right.mem);break;
                    case TYPE_PRIMARY_INT16:
                    Cast<i16>(left.mem) = Cast<i16>(left.mem) / Cast<i16>(right.mem);break;
                    case TYPE_PRIMARY_INT32:
                    Cast<i32>(left.mem) = Cast<i32>(left.mem) / Cast<i32>(right.mem);break;
                    case TYPE_PRIMARY_INT64:
                    Cast<i64>(left.mem) = Cast<i64>(left.mem) / Cast<i64>(right.mem);break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<u8>(left.mem) = Cast<u8>(left.mem) / Cast<u8>(right.mem);break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<u16>(left.mem) = Cast<u16>(left.mem) / Cast<u16>(right.mem);break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<u32>(left.mem) = Cast<u32>(left.mem) / Cast<u32>(right.mem);break;
                    case TYPE_PRIMARY_UINT64:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) / Cast<u64>(right.mem);break;
                    case TYPE_PRIMARY_F32:
                    Cast<f32>(left.mem) = Cast<f32>(left.mem) / Cast<f32>(right.mem);break;
                    case TYPE_PRIMARY_F64:
                    Cast<f64>(left.mem) = Cast<f64>(left.mem) / Cast<f64>(right.mem);break;
                    }break;
                case TOKEN_PLUS:
                    switch(left.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<i8>(left.mem) = Cast<i8>(left.mem) + Cast<i8>(right.mem);break;
                    case TYPE_PRIMARY_INT16:
                    Cast<i16>(left.mem) = Cast<i16>(left.mem) + Cast<i16>(right.mem);break;
                    case TYPE_PRIMARY_INT32:
                    Cast<i32>(left.mem) = Cast<i32>(left.mem) + Cast<i32>(right.mem);break;
                    case TYPE_PRIMARY_INT64:
                    Cast<i64>(left.mem) = Cast<i64>(left.mem) + Cast<i64>(right.mem);break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<u8>(left.mem) = Cast<u8>(left.mem) + Cast<u8>(right.mem);break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<u16>(left.mem) = Cast<u16>(left.mem) + Cast<u16>(right.mem);break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<u32>(left.mem) = Cast<u32>(left.mem) + Cast<u32>(right.mem);break;
                    case TYPE_PRIMARY_UINT64:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) + Cast<u64>(right.mem);break;
                    case TYPE_PRIMARY_F32:
                    Cast<f32>(left.mem) = Cast<f32>(left.mem) + Cast<f32>(right.mem);break;
                    case TYPE_PRIMARY_F64:
                    Cast<f64>(left.mem) = Cast<f64>(left.mem) + Cast<f64>(right.mem);break;

                    case TYPE_MODIFIER_POINTER:
                    switch(right.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) + Cast<i8>(right.mem) * size;break;
                    case TYPE_PRIMARY_INT16:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) + Cast<i16>(right.mem) * size;break;
                    case TYPE_PRIMARY_INT32:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) + Cast<i32>(right.mem) * size;break;
                    case TYPE_PRIMARY_INT64:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) + Cast<i64>(right.mem) * size;break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) + Cast<u8>(right.mem) * size;break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) + Cast<u16>(right.mem) * size;break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) + Cast<u32>(right.mem) * size;break;
                    case TYPE_PRIMARY_UINT64:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) + Cast<u64>(right.mem) * size;break;
                    }
                    break;
                    }break;
                case TOKEN_MINUS:
                    switch(left.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<i8>(left.mem) = Cast<i8>(left.mem) - Cast<i8>(right.mem);break;
                    case TYPE_PRIMARY_INT16:
                    Cast<i16>(left.mem) = Cast<i16>(left.mem) - Cast<i16>(right.mem);break;
                    case TYPE_PRIMARY_INT32:
                    Cast<i32>(left.mem) = Cast<i32>(left.mem) - Cast<i32>(right.mem);break;
                    case TYPE_PRIMARY_INT64:
                    Cast<i64>(left.mem) = Cast<i64>(left.mem) - Cast<i64>(right.mem);break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<u8>(left.mem) = Cast<u8>(left.mem) - Cast<u8>(right.mem);break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<u16>(left.mem) = Cast<u16>(left.mem) - Cast<u16>(right.mem);break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<u32>(left.mem) = Cast<u32>(left.mem) - Cast<u32>(right.mem);break;
                    case TYPE_PRIMARY_UINT64:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) - Cast<u64>(right.mem);break;
                    case TYPE_PRIMARY_F32:
                    Cast<f32>(left.mem) = Cast<f32>(left.mem) - Cast<f32>(right.mem);break;
                    case TYPE_PRIMARY_F64:
                    Cast<f64>(left.mem) = Cast<f64>(left.mem) - Cast<f64>(right.mem);break;

                    case TYPE_MODIFIER_POINTER:
                    switch(right.type) {
                    case TYPE_PRIMARY_INT8:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) - Cast<i8>(right.mem) * size;break;
                    case TYPE_PRIMARY_INT16:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) - Cast<i16>(right.mem) * size;break;
                    case TYPE_PRIMARY_INT32:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) - Cast<i32>(right.mem) * size;break;
                    case TYPE_PRIMARY_INT64:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) - Cast<i64>(right.mem) * size;break;
                    case TYPE_PRIMARY_UINT8:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) - Cast<u8>(right.mem) * size;break;
                    case TYPE_PRIMARY_UINT16:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) - Cast<u16>(right.mem) * size;break;
                    case TYPE_PRIMARY_UINT32:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) - Cast<u32>(right.mem) * size;break;
                    case TYPE_PRIMARY_UINT64:
                    Cast<u64>(left.mem) = Cast<u64>(left.mem) - Cast<u64>(right.mem) * size;break;
                    }
                    break;
                    }break;
                }
                return left;
            }
        case EXPRESSION_VARIABLE_LOAD:
            {
                VariableLoadExpr* node = Get<VariableLoadExpr>(parser, expr.index);
                u32 index = FindVariable(&compiler->stack, node->var.name);
                TypeName t = GetLastType(parser->mem, node->var.type);
                if(t == TYPE_MODIFIER_ARRAY || t > TYPE_COUNT) {
                    Value v;
                    Cast<i64>(v.mem) = node->var.address;
                    v.type = TYPE_PRIMARY_INT64;
                    return v;
                }
                return EvaluateConstExpression(compiler, parser, compiler->stack[index].val);
            }
        case EXPRESSION_MEMORY_LOAD:
            {
                MemoryLoadExpr* node = Get<MemoryLoadExpr>(parser, expr.index);
                if(IsConstExpression(compiler, parser, node->address)) {
                    return EvaluateConstExpression(compiler, parser, node->address);
                }
                return {};
            }
        case EXPRESSION_VARIABLE_ASSIGNMENT:
            {
                VariableAssignmentExpr* node = Get<VariableAssignmentExpr>(parser, expr.index);
                u32 index = FindVariable(&compiler->stack, node->var.name);
                Value q = EvaluateConstExpression(compiler, parser, node->value);
                if(index == ~u32(0)) index = compiler->stack.PushBack(node->var);
                compiler->stack[index].val = node->value;
                return q;
            }
        case EXPRESSION_MEMORY_ASSIGNMENT:
            {
                MemoryStoreExpr* node = Get<MemoryStoreExpr>(parser, expr.index);
                if(IsConstExpression(compiler, parser, node->address)) {
                    EvaluateConstExpression(compiler, parser, node->address);
                }
                if(IsConstExpression(compiler, parser, node->value)) {
                    EvaluateConstExpression(compiler, parser, node->value);
                }
                return {};
            }
        case EXPRESSION_ADDRESS_OF:
        case EXPRESSION_GET:
            {
                VariableGetExpr* node = Get<VariableGetExpr>(parser, expr.index);
                Value v;
                Cast<u64>(v.mem) = Cast<u64>(EvaluateConstExpression(compiler, parser, node->prev).mem);
                Cast<u64>(v.mem) += MemberOffset(compiler, parser, node->name, node->prev);
                return v;
            }
        case EXPRESSION_PEEL_TYPE:
            {
                PeelTypeExpr* node = Get<PeelTypeExpr>(parser, expr.index);
                return EvaluateConstExpression(compiler, parser, node->expr);
            }
        case EXPRESSION_MEM_COPY:
            {
                return {};
                break;
            }
        case EXPRESSION_CALL:
            {
                return {};
                break;
            }
        case EXPRESSION_CONVERSION:
            {
                ConversionExpr* node = Get<ConversionExpr>(parser, expr.index);
                Value v = EvaluateConstExpression(compiler, parser, node->from);
                TypeName to = GetLastType(parser->mem, node->type);

                if( to >= TYPE_PRIMARY_INT8 && to <= TYPE_PRIMARY_INT64) {
                    switch(v.type) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                    case TYPE_PRIMARY_BOOL:
                    case TYPE_PRIMARY_CHAR:
                    case TYPE_MODIFIER_POINTER:
                        break;

                    case TYPE_PRIMARY_F32:
                        switch(to) {
                        case TYPE_PRIMARY_INT8:Cast<i8>(v.mem) = Cast<f32>(v.mem);break;
                        case TYPE_PRIMARY_INT16:Cast<i16>(v.mem) = Cast<f32>(v.mem);break;
                        case TYPE_PRIMARY_INT32:Cast<i32>(v.mem) = Cast<f32>(v.mem);break;
                        case TYPE_PRIMARY_INT64:Cast<i64>(v.mem) = Cast<f32>(v.mem);break;
                        }break;
                    case TYPE_PRIMARY_F64:
                        switch(to) {
                        case TYPE_PRIMARY_INT8:Cast<i8>(v.mem) = Cast<f64>(v.mem);break;
                        case TYPE_PRIMARY_INT16:Cast<i16>(v.mem) = Cast<f64>(v.mem);break;
                        case TYPE_PRIMARY_INT32:Cast<i32>(v.mem) = Cast<f64>(v.mem);break;
                        case TYPE_PRIMARY_INT64:Cast<i64>(v.mem) = Cast<f64>(v.mem);break;
                        }break;
                    }
                }
                else if( (to >= TYPE_PRIMARY_UINT8 && to <= TYPE_PRIMARY_UINT64) || (to == TYPE_MODIFIER_POINTER)) {
                    switch(v.type) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                    case TYPE_PRIMARY_BOOL:
                    case TYPE_PRIMARY_CHAR:
                    case TYPE_MODIFIER_POINTER:
                    case TYPE_MODIFIER_ARRAY:
                        break;

                    case TYPE_PRIMARY_F32:
                        switch(to) {
                        case TYPE_PRIMARY_INT8:Cast<u8>(v.mem) = Cast<f32>(v.mem);break;
                        case TYPE_PRIMARY_INT16:Cast<u16>(v.mem) = Cast<f32>(v.mem);break;
                        case TYPE_PRIMARY_INT32:Cast<u32>(v.mem) = Cast<f32>(v.mem);break;
                        case TYPE_PRIMARY_INT64:Cast<u64>(v.mem) = Cast<f32>(v.mem);break;
                        }break;
                    case TYPE_PRIMARY_F64:
                        switch(to) {
                        case TYPE_PRIMARY_INT8:Cast<u8>(v.mem) = Cast<f64>(v.mem);break;
                        case TYPE_PRIMARY_INT16:Cast<u16>(v.mem) = Cast<f64>(v.mem);break;
                        case TYPE_PRIMARY_INT32:Cast<u32>(v.mem) = Cast<f64>(v.mem);break;
                        case TYPE_PRIMARY_INT64:Cast<u64>(v.mem) = Cast<f64>(v.mem);break;
                        }break;
                    }
                }
                else if( to == TYPE_PRIMARY_CHAR ) {
                    switch(v.type) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:
                    case TYPE_PRIMARY_BOOL:
                    case TYPE_PRIMARY_CHAR:
                        break;
                    }
                }
                else if( to == TYPE_PRIMARY_F32 ) {
                    switch(v.type) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_INT8:Cast<f32>(v.mem) = Cast<i8>(v.mem);break;
                    case TYPE_PRIMARY_INT16:Cast<f32>(v.mem) = Cast<i16>(v.mem);break;
                    case TYPE_PRIMARY_INT32:Cast<f32>(v.mem) = Cast<i32>(v.mem);break;
                    case TYPE_PRIMARY_INT64:Cast<f32>(v.mem) = Cast<i64>(v.mem);break;
                    case TYPE_PRIMARY_UINT8:Cast<f32>(v.mem) = Cast<u8>(v.mem);break;
                    case TYPE_PRIMARY_UINT16:Cast<f32>(v.mem) = Cast<u16>(v.mem);break;
                    case TYPE_PRIMARY_UINT32:Cast<f32>(v.mem) = Cast<u32>(v.mem);break;
                    case TYPE_PRIMARY_UINT64:Cast<f32>(v.mem) = Cast<u64>(v.mem);break;
                    case TYPE_PRIMARY_BOOL:Cast<f32>(v.mem) = Cast<bool>(v.mem);break;
                    case TYPE_PRIMARY_CHAR:Cast<f32>(v.mem) = Cast<char>(v.mem);break;
                    case TYPE_PRIMARY_F32:break;
                    case TYPE_PRIMARY_F64:Cast<f32>(v.mem) = Cast<f64>(v.mem);break;
                    }
                }
                else if( to == TYPE_PRIMARY_F64 ) {
                    switch(v.type) {
                    default:LOGASSERT(false, "Unkown type") break;
                    case TYPE_PRIMARY_INT8:Cast<f64>(v.mem) = Cast<i8>(v.mem);break;
                    case TYPE_PRIMARY_INT16:Cast<f64>(v.mem) = Cast<i16>(v.mem);break;
                    case TYPE_PRIMARY_INT32:Cast<f64>(v.mem) = Cast<i32>(v.mem);break;
                    case TYPE_PRIMARY_INT64:Cast<f64>(v.mem) = Cast<i64>(v.mem);break;
                    case TYPE_PRIMARY_UINT8:Cast<f64>(v.mem) = Cast<u8>(v.mem);break;
                    case TYPE_PRIMARY_UINT16:Cast<f64>(v.mem) = Cast<u16>(v.mem);break;
                    case TYPE_PRIMARY_UINT32:Cast<f64>(v.mem) = Cast<u32>(v.mem);break;
                    case TYPE_PRIMARY_UINT64:Cast<f64>(v.mem) = Cast<u64>(v.mem);break;
                    case TYPE_PRIMARY_BOOL:Cast<f64>(v.mem) = Cast<bool>(v.mem);break;
                    case TYPE_PRIMARY_CHAR:Cast<f64>(v.mem) = Cast<char>(v.mem);break;
                    case TYPE_PRIMARY_F32:Cast<f64>(v.mem) = Cast<f32>(v.mem);break;
                    case TYPE_PRIMARY_F64:break;
                    }
                }
                v.type = to;
                return v;
            }
        case EXPRESSION_LOAD:
            {
                return {};
            }
        case EXPRESSION_STORE:
            {
                return {};
            }
    }
}
bool IsConstExpression(Compiler* compiler, Parser* parser, Expr expr) {
    
    if(expr.index == 0) return false;
    ExprType e = Cast<ExprType>(parser->mem + expr.index);
    switch (e) {
        case EXPRESSION_IMMEDIATE:
        case EXPRESSION_LITERAL:
            {
                LiteralExpr* node = Get<LiteralExpr>(parser, expr.index);
                return node->literal.type != TOKEN_STRING_LITERAL;
            }
        case EXPRESSION_GROUP:
            {
                GroupExpr* node = Get<GroupExpr>(parser, expr.index);
                return IsConstExpression(compiler, parser, node->expr);
            }
        case EXPRESSION_UNARY:
            {
                UnaryExpr* node = Get<UnaryExpr>(parser, expr.index);
                return IsConstExpression(compiler, parser, node->primaryExpr);
            }
        case EXPRESSION_BINARY:
            {
                BinaryExpr* node = Get<BinaryExpr>(parser, expr.index);
                bool left = IsConstExpression(compiler, parser, node->left);
                bool right = IsConstExpression(compiler, parser, node->right);
                return  left && right;
            }
        case EXPRESSION_VARIABLE_LOAD:
            {
                VariableLoadExpr* node = Get<VariableLoadExpr>(parser, expr.index);
                u32 index = FindVariable(&compiler->stack, node->var.name);
                if(index == ~u32(0)) return false;
                return IsConstExpression(compiler, parser, compiler->stack[index].val);
            }
        case EXPRESSION_MEMORY_LOAD:
            {
                MemoryLoadExpr* node = Get<MemoryLoadExpr>(parser, expr.index);
                return false;
            }
        case EXPRESSION_VARIABLE_ASSIGNMENT:
            {
                VariableAssignmentExpr* node = Get<VariableAssignmentExpr>(parser, expr.index);
                u32 index = FindVariable(&compiler->stack, node->var.name);
                bool c = IsConstExpression(compiler, parser, node->value);
                if(index == ~u32(0)) index = compiler->stack.PushBack(node->var);
                compiler->stack[index].val = node->value;
                return c;
            }
        case EXPRESSION_MEMORY_ASSIGNMENT:
            {
                MemoryStoreExpr* node = Get<MemoryStoreExpr>(parser, expr.index);
                return false;
            }
        case EXPRESSION_ADDRESS_OF:
            {
                Expr* node = Get<Expr>(parser, expr.index);
                return false;
            }
        case EXPRESSION_GET:
            {
                VariableGetExpr* node = Get<VariableGetExpr>(parser, expr.index);
                return IsConstExpression(compiler, parser, node->prev);
            }
        case EXPRESSION_PEEL_TYPE:
            {
                PeelTypeExpr* node = Get<PeelTypeExpr>(parser, expr.index);
                return IsConstExpression(compiler, parser, node->expr);
            }
        case EXPRESSION_MEM_COPY:
            {
                Expr* node = Get<Expr>(parser, expr.index);
                return false;
            }
        case EXPRESSION_CALL:
            {
                Expr* node = Get<Expr>(parser, expr.index);
                return false;
            }
        case EXPRESSION_CONVERSION:
            {
                ConversionExpr* node = Get<ConversionExpr>(parser, expr.index);
                return IsConstExpression(compiler, parser, node->from);
            }
        case EXPRESSION_LOAD:
            {
                Expr* node = Get<Expr>(parser, expr.index);
                return false;
            }
        case EXPRESSION_STORE:
            {
                Expr* node = Get<Expr>(parser, expr.index);
                return false;
            }
    }
}


Expr OptimizeExpr(Compiler* compiler, Parser* parser, Expr expr) {

    ExprType t = Cast<ExprType>(parser->mem + expr.index);
    switch(t) {
    case EXPRESSION_NULL:
    case EXPRESSION_LITERAL:return expr;
    case EXPRESSION_GROUP:
        {
            GroupExpr* node = Get<GroupExpr>(parser, expr.index);
            if(IsConstExpression(compiler, parser, node->expr)) {
                Expr ret{parser->exprAllocator};
                ImmediateExpr* im = Allocate<ImmediateExpr>(parser);
                im->index = EXPRESSION_IMMEDIATE;
                im->v = EvaluateConstExpression(compiler, parser, node->expr);
                return ret;
            }
            else {
                return OptimizeExpr(compiler, parser, node->expr);
            }
            break;
        }
    case EXPRESSION_UNARY:
        {
            UnaryExpr* node = Get<UnaryExpr>(parser, expr.index);
            if(IsConstExpression(compiler, parser, node->primaryExpr)) {
                Expr ret{parser->exprAllocator};
                ImmediateExpr* im = Allocate<ImmediateExpr>(parser);
                im->index = EXPRESSION_IMMEDIATE;
                im->v = EvaluateConstExpression(compiler, parser, node->primaryExpr);
                return ret;
            }
            else {
                return OptimizeExpr(compiler, parser, node->primaryExpr);
            }
            break;
        }
    case EXPRESSION_BINARY:
        {
            BinaryExpr* node = Get<BinaryExpr>(parser, expr.index);
            bool left = IsConstExpression(compiler, parser, node->left);
            bool right = IsConstExpression(compiler, parser, node->right);

            /*Expr leftExpr = node->left;
            Expr rightExpr = node->right;
            ExprType leftExprT = FirstNotGroupExpr(parser, &leftExpr);
            ExprType rightExprT = FirstNotGroupExpr(parser, &rightExpr);*/

            if(left) {
                
                Expr l{parser->exprAllocator};
                ImmediateExpr* im = Allocate<ImmediateExpr>(parser);
                im->index = EXPRESSION_IMMEDIATE;
                im->v = EvaluateConstExpression(compiler, parser, node->left);
                node->left = l;
            }
            else {
                node->left = OptimizeExpr(compiler, parser, node->left);
            }
            if(right) {

                Expr l{parser->exprAllocator};
                ImmediateExpr* im = Allocate<ImmediateExpr>(parser);
                im->index = EXPRESSION_IMMEDIATE;
                im->v = EvaluateConstExpression(compiler, parser, node->right);
                node->right = l;
            }
            else {
                node->right = OptimizeExpr(compiler, parser, node->right);
            }
            
            return expr;
            break;
        }
    case EXPRESSION_VARIABLE_LOAD:
        {
            VariableLoadExpr* node = Get<VariableLoadExpr>(parser, expr.index);
            u32 index = FindVariable(&compiler->stack, node->var.name);
            compiler->stack[index];
            return expr;
            break;
        }
    case EXPRESSION_MEMORY_LOAD:
        {
            MemoryLoadExpr* node = Get<MemoryLoadExpr>(parser, expr.index);
            if(IsConstExpression(compiler, parser, node->address)) {
                Expr l{parser->exprAllocator};
                ImmediateExpr* im = Allocate<ImmediateExpr>(parser);
                im->index = EXPRESSION_IMMEDIATE;
                im->v = EvaluateConstExpression(compiler, parser, node->address);
                node->address = l;
            }
            else {
                node->address = OptimizeExpr(compiler, parser, node->address);
            }
            return expr;
            break;
        }
    case EXPRESSION_VARIABLE_ASSIGNMENT:
        {
            VariableAssignmentExpr* node = Get<VariableAssignmentExpr>(parser, expr.index);
            if(IsConstExpression(compiler, parser, node->value)) {
                Expr l{parser->exprAllocator};
                ImmediateExpr* im = Allocate<ImmediateExpr>(parser);
                im->index = EXPRESSION_IMMEDIATE;
                im->v = EvaluateConstExpression(compiler, parser, node->value);
                node->value = l;
            }
            else {
                node->value = OptimizeExpr(compiler, parser, node->value);
            }
            return expr;
            break;
        }
    case EXPRESSION_MEMORY_ASSIGNMENT:
        {
            MemoryStoreExpr* node = Get<MemoryStoreExpr>(parser, expr.index);
            if(IsConstExpression(compiler, parser, node->address)) {
                Expr l{parser->exprAllocator};
                ImmediateExpr* im = Allocate<ImmediateExpr>(parser);
                im->index = EXPRESSION_IMMEDIATE;
                im->v = EvaluateConstExpression(compiler, parser, node->address);
                node->address = l;
            }
            else {
                node->address = OptimizeExpr(compiler, parser, node->address);
            }

            if(IsConstExpression(compiler, parser, node->value)) {
                Expr l{parser->exprAllocator};
                ImmediateExpr* im = Allocate<ImmediateExpr>(parser);
                im->index = EXPRESSION_IMMEDIATE;
                im->v = EvaluateConstExpression(compiler, parser, node->value);
                node->value = l;
            }
            else {
                node->value = OptimizeExpr(compiler, parser, node->value);
            }
            return expr;
            break;
        }
    case EXPRESSION_ADDRESS_OF:
        {
            return expr;
            break;
        }
    case EXPRESSION_GET:
        {
            VariableGetExpr* node = Get<VariableGetExpr>(parser, expr.index);
            if(IsConstExpression(compiler, parser, node->prev)) {
                Expr l{parser->exprAllocator};
                ImmediateExpr* im = Allocate<ImmediateExpr>(parser);
                im->index = EXPRESSION_IMMEDIATE;
                im->v = EvaluateConstExpression(compiler, parser, node->prev);
                node->prev = l;
            }
            else {
                node->prev = OptimizeExpr(compiler, parser, node->prev);
            }
            return expr;
            break;
        }
    case EXPRESSION_PEEL_TYPE:
        {
            PeelTypeExpr* node = Get<PeelTypeExpr>(parser, expr.index);
            node->expr = OptimizeExpr(compiler, parser, node->expr);
            return expr;
            break;
        }
    case EXPRESSION_MEM_COPY:
        {
            return expr;
            break;
        }
    case EXPRESSION_CALL:
        {
            return expr;
            break;
        }
    case EXPRESSION_CONVERSION:
        {
            ConversionExpr* node = Get<ConversionExpr>(parser, expr.index);
            node->from = OptimizeExpr(compiler, parser, node->from);
            return expr;
            break;
        }
    case EXPRESSION_LOAD:
        {
            return expr;
            break;
        }
    case EXPRESSION_STORE:
        {
            return expr;
            break;
        }
    }
    ASSERT(false);
}

Expr OptimizeExprWrapper(Compiler* compiler, Parser* parser, Expr e, bool keep) {
    u32 stackSize = compiler->stack.size;
    Expr restore[stackSize];
    for(u32 i = 0; i < compiler->stack.size ;i++) {
        restore[i] = compiler->stack[i].val;
    }

    if(IsConstExpression(compiler, parser, e)) {

        compiler->stack.size = stackSize;
        for(u32 i = 0; i < compiler->stack.size; i++) {
            compiler->stack[i].val = restore[i];
        }

        Expr l{parser->exprAllocator};

        ImmediateExpr* im = Allocate<ImmediateExpr>(parser);
        im->index = EXPRESSION_IMMEDIATE;
        im->v = EvaluateConstExpression(compiler, parser, e);

        if(Cast<ExprType>(parser->mem + e.index) == EXPRESSION_VARIABLE_ASSIGNMENT) {
            VariableAssignmentExpr* node = Get<VariableAssignmentExpr>(parser, e.index);
            u32 index = FindVariable(&compiler->stack, node->var.name);
            compiler->stack[index].val = l;
        }

        if(keep) {
            return e;
        }
        return l;
    }
    else {
        compiler->stack.size = stackSize;
        for(u32 i = 0; i < compiler->stack.size; i++) {
            compiler->stack[i].val = restore[i];
        }
        return OptimizeExpr(compiler, parser, e);
    }
}

bool OptimizeStatements(Compiler* compiler, Parser* parser, Stmt* program, bool keep) {

    StatementType t = Cast<StatementType>(compiler->mem + program->index);
    switch(t) {
    case STATEMENT_NON:
        return false;
    case STATEMENT_EXPRESSION:
        {
            ExprStmt* stmt = Get<ExprStmt>(compiler, program->index);
            stmt->expr = OptimizeExprWrapper(compiler, parser, stmt->expr, keep);
            if(Cast<ExprType>(parser->mem + stmt->expr.index) == EXPRESSION_IMMEDIATE) {
                Cast<ExprType>(parser->mem + stmt->expr.index) = EXPRESSION_NULL;
            }
            program->index += sizeof(ExprStmt);
        }
        break;
    case STATEMENT_PRINT:
        {
            program->index += sizeof(PrintStmt);
            Expr printExpr = Cast<Expr>(compiler->mem + program->index);
            while(printExpr.index != 0) {

                Cast<Expr>(compiler->mem + program->index) = OptimizeExprWrapper(compiler, parser, printExpr, keep);
                program->index += sizeof(Expr) * 2;
                printExpr = Cast<Expr>(compiler->mem + program->index);
            }
            program->index += sizeof(Expr);
        }
        break;
    case STATEMENT_ENTRY_POINT:
        program->index += sizeof(Stmt);
        break;
    case STATEMENT_FUNCTION_ENTRY:
        program->index += sizeof(FuncFrame);
        break;
    case STATEMENT_FUNCTION_RET:
        program->index += sizeof(FuncFrame);
        break;
    case STATEMENT_BRANCH:
        {
            BranchStmt* stmt = Get<BranchStmt>(compiler, program->index);
            if(IsConstExpression(compiler, parser, stmt->cond)) {

                SkipStmt* skip = Get<SkipStmt>(compiler, program->index);
                Stmt end = stmt->end;
                Value c = EvaluateConstExpression(compiler, parser, stmt->cond);
                if(Cast<bool>(c.mem)) {
                    skip->target = stmt->thenBranch;
                }
                else {
                    skip->target = stmt->elseBranch;
                }
                skip->index = STATEMENT_SKIP;
                skip->end = end;
                *program = skip->target;
                OptimizeStatements(compiler, parser, program, keep);
                *program = skip->end;
            }
            else {
                *program = stmt->end;
            }
        }
        break;
    case STATEMENT_FOR_LOOP:
        {

            ForStmt* stmt = Get<ForStmt>(compiler, program->index);
            *program = stmt->init;
            OptimizeStatements(compiler, parser, program, true);
            if(IsConstExpression(compiler, parser, stmt->cond)) {

                Value cond = EvaluateConstExpression(compiler, parser, stmt->cond);
                if(!Cast<bool>(cond.mem)) {
                    SkipStmt* skip = (SkipStmt*)stmt;
                    Stmt end = stmt->end;
                    skip->index = STATEMENT_SKIP;
                    skip->target = end;
                    skip->end.index = 0;
                    *program = end;
                    break;
                }
            }
            *program = stmt->end;
        }
        break;
    case STATEMENT_REPEAT:
        program->index += sizeof(Stmt);
        while(OptimizeStatements(compiler, parser, program, keep));
        program->index += sizeof(Stmt);
        break;
    case STATEMENT_RRV_ASSIGN:
        program->index += sizeof(RetAssignStmt);
        break;
    case STATEMENT_VAR_DECL:
        
        program->index += sizeof(VarDeclStm);
        break;
    case STATEMENT_ABORT:
        program->index += sizeof(Stmt);
        break;
    }

    return true;
}
void Optimize(Compiler* compiler, Parser* parser, Stmt program) {
    
    u32 stackSize = compiler->stack.size;
    bool running = true;
    while(running) {
        running = OptimizeStatements(compiler, parser, &program, false);
    }
    compiler->stack.size = stackSize;
}

bool ParseStatement(Compiler* compiler, Parser* parser, Chunk* chunk) {

    Token peek = PeekToken(parser->tokenizer);

    switch(peek.type) {
    case TOKEN_KEYWORD_MAIN:
        {
            ExpectToken(parser, TOKEN_KEYWORD_MAIN);
            Token name = PreviousToken(parser);
            
            Expect(compiler, parser, PeekToken(parser->tokenizer).type != TOKEN_OPEN_PAREN , "main is non recursive");
            Expect(compiler, parser, compiler->entryPoint.index == ~u32(0) , "there can be only one main in a program");
            ExpectToken(parser, TOKEN_OPEN_BRACES);
            compiler->entryPoint.index = compiler->allocator;
            
            Stmt* main = Allocate<Stmt>(compiler);
            main->index = STATEMENT_ENTRY_POINT;

            compiler->currentFunction = compiler->fnAllocator;

            Stmt program{compiler->allocator};
            FuncFrame* enter = Allocate<FuncFrame>(compiler);
            enter->index = STATEMENT_FUNCTION_ENTRY;
            enter->function = compiler->fnAllocator;
            enter->expressions = 0;
            u32 func = compiler->fnAllocator;

            Function* fn = AllocateF<Function>(compiler);
            *fn = Function{};
            fn->reg.virtualRegisters.Init();
            fn->reg.preAssigned.Init();
            fn->reg.params.Init();

            fn->name = name;
            fn->paramCount = 0;

            compiler->scope++;
            while(!Check(parser, TOKEN_CLOSE_BRACES) && PeekToken(parser->tokenizer).type != TOKEN_EOF) {
                ParseStatement(compiler, parser, chunk);
            }

            compiler->scope--;
            PopVariables(parser, compiler);
            ExpectToken(parser, TOKEN_CLOSE_BRACES);

            FuncFrame* out = Allocate<FuncFrame>(compiler);
            out->index = STATEMENT_FUNCTION_RET;
            out->function = enter->function;

            if(compiler->config->optimization_constant_propogation) {
                Optimize(compiler, parser, program);
            }
            AssignHWRegisters(compiler, parser, program, func);
        }
        break;
    case TOKEN_KEYWORD_FN:
        {
            NextToken(parser);
            ExpectToken(parser, TOKEN_IDENTIFIER);

            u32 fIndex = compiler->fnAllocator;
            compiler->currentFunction = fIndex;
            Function* f = AllocateF<Function>(compiler);
            *f = Function{};
            f->name = PreviousToken(parser);
            f->reg.virtualRegisters.Init();
            f->reg.preAssigned.Init();
            f->reg.params.Init();
            f->frameSize = 0;

            Variable fn;
            fn.scope = compiler->scope;
            fn.name = f->name;
            fn.address = fIndex;
            fn.type.index = parser->exprAllocator;
            FnTypeExpr* fnExpr = Allocate<FnTypeExpr>(parser);
            fnExpr->index = TYPE_PRIMARY_FN;

            u32 index = FindVariable(&compiler->stack, fn.name);
            if(index != ~u32(0)) {
                (std::cout << "ERROR: function redecleration (").write(fn.name.text, fn.name.lenght) << ") at line: " << parser->tokenizer.line << std::endl;
                compiler->error = true;
            }
            compiler->stack.PushBack(fn);
            
            Stmt program{compiler->allocator};
            FuncFrame* enter = Allocate<FuncFrame>(compiler);
            enter->index = STATEMENT_FUNCTION_ENTRY;
            enter->function = fIndex;
            enter->expressions = 0;

            TokenType close = TOKEN_CLOSE_PAREN;
            TokenType comma = TOKEN_COMMA;
            TokenType arrow = TOKEN_KEYWORD_ARROW;
            Tokenizer peek = parser->tokenizer;
            compiler->scope++;
            ExpectToken(parser, TOKEN_OPEN_PAREN);

            bool hidden = false;
            u32 hidden_index;
            {
                ParserState save = SaveParserState(parser);
                if(!Match(parser, &close, 1)) {
                    do {
                        ParseTypeExpression(parser);
                        NextToken(parser);
                    } while( Match(parser, &comma ,1 ) );
                    NextToken(parser);
                }
                    
                if(Match(parser, &arrow, 1)) {

                    TypeExpr retT = ParseTypeExpression(parser);
                    TypeName last = GetLastType(parser->mem, retT);
                    if(last > TYPE_COUNT || last == TYPE_MODIFIER_ARRAY) {
                        TypeExpr end = CpyTypeExpr(compiler->scratchMem, TypeExpr{0}, parser->mem, retT);
                        Cast<TypeName>(compiler->scratchMem + end.index - sizeof(TypeName)) = TYPE_MODIFIER_POINTER;
                        Cast<TypeName>(compiler->scratchMem + end.index) = TYPE_NON;

                        Variable hidden_param;
                        hidden_param.scope = compiler->scope;
                        hidden_param.name.lenght = sizeof("hidden_param0")-1;
                        hidden_param.name.type = TOKEN_IDENTIFIER;
                        hidden_param.name.text = (char*)my_malloc(sizeof("hidden_param0"));
                        for(u32 i = 0; i < sizeof("hidden_param0"); i++) {
                            hidden_param.name.text[i] = ("hidden_param0")[i];
                        }
                        hidden_index = compiler->stack.PushBack(hidden_param);

                        VarDeclStm* varDecl = Allocate<VarDeclStm>(compiler);
                        varDecl->index = STATEMENT_VAR_DECL;
                        varDecl->init = false;
                        varDecl->param = true;
                        varDecl->preColor = true;
                        varDecl->hidden = true;
                        varDecl->reg = R0;
                        varDecl->var = hidden_param;


                        fnExpr->param_count = 1;
                        hidden = true;
                    }
                }

                RestoreParserState(parser, save);
            }

            if(!Match(parser, &close, 1)) {

                ParserState save = SaveParserState(parser);
                do {
                    fnExpr->param_count++;
                    ParseTypeExpression(parser);
                    NextToken(parser);
                } while( Match(parser, &comma ,1 ) );
                RestoreParserState(parser, save);

                if(hidden) {
                    fnExpr->params = parser->exprAllocator;
                    for(u32 i = 0; i < fnExpr->param_count; i++) Allocate<TypeExpr>(parser);
                    fnExpr->param_count = 1;

                    compiler->stack[hidden_index].type = {parser->exprAllocator};
                    TypeExpr end = CpyTypeExpr(parser->mem, {parser->exprAllocator}, compiler->scratchMem, TypeExpr{0});
                    Cast<TypeExpr>(parser->mem + fnExpr->params).index = parser->exprAllocator;
                    parser->exprAllocator = end.index;
                }
                else {
                    fnExpr->params = parser->exprAllocator;
                    for(u32 i = 0; i < fnExpr->param_count; i++) Allocate<TypeExpr>(parser);
                    fnExpr->param_count = 0;
                }

                do {
                    Variable param;
                    param.type = ParseTypeExpression(parser);
                    param.name = NextToken(parser);


                    TypeName paramT = GetLastType(parser->mem, param.type);
                    if(paramT == TYPE_MODIFIER_ARRAY || paramT > TYPE_COUNT) {
                        param.address = f->currentFrameSize;
                        u32 size = GetTypeSize(parser->mem, parser->mem, param.type);
                        f->currentFrameSize += size;
                        f->frameSize += size;
                    }

                    Expect(compiler, parser, param.name.type == TOKEN_IDENTIFIER ,"expected identifier");
                    param.scope = compiler->scope;

                    VarDeclStm* decl = Allocate<VarDeclStm>(compiler);
                    decl->index = STATEMENT_VAR_DECL;
                    decl->init = false;
                    decl->param = false;
                    decl->preColor = false;
                    decl->var = param;
                    if( fnExpr->param_count < R4 ) {
                        decl->preColor = true;
                        decl->reg = (RegisterName)fnExpr->param_count;
                    }

                    compiler->stack.PushBack(param);
                    if( fnExpr->param_count < 4) {
                        decl->param = true;
                        f->reg.params.PushBack(param);
                    }
                    Cast<TypeExpr>(parser->mem + fnExpr->params + fnExpr->param_count * sizeof(TypeExpr)) = param.type;

                    fnExpr->param_count++;
                } while( Match(parser, &comma ,1 ) );
                ExpectToken(parser, TOKEN_CLOSE_PAREN);
            }
            else if(hidden && fnExpr->param_count == 1) {
                fnExpr->params = parser->exprAllocator;
                for(u32 i = 0; i < fnExpr->param_count; i++) Allocate<TypeExpr>(parser);

                compiler->stack[hidden_index].type = {parser->exprAllocator};
                TypeExpr end = CpyTypeExpr(parser->mem, {parser->exprAllocator}, compiler->scratchMem, TypeExpr{0});
                Cast<TypeExpr>(parser->mem + fnExpr->params).index = parser->exprAllocator;
                parser->exprAllocator = end.index;
            }
            f->paramCount = fnExpr->param_count;

            if(Match(parser, &arrow, 1 )) {
                fnExpr->ret_t = ParseTypeExpression(parser);
                f->ret_t = fnExpr->ret_t;
            }
            else {
                fnExpr->ret_t.index = parser->exprAllocator;
                f->ret_t = fnExpr->ret_t;
                *Allocate<TypeName>(parser) = TYPE_PRIMARY_VOID;
                *Allocate<TypeName>(parser) = TYPE_NON;
            }
            fnExpr->modifier.index = parser->exprAllocator;
            *Allocate<TypeName>(parser) = TYPE_NON;

            ExpectToken(parser, TOKEN_OPEN_BRACES);
            while(!Check(parser, TOKEN_CLOSE_BRACES) && PeekToken(parser->tokenizer).type != TOKEN_EOF) {
                ParseStatement(compiler, parser, chunk);
            }
            compiler->scope--;
            PopVariables(parser, compiler);
            ExpectToken(parser, TOKEN_CLOSE_BRACES);


            FuncFrame* exit = Allocate<FuncFrame>(compiler);
            exit->index = STATEMENT_FUNCTION_RET;
            exit->function = enter->function;

            if(compiler->config->optimization_constant_propogation) {
                Optimize(compiler, parser, program);
            }
            AssignHWRegisters(compiler, parser, program , fIndex);
        }
        break;
    case TOKEN_KEYWORD_STRUCT:
        {
            ExpectToken(parser, TOKEN_KEYWORD_STRUCT);
            parser->structs.PushBack( StructName{parser->exprAllocator, NextToken(parser)} );
            StructTypeExpr* node = Allocate<StructTypeExpr>(parser);
            node->index = TYPE_STRUCTURE;
            node->name = parser->structs.Back().name;
            Expect(compiler, parser, node->name.type == TOKEN_IDENTIFIER, "expected identifier");
            ExpectToken(parser, TOKEN_OPEN_BRACES);

            node->members.index = parser->exprAllocator;
            u32 allocator = parser->exprAllocator;
            u32 size = parser->tokenBuffer.size;
            Tokenizer restore = parser->tokenizer;

            u32 memberCount = 0;
            while(PeekToken(parser->tokenizer).type != TOKEN_CLOSE_BRACES) {
                memberCount++;
                ParseTypeExpression(parser);
                ExpectToken(parser, TOKEN_IDENTIFIER);
                ExpectToken(parser, TOKEN_SEMICOLON);
            }

            parser->exprAllocator = allocator;
            parser->tokenBuffer.size = size;
            parser->tokenizer = restore;

            for(u32 i = 0; i < memberCount; i++) Allocate<TypeExpr>(parser);
            Allocate<TypeExpr>(parser)->index = TYPE_NON;

            for(u32 i = 0; i < memberCount; i++) {

                Cast<TypeExpr>(parser->mem + node->members.index + i * sizeof(TypeExpr)).index = parser->exprAllocator;
                StructMemberTypeExpr* member = Allocate<StructMemberTypeExpr>(parser);
                member->index = TYPE_STRUCTURE_MEMBER;
                member->type = ParseTypeExpression(parser);
                member->name = NextToken(parser);
                ExpectToken(parser, TOKEN_SEMICOLON);
                ASSERT(member->name.type == TOKEN_IDENTIFIER);
            }

            ExpectToken(parser, TOKEN_CLOSE_BRACES);

            PrintStruct(parser->mem, parser->mem, TypeExpr{parser->structs.Back().ptr} );
        }
        break;
    case TOKEN_OPEN_BRACES:
        NextToken(parser);
        Expect(compiler, parser, compiler->scope != 0 , "expected declaration");
        compiler->scope++;
        while(!Check(parser, TOKEN_CLOSE_BRACES) && PeekToken(parser->tokenizer).type != TOKEN_EOF) {
            ParseStatement(compiler, parser, chunk);
        }
        ParseStatement(compiler, parser, chunk);
        break;
    case TOKEN_CLOSE_BRACES:
        NextToken(parser);
        compiler->scope--;
        PopVariables(parser, compiler);
        return false;
        break;
    case TOKEN_KEYWORD_RETURN:
        {
            Function* f = Get<Function>(compiler, compiler->currentFunction);
            ExpectToken(parser, TOKEN_KEYWORD_RETURN);
            RetAssignStmt* node = Allocate<RetAssignStmt>(compiler);
            node->index = STATEMENT_RRV_ASSIGN;
            node->retExpr.index = 0;
            if(PeekToken(parser->tokenizer).type != TOKEN_SEMICOLON) {
                node->retExpr = Expression(parser, compiler);
                GetTypeExpr(compiler, parser, node->retExpr, compiler->scratchMem, TypeExpr{0});
                bool match = TypesEqual(parser->mem, parser->mem, compiler->scratchMem, parser->mem, TypeExpr{0} , f->ret_t);
                Expect(compiler, parser, match, "function return type mismatch");

                TypeName type = GetLastType(compiler->scratchMem, TypeExpr{0});
                if(type == TYPE_MODIFIER_ARRAY || type > TYPE_COUNT) {
                    MemCopyExpr* memCopy = Allocate<MemCopyExpr>(parser);
                    memCopy->index = EXPRESSION_MEM_COPY;
                    memCopy->size = GetTypeSize(parser->mem, compiler->scratchMem, TypeExpr{0});
                    memCopy->src = node->retExpr;
                    memCopy->dst.index = parser->exprAllocator;

                    VariableLoadExpr* retPtr = Allocate<VariableLoadExpr>(parser);
                    retPtr->index = EXPRESSION_VARIABLE_LOAD;
                    retPtr->relative = RBP;
                    for(u32 i = 0; i < compiler->stack.size; i++) {
                        if(TokensEquals(f->name, compiler->stack[i].name)) {
                            retPtr->var = compiler->stack[i+1];
                            break;
                        }
                    }

                    node->retExpr = Convert(parser, memCopy);
                }
            }
            ExpectToken(parser, TOKEN_SEMICOLON);
        }   
        break;     
    case TOKEN_KEYWORD_IF:
        {
            Expect(compiler, parser, compiler->scope != 0, "statement (if) at global scope");
            NextToken(parser);
            ExpectToken(parser, TOKEN_OPEN_PAREN);

            BranchStmt* b = Allocate<BranchStmt>(compiler);
            b->index = STATEMENT_BRANCH;
            b->cond = Expression(parser, compiler);

            GetTypeExpr(compiler, parser, b->cond, compiler->scratchMem, TypeExpr{0} );
            TypeName cond_t = GetLastType(compiler->scratchMem, TypeExpr{0});
            Expect(compiler, parser, cond_t == TYPE_PRIMARY_BOOL , "branch condition must be a boolean expression");

            ExpectToken(parser, TOKEN_CLOSE_PAREN);
            b->thenBranch.index = compiler->allocator;
            *Allocate<StatementType>(compiler) = STATEMENT_REPEAT;
            ParseStatement(compiler, parser, chunk);
            *Allocate<StatementType>(compiler) = STATEMENT_NON;
            b->elseBranch.index = 0;

            TokenType keywordelse = TOKEN_KEYWORD_ELSE;
            if(Match(parser, &keywordelse, 1) ) {
                b->elseBranch.index = compiler->allocator;
                *Allocate<StatementType>(compiler) = STATEMENT_REPEAT;
                ParseStatement(compiler, parser, chunk);
                *Allocate<StatementType>(compiler) = STATEMENT_NON;
            }
            b->end.index = compiler->allocator;
        }
        break;
    case TOKEN_KEYWORD_FOR:
        {
            Expect(compiler, parser, compiler->scope != 0, "statement (for) at global scope");
            NextToken(parser);
            ExpectToken(parser, TOKEN_OPEN_PAREN);

            ForStmt* forStmt = Allocate<ForStmt>(compiler);
            forStmt->index = STATEMENT_FOR_LOOP;

            compiler->scope++;
            forStmt->init.index = compiler->allocator;
            *Allocate<StatementType>(compiler) = STATEMENT_REPEAT;
            
            TokenType op = TOKEN_OPEN_BRACES;
            if(Match(parser, &op, 1)) {
                while(PeekToken(parser->tokenizer).type != TOKEN_CLOSE_BRACES) ParseStatement(compiler, parser, chunk);
                NextToken(parser);
            }
            else if(PeekToken(parser->tokenizer).type != TOKEN_SEMICOLON ) {
                ParseStatement(compiler, parser, chunk);
            }
            else {
                ExpectToken(parser, TOKEN_SEMICOLON );
            }
            *Allocate<StatementType>(compiler) = STATEMENT_NON;
            
            if(PeekToken(parser->tokenizer).type != TOKEN_SEMICOLON) {
                forStmt->cond = Expression(parser, compiler);
                GetTypeExpr(compiler, parser, forStmt->cond, compiler->scratchMem, TypeExpr{0});
                TypeName t = GetLastType(compiler->scratchMem , TypeExpr{0});
                Expect(compiler, parser, t == TYPE_PRIMARY_BOOL, "loop condition must be a boolean expression");
            }
            else {
                forStmt->cond.index = EXPRESSION_NULL;
            }
            ExpectToken(parser, TOKEN_SEMICOLON);

            forStmt->inc.index = compiler->allocator;
            *Allocate<StatementType>(compiler) = STATEMENT_REPEAT;
            if(PeekToken(parser->tokenizer).type != TOKEN_SEMICOLON) {
                ParseStatement(compiler, parser, chunk);
            }
            else {
                ExpectToken(parser, TOKEN_SEMICOLON);
            }
            *Allocate<StatementType>(compiler) = STATEMENT_NON;
            ExpectToken(parser, TOKEN_CLOSE_PAREN);

            forStmt->body.index = compiler->allocator;
            *Allocate<StatementType>(compiler) = STATEMENT_REPEAT;
            if(PeekToken(parser->tokenizer).type != TOKEN_SEMICOLON) {
                ParseStatement(compiler, parser, chunk);
            }
            else {
                ExpectToken(parser, TOKEN_SEMICOLON);
            }
            *Allocate<StatementType>(compiler) = STATEMENT_NON;

            compiler->scope--;
            PopVariables(parser, compiler);
        }
        break;
    case TOKEN_KEYWORD_PRINT:
        {
            Expect(compiler, parser, compiler->scope != 0, "print statement in global scope");
            NextToken(parser);

            PrintStmt* print = Allocate<PrintStmt>(compiler);
            print->index = STATEMENT_PRINT;

            bool empty = true;
            TokenType comma = TOKEN_COMMA;

            if(!Check(parser, TOKEN_SEMICOLON)) {
                do {
                    Expr* expr = Allocate<Expr>(compiler);
                    Allocate<u32>(compiler);
                    *expr = Expression(parser, compiler);
                    
                    Expect(compiler, parser, expr->index != 0, "print error");

                    GetTypeExpr(compiler, parser, *expr , compiler->scratchMem, TypeExpr{0});
                    TypeName last = GetLastType(compiler->scratchMem, TypeExpr{0});
                    Expect(compiler, parser, last >= TYPE_PRIMARY_CHAR && last <= TYPE_MODIFIER_POINTER, "invalid print type");

                    empty = false;
                } while( Match(parser, &comma , 1) );
            }

            Allocate<Expr>(compiler)->index = EXPRESSION_NULL;
            Expect(compiler, parser, !empty, "print statement without expression");
            ExpectToken(parser, TOKEN_SEMICOLON);
        }
        break;
    case TOKEN_IDENTIFIER:identifier_stmt:
        {
            if(!IsType(parser, parser->tokenizer)) {
                goto expr_stmt;
            }
            Variable var;
            var.val = {};

            TokenType auto_token = TOKEN_KEYWORD_AUTO;
            if(Match(parser, &auto_token, 1)) {
                ParserState state = SaveParserState(parser);
                ExpectToken(parser, TOKEN_IDENTIFIER);
                ExpectToken(parser, TOKEN_EQUAL_SIGN);
                Expr val = Expression(parser, compiler);
                RestoreParserState(parser, state);

                GetTypeExpr(compiler, parser, val, compiler->scratchMem, {0});
                TypeName type = GetLastType(compiler->scratchMem, {0});
                Expect(compiler, parser, type != TYPE_PRIMARY_AUTO && type != TYPE_PRIMARY_VOID, "cant deduce declaration type");
                var.type = {parser->exprAllocator};
                parser->exprAllocator = CpyTypeExpr(parser->mem, {parser->exprAllocator}, compiler->scratchMem, {0}).index;
            }
            else {
                var.type = ParseTypeExpression(parser);
            }

            TypeName t = GetLastType(parser->mem, var.type);
            Expect(compiler, parser, t != TYPE_PRIMARY_FN, "variable with function type");
            var.name = PeekToken(parser->tokenizer);
            var.scope = compiler->scope;

            Function* fn = Get<Function>(compiler, compiler->currentFunction);
            if(t == TYPE_MODIFIER_ARRAY || t > TYPE_COUNT) {
                fn->currentFrameSize += GetTypeSize(parser->mem, parser->mem, var.type);
                var.address = fn->currentFrameSize;
            }

            u32 varIndex = FindVariable(&compiler->stack, var.name);
            if(varIndex != ~u32(0)) {
                (std::cout << "ERROR: variable redecleration (").write(var.name.text, var.name.lenght) << ") at line: " << parser->tokenizer.line << std::endl;
                compiler->error = true;
            }
            varIndex = compiler->stack.PushBack(var);

            VarDeclStm* varDeclStmt = Allocate<VarDeclStm>(compiler);
            varDeclStmt->index = STATEMENT_VAR_DECL;
            varDeclStmt->init = false;
            varDeclStmt->hidden = false;
            varDeclStmt->param = false;
            varDeclStmt->preColor = false;
            varDeclStmt->var = var;

            Tokenizer peek = parser->tokenizer;
            GetToken(&peek);
            if(GetToken(&peek).type == TOKEN_EQUAL_SIGN) {
                varDeclStmt->init = true;
                ExprStmt* stmt = Allocate<ExprStmt>(compiler);
                stmt->index = STATEMENT_EXPRESSION;
                stmt->expr = Expression(parser, compiler);
                compiler->stack[varIndex].val = stmt->expr;
                
                GetTypeExpr(compiler, parser, stmt->expr, compiler->scratchMem, TypeExpr{0} );
            }
            else {
                ExpectToken(parser, TOKEN_IDENTIFIER);
            }
            ExpectToken(parser, TOKEN_SEMICOLON);
        }
        break;
    default:
        {
            goto identifier_stmt;
            expr_stmt:
            
            if( TokenEquals(PeekToken(parser->tokenizer), "abort")) {
                NextToken(parser);
                ExpectToken(parser, TOKEN_OPEN_PAREN);
                ExpectToken(parser, TOKEN_CLOSE_PAREN);
                ExpectToken(parser, TOKEN_SEMICOLON);
                Stmt* abortStmt = Allocate<Stmt>(compiler);
                abortStmt->index = STATEMENT_ABORT;
                break;
            }

            ExprStmt* stmt = Allocate<ExprStmt>(compiler);
            stmt->index = STATEMENT_EXPRESSION;
            stmt->expr = Expression(parser, compiler);
            GetTypeExpr(compiler, parser, stmt->expr, compiler->scratchMem, TypeExpr{0} );
            ExpectToken(parser, TOKEN_SEMICOLON);
        }
        break;
    }

    return true;
}

bool CompileStatement(Compiler* compiler, Chunk* chunk, Parser* parser, Stmt* stmt) {

    StatementType t = *Get<StatementType>(compiler, stmt->index);


    switch(t) {
    default:LOGASSERT(false, "Uknown statement");break;
    case STATEMENT_ABORT:
        {
            WriteChunkOpCode(&chunk->main, OP_XOR);
            WriteChunkIM8(&chunk->main, ToFrom(R0,R0));
            WriteChunkOpCode(&chunk->main, OP_INTERUPT);
            stmt->index += sizeof(Stmt);
        }
        break;
    case STATEMENT_SKIP:
        {
            SkipStmt* skip = Get<SkipStmt>(compiler, stmt->index);
            *stmt = skip->target;
            CompileStatement(compiler, chunk, parser, stmt);
            if(skip->end.index != 0) {

                *stmt = skip->end;
            }
            break;
        }
    case STATEMENT_NON:
        return false;
        break;
    case STATEMENT_REPEAT:
        {
            stmt->index += sizeof(Stmt);
            while(CompileStatement(compiler, chunk, parser, stmt) );
            stmt->index += sizeof(Stmt);
        }
        break;
    case STATEMENT_ENTRY_POINT:
        chunk->entryPoint = chunk->main.buffer.size;
        stmt->index += sizeof(Stmt);
        break;
    case STATEMENT_VAR_DECL:
        stmt->index += sizeof(VarDeclStm);
        break;
    case STATEMENT_FUNCTION_ENTRY:
        {
            FuncFrame* frame = Get<FuncFrame>(compiler, stmt->index);
            Function* f = Get<Function>(compiler, frame->function);
            f->address = chunk->main.buffer.size;
            compiler->currentFunction = frame->function;
            //RegisterName r = (RegisterName)f->reg.colors[frame->g_index];

            if(f->frameSize != 0) {
                WriteChunkOpCode(&chunk->main, OP_PUSH);
                WriteChunkIM8(&chunk->main, ToFrom(RBP,R3) );
                WriteChunkOpCode(&chunk->main, OP_MOV64);
                WriteChunkIM8(&chunk->main, ToFrom(RBP,RSP) );

                WriteChunkOpCode(&chunk->main, OP_MOV_IM16);
                WriteChunkIM8(&chunk->main, RRV);
                WriteChunkIM16(&chunk->main, f->frameSize+8);
                WriteChunkOpCode(&chunk->main, OP_SUB);
                WriteChunkIM8(&chunk->main, ToFrom(RSP,RRV) );
            }

            if(!TokenEquals(f->name, "main")) {
                u32 m = max<i32>(0, min<i32>(f->paramCount,4) - 1);
                bool regs[12]{};
                for(u32 i = 0; i < f->reg.virtualRegisters.size; i++) {
                    if( f->reg.colors[i] > m) {
                        regs[ f->reg.colors[i] ] = true;
                    }
                }

                for(u32 i = 0; i < 12; i++) {
                    if(regs[i]) {
                        WriteChunkOpCode(&chunk->main, OP_PUSH);
                        WriteChunkIM8(&chunk->main, ToFrom((RegisterName)i, R3) );
                    }
                }

                if(frame->expressions != 0) {
                    u32 i = frame->expressions;
                    while( Cast<ExprType>(compiler->mem + i) != EXPRESSION_NULL ) {
                        CompileExpression(compiler, parser, &chunk->main, Cast<Expr>(compiler->mem + i));
                        i += sizeof(ExprType);
                    }
                }
            }
            stmt->index += sizeof(FuncFrame);
        }
        break;
    case STATEMENT_FUNCTION_RET:
        {
            FuncFrame* frame = Get<FuncFrame>(compiler, stmt->index);
            Function* f = Get<Function>(compiler, frame->function);

            if(f->retJmpAddress != 0) {
                u32 i = f->retJmpAddress;
                while( Cast<u32>(compiler->mem + i) != ~u32(0)) {
                    u32 ref = Cast<u32>(compiler->mem + i);
                    Cast<u64>(chunk->main.refs[ref].value) = chunk->main.buffer.size;
                    i += sizeof(u32);
                }
                compiler->miscAllocator = f->retJmpAddress;
            }

            if(frame->expressions != 0) {
                u32 i = frame->expressions;
                while( Cast<ExprType>(compiler->mem + i) != EXPRESSION_NULL ) {
                    CompileExpression(compiler, parser, &chunk->main, Cast<Expr>(compiler->mem + i));
                    i += sizeof(ExprType);
                }
            }

            if(!TokenEquals(f->name, "main")) {
                u32 m = max<i32>(0, min<i32>(f->paramCount,4) - 1);
                bool regs[12]{};
                for(u32 i = 0; i < f->reg.virtualRegisters.size; i++) {
                    if( f->reg.colors[i] > m) {
                        regs[ f->reg.colors[i] ] = true;
                    }
                }
                for(i32 i = 11; i > -1; i--) {
                    if(regs[i]) {
                        WriteChunkOpCode(&chunk->main, OP_POP);
                        WriteChunkIM8(&chunk->main, ToFrom((RegisterName)i, R3) );
                    }
                }
            }

            //RegisterName r = (RegisterName)f->reg.colors[frame->g_index];
            if(f->frameSize != 0) {

                WriteChunkOpCode(&chunk->main, OP_MOV64);
                WriteChunkIM8(&chunk->main, ToFrom(RSP,RBP) );
                WriteChunkOpCode(&chunk->main, OP_POP);
                WriteChunkIM8(&chunk->main, ToFrom(RBP,R3) );
            }
            WriteChunkOpCode(&chunk->main, OP_RET);

            stmt->index += sizeof(FuncFrame);
        }
        break;
    case STATEMENT_RRV_ASSIGN:
        {
            Function* f = Get<Function>(compiler, compiler->currentFunction);
            RetAssignStmt* a = Get<RetAssignStmt>(compiler, stmt->index);
            RegisterName r;
            if(a->retExpr.index != 0) {
                r = CompileExpression(compiler, parser, &chunk->main , a->retExpr);
                if(r != RRV) {
                    WriteChunkOpCode(&chunk->main, OP_MOV64);
                    WriteChunkIM8(&chunk->main, ToFrom(RRV,r));
                }
            }
            if(f->retJmpAddress == 0) { 
                f->retJmpAddress = compiler->miscAllocator;
            }

            WriteChunkOpCode(&chunk->main, OP_JMP_IM);
            *AllocateMisc<u32>(compiler) = chunk->main.refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, chunk->main.buffer.size ,0,8));
            Cast<u32>(compiler->mem + compiler->miscAllocator) = ~u32(0);

            for(u32 i = 0; i < 8 ; i++) WriteChunkIM8(&chunk->main, 0xFF);
            stmt->index += sizeof(RetAssignStmt);
        }
        break;
    case STATEMENT_EXPRESSION:
        {
            ExprStmt exStmt = *Get<ExprStmt>(compiler, stmt->index);
            CompileExpression(compiler, parser, &chunk->main, exStmt.expr);
            stmt->index += sizeof(ExprStmt);
        }
        break;
    case STATEMENT_BRANCH:
        {
            u32 ref0 = chunk->main.refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, 0, 0,4));

            BranchStmt b = *Get<BranchStmt>(compiler, stmt->index);
            stmt->index += sizeof(BranchStmt);

            Function* fn = Get<Function>(compiler, compiler->currentFunction);
            RegisterName r0 = CompileExpression(compiler, parser, &chunk->main, b.cond);
            RegisterName r1 = (RegisterName)fn->reg.colors[b.g_index];

            WriteChunkOpCode(&chunk->main, OP_NOT);
            WriteChunkIM8(&chunk->main, ToFrom(r0,r0));

            WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
            WriteChunkIM8(&chunk->main, r1);
            WriteChunkIM8(&chunk->main, 1);
            
            WriteChunkOpCode(&chunk->main, OP_AND);
            WriteChunkIM8(&chunk->main, ToFrom( r0,r1));

            WriteChunkOpCode(&chunk->main, OP_MOV_IM32);
            WriteChunkIM8(&chunk->main, r1);
            AnchorRefAtCurrent(&chunk->main, ref0);
            WriteChunkIM16(&chunk->main, 0xFFFF);
            WriteChunkIM16(&chunk->main, 0xFFFF);

            WriteChunkOpCode(&chunk->main, OP_CJMP);
            WriteChunkIM8(&chunk->main, ToFrom(r0,r1));

            Stmt thenBranch = b.thenBranch;
            Stmt elseBranch = b.elseBranch;

            if(elseBranch.index != 0) {
                u32 ref1 = chunk->main.refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, 0, 0,8));

                CompileStatement(compiler, chunk, parser, &thenBranch);
                WriteChunkOpCode(&chunk->main, OP_JMP_IM);
                AnchorRefAtCurrent(&chunk->main, ref1);
                for(u32 i = 0; i < 8 ; i++) WriteChunkIM8(&chunk->main, 0xFF);

                Cast<u64>(chunk->main.refs[ref0].value) = chunk->main.buffer.size;
                CompileStatement(compiler, chunk, parser, &elseBranch);
                Cast<u64>(chunk->main.refs[ref1].value) = chunk->main.buffer.size;

                *stmt = elseBranch;
            }
            else {
                CompileStatement(compiler, chunk, parser, &thenBranch);
                Cast<u64>(chunk->main.refs[ref0].value) = chunk->main.buffer.size;
                *stmt = thenBranch;
            }

        }
        break;
    case STATEMENT_FOR_LOOP:
        {
            u32 ref1 = chunk->main.refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, 0,0,8));
            u32 ref0 = chunk->main.refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, 0,0,4));

            ForStmt* loop = Get<ForStmt>(compiler, stmt->index);
            stmt->index += sizeof(ForStmt);
            Stmt init = loop->init;
            CompileStatement(compiler, chunk, parser, &init);

            Function* fn = Get<Function>(compiler, compiler->currentFunction);
            RegisterName r1 = (RegisterName)fn->reg.colors[loop->g_index];
            Cast<u64>(chunk->main.refs[ref1].value) = chunk->main.buffer.size;
            if(loop->cond.index != 0) {
                RegisterName r0 = CompileExpression(compiler, parser, &chunk->main, loop->cond);
                RegisterName r2 = r0;
                
                Expr expr = loop->cond;
                ExprType t = FirstNotGroupExpr(parser, &expr);
                if(t == EXPRESSION_VARIABLE_LOAD) {
                    r2 = (RegisterName)fn->reg.colors[loop->g_index2];
                    WriteChunkOpCode(&chunk->main, OP_MOV64);
                    WriteChunkIM8(&chunk->main, ToFrom(r2,r0) );
                }
                
                WriteChunkOpCode(&chunk->main, OP_NOT);
                WriteChunkIM8(&chunk->main, ToFrom(r2,r2) );
                WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                WriteChunkIM8(&chunk->main, r1 );
                WriteChunkIM8(&chunk->main, 1 );
                WriteChunkOpCode(&chunk->main, OP_AND);
                WriteChunkIM8(&chunk->main, ToFrom(r2,r1) );

                WriteChunkOpCode(&chunk->main, OP_MOV_IM32);
                WriteChunkIM8(&chunk->main, r1 );
                AnchorRefAtCurrent(&chunk->main, ref0);
                for(u32 i = 0; i < 4 ; i++) WriteChunkIM8(&chunk->main, 0xFF );

                WriteChunkOpCode(&chunk->main, OP_CJMP);
                WriteChunkIM8(&chunk->main, ToFrom(r2,r1) );
            }
            else {
                ref0 = ~u32(0);
                chunk->main.refs.PopBack();
            }

            Stmt body = loop->body;
            CompileStatement(compiler, chunk, parser, &body);
            Stmt inc = loop->inc;
            CompileStatement(compiler, chunk, parser, &inc);

            WriteChunkOpCode(&chunk->main, OP_JMP_IM);
            AnchorRefAtCurrent(&chunk->main, ref1);
            for(u32 i = 0; i < 8 ; i++) WriteChunkIM8(&chunk->main, 0xFF );

            if(ref0 != ~u32(0)) {
                Cast<u64>(chunk->main.refs[ref0].value) = chunk->main.buffer.size;
            }
            *stmt = body;
        }
        break;
    case STATEMENT_PRINT:
       {    
            stmt->index += sizeof(PrintStmt);
            while( Cast<ExprType>(compiler->mem + stmt->index) != EXPRESSION_NULL) {
                SavePrintStrRegs(compiler, &chunk->main, Cast<u32>(compiler->mem + stmt->index + 4));

                Expr expr = Cast<Expr>(compiler->mem + stmt->index);
                GetTypeExpr(compiler, parser, expr, compiler->scratchMem, TypeExpr{0});
                TypeName type = GetLastType(compiler->scratchMem, TypeExpr{0});

                RegisterName r;
                switch(type) {
                default:LOGASSERT(false, "Uknown print type");break;
                case TYPE_PRIMARY_INT8:case TYPE_PRIMARY_INT16:case TYPE_PRIMARY_INT32:case TYPE_PRIMARY_INT64:
                    {
                        r = CompileExpression(compiler, parser, &chunk->main, expr);
                        WriteChunkOpCode(&chunk->main, OP_MOV64);
                        WriteChunkIM8(&chunk->main, ToFrom(R5,r));
                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R0);
                        WriteChunkIM8(&chunk->main, 3);
                        WriteChunkOpCode(&chunk->main, OP_INTERUPT);
                    }
                    break;
                case TYPE_PRIMARY_UINT8:case TYPE_PRIMARY_UINT16:case TYPE_PRIMARY_UINT32:case TYPE_PRIMARY_UINT64:integer_expr: case TYPE_PRIMARY_FN:
                    {
                        r = CompileExpression(compiler, parser, &chunk->main, expr);
                        WriteChunkOpCode(&chunk->main, OP_MOV64);
                        WriteChunkIM8(&chunk->main, ToFrom(R5,r));
                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R0);
                        WriteChunkIM8(&chunk->main, 2);
                        WriteChunkOpCode(&chunk->main, OP_INTERUPT);
                    }
                    break;
                case TYPE_PRIMARY_F32:
                    {
                        r = CompileExpression(compiler, parser, &chunk->main, expr);
                        WriteChunkOpCode(&chunk->main, OP_MOV64);
                        WriteChunkIM8(&chunk->main, ToFrom(R5,r));
                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R0);
                        WriteChunkIM8(&chunk->main, 5);
                        WriteChunkOpCode(&chunk->main, OP_INTERUPT);
                    }
                    break;
                case TYPE_PRIMARY_F64:
                    {
                        r = CompileExpression(compiler, parser, &chunk->main, expr);

                        WriteChunkOpCode(&chunk->main, OP_MOV64);
                        WriteChunkIM8(&chunk->main, ToFrom(R5,r));
                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R0);
                        WriteChunkIM8(&chunk->main, 4);
                        WriteChunkOpCode(&chunk->main, OP_INTERUPT);
                    }
                    break;
                case TYPE_PRIMARY_BOOL:
                    {
                        u32 ref0 = chunk->main.refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, 0,0,4));
                        u32 ref1 = chunk->main.refs.PushBack(RelativeReference(RELATIVE_TO_STRING_CONSTANTS, 0,compiler->falseStrConstantOffset,4));
                        u32 ref2 = chunk->main.refs.PushBack(RelativeReference(RELATIVE_TO_STRING_CONSTANTS, 0,compiler->trueStrConstantOffset,4));
                        u32 ref3 = chunk->main.refs.PushBack(RelativeReference(RELATIVE_TO_CODE_MAIN, 0,0,8));

                        r = CompileExpression(compiler, parser, &chunk->main, expr);
                        RegisterName cmpReg = r;
                        ASSERT(cmpReg < RRV);

                        RegisterName jmpReg = (cmpReg == R0 ? R1 : R0);

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM32);
                        WriteChunkIM8(&chunk->main, jmpReg);
                        AnchorRefAtCurrent(&chunk->main, ref0);
                        for(u32 i = 0; i < 4 ; i++) WriteChunkIM8(&chunk->main, 0xFF);

                        WriteChunkOpCode(&chunk->main, OP_CJMP);
                        WriteChunkIM8(&chunk->main, ToFrom(cmpReg, jmpReg));

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM32);
                        WriteChunkIM8(&chunk->main, R4);
                        AnchorRefAtCurrent(&chunk->main, ref1);
                        for(u32 i = 0; i < 4 ; i++) WriteChunkIM8(&chunk->main, 0xFF);

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R2);
                        WriteChunkIM8(&chunk->main, sizeof("false")-1);

                        WriteChunkOpCode(&chunk->main, OP_JMP_IM);
                        AnchorRefAtCurrent(&chunk->main, ref3);
                        for(u32 i = 0; i < 8; i++) WriteChunkIM8(&chunk->main, 0xFF);
                        Cast<u64>(chunk->main.refs[ref0].value) = chunk->main.buffer.size;

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM32);
                        WriteChunkIM8(&chunk->main, R4);
                        AnchorRefAtCurrent(&chunk->main, ref2);
                        for(u32 i = 0; i < 4 ; i++) WriteChunkIM8(&chunk->main, 0xFF);

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R2);
                        WriteChunkIM8(&chunk->main, sizeof("true")-1);
                        
                        Cast<u64>(chunk->main.refs[ref3].value) = chunk->main.buffer.size;
                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R0);
                        WriteChunkIM8(&chunk->main, 1);
                        WriteChunkOpCode(&chunk->main, OP_MOV64);
                        WriteChunkIM8(&chunk->main, ToFrom(R1,R0));
                        WriteChunkOpCode(&chunk->main, OP_MOV64);
                        WriteChunkIM8(&chunk->main, ToFrom(R5,R0));
                        WriteChunkOpCode(&chunk->main, OP_INTERUPT);
                    }
                    break;

                case TYPE_MODIFIER_POINTER:
                    {
                        if( Cast<TypeName>(compiler->scratchMem + GetNthType(compiler->scratchMem, TypeExpr{0}, 1).index ) != TYPE_PRIMARY_CHAR ) {
                            goto integer_expr;
                        }
                        Function* fn = Get<Function>(compiler, compiler->currentFunction);
                        LiteralExpr* node = Get<LiteralExpr>(parser, expr.index);

                        RegisterName r =  (RegisterName)fn->reg.colors[node->g_index];
                        u32 len = node->literal.lenght;

                        char msg[len]{};
                        i32 size = 0;
                        for(u32 i = 0; i < node->literal.lenght; i++,size++) {
                            char c;
                            if( node->literal.text[i] == '\\' && node->literal.text[i+1] == 'n' ) {
                                c = '\n';
                                i += 2;
                            }
                            else {
                                c = node->literal.text[i];
                            }
                            msg[size] = c;
                        }
                        len = size;
                        size--;
                        for(; size > -1; size--) {
                            char c = msg[size];
                            WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                            WriteChunkIM8(&chunk->main, r);
                            WriteChunkIM8(&chunk->main, c);
                            WriteChunkOpCode(&chunk->main, OP_PUSH);
                            WriteChunkIM8(&chunk->main, ToFrom(r,R0));
                        }

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R0);
                        WriteChunkIM8(&chunk->main, 1);

                        WriteChunkOpCode(&chunk->main, OP_GET_STACK_PTR);
                        WriteChunkIM8(&chunk->main, R5);

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R4);
                        WriteChunkIM8(&chunk->main, len);
                        WriteChunkOpCode(&chunk->main, OP_INTERUPT);

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM16);
                        WriteChunkIM8(&chunk->main, R0);
                        WriteChunkIM16(&chunk->main, len);
                        WriteChunkOpCode(&chunk->main, OP_ADD);
                        WriteChunkIM8(&chunk->main, ToFrom(RSP,R0));
                    }
                    break;
                case TYPE_PRIMARY_CHAR:
                    {
                        r = CompileExpression(compiler, parser, &chunk->main, expr);
                        WriteChunkOpCode(&chunk->main, OP_PUSH);
                        WriteChunkIM8(&chunk->main, ToFrom(r,R0));

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R0);
                        WriteChunkIM8(&chunk->main, 1);

                        WriteChunkOpCode(&chunk->main, OP_GET_STACK_PTR);
                        WriteChunkIM8(&chunk->main, R5);

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM8);
                        WriteChunkIM8(&chunk->main, R4);
                        WriteChunkIM8(&chunk->main, 1);
                        WriteChunkOpCode(&chunk->main, OP_INTERUPT);

                        WriteChunkOpCode(&chunk->main, OP_MOV_IM16);
                        WriteChunkIM8(&chunk->main, R0);
                        WriteChunkIM16(&chunk->main, 1);
                        WriteChunkOpCode(&chunk->main, OP_ADD);
                        WriteChunkIM8(&chunk->main, ToFrom(RSP,R0));
                    }
                    break;
                }

                RestorePrintStrRegs(compiler, &chunk->main, Cast<u32>(compiler->mem + stmt->index + 4));
                stmt->index += sizeof(Expr) * 2;
            }
            stmt->index += sizeof(Expr);
            
        }
        break;
    }

    return true;
}





bool Compile(Compiler* compiler , Chunk* chunk, Parser* parser) {

    compiler->falseStrConstantOffset = WriteChunkDataString(chunk, "false\n", 6);
    compiler->trueStrConstantOffset = WriteChunkDataString(chunk, "true\n", 5);

    Stmt program{compiler->allocator};
    compiler->entryPoint.index = ~u32(0);

    for(;;) {

        if(PeekToken(parser->tokenizer).type != TOKEN_EOF) {
            ParseStatement(compiler, parser, chunk);
        }
        else {
            break;
        }
        if(compiler->panic) Sync(compiler, parser);
    }
    Allocate<Stmt>(compiler)->index = STATEMENT_NON;

    if(compiler->entryPoint.index == ~u32(0)) {
        std::cout << "ERROR: entry point not found" << std::endl;
        return false;
    }
    if(compiler->error || parser->error) {
        return false;
    }

    while(CompileStatement(compiler, chunk, parser, &program));

    return true;
}


CompileConfig ParseArgs(u32 argc, char** args) {

    CompileConfig ret{};
    ret.mem_cap = 2 * MEGA_BYTE;

    if(argc < 2) {
        std::cout << "ERROR: no arguments please run with --help" << std::endl;
        exit(1);
    }

    for(u32 i = 1; i < argc; i++) {
        if(StrCmp(args[i], "--help")) {
            std::cout << "--help            print this message" << std::endl;
            std::cout << "--dissassemble    dumps the dissassembly to the console" << std::endl;
            std::cout << "--in              specifies the source file to be compiled " << std::endl;
            std::cout << "--out             specifies the output binary file " << std::endl;
            std::cout << "--memory          specifies the amount of memory(KB) the compiler should use default is 2000 minimum is 128" << std::endl;
        }
        else if(StrCmp(args[i], "--dissassemble")) {
            ret.dump_dissassembly = true;
        }
        else if(StrCmp(args[i], "--in")) {
            if(argc-1 <= i) {
                std::cout << "ERROR: missing arg" << std::endl;
                exit(1);
            }
            ret.input_file = args[i+1];
            i += 1;
        }
        else if(StrCmp(args[i], "--out")) {
            if(argc-1 <= i) {
                std::cout << "ERROR: missing arg" << std::endl;
                exit(1);
            }
            ret.output_file = args[i+1];
            i += 1;
        }
        else if(StrCmp(args[i], "--memory")) {
            if(argc-1 <= i) {
                std::cout << "ERROR: missing arg" << std::endl;
                exit(1);
            }
            Token num;
            num.text = args[i+1];
            num.lenght = StrLen(args[i+1]);
            u64 n = GetU64(num);
            ret.mem_cap = max<u64>(n, 128) * KILO_BYTE;
            i += 1;
        }
        else if(StrCmp(args[i], "-O1")) {
            ret.optimization_constant_propogation = true;
        }
        else {
            std::cout << "ERROR: unknown argument please run with --help" << std::endl;
        }
    }

    u32 size = ret.mem_cap / 10;

    ret.parser_mem = ret.mem_cap / 2;
    ret.comp_mem = 64 + size;

    ret.expr_allocator = 64;
    ret.comp_allocator = ret.expr_allocator + size;
    ret.comp_fn_allocator = ret.comp_allocator + size;
    ret.comp_misc_allocator = ret.comp_fn_allocator + size;
    ret.comp_scratch_mem = ret.comp_misc_allocator + size;

    return ret;
}

extern byte* base;
void InitParser(Parser* parser, CompileConfig* const confg) {
    u32 S;
    parser->source = (char*)ReadFileTerminated(confg->input_file,nullptr, &S);
    if(parser->source == nullptr) {
        std::cout << "input file doesn't exist" << std::endl;
        exit(1);
    }
    parser->tokenizer.at = parser->source;
    parser->tokenizer.line = 1;
    parser->mem = base + confg->parser_mem;
    parser->exprAllocator = confg->expr_allocator;

    parser->error = false;
    parser->tokenBuffer.Init();
}

void AssignBit(byte* bitArray, u32 i, bool val) {
    byte b = bitArray[i >> 3];
    u32 mod = i & 7;
    b &= ~(1 << mod);
    b |= (val << mod);
}
void ClearBit(byte* bitArray, u32 i) {
    bitArray[i >> 3] &= ~(1 << (i & 7)); 
}
void SetBit(byte* bitArray, u32 i) {
    bitArray[i >> 3] |= (1 << (i & 7));
}
bool GetBit(byte* bitArray, u32 i) {
    return (bitArray[i >> 3] >> (i & 7)) & 1;
}
void InitCompiler(Compiler* compiler, Parser* parser, CompileConfig* const confg) {
    InitParser(parser, confg);

    *compiler = {};
    compiler->mem = (parser->mem + confg->comp_mem);
    compiler->scratchMem = (parser->mem + confg->comp_scratch_mem);
    compiler->allocator = confg->comp_allocator;
    compiler->fnAllocator = confg->comp_fn_allocator;
    compiler->miscAllocator = confg->comp_misc_allocator;

    compiler->stack.Init();
    compiler->hiddenParams.Init();
    compiler->registerVariable.Init();

    compiler->panic = false;
    compiler->error = false;
    compiler->currentFunction = 0;
    compiler->currentI = 0;
    compiler->entryPoint.index = ~u32(0);
}

i32 main(i32 argc, char** args) {


    CompileConfig config = ParseArgs(argc, args);
    base = (byte*)mmap(nullptr, config.mem_cap, PROT_WRITE | PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, 0,0);
    init_my_malloc(base, config.mem_cap / 2);

    if( config.input_file != nullptr && (config.dump_dissassembly || (config.output_file != nullptr) ) ) {

        Parser parser;
        Compiler comp;
        InitCompiler(&comp, &parser, &config);
        comp.config = &config;
        Chunk chunk{};

        const char* name = config.output_file != nullptr ? config.output_file : config.input_file;
        MemCpy(chunk.header.name, name, StrLen(name));
        InitChunk(&chunk, 4 * KILO_BYTE, 16 * KILO_BYTE, KILO_BYTE , KILO_BYTE,KILO_BYTE);

        if(Compile(&comp ,&chunk, &parser) ) {

            Executable* program = CreateExecutable(&chunk);
            if(config.dump_dissassembly) DumpExecutable(program);
            if(config.output_file != nullptr) {
                FILE* file = fopen(config.output_file , "w");
                fwrite(program , 1 , program->header.size , file);
                fclose(file);
            }
            my_free(program);
        }

        FreeChunk(&chunk);
        my_free(parser.mem);
        my_free(parser.source);
        parser.tokenBuffer.Free();
        comp.stack.Free();
        comp.hiddenParams.Free();
    }
    munmap(base, config.mem_cap);

    return 0;
}