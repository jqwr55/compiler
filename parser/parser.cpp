#include <common.h>

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
constexpr const char* TOKEN_STRS[] = {
    "TOKEN_EOF",
    "TOKEN_IDENTIFIER",
    "TOKEN_KEYWORD_MAIN",
    "TOKEN_KEYWORD_PRINT",
    "TOKEN_KEYWORD_IF",
    "TOKEN_KEYWORD_ELSE",
    "TOKEN_KEYWORD_FOR",
    "TOKEN_KEYWORD_DO",
    "TOKEN_KEYWORD_RETURN",
    "TOKEN_KEYWORD_STRUCT",
    "TOKEN_KEYWORD_ARROW",
    "TOKEN_KEYWORD_FN",
    "TOKEN_KEYWORD_ASSUME",
    "TOKEN_KEYWORD_COLD",
    "TOKEN_KEYWORD_HOT",
    "TOKEN_KEYWORD_AUTO",
    "TOKEN_KEYWORD_RESTRICT",
    "TOKEN_KEYWORD_VOLATILE",
    "TOKEN_KEYWORD_ATOMIC",
    "TOKEN_KEYWORD_CONST",
    "TOKEN_OPEN_PAREN",
    "TOKEN_CLOSE_PAREN",
    "TOKEN_OPEN_BRACKET",
    "TOKEN_CLOSE_BRACKET",
    "TOKEN_OPEN_BRACES",
    "TOKEN_CLOSE_BRACES",
    "TOKEN_PLUS_EQUALS",
    "TOKEN_MINUS_EQUALS",
    "TOKEN_ASTERISTK_EQUALS",
    "TOKEN_SLASH_EQUALS",
    "TOKEN_AMPERSAND_EQUALS",
    "TOKEN_VERTICAL_BAR_EQUALS",
    "TOKEN_CIRCUMFLEX_EQUALS",
    "TOKEN_TILDE_EQUALS",
    "TOKEN_EQUALS_EQUALS",
    "TOKEN_EXCLAMATION_EQUALS",
    "TOKEN_AMPERSAND_AMPERSAND",
    "TOKEN_VERTICAL_BAR_VERTICAL_BAR",
    "TOKEN_TILDE",
    "TOKEN_AMPERSAND",
    "TOKEN_VERTICAL_BAR",
    "TOKEN_CIRCUMFLEX",
    "TOKEN_NEG",
    "TOKEN_ASTERISK",
    "TOKEN_SLASH",
    "TOKEN_PLUS",
    "TOKEN_MINUS",
    "TOKEN_LSHIFT_LSHIFT",
    "TOKEN_RSHIFT_RSHIFT",
    "TOKEN_LSHIFT",
    "TOKEN_RSHIFT",
    "TOKEN_LSHIFT_EQUALS",
    "TOKEN_RSHIFT_EQUALS, ",
    "TOKEN_DOT",
    "TOKEN_COMMA",
    "TOKEN_COLON",
    "TOKEN_SEMICOLON",
    "TOKEN_PLUS_PLUS",
    "TOKEN_MINUS_MINUS",
    "TOKEN_EQUAL_SIGN",
    "TOKEN_SUB_SCRIPT_OPR",
    "TOKEN_EXCLAMATION_MARK",
    "TOKEN_CHAR_LITERAL",
    "TOKEN_STRING_LITERAL",
    "TOKEN_NUMBER_LITERAL",
    "TOKEN_BOOL_LITERAL",
    "TOKEN_NULL_LITERAL",
    "TOKEN_UNKNOWN",
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

u32 GetLineNumber(const char *source, char *at) {
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

struct Expr {
    i32 index;
    void operator = (Expr* addr) {
        index = (addr == nullptr ? 0 : (i32)((byte*)addr - (byte*)this));
    }
    void operator = (Expr e) = delete;
    Expr* operator*() {
        ASSERT(index != 0);
        return (Expr*)((byte*)this + index);
    }
    Expr* operator->() {
        ASSERT(index != 0);
        return (Expr*)((byte*)this + index);
    }
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

typedef Expr TypeExpr;
struct FnTypeExpr : TypeExpr {
    u32      param_count;
    TypeExpr params;
    TypeExpr ret_t;
    TypeExpr modifier;
};
struct ArrayTypeExpr : TypeExpr {
    Expr     arraySizeExpr;
    TypeExpr modifier;
};
struct StructMemberTypeExpr : TypeExpr {
    Token    name;
    TypeExpr type;
};
struct StructTypeExpr : TypeExpr {
    u32      memberCount;
    TypeExpr members;
    TypeExpr modifier;
};
struct StructSymbol {
    Token name;
    TypeExpr* ptr;
    Token* members;
    u32 memberCount;
};

enum ExprType : u32 {
    EXPR_NULL,

    EXPR_LITERAL,
    EXPR_UNARY,
    EXPR_BINARY,

    EXPR_SYMBOL,
    EXPR_MEMORY_LOAD,
    EXPR_MEMORY_STORE,
    EXPR_MEMBER,

    EXPR_CALL,
    EXPR_CONVERSION,
    EXPR_ADDR_OF,
    EXPR_PEEL_TYPE,

    EXPR_COUNT,
};
struct Value {
    byte mem[8];
    u8 type;
};
struct Symbol {
    void* extra;
    Token name;
    TypeExpr type;
    u32 scope;
};


struct LiteralExpr : Expr  {
    Value v;
};
struct UnaryExpr : Expr {
    u32 opr;
    Expr e;
};
struct BinaryExpr : Expr {
    u32 opr;
    Expr left;
    Expr right;
};
struct SymbolExpr : Expr {
    Symbol symbol;
};
struct MemoryLoadExpr : Expr {
    Expr address;
};
struct MemoryStoreExpr : Expr {
    Expr address;
    Expr value;
};
struct MemberExpr : Expr {
    Token name;
    Expr prev;
    u32 offset;
};
struct CallExpr : Expr {
    Expr callee;
    u32 argCount;
    Expr args[];
};
struct ConversionExpr : Expr {
    Expr from;
    TypeExpr to;
};
struct AddrOfExpr : Expr {
    Expr e;
};
struct PeelTypeExpr : Expr {
    Expr e;
};

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

struct ParserState {
    const char* begin;
    Token* stream;
    SymbolTable table;
    u32 currentToken;
};


struct SymbolTable {
    Symbol* symbols;
    u32 symbolCount;
};

u32 FindSymbol(SymbolTable table, Token name) {
    for(u32 i = 0; i < table.symbolCount; i++) {
        if(TokensEquals(name, table.symbols[i].name)) {
            return i;
        }
    }
    return ~u32(0);
}

Token GetPrevToken(ParserState* ctx) {
    ASSERT(ctx->currentToken);
    return ctx->stream[ctx->currentToken-1];
}
Token GetCurrentToken(ParserState* ctx) {
    return ctx->stream[ctx->currentToken];
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

u32 Match(Token token, const TokenType* types, u32 count) {
    for(u32 i = 0; i < count; i++) {
        if(token.type == types[i]) return i;
    }

    return ~u32(0);
}
bool ExpectToken(ParserState* ctx, TokenType t) {
    if(ctx->stream[ctx->currentToken++].type != t) {
        global_print("%s%s%c", "Expected ", TOKEN_STRS[t], '\n');
        return false;
    }
    return true;
}
void Expect(bool cond , const char* errMsg) {
    if(!cond) {
        global_print("%s%s%c", "ERROR ", errMsg, '\n');
    }
}



bool IsTypeExpr(ParserState* ctx) {

    auto t = ctx->stream[ctx->currentToken];
    switch (t.type) {
    case TOKEN_IDENTIFIER:
        for(auto i = typeTokens; i < 1[&typeTokens]; i++) {
            if(TokenEquals(t, i->name)) {
                return true;
            }
        }
        for (u32 i = 0; i < ctx->table.symbolCount; i++) {
            if (TokensEquals(t, ctx->table.symbols[i].name)) {
                return true;
            }
        }
        break;
    case TOKEN_OPEN_PAREN:
        {
            ctx->currentToken++;
            bool ret = IsTypeExpr(ctx);
            ctx->currentToken--;
            return ret;
        }
    case TOKEN_KEYWORD_FN:
    case TOKEN_KEYWORD_AUTO:
        return true;
    default:
        return false;
    }

    return false;
}


Token NextToken(ParserState* ctx) {
    auto t = ctx->stream[ctx->currentToken++];
    return t;
}
Token PeekToken(ParserState* ctx) {
    return ctx->stream[ctx->currentToken];
}
byte* CpyTypeExpr(byte* dst, TypeExpr* srcExpr);
TypeExpr* ParseTypeExpression(ParserState* ctx, LinearAllocator* alloc, SymbolTable symbolTable);
TypeExpr* PrimaryTypeExpression(ParserState* ctx, LinearAllocator* alloc) {

    TypeExpr* ret = (TypeExpr*)linear_allocator_top(alloc);
    Token base = NextToken(ctx);
    for(auto i = typeTokens; i < 1[&typeTokens]; i++) {
        if(TokenEquals(base, i->name)) {
            auto node = (TypeName*)linear_allocate(alloc, sizeof(TypeName));
            *node = i->type;
            return ret;
        }
    }
    for(u32 i = 0; i < ctx->table.symbolCount; i++) {
        if(TokensEquals(ctx->table.symbols[i].name, base)) {
            auto dst = (byte*)linear_allocator_top(alloc);
            auto end = CpyTypeExpr(dst, *(ctx->table.symbols[i].type));
            alloc->top += end - dst;
            return (TypeExpr*)dst;
        }
    }
    constexpr TokenType t[] = {
        TOKEN_KEYWORD_FN,
        TOKEN_OPEN_PAREN,
        TOKEN_IDENTIFIER,
    };

    u32 match = Match(base, t, 3);
    switch(match) {
    case 0: // fn
        {
            auto node = (FnTypeExpr*)linear_allocate(alloc, sizeof(FnTypeExpr));
            ExpectToken(ctx, TOKEN_OPEN_PAREN);
            node->index = TYPE_PRIMARY_FN;
            node->param_count = 0;
            node->params = 0;

            u32 it = ctx->currentToken;
            do {
                node->param_count++;
                Token q;
                for(; q.type != TOKEN_COMMA && q.type != TOKEN_CLOSE_PAREN; it++, q = ctx->stream[it]);
            } while(ctx->stream[it].type == TOKEN_COMMA);

            node->params = (TypeExpr*)linear_allocator_top(alloc);
            auto paramTypes = (TypeExpr*)linear_allocate(alloc, node->param_count * sizeof(TypeExpr));

            for(u32 i = 0; i < node->param_count; i++) {
                *(paramTypes++) = ParseTypeExpression(ctx, alloc);
                auto t = ctx->stream[ctx->currentToken];
                if(i+1 != node->param_count) {
                    ExpectToken(ctx, TOKEN_COMMA);
                }
            }
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
            if (NextToken(ctx).type == TOKEN_KEYWORD_ARROW) {
                node->ret_t = ParseTypeExpression(ctx, alloc);
            }
            else {
                node->ret_t.index = alloc->top;
                auto last = (TypeName*)linear_allocate(alloc, sizeof(TypeName) * 2);
                last[0] = TYPE_PRIMARY_VOID;
                last[1] = TYPE_NON;
            }
        }
        break;
    case 1: // (TypeExpr) | (TypeExpr, TypeExpr, ...)
        {
            auto m0 = ParseTypeExpression(ctx, alloc);
            if(GetCurrentToken(ctx).type == TOKEN_COMMA) {
                TypeExpr* m[255];
                m[0] = m0;
                u32 c = 1;
                while(GetCurrentToken(ctx).type != TOKEN_CLOSE_PAREN) {
                    ExpectToken(ctx, TOKEN_COMMA);
                    ASSERT(c < 255);
                    m[c++] = ParseTypeExpression(ctx, alloc);
                }
                u32 size = (byte*)linear_allocator_top(alloc) - (byte*)m0;
                u32 offset = sizeof(StructTypeExpr) + c * sizeof(TypeExpr);
                memcpy((byte*)m0 + offset, m0, size);

                auto st = (StructTypeExpr*)m0;
                st->index = TYPE_STRUCTURE;
                st->memberCount = c;
                auto t = (TypeExpr*)(st + 1);
                st->members = t;
                for(u32 i = 0; i < c; i++) {
                    t[i] = (TypeExpr*)( ((byte*)(m[i])) + offset);
                }

                alloc->top += offset;
                st->modifier = (TypeExpr*)linear_allocator_top(alloc);
            }
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
        }
        break;
    case 2: // struct
        {
            for (u32 i = 0; i < ctx->table.symbolCount; i++) {
                if (TokensEquals(base, ctx->table.symbols[i].name)) {
                    *((TypeExpr*)linear_allocate(alloc, sizeof(TypeExpr))) = *(ctx->table.symbols[i].type);
                    return ret;
                }
            }
            global_print("%s%s*%s", "Undefined type symbol(", base.text, base.lenght ,") used in type expression\n");
        }
        break;
    }

    return ret;
}
Expr* ParseAssignment(ParserState* ctx, LinearAllocator* alloc, SymbolTable symbolTable);
TypeExpr* ParseTypeModifierExpression(ParserState* ctx, LinearAllocator* alloc) {


    TypeExpr* expr = (TypeExpr*)linear_allocator_top(alloc);
    constexpr TokenType tokens[] = {
        TOKEN_ASTERISK,
        TOKEN_OPEN_PAREN,
        TOKEN_KEYWORD_VOLATILE,
        TOKEN_KEYWORD_CONST,
        TOKEN_KEYWORD_ATOMIC,
        TOKEN_OPEN_BRACKET,
    };
    constexpr TypeName modifiers[] = {
        TYPE_MODIFIER_VOLATILE,
        TYPE_MODIFIER_CONST,
        TYPE_MODIFIER_ATOMIC,
    };
    for(bool run = true;run;) {
        
        u32 match = Match(GetCurrentToken(ctx), tokens, 6);
        switch(match) {
        case 0:
            {
                ctx->currentToken++;
                auto ptrT = (TypeName*)linear_allocate(alloc, sizeof(TypeName));
                *ptrT = (ctx->stream[ctx->currentToken].type == TOKEN_KEYWORD_RESTRICT ? TYPE_MODIFIER_RESTRICTED_POINTER : TYPE_MODIFIER_POINTER);
                break;
            }
        case 1:
            ctx->currentToken++;
            ParseTypeModifierExpression(ctx, alloc);
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
            break;
        case 2: case 3: case 4:
            {
                ctx->currentToken++;
                auto type = (TypeName*)linear_allocate(alloc, sizeof(TypeName));
                *type = modifiers[match-4];
            }
            break;
        case 5:
            {
                ctx->currentToken++;
                auto node = (ArrayTypeExpr*)linear_allocate(alloc, sizeof(ArrayTypeExpr));
                node->index = TYPE_MODIFIER_ARRAY;
                node->arraySizeExpr = ParseAssignment(ctx, alloc);
                ExpectToken(ctx, TOKEN_CLOSE_BRACKET);
                break;
            } 
        case ~u32(0):
            run = false;
            break;
        }
    }

    return expr;
}
TypeExpr* ParseTypeExpression(ParserState* ctx, LinearAllocator* alloc) {

    TypeExpr* expr = PrimaryTypeExpression(ctx, alloc);
    if (expr->index == TYPE_PRIMARY_FN) {
        Mem<FnTypeExpr>(expr).modifier = ParseTypeModifierExpression(ctx, alloc);
    }
    else {
        ParseTypeModifierExpression(ctx, alloc);
    }

    Mem<TypeName>(linear_allocate(alloc, sizeof(TypeName))) = TYPE_NON;
    return expr;
}

u32 GetTypeExprMemorySize(byte* base, TypeExpr* expr) {

    byte* begin = (byte*)expr;
    while (expr->index != TYPE_NON) {
        switch (expr->index) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            expr = *Mem<FnTypeExpr>(expr).modifier;
            break;
        case TYPE_MODIFIER_ARRAY:
            expr = *Mem<ArrayTypeExpr>(expr).modifier;
            break;
        default:
            expr++;
            break;
        }
    }
    expr++;
    return (byte*)expr - begin;
}

byte* CpyExpr(byte* dst, Expr* srcExpr) {

    switch(srcExpr->index) {
    case EXPR_NULL:
        ASSERT(false);break;
    case EXPR_LITERAL:
        Mem<LiteralExpr>(dst).index = EXPR_LITERAL;
        Mem<LiteralExpr>(dst).v = Mem<LiteralExpr>(srcExpr).v;
        return dst + sizeof(LiteralExpr);
    case EXPR_UNARY:
        Mem<UnaryExpr>(dst).index = EXPR_UNARY;
        Mem<UnaryExpr>(dst).opr = Mem<UnaryExpr>(srcExpr).opr;
        Mem<UnaryExpr>(dst).e = (Expr*)dst;
        dst += sizeof(LiteralExpr);
        dst = CpyExpr(dst, *Mem<UnaryExpr>(srcExpr).e);
        return dst;
    case EXPR_BINARY:
        {
            auto dstE = Mem<BinaryExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<BinaryExpr>(srcExpr);
            dstE.index = EXPR_BINARY;
            dstE.opr = srcE.opr;
            dstE.left = (Expr*)dst;

            dst += sizeof(BinaryExpr);
            dst = CpyExpr(dst, *srcE.left);
            dst = CpyExpr(dst, *srcE.right);

            return dst;
        }
    case EXPR_SYMBOL:
        {
            auto dstE = Mem<SymbolExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<SymbolExpr>(srcExpr);
            dstE.index = EXPR_SYMBOL;
            dstE.symbol.type = *srcE.symbol.type;
            dstE.symbol.scope = srcE.symbol.scope;
            dstE.symbol.name = srcE.symbol.name;
            dstE.symbol.extra = srcE.symbol.extra;
            return dst;
        }
    case EXPR_MEMORY_LOAD:
        {
            auto dstE = Mem<MemoryLoadExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<MemoryLoadExpr>(srcExpr);
            dstE.index = EXPR_MEMORY_LOAD;
            dstE.address = (Expr*)dst;
            return CpyExpr(dst, *srcE.address);
        }
    case EXPR_MEMORY_STORE:
        {
            auto dstE = Mem<MemoryStoreExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<MemoryStoreExpr>(srcExpr);
            dstE.index = EXPR_MEMORY_STORE;
            dstE.address = (Expr*)dst;
            dst = CpyExpr(dst, *srcE.address);
            dstE.value = (Expr*)dst;
            dst = CpyExpr(dst, *srcE.value);

            return dst;
        }
    case EXPR_MEMBER:
        {
            auto dstE = Mem<MemberExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<MemberExpr>(srcExpr);
            dstE.index = EXPR_MEMBER;
            dstE.prev = (Expr*)dst;
            dst = CpyExpr(dst, *srcE.prev);
            dstE.name = srcE.name;
            dstE.offset = srcE.offset;

            return dst;
        }
    case EXPR_CALL:
        {
            auto dstE = Mem<CallExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<CallExpr>(srcExpr);
            dstE.index = EXPR_CALL;
            dstE.callee = (Expr*)dst;
            dst = CpyExpr(dst, *srcE.callee);
            for(u32 i = 0; i < srcE.argCount; i++) {
                dstE.args[i] = (Expr*)dst;
                dst = CpyExpr(dst, *srcE.args[i]);
            }
            srcE.argCount =  dstE.argCount;
            return dst;
        }
    case EXPR_CONVERSION:
        {
            auto dstE = Mem<ConversionExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<ConversionExpr>(srcExpr);
            dstE.index = EXPR_CONVERSION;
            dstE.from = (Expr*)dst;
            dst = CpyExpr(dst, *srcE.from);

            dstE.to = (Expr*)dst;
            dst = CpyExpr(dst, *srcE.to);

            return dst;
        }
    case EXPR_ADDR_OF:
        {
            auto dstE = Mem<AddrOfExpr>(dst);
            dst += sizeof(AddrOfExpr);

            auto srcE = Mem<AddrOfExpr>(srcExpr);
            dstE.index = EXPR_ADDR_OF;

            dstE.e = (Expr*)dst;
            dst = CpyExpr(dst, *srcE.e);

            return dst;
        }
    case EXPR_PEEL_TYPE:
        {
            auto dstE = Mem<PeelTypeExpr>(dst);
            dst += sizeof(PeelTypeExpr);

            auto srcE = Mem<PeelTypeExpr>(srcExpr);
            dstE.index = EXPR_PEEL_TYPE;

            dstE.e = (Expr*)dst;
            dst = CpyExpr(dst, *srcE.e);

            return dst;
        }
    }
}

byte* CpyTypeExpr(byte* dst, TypeExpr* srcExpr) {

    auto t = Mem<TypeName>(srcExpr);
    while (t != TYPE_NON) {

        switch (t) {
        case TYPE_PRIMARY_FN:
            {
                FnTypeExpr* srcNode = (FnTypeExpr*)srcExpr;
                FnTypeExpr* dstNode = (FnTypeExpr*)dst;
                memcpy(dstNode, srcNode, sizeof(FnTypeExpr));
                dst += sizeof(FnTypeExpr);

                if (srcNode->param_count != 0) {
                    ASSERT(*srcNode->params);

                    dstNode->params = (TypeExpr*)dst;
                    dst += srcNode->param_count * sizeof(TypeExpr);
                    for (u32 i = 0; i < srcNode->param_count; i++) {
                        (*dstNode->params)[i] = (TypeExpr*)dst;
                        dst = CpyTypeExpr(dst, (*srcNode->params) + i);
                    }
                }

                dstNode->ret_t = (TypeExpr*)dst;
                dst = CpyTypeExpr(dst, *srcNode->ret_t);

                dstNode->modifier = (TypeExpr*)dst;
                srcExpr = *(srcNode->modifier);
            }
            break;
        case TYPE_MODIFIER_ARRAY:
            {
                auto& srcArr = Mem<ArrayTypeExpr>(srcExpr);
                auto& dstArr = Mem<ArrayTypeExpr>(dst);
                dst += sizeof(ArrayTypeExpr);

                dstArr.index = TYPE_MODIFIER_ARRAY;
                dstArr.arraySizeExpr = (Expr*)dst;
                dst = CpyExpr(dst, *srcArr.arraySizeExpr);
                dstArr.modifier = (TypeExpr*)dst;
                srcExpr = *(srcArr.modifier);
            }
            break;
        case TYPE_STRUCTURE:
            {
                auto& srcSt = Mem<StructTypeExpr>(srcExpr);
                auto& dstSt = Mem<StructTypeExpr>(dst);
                dstSt.index = TYPE_STRUCTURE;
                dstSt.memberCount = srcSt.memberCount;
                dstSt.members = (TypeExpr*)(dst + sizeof(StructTypeExpr));
                dst += sizeof(StructTypeExpr) + srcSt.memberCount * sizeof(TypeExpr);
                for(u32 i = 0; i < dstSt.memberCount; i++) {
                    (*dstSt.members)[i] = (TypeExpr*)dst;
                    dst = CpyTypeExpr(dst, *((*srcSt.members)[i]) );
                }
                dstSt.modifier = (TypeExpr*)dst;
                srcExpr = *srcSt.modifier;
            }
            break;
        default:
            memcpy(dst, srcExpr, sizeof(TypeName));
            srcExpr++;
            dst += sizeof(TypeName);
            break;
        }
        t = Mem<TypeName>(srcExpr);
    }

    Mem<TypeName>(dst) = TYPE_NON;
    dst += sizeof(TypeName);
    return dst;
}

TypeName GetLastType(TypeExpr* expr) {

    TypeName last = Mem<TypeName>(expr);
    while (Mem<TypeName>(expr) != TYPE_NON) {

        last = Mem<TypeName>(expr);
        switch (Mem<TypeName>(expr)) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            expr = *(Mem<FnTypeExpr>(expr).modifier);
            break;
        case TYPE_MODIFIER_ARRAY:
            expr = *(Mem<ArrayTypeExpr>(expr).modifier);
            break;
        default:
            expr++;
            break;
        }
    }

    return last;
}
TypeExpr* GetNthType(TypeExpr* expr, u32 nth) {

    TypeExpr* ptrs[nth + 1]{};
    while (Mem<TypeName>(expr) != TYPE_NON) {

        for (i32 i = nth; i > 0; i--) {
            ptrs[i] = ptrs[i - 1];
        }
        ptrs[0] = expr;

        auto type = Mem<TypeName>(expr);
        switch (Mem<TypeName>(expr)) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            expr = *(Mem<FnTypeExpr>(expr).modifier);
            break;
        case TYPE_MODIFIER_ARRAY:
            expr = *(Mem<ArrayTypeExpr>(expr).modifier);
            break;
        default:
            expr++;
            break;
        }
    }

    ASSERT(Mem<TypeName>(ptrs[nth]) != TYPE_NON);
    return ptrs[nth];
}
u32 GetAllMembers(TypeExpr* e, TypeExpr** result) {

    if(e->index != TYPE_STRUCTURE) {
        result[0] = e;
        return 1;
    }
    auto st = (StructTypeExpr*)e;

    auto m = *(st->members);
    u32 ret = 0;
    for(u32 i = 0; i < st->memberCount; i++) {
        ret += GetAllMembers( *(m[i]), result + ret);
    }
    return ret;
}
bool TypesEqual(TypeExpr* e0, TypeExpr* e1) {

    auto t0 = Mem<TypeName>(e0);
    auto t1 = Mem<TypeName>(e1);
    if (t0 != t1)
        return false;

    while (e0->index != TYPE_NON && e1->index != TYPE_NON) {

        switch (Mem<TypeName>(e0)) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN:
            {
                auto& fn0 = Mem<FnTypeExpr>(e0);
                auto& fn1 = Mem<FnTypeExpr>(e1);

                TypeExpr* localMem[512];
                TypeExpr** m0 = localMem;
                u32 mCount0 = 0;
                u32 mCount1 = 0;
                for (u32 i = 0; i < fn0.param_count; i++) {
                    mCount0 += GetAllMembers(*fn0.params + i, m0 + mCount0);
                }
                TypeExpr** m1 = localMem + mCount0;
                for (u32 i = 0; i < fn1.param_count; i++) {
                    mCount1 += GetAllMembers(*fn1.params + i, m1 + mCount1);
                }
                if (mCount0 != mCount1) {
                    return false;
                }

                for (u32 i = 0; i < mCount0; i++) {
                    if (!TypesEqual( m0[i], m1[i] )) {
                        return false;
                    }
                }
                if (!TypesEqual(*fn0.ret_t, *fn1.ret_t))
                    return false;

                e0 = *fn0.modifier;
                e1 = *fn1.modifier;
            }
            break;
        case TYPE_MODIFIER_ARRAY:
            {
                auto& fn0 = Mem<ArrayTypeExpr>(e0);
                auto& fn1 = Mem<ArrayTypeExpr>(e1);
                if (*fn0.arraySizeExpr != *fn1.arraySizeExpr) {
                    return false;
                }
                e0 = *(Mem<ArrayTypeExpr>(e0).modifier);
                e1 = *(Mem<ArrayTypeExpr>(e1).modifier);
            }
            break;
        default:
            {
                TypeExpr* localMem[512];
                TypeExpr** m0 = localMem;
                u32 mCount0 = GetAllMembers(e0, m0);
                TypeExpr** m1 = localMem + mCount0;
                u32 mCount1 = GetAllMembers(e1, m1);

                if(mCount0 != mCount1) return false;
                for(u32 i = 0; i < mCount0; i++) {
                    if(!TypesEqual(m0[i], m1[i])) return false;
                }
                if(e0->index == TYPE_STRUCTURE) {
                    e0 = *(Mem<StructTypeExpr>(e0).modifier);
                }
                else {
                    e0++;
                }
                if(e1->index == TYPE_STRUCTURE) {
                    e1 = *(Mem<StructTypeExpr>(e1).modifier);
                }
                else {
                    e1++;
                }
            }
            break;
        }
    }

    return e0->index == e1->index;
}
ExprType GetLastExprType(byte* baseMem, Expr *expr) {
    return Mem<ExprType>(baseMem + expr->index);
}
byte* GetTypeExpr(Expr* expr, byte* dst) {

    switch(expr->index) {
    case EXPR_NULL:
        ASSERT(false);break;
    case EXPR_LITERAL:
        {
            auto l = Mem<LiteralExpr>(expr);
            Mem<TypeName>(dst) = (TypeName)l.v.type;
            Mem<TypeName>(dst + sizeof(TypeName)) = TYPE_NON;
            break;
        }
    case EXPR_UNARY:
        {
            auto u = Mem<UnaryExpr>(expr);
            return GetTypeExpr(*u.e, dst);
        }
    case EXPR_BINARY:
        {
            auto b = Mem<BinaryExpr>(expr);
            auto end = GetTypeExpr(*b.left, dst);
            end = GetTypeExpr(*b.left, end);

            auto left_t = GetLastType((Expr*)dst);
            auto right_t = GetLastType((Expr*)end);
            switch(b.opr) {
            case TOKEN_LSHIFT:
            case TOKEN_RSHIFT:
            case TOKEN_LSHIFT_EQUALS:
            case TOKEN_RSHIFT_EQUALS:
            case TOKEN_EXCLAMATION_MARK:
            case TOKEN_EQUALS_EQUALS:
            case TOKEN_EXCLAMATION_EQUALS:
            case TOKEN_AMPERSAND_AMPERSAND:
            case TOKEN_VERTICAL_BAR_VERTICAL_BAR:
                Mem<TypeName>(dst) = TYPE_PRIMARY_BOOL;
                Mem<TypeName>(dst + sizeof(TypeName)) = TYPE_NON;
                return dst + sizeof(TypeName) * 2;

            case TOKEN_TILDE:
            case TOKEN_AMPERSAND:
            case TOKEN_VERTICAL_BAR:
            case TOKEN_CIRCUMFLEX:
            case TOKEN_EQUAL_SIGN:
            case TOKEN_LSHIFT_LSHIFT:
            case TOKEN_RSHIFT_RSHIFT:
            case TOKEN_ASTERISK:
            case TOKEN_SLASH:
            case TOKEN_NEG:
                return end;
            case TOKEN_PLUS:
            case TOKEN_MINUS:
                if (left_t == TYPE_MODIFIER_POINTER || left_t == TYPE_MODIFIER_RESTRICTED_POINTER)
                    return end;
                else if (right_t == TYPE_MODIFIER_POINTER || right_t == TYPE_MODIFIER_RESTRICTED_POINTER)
                    return GetTypeExpr(*b.left, dst);
                else
                    return end;
            case TOKEN_SUB_SCRIPT_OPR:
                if (left_t == TYPE_MODIFIER_ARRAY || left_t == TYPE_MODIFIER_POINTER || left_t == TYPE_MODIFIER_RESTRICTED_POINTER)
                    return end;
                else if (right_t == TYPE_MODIFIER_POINTER || right_t == TYPE_MODIFIER_RESTRICTED_POINTER) {
                    return GetTypeExpr(*b.left, dst);
                }
            }
        }
    case EXPR_SYMBOL:
        return CpyTypeExpr(dst, *Mem<SymbolExpr>(expr).symbol.type);
    case EXPR_MEMORY_LOAD:
    case EXPR_MEMORY_STORE:
        {
            GetTypeExpr(*Mem<MemoryLoadExpr>(expr).address, dst);
            TypeExpr* nth = GetNthType((TypeExpr*)dst, 0);
            Mem<TypeName>(nth) = TYPE_NON;
            return (byte*)(nth + 1);
        }
    case EXPR_MEMBER:
        {
            GetTypeExpr(*Mem<MemberExpr>(expr).prev, dst);
            auto t = GetLastType((TypeExpr*)dst);
            ASSERT(false);
            break;
        }
    case EXPR_CALL:
        {
            GetTypeExpr(*Mem<CallExpr>(expr).callee, dst);
            auto fn = (FnTypeExpr*)dst;
            return CpyTypeExpr(dst, *fn->ret_t);
        }
    case EXPR_CONVERSION:
        return CpyTypeExpr(dst, *Mem<ConversionExpr>(expr).to);
    case EXPR_ADDR_OF:
        {
            auto end = GetTypeExpr(*Mem<AddrOfExpr>(expr).e, dst);
            Mem<TypeName>(end) = TYPE_MODIFIER_POINTER;
            Mem<TypeName>(end + sizeof(TypeName)) = TYPE_NON;
            return end + sizeof(TypeName);
        }
    case EXPR_PEEL_TYPE:
        {
            auto end = GetTypeExpr(*Mem<PeelTypeExpr>(expr).e, dst);
            auto last = GetNthType((Expr*)dst, 0);
            last->index = TYPE_NON;
            return (byte*)last + sizeof(TypeName);
        }
    }

}


Expr* ParsePrimary(ParserState* ctx, LinearAllocator* alloc) {

    TokenType tokens[] = {
        TOKEN_CHAR_LITERAL,
        TOKEN_STRING_LITERAL,
        TOKEN_NUMBER_LITERAL,
        TOKEN_BOOL_LITERAL,
        TOKEN_NULL_LITERAL,

        TOKEN_OPEN_PAREN,
        TOKEN_IDENTIFIER,
        TOKEN_ASTERISK,
        TOKEN_AMPERSAND,
    };

    auto expr = (Expr*)linear_allocator_top(alloc);
    u32 match = Match(GetCurrentToken(ctx), tokens, 9);
    switch(match) {
    case 0: case 1: case 2: case 3: case 4:
        { // literal
            auto node = (LiteralExpr*)linear_allocate(alloc, sizeof(LiteralExpr));
            node->index = EXPR_LITERAL;
            node->v = GetValueFromLiteral(NextToken(ctx));
            return node;
        }
    case 5: // (Expr) | (TypeExpr)
        if(IsTypeExpr(ctx)) {
            auto node = (ConversionExpr*)linear_allocate(alloc, sizeof(ConversionExpr));
            node->index = EXPR_CONVERSION;
            node->to = ParseTypeExpression(ctx, alloc);
            node->from = ParseAssignment(ctx, alloc);
            return node;
        }
        else {
            ctx->currentToken++;
            expr = ParseAssignment(ctx, alloc);
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
            return expr;
        }
    case 6: // symbol
        {
            auto node = (SymbolExpr*)linear_allocate(alloc, sizeof(SymbolExpr));
            node->index = EXPR_SYMBOL;
            auto name = NextToken(ctx);
            u32 index = FindSymbol(ctx->table, name);
            if(index == ~u32(0)) {
                global_print("%s%s*%s%i%\n", "ERROR: undefined symbol(", name.text, name.lenght, ") referenced in expression at: ", GetLineNumber(ctx->begin, name.text));
            }
            node->symbol.extra = ctx->table.symbols[index].extra;
            node->symbol.name = ctx->table.symbols[index].name;
            node->symbol.scope = ctx->table.symbols[index].scope;
            node->symbol.type = *ctx->table.symbols[index].type;
            return node;
        }
    case 7: // *Expr
        {
            ctx->currentToken++;
            auto node = (MemoryLoadExpr*)linear_allocate(alloc, sizeof(MemoryLoadExpr));
            node->index = EXPR_MEMORY_LOAD;
            node->address = ParseAssignment(ctx, alloc);
            return node;
        }
    case 8: // &Expr
        {
            ctx->currentToken++;
            auto node = (AddrOfExpr*)linear_allocate(alloc, sizeof(AddrOfExpr));
            node->index = EXPR_ADDR_OF;
            node->e = ParseAssignment(ctx, alloc);
            return node;
        }
    case ~u32(0):
        alloc->top -= sizeof(Expr);
        return nullptr;
    }
}
Expr* ParseMemory(ParserState* ctx, LinearAllocator* alloc) {

    TokenType tokens[] = {
        TOKEN_DOT,
        TOKEN_OPEN_BRACKET,
    };

    auto expr = ParsePrimary(ctx, alloc);
    u32 match = Match(GetCurrentToken(ctx), tokens, 2);

    while(match != ~u32(0)) {
        ctx->currentToken++;
        switch(match) {
        case 0:
            {
                ExpectToken(ctx, TOKEN_IDENTIFIER);
                auto node = (MemberExpr*)linear_allocate(alloc, sizeof(MemberExpr));
                node->index = EXPR_MEMBER;
                node->prev = expr;
                node->name = ctx->stream[ctx->currentToken-1];
                expr = (Expr*)node;

                auto top = (TypeExpr*)linear_allocator_top(alloc);
                GetTypeExpr(expr, (byte*)top);
                auto nth = GetNthType(top, 0);
                Mem<TypeName>(nth) = TYPE_NON;
                TypeName t = GetLastType(top);

                if(t != TYPE_MODIFIER_ARRAY && t < TYPE_COUNT) {
                    expr = (Expr*)linear_allocator_top(alloc);
                    auto memRef = (MemoryLoadExpr*)linear_allocate(alloc, sizeof(MemoryLoadExpr));
                    memRef->index = EXPR_MEMORY_LOAD;
                    memRef->address = expr;
                }
                else {
                    expr = (Expr*)linear_allocator_top(alloc);
                    auto peel = (PeelTypeExpr*)linear_allocate(alloc, sizeof(PeelTypeExpr));
                    peel->index = EXPR_PEEL_TYPE;
                    peel->e = expr;
                }
            }
            break;
        case 1:
            {
                auto top = (TypeExpr*)linear_allocator_top(alloc);
                GetTypeExpr(expr, (byte*)top);
                TypeName t = GetLastType(top);
                Expect(t == TYPE_MODIFIER_ARRAY || t == TYPE_MODIFIER_POINTER || t == TYPE_MODIFIER_RESTRICTED_POINTER, "operator ([]) expects pointer or array types");
                TypeExpr* peeled = GetNthType(top, 1);
                t = Mem<TypeName>(peeled);

                auto tmp = linear_allocator_top(alloc);
                if (t != TYPE_MODIFIER_ARRAY && t < TYPE_COUNT) {

                    auto memRef = (MemoryLoadExpr*)linear_allocate(alloc, sizeof(MemoryLoadExpr));
                    memRef->index = EXPR_MEMORY_LOAD;
                    memRef->address = (Expr*)linear_allocator_top(alloc);
                }
                else {
                    auto peel = (PeelTypeExpr*)linear_allocate(alloc, sizeof(PeelTypeExpr));
                    peel->index = EXPR_PEEL_TYPE;
                    peel->e = (Expr*)linear_allocator_top(alloc);
                }

                auto node = (BinaryExpr*)linear_allocate(alloc, sizeof(BinaryExpr));
                node->index = EXPR_BINARY;
                node->opr = TOKEN_SUB_SCRIPT_OPR;
                node->left = expr;
                node->right = ParseAssignment(ctx, alloc);
                ExpectToken(ctx, TOKEN_CLOSE_BRACKET);
                expr = (Expr*)tmp;
            }
            break;
        }
    }
    return expr;
}

Expr* ParseCall(ParserState* ctx, LinearAllocator* alloc) {

    Expr* callee = ParseMemory(ctx, alloc);
    while(GetCurrentToken(ctx).type == TOKEN_OPEN_PAREN) {
        ctx->currentToken++;
        Expr* args[255];
        u32 i = 0;

        GetTypeExpr(callee, (byte*)linear_allocator_top(alloc));
        FnTypeExpr *fn = (FnTypeExpr*)linear_allocator_top(alloc);
        ASSERT(fn->index == TYPE_PRIMARY_FN);// or fn*
        TypeName retT = GetLastType((TypeExpr*)linear_allocator_top(alloc));
        if (retT == TYPE_MODIFIER_ARRAY || retT > TYPE_COUNT) {
            auto addrOf = (AddrOfExpr*)linear_allocate(alloc, sizeof(AddrOfExpr));
            addrOf->index = EXPR_ADDR_OF;
            addrOf->e.index = ~u32(0);
            args[i++] = addrOf;
        }

        // max args count: 255
        if(GetCurrentToken(ctx).type != TOKEN_CLOSE_PAREN) {
            do {
                args[i++] = ParseAssignment(ctx, alloc);
            } while(GetCurrentToken(ctx).type == TOKEN_COMMA);
        }
        ExpectToken(ctx, TOKEN_CLOSE_PAREN);
        Expect(i < 256, "ERROR: max argument count is 255" );

        auto call = (CallExpr*)linear_allocate(alloc, sizeof(CallExpr) + i * sizeof(Expr));
        call->index = EXPR_CALL;
        call->callee;
        call->argCount = i;
        for(u32 k = 0; k < i; k++) {
            call->args[k] = args[k];
        }
        callee = call;
    }

    return callee;
}

Expr* ParseUnary(ParserState* ctx, LinearAllocator* alloc) {
    
    TokenType unaries[4] = {
        TOKEN_EXCLAMATION_MARK,
        TOKEN_MINUS,
        TOKEN_PLUS_PLUS,
        TOKEN_MINUS_MINUS
    };

    Expr* ret = nullptr;
    auto match = Match(GetCurrentToken(ctx), unaries, 4);
    while (match != ~u32(0)) {
        ctx->currentToken++;

        auto *node = (UnaryExpr*)linear_allocate(alloc, sizeof(UnaryExpr));
        node->index = EXPR_UNARY;
        node->opr = unaries[match];
        if(node->opr == TOKEN_MINUS) {
            node->opr = TOKEN_NEG;
        }
        node->e = ParseCall(ctx, alloc);
        match = Match(GetCurrentToken(ctx), unaries, 2);
        ret = node;
    }

    if(!ret) {
        ret = ParseCall(ctx, alloc);
    }
    return ret;
}
Expr* ParseFactor(ParserState* ctx, LinearAllocator* alloc) {

    Expr* expr = ParseUnary(ctx, alloc);

    TokenType binaries[2] = {TOKEN_ASTERISK, TOKEN_SLASH};
    u32 match = Match(GetCurrentToken(ctx), binaries, 2);
    while(match != ~u32(0)) {
        ctx->currentToken++;

        auto node = (BinaryExpr*)linear_allocate(alloc, sizeof(BinaryExpr));
        node->index = EXPR_BINARY;
        node->opr = binaries[match];
        node->left = expr;
        node->right = ParseUnary(ctx, alloc);

        expr = node;
    }

    return expr;
}
Expr* ParseTerm(ParserState* ctx, LinearAllocator* alloc) {

    Expr* expr = ParseFactor(ctx, alloc);

    TokenType binaries[9] = {
        TOKEN_PLUS,
        TOKEN_MINUS,
        TOKEN_AMPERSAND_AMPERSAND,
        TOKEN_VERTICAL_BAR_VERTICAL_BAR,
        TOKEN_CIRCUMFLEX,
        TOKEN_AMPERSAND,
        TOKEN_VERTICAL_BAR,
        TOKEN_LSHIFT_LSHIFT,
        TOKEN_RSHIFT_RSHIFT
    };
    u32 match = Match(GetCurrentToken(ctx), binaries, 9);
    while(match != ~u32(0)) {
        ctx->currentToken++;

        auto node = (BinaryExpr*)linear_allocate(alloc, sizeof(BinaryExpr));
        node->index = EXPR_BINARY;
        node->opr = binaries[match];
        node->left = expr;
        node->right = ParseFactor(ctx, alloc);

        expr = node;
    }

    return expr;
}
Expr* ParseComparision(ParserState* ctx, LinearAllocator* alloc) {

    Expr* expr = ParseTerm(ctx, alloc);

    TokenType binaries[6] = {
        TOKEN_RSHIFT,
        TOKEN_LSHIFT,
        TOKEN_RSHIFT_EQUALS,
        TOKEN_LSHIFT_EQUALS,
        TOKEN_EXCLAMATION_EQUALS,
        TOKEN_EQUALS_EQUALS
    };
    u32 match = Match(GetCurrentToken(ctx), binaries, 6);
    while(match != ~u32(0)) {
        ctx->currentToken++;

        auto node = (BinaryExpr*)linear_allocate(alloc, sizeof(BinaryExpr));
        node->index = EXPR_BINARY;
        node->opr = binaries[match];
        node->left = expr;
        node->right = ParseTerm(ctx, alloc);

        expr = node;
    }

    return expr;
}
Expr* ParseAssignment(ParserState* ctx, LinearAllocator* alloc) {

    Expr* lval = ParseComparision(ctx, alloc);
    
    bool match = (GetCurrentToken(ctx).type == TOKEN_EQUAL_SIGN);
    while(match) {
        ctx->currentToken++;
        Expr* rval = ParseAssignment(ctx, alloc);

        if(rval->index == EXPR_CALL) {
            GetTypeExpr(lval, (byte*)linear_allocator_top(alloc));
            auto lvalType = GetLastType((TypeExpr*)linear_allocator_top(alloc));
            if (lvalType == TYPE_MODIFIER_ARRAY || lvalType > TYPE_COUNT) {
                auto call = (CallExpr*)rval;
                ASSERT((*call->args[0])->index == EXPR_ADDR_OF);
                ((AddrOfExpr*)*call->args[0])->e = lval;
                lval = rval;
            }
        }
        else {
            switch(lval->index) {
            case EXPR_MEMORY_LOAD:
                {
                    auto load = (MemoryLoadExpr*)lval;
                    auto store = (MemoryStoreExpr*)linear_allocate(alloc, sizeof(MemoryStoreExpr));
                    store->address = *(load->address);
                    store->value = rval;
                    lval = store;
                }
                break;
            case EXPR_SYMBOL:
                {
                    auto assignment = (BinaryExpr*)linear_allocate(alloc, sizeof(BinaryExpr));
                    assignment->index = EXPR_BINARY;
                    assignment->opr = TOKEN_EQUAL_SIGN;
                    assignment->left = lval;
                    assignment->right = rval;
                    lval = assignment;
                }
                break;
            }
        }

        match = (GetCurrentToken(ctx).type == TOKEN_EQUAL_SIGN);
    }

    return lval;
}

enum StatementType {
    STATEMENT_NON,

    STATEMENT_FN_BODY,

    STATEMENT_EXPRESSION,
    STATEMENT_PRINT,

    STATEMENT_BRANCH,
    STATEMENT_FOR_LOOP,

    STATEMENT_RETURN,
    STATEMENT_BREAK,
    STATEMENT_NEXT,

    STATEMENT_COUNT,
};

typedef Expr Stmt;
struct StmtReturn : Stmt {
    Expr expr;
};
struct StmtFnBody : Stmt {
    u32 symbolIndex;
    u32 argCount;
    Token* args;
};
struct StmtExpr : Stmt {
    Expr expr;
};
struct StmtPrint : Stmt {
    u32 exprCount;
    Expr exprString[0];// inline buffer
};
struct StmtBranch : Stmt {
    Expr cond;
    Stmt thenBranch;
    Stmt elseBranch;
    Stmt end;
};
struct StmtFor : Stmt {
    Expr cond;
    Stmt init;
    Stmt inc;
    Stmt body;
    Stmt end;
};
Symbol* MakeSymbolTableEntry(SymbolTable* table) {
    return table->symbols + (table->symbolCount++);
}
void PurgeSymbols(u32 scope, SymbolTable table) {
    for(u32 i = 0; i < table.symbolCount; i++) {
        if(scope < table.symbols[i].scope) {
            auto q = table.symbols[--table.symbolCount];
            table.symbols[i].type = *q.type;
            table.symbols[i].extra = q.extra;
            table.symbols[i].name = q.name;
            table.symbols[i].scope = q.scope;
            i--;
        }
    }
}
void PrintTypeExpression(TypeExpr* expr);

Stmt* ParseStatement(ParserState* ctx, LinearAllocator* exprAlloc, LinearAllocator* stmtAlloc, u32 scope) {

    TokenType tokens[] = {
        TOKEN_EOF,
        TOKEN_KEYWORD_MAIN,
        TOKEN_KEYWORD_FN,
        TOKEN_KEYWORD_STRUCT,
        TOKEN_OPEN_BRACES,
        TOKEN_CLOSE_BRACES,
        TOKEN_KEYWORD_RETURN,
        TOKEN_KEYWORD_IF,
        TOKEN_KEYWORD_FOR,
        TOKEN_KEYWORD_DO,
        TOKEN_KEYWORD_ASSUME,
        TOKEN_KEYWORD_PRINT,
        TOKEN_IDENTIFIER,
    };
    fn_entry:
    u32 match = Match(NextToken(ctx),  tokens, 13);
    switch(match) {
    case 0:// eof
        {
            auto sentinel = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
            sentinel->index = STATEMENT_NON;
            return nullptr;
        }
    case 1:// main
        {
            auto fnBody = (StmtFnBody*)linear_allocate(stmtAlloc, sizeof(StmtFnBody));
            fnBody->index = STATEMENT_FN_BODY;
            fnBody->symbolIndex = ctx->table.symbolCount;

            auto symbol = MakeSymbolTableEntry(&ctx->table);
            symbol->name = GetPrevToken(ctx);
            symbol->scope = scope;
            Mem<Expr>(&symbol->extra) = (Expr*)ParseStatement(ctx, exprAlloc, stmtAlloc, scope);
            auto sentinel = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
            sentinel->index = STATEMENT_NON;
            return fnBody;
        }
    case 2: // fn q()
        {
            auto fnBody = (StmtFnBody*)linear_allocate(stmtAlloc, sizeof(StmtFnBody));
            fnBody->index = STATEMENT_FN_BODY;
            fnBody->symbolIndex = ctx->table.symbolCount;

            auto symbol = MakeSymbolTableEntry(&ctx->table);
            symbol->name = NextToken(ctx);
            symbol->scope = scope;
            ExpectToken(ctx, TOKEN_OPEN_PAREN);

            auto fn_t = (FnTypeExpr*)linear_allocate(exprAlloc, sizeof(FnTypeExpr));
            symbol->type = fn_t;
            fn_t->index = TYPE_PRIMARY_FN;
            
            TypeExpr argTypes[255];
            u32 argNames[255];
            u32 i = 0;

            if(GetCurrentToken(ctx).type != TOKEN_CLOSE_PAREN) {
                argTypes[0] = ParseTypeExpression(ctx, exprAlloc);
                argNames[0] = ctx->currentToken;
                i = 1;
                while(GetCurrentToken(ctx).type != TOKEN_CLOSE_PAREN) {
                    ExpectToken(ctx, TOKEN_COMMA);
                    argTypes[i] = ParseTypeExpression(ctx, exprAlloc);
                    argNames[i++] = ctx->currentToken;
                }
            }
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
            fn_t->param_count = i;
            fn_t->params = (TypeExpr*)linear_allocate(exprAlloc, sizeof(TypeExpr) * i);
            memcpy(*fn_t->params, argTypes, sizeof(TypeExpr) * i);
            fnBody->argCount = i;
            fnBody->args = (Token*)linear_allocate(exprAlloc, sizeof(Token) * i);
            for(u32 k = 0; k < i; k++) {
                fnBody->args[k] = ctx->stream[argNames[k]];
            }

            if(GetCurrentToken(ctx).type == TOKEN_KEYWORD_ARROW) {
                ctx->currentToken++;
                fn_t->ret_t = ParseTypeExpression(ctx, exprAlloc);
            }
            else {
                fn_t->ret_t.index = exprAlloc->top;
                auto void_t = (TypeName*)linear_allocate(exprAlloc, sizeof(TypeName) * 2);
                void_t[0] = TYPE_PRIMARY_VOID;
                void_t[1] = TYPE_NON;
            }

            Mem<Stmt>(&symbol->extra) = (Stmt*)ParseStatement(ctx, exprAlloc, stmtAlloc, scope);
            auto sentinel = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
            sentinel->index = STATEMENT_NON;

            return fnBody;
        }
    case 3: // struct
        {
            auto st = ctx->table.symbols + (ctx->table.symbolCount++);
            st->name = NextToken(ctx);

            TypeExpr* m[255];
            u32 names[255];
            u32 c = 0;
            if(GetCurrentToken(ctx).type != TOKEN_CLOSE_BRACES) {
                ctx->currentToken += (GetCurrentToken(ctx).type == TOKEN_OPEN_BRACES);
                m[0] = ParseTypeExpression(ctx, exprAlloc);
                names[0] = ctx->currentToken++;
                c = 1;
                while(GetCurrentToken(ctx).type != TOKEN_CLOSE_BRACES) {
                    ExpectToken(ctx, TOKEN_COMMA);
                    ASSERT(c < 255);
                    m[c] = ParseTypeExpression(ctx, exprAlloc);
                    names[c++] = ctx->currentToken++;
                }
                ctx->currentToken += GetCurrentToken(ctx).type == TOKEN_COMMA;
            }
            ExpectToken(ctx, TOKEN_CLOSE_BRACES);
            st->extra = linear_allocate(exprAlloc, c * sizeof(Token));
            Mem<u32>(st->extra) = c;
            auto st_t = (StructTypeExpr*)linear_allocate(exprAlloc, sizeof(StructTypeExpr) + c * sizeof(TypeExpr));
            st_t->index = TYPE_STRUCTURE;
            st_t->memberCount = c;
            st_t->members = (st_t + 1);

            auto names = (byte*)st->extra + sizeof(u32);
            for(u32 i = 0; i < c; i++) {
                ((Token*)names)[i] = ctx->stream[names[i]];
                (*st_t->members)[i] = m[i];
            }
            st_t->modifier = (TypeExpr*)linear_allocate(exprAlloc, sizeof(TypeName));
            (*st_t->modifier)->index = TYPE_NON;
            st->type = st_t;;

            goto fn_entry;
        }
    case 4: // {
        {
            auto ret = ParseStatement(ctx, exprAlloc, stmtAlloc, scope+1);
            if(ret) {
                while(ParseStatement(ctx, exprAlloc, stmtAlloc, scope+1));
                PurgeSymbols(scope, ctx->table);
            }
            ExpectToken(ctx, TOKEN_CLOSE_BRACES);
            return ret;
        }
    case 5: // }
        {
            ctx->currentToken--;
            return nullptr;
        }
    case 6: // return
        {
            auto stmtRet = (StmtReturn*)linear_allocate(stmtAlloc, sizeof(StmtReturn));
            stmtRet->index = STATEMENT_RETURN;
            stmtRet->expr = ParseAssignment(ctx, exprAlloc);
            ExpectToken(ctx, TOKEN_SEMICOLON);
            return stmtRet;
        }
    case 7: // if
        {
            ExpectToken(ctx, TOKEN_OPEN_PAREN);
            auto branch = (StmtBranch*)linear_allocate(stmtAlloc, sizeof(StmtBranch));
            branch->index = STATEMENT_BRANCH;
            branch->cond = ParseAssignment(ctx, exprAlloc);
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
            branch->thenBranch = ParseStatement(ctx, exprAlloc, stmtAlloc, scope);
            auto sentinel0 = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
            sentinel0->index = STATEMENT_NON;

            if(GetCurrentToken(ctx).type == TOKEN_KEYWORD_ELSE) {
                branch->elseBranch = ParseStatement(ctx, exprAlloc, stmtAlloc, scope);
                auto sentinel1 = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
                sentinel1->index = STATEMENT_NON;
            }
            branch->end = (Stmt*)linear_allocator_top(stmtAlloc);
            return branch;
        }
    case 8: // for
        {
            ExpectToken(ctx, TOKEN_OPEN_PAREN);
            auto loop = (StmtFor*)linear_allocate(exprAlloc, sizeof(StmtFor));
            loop->index = STATEMENT_FOR_LOOP;
            loop->init = ParseStatement(ctx, exprAlloc, stmtAlloc, scope+1);
            ((Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt)))->index = STATEMENT_NON;

            loop->cond = ParseAssignment(ctx, exprAlloc);
            loop->inc = ParseStatement(ctx, exprAlloc, stmtAlloc, scope+1);
            ((Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt)))->index = STATEMENT_NON;
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);

            loop->body = ParseStatement(ctx, exprAlloc, stmtAlloc, scope+1);
            ((Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt)))->index = STATEMENT_NON;
            PurgeSymbols(scope, ctx->table);

            loop->end = (Stmt*)linear_allocator_top(stmtAlloc);

            return loop;
        }
    case 9: // do
        {
            auto loop = (StmtFor*)linear_allocate(exprAlloc, sizeof(StmtFor));
            loop->index = STATEMENT_FOR_LOOP;
            loop->init = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
            (*loop->init)->index = STATEMENT_NON;

            auto litExpr = (LiteralExpr*)linear_allocate(exprAlloc, sizeof(LiteralExpr));
            loop->cond = litExpr;
            litExpr->index = EXPR_LITERAL;
            Mem<bool>(litExpr->v.mem) = true;
            litExpr->v.type = TYPE_PRIMARY_BOOL;

            loop->inc = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
            (*loop->inc)->index = STATEMENT_NON;

            loop->body = ParseStatement(ctx, exprAlloc, stmtAlloc, scope+1);
            ((Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt)))->index = STATEMENT_NON;

            return loop;
        }
    case 10: // assume
        {
            return nullptr;
        }
    case 11: // print
        {
            auto print = (StmtPrint*)linear_allocate(stmtAlloc, sizeof(StmtPrint));
            print->index = STATEMENT_PRINT;

            u32 i = 0;
            while(GetCurrentToken(ctx).type != TOKEN_SEMICOLON) {
                print->exprString[i++] = ParseAssignment(ctx, exprAlloc);
            }
            ExpectToken(ctx, TOKEN_SEMICOLON);
            print->exprCount = i;
            stmtAlloc->top += i * sizeof(Expr);
            return print;
        }
    case 12: // identifier
    case ~u32(0):
        {
            ctx->currentToken--;
            if(IsTypeExpr(ctx)) {
                auto save = ctx->currentToken;
                auto saveTop = exprAlloc->top;
                auto type = ParseTypeExpression(ctx, exprAlloc);
                PrintTypeExpression(type);
                global_io_flush();
                auto name = GetCurrentToken(ctx);
                if(name.type != TOKEN_IDENTIFIER || FindSymbol(ctx->table, name) != ~u32(0)) {
                    ctx->currentToken = save;
                    exprAlloc->top = saveTop;
                }
                else {
                    auto symbol = MakeSymbolTableEntry(&ctx->table);
                    symbol->type = type;
                    symbol->name = name;
                    symbol->scope = scope;
                    symbol->extra = 0;
                }
            }

            auto exprStmt = (StmtExpr*)linear_allocate(stmtAlloc, sizeof(StmtExpr));
            exprStmt->index = STATEMENT_EXPRESSION;
            exprStmt->expr = ParseAssignment(ctx, exprAlloc);
            ExpectToken(ctx, TOKEN_SEMICOLON);
            return exprStmt;
        }
    case 13: // ;
        {
            auto exprStmt = (StmtExpr*)linear_allocate(stmtAlloc, sizeof(StmtExpr));
            exprStmt->index = STATEMENT_EXPRESSION;
            exprStmt->expr.index = 0;
            return exprStmt;
        }
    }
}


u32 Tokenize(Tokenizer* tokenizer, Token* result) {

    u32 count = 0;
    for(auto t = GetToken(tokenizer); t.type != TOKEN_EOF; t = GetToken(tokenizer)) {
        result[count++] = t;
    }
    return count;
}

Symbol FindMain(SymbolTable table) {
    for(u32 i = 0; i < table.symbolCount; i++) {
        if(TokenEquals(table.symbols[i].name, "main")) return table.symbols[i];
    }
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

void PrintExpr(Expr* expr);
void PrintTypeModifierExpression(TypeExpr* modifier) {

    while(modifier->index != TYPE_NON) {

        switch(modifier->index) {
        case TYPE_MODIFIER_RESTRICTED_POINTER:
            global_print("%s", "*restrict");
            modifier++;
            break;
        case TYPE_MODIFIER_POINTER:
            global_print("%c", '*');
            modifier++;
            break;
        case TYPE_MODIFIER_CONST:
            global_print("%s", " const");
            modifier++;
            break;
        case TYPE_MODIFIER_ATOMIC:
            global_print("%s", " atomic");
            modifier++;
            break;
        case TYPE_MODIFIER_VOLATILE:
            global_print("%s", " volatile");
            modifier++;
            break;
        case TYPE_MODIFIER_ARRAY:
            global_print("%c", '[');
            PrintExpr(*Mem<ArrayTypeExpr>(modifier).arraySizeExpr);
            global_print("%c", ']');
            modifier = *Mem<ArrayTypeExpr>(modifier).modifier;
            break;
        default:
            global_print("%s%\n", "unkonw type modifier");
            ASSERT(false);
        }
    }
}
void PrintTypeExpression(TypeExpr* expr) {

    auto n = (TypeName)expr->index;
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
    case TYPE_PRIMARY_FN:
        {
            auto& fn = Mem<FnTypeExpr>(expr);
            if (fn.modifier.index != TYPE_NON) {
                global_print("%s", "(fn(");
                for (u32 i = 0; i < fn.param_count; i++) {
                    PrintTypeExpression( *((*fn.params)[i]) );
                    if (i != fn.param_count - 1) {
                        global_print("%s", ", ");
                    }
                }

                global_print("%s", ") ->  ");
                PrintTypeExpression(*fn.ret_t);
                global_print("%c", ')');
                PrintTypeModifierExpression(*fn.modifier);
            }
            else {
                global_print("%s", "fn(");
                for (u32 i = 0; i < fn.param_count; i++) {
                    PrintTypeExpression( *((*fn.params)[i]) );
                    if (i != fn.param_count - 1) {
                        global_print("%s", ", ");
                    }
                }

                global_print("%s", ") -> ");
                PrintTypeExpression(*fn.ret_t);
            }
            return;
        }
        break;
    case TYPE_STRUCTURE:
        {
            auto& st = Mem<StructTypeExpr>(expr);
            global_print("%c", '(');
            if(st.memberCount) {
                auto members = *st.members;
                PrintTypeExpression( *(members[0]) );
                for(u32 i = 1; i < st.memberCount; i++) {
                    global_print("%s", ", ");
                    PrintTypeExpression( *(members[i]) );
                }
            }
            global_print("%c", ')');
            PrintTypeModifierExpression(*st.modifier);
            return;
        }
    default:ASSERT(false);break;
    }

    expr++;
    PrintTypeModifierExpression(expr);
}
void PrintExpr(Expr* expr) {

    switch(expr->index) {
    case EXPR_NULL:
        ASSERT(false);break;
    case EXPR_LITERAL:
        PrintValue(Mem<LiteralExpr>(expr).v);
        break;
    case EXPR_UNARY:
        global_print("%i", Mem<UnaryExpr>(expr).opr);
        PrintExpr(*Mem<UnaryExpr>(expr).e);
        break;
    case EXPR_BINARY:
        PrintExpr(*Mem<BinaryExpr>(expr).left);
        global_print("%c%i%c", ' ', Mem<BinaryExpr>(expr).opr, ' ');
        PrintExpr(*Mem<BinaryExpr>(expr).right);
        break;
    case EXPR_SYMBOL:
        {
            auto t = Mem<SymbolExpr>(expr).symbol.name;
            global_print("%s*", t.text, t.lenght);
            break;
        }
    case EXPR_MEMORY_LOAD:
        global_print("%c", '*');
        PrintExpr(*Mem<MemoryLoadExpr>(expr).address);
        break;
    case EXPR_MEMORY_STORE:
        global_print("%c", '*');
        PrintExpr(*Mem<MemoryStoreExpr>(expr).address);
        global_print("%s", " = ");
        PrintExpr(*Mem<MemoryStoreExpr>(expr).value);
        break;
    case EXPR_MEMBER:
        {
            PrintExpr(*Mem<MemberExpr>(expr).prev);
            auto t = Mem<MemberExpr>(expr).name;
            global_print("%c%s*", '.', t.text, t.lenght);
        }
        break;
    case EXPR_CALL:
        {
            PrintExpr(*Mem<CallExpr>(expr).callee);
            auto call = Mem<CallExpr>(expr);
            global_print("%c", '(');
            for(u32 i = 0; i < call.argCount; i++) {
                PrintExpr(*call.args[i]);
                global_print("%c", ' ');
            }
            global_print("%c", ')');
            break;
        }
    case EXPR_CONVERSION:
        {
            global_print("%c", '(');
            PrintTypeExpression(*Mem<ConversionExpr>(expr).to);
            global_print("%c", ')');
            global_print("%c", '(');
            PrintExpr(*Mem<ConversionExpr>(expr).from);
            global_print("%c", ')');
            break;
        }
    case EXPR_ADDR_OF:
        global_print("%c", '&');
        PrintExpr(*Mem<AddrOfExpr>(expr).e);
        break;
    case EXPR_PEEL_TYPE:
        PrintExpr(*Mem<PeelTypeExpr>(expr).e);
        break;
    }
}

Stmt* PrintStmt(Stmt* stmt, SymbolTable table) {

    switch(stmt->index) {
    case STATEMENT_NON:
        return nullptr;
    case STATEMENT_FN_BODY:
        {
            auto& fn = Mem<StmtFnBody>(stmt);
            PrintToken(table.symbols[fn.symbolIndex].name);
            auto& q = table.symbols[fn.symbolIndex];
            ASSERT((*table.symbols[fn.symbolIndex].type)->index == TYPE_PRIMARY_FN);
            auto fn_t = (FnTypeExpr*)*table.symbols[fn.symbolIndex].type;
            global_print("%c", '(');
            for(u32 i = 0; i < fn.argCount; i++) {
                PrintTypeExpression(*((*fn_t->params)[i]));
                global_print("%c%s*%c", ' ', fn.args[i].text, fn.args[i].lenght, ' ');
            }
            global_print("%s", ")\n\t");
            stmt = *Mem<Stmt>(&q.extra);
            while(stmt = PrintStmt(stmt, table)) {
                global_print("%c", '\t');
            }
            return nullptr;
        }
        break;
    case STATEMENT_EXPRESSION:
        {
            auto& stmtExpr = Mem<StmtExpr>(stmt);
            if(stmtExpr.expr.index) {
                PrintExpr(*stmtExpr.expr);
                global_print("%s", ";\n");
            }
            return &stmtExpr + 1;
        }
    case STATEMENT_PRINT:
        {
            auto& print = Mem<StmtPrint>(stmt);
            global_print("%s", "print ");
            for(u32 i = 0; i < print.exprCount; i++) {
                PrintExpr(*print.exprString[i]);
                global_print("%c", ' ');
            }
            global_print("%c", '\n');
            return &print + 1;
        }
    case STATEMENT_BRANCH:
        {
            auto& branch = Mem<StmtBranch>(stmt);
            global_print("%s", "if(");
            PrintExpr(*branch.cond);
            global_print("%s", ")\n");
            while(stmt = PrintStmt(*branch.thenBranch, table)) {
                global_print("%c", "\t");
            }
            if(branch.elseBranch.index) {
                global_print("%s", "else\n");
                while(stmt = PrintStmt(*branch.elseBranch, table)) {
                    global_print("%c", "\t");
                }
            }
            return *branch.end;
        }
    case STATEMENT_FOR_LOOP:
        {
            auto& loop = Mem<StmtFor>(stmt);
            global_print("%s", "for(");
            while(stmt = PrintStmt(*loop.init, table));
            global_print("%c", ";");
            PrintExpr(*loop.cond);
            global_print("%c", ";");
            while(stmt = PrintStmt(*loop.inc, table));

            global_print("%s", ")\n");
            while(stmt = PrintStmt(*loop.body, table));
            return *loop.end;
        }
    case STATEMENT_RETURN:
        {
            auto& ret = Mem<StmtReturn>(stmt);
            global_print("%s", "return ");
            if(ret.expr.index) {
                PrintExpr(*ret.expr);
            }
            global_print("%s", ";\n");
            return &ret + 1;
        }
    case STATEMENT_BREAK:
        global_print("%s", "break;");
        return stmt + 1;
    case STATEMENT_NEXT:
        global_print("%s", "next;");
        return stmt + 1;
    }
}

void VerifyTypes(SymbolTable table, Expr* e, byte* mem) {

    switch(e->index) {
    case EXPR_NULL:ASSERT(false);
    case EXPR_LITERAL:
        return;
    case EXPR_UNARY:

    case EXPR_BINARY:
    case EXPR_SYMBOL:
    case EXPR_MEMORY_LOAD:
    case EXPR_MEMORY_STORE:
    case EXPR_MEMBER:
    case EXPR_CALL:
    case EXPR_CONVERSION:
    case EXPR_ADDR_OF:
    case EXPR_PEEL_TYPE:
    }
}

i32 main(i32 argc, const char** args) {

    u32 size;
    auto compilerMemory = init_global_state(Megabyte(16), Megabyte(64), 512);

    auto text = (char*)ReadFileTerminated("test_parser", compilerMemory, &size);
    auto Tokenizer = MakeTokenizer(text);

    auto rem = Megabyte(48) - size;
    auto linExpr = make_linear_allocator(compilerMemory + size, rem/2);
    auto linStmt = make_linear_allocator(compilerMemory + size + (rem/2), rem/2);
    auto end = compilerMemory + size + rem;

    ParserState ctx;
    ctx.begin = text;
    ctx.currentToken = 0;
    ctx.stream = (Token*)end;
    u32 tokenCount = Tokenize(&Tokenizer, ctx.stream);

    ctx.table.symbolCount = 0;
    ctx.table.symbols = (Symbol*)(end + tokenCount * sizeof(Token));

    u32 it = -1;
    Stmt* program[150];
    while(program[++it] = ParseStatement(&ctx, &linExpr, &linStmt, 0));

    for(u32 i = 0; i < it; i++) {
        PrintStmt(program[i], ctx.table);
    }

    global_print("%c", '\n');
    global_io_flush();
}