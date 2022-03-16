#include "./common.h"
#include <typeinfo>
#include <iostream>

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
    const char *name;
};

constexpr TokenKeyWord keyWords[] = {
    {TOKEN_KEYWORD_PRINT, "print"},
    {TOKEN_KEYWORD_IF, "if"},
    {TOKEN_KEYWORD_ELSE, "else"},
    {TOKEN_KEYWORD_FOR, "for"},
    {TOKEN_KEYWORD_RETURN, "return"},
    {TOKEN_KEYWORD_STRUCT, "struct"},
    {TOKEN_KEYWORD_AUTO, "auto"},
    {TOKEN_KEYWORD_FN, "fn"},
    {TOKEN_KEYWORD_ARROW, "->"},
    {TOKEN_KEYWORD_ASSUME, "assume"},
    {TOKEN_KEYWORD_DO, "do"},
    {TOKEN_KEYWORD_COLD, "cold"},
    {TOKEN_KEYWORD_HOT, "hot"},
    {TOKEN_KEYWORD_RESTRICT, "restrict"},
    {TOKEN_KEYWORD_VOLATILE, "volatile"},
    {TOKEN_KEYWORD_ATOMIC, "atomic"},
    {TOKEN_KEYWORD_CONST, "const"},
    {TOKEN_KEYWORD_MAIN, "main"},
};

struct Token {
    const char *text;
    u32 lenght;
    TokenType type;
};

struct Tokenizer {
    char *at;
    u32 line = 1;
};
Tokenizer MakeTokenizer(const char* source) {
    return {(char*)source, 1};
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
    return (c == ' ')   ||
           (c == '\n')  ||
           (c == '\t')  ||
           (c == '\r');
}

u32 GetLineNumber(const char *source, const char *at) {
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
bool TokenEquals(Token t, const char *match) {

    auto ptr = t.text;
    auto end = t.text + t.lenght;
    for (;;) {
        if (!*match)
            return ptr == end;
        if (ptr == end)
            return *match == 0;
        if (*ptr++ != *match++)
            return false;
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
    for (; ptr != end;) {
        if (*ptr++ != *t1.text++)
            return false;
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
            for (auto i = keyWords; i < 1 [&keyWords]; i++) {
                if (TokenEquals(token, i->name)) {
                    token.type = i->type;
                }
            }
            if (TokenEquals(token, "true") || TokenEquals(token, "false")) {
                token.type = TOKEN_BOOL_LITERAL;
            } else if (TokenEquals(token, "null")) {
                token.type = TOKEN_NULL_LITERAL;
            }
        } else {
            token.type = TOKEN_UNKNOWN;
            global_print("%s%i%\n", "ERROR: unknown token at line: ", tokenizer->line);
        }
        break;
    }

    return token;
}

// --------------------------------------- Lexer END --------------------------------


// ----------------------------------------Parse BEGIN -----------------------------
struct Expr {
    i32 index;
    void operator=(Expr*addr) {
        index = (addr == nullptr ? 0 : (i32)((byte*)addr - (byte*)this));
    }
    void operator=(Expr e) = delete;
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
    u32 param_count;
    TypeExpr params;
    TypeExpr ret_t;
    TypeExpr modifier;
};
struct ArrayTypeExpr : TypeExpr {
    Expr arraySizeExpr;
    TypeExpr modifier;
};
struct StructMemberTypeExpr : TypeExpr {
    Token name;
    TypeExpr type;
};
struct StructTypeExpr : TypeExpr {
    u32 memberCount;
    TypeExpr members;
    TypeExpr modifier;
};
struct StructSymbol {
    Token name;
    TypeExpr *ptr;
    Token *members;
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
    void *extra;
    TypeExpr* type;
    Token name;
    u32 scope;
};
struct TypeSymbol {
    void* extra;
    TypeExpr* type;
    Token name;
};
void CopySymbol(Symbol* dst, Symbol* src) {
    dst->extra = src->extra;
    dst->name = src->name;
    dst->scope = src->scope;
    dst->type = src->type;
}

struct LiteralExpr : Expr {
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
    u32 symbolIndex;
    Token debug_name;
    bool global;
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
    const char *name;
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

Value EvalExpr(Expr* e) {}

typedef StaticBufferLocal<TypeSymbol, LinearAllocator,linear_allocate,roll_back_linear_allocator> TypeSymbolTable;
typedef StaticBufferLocal<Symbol, LinearAllocator,linear_allocate,roll_back_linear_allocator> SymbolTable;
struct ParserState {
    const char *begin;
    Token *stream;
    TypeSymbolTable typeTable;
    SymbolTable table;
    
    u32 tokenCount;
    u32 globalCount;
    u32 currentToken;
    u32 scope;
};

u32 FindSymbol(SymbolTable table, Token name) {
    for (u32 i = 0; i < table.size; i++) {
        if (TokensEquals(name, table[i].name)) {
            return i;
        }
    }
    return ~u32(0);
}

Token GetPrevToken(ParserState *ctx) {
    ASSERT(ctx->currentToken);
    return ctx->stream[ctx->currentToken - 1];
}
Token GetCurrentToken(ParserState *ctx) {
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
        if(literal.text[0] == '\\' && literal.text[1] == 'n') {
            Mem<char>(ret.mem) = '\n';
        }
        else {
            Mem<char>(ret.mem) = literal.text[0];
        }
        return ret;
    case TYPE_MODIFIER_RESTRICTED_POINTER:
    case TYPE_MODIFIER_POINTER:
        ret.type = t;
        Mem<Token *>(ret.mem) = (Token *)global_malloc_debug(sizeof(Token));
        *Mem<Token *>(ret.mem) = literal;
        return ret;
    }
}

u32 Match(Token token, const TokenType *types, u32 count) {
    for (u32 i = 0; i < count; i++) {
        if (token.type == types[i])
            return i;
    }

    return ~u32(0);
}
bool ExpectToken(ParserState *ctx, TokenType t) {
    if (ctx->stream[ctx->currentToken++].type != t) {
        global_print("%s%s%c", "Expected ", TOKEN_STRS[t], '\n');
        return false;
    }
    return true;
}
void Expect(bool cond, const char *errMsg) {
    if (!cond) {
        global_print("%s%s%c", "ERROR ", errMsg, '\n');
    }
}

bool IsTypeExpr(ParserState *ctx) {

    auto t = ctx->stream[ctx->currentToken];
    switch (t.type) {
    case TOKEN_IDENTIFIER:
        for (auto i = typeTokens; i < 1 [&typeTokens]; i++) {
            if (TokenEquals(t, i->name)) {
                return true;
            }
        }
        for (u32 i = 0; i < ctx->typeTable.size; i++) {
            if (TokensEquals(t, ctx->typeTable[i].name)) {
                return true;
            }
        }
        break;
    case TOKEN_OPEN_PAREN: {
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

Token NextToken(ParserState *ctx) {
    auto t = ctx->stream[ctx->currentToken++];
    return t;
}
Token PeekToken(ParserState *ctx) {
    return ctx->stream[ctx->currentToken];
}

u32 GetParamCountType(TypeExpr* expr) {

    FnTypeExpr* fn = (FnTypeExpr*)expr;
    ASSERT(fn->index = TYPE_PRIMARY_FN);
    return fn->param_count;
}

byte* CpyTypeExpr(byte* dst, TypeExpr* srcExpr);
TypeExpr* ParseTypeExpression(ParserState* ctx, LinearAllocator* alloc);
TypeExpr* PrimaryTypeExpression(ParserState* ctx, LinearAllocator* alloc) {

    TypeExpr* ret = (TypeExpr*)linear_allocator_top(alloc);
    Token base = NextToken(ctx);
    for (auto i = typeTokens; i < 1 [&typeTokens]; i++) {
        if (TokenEquals(base, i->name)) {
            auto node = (TypeName*)linear_allocate(alloc, sizeof(TypeName));
            *node = i->type;
            return ret;
        }
    }
    for (u32 i = 0; i < ctx->typeTable.size; i++) {
        if (TokensEquals(ctx->typeTable[i].name, base)) {
            auto dst = (byte *)linear_allocator_top(alloc);
            auto end = CpyTypeExpr(dst, ctx->typeTable[i].type);
            alloc->top += end - dst;
            return (TypeExpr *)dst;
        }
    }
    constexpr TokenType t[] = {
        TOKEN_KEYWORD_FN,
        TOKEN_OPEN_PAREN,
        TOKEN_IDENTIFIER,
    };

    u32 match = Match(base, t, 3);
    switch (match) {
    case 0: // fn
        {
            auto node = (FnTypeExpr *)linear_allocate(alloc, sizeof(FnTypeExpr));
            ExpectToken(ctx, TOKEN_OPEN_PAREN);
            node->index = TYPE_PRIMARY_FN;
            node->param_count = 0;
            node->params = 0;

            u32 it = ctx->currentToken;
            do {
                node->param_count++;
                Token q;
                for (; q.type != TOKEN_COMMA && q.type != TOKEN_CLOSE_PAREN; it++, q = ctx->stream[it]);
            } while (ctx->stream[it].type == TOKEN_COMMA);

            node->params = (TypeExpr *)linear_allocator_top(alloc);
            auto paramTypes = (TypeExpr *)linear_allocate(alloc, node->param_count * sizeof(TypeExpr));

            for(u32 i = 0; i < node->param_count; i++) {
                *(paramTypes++) = ParseTypeExpression(ctx, alloc);
                auto t = ctx->stream[ctx->currentToken];
                if (i + 1 != node->param_count) {
                    ExpectToken(ctx, TOKEN_COMMA);
                }
            }
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
            if (NextToken(ctx).type == TOKEN_KEYWORD_ARROW) {
                node->ret_t = ParseTypeExpression(ctx, alloc);
            }
            else {
                node->ret_t.index = alloc->top;
                auto last = (TypeName *)linear_allocate(alloc, sizeof(TypeName) * 2);
                last[0] = TYPE_PRIMARY_VOID;
                last[1] = TYPE_NON;
            }
            node->modifier = (TypeExpr*)linear_allocator_top(alloc);
        }
        break;
    case 1: // (TypeExpr) | (TypeExpr, TypeExpr, ...)
        {
            auto m0 = ParseTypeExpression(ctx, alloc);
            if (GetCurrentToken(ctx).type == TOKEN_COMMA) {
                TypeExpr *m[255];
                m[0] = m0;
                u32 c = 1;
                while(GetCurrentToken(ctx).type != TOKEN_CLOSE_PAREN) {
                    ExpectToken(ctx, TOKEN_COMMA);
                    ASSERT(c < 255);
                    m[c++] = ParseTypeExpression(ctx, alloc);
                }
                u32 size = (byte *)linear_allocator_top(alloc) - (byte *)m0;
                u32 offset = sizeof(StructTypeExpr) + c * sizeof(TypeExpr);
                memcpy((byte *)m0 + offset, m0, size);

                auto st = (StructTypeExpr *)m0;
                st->index = TYPE_STRUCTURE;
                st->memberCount = c;
                auto t = (TypeExpr *)(st + 1);
                st->members = t;
                for(u32 i = 0; i < c; i++) {
                    t[i] = (TypeExpr *)(((byte *)(m[i])) + offset);
                }

                alloc->top += offset;
                st->modifier = (TypeExpr *)linear_allocator_top(alloc);
            }
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
        }
        break;
    case 2: // struct
        {
            ASSERT(false);
            for (u32 i = 0; i < ctx->typeTable.size; i++) {
                if (TokensEquals(base, ctx->typeTable[i].name)) {
                    *((TypeExpr *)linear_allocate(alloc, sizeof(TypeExpr))) = ctx->typeTable[i].type;
                    return ret;
                }
            }
            global_print("%s%s*%s", "Undefined type symbol(", base.text, base.lenght, ") used in type expression\n");
        }
        break;
    }

    return ret;
}
Expr* ParseAssignment(ParserState* ctx, LinearAllocator* alloc);
TypeExpr* ParseTypeModifierExpression(ParserState* ctx, LinearAllocator* alloc) {

    TypeExpr* expr = (TypeExpr* )linear_allocator_top(alloc);
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
    for (bool run = true; run;) {

        u32 match = Match(GetCurrentToken(ctx), tokens, 6);
        switch (match) {
        case 0: {
            ctx->currentToken++;
            auto ptrT = (TypeName *)linear_allocate(alloc, sizeof(TypeName));
            *ptrT = (ctx->stream[ctx->currentToken].type == TOKEN_KEYWORD_RESTRICT ? TYPE_MODIFIER_RESTRICTED_POINTER : TYPE_MODIFIER_POINTER);
            break;
        }
        case 1:
            ctx->currentToken++;
            ParseTypeModifierExpression(ctx, alloc);
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
            break;
        case 2:
        case 3:
        case 4: {
            ctx->currentToken++;
            auto type = (TypeName *)linear_allocate(alloc, sizeof(TypeName));
            *type = modifiers[match - 4];
        } break;
        case 5: {
            ctx->currentToken++;
            auto node = (ArrayTypeExpr *)linear_allocate(alloc, sizeof(ArrayTypeExpr));
            node->index = TYPE_MODIFIER_ARRAY;
            node->arraySizeExpr = ParseAssignment(ctx, alloc);
            node->modifier = (Expr *)linear_allocator_top(alloc);
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
TypeExpr *ParseTypeExpression(ParserState *ctx, LinearAllocator *alloc) {

    TypeExpr *expr = PrimaryTypeExpression(ctx, alloc);
    ParseTypeModifierExpression(ctx, alloc);

    Mem<TypeName>(linear_allocate(alloc, sizeof(TypeName))) = TYPE_NON;
    return expr;
}

u32 GetTypeExprMemorySize(byte *base, TypeExpr *expr) {

    byte *begin = (byte *)expr;
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
    return (byte *)expr - begin;
}

byte *CpyExpr(byte *dst, Expr *srcExpr) {

    switch (srcExpr->index) {
    case EXPR_NULL:
        ASSERT(false);
        break;
    case EXPR_LITERAL:
        Mem<LiteralExpr>(dst).index = EXPR_LITERAL;
        Mem<LiteralExpr>(dst).v = Mem<LiteralExpr>(srcExpr).v;
        return dst + sizeof(LiteralExpr);
    case EXPR_UNARY:
        Mem<UnaryExpr>(dst).index = EXPR_UNARY;
        Mem<UnaryExpr>(dst).opr = Mem<UnaryExpr>(srcExpr).opr;
        Mem<UnaryExpr>(dst).e = (Expr *)dst;
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
            dstE.left = (Expr *)dst;

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
            dstE.symbolIndex = srcE.symbolIndex;
            
            return dst;
        }
    case EXPR_MEMORY_LOAD:
        {
            auto dstE = Mem<MemoryLoadExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<MemoryLoadExpr>(srcExpr);
            dstE.index = EXPR_MEMORY_LOAD;
            dstE.address = (Expr *)dst;
            return CpyExpr(dst, *srcE.address);
        }
    case EXPR_MEMORY_STORE:
        {
            auto dstE = Mem<MemoryStoreExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<MemoryStoreExpr>(srcExpr);
            dstE.index = EXPR_MEMORY_STORE;
            dstE.address = (Expr *)dst;
            dst = CpyExpr(dst, *srcE.address);
            dstE.value = (Expr *)dst;
            dst = CpyExpr(dst, *srcE.value);

            return dst;
        }
    case EXPR_MEMBER:
        {
            auto dstE = Mem<MemberExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<MemberExpr>(srcExpr);
            dstE.index = EXPR_MEMBER;
            dstE.prev = (Expr *)dst;
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
            dstE.callee = (Expr *)dst;
            dst = CpyExpr(dst, *srcE.callee);
            for (u32 i = 0; i < srcE.argCount; i++) {
                dstE.args[i] = (Expr *)dst;
                dst = CpyExpr(dst, *srcE.args[i]);
            }
            srcE.argCount = dstE.argCount;
            return dst;
        }
    case EXPR_CONVERSION:
        {
            auto dstE = Mem<ConversionExpr>(dst);
            dst += sizeof(ConversionExpr);

            auto srcE = Mem<ConversionExpr>(srcExpr);
            dstE.index = EXPR_CONVERSION;
            dstE.from = (Expr *)dst;
            dst = CpyExpr(dst, *srcE.from);

            dstE.to = (Expr *)dst;
            dst = CpyExpr(dst, *srcE.to);

            return dst;
        }
    case EXPR_ADDR_OF:
        {
            auto dstE = Mem<AddrOfExpr>(dst);
            dst += sizeof(AddrOfExpr);

            auto srcE = Mem<AddrOfExpr>(srcExpr);
            dstE.index = EXPR_ADDR_OF;

            dstE.e = (Expr *)dst;
            dst = CpyExpr(dst, *srcE.e);

            return dst;
        }
    case EXPR_PEEL_TYPE:
        {
            auto dstE = Mem<PeelTypeExpr>(dst);
            dst += sizeof(PeelTypeExpr);

            auto srcE = Mem<PeelTypeExpr>(srcExpr);
            dstE.index = EXPR_PEEL_TYPE;

            dstE.e = (Expr *)dst;
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
        case TYPE_PRIMARY_NATIVE_FN: {
            FnTypeExpr *srcNode = (FnTypeExpr *)srcExpr;
            FnTypeExpr *dstNode = (FnTypeExpr *)dst;
            memcpy(dstNode, srcNode, sizeof(FnTypeExpr));
            dst += sizeof(FnTypeExpr);

            if (srcNode->param_count != 0) {
                ASSERT(*srcNode->params);

                dstNode->params = (TypeExpr *)dst;
                dst += srcNode->param_count * sizeof(TypeExpr);
                for (u32 i = 0; i < srcNode->param_count; i++) {
                    (*dstNode->params)[i] = (TypeExpr *)dst;
                    dst = CpyTypeExpr(dst, (*srcNode->params) + i);
                }
            }

            dstNode->ret_t = (TypeExpr *)dst;
            dst = CpyTypeExpr(dst, *srcNode->ret_t);

            dstNode->modifier = (TypeExpr *)dst;
            srcExpr = *(srcNode->modifier);
        } break;
        case TYPE_MODIFIER_ARRAY: {
            auto &srcArr = Mem<ArrayTypeExpr>(srcExpr);
            auto &dstArr = Mem<ArrayTypeExpr>(dst);
            dst += sizeof(ArrayTypeExpr);

            dstArr.index = TYPE_MODIFIER_ARRAY;
            dstArr.arraySizeExpr = (Expr *)dst;
            dst = CpyExpr(dst, *srcArr.arraySizeExpr);
            dstArr.modifier = (TypeExpr *)dst;
            srcExpr = *(srcArr.modifier);
        } break;
        case TYPE_STRUCTURE: {
            auto &srcSt = Mem<StructTypeExpr>(srcExpr);
            auto &dstSt = Mem<StructTypeExpr>(dst);
            dstSt.index = TYPE_STRUCTURE;
            dstSt.memberCount = srcSt.memberCount;
            dstSt.members = (TypeExpr *)(dst + sizeof(StructTypeExpr));
            dst += sizeof(StructTypeExpr) + srcSt.memberCount * sizeof(TypeExpr);
            for (u32 i = 0; i < dstSt.memberCount; i++) {
                (*dstSt.members)[i] = (TypeExpr *)dst;
                dst = CpyTypeExpr(dst, *((*srcSt.members)[i]));
            }
            dstSt.modifier = (TypeExpr *)dst;
            srcExpr = *srcSt.modifier;
        } break;
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

TypeName GetLastType(TypeExpr *expr) {

    auto last = Mem<TypeName>(expr);
    while (expr->index != TYPE_NON) {

        last = Mem<TypeName>(expr);
        switch (last) {
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
u32 GetTypeSize(TypeExpr* type) {

    u32 size = 0;
    switch(type->index) {
    case TYPE_PRIMARY_INT8:
    case TYPE_PRIMARY_UINT8:
    case TYPE_PRIMARY_CHAR:
    case TYPE_PRIMARY_BOOL:
        size = 1;
        break;
    case TYPE_PRIMARY_INT16:
    case TYPE_PRIMARY_UINT16:
        size = 2;
        break;
    case TYPE_PRIMARY_INT32:
    case TYPE_PRIMARY_UINT32:
    case TYPE_PRIMARY_F32:
        size = 4;
        break;
    case TYPE_PRIMARY_INT64:
    case TYPE_PRIMARY_UINT64:
    case TYPE_PRIMARY_F64:
    case TYPE_MODIFIER_RESTRICTED_POINTER:
    case TYPE_MODIFIER_POINTER:
        size = 8;
        break;
    case TYPE_STRUCTURE:
        {
            auto count = Mem<StructTypeExpr>(type).memberCount;
            TypeExpr* members = *Mem<StructTypeExpr>(type).members;
            for(u32 i = 0; i < count; i++) {
                size += GetTypeSize(*members[i]);
            }
            type = (*Mem<StructTypeExpr>(type).modifier) - 1;
            break;
        }
    }
    type++;

    while(type->index) {
    
        switch(type->index) {
        case TYPE_MODIFIER_RESTRICTED_POINTER:
        case TYPE_MODIFIER_POINTER:
            size = 8;
            type++;
            break;
        case TYPE_MODIFIER_ARRAY:
            {
                size *= Mem<u64>(EvalExpr(*Mem<ArrayTypeExpr>(type).arraySizeExpr).mem);
                type = *Mem<ArrayTypeExpr>(type).modifier;
                break;
            }
        default:
            {
                type++;
            }
        }
    }
    return size;
}
TypeExpr *GetNthType(TypeExpr *expr, u32 nth) {

    TypeExpr *ptrs[nth + 1]{};
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
u32 GetAllMembers(TypeExpr *e, TypeExpr **result) {

    if (e->index != TYPE_STRUCTURE) {
        result[0] = e;
        return 1;
    }
    auto st = (StructTypeExpr *)e;

    auto m = *(st->members);
    u32 ret = 0;
    for (u32 i = 0; i < st->memberCount; i++) {
        ret += GetAllMembers(*(m[i]), result + ret);
    }
    return ret;
}
bool TypesEqual(TypeExpr *e0, TypeExpr *e1) {

    auto t0 = Mem<TypeName>(e0);
    auto t1 = Mem<TypeName>(e1);
    if (t0 != t1)
        return false;

    while (e0->index != TYPE_NON && e1->index != TYPE_NON) {

        switch (Mem<TypeName>(e0)) {
        case TYPE_PRIMARY_NATIVE_FN:
        case TYPE_PRIMARY_FN: {
            auto &fn0 = Mem<FnTypeExpr>(e0);
            auto &fn1 = Mem<FnTypeExpr>(e1);

            TypeExpr *localMem[512];
            TypeExpr **m0 = localMem;
            u32 mCount0 = 0;
            u32 mCount1 = 0;
            for (u32 i = 0; i < fn0.param_count; i++) {
                mCount0 += GetAllMembers(*fn0.params + i, m0 + mCount0);
            }
            TypeExpr **m1 = localMem + mCount0;
            for (u32 i = 0; i < fn1.param_count; i++) {
                mCount1 += GetAllMembers(*fn1.params + i, m1 + mCount1);
            }
            if (mCount0 != mCount1) {
                return false;
            }

            for (u32 i = 0; i < mCount0; i++) {
                if (!TypesEqual(m0[i], m1[i])) {
                    return false;
                }
            }
            if (!TypesEqual(*fn0.ret_t, *fn1.ret_t))
                return false;

            e0 = *fn0.modifier;
            e1 = *fn1.modifier;
        } break;
        case TYPE_MODIFIER_ARRAY: {
            auto &fn0 = Mem<ArrayTypeExpr>(e0);
            auto &fn1 = Mem<ArrayTypeExpr>(e1);
            if (*fn0.arraySizeExpr != *fn1.arraySizeExpr) {
                return false;
            }
            e0 = *(Mem<ArrayTypeExpr>(e0).modifier);
            e1 = *(Mem<ArrayTypeExpr>(e1).modifier);
        } break;
        default: {
            TypeExpr *localMem[512];
            TypeExpr **m0 = localMem;
            u32 mCount0 = GetAllMembers(e0, m0);
            TypeExpr **m1 = localMem + mCount0;
            u32 mCount1 = GetAllMembers(e1, m1);

            if (mCount0 != mCount1)
                return false;
            for (u32 i = 0; i < mCount0; i++) {
                if (!TypesEqual(m0[i], m1[i]))
                    return false;
            }
            if (e0->index == TYPE_STRUCTURE) {
                e0 = *(Mem<StructTypeExpr>(e0).modifier);
            } else {
                e0++;
            }
            if (e1->index == TYPE_STRUCTURE) {
                e1 = *(Mem<StructTypeExpr>(e1).modifier);
            } else {
                e1++;
            }
        } break;
        }
    }

    return e0->index == e1->index;
}
ExprType GetLastExprType(byte *baseMem, Expr *expr) {
    return Mem<ExprType>(baseMem + expr->index);
}
u32 GetGlobalSymbolCount(SymbolTable table) {

    u32 count = 0;
    for(u32 i = 0; i < table.size; i++) {
        if(table[i].scope > 0) break;
        count++;
    }
    return count;
}
byte* GetTypeExpr(Expr* expr, byte* dst, SymbolTable table) {

    switch (expr->index) {
    case EXPR_NULL:
        ASSERT(false);
        break;
    case EXPR_LITERAL: 
        {
            auto &l = Mem<LiteralExpr>(expr);
            Mem<TypeName>(dst) = (TypeName)l.v.type;
            Mem<TypeName>(dst + sizeof(TypeName)) = TYPE_NON;
            break;
        }
    case EXPR_UNARY:
        {
            auto &u = Mem<UnaryExpr>(expr);
            return GetTypeExpr(*u.e, dst, table);
        }
    case EXPR_BINARY:
        {
            auto &b = Mem<BinaryExpr>(expr);
            auto end = GetTypeExpr(*b.left, dst, table);
            end = GetTypeExpr(*b.left, end, table);

            auto left_t = GetLastType((Expr *)dst);
            auto right_t = GetLastType((Expr *)end);
            switch (b.opr) {
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
                    return GetTypeExpr(*b.left, dst, table);
                else
                    return end;
            case TOKEN_SUB_SCRIPT_OPR:
                if (left_t == TYPE_MODIFIER_ARRAY || left_t == TYPE_MODIFIER_POINTER || left_t == TYPE_MODIFIER_RESTRICTED_POINTER)
                    return end;
                else if (right_t == TYPE_MODIFIER_POINTER || right_t == TYPE_MODIFIER_RESTRICTED_POINTER) {
                    return GetTypeExpr(*b.left, dst, table);
                }
            }
            ASSERT(false);
        }
    case EXPR_SYMBOL:
        {
            auto global = Mem<SymbolExpr>(expr).global;
            auto i = Mem<SymbolExpr>(expr).symbolIndex;
            u32 globalCount = GetGlobalSymbolCount(table);
            i = global ? i : i + globalCount;
            return CpyTypeExpr(dst, table[i].type);
        }
    case EXPR_MEMORY_LOAD:
    case EXPR_MEMORY_STORE:
        {
            GetTypeExpr(*Mem<MemoryLoadExpr>(expr).address, dst, table);
            TypeExpr *nth = GetNthType((TypeExpr *)dst, 0);
            Mem<TypeName>(nth) = TYPE_NON;
            return (byte *)(nth + 1);
        }
    case EXPR_MEMBER:
        {
            GetTypeExpr(*Mem<MemberExpr>(expr).prev, dst, table);
            auto t = GetLastType((TypeExpr *)dst);
            ASSERT(false);
            break;
        }
    case EXPR_CALL:
        {
            GetTypeExpr(*Mem<CallExpr>(expr).callee, dst, table);
            auto fn = (FnTypeExpr *)dst;
            return CpyTypeExpr(dst, *fn->ret_t);
        }
    case EXPR_CONVERSION:
        return CpyTypeExpr(dst, *Mem<ConversionExpr>(expr).to);
    case EXPR_ADDR_OF:
        {
            auto end = GetTypeExpr(*Mem<AddrOfExpr>(expr).e, dst, table);
            Mem<TypeName>(end) = TYPE_MODIFIER_POINTER;
            Mem<TypeName>(end + sizeof(TypeName)) = TYPE_NON;
            return end + sizeof(TypeName);
        }
    case EXPR_PEEL_TYPE:
        {
            auto end = GetTypeExpr(*Mem<PeelTypeExpr>(expr).e, dst, table);
            auto last = GetNthType((Expr *)dst, 0);
            last->index = TYPE_NON;
            return (byte *)last + sizeof(TypeName);
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
    switch (match) {
    case 0:
    case 1:
    case 2:
    case 3:
    case 4: { // literal
        auto node = (LiteralExpr*)linear_allocate(alloc, sizeof(LiteralExpr));
        node->index = EXPR_LITERAL;
        node->v = GetValueFromLiteral(NextToken(ctx));
        return node;
    }
    case 5: // (Expr) | (TypeExpr)
        if (IsTypeExpr(ctx)) {
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
        auto name = NextToken(ctx);
        auto node = (SymbolExpr*)linear_allocate(alloc, sizeof(SymbolExpr));
        node->index = EXPR_SYMBOL;
        node->symbolIndex = FindSymbol(ctx->table, name);
        node->global = (ctx->table[node->symbolIndex].scope == 0);
        node->debug_name = name;

        u32 f = !node->global ? ~u32(0) : 0;
        node->symbolIndex -= f & ctx->globalCount;

        if(node->symbolIndex == ~u32(0)) {
            global_print("s, s*, s, i, c", "ERROR: undefined symbol(", name.text, name.lenght, ") referenced in expression at: ", GetLineNumber(ctx->begin, name.text), '\n');
        }

        return node;
    }
    case 7: // *Expr
    {
        ctx->currentToken++;
        auto node = (MemoryLoadExpr*)linear_allocate(alloc, sizeof(MemoryLoadExpr));
        node->index = EXPR_MEMORY_LOAD;
        node->address = ParsePrimary(ctx, alloc);
        return node;
    }
    case 8: // &Expr
    {
        ctx->currentToken++;
        auto node = (AddrOfExpr *)linear_allocate(alloc, sizeof(AddrOfExpr));
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

    while (match != ~u32(0)) {
        ctx->currentToken++;
        switch (match) {
        case 0: 
            {
                ExpectToken(ctx, TOKEN_IDENTIFIER);
                auto node = (MemberExpr *)linear_allocate(alloc, sizeof(MemberExpr));
                node->index = EXPR_MEMBER;
                node->prev = expr;
                node->name = ctx->stream[ctx->currentToken - 1];
                expr = (Expr *)node;

                auto top = (TypeExpr *)linear_allocator_top(alloc);
                GetTypeExpr(expr, (byte *)top, ctx->table);
                auto nth = GetNthType(top, 0);
                Mem<TypeName>(nth) = TYPE_NON;
                TypeName t = GetLastType(top);

                if(t != TYPE_MODIFIER_ARRAY && t < TYPE_COUNT) {
                    expr = (Expr *)linear_allocator_top(alloc);
                    auto memRef = (MemoryLoadExpr *)linear_allocate(alloc, sizeof(MemoryLoadExpr));
                    memRef->index = EXPR_MEMORY_LOAD;
                    memRef->address = expr;
                }
                else {
                    expr = (Expr *)linear_allocator_top(alloc);
                    auto peel = (PeelTypeExpr *)linear_allocate(alloc, sizeof(PeelTypeExpr));
                    peel->index = EXPR_PEEL_TYPE;
                    peel->e = expr;
                }
            } 
        break;
        case 1:
            {
                auto top = (TypeExpr *)linear_allocator_top(alloc);
                GetTypeExpr(expr, (byte *)top, ctx->table);
                TypeName t = GetLastType(top);
                Expect(t == TYPE_MODIFIER_ARRAY || t == TYPE_MODIFIER_POINTER || t == TYPE_MODIFIER_RESTRICTED_POINTER, "operator ([]) expects pointer or array types");
                TypeExpr *peeled = GetNthType(top, 1);
                t = Mem<TypeName>(peeled);

                auto tmp = linear_allocator_top(alloc);
                if(t != TYPE_MODIFIER_ARRAY && t < TYPE_COUNT) {

                    auto memRef = (MemoryLoadExpr *)linear_allocate(alloc, sizeof(MemoryLoadExpr));
                    memRef->index = EXPR_MEMORY_LOAD;
                    memRef->address = (Expr *)linear_allocator_top(alloc);
                }
                else {
                    auto peel = (PeelTypeExpr *)linear_allocate(alloc, sizeof(PeelTypeExpr));
                    peel->index = EXPR_PEEL_TYPE;
                    peel->e = (Expr *)linear_allocator_top(alloc);
                }

                auto node = (BinaryExpr *)linear_allocate(alloc, sizeof(BinaryExpr));
                node->index = EXPR_BINARY;
                node->opr = TOKEN_SUB_SCRIPT_OPR;
                node->left = expr;
                node->right = ParseAssignment(ctx, alloc);
                ExpectToken(ctx, TOKEN_CLOSE_BRACKET);
                expr = (Expr *)tmp;
                break;
            } 
        }
        match = Match(GetCurrentToken(ctx), tokens, 2);
    }
    return expr;
}

Expr* ParseCall(ParserState* ctx, LinearAllocator* alloc) {

    Expr* callee = ParseMemory(ctx, alloc);
    while (GetCurrentToken(ctx).type == TOKEN_OPEN_PAREN) {
        ctx->currentToken++;
        Expr* args[255];
        u32 i = 0;

        GetTypeExpr(callee, (byte*)linear_allocator_top(alloc), ctx->table);
        FnTypeExpr* fn = (FnTypeExpr*)linear_allocator_top(alloc);
        ASSERT(fn->index == TYPE_PRIMARY_FN || fn->index == TYPE_PRIMARY_NATIVE_FN); // or fn*
        TypeName retT = GetLastType((TypeExpr*)linear_allocator_top(alloc));
        if (retT == TYPE_MODIFIER_ARRAY || retT > TYPE_COUNT) {
            auto addrOf = (AddrOfExpr*)linear_allocate(alloc, sizeof(AddrOfExpr));
            addrOf->index = EXPR_ADDR_OF;
            addrOf->e.index = ~u32(0);
            args[i++] = addrOf;
        }

        // max args count: 255
        if (GetCurrentToken(ctx).type != TOKEN_CLOSE_PAREN) {
            args[i++] = ParseAssignment(ctx, alloc);
            while(GetCurrentToken(ctx).type != TOKEN_CLOSE_PAREN) {
                ExpectToken(ctx, TOKEN_COMMA);
                args[i++] = ParseAssignment(ctx, alloc);
            }
        }
        ExpectToken(ctx, TOKEN_CLOSE_PAREN);
        Expect(i < 256, "ERROR: max argument count is 255");

        auto call = (CallExpr*)linear_allocate(alloc, sizeof(CallExpr) + i * sizeof(Expr));
        call->index = EXPR_CALL;
        call->callee = callee;
        call->argCount = i;
        for (u32 k = 0; k < i; k++) {
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

    if (!ret) {
        ret = ParseCall(ctx, alloc);
    }
    return ret;
}
Expr* ParseFactor(ParserState* ctx, LinearAllocator* alloc) {

    Expr* expr = ParseUnary(ctx, alloc);

    TokenType binaries[2] = {TOKEN_ASTERISK, TOKEN_SLASH};
    u32 match = Match(GetCurrentToken(ctx), binaries, 2);
    while (match != ~u32(0)) {
        ctx->currentToken++;

        auto node = (BinaryExpr*)linear_allocate(alloc, sizeof(BinaryExpr));
        node->index = EXPR_BINARY;
        node->opr = binaries[match];
        node->left = expr;
        node->right = ParseUnary(ctx, alloc);

        match = Match(GetCurrentToken(ctx), binaries, 2);
        expr = node;
    }

    return expr;
}
Expr* ParseTerm(ParserState* ctx, LinearAllocator* alloc) {

    Expr *expr = ParseFactor(ctx, alloc);

    TokenType binaries[9] = {
        TOKEN_PLUS,
        TOKEN_MINUS,
        TOKEN_AMPERSAND_AMPERSAND,
        TOKEN_VERTICAL_BAR_VERTICAL_BAR,
        TOKEN_CIRCUMFLEX,
        TOKEN_AMPERSAND,
        TOKEN_VERTICAL_BAR,
        TOKEN_LSHIFT_LSHIFT,
        TOKEN_RSHIFT_RSHIFT};
    u32 match = Match(GetCurrentToken(ctx), binaries, 9);
    while(match != ~u32(0)) {
        ctx->currentToken++;

        auto node = (BinaryExpr *)linear_allocate(alloc, sizeof(BinaryExpr));
        node->index = EXPR_BINARY;
        node->opr = binaries[match];
        node->left = expr;
        node->right = ParseFactor(ctx, alloc);

        expr = node;
        match = Match(GetCurrentToken(ctx), binaries, 9);
    }

    return expr;
}
Expr* ParseComparision(ParserState* ctx, LinearAllocator* alloc) {

    Expr *expr = ParseTerm(ctx, alloc);

    TokenType binaries[6] = {
        TOKEN_RSHIFT,
        TOKEN_LSHIFT,
        TOKEN_RSHIFT_EQUALS,
        TOKEN_LSHIFT_EQUALS,
        TOKEN_EXCLAMATION_EQUALS,
        TOKEN_EQUALS_EQUALS};
    u32 match = Match(GetCurrentToken(ctx), binaries, 6);
    while (match != ~u32(0)) {
        ctx->currentToken++;

        auto node = (BinaryExpr *)linear_allocate(alloc, sizeof(BinaryExpr));
        node->index = EXPR_BINARY;
        node->opr = binaries[match];
        node->left = expr;
        node->right = ParseTerm(ctx, alloc);

        expr = node;
        match = Match(GetCurrentToken(ctx), binaries, 6);
    }

    return expr;
}
Expr* ParseAssignment(ParserState* ctx, LinearAllocator* alloc) {

    Expr *lval = ParseComparision(ctx, alloc);

    bool match = (GetCurrentToken(ctx).type == TOKEN_EQUAL_SIGN);
    while(match) {
        ctx->currentToken++;
        Expr *rval = ParseAssignment(ctx, alloc);

        if (rval->index == EXPR_CALL) {
            GetTypeExpr(lval, (byte *)linear_allocator_top(alloc), ctx->table);
            auto lvalType = GetLastType((TypeExpr *)linear_allocator_top(alloc));
            if (lvalType == TYPE_MODIFIER_ARRAY || lvalType > TYPE_COUNT) {
                auto call = (CallExpr *)rval;
                ASSERT((*call->args[0])->index == EXPR_ADDR_OF);
                ((AddrOfExpr *)*call->args[0])->e = lval;
                lval = rval;
            }
        }
        switch (lval->index) {
        case EXPR_MEMORY_LOAD:
            {
                auto load = (MemoryLoadExpr *)lval;
                auto store = (MemoryStoreExpr *)linear_allocate(alloc, sizeof(MemoryStoreExpr));
                store->index = EXPR_MEMORY_STORE;
                store->address = *(load->address);
                store->value = rval;
                lval = store;
            }
            break;
        case EXPR_SYMBOL:
            {
                auto assignment = (BinaryExpr *)linear_allocate(alloc, sizeof(BinaryExpr));
                assignment->index = EXPR_BINARY;
                assignment->opr = TOKEN_EQUAL_SIGN;
                assignment->left = lval;
                assignment->right = rval;
                lval = assignment;
            }
            break;
        }

        match = (GetCurrentToken(ctx).type == TOKEN_EQUAL_SIGN);
    }

    return lval;
}

enum StatementType {
    STATEMENT_NON,

    STATEMENT_FN_BEGIN,
    STATEMENT_FN_END,
    STATEMENT_EXPRESSION,
    STATEMENT_PRINT,

    STATEMENT_BRANCH,
    STATEMENT_FOR_LOOP,
    STATEMENT_DECLARE,

    STATEMENT_RETURN,
    STATEMENT_BREAK,
    STATEMENT_NEXT,

    STATEMENT_COUNT,
};

struct ParamList {
    u32 count;
    Token names[];
};

typedef Expr Stmt;
struct StmtReturn : Stmt {
    Expr expr;
};
struct StmtFnBody : Stmt {
    u32 symbolIndex;
    u32 argCount;
    Token *args;
};
struct StmtExpr : Stmt {
    Expr expr;
};
struct StmtPrint : Stmt {
    u32 exprCount;
    Expr exprString[0]; // inline buffer
};
struct StmtDecl : Stmt {
    Symbol s;
    void* extra;
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
    return table->memory + table->size++;
}
TypeSymbol* MakeTypeSymbolTableEntry(TypeSymbolTable* table) {
    return table->memory + table->size++;
}
void PurgeParseSymbols(u32 scope, SymbolTable* table) {

    for (u32 i = 0; i < table->size; i++) {
        auto it = (*table)[i];
        if (scope < (*table)[i].scope) {
            auto q = (*table)[--table->size];
            (*table)[i].type  = q.type;
            (*table)[i].extra = q.extra;
            (*table)[i].name  = q.name;
            (*table)[i].scope = q.scope;
            i--;
        }
    }
}

void PrintTypeExpression(TypeExpr* expr, SymbolTable table);
void PrintExpr(Expr* expr, SymbolTable table);

void PrintSymbols(SymbolTable table) {
    global_print("c", '\n');
    for(u32 i = 0; i < table.size; i++) {
        auto name = table[i].name;
        global_print("s*,c", name.text, name.lenght, '\n');
    }
}

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
    u32 match = Match(NextToken(ctx), tokens, 13);
    switch (match) {
    case 0: // eof
        {
            auto sentinel = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
            sentinel->index = STATEMENT_NON;
            return nullptr;
        }
    case 1: // main
        {
            auto fnBegin = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
            fnBegin->index = STATEMENT_FN_BEGIN;

            auto fnBody = (StmtDecl*)linear_allocate(stmtAlloc, sizeof(StmtDecl));
            fnBody->index = STATEMENT_DECLARE;
            fnBody->extra = (ParamList*)linear_allocate(exprAlloc, sizeof(u32));
            Mem<ParamList>(fnBody->extra).count = 0;

            ctx->globalCount += (scope == 0);
            auto symbol = MakeSymbolTableEntry(&ctx->table);
            auto tableSize = ctx->table.size;
            symbol->name = GetPrevToken(ctx);
            symbol->scope = scope;
            symbol->extra = ParseStatement(ctx, exprAlloc, stmtAlloc, scope + 1);
            symbol->type = nullptr;
            CopySymbol(&fnBody->s, symbol);

            ctx->table.size = tableSize;
            auto sentinel = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
            sentinel->index = STATEMENT_NON;
            return fnBody;
        }
    case 2: // fn q()
        {
            auto fnBegin = (Stmt*)linear_allocate(stmtAlloc, sizeof(Stmt));
            fnBegin->index = STATEMENT_FN_BEGIN;

            auto fnBody = (StmtDecl*)linear_allocate(stmtAlloc, sizeof(StmtDecl));
            fnBody->index = STATEMENT_DECLARE;


            ctx->globalCount += (scope == 0);
            auto symbol = MakeSymbolTableEntry(&ctx->table);
            symbol->name = NextToken(ctx);
            symbol->scope = scope;
            ExpectToken(ctx, TOKEN_OPEN_PAREN);
            auto tableINdex = ctx->table.size;

            auto fn_t = (FnTypeExpr*)linear_allocate(exprAlloc, sizeof(FnTypeExpr));
            symbol->type = fn_t;
            fn_t->index = TYPE_PRIMARY_FN;

            TypeExpr* argTypes[255];
            u32 argNames[255];
            u32 i = 0;

            if (GetCurrentToken(ctx).type != TOKEN_CLOSE_PAREN) {
                argTypes[0] = ParseTypeExpression(ctx, exprAlloc);
                argNames[0] = ctx->currentToken++;
                i = 1;
                while (GetCurrentToken(ctx).type != TOKEN_CLOSE_PAREN) {
                    ExpectToken(ctx, TOKEN_COMMA);
                    argTypes[i] = ParseTypeExpression(ctx, exprAlloc);
                    argNames[i++] = ctx->currentToken++;
                }
            }
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
            fn_t->param_count = i;
            fn_t->params = (TypeExpr *)linear_allocate(exprAlloc, sizeof(TypeExpr) * i);

            if (GetCurrentToken(ctx).type == TOKEN_KEYWORD_ARROW) {
                ctx->currentToken++;
                fn_t->ret_t = ParseTypeExpression(ctx, exprAlloc);
            }
            else {
                fn_t->ret_t.index = exprAlloc->top;
                auto void_t = (TypeName *)linear_allocate(exprAlloc, sizeof(TypeName) * 2);
                void_t[0] = TYPE_PRIMARY_VOID;
                void_t[1] = TYPE_NON;
            }
            fnBody->extra = (ParamList*)linear_allocate(exprAlloc, sizeof(u32) + i * sizeof(Token));
            Mem<ParamList>(fnBody->extra).count = i;

            auto params = *fn_t->params;
            for(u32 k = 0; k < i; k++) {
                auto arg = MakeSymbolTableEntry(&ctx->table);
                arg->extra = 0;
                arg->name = ctx->stream[argNames[k]];
                Mem<ParamList>(fnBody->extra).names[k] = arg->name;
                arg->scope = ctx->scope + 1;
                arg->type = argTypes[k];
                params[k] = argTypes[k];
            }

            fn_t->modifier = (TypeExpr*)linear_allocate(exprAlloc, sizeof(TypeName));
            (*fn_t->modifier)->index = 0;

            symbol->extra = (Value*)linear_allocate(exprAlloc, sizeof(Value));
            Mem<Value>(symbol->extra).type = TYPE_MODIFIER_POINTER;
            Mem<void*>(Mem<Value>(symbol->extra).mem) = ParseStatement(ctx, exprAlloc, stmtAlloc, scope + 1);
            CopySymbol(&fnBody->s, symbol);
            ctx->table.size = tableINdex;

            auto sentinel = (Stmt *)linear_allocate(stmtAlloc, sizeof(Stmt));
            sentinel->index = STATEMENT_NON;

            return fnBody;
        }
    case 3: // struct
        {
            auto st = MakeTypeSymbolTableEntry(&ctx->typeTable);
            st->name = NextToken(ctx);

            TypeExpr* m[255];
            u32 namesTokenIndex[255];
            u32 c = 0;
            if (GetCurrentToken(ctx).type != TOKEN_CLOSE_BRACES) {
                ctx->currentToken += (GetCurrentToken(ctx).type == TOKEN_OPEN_BRACES);
                m[0] = ParseTypeExpression(ctx, exprAlloc);
                namesTokenIndex[0] = ctx->currentToken++;
                c = 1;
                while (GetCurrentToken(ctx).type != TOKEN_CLOSE_BRACES) {
                    ExpectToken(ctx, TOKEN_COMMA);
                    ASSERT(c < 255);
                    m[c] = ParseTypeExpression(ctx, exprAlloc);
                    namesTokenIndex[c++] = ctx->currentToken++;
                }
                ctx->currentToken += GetCurrentToken(ctx).type == TOKEN_COMMA;
            }
            ExpectToken(ctx, TOKEN_CLOSE_BRACES);
            st->extra = linear_allocate(exprAlloc, c * sizeof(Token));
            Mem<u32>(st->extra) = c;
            auto st_t = (StructTypeExpr *)linear_allocate(exprAlloc, sizeof(StructTypeExpr) + c * sizeof(TypeExpr));
            st_t->index = TYPE_STRUCTURE;
            st_t->memberCount = c;
            st_t->members = (st_t + 1);

            auto names = (byte *)st->extra + sizeof(u32);
            for (u32 i = 0; i < c; i++) {
                ((Token *)names)[i] = ctx->stream[namesTokenIndex[i]];
                (*st_t->members)[i] = m[i];
            }
            st_t->modifier = (TypeExpr *)linear_allocate(exprAlloc, sizeof(TypeName));
            (*st_t->modifier)->index = TYPE_NON;
            st->type = st_t;

            goto fn_entry;
        }
    case 4: // {
        {
            auto ret = ParseStatement(ctx, exprAlloc, stmtAlloc, scope + 1);
            if (ret) {
                while (ParseStatement(ctx, exprAlloc, stmtAlloc, scope + 1));
            }
            ExpectToken(ctx, TOKEN_CLOSE_BRACES);
            PurgeParseSymbols(scope, &ctx->table);
            return ret;
        }
    case 5: // }
        {
            ctx->currentToken--;
            return nullptr;
        }
    case 6: // return
        {
            auto stmtRet = (StmtReturn *)linear_allocate(stmtAlloc, sizeof(StmtReturn));
            stmtRet->index = STATEMENT_RETURN;
            stmtRet->expr = ParseAssignment(ctx, exprAlloc);
            ExpectToken(ctx, TOKEN_SEMICOLON);
            return stmtRet;
        }
    case 7: // if
        {
            ExpectToken(ctx, TOKEN_OPEN_PAREN);
            auto branch = (StmtBranch *)linear_allocate(stmtAlloc, sizeof(StmtBranch));
            branch->index = STATEMENT_BRANCH;

            branch->cond = ParseAssignment(ctx, exprAlloc);
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);
            branch->thenBranch = ParseStatement(ctx, exprAlloc, stmtAlloc, scope + 1);
            auto sentinel0 = (Stmt *)linear_allocate(stmtAlloc, sizeof(Stmt));
            sentinel0->index = STATEMENT_NON;

            if (GetCurrentToken(ctx).type == TOKEN_KEYWORD_ELSE) {
                ctx->currentToken++;
                branch->elseBranch = ParseStatement(ctx, exprAlloc, stmtAlloc, scope + 1);
                auto sentinel1 = (Stmt *)linear_allocate(stmtAlloc, sizeof(Stmt));
                sentinel1->index = STATEMENT_NON;
            }
            branch->end = (Stmt *)linear_allocator_top(stmtAlloc);
            PurgeParseSymbols(scope, &ctx->table);
            return branch;
        }
    case 8: // for
        {
            ExpectToken(ctx, TOKEN_OPEN_PAREN);
            auto loop = (StmtFor *)linear_allocate(stmtAlloc, sizeof(StmtFor));
            loop->index = STATEMENT_FOR_LOOP;
            loop->init = ParseStatement(ctx, exprAlloc, stmtAlloc, scope + 1);
            ((Stmt *)linear_allocate(stmtAlloc, sizeof(Stmt)))->index = STATEMENT_NON;

            loop->cond = ParseAssignment(ctx, exprAlloc);
            ExpectToken(ctx, TOKEN_SEMICOLON);
            loop->inc = ParseStatement(ctx, exprAlloc, stmtAlloc, scope + 1);
            ((Stmt *)linear_allocate(stmtAlloc, sizeof(Stmt)))->index = STATEMENT_NON;
            ExpectToken(ctx, TOKEN_CLOSE_PAREN);

            loop->body = ParseStatement(ctx, exprAlloc, stmtAlloc, scope + 1);

            ((Stmt *)linear_allocate(stmtAlloc, sizeof(Stmt)))->index = STATEMENT_NON;
            PurgeParseSymbols(scope, &ctx->table);

            loop->end = (Stmt *)linear_allocator_top(stmtAlloc);

            return loop;
        }
    case 9: // do
        {
            auto loop = (StmtFor *)linear_allocate(exprAlloc, sizeof(StmtFor));
            loop->index = STATEMENT_FOR_LOOP;
            loop->init = (Stmt *)linear_allocate(stmtAlloc, sizeof(Stmt));
            (*loop->init)->index = STATEMENT_NON;

            auto litExpr = (LiteralExpr *)linear_allocate(exprAlloc, sizeof(LiteralExpr));
            loop->cond = litExpr;
            litExpr->index = EXPR_LITERAL;
            Mem<bool>(litExpr->v.mem) = true;
            litExpr->v.type = TYPE_PRIMARY_BOOL;

            loop->inc = (Stmt *)linear_allocate(stmtAlloc, sizeof(Stmt));
            (*loop->inc)->index = STATEMENT_NON;

            loop->body = ParseStatement(ctx, exprAlloc, stmtAlloc, scope + 1);
            ((Stmt *)linear_allocate(stmtAlloc, sizeof(Stmt)))->index = STATEMENT_NON;

            return loop;
        }
    case 10: // assume
        return nullptr;
    case 11: // print
        {
            auto print = (StmtPrint *)linear_allocate(stmtAlloc, sizeof(StmtPrint));
            print->index = STATEMENT_PRINT;

            u32 i = 0;
            while(1) {
                print->exprString[i++] = ParseAssignment(ctx, exprAlloc);

                if (GetCurrentToken(ctx).type != TOKEN_SEMICOLON) {
                    ExpectToken(ctx, TOKEN_COMMA);
                }
                else {
                    break;
                }
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
            auto t = ctx->stream[ctx->currentToken];
            if (IsTypeExpr(ctx)) {
                auto save = ctx->currentToken;
                auto saveTop = exprAlloc->top;
                auto type = ParseTypeExpression(ctx, exprAlloc);
                auto name = GetCurrentToken(ctx);
                if(name.type != TOKEN_IDENTIFIER || FindSymbol(ctx->table, name) != ~u32(0)) {
                    ctx->currentToken = save;
                    exprAlloc->top = saveTop;
                }
                else {
                    auto decl = (StmtDecl*)linear_allocate(stmtAlloc, sizeof(StmtDecl));
                    decl->index = STATEMENT_DECLARE;
                    auto symbol = MakeSymbolTableEntry(&ctx->table);
                    symbol->type = type;
                    symbol->name = name;
                    symbol->scope = scope;
                    symbol->extra = 0;
                    CopySymbol(&decl->s, symbol);
                }
            }

            auto exprStmt = (StmtExpr *)linear_allocate(stmtAlloc, sizeof(StmtExpr));
            exprStmt->index = STATEMENT_EXPRESSION;
            exprStmt->expr = ParseAssignment(ctx, exprAlloc);
            t = ctx->stream[ctx->currentToken];
            ExpectToken(ctx, TOKEN_SEMICOLON);
            return exprStmt;
        }
    case 13: // ;
        {
            auto exprStmt = (StmtExpr *)linear_allocate(stmtAlloc, sizeof(StmtExpr));
            exprStmt->index = STATEMENT_EXPRESSION;
            exprStmt->expr.index = 0;
            return exprStmt;
        }
    }
}

u32 Tokenize(Tokenizer *tokenizer, Token *result) {

    u32 count = 0;
    for (auto t = GetToken(tokenizer); t.type != TOKEN_EOF; t = GetToken(tokenizer)) {
        result[count++] = t;
    }
    return count;
}

Symbol FindMain(SymbolTable table) {
    for (u32 i = 0; i < table.size; i++) {
        if (TokenEquals(table[i].name, "main")) {
            return table[i];
        }
    }
}
void PrintValue(Value v) {
    switch (v.type) {
    case TYPE_MODIFIER_POINTER: {
        auto t = Mem<Token *>(v.mem);
        global_print("%s*", t->text, t->lenght);
    } break;
    case TYPE_PRIMARY_CHAR:
        global_print("%c", Mem<char>(v.mem));
        break;
    case TYPE_PRIMARY_BOOL:
        global_print("%s", (Mem<bool>(v.mem) ? "true" : "false"));
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

void PrintTypeModifierExpression(TypeExpr* modifier, SymbolTable table) {

    while (modifier->index != TYPE_NON) {

        switch (modifier->index) {
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
            PrintExpr(*Mem<ArrayTypeExpr>(modifier).arraySizeExpr, table);
            global_print("%c", ']');
            modifier = *Mem<ArrayTypeExpr>(modifier).modifier;
            break;
        default:
            global_print("%s%\n", "unkonw type modifier");
            ASSERT(false);
        }
    }
}
void PrintTypeExpression(TypeExpr *expr, SymbolTable table) {

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
            auto &fn = Mem<FnTypeExpr>(expr);
            if (fn.modifier.index != TYPE_NON) {
                global_print("%s", "(fn(");
                for (u32 i = 0; i < fn.param_count; i++) {
                    PrintTypeExpression(*((*fn.params)[i]), table);
                    if (i != fn.param_count - 1) {
                        global_print("%s", ", ");
                    }
                }

                global_print("%s", ") ->  ");
                PrintTypeExpression(*fn.ret_t, table);
                global_print("%c", ')');
                PrintTypeModifierExpression(*fn.modifier, table);
            }
            else {
                global_print("%s", "fn(");
                for (u32 i = 0; i < fn.param_count; i++) {
                    PrintTypeExpression(*((*fn.params)[i]), table);
                    if (i != fn.param_count - 1) {
                        global_print("%s", ", ");
                    }
                }

                global_print("%s", ") -> ");
                PrintTypeExpression(*fn.ret_t, table);
            }
            return;
        }
        break;
    case TYPE_STRUCTURE: {
        auto &st = Mem<StructTypeExpr>(expr);
        global_print("%c", '(');
        if (st.memberCount) {
            auto members = *st.members;
            PrintTypeExpression(*(members[0]), table);
            for (u32 i = 1; i < st.memberCount; i++) {
                global_print("%s", ", ");
                PrintTypeExpression(*(members[i]), table);
            }
        }
        global_print("%c", ')');
        PrintTypeModifierExpression(*st.modifier, table);
        return;
    }
    default:
        ASSERT(false);
        break;
    }

    expr++;
    PrintTypeModifierExpression(expr, table);
}
void PrintExpr(Expr* expr, SymbolTable table) {

    switch (expr->index) {
    case EXPR_NULL:
        ASSERT(false);
        break;
    case EXPR_LITERAL:
        PrintValue(Mem<LiteralExpr>(expr).v);
        break;
    case EXPR_UNARY:
        global_print("%i", Mem<UnaryExpr>(expr).opr);
        PrintExpr(*Mem<UnaryExpr>(expr).e, table);
        break;
    case EXPR_BINARY:
        PrintExpr(*Mem<BinaryExpr>(expr).left, table);
        global_print("%c%i%c", ' ', Mem<BinaryExpr>(expr).opr, ' ');
        PrintExpr(*Mem<BinaryExpr>(expr).right, table);
        break;
    case EXPR_SYMBOL:
        {
            Mem<SymbolExpr>(expr).global;
            auto index = Mem<SymbolExpr>(expr).symbolIndex;
            auto t = table[index].name;
            global_print("%s*", t.text, t.lenght);
            break;
        }
    case EXPR_MEMORY_LOAD:
        global_print("%c", '*');
        PrintExpr(*Mem<MemoryLoadExpr>(expr).address, table);
        break;
    case EXPR_MEMORY_STORE:
        global_print("%c", '*');
        PrintExpr(*Mem<MemoryStoreExpr>(expr).address, table);
        global_print("%s", " = ");
        PrintExpr(*Mem<MemoryStoreExpr>(expr).value, table);
        break;
    case EXPR_MEMBER:
        {
            PrintExpr(*Mem<MemberExpr>(expr).prev, table);
            auto t = Mem<MemberExpr>(expr).name;
            global_print("%c%s*", '.', t.text, t.lenght);
        }
        break;
    case EXPR_CALL:
        {
            PrintExpr(*Mem<CallExpr>(expr).callee, table);
            auto call = Mem<CallExpr>(expr);
            global_print("%c", '(');
            for (u32 i = 0; i < call.argCount; i++) {
                PrintExpr(*call.args[i], table);
                global_print("%c", ' ');
            }
            global_print("%c", ')');
            break;
        }
    case EXPR_CONVERSION:
        {
            global_print("%c", '(');
            PrintTypeExpression(*Mem<ConversionExpr>(expr).to, table);
            global_print("%c", ')');
            global_print("%c", '(');
            PrintExpr(*Mem<ConversionExpr>(expr).from, table);
            global_print("%c", ')');
            break;
        }
    case EXPR_ADDR_OF:
        global_print("%c", '&');
        PrintExpr(*Mem<AddrOfExpr>(expr).e, table);
        break;
    case EXPR_PEEL_TYPE:
        PrintExpr(*Mem<PeelTypeExpr>(expr).e, table);
        break;
    }
}

Stmt* PrintStmt(Stmt* stmt, SymbolTable table) {

    switch (stmt->index) {
    case STATEMENT_NON:
        return nullptr;
    case STATEMENT_EXPRESSION: {
        auto &stmtExpr = Mem<StmtExpr>(stmt);
        if (stmtExpr.expr.index) {
            PrintExpr(*stmtExpr.expr, table);
            global_print("%s", ";\n");
        }
        return &stmtExpr + 1;
    }
    case STATEMENT_PRINT: {
        auto &print = Mem<StmtPrint>(stmt);
        global_print("%s", "print ");
        for (u32 i = 0; i < print.exprCount; i++) {
            PrintExpr(*print.exprString[i], table);
            global_print("%c", ' ');
        }
        global_print("%c", '\n');
        return &print + 1;
    }
    case STATEMENT_BRANCH: {
        auto &branch = Mem<StmtBranch>(stmt);
        global_print("%s", "if(");
        PrintExpr(*branch.cond, table);
        global_print("%s", ")\n");
        while (stmt = PrintStmt(*branch.thenBranch, table)) {
            global_print("%c", "\t");
        }
        if (branch.elseBranch.index) {
            global_print("%s", "else\n");
            while (stmt = PrintStmt(*branch.elseBranch, table)) {
                global_print("%c", "\t");
            }
        }
        return *branch.end;
    }
    case STATEMENT_FOR_LOOP: {
        auto &loop = Mem<StmtFor>(stmt);
        global_print("%s", "for(");
        while (stmt = PrintStmt(*loop.init, table));
        global_print("%c", ";");
        PrintExpr(*loop.cond, table);
        global_print("%c", ";");
        while (stmt = PrintStmt(*loop.inc, table));

        global_print("%s", ")\n");
        while (stmt = PrintStmt(*loop.body, table));
        return *loop.end;
    }
    case STATEMENT_RETURN:
    {
        auto &ret = Mem<StmtReturn>(stmt);
        global_print("%s", "return ");
        if (ret.expr.index) {
            PrintExpr(*ret.expr, table);
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
Value EvalOpr(Value v0, Value v1, u32 opr, u32 size) {

    switch (opr) {
    case TOKEN_RSHIFT_RSHIFT: {
        Value v1Same = ConvertValue(v1, TYPE_PRIMARY_INT64);
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<i8>(v0.mem) = Mem<i8>(v0.mem) >> Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<i16>(v0.mem) = Mem<i16>(v0.mem) >> Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<i32>(v0.mem) = Mem<i32>(v0.mem) >> Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<i64>(v0.mem) = Mem<i64>(v0.mem) >> Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<u8>(v0.mem) = Mem<u8>(v0.mem) >> Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<u16>(v0.mem) = Mem<u16>(v0.mem) >> Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<u32>(v0.mem) = Mem<u32>(v0.mem) >> Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_UINT64:
            Mem<u64>(v0.mem) = Mem<u64>(v0.mem) >> Mem<i64>(v1Same.mem);
            break;
        }
        break;
    }
    case TOKEN_LSHIFT_LSHIFT: {
        Value v1Same = ConvertValue(v1, TYPE_PRIMARY_INT64);
        switch (v0.type) {
        case TYPE_PRIMARY_INT8:
            Mem<i8>(v0.mem) = Mem<i8>(v0.mem) << Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_INT16:
            Mem<i16>(v0.mem) = Mem<i16>(v0.mem) << Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_INT32:
            Mem<i32>(v0.mem) = Mem<i32>(v0.mem) << Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_INT64:
            Mem<i64>(v0.mem) = Mem<i64>(v0.mem) << Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_UINT8:
            Mem<u8>(v0.mem) = Mem<u8>(v0.mem) << Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_UINT16:
            Mem<u16>(v0.mem) = Mem<i16>(v0.mem) << Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_UINT32:
            Mem<u32>(v0.mem) = Mem<i32>(v0.mem) << Mem<i64>(v1Same.mem);
            break;
        case TYPE_PRIMARY_UINT64:
            Mem<u64>(v0.mem) = Mem<i64>(v0.mem) << Mem<i64>(v1Same.mem);
            break;
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
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
            Mem<bool>(v0.mem) = Mem<char>(v0.mem) >= Mem<char>(v1.mem);
            break;
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
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
            Mem<bool>(v0.mem) = Mem<char>(v0.mem) != Mem<char>(v1.mem);
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
    case TOKEN_EXCLAMATION_MARK:
        Mem<bool>(v0.mem) = !Mem<bool>(v0.mem);
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


Symbol* AddFunctionSymbol(ParserState* ctx, LinearAllocator* alloc, const char* name, TypeExpr* type, void* fn) {

    ctx->globalCount++;
    auto symbol = MakeSymbolTableEntry(&ctx->table);
    symbol->scope = 0;
    symbol->name.type = TOKEN_IDENTIFIER;
    symbol->name.text = name;
    symbol->name.lenght = str_len(name) - 1;
    symbol->type = type;
    auto val = (Value*)linear_allocate(alloc, sizeof(Value));
    symbol->extra = val;
    Mem<void*>(val->mem) = fn;
    val->type = TYPE_PRIMARY_NATIVE_FN;

    return symbol;
}
Symbol* AddGlobalSymbol(ParserState* ctx, LinearAllocator* alloc, const char* name, TypeExpr* type, Value val) {
    
    ctx->globalCount++;
    auto symbol = MakeSymbolTableEntry(&ctx->table);
    symbol->scope = 0;
    symbol->name.type = TOKEN_IDENTIFIER;
    symbol->name.text = name;
    symbol->name.lenght = str_len(name) - 1;
    symbol->type = type;

    auto v = (Value*)linear_allocate(alloc, sizeof(Value));
    *v = val;
    symbol->extra = v;
    
    return symbol;
}


struct CompileConfig {
    bool optimize;
    bool dumpIR;
    f32 growthBudget;
};
CompileConfig ParseArgs(i32 argc, const char** args) {

    RUNTIME_CHECK(args);
    CompileConfig config{false,false,1.1f};
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
// --------------------------------- Parse END -----------------------------

u32 ParseFile(const char* file, LinearAllocator* exprMem, LinearAllocator* stmtMem, Stmt* program, byte* scratch) {

    char* text = (char*)linear_allocator_top(exprMem);
    u32 size = ReadFileTerminated(file, (byte*)text);
    auto tokenizer = MakeTokenizer(text);

    ParserState parser;
    parser.begin = text;
    parser.currentToken = 0;
    parser.stream = (Token*)scratch;
    
    u32 tokenCount = Tokenize(&tokenizer, parser.stream);

    parser.scope = 0;
    parser.typeTable.Init(exprMem, 150);
    parser.table.Init(exprMem, 150);
    parser.typeTable.size = 0;
    parser.globalCount = 0;
    parser.table.size = 0;

    u32 it = -1;
    while(parser.currentToken < tokenCount) {
        program[++it] = ParseStatement(&parser, exprMem, stmtMem, 0);
    }

    return it;
}
u32 ParseString(const char* str, LinearAllocator* exprMem, LinearAllocator* stmtMem, Stmt* program, byte* scratch) {

    auto tokenizer = MakeTokenizer(str);

    ParserState parser;
    parser.begin = (char*)str;
    parser.currentToken = 0;
    parser.stream = (Token*)scratch;
    
    u32 tokenCount = Tokenize(&tokenizer, parser.stream);

    parser.scope = 0;
    parser.typeTable.Init(exprMem, 150);
    parser.table.Init(exprMem, 150);
    parser.typeTable.size = 0;
    parser.globalCount = 0;
    parser.table.size = 0;

    u32 it = -1;
    while(parser.currentToken < tokenCount) {
        program[++it] = ParseStatement(&parser, exprMem, stmtMem, 0);
    }

    return it;
}

u32 Parse(ParserState* parser, LinearAllocator* exprMem, LinearAllocator* stmtMem, Stmt* result) {

    u32 it = -1;
    while(parser->currentToken < parser->tokenCount) {
        result[++it] = ParseStatement(parser, exprMem, stmtMem, 0);
    }
    return it;
}

// ------------------------------------------------ SSA BEGIN -----------------------------------
u64 HashSSA(void *compiler, u32 vReg);
bool EQSSAHelper(void *compiler, u32 k0, u32 k1);
bool EQSSA(void *compiler, u32 k0, u32 k1, u32 *visited);

enum SSAEnums : u32 {
    SSA_NONE,
    SSA_ANY,
    
    // ssa ops
    SSA_MEMORY_LOAD = EXPR_MEMORY_LOAD,
    SSA_MEMORY_STORE = EXPR_MEMORY_STORE,
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
    SSA_CONSTANT = EXPR_LITERAL,
    SSA_CALL = EXPR_CALL,
    SSA_CONVERT = EXPR_CONVERSION,
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
    u32 extraPtr;
    TypeExpr* type;
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

u64 HashSSA(void *user, u32 defPtr) {

}
bool EQSSAHelper(void *compiler, u32 k0, u32 k1) {

}

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

struct CompileContext {

    CoalescingLinearAllocator ptrHeap;
    LocalMallocState localHeap;
    MemoryPool<sizeof(SSABasicBlock)> ssaBlockPool;
    MemoryPool<sizeof(SSADefinition)> ssaDefPool;
    MemoryPool<sizeof(SSAMemoryDef)> memoryDefPool;
    LinearAllocator symbolicMem;

    DynamicBufferLocalMalloc<IncompleteMemoryOp> incompleteMemoryOps;
    DynamicBufferLocalMalloc<HashNode<VarDefintion*, u32>> basicBlockVarDefs;

    SymbolTable symbols;
    SSAFunction* fn;
};


void InsertCFGEdge(CompileContext* ctx, u32 pred, u32 succ) {

    bool found = false;
    SSABasicBlock* predBlock = (SSABasicBlock*)(ctx->ssaBlockPool.base + pred);
    SSABasicBlock* succBlock = (SSABasicBlock*)(ctx->ssaBlockPool.base + succ);
    
    for (u32 i = 0; i < predBlock->successors.edgeCount; i++) {
        u32 s = Mem<u32>(ctx->ptrHeap.base + predBlock->successors.edges + i * sizeof(u32));
        if (s == succ) {
            found = true;
        }
    }
    if (!found) {

        linear_free(&ctx->ptrHeap, ctx->ptrHeap.base + predBlock->successors.edges, predBlock->successors.edgeCount * sizeof(u32));
        u32* tmp = (u32*)linear_alloc(&ctx->ptrHeap, sizeof(u32) * (predBlock->successors.edgeCount + 1));
        u32 tmpPtr = (byte*)tmp - ctx->ptrHeap.base;
        tmp[predBlock->successors.edgeCount] = succ;

        memcpy(tmp, (ctx->ptrHeap.base + predBlock->successors.edges), sizeof(u32) * predBlock->successors.edgeCount);
        predBlock->successors.edgeCount++;
        predBlock->successors.edges = tmpPtr;
    }

    found = false;
    for (u32 i = 0; i < succBlock->predecessors.edgeCount; i++) {

        u32 s = Mem<u32>(ctx->ptrHeap.base + succBlock->predecessors.edges + i * sizeof(u32));
        if (s == pred) {
            found = true;
        }
    }

    if (!found) {

        linear_free(&ctx->ptrHeap, ctx->ptrHeap.base + succBlock->successors.edges, succBlock->successors.edgeCount * sizeof(u32));
        u32* tmp = (u32*)linear_alloc(&ctx->ptrHeap, sizeof(u32) * (succBlock->successors.edgeCount + 1));
        u32 tmpPtr = (byte*)tmp - ctx->ptrHeap.base;
        tmp[predBlock->successors.edgeCount] = pred;

        memcpy(tmp, (ctx->ptrHeap.base + succBlock->successors.edges), sizeof(u32) * succBlock->successors.edgeCount);
        succBlock->successors.edgeCount++;
        succBlock->successors.edges = tmpPtr;

    }
}
void RemoveCFGEdge(CompileContext* ctx, u32 pred, u32 succ) {

    SSABasicBlock* predBlock = (SSABasicBlock*)(ctx->ptrHeap.base + pred);
    SSABasicBlock* succBlock = (SSABasicBlock*)(ctx->ptrHeap.base + succ);

    u32 successorCount = predBlock->successors.edgeCount;
    u32* successors = (u32*)(ctx->ptrHeap.base + predBlock->successors.edges);
    for (u32 i = 0; i < predBlock->successors.edgeCount; i++) {
        if(succ == successors[i]) {
            successors[i] = successors[--predBlock->successors.edgeCount];
            break;
        }
    }
    linear_free(&ctx->ptrHeap, successors + predBlock->successors.edgeCount, (successorCount - predBlock->successors.edgeCount) * sizeof(u32));

    u32 predecessorCount = succBlock->predecessors.edgeCount;
    u32* predecessor = (u32*)(ctx->ptrHeap.base + succBlock->predecessors.edges);
    for (u32 i = 0; i < succBlock->predecessors.edgeCount; i++) {

        if(pred == predecessor[i]) {
            predecessor[i] = predecessor[--succBlock->predecessors.edgeCount];
            break;
        }
    }

    linear_free(&ctx->ptrHeap, predecessor + succBlock->predecessors.edgeCount, (predecessorCount - succBlock->predecessors.edgeCount) * sizeof(u32));
}
void CopyValues(CompileContext* ctx, SSABasicBlock* block) {

    ASSERT(ctx != nullptr && block != nullptr);
    u32 values[block->predecessors.edgeCount];
    for (u32 i = 0; i < block->predecessors.edgeCount; i++) {

        
        u32 predPtr = Mem<u32>(ctx->ptrHeap.base + block->predecessors.edges + i * sizeof(u32));
        SSABasicBlock* predBlock = (SSABasicBlock*)(ctx->ssaBlockPool.base + predPtr);

        for (u32 k = 0; k < predBlock->values.cap; k++) {
            if (predBlock->values.mem[k].key != ~u32(0)) {

                bool notFound = false;
                auto value = predBlock->values.mem[k].value;
                values[i] = value;

                for (u32 j = 0; j < block->predecessors.edgeCount; j++) {
                    if (j == i) continue;
                    u32 predPtrJ = Mem<u32>(ctx->ptrHeap.base + block->predecessors.edges + j * sizeof(u32));
                    SSABasicBlock* predBlockJ = (SSABasicBlock*)(ctx->ssaBlockPool.base + predPtrJ);

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
                    u32 index = block->values.Find(ctx, value);
                    if (index == ~u32(0)) {
                        block->values.Insert(ctx, value);
                    }
                }
            }
        }
    }

}
SSABasicBlock* MergeCF(CompileContext* ctx, SSAFunction* fn, CFGEdges pred, bool jmp) {

    if(pred.edgeCount == 1 && jmp == false) {

        return (SSABasicBlock*)(ctx->ssaBlockPool.base + Mem<u32>(ctx->ptrHeap.base + pred.edges));
    }
    else {

        auto merge = (SSABasicBlock*)pool_allocate(&ctx->ssaBlockPool);
        u32 mergePtr = (u32) ((byte*)merge - ctx->ssaBlockPool.base);
        *merge = {};
        merge->name = ctx->basicBlockVarDefs.PushBack(&ctx->localHeap, {nullptr, 0});
        fn->maxBlockName++;
        merge->hotness = 0.f;
        merge->nextBlock.opr = JMP;
        
        merge->values.Init();
        merge->memoryPtrs.Init();

        u32 predPtr = Mem<u32>(ctx->ptrHeap.base + pred.edges);
        SSABasicBlock* predBlock = (SSABasicBlock*)(ctx->ssaBlockPool.base + predPtr);

        for (u32 i = 0; i < pred.edgeCount; i++) {

            predPtr = Mem<u32>(ctx->ptrHeap.base + pred.edges + i * sizeof(u32));
            predBlock = (SSABasicBlock*)(ctx->ssaBlockPool.base + predPtr);
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
            InsertCFGEdge(ctx, predPtr, mergePtr);
        }

        CopyValues(ctx, merge);
        return merge;
    }
}
SSADefinition* AddPhiNode(CompileContext* ctx, SSABasicBlock* block, TypeExpr* type) {


    u32* tmp = (u32*)linear_alloc(&ctx->ptrHeap, sizeof(u32) * (block->phiCount + 1));
    u32 tmpPtr = (byte*)tmp - ctx->ptrHeap.base;

    auto phi = (SSADefinition*)pool_allocate(&ctx->ssaDefPool);
    tmp[block->phiCount] = (byte*)phi - ctx->ssaDefPool.base;
    
    *phi = {};
    phi->opr = SSA_PHI_NODE;
    phi->value = ctx->fn->maxSSAName++;
    phi->block = (byte*)block - ctx->ssaBlockPool.base;
    phi->type = type;

    memcpy(tmp, (u32*)(ctx->ptrHeap.base + block->phis), sizeof(u32) * block->phiCount);

    block->phis = tmpPtr;
    block->phiCount++;

    return phi;
}


void WriteSymbol(CompileContext* ctx, SSABasicBlock* block, Token symbol, u32 val) {

    if (val == ~u32(0)) return;

    VarDefintion* defs = ctx->basicBlockVarDefs[block->name].key;
    u32 defCount = ctx->basicBlockVarDefs[block->name].value;
    for (u32 i = 0; i < defCount; i++) {
        if (TokensEquals(defs[i].name, symbol)) {
            defs[i].defPtr = val;
            return;
        }
    }
    DefineSymbolInBlock(ctx, block, symbol, val);
}
u32 MakeUnkownSymbol(CompileContext* ctx, SSABasicBlock* block, Token symbol, TypeExpr* symbolType) {

    u32 defPtr;
    TypeName symbol_t = GetLastType(symbolType);

    if (symbol_t == TYPE_MODIFIER_ARRAY || symbol_t > TYPE_COUNT) {

        auto allocaSizeDef = (SSADefinition*)pool_allocate(&ctx->ssaDefPool);
        u32 allocaSizePtr = (byte*)allocaSizeDef - ctx->ssaDefPool.base;
        *allocaSizeDef = {};
        allocaSizeDef->opr = SSA_CONSTANT;

        auto v = (Value*)linear_alloc(&ctx->ptrHeap, sizeof(Value));
        allocaSizeDef->operand0 = (byte*)v - ctx->ptrHeap.base;
        
        Mem<u64>(v->mem) = GetTypeSize(symbolType);
        v->type = TYPE_PRIMARY_UINT64;

        allocaSizePtr = MakeDef(ctx, block, allocaSizePtr);
        auto allocaDef = (SSADefinition*)pool_allocate(&ctx->ssaDefPool);

        defPtr = (byte*)allocaDef - ctx->ssaDefPool.base;
        *allocaDef = {};
        allocaDef->opr = SSA_ALLOCA;
        allocaDef->operand0 = allocaSizePtr;
        auto tmp = (byte*)linear_allocator_top(&ctx->symbolicMem);
        auto exprsizeMemSize = CpyTypeExpr(tmp, symbolType) - tmp;
        auto dst = linear_alloc(&ctx->ptrHeap, exprsizeMemSize + sizeof(TypeName) * 2);
        memcpy(dst, tmp, exprsizeMemSize);
        Mem<TypeName>(dst + exprsizeMemSize - 4) = TYPE_MODIFIER_RESTRICTED_POINTER;
        Mem<TypeName>(dst + exprsizeMemSize) = TYPE_NON;

        allocaDef->type = (TypeExpr*)dst;
        defPtr = MakeDefWithSideEffects(ctx, block, defPtr);
    }
    else {

        auto unInit = (SSADefinition*)pool_allocate(&ctx->ssaDefPool);
        defPtr = (byte*)unInit - ctx->ssaDefPool.base;
        *unInit = {};
        unInit->opr = SSA_UN_INIT;
        unInit->type = symbolType;
        defPtr = MakeDef(ctx, block, defPtr);
    }

    WriteSymbol(ctx, block, symbol, defPtr);
    return defPtr;
}
void RemovePhiFromBlock(CompileContext* ctx, SSABasicBlock* block, u32 i) {

    //u32 last = Mem<u32>(context.compiler->mem + block->phis + (--block->phiCount) * sizeof(u32));
    //Mem<u32>(context.compiler->mem + block->phis + i * sizeof(u32)) = last;
}
u32 ReadSymbol(CompileContext* ctx, SSABasicBlock* block, Token symbol, TypeExpr* symbolType, byte* incompletes, u32* visited) {

    u32 visitedCount = visited[0];
    for (u32 i = 1; i < visitedCount + 1; i++) {
        if ((SSABasicBlock *)(ctx->ssaBlockPool.base + visited[i]) == block) {
            return ~u32(0);
        }
    }

    visited[++visited[0]] = (byte*)block - ctx->ssaBlockPool.base;
    VarDefintion* defs = ctx->basicBlockVarDefs[block->name].key;
    u32 defCount = ctx->basicBlockVarDefs[block->name].value;
    for (u32 i = 0; i < defCount; i++) {
        if (TokensEquals(defs[i].name, symbol)) {
            return defs[i].defPtr;
        }
    }
    
    auto lastType = GetLastType(symbolType);
    bool memoryType = (lastType > TYPE_COUNT || lastType == TYPE_MODIFIER_ARRAY);

    u32 ret = ~u32(0);
    if (block->incomplete && !memoryType) {

        auto incompletePhiCount = Mem<u32>(incompletes);
        auto incompletePhis = (IncompletePhi*)(incompletes + 4);
        auto phi = AddPhiNode(ctx, block, symbolType);

        ret = (byte*)phi - ctx->ssaDefPool.base;
        incompletePhis[incompletePhiCount].phi = block->phiCount - 1;
        incompletePhis[incompletePhiCount].symbol = symbol;
        incompletePhis[incompletePhiCount].symbolType = symbolType;
        incompletePhis[incompletePhiCount].block = block;
        Mem<u32>(incompletes) = incompletePhiCount + 1;

    }
    else if (block->predecessors.edgeCount == 1) {

        u32 predPtr = Mem<u32>(ctx->ptrHeap.base + block->predecessors.edges);
        auto predBlock = (SSABasicBlock*)(ctx->ssaBlockPool.base + predPtr);

        ret = ReadSymbol(ctx, predBlock, symbol, symbolType, incompletes, visited);
    }
    else if (block->predecessors.edgeCount == 0) {
        ret = MakeUnkownSymbol(ctx, block, symbol, symbolType);
    }
    else {

        u32 phiIndex = block->phiCount;
        auto witness = AddPhiNode(ctx, block, symbolType);
        u32 witnessPtr = (byte*)witness - ctx->ssaDefPool.base;
        WriteSymbol(ctx, block, symbol, witnessPtr);

        const u32 predCount = block->predecessors.edgeCount;
        u32 symbolDefs[predCount];
        for (u32 i = 0; i < predCount; i++) {

            u32 predBlockPtr = Mem<u32>(ctx->ptrHeap.base + block->predecessors.edges + i * sizeof(u32));
            auto predBlock = (SSABasicBlock*)(ctx->ssaBlockPool.base + predBlockPtr);

            visited[visited[0] + 1] = 0;
            symbolDefs[i] = ReadSymbol(ctx, predBlock, symbol, symbolType, incompletes, visited + visited[0] + 1);
        }

        RemovePhiFromBlock(ctx, block, phiIndex);
    }
    /*

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
    */
}

u32 SSAExpr(CompileContext* ctx, SSABasicBlock* block, Expr* e, byte* IncompletePhiBuffer, u32* visited) {

    ASSERT(e->index);
    switch(e->index) {
    case EXPR_LITERAL:
        {
            auto exp = (LiteralExpr*)e;
            auto def = (SSADefinition*)pool_allocate(&ctx->ssaDefPool);
            u32 defPtr = (byte*)def - ctx->ssaDefPool.base;
            *def = {};
            def->opr = SSA_CONSTANT;

            auto v = (Value*)local_malloc(&ctx->localHeap, sizeof(Value));
            def->operand0 = (byte*)v - (byte*)ctx->localHeap.headBlock;
            *v = exp->v;

            return MakeDef(ctx, block, defPtr);
        }
    case EXPR_UNARY:
        {

            auto node = (UnaryExpr*)e;
            u32 primaryDefPtr = SSAExpr(ctx, block, *node->e, IncompletePhiBuffer, visited);

            auto def = (SSADefinition*)pool_allocate(&ctx->ssaDefPool);
            u32 defPtr = (byte*)def - ctx->ssaDefPool.base;

            *def = {};
            def->opr = node->opr;
            def->operand0 = primaryDefPtr;
            def->operand1 = ~u32(0);

            SSADefinition* primary = (SSADefinition*)(ctx->ssaDefPool.base + primaryDefPtr);
            def->type = primary->type;

            return MakeDef(ctx, block, defPtr);
        }
    case EXPR_BINARY:
        {
            auto node = (BinaryExpr*)e;
            
            //byte* scratch = GetEndOfIncompletePhiBuffer(IncompletePhiBuffer);
            //DeSugarSubScriptDef(context.compiler, node, scratch);

            u32 leftDefPtr = SSAExpr(ctx, block, *node->left, IncompletePhiBuffer, visited);
            u32 rightDefPtr = SSAExpr(ctx, block, *node->right, IncompletePhiBuffer, visited);

            auto binaryDef = (SSADefinition*)pool_allocate(&ctx->ssaDefPool);
            u32 binaryDefPtr = (byte*)binaryDef - ctx->ssaDefPool.base;

            *binaryDef = {};
            binaryDef->opr = (node->opr == TOKEN_SUB_SCRIPT_OPR ? TOKEN_PLUS : node->opr);
            binaryDef->operand0 = leftDefPtr;
            binaryDef->operand1 = rightDefPtr;

            u32 size = GetTypeExpr(e, (byte*)visited, ctx->symbols) - (byte*)visited;
            binaryDef->type = (TypeExpr*)local_malloc(&ctx->localHeap, size);
            memcpy(binaryDef->type, visited, size);

            return MakeDef(ctx, block, binaryDefPtr);
        }
    case EXPR_SYMBOL:
        {

            auto expr = (SymbolExpr*)e;
            auto symbol_t = GetLastType(ctx->symbols[expr->symbolIndex].type);
            auto symbol = ctx->symbols[expr->symbolIndex];

            u32 defPtr;
            if(symbol_t == TYPE_PRIMARY_FN) {

                auto functionName = (SSADefinition*)pool_allocate(&ctx->ssaDefPool);
                defPtr = (byte*)functionName - ctx->ssaDefPool.base;

                *functionName = {};
                functionName->opr = SSA_FUNCTION;
                u32 index = FindSymbol(ctx->symbols, symbol.name);
                ASSERT(index != ~u32(0));
                functionName->operand0 = (byte*)ctx->symbols[index].extra - ctx->ssaBlockPool.base;

                defPtr = MakeDef(ctx, block, defPtr);
            }
            else {
                visited[0] = 0;
                //defPtr = ReadSymbol(context, block, node->var.name, node->var.type, IncompletePhiBuffer, visited);
                //WriteSymbol(context, block, node->var.name, defPtr);
            }
            return defPtr;
        }
    }
}

u32 MakeDef(CompileContext* ctx, SSABasicBlock* block, u32 defPtr) {

    SSADefinition* def = (SSADefinition*)(ctx->ssaDefPool.base + defPtr);
    def->value = ctx->fn->maxSSAName++;
    PushBackDef(ctx, block, defPtr);
    return defPtr;
}
void PushBackDefIntoBlock(CompileContext* ctx, SSABasicBlock* block, u32 defPtr) {

    SSADefinition* def = (SSADefinition*)(ctx->ssaDefPool.base + defPtr);
    if (block->firstDef == 0) {

        block->firstDef = defPtr;
        block->lastDef = defPtr;
        def->prevDef = 0;
        def->nextDef = 0;
    }
    else {
        ASSERT(block->lastDef);
        Mem<SSADefinition>(ctx->ssaDefPool.base + block->lastDef).nextDef = defPtr;
        def->prevDef = block->lastDef;
        block->lastDef = defPtr;
        def->nextDef = 0;
    }

    def->block = (byte*)block - ctx->ssaBlockPool.base;
}
void PushBackDef(CompileContext* ctx, SSABasicBlock* block, u32 defPtr) {

    PushBackDefIntoBlock(ctx, block, defPtr);
    SSADefinition* def = (SSADefinition*)(ctx->ssaDefPool.base + defPtr);
    def->block = (byte*)block - ctx->ssaBlockPool.base;
    u32 index = block->values.Find(ctx, defPtr);
    if(index == ~u32()) block->values.Insert(ctx, defPtr);
}

u32 MakeDefWithSideEffects(CompileContext* ctx, SSABasicBlock* block, u32 defPtr) {

    SSADefinition* def = (SSADefinition*)(ctx->ssaDefPool.base + defPtr);
    def->value = ctx->fn->maxSSAName++;
    PushBackDef(ctx, block, defPtr);

    return defPtr;
}
void DefineSymbolInBlock(CompileContext* ctx, SSABasicBlock* block, Token symbol, u32 defPtr) {

    VarDefintion* defs = ctx->basicBlockVarDefs[block->name].key;
    u32 count = ctx->basicBlockVarDefs[block->name].value;

    SSADefinition* def = (SSADefinition*)(ctx->ssaDefPool.base + defPtr);
    VarDefintion* tmp = (VarDefintion*)LOG(global_malloc_debug(sizeof(VarDefintion) * (count + 1)));
    memcpy(tmp, defs, sizeof(VarDefintion) * count);
    tmp[count].name = symbol;
    tmp[count].defPtr = defPtr;

    LOG(global_free_debug(defs));
    ctx->basicBlockVarDefs[block->name].key = tmp;
    ctx->basicBlockVarDefs[block->name].value++;
}
u32 GetReturnBlocksFromFunction(CompileContext* ctx, SSABasicBlock* entry, bool* visited, u32* result) {

    u32 ret = 0;
    if(entry->successors.edgeCount == 0 || entry->nextBlock.opr == RET) {
        result[ret++] = (byte*)entry - ctx->ssaBlockPool.base;
    }

    for (u32 i = 0; i < entry->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(ctx->ptrHeap.base + entry->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(ctx->ssaBlockPool.base + succBlockPtr);

        if(visited[succBlock->name] == true) continue;
        visited[succBlock->name] = true;   
        ret += GetReturnBlocksFromFunction(ctx, succBlock, visited, result+ret);
    }
    return ret;
}
void CompletePhis(CompileContext* context, SSABasicBlock* loopHeaderBlock, byte* incompletePhiBuffer, u32* visitMem) {}

CFGEdges CreateCDFG(CompileContext* ctx, Stmt* program, CFGEdges pred, bool jmp, byte* mem, u32* visitMem) {

    ASSERT(mem != nullptr);
    ASSERT(program != nullptr);

    u32 t = program->index;
    switch (t) {
    case STATEMENT_NON:
        return pred;
    case STATEMENT_FN_END:
        {
            program++;
            auto merge = MergeCF(ctx, ctx->fn, pred, true);
            merge->nextBlock.opr = RET;
            merge->nextBlock.ret.retDef = ~u32(0);
            CFGEdges edges;
            edges.edgeCount = 1;
            u32* tmp = (u32*)linear_alloc(&ctx->ptrHeap, sizeof(u32));
            edges.edges = (byte*)tmp - ctx->ptrHeap.base;
            *tmp = (byte*)merge - ctx->ssaBlockPool.base;
            return edges;   
        }
    case STATEMENT_FN_BEGIN:
        {
            auto fnDecl = (StmtDecl*)(program + 1);
            program = fnDecl + 1;
            
            auto symbol = MakeSymbolTableEntry(&ctx->symbols);
            CopySymbol(symbol, &fnDecl->s);

            auto entry = (SSABasicBlock*)pool_allocate(&ctx->ssaBlockPool);
            u32 entryPtr = (byte*)entry - ctx->ssaBlockPool.base;

            auto s = MakeSymbolTableEntry(&ctx->symbols);
            CopySymbol(s, &fnDecl->s);
            s->extra = ctx->fn;

            *entry = {};
            entry->hotness = 0.f;
            entry->name = ctx->basicBlockVarDefs.PushBack(&ctx->localHeap, {nullptr, 0});
            ctx->fn->maxBlockName++;
            entry->values.Init();

            entry->memoryPtrs.Init();
            entry->predecessors = pred;

            u32 paramCount = GetParamCountType(fnDecl->s.type);
            ctx->fn->params.Init(paramCount);
            ctx->fn->entry = entry;

            auto params = (ParamList*)fnDecl->extra;
            auto fnType = (FnTypeExpr*)fnDecl->s.type;
            for (u32 i = 0; i < paramCount; i++) {

                auto paramDef = (SSADefinition*)pool_allocate(&ctx->ssaDefPool);
                *paramDef = {};
                paramDef->opr = SSA_FN_PARAMETER;
                paramDef->type = *((*fnType->params)[i]);

                u32 paramValPtr = (byte*)paramDef - ctx->ssaDefPool.base;
                MakeDefWithSideEffects(ctx, entry, paramValPtr);
                DefineSymbolInBlock(ctx, entry, params->names[i], paramValPtr);
                ctx->fn->params[i].ssaDef = paramValPtr;
                ctx->fn->params[i].clobbered = {};
                ctx->fn->params[i].observed = {};
            }

            CFGEdges entryPred;
            entryPred.edgeCount = 1;
            u32* edges = (u32*)linear_alloc(&ctx->ptrHeap, sizeof(u32));
            *edges = entryPtr;
            entryPred.edges = (byte*)edges- ctx->ptrHeap.base;

            CreateCDFG(ctx, program, entryPred, jmp, mem, visitMem);
            CFGEdges retBlocks;
            memset(visitMem, 0, ctx->basicBlockVarDefs.size);

            retBlocks.edgeCount = GetReturnBlocksFromFunction(ctx, entry, (bool*)visitMem, (u32*)mem);

            u32* tmp = (u32*)linear_alloc(&ctx->ptrHeap, retBlocks.edgeCount * sizeof(u32));
            retBlocks.edges = (byte*)tmp - ctx->ptrHeap.base;
            memcpy(tmp, mem, retBlocks.edgeCount * sizeof(u32));

            ctx->fn->outBlocks = retBlocks;
            return retBlocks;
        }
    case STATEMENT_EXPRESSION:
        {
            auto stmt = (StmtExpr*)program;
            program = stmt + 1;
            auto merge = MergeCF(ctx, ctx->fn, pred, jmp);
            CFGEdges edges;
            edges.edgeCount = 1;

            u32* tmp = (u32*)linear_alloc(&ctx->ptrHeap, sizeof(u32));
            edges.edges = (byte*)tmp - ctx->ptrHeap.base;

            SSAExpr(ctx, merge, *stmt->expr, mem, visitMem);
            return CreateCDFG(ctx, program, edges, false, mem, visitMem);
        }
    case STATEMENT_BRANCH:
        {
            auto stmt = (StmtBranch*)program;
            program = stmt + 1;

            CFGEdges thenPred{};
            CFGEdges elsePred{};
            CFGEdges endPred{};

            SSABasicBlock* thenBlock{};
            SSABasicBlock* condBlock{};
            u32 thenBlockPtr{};
            u32 condBlockPtr{};

            condBlock = MergeCF(ctx, ctx->fn, pred, jmp);
            condBlockPtr = (byte*)condBlock - ctx->ssaBlockPool.base;
            condBlock->nextBlock.opr = BRANCH;
            condBlock->nextBlock.branch.conditionDef = SSAExpr(ctx, condBlock, *stmt->cond, mem, visitMem);

            thenBlock = (SSABasicBlock*)pool_allocate(&ctx->ssaBlockPool);
            thenBlockPtr = (byte*)thenBlock - ctx->ssaBlockPool.base;
            condBlock->nextBlock.branch.thenBlock = thenBlockPtr;
            condBlock->nextBlock.branch.elseBlock = 0;

            *thenBlock = {};
            thenBlock->hotness = 0.f;
            thenBlock->name = ctx->basicBlockVarDefs.PushBack(&ctx->localHeap, {nullptr, 0});
            ctx->fn->maxBlockName++;
            thenBlock->values.CopyInit(&condBlock->values);

            thenBlock->memoryPtrs.CopyInit(&condBlock->memoryPtrs);

            thenBlock->nextBlock.opr = JMP;
            InsertCFGEdge(ctx, condBlockPtr, thenBlockPtr);

            thenPred.edgeCount = 1;
            thenPred.edges = (byte*)linear_alloc(&ctx->ptrHeap, sizeof(u32)) - ctx->ptrHeap.base;
            Mem<u32>(ctx->ptrHeap.base + thenPred.edges) = thenBlockPtr;
            
            endPred = CreateCDFG(ctx, *stmt->thenBranch, thenPred, false, mem, visitMem);

            if (stmt->elseBranch.index != 0) {

                auto elseBlock = (SSABasicBlock*)pool_allocate(&ctx->ssaBlockPool);
                u32 elseBlockPtr = (byte*)elseBlock - ctx->ssaBlockPool.base;

                condBlock->nextBlock.branch.elseBlock = elseBlockPtr;

                *elseBlock = {};
                elseBlock->hotness = 0.f;
                elseBlock->name = ctx->basicBlockVarDefs.PushBack(&ctx->localHeap, {nullptr, 0});
                ctx->fn->maxBlockName++;
                elseBlock->nextBlock.opr = JMP;
                elseBlock->values.CopyInit(&condBlock->values);
                elseBlock->memoryPtrs.CopyInit(&condBlock->memoryPtrs);

                InsertCFGEdge(ctx, condBlockPtr, elseBlockPtr);

                elsePred.edgeCount = 1;
                elsePred.edges = (byte*)linear_alloc(&ctx->ptrHeap, sizeof(u32)) - ctx->ptrHeap.base;
                Mem<u32>(ctx->ptrHeap.base + elsePred.edges) = elseBlockPtr;

                elsePred = CreateCDFG(ctx, *stmt->elseBranch, elsePred, false, mem, visitMem);

                u32* tmp = (u32*)linear_alloc(&ctx->ptrHeap, sizeof(u32) * (endPred.edgeCount + elsePred.edgeCount));
                u32 tmpPtr = (byte*)tmp - ctx->ptrHeap.base;

                memcpy(tmp, ctx->ptrHeap.base + endPred.edges, sizeof(u32) * endPred.edgeCount);
                memcpy(tmp + endPred.edgeCount, ctx->ptrHeap.base + elsePred.edges, sizeof(u32) * elsePred.edgeCount);

                endPred.edgeCount = endPred.edgeCount + elsePred.edgeCount;
                endPred.edges = tmpPtr;
            }
            else {

                u32* tmp = (u32*)linear_alloc(&ctx->ptrHeap, sizeof(u32) * (endPred.edgeCount + 1));
                u32 tmpPtr = (byte*)tmp - ctx->ptrHeap.base;
                tmp[0] = condBlockPtr;

                memcpy(tmp + 1, ctx->ptrHeap.base + endPred.edges, sizeof(u32) * endPred.edgeCount);
                endPred.edgeCount++;

                endPred.edges = tmpPtr;
            }

            return CreateCDFG(ctx, *stmt->end, endPred, false, mem, visitMem);
        }
    case STATEMENT_FOR_LOOP:
        {
        
            auto stmt = (StmtFor*)program;
            program = stmt + 1;

            u32 condBlockPtr;
            SSABasicBlock *condBlock;
            SSABasicBlock *initBlock;
            CFGEdges initEdges;

            {
                initBlock = MergeCF(ctx, ctx->fn, pred, jmp);
                u32 initBlockPtr = (byte*)initBlock - ctx->ssaBlockPool.base;
                initBlock->nextBlock.opr = JMP;
                CFGEdges initPred;
                initPred.edgeCount = 1;
                initPred.edges = (byte*)linear_alloc(&ctx->ptrHeap, sizeof(u32)) - ctx->ptrHeap.base;
                Mem<u32>(ctx->ptrHeap.base + initPred.edges) = initBlockPtr;

                initEdges = CreateCDFG(ctx, *stmt->init, initPred, false, mem, visitMem);
            }
            condBlock = MergeCF(ctx, ctx->fn, initEdges, true);
            condBlockPtr = (byte*)condBlock - ctx->ssaBlockPool.base;
            condBlock->incomplete = true;
            condBlock->nextBlock.opr = BRANCH;
            condBlock->nextBlock.branch.elseBlock = 0;
            condBlock->nextBlock.branch.conditionDef = SSAExpr(ctx, condBlock, *stmt->cond, mem, visitMem);

            auto bodyBlock = (SSABasicBlock*)pool_allocate(&ctx->ssaBlockPool);
            const u32 bodyBlockPtr = (byte*)bodyBlock - ctx->ssaBlockPool.base;

            condBlock->nextBlock.branch.thenBlock = bodyBlockPtr;
            *bodyBlock = {};
            bodyBlock->hotness = 0.f;
            bodyBlock->name = ctx->basicBlockVarDefs.PushBack(&ctx->localHeap, {nullptr, 0});
            ctx->fn->maxBlockName++;
            bodyBlock->values.CopyInit(&condBlock->values);
            bodyBlock->memoryPtrs.CopyInit(&condBlock->memoryPtrs);
            bodyBlock->nextBlock.opr = JMP;
            InsertCFGEdge(ctx, condBlockPtr, bodyBlockPtr);

            CFGEdges bodyPred;
            bodyPred.edgeCount = 1;
            bodyPred.edges = (byte*)linear_alloc(&ctx->ptrHeap, sizeof(u32)) - ctx->ptrHeap.base;
            Mem<u32>(ctx->ptrHeap.base + bodyPred.edges) = bodyBlockPtr;

            CFGEdges incPred = CreateCDFG(ctx, *stmt->body, bodyPred, false, mem, visitMem);

            bool jmp = false;
            if (incPred.edgeCount == 1) {

                u32 incPredBlockPtr = Mem<u32>(ctx->ptrHeap.base + incPred.edges);
                SSABasicBlock *incPredBlock = (SSABasicBlock *)(ctx->ssaBlockPool.base + incPredBlockPtr);
                if (incPredBlock != condBlock && incPredBlock->nextBlock.opr == BRANCH) {
                    jmp = true;
                }
            }

            CFGEdges condPred = CreateCDFG(ctx, *stmt->inc, incPred, jmp, mem, visitMem);

            condBlock->incomplete = false;
            for (u32 i = 0; i < condPred.edgeCount; i++) {
                u32 condPredPtr = Mem<u32>(ctx->ptrHeap.base + condPred.edges + i * sizeof(u32));
                SSABasicBlock* condPredBlock = (SSABasicBlock*)(ctx->ssaBlockPool.base + condPredPtr);

                if(condPredBlock->nextBlock.opr == RET) continue;
                if(condPredBlock->nextBlock.opr == JMP) {
                    condPredBlock->nextBlock.jmp.targetBlock = condBlockPtr;
                }
                InsertCFGEdge(ctx, Mem<u32>(ctx->ptrHeap.base + condPred.edges + i * sizeof(u32)), condBlockPtr);
            }

            CompletePhis(ctx, condBlock, mem, (u32*)visitMem);

            CFGEdges endPred;
            endPred.edgeCount = 1;
            endPred.edges = (byte*)linear_alloc(&ctx->ptrHeap, sizeof(u32)) - ctx->ptrHeap.base;
            Mem<u32>(ctx->ptrHeap.base + endPred.edges) = condBlockPtr;

            CFGEdges endP = CreateCDFG(ctx, *stmt->end, endPred, true, mem, visitMem);

            return endP;
        }
    case STATEMENT_DECLARE:
        {
            auto decl = (StmtDecl*)program;
            decl->s.type;
        }
    }
}

void CompileASTProgram(CompileContext* ctx, Stmt* program, u32 treeCount) {
}

i32 main(i32 argc, const char** args) {

    auto compilerMemory = init_global_state(Megabyte(48), Megabyte(64), 512);
    auto config = ParseArgs(argc, args);

    ASSERT(argc > 1);
    auto exprAlloc = make_linear_allocator(compilerMemory, Megabyte(8));
    compilerMemory += Megabyte(8);
    auto stmtAlloc = make_linear_allocator(compilerMemory, Megabyte(8));
    compilerMemory += Megabyte(8);

    Stmt trees[150];
    u32 treeCount = ParseFile(args[0], &exprAlloc, &stmtAlloc, trees, compilerMemory);


    global_io_flush();
}