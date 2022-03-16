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
Tokenizer MakeTokenizer(char *source) {
    return {source, 1};
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

struct Expr {
    i32 index;
    void operator=(Expr *addr) {
        index = (addr == nullptr ? 0 : (i32)((byte *)addr - (byte *)this));
    }
    void operator=(Expr e) = delete;
    Expr *operator*() {
        ASSERT(index != 0);
        return (Expr *)((byte *)this + index);
    }
    Expr *operator->() {
        ASSERT(index != 0);
        return (Expr *)((byte *)this + index);
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


typedef StaticBufferLocal<TypeSymbol, LinearAllocator,linear_allocate,roll_back_linear_allocator> TypeSymbolTable;
typedef StaticBufferLocal<Symbol, LinearAllocator,linear_allocate,roll_back_linear_allocator> SymbolTable;
struct ParserState {
    const char *begin;
    Token *stream;
    TypeSymbolTable typeTable;
    SymbolTable table;
    
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
byte *CpyTypeExpr(byte *dst, TypeExpr *srcExpr);
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
Expr *ParseAssignment(ParserState *ctx, LinearAllocator *alloc);
TypeExpr *ParseTypeModifierExpression(ParserState *ctx, LinearAllocator *alloc) {

    TypeExpr *expr = (TypeExpr *)linear_allocator_top(alloc);
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
        case 0: {
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

            if (t != TYPE_MODIFIER_ARRAY && t < TYPE_COUNT) {
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
        case 1: {
            auto top = (TypeExpr *)linear_allocator_top(alloc);
            GetTypeExpr(expr, (byte *)top, ctx->table);
            TypeName t = GetLastType(top);
            Expect(t == TYPE_MODIFIER_ARRAY || t == TYPE_MODIFIER_POINTER || t == TYPE_MODIFIER_RESTRICTED_POINTER, "operator ([]) expects pointer or array types");
            TypeExpr *peeled = GetNthType(top, 1);
            t = Mem<TypeName>(peeled);

            auto tmp = linear_allocator_top(alloc);
            if (t != TYPE_MODIFIER_ARRAY && t < TYPE_COUNT) {

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
        } break;
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

    STATEMENT_SCOPE_ENTRY,
    STATEMENT_SCOPE_EXIT,
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
struct ExecutionContext {
    char *dir;
    Stmt *currentStmt;
    LocalMallocState localHeap;
    SymbolTable table;
    TypeSymbolTable typeTable;
    u32 currentScope;
    u32 globalCount;
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
void PurgeSymbols(ExecutionContext* ctx, u32 scope) {

    for (u32 i = 0; i < ctx->table.size; i++) {
        if (scope < ctx->table[i].scope) {
            auto q = ctx->table[--ctx->table.size];
            local_free(&ctx->localHeap, ctx->table[i].extra);
            ctx->table[i].type  = q.type;
            ctx->table[i].extra = q.extra;
            ctx->table[i].name  = q.name;
            ctx->table[i].scope = q.scope;
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
            auto fnBody = (StmtDecl*)linear_allocate(stmtAlloc, sizeof(StmtDecl));
            fnBody->index = STATEMENT_DECLARE;

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
            auto params = *fn_t->params;
            for(u32 k = 0; k < i; k++) {
                auto arg = MakeSymbolTableEntry(&ctx->table);
                arg->extra = 0;
                arg->name = ctx->stream[argNames[k]];
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

            TypeExpr *m[255];
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
        /*
    case STATEMENT_FN_BODY:
        {
            auto &fn = Mem<StmtFnBody>(stmt);
            PrintToken(table.symbols[fn.symbolIndex].name);
            auto &q = table.symbols[fn.symbolIndex];
            ASSERT((*table.symbols[fn.symbolIndex].type)->index == TYPE_PRIMARY_FN);
            auto fn_t = (FnTypeExpr *)*table.symbols[fn.symbolIndex].type;
            global_print("%c", '(');
            for (u32 i = 0; i < fn.argCount; i++) {
                PrintTypeExpression(*((*fn_t->params)[i]));
                global_print("%c%s*%c", ' ', fn.args[i].text, fn.args[i].lenght, ' ');
            }
            global_print("%s", ")\n\t");
            stmt = *Mem<Stmt>(&q.extra);
            while (stmt = PrintStmt(stmt, table)) {
                global_print("%c", '\t');
            }
            return nullptr;
        }
        break;
        */
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

Value EvalExpr(ExecutionContext *ctx, Expr *e);
u32 GetTypeSize(ExecutionContext *ctx, TypeExpr *type) {

    u32 ret = 0;
    while (type->index) {

        switch (type->index) {
        case TYPE_PRIMARY_BOOL:
        case TYPE_PRIMARY_CHAR:
        case TYPE_PRIMARY_INT8:
        case TYPE_PRIMARY_UINT8:
            ret++;
            type++;
            break;
        case TYPE_PRIMARY_INT16:
        case TYPE_PRIMARY_UINT16:
            ret += 2;
            type++;
            break;
        case TYPE_PRIMARY_INT32:
        case TYPE_PRIMARY_UINT32:
        case TYPE_PRIMARY_F32:
            ret += 4;
            type++;
            break;

        case TYPE_PRIMARY_UINT64:
        case TYPE_PRIMARY_INT64:
        case TYPE_PRIMARY_F64:
        case TYPE_MODIFIER_RESTRICTED_POINTER:
        case TYPE_MODIFIER_POINTER:
            ret += 8;
            type++;
            break;
        case TYPE_PRIMARY_FN:
        case TYPE_PRIMARY_NATIVE_FN:
            ret += 8;
            type = *Mem<FnTypeExpr>(type).modifier;
            break;

        case TYPE_MODIFIER_ARRAY: {
            auto &arr = Mem<ArrayTypeExpr>(type);
            ret *= Mem<u64>(EvalExpr(ctx, *arr.arraySizeExpr).mem);
            type = *arr.modifier;
        } break;

        case TYPE_MODIFIER_CONST:
        case TYPE_MODIFIER_VOLATILE:
        case TYPE_MODIFIER_ATOMIC:
            type++;
        }
    }
    return ret;
}

Value CallNative(ExecutionContext* ctx, Value callee, CallExpr* call) {

    typedef Value(*NativeFn)(ExecutionContext* ctx, u32 argCount, Value* args);
    auto fn = Mem<NativeFn>(callee.mem);
    Value args[call->argCount];
    for(u32 i = 0; i < call->argCount; i++) {
        args[i] = EvalExpr(ctx, *call->args[i]);
    }
    return fn(ctx, call->argCount, args);
}

void Execute(ExecutionContext* ctx);
Value EvalExpr(ExecutionContext* ctx, Expr* e) {

    switch (e->index) {
    case EXPR_NULL:
        ASSERT(false);
        break;
    case EXPR_LITERAL:
        return Mem<LiteralExpr>(e).v;
    case EXPR_UNARY: 
        {
            auto v = EvalExpr(ctx, *Mem<UnaryExpr>(e).e);
            return EvalOpr(v, {}, Mem<UnaryExpr>(e).opr, 3);
        }
    case EXPR_BINARY:
        {
            auto l = *Mem<BinaryExpr>(e).left;
            auto r = *Mem<BinaryExpr>(e).right;
            auto opr = Mem<BinaryExpr>(e).opr;
            auto v0 = EvalExpr(ctx, l);
            auto v1 = EvalExpr(ctx, r);
            if (opr == TOKEN_EQUAL_SIGN) {
                auto& symbol = Mem<SymbolExpr>(l);
                auto index = symbol.symbolIndex;
                index = symbol.global ? symbol.symbolIndex : (ctx->globalCount) + symbol.symbolIndex;
                if (index == ~u32(0)) {
                    global_print("s", "undefinde symbol\n");
                }
                Mem<Value>(ctx->table[index].extra) = v1;
                return v1;
            }
            else if (opr == TOKEN_SUB_SCRIPT_OPR) {
                byte local[1000];
                GetTypeExpr(l, local, ctx->table);
                auto nth = GetNthType((TypeExpr *)local, 0);
                nth->index = 0;
                auto s = GetTypeSize(ctx, (TypeExpr *)local);

                Value scale;
                Mem<i64>(scale.mem) = s;
                scale.type = TYPE_PRIMARY_INT64;
                v1 = EvalOpr(scale, v1, TOKEN_ASTERISK, 3);
                auto r = EvalOpr(v0, v1, TOKEN_PLUS, 1);
                return r;
            }
            return EvalOpr(v0, v1, opr, 3);
        }
    case EXPR_SYMBOL:
        {
            auto& expr = Mem<SymbolExpr>(e);
            u32 index = expr.global ? expr.symbolIndex : (ctx->globalCount) + expr.symbolIndex;

            if(!ctx->table[index].extra) {
                auto s = GetTypeSize(ctx, ctx->table[index].type);
                Value* v;
                if (s <= 8) {
                    v = (Value*)local_malloc(&ctx->localHeap, sizeof(Value));
                }
                else {
                    v = (Value*)local_malloc(&ctx->localHeap, s + sizeof(Value));
                    Mem<void*>(v->mem) = v + 1;
                    v->type = TYPE_MODIFIER_POINTER;
                }
                ctx->table[index].extra = v;
            }
            return Mem<Value>(ctx->table[index].extra);
        }
        break;
    case EXPR_MEMORY_LOAD:
        {
            auto addE = *Mem<MemoryLoadExpr>(e).address;
            auto addr = EvalExpr(ctx, addE);
            ASSERT(addr.type == TYPE_MODIFIER_POINTER);
            byte mem[1000];
            Value r;
            GetTypeExpr(addE, mem, ctx->table);
            auto nth = GetNthType((TypeExpr*)mem, 0);
            nth->index = 0;
            auto t = GetLastType((TypeExpr*)mem);
            r.type = t;

            memcpy(r.mem, Mem<void*>(addr.mem), sizeof(byte[8]));
            return r;
        } 
        break;
    case EXPR_MEMORY_STORE: 
        {
            auto addE = *Mem<MemoryStoreExpr>(e).address;
            auto valE = *Mem<MemoryStoreExpr>(e).value;
            auto addr = EvalExpr(ctx, addE);
            auto val = EvalExpr(ctx, valE);
            ASSERT(addr.type == TYPE_MODIFIER_POINTER);
            memcpy(Mem<void *>(addr.mem), val.mem, sizeof(byte[8]));
            return val;
        }
        break;
    case EXPR_MEMBER:
        {
            auto prevE = *Mem<MemberExpr>(e).prev;
            auto off = Mem<MemberExpr>(e).offset;
            auto addr = EvalExpr(ctx, prevE);
            ASSERT(addr.type == TYPE_MODIFIER_POINTER);

            byte local[1000];
            GetTypeExpr(e, local, ctx->table);
            auto last = GetLastType((TypeExpr *)local);
            Value r;
            if (last != TYPE_MODIFIER_ARRAY && last > TYPE_COUNT) {
                r.type = TYPE_MODIFIER_POINTER;
                Mem<byte *>(r.mem) = Mem<byte *>(addr.mem) + off;
            }
            else {
                r.type = last;
                memcpy(r.mem, (Mem<byte *>(addr.mem) + off), sizeof(byte[8]));
            }
            return r;
        }
    case EXPR_CALL:
        {
            auto &call = Mem<CallExpr>(e);
            Value args[call.argCount];
            auto callee = EvalExpr(ctx, *call.callee);
            if(callee.type == TYPE_PRIMARY_NATIVE_FN) {
                return CallNative(ctx, callee, &call);
            }

            ASSERT(callee.type == TYPE_MODIFIER_POINTER || callee.type == TYPE_PRIMARY_NATIVE_FN);
            auto entry = Mem<Stmt*>(callee.mem);

            byte localMem[500 * sizeof(Symbol)];
            auto tableSave = ctx->table;

            SymbolTable table;
            table.memory = (Symbol*)localMem;
            table.size = 0;

            Value retV;
            auto retSymbol = MakeSymbolTableEntry(&table);
            retSymbol->extra = &retV;
            for (u32 i = 1; i < tableSave.size; i++) {
                if (tableSave[i].scope < 1) {
                    CopySymbol(MakeSymbolTableEntry(&table), tableSave.memory + i);
                }
            }
            for (u32 i = 0; i < call.argCount; i++) {
                args[i] = EvalExpr(ctx, *(call.args[i]));
                auto arg = MakeSymbolTableEntry(&table);
                arg->extra = args + i;
            }

            ctx->table = table;
            auto save = ctx->currentStmt;
            ctx->currentStmt = entry;
            Execute(ctx);
            ctx->table = tableSave;
            ctx->currentStmt = save;
            return Mem<Value>(retSymbol->extra);
        }
        break;
    case EXPR_CONVERSION:
        {
            auto &conv = Mem<ConversionExpr>(e);
            auto v = EvalExpr(ctx, *conv.from);
            auto t = GetLastType(*conv.to);
            return ConvertValue(v, t);
        }
        break;
    case EXPR_ADDR_OF:
        {
            auto &addr = Mem<AddrOfExpr>(e);
            Value v;
            v.type = TYPE_MODIFIER_POINTER;
            switch ((*addr.e)->index) {
            case EXPR_MEMORY_LOAD:
            case EXPR_MEMORY_STORE:
                v = EvalExpr(ctx, *Mem<MemoryLoadExpr>(*addr.e).address);
                break;
            case EXPR_SYMBOL:
                Mem<void *>(v.mem) = ctx->table[Mem<SymbolExpr>(*addr.e).symbolIndex].extra;
                break;
            case EXPR_MEMBER:
                v = EvalExpr(ctx, *Mem<MemoryLoadExpr>(*addr.e).address);
                Mem<byte *>(v.mem) += Mem<MemberExpr>(*addr.e).offset;
                break;
            default:
                return EvalExpr(ctx, *addr.e);
            }
            return v;
        }
        break;
    case EXPR_PEEL_TYPE:
        return EvalExpr(ctx, *Mem<PeelTypeExpr>(e).e);
    }
}

void Execute(ExecutionContext* ctx) {

    begin_fn:
    switch (ctx->currentStmt->index) {
    case STATEMENT_NON:
        return;
    case STATEMENT_DECLARE:
        {
            auto& decl = Mem<StmtDecl>(ctx->currentStmt);
            auto symbol = MakeSymbolTableEntry(&ctx->table);
            CopySymbol(symbol, &decl.s);
            ctx->currentStmt = &decl + 1;
        }
        break;
    case STATEMENT_EXPRESSION:
        {
            auto &exprStmt = Mem<StmtExpr>(ctx->currentStmt);
            EvalExpr(ctx, *exprStmt.expr);
            ctx->currentStmt = &exprStmt + 1;
        }
        break;
    case STATEMENT_PRINT:
        {
            auto &prinStmt = Mem<StmtPrint>(ctx->currentStmt);
            for (u32 i = 0; i < prinStmt.exprCount; i++) {
                PrintValue(EvalExpr(ctx, *prinStmt.exprString[i]));
            }

            ctx->currentStmt = (Stmt*)(prinStmt.exprString + prinStmt.exprCount);
        }
        break;
    case STATEMENT_BRANCH:
        {
            auto &branch = Mem<StmtBranch>(ctx->currentStmt);
            ctx->currentScope++;
            auto cond = Mem<bool>(EvalExpr(ctx, *branch.cond).mem);
            if (cond) {
                ctx->currentStmt = *branch.thenBranch;
            }
            else {
                ctx->currentStmt = *branch.elseBranch;
            }
            Execute(ctx);
            ctx->currentScope--;
            PurgeSymbols(ctx, ctx->currentScope);
            ctx->currentStmt =  *branch.end;
        }
        break;
    case STATEMENT_FOR_LOOP:
        {
            auto &loop = Mem<StmtFor>(ctx->currentStmt);
            ctx->currentScope++;
            ctx->currentStmt = *loop.init;
            Execute(ctx);
            bool cond = Mem<bool>(EvalExpr(ctx, *loop.cond).mem) && loop.body.index;
            while(cond) {
                ctx->currentStmt = *loop.body;
                Execute(ctx);
                ctx->currentStmt = *loop.inc;
                Execute(ctx);
                cond = Mem<bool>(EvalExpr(ctx, *loop.cond).mem);
            }
            ctx->currentScope--;
            PurgeSymbols(ctx, ctx->currentScope);
            ctx->currentStmt = *loop.end;
        }
        break;
    case STATEMENT_RETURN:
        Mem<Value>(ctx->table[0].extra) = EvalExpr(ctx, *Mem<StmtReturn>(ctx->currentStmt).expr);
        return;
    case STATEMENT_BREAK:
    case STATEMENT_NEXT: 
        ASSERT(false);
        break;
    }
    goto begin_fn;
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

Value BuiltinMalloc(ExecutionContext* ctx, u32 argCount, Value* args) {

    u64 size = Mem<u64>(args[0].mem);
    ASSERT(args[0].type == TYPE_PRIMARY_UINT64);
    auto ret = local_malloc(&ctx->localHeap, sizeof(i64) * size);
    memset(ret, 0, size);

    Value v;
    Mem<void*>(v.mem) = ret;
    v.type = TYPE_MODIFIER_POINTER;
    return v;
}
Value BuiltinFree(ExecutionContext* ctx, u32 argCount, Value* args) {

    ASSERT(args[0].type == TYPE_MODIFIER_POINTER);
    void* mem = Mem<void*>(args[0].mem);

    for(u32 i = 0; i < 4; i++) {
        global_print("i, c", ((i64*)mem)[i], '\n');
    }
    global_io_flush();
    local_free(&ctx->localHeap, mem);

    return {};
}

Value BuiltinCD(ExecutionContext* ctx, u32 argCount, Value* args) {

    ASSERT(argCount == 1);
    ASSERT(args[0].type == TYPE_MODIFIER_POINTER);
    auto dir = *Mem<Token*>(args[0].mem);

    auto senitnelChar = dir.text[dir.lenght];
    Mem<char>((char*)dir.text + dir.lenght) = 0;
    chdir(dir.text);
    Mem<char>((char*)dir.text + dir.lenght) = senitnelChar;

    local_free(&ctx->localHeap, ctx->dir);

    char* path = (char*)local_max_malloc(&ctx->localHeap);
    auto allocSize = local_malloc_allocation_size(path);
    getcwd(path, allocSize);
    auto len = str_len(path);
    local_malloc_shrink(&ctx->localHeap, path, len);

    ctx->dir = path;

    u32 index = FindSymbol(ctx->table, Token{"current_dir", sizeof("current_dir")-1, TOKEN_EOF});
    auto t = Mem<Token*>(Mem<Value>(ctx->table[index].extra).mem);
    t->text = path;
    t->lenght = len;

    return {};
}

TypeExpr* MakeNativeFnType(LinearAllocator* alloc, u32 typeCount, TypeExpr** types) {

    auto fnType = (FnTypeExpr*)linear_allocate(alloc, sizeof(FnTypeExpr));
    fnType->index = TYPE_PRIMARY_NATIVE_FN;
    fnType->param_count = typeCount - 1;
    fnType->ret_t = types[0];

    auto params = (TypeExpr*)linear_allocate(alloc, sizeof(TypeExpr) * fnType->param_count);
    fnType->params = params;
    for(u32 i = 1; i < typeCount; i++) {
        params[i] = types[i];
    }

    auto mod_t = (TypeExpr*)linear_allocate(alloc, sizeof(TypeExpr));
    mod_t->index = TYPE_NON;
    fnType->modifier = mod_t;

    return fnType;
}


template<typename T>
TypeName GetBasicTypeName() {}
template<> TypeName GetBasicTypeName<i8>()   {return TYPE_PRIMARY_INT8;}
template<> TypeName GetBasicTypeName<i16>()  {return TYPE_PRIMARY_INT16;}
template<> TypeName GetBasicTypeName<i32>()  {return TYPE_PRIMARY_INT32;}
template<> TypeName GetBasicTypeName<i64>()  {return TYPE_PRIMARY_INT64;}
template<> TypeName GetBasicTypeName<u8>()   {return TYPE_PRIMARY_UINT8;}
template<> TypeName GetBasicTypeName<u16>()  {return TYPE_PRIMARY_UINT16;}
template<> TypeName GetBasicTypeName<u32>()  {return TYPE_PRIMARY_UINT32;}
template<> TypeName GetBasicTypeName<u64>()  {return TYPE_PRIMARY_UINT64;}
template<> TypeName GetBasicTypeName<f32>()  {return TYPE_PRIMARY_F32;}
template<> TypeName GetBasicTypeName<f64>()  {return TYPE_PRIMARY_F64;}
template<> TypeName GetBasicTypeName<bool>() {return TYPE_PRIMARY_BOOL;}
template<> TypeName GetBasicTypeName<void>() {return TYPE_PRIMARY_VOID;}
template<> TypeName GetBasicTypeName<char>() {return TYPE_PRIMARY_CHAR;}

template<typename T>
struct PeelPointer { typedef T type; };
template<typename T>
struct PeelPointer<T*> { typedef T type; };


template<typename T>
TypeExpr* GenerateType(LinearAllocator* alloc) {

    if constexpr (std::is_array<T>::value) {
      
        T arrType;

        auto arr = (ArrayTypeExpr*)linear_allocate(alloc, sizeof(ArrayTypeExpr));
        arr->index = TYPE_MODIFIER_ARRAY;

        auto size = (LiteralExpr*)linear_allocate(alloc, sizeof(LiteralExpr));
        size->index = EXPR_LITERAL;
        size->v.type = TYPE_PRIMARY_INT64;
        Mem<i64>(size->v.mem) = SIZE_OF_ARRAY(arrType);

        arr->arraySizeExpr = size;
        arr->modifier = (TypeExpr*)linear_allocator_top(alloc);

        auto ptr = arrType;
        typedef typename PeelPointer<decltype(ptr)>::type element_t;
        GenerateType<element_t>(alloc);

        Mem<TypeName>(linear_allocate(alloc, sizeof(TypeName))) = TYPE_NON;
        return arr;
    }
    else if constexpr (std::is_pointer<T>::value) {

        typedef typename PeelPointer<T>::type peeled_t;
        auto type = GenerateType<peeled_t>(alloc);

        auto mod = (TypeName*)linear_allocate(alloc, sizeof(TypeName));
        mod[0] = TYPE_MODIFIER_POINTER;
        mod[0] = TYPE_NON;
        return type;
    }
    else {
        auto type = (TypeName*)linear_allocate(alloc, sizeof(TypeName) * 2);
        type[0] = GetBasicTypeName<T>();
        type[1] = TYPE_NON;
        return (TypeExpr*)type;


    }
}



i32 main(i32 argc, const char** args) {


    u32 size;
    auto compilerMemory = init_global_state(Megabyte(16), Megabyte(64), 512);

    ASSERT(argc > 1);
    auto text = (char*)ReadFileTerminated(args[1], compilerMemory, &size);
    auto tokenizer = MakeTokenizer(text);

    auto rem = Megabyte(48) - size;
    auto linExpr = make_linear_allocator(compilerMemory + size, rem / 2);
    auto linStmt = make_linear_allocator(compilerMemory + size + (rem / 2), rem / 2);
    auto end = compilerMemory + size + rem;
    auto endAlloc = make_linear_allocator(end, Megabyte(64) - rem);

    ParserState ctx;
    ctx.begin = text;
    ctx.currentToken = 0;
    ctx.stream = (Token*)end;

    
    u32 tokenCount = Tokenize(&tokenizer, ctx.stream);

    ctx.scope = 0;
    ctx.typeTable.Init(&endAlloc, 150);
    ctx.table.Init(&endAlloc, 150);
    ctx.typeTable.size = 0;
    ctx.globalCount = 0;
    ctx.table.size = 0;


    {
        auto mallocTypes = (TypeExpr*)linear_allocate(&linExpr, sizeof(TypeExpr) * 5);
        mallocTypes[0].index = TYPE_PRIMARY_VOID;
        mallocTypes[1].index = TYPE_MODIFIER_POINTER;
        mallocTypes[2].index = TYPE_NON;
        mallocTypes[3].index = TYPE_PRIMARY_UINT64;
        mallocTypes[4].index = TYPE_NON;

        TypeExpr* types[2] = {mallocTypes, mallocTypes + 3};
        auto fnType = MakeNativeFnType(&linExpr, 2, types);
        AddFunctionSymbol(&ctx, &linExpr, "malloc", fnType, (void*)BuiltinMalloc);
    }
    {
        auto ret_t = (TypeExpr*)linear_allocate(&linExpr, sizeof(TypeExpr) * 5);
        ret_t[0].index = TYPE_PRIMARY_VOID;
        ret_t[1].index = TYPE_NON;
        ret_t[2].index = TYPE_PRIMARY_VOID;
        ret_t[3].index = TYPE_MODIFIER_POINTER;
        ret_t[4].index = TYPE_NON;

        TypeExpr* types[2] = {ret_t, ret_t + 2};
        auto fnType = MakeNativeFnType(&linExpr, 2, types);
        AddFunctionSymbol(&ctx, &linExpr, "free", fnType, (void*)BuiltinFree);
    }

    {
        auto cd_t = (TypeExpr*)linear_allocate(&linExpr, sizeof(TypeExpr) * 5);
        cd_t[0].index = TYPE_PRIMARY_VOID;
        cd_t[1].index = TYPE_NON;
        cd_t[2].index = TYPE_PRIMARY_CHAR;
        cd_t[3].index = TYPE_MODIFIER_POINTER;
        cd_t[4].index = TYPE_NON;

        TypeExpr* cdArgTypes[2] = {cd_t, cd_t + 2};
        auto cdType = MakeNativeFnType(&linExpr, 2, cdArgTypes);
        AddFunctionSymbol(&ctx, &linExpr, "change_dir", cdType, (void*)BuiltinCD);
    }
    {
        auto dirType = (TypeExpr*)linear_allocate(&linExpr, sizeof(TypeExpr) * 3);
        dirType[0].index = TYPE_PRIMARY_CHAR;
        dirType[1].index = TYPE_MODIFIER_POINTER;
        dirType[2].index = TYPE_NON;
        Value string;
        string.type = TYPE_MODIFIER_POINTER;

        auto dirToken = (Token*)linear_allocate(&linExpr, sizeof(Token));
        dirToken->text = args[1];
        dirToken->lenght = str_len(args[1]) - 1;
        dirToken->type = TOKEN_STRING_LITERAL;
        Mem<Token*>(string.mem) = dirToken;
        AddGlobalSymbol(&ctx, &linExpr, "current_dir", dirType, string);
    }

    u32 it = -1;
    Stmt* program[150];
    while(ctx.currentToken < tokenCount) {
        program[++it] = ParseStatement(&ctx, &linExpr, &linStmt, 0);
    }

    ExecutionContext ctxE;
    ctxE.currentScope = 1;

    ctxE.table = ctx.table;
    ctxE.typeTable = ctx.typeTable;
    ctxE.globalCount = ctx.table.size;
    ctxE.localHeap = make_local_malloc((byte*)global_malloc(Megabyte(15)), Megabyte(15));
    
    auto len = str_len(args[1]);
    ctxE.dir = (char*)local_malloc(&ctxE.localHeap, len);
    memcpy(ctxE.dir, args[1], len);

    for(u32 i = 0; i < ctx.table.size; i++) {
        if(TokenEquals(ctx.table[i].name, "main")) {
            ctxE.currentStmt = (Stmt*)ctx.table[i].extra;
        }
    }

    Execute(&ctxE);

    global_io_flush();
}