#include <iostream>
#include <chrono>
#include <thread>

unsigned long int GetIntFromToken(char* str, int len) {
    
    unsigned long int r = 0;
    int e = 1;
    for (int i = len - 1; i > -1; i--) {
        r += (str[i] - '0') * e;
        e *= 10;
    }
    return r;
}
unsigned int str_len(const char *str) {
    const char* it = str;
    while(*it++);
    return it - str;
}
bool str_cmp(const char *str0, const char *str1) {
    for(;;) {
        if(!*str0) break;
        if(!*str1) break;
        if(*str0++ != *str1++) return false;
    }
    
    return *str0 == *str1;
}
constexpr unsigned long int MICRO_SEC       = 1;
constexpr unsigned long int MILI_SEC        = 1000 * MICRO_SEC;
constexpr unsigned long int SECOND          = 1000 * MILI_SEC;
constexpr unsigned long int MINUTE          = 60 * SECOND;

int main(int argc, char* argv[]) {

    if(argc != 3) return 1;
    unsigned long int amount = GetIntFromToken(argv[1], str_len(argv[1]) - 1);
    
    if(str_cmp(argv[2], "s")) {
        amount *= SECOND;
    }
    else if(str_cmp(argv[2], "m")) {
        amount *= MINUTE;
    }
    else {
        return 1;
    }

    std::this_thread::sleep_for(std::chrono::microseconds(amount));
    system("/usr/bin/notify-send MessageSubject \"time end\"");

    return 1;
}