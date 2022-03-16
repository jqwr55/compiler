fn fibIter(i64 n) -> i64 {
    i64 prevPrev = 0;
    i64 prev = 0;
    i64 curr = 1;

    for(i64 i = 1; i < n; ++i;) {

        prevPrev = prev;
        prev = curr;
        curr = prevPrev + prev;
    }

    return curr;
}

main {

    print "iterative fibonnaci test with 35\n";
    print "should result in 9227465 ",fibIter(350000000000)," \n";
}