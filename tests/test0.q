fn fibRec(i64 n) -> i64 {
    if(n <= 1) return n;
    return fibRec(n-1) + fibRec(n-2);
}

main {

    print fibRec(40);
    //print "recursive fibonnaci test with 35\n";
    //print "should result in 9227465 ",fibRec(35)," \n";
}