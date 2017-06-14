#include <cstring>
#include <cstdlib>

#include <algorithm>
#include <iostream>
#include <iomanip>
#include <iterator>
#include <string>
#include <vector>

using namespace std;

#include "sa.hpp"

const vector< string > test_data {
    "ABCABXABCD",
    "MISSISSIPPI",
    "ABABBAAAAAAAAAAAAAAAAAAAAABABABABBBBBBBBBBBBABABBBBBBBBBBBBBBAABA",
    "ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJ"
    "HKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEF"
    "GHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTF"
    "ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJ"
    "HKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEF"
    "GHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTF"
    "ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJ"
    "HKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEF"
    "GHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTF"
    "ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJ"
    "HKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEF"
    "GHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTF"
    "ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJ"
    "HKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEF"
    "GHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTF"
    "ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJ"
    "HKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEF"
    "GHEADGBKJFLKJNLKNBJHKFJYTFKHLYGKUYTFK1ABCCDEFGHEADGBKJFLKJNLKNBJHKFJYTF"
};

static vector< int >
make_sa_ref (const string& s) {
    int N = s.size (), i, gap;
    vector< int > sa (N, 0), pos (N, 0), tmp (N, 0);

    const auto cmp = [&](int i, int j) {
        if (pos [i] != pos [j])
            return pos [i] < pos [j];

        i += gap;
        j += gap;

        return (i < N && j < N) ? pos [i] < pos [j] : i > j;
    };

#define REP(i, n) for(int i = 0; i < n; ++i)

    REP (i, N) sa [i] = i, pos [i] = s [i];

    for (gap = 1;; gap <<= 1) {
        sort (begin (sa), begin (sa) + N, cmp);
        REP (i, N - 1) tmp [i + 1] = tmp [i] + cmp (sa [i], sa [i + 1]);
        REP (i, N) pos [sa [i]] = tmp [i];

        if (tmp [N - 1] == N - 1)
            break;
    }

#undef REP

    return vector< int > (begin (sa), begin (sa) + N);
}

static void
test (string text) {
    const auto sa1 = make_sa_ref (text);

    text.append (1U, char (0));
    const auto sa2 = ka::make_sa (text);

    bool b = sa1.size () + 1 == sa2.size () && equal (
        begin (sa1), end (sa1), ++begin (sa2));

    cout << (b ? "." : "FAIL");
}

int
main () {
    for (const auto& text : test_data) {
        test (text);

        string rtext (text);
        reverse (begin (rtext), end (rtext));

        test (rtext);
    }

    cout << endl;

    return 0;
}
