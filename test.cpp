#include <fstream>
#include <iostream>

using namespace std;

#include "sa.hpp"

static void
test (const string& text) {
    const volatile auto ignore = ka::make_sa (text);
}

static void
test (const string& filename, long n) {
    ifstream f (filename);

    string text;
    f >> text;

    if (0 < n && n < text.size ())
        text.resize (n);

    text.append (1U, char (0));

    test (text);
}

int
main (int argc, char** argv) {
    if (argv [1] && argv [1][0]) {
        long n = -1;

        if (argv [2] && argv [2][0])
            n = atol (argv [2]);

        test (argv [1], n);
    }

    return 0;
}
