#include <cstring>
#include <cstdlib>

#include <algorithm>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <iterator>
#include <string>
#include <vector>

using namespace std;

#include <benchmark/benchmark.h>

using namespace benchmark;

#include "sa.hpp"

static inline string
abbr (const string& arg)
{
    string text = arg.substr (0, 4);

    if (arg.size () > 4)
        text += "...";

    return text;
}

static void
BM_sa (benchmark::State& state, const string& text) {
    while (state.KeepRunning ())
        benchmark::DoNotOptimize (ka::make_sa (text));
}

int
main (int argc, char** argv)
{
    using namespace benchmark;

    string text;
    cin >> text;

    text.append (1U, char (0));

    stringstream ss;
    ss << "BM_sa/" << abbr (text) << "/" << (text.size () - 1);

    RegisterBenchmark (ss.str ().c_str (), &BM_sa, text);

    Initialize (&argc, argv);
    RunSpecifiedBenchmarks ();

    return 0;
}
