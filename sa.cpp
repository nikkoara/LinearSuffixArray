#include <cassert>
#include <cstring>

#include <algorithm>
#include <functional>
#include <iostream>
#include <iterator>
#include <limits>
#include <vector>

using namespace std;

namespace ka {
namespace detail {

constexpr auto npos = (numeric_limits< size_t >::max) ();

template< typename T >
inline auto size_cast (T value)
{
    return size_t (make_unsigned_t< T > (value));
}

template< typename ForwardIterator >
inline tuple <
    typename iterator_traits< ForwardIterator >::value_type,
    typename iterator_traits< ForwardIterator >::value_type >
minmax (ForwardIterator first, ForwardIterator last)
{
    using value_type = typename iterator_traits< ForwardIterator >::value_type;

    auto max_ = (numeric_limits< value_type >::min) ();
    auto min_ = (numeric_limits< value_type >::max) ();

    for (; first != last; ++first) {
        const auto c = *first;

        if (c < min_) {
            min_ = c;
        }

        if (c > max_) {
            max_ = c;
        }
    }

    return  { min_, max_ };
}

template< typename ForwardIterator >
tuple< vector< size_t >, vector< bool > >
semisort1 (ForwardIterator first, ForwardIterator last)
{
    using value_type = typename iterator_traits< ForwardIterator >::value_type;

    auto hi = (numeric_limits< value_type >::min) ();
    auto lo = (numeric_limits< value_type >::max) ();

    size_t n = 0;

    for (auto iter = first; iter != last; ++iter, ++n) {
        const auto value = *iter;

        if (value < lo) {
            lo = value;
        }

        if (value > hi) {
            hi = value;
        }
    }

    const size_t off = 0 - lo;

    vector< size_t > hist (hi - lo + 1, 0);

    for (auto iter = first; iter != last; ++iter) {
        ++hist [*iter + off];
    }

    size_t accum = 0;

    for (auto iter = hist.begin (); iter != hist.end (); ++iter) {
        if (const auto tmp = *iter) {
            *iter = accum;
            accum += tmp;
        }
    }

    vector< bool > b (n, false);
    b.back () = true;

    for (auto iter = hist.begin (); iter != hist.end (); ++iter)
        if (const auto i = *iter) {
            b [i - 1] = true;
        }

    vector< size_t > B (n, 0);

    for (size_t i = 0; first != last; ++first, ++i) {
        B [hist [*first + off]++] = i;
    }

    return { B, b };
}

template< typename T >
tuple< vector< bool >, size_t >
make_S_array (const T& text)
{
    size_t c [2] = { 0 };

    vector< bool > S (text.size ());

    for (size_t i = 0, j, n = text.size () - 1; i < n;) {
        for (j = i; j < n && text [j] == text [j + 1]; ++j) ;

        assert (j < n);
        const auto b = text [j] < text [j + 1];

        for (; i <= j; ++i) {
            S [i] = b;
            ++c [size_t (b)];
        }
    }

    S.back () = c [1] < c [0];
    ++c [size_t (c [1] < c [0])];

    return { S, c [size_t (S.back ())] };
}

template< typename ForwardIterator >
inline size_t
max_S_distance (
    ForwardIterator iter, ForwardIterator last,
    typename iterator_traits< ForwardIterator >::value_type S)
{
    size_t max_ = 0, accum = 0;

    for (; iter != last; ++iter) {
        const auto value = *iter;

        if (S == value) {
            if (accum > max_) {
                max_ = accum;
            }

            accum = 1;
        }
        else {
            ++accum;
        }
    }

    if (accum > max_) {
        max_ = accum;
    }

    return max_;
}

static vector< size_t >
make_T_ (
    const vector< size_t >& B,
    const vector< bool >& b,
    const vector< bool >& S)
{
    vector< size_t > buckets (S.size (), 0);

    for (size_t i = 0, c = 0; i < B.size (); c += b [i++]) {
        buckets [B [i]] = c;
    }

    vector< size_t > T_;

    for (size_t i = 0; i < S.size (); ++i) {
        if (S [i] == S.back ()) {
            T_.emplace_back (buckets [i]);
        }
    }

    return T_;
}

static tuple< vector< size_t >, vector< bool > >
make_substrings_array (
    const vector< size_t >& A,
    const vector< bool >& a,
    const vector< bool >& S,
    size_t n)
{
    vector< size_t > B (n, 0);
    vector< bool > b (n, false);

    for (size_t i = 0, j = 0; i < S.size (); ++i) {
        if (S [A [i]] == S.back ()) {
            B [j++] = A [i];
        }

        if (a [i] && j) {
            b [j - 1] = true;
        }
    }

    return { B, b };
}

static tuple< vector< size_t >, vector< size_t > >
S_distance (const vector< bool >& S)
{
    const auto type = S.back ();

    size_t i = 0, accum = 0, max_ = 0;

    //
    // Skip suffixes of different type:
    //
    for (; i < S.size () && S [i] != type; ++i) ;

    vector< size_t > sdist (S.size (), 0);

    for (++i; i < S.size (); ++i) {
        sdist [i] = ++accum;

        if (max_ < sdist [i]) {
            max_ = sdist [i];
        }

        if (S [i] == type) {
            accum = 0;
        }
    }

    vector< size_t > cdist (max_ + 1, 0);

    for (const auto d : sdist) {
        ++cdist [d];
    }

    cdist [0] = 0;

    for (size_t i = 1; i < cdist.size (); ++i) {
        cdist [i] += cdist [i - 1];
    }

    return { sdist, cdist };
}

static tuple< vector< size_t >, vector< bool > >
make_bucket_list (
    vector< size_t >& A, const vector< bool >& a, const vector< bool >& S)
{
    vector< size_t > sdist, cdist;
    tie (sdist, cdist) = S_distance (S);

    const auto n = cdist.back ();

    vector< size_t > M (n, 0);
    vector< bool > m (n, false);

    for (size_t i = 0, j = 0, from = i, to; j < A.size (); from = i) {
        for (; j < A.size () && !a [j]; ++j) ;

        for (to = ++j; i < j; ++i) {
            if (const int tmp = sdist [A [i]]) {
                size_t pos = cdist [tmp - 1]++;

                sdist [A [i]] = pos;
                m [pos] = true;
            }
            else {
                sdist [A [i]] = npos;
            }
        }

        for (; from < to; ++from) {
            const auto pos = sdist [A [from]];

            if (pos != npos && pos < int (n) - 1) {
                if (m [pos + 1]) {
                    m [pos] = false;
                }
            }
        }
    }

    for (size_t i = 0; i < A.size (); ++i)
        if (sdist [i] != npos) {
            A [sdist [i]] = i;
        }

    for (size_t i = 0, j = 0; i < cdist.size () - 1; ++i)
        for (; j < cdist [i]; ++j) {
            M [j] = A [j] - i - 1;
        }

    for (size_t i = 0; i < cdist.size () - 1; ++i) {
        m [cdist [i] - 1] = true;
    }

    return { M, m };
}

static void
bucketsort_S_substrings (
    vector< size_t >& B, vector< bool >& b,
    const vector< bool >& S,
    const vector< size_t >& M, const vector< bool >& m)
{
    vector< size_t > Rev (S.size (), npos);
    vector< size_t > L (B.size (), npos);

    size_t r = B.size () - 1;

    for (size_t i = B.size () - 1; i; --i) {
        Rev [B [i]] = r;

        if (b [i - 1] == 1) {
            L [r] = i;
            r = i - 1;
        }
    }

    Rev [B [0]] = r;
    L [r] = 0;

    for (size_t i = 0, j = 0; i < M.size (); i = j) {
        do {
            ++L [Rev [M [j]]];
        } while (m [j++] == 0);

        j = i;

        do {
            Rev [M [j]] = L [Rev [M [j]]] - 1;
        } while (m [j++] == 0);

        j = i;

        do {
            const auto n = Rev [M [j]];

            if (L [n] == npos) {
                L [n] = n;
            }
            else {
                --L [n];
            }

            b [n] = true;
        } while (m [j++] == 0);
    }

    for (size_t i = 0; i < S.size (); ++i) {
        const auto bucket = Rev [i];

        if (bucket != npos) {
            B [L [bucket]++] = i;
        }
    }
}

static void
bucketsort_L_substrings (
    vector< size_t >& B, vector< bool >& b,
    const vector< bool >& S,
    const vector< size_t >& M, const vector< bool >& m)
{
    vector< size_t > Rev (S.size (), npos);
    vector< size_t > R (B.size (), npos);

    for (size_t i = 0, l = 0; i < B.size (); ++i) {
        Rev [B [i]] = l;

        if (b [i] == 1) {
            R [l] = i;
            l = i + 1;
        }
    }

    for (size_t i = 0, j = 0; i < M.size (); i = j) {
        do {
            --R [Rev [M [j]]];
        } while (m [j++] == 0);

        j = i;

        do {
            Rev [M [j]] = R [Rev [M [j]]] + 1;
        } while (m [j++] == 0);

        j = i;

        do {
            const auto n = Rev [M [j]];

            if (R [n] == npos) {
                R [n] = n;
            }
            else {
                ++R [n];
            }

            if (n) {
                b [n - 1] = true;
            }
        } while (m [j++] == 0);
    }

    for (size_t i = 0; i < S.size (); ++i) {
        const auto n = Rev [i];

        if (n != npos) {
            B [R [n]--] = i;
        }
    }
}

template< typename T >
tuple< vector< size_t >, vector< bool >, vector< bool > >
bucketsort_substrings (const T& text)
{
    size_t n;
    vector< bool > S;

    tie (S, n) = make_S_array (text);

    vector< size_t > A, B, M;
    vector< bool > a, b, m;

    tie (A, a) = semisort1 (text.begin (), text.end ());
    tie (B, b) = make_substrings_array (A, a, S, n);

    if (!S.back ()) {
        reverse (A.begin (), A.end ());
        reverse (a.begin (), a.end ());

        copy (a.begin () + 1, a.end (), a.begin ());
    }

    tie (M, m) = make_bucket_list (A, a, S);

    if (S.back ())
        bucketsort_S_substrings (B, b, S, M, m);
    else
        bucketsort_L_substrings (B, b, S, M, m);

    return { B, b, S };
}

static void
remap_suffixes (vector< size_t >& B, const vector< bool >& S)
{
    vector< size_t > arr;

    for (size_t i = 0, j = 0; i < S.size (); ++i) {
        if (S [i] == S.back ()) {
            arr.emplace_back (i);
        }
    }

    for (size_t i = 0; i < B.size (); ++i) {
        B [i] = arr [B [i]];
    }
}

template< typename T >
vector< size_t >
make_S_sa (const vector< size_t >& B, const T& text, const vector< bool >& S)
{
    vector< size_t > sa (text.size (), npos);

    ssize_t max_, min_;
    tie (min_, max_) = detail::minmax (begin (text), end (text));

    vector< size_t > b (max_ - min_ + 1, 0);

    for (size_t i = 0; i < text.size (); ++i) {
        assert (text [i] - min_ >= 0);
        ++b [text [i] - min_];
    }

    auto prev = b [0];
    b [0] = 0;

    for (size_t i = 1; i < b.size (); ++i) {
        const auto tmp = b [i];
        b [i] = b [i - 1] + prev;
        prev = tmp;
    }

    for (size_t i = 0, j = 0; i < text.size (); ++i) {
        if (sa [i] == npos) {
            assert (j < B.size ());
            sa [i] = B [j++];
        }

        const auto prev = sa [i] - 1;

        if (prev != npos && S [prev] == 0) {
            const auto k = text [prev] - min_;
            assert (0 <= k && k < b.size ());

            if (b [k] > i) {
                sa [b [k]++] = prev;
            }
        }
    }

    return sa;
}

template< typename T >
vector< size_t >
make_L_sa (const vector< size_t >& B, const T& text, const vector< bool >& S)
{
    vector< size_t > sa (text.size (), npos);

    ssize_t max_, min_;
    tie (min_, max_) = detail::minmax (begin (text), end (text));

    vector< size_t > b (max_ - min_ + 1, 0);

    for (size_t i = 0; i < text.size (); ++i) {
        assert (text [i] - min_ >= 0);
        ++b [text [i] - min_];
    }

    --b [0];

    for (size_t i = 1; i < b.size (); ++i) {
        b [i] = b [i - 1] + b [i];
    }

    for (size_t i = text.size (), j = B.size () - 1; i > 0; --i) {
        if (sa [i - 1] == npos) {
            assert (j < B.size ());
            sa [i - 1] = B [j--];
        }

        const auto prev = sa [i - 1] - 1;

        if (prev != npos && S [prev] == 1) {
            const auto k = text [prev] - min_;
            assert (0 <= k && k < b.size ());

            if (b [k] < i - 1) {
                sa [b [k]--] = prev;
            }
        }
    }

    return sa;
}


template< typename T >
inline vector< size_t >
make_sa (const vector< size_t >& B, const T& text, const vector< bool >& S)
{
    return S.back ()
        ? make_S_sa (B, text, S)
        : make_L_sa (B, text, S);
}

template< typename T >
vector< size_t >
make_sa (const T& text)
{
    vector< size_t > B;
    vector< bool > b, S;

    tie (B, b, S) = bucketsort_substrings (text);

    if (!all_of (b.begin (), b.end (), [](auto x) { return x; })) {
        B = make_sa (make_T_ (B, b, S));
        remap_suffixes (B, S);
    }

    return make_sa (B, text, S);
}

} // namespace detail

std::vector< size_t >
make_sa (const std::string& text)
{
    return detail::make_sa (text);
}

} // namespace ka
