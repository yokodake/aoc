#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

use File::Basename qw(dirname);
use Cwd qw(abs_path);
use lib dirname(dirname abs_path $0);

use Aoc::Day10 qw(knot_hash);

use Inline 'C' => Config => BUILD_NOISY => 1, CLEAN_AFTER_BUILD => 0;
use Inline 'C' => <<'END_C';

#include <stdint.h>

unsigned popcnt2( SV *sv ) {
    unsigned count = 0;

    #ifdef __LP64__
        static const uint64_t m1  = UINT64_C(0x5555555555555555);
        static const uint64_t m2  = UINT64_C(0x3333333333333333);
        static const uint64_t m4  = UINT64_C(0x0f0f0f0f0f0f0f0f);
        static const uint64_t h01 = UINT64_C(0x0101010101010101);

        uint64_t x = (uint64_t) SvUVX( sv );

        x =  x       - ((x >> 1)  & m1);
        x = (x & m2) + ((x >> 2)  & m2);
        x = (x       +  (x >> 4)) & m4;

        count += (unsigned) ((x * h01) >> 56);
    #else
        static const uint32_t m1  = UINT32_C(0x55555555);
        static const uint32_t m2  = UINT32_C(0x33333333);
        static const uint32_t m4  = UINT32_C(0x0f0f0f0f);
        static const uint32_t h01 = UINT32_C(0x01010101);

        uint32_t x = (uint32_t) SvUVX( sv );

        x =  x       - ((x >> 1)  & m1);
        x = (x & m2) + ((x >> 2)  & m2);
        x = (x       +  (x >> 4)) & m4;

        count += (unsigned) ((x * h01) >> 24);
    #endif

    return count;
}

END_C

my $input = shift;

my $hash = knot_hash($input);
say $hash;

say popcnt2(hex substr $hash, $_*8, 8) for(0..3);