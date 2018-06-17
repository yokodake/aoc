#! /usr/bin/perl
use strict;
use warnings;
use v5.26;

sub generator1 {
    my ($div, $mul, $start) = @_;
    return sub {
        $start = ($mul * $start ) % $div;
        return $start;
    }
}
sub generator2 {
    my ($div, $mul, $start, $cond) = @_;
    my $next = generator1($div, $mul, $start);
    return sub {
        my $ret;
        do {
            $ret = &$next();
        } while($ret % $cond != 0);
        return $ret;
    }
}
sub cmphextet {
    my ($a, $b) = @_;
    return ($a & 0xffff) == ($b & 0xffff);
}

# part 1
my $gena = generator1(2147483647, 16807, 703);
my $genb = generator1(2147483647, 48271, 516);
my $iterations = 40_000_000;
my $acc1 = 0;
for(my $i =1; $i <= $iterations; $i++) {
    $acc1++ if(cmphextet &$gena(), &$genb());
}

# part 2
$gena = generator2(2147483647, 16807, 703, 4);
$genb = generator2(2147483647, 48271, 516, 8);
my $acc2 = 0;
$iterations = 5_000_000;
for(my $i =1; $i <= $iterations; $i++) {
    $acc2++ if(cmphextet &$gena(), &$genb());
}
say "part1 : $acc1";
say "part2 : $acc2";
