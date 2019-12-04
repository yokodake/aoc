#! /usr/bin/env perl6
use strict;

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
        repeat {
            $ret = &$next();
        } while ($ret % $cond) != 0;
        return $ret;
    }
}
sub cmphextet {
    my ($a, $b) = @_;
    return ($a +& 0xffff) == ($b +& 0xffff);
}

# part 1
my $gena = generator1(2147483647, 16807, 703);
my $genb = generator1(2147483647, 48271, 516);

#$gena = generator1(2147483647, 16807, 65);
#$genb = generator1(2147483647, 48271, 8921);
my $iterations = 40_000_000;
my $acc1 = 0;

loop (my $i =1; $i <= $iterations; $i++) {
    $acc1++ if cmphextet(&$gena(), &$genb());
    say "$i" if $i % ($iterations/100) == 0;
}
say "part1 : $acc1";

# part 2
$gena = generator2(2147483647, 16807, 703, 4);
$genb = generator2(2147483647, 48271, 516, 8);
my $acc2 = 0;
$iterations = 5_000_000;

loop ($i =1; $i <= $iterations; $i++) {
    $acc2++ if cmphextet(&$gena(), &$genb());
    say "$i" if $i % ($iterations/100) == 0;
}

say "part2 : $acc2";
