#!/usr/bin/perl
use warnings;
use strict;
use v5.26;

use File::Basename qw(dirname);
use Cwd  qw(abs_path);
use lib dirname(dirname abs_path $0);

use My::KHash qw(knot_hash);
use List::Util qw(reduce);

my $test = main("flqrgnkx");
die "Test failed. Actual : $test" if ($test != 8108);
say "Test succeeded.";
print main("ugkiagan");

# step 1 : make the array
# mk arr
sub inputArr {
    my ($in) = @_;
    return map { "$in-$_" } @{[0..127]};
}

# step 2 : convert to binary
sub convert {
    my ($in) = @_;
    my @arr = map { sprintf "%04b", hex } split //, $in;
    return join "", @arr;
}
# step 3 : count
sub count {
    my @linecounts = map { $_ =~ tr/1// } @_;
    return reduce { $a + $b } @linecounts;
}

# step 4 : compose
sub main {
    my ($in) = @_;
    my @knots = map { knot_hash($_) } inputArr($in);
    my @bins = map { convert($_) } @knots;
    return count(@bins);
}
