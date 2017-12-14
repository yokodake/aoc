#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

# test trip severity is 0*3 + 6*4 = 24
my %conn = map {
	/(^\d+): (\d+)/ or die "failed to map";
	$1 => $2;
} <>;

my $severity = 0;
for (keys %conn) {
	$severity += $_ * $conn{$_} if(get_idx($_, $conn{$_}) == 0);
}
say $severity;

sub get_idx {
	my $depth = shift;
	my $range = shift;
	return 0 if($range == 1);
	my $idx = $depth % (($range-1)*2);
	$idx = (($range-1)*2) - $idx if($idx > ($range-1));
	$idx = $idx % $range;
	return $idx;
}

# 00
#    01 02 03 04
# 08 07 06 05
#    09 01 01 02

# 00 01 02 03 04 [ 05 06 07 ]
# 08 09 10 11 12 [ 13 14 15 ]