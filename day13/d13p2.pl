#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

# shortest delay
my %conn = map {
	/(^\d+): (\d+)/ or die "failed to map";
	$1 => $2;
} <>;

my $d = 0;
while(1) {
	last if(pass($d++));
}
say $d-1;

sub pass {
	my $delay = shift;
	for (keys %conn) {
		return 0 if(get_idx($_+$delay, $conn{$_}) == 0);
	}
	return 1;
}
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