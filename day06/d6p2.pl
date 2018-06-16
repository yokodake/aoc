#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

my %states;
my @banks = split /\t/, <>;

say (d6p1() - $states{join ",",@banks});

sub d6p1 {
	my $count = 0;
	do {
		$states{join ",", @banks} = $count;
		spread(max(@banks));
		$count++;
	} while(!$states{join ",", @banks});

	return $count;
}
sub max {
	my $max = 0;
	for(0..$#banks){
		$max = $_ if($banks[$_] > $banks[$max]);
	}
	return $max;
}
sub spread {
	my ($i) = @_;
	my $cells = $banks[$i];
	$banks[$i++] = 0;
	while($cells) {
		$banks[$i++ % scalar(@banks)]++;
		$cells--;
	}
}