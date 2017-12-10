#!/bin/perl

use warnings;
use strict;
use v5.26;

my $sum=0;
while (my $line = <>){
	my @row = split ' ', $line;
	for(@row) {
		my $n = $_;
		$sum += ($n/$_)*($n!=$_ && !($n % $_)) for(@row);
	}
}
say $sum;

