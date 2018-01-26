#!/usr/bin/perl

use strict;
use warnings;
use 5.022;

my ($total, $invalid) = (0, 0);
while(my $line = <>) {
	chomp $line;
	my @words = sort map { join "", sort split //, $_ } split / /, $line;
	for(0 .. $#words-1) {
		++$invalid && last if $words[$_] eq $words[$_+1];
	}
	$total++;
}

say "part2: ", $total - $invalid;