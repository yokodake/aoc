#!/usr/bin/perl

use strict;
use warnings;
use 5.022;

my ($total, $invalid) = (0, 0);

sub sort_string {
	return join "", sort split //, $_[0];
}

while(my $line = <>) {
	chomp $line;
	my @words = sort map { sort_string($_) } split / /, $line;
	for(my $i = 0; $i < @words - 1; $i++) {
		if($words[$i] eq $words[$i+1]) {
			$invalid++;
			last;
		}
	}
	$total++;
}

say "valid: ", $total - $invalid;