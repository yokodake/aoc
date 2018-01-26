#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

my %childs;
my %pipes;
say main(<>);

sub main {
	my $count = 0;
	%childs = ();
	%pipes = ();

	for(@_) {
		$_ =~ /^(\d+) <->/s;
		my $parent = $1;
		#my @childs = $_ =~ /(?: )\d(?:,?)/g;
		my @childs = $_ =~ / (\d+),?/gs;
		$childs{$parent} = \@childs;
	}

	prog_hash(0);

	return scalar keys %pipes;
}

sub prog_hash {
	my $parent = shift;
	return 0 if (exists $pipes{$parent});
	
	$pipes{$parent}++;
	prog_hash($_) for(@{$childs{$parent}});
}