#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

my %childs;
my @groups;

build_childs(<>);
for(keys %childs) {
	next if is_grouped($_); 
	push @groups, build_group($_);
}
say scalar @groups;


sub build_childs {
	for(@_) {
		$_ =~ /^(\d+) <->/s;
		my $parent = $1;
		my @childs = $_ =~ / (\d+),?/gs;
		$childs{$parent} = \@childs;
	}
}

sub is_grouped {
	my $prog = shift;

	for (@groups) {
		return 1 if exists $_->{$prog};
	}
	return 0;
}

sub build_group {
	my $p = shift;
	my %pipes = ();

	add_programs($p, \%pipes);
	# delete $childs{$_} for keys %pipes;
	return \%pipes;
}

sub add_programs {
	my $parent = shift;
	my $pipe_r = shift;
	return 0 if (exists $pipe_r->{$parent});
	
	$pipe_r->{$parent}++;
	for(@{$childs{$parent}}) {
		add_programs($_, $pipe_r);
	}
}