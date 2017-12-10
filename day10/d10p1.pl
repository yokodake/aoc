#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

my @list;
my @size_list = ( 3, 4, 1, 5 );
my $idx = 0;
my $skip = 0;
my $length;

@list =  make_list(10);

say @nlist;

# returns the reverse list
sub rv {
	my @rlist;


}
# extract : return the sub-list of length
sub ext {
	my @nlist;
	for(0..$length){
		push @nilst, $list[$idx % @list];
	}
	return @nlist;
}

sub make_list {
	my $size = (shift) - 1;
	my @list;
	push @list, $_ for(0..$size);
	return @list;
}