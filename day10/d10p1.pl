#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

my @list;
my @size_list;
my $idx = 0;
my $skip = 0;
my $length;

if($ARGV[0] eq 'test') { @list = make_list(5); @size_list = ( 3, 4, 1, 5 ); }
else { @list = make_list(256); @size_list = split ',', <>;}

say round();

sub round {
	for(@size_list) {
		$length = $_;
		# debug();
		ins($idx, $length, reverse ext($idx, $length, @list));
		# say @list;
		$idx = ($idx +($length + $skip++))% @list;
	}
	return ($list[0] * $list[1]);
}

# extract : return the sub-list of length
sub ext {
	my $i = shift;
	my $len = shift;
	my @olist = @_;
	my @nlist;
	for(0..($len-1)){
		push @nlist, $olist[($i+$_) % @olist];
	}
	return @nlist;
}
# insert : inserts the new list inside it
sub ins {
	my $i = shift;
	my $len = shift;
	my @nlist = @_;
	for(0..($len-1)){
		$list[($i+$_) % @list] = $nlist[$_];
	}
}
sub make_list {
	my $size = (shift) - 1;
	my @list;
	push @list, $_ for(0..$size);
	return @list;
}
sub debug {
	print "i:$idx l:$length s:$skip  ";
}