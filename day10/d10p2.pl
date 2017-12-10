#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

my @list;
my @len_list;

my %tests = ( '' => 'a2582a3a0e66e6e86e3812dcb672a272',
		   'AoC 2017' => '33efeb34ea91902bb2f59c9920caa6cd',
		   '1,2,3' => '3efbe78a8d82f29979031a4aa0b16a9d', 
		   '1,2,4' => '63960835bcdc130f0b66d7ff4f6a5a8e');

if($ARGV[0] eq 'test') { 
	for(keys %tests) {
		my $result = main($_);
		say "\"$_\" ($result)" if($result ne $tests{$_});
	}
} else { 
	say main(<>);
}


sub main {
	my $idx = 0;
	my $skip = 0;
	my $input = shift;
	@list = make_list(256);
	@len_list = make_len($input);

	($idx, $skip) = round($idx, $skip) for(0..63);
	my @hash = densify(@list);

	return sprintf "%02x" x @hash, @hash;
}
sub round {
	my $idx = shift;
	my $skip = shift;
	my $length;

	for(@len_list) {
		$length = $_;
		my @reversed = reverse ext($idx, $length, @list);
		for(0..($length-1)) {
			$list[($idx+$_) % @list] = $reversed[$_];
		}
		$idx = ($idx +($length + $skip++))% @list;
	}
	return ($idx, $skip);
}
sub densify {
	my @sparse_hash = @_;
	die "block from sparse hash is not 16 bytes" if (@sparse_hash != 256);
	
	my @dense_hash;
	for (0..15) {
		push @dense_hash, reduce(ext($_*16, 16, @sparse_hash));
	}
	return @dense_hash;

	sub reduce {
		my @block = @_;
		die "block from sparse hash is not 16 bytes" if (@block != 16);

		my $dense = 0;
		for(@block) {
			$dense ^= $_;
		}
		return $dense;
	}
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
sub make_list {
	my $size = (shift) - 1;
	my @list;
	push @list, $_ for(0..$size);
	return @list;
}
sub make_len {
	my $string = shift;
	my @len_list;
	for(split //, $string) {
		push @len_list, ord($_);
	}
	push @len_list, (17,31,73,47,23);
	return @len_list;
}