#!/bin/perl

use warnings;
use strict;
use v5.22;

my $a = $ARGV[0];
test() if($a eq 'test');
say main(<>);

sub main {
	return main_lazy(shift);
}
sub main_lazy {
	my $input = shift;
	# remove ignored chars and commas
	$input =~ s/!.//g;
	# remove garbage
	my $cnt =0;
	my $ignored=0;
	for(split //, $input) {
		if($ignored && $_ eq '>') {
			$ignored =0
		} elsif(!$ignored && $_ eq '<') {
			$ignored = 1;
		}elsif($ignored) {
			$cnt++;
		}

	}
	return $cnt;
}

sub test {
	my %test = ( 
		'<>' => 0,
		'<random characters>' => 17,
		'<<<<>' => 3,
		'<{!>}>' => 2,
		'<!!>' => 0,
		'<!!!>>' => 0,
		'<{o"i!a,<{i<a>' => 10);

	for(keys %test) {
		my $result = main($_);
		if ($result == $test{$_}) {
			say "   OK: $result for $_";
		} else {
			say "ERROR: $result for ($_ => $test{$_})";
		}
	}
	exit;
}
