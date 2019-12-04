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
	$input =~ s/!.|,//g;
	# remove garbage
	my $sum = 0;
	my $d=1;
	my $ignored;
	for(split //, $input) {
		if($_ eq '<') {
			$ignored = 1;
		}
		elsif($_ eq '>') {
			$ignored = 0;
		}
		elsif(!$ignored) {
			$sum += $d++ if($_ eq '{');
			$d-- if($_ eq '}');
		}
	}
	return $sum;
}

sub test {
	my %test = ( 
		'{}' => 1,
		'{{{}}}' => 6,
		'{{},{}}' => 5,
		'{{{},{},{{}}}}' => 16,
		'{<a>,<a>,<a>,<a>}' => 1,
		'{{<ab>},{<ab>},{<ab>},{<ab>}}' => 9,
		'{{<!!>},{<!!>},{<!!>},{<!!>}}' => 9,
		'{{<a!>},{<a!>},{<a!>},{<ab>}}' => 3 );

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
