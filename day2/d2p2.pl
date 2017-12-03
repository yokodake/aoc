#!/bin/perl

use warnings;
use strict;
use 5.010;
use Scalar::Util::Numeric qw(isint);

my $fname = "data";

foreach(@ARGV) { $fname="test2" if($_ eq "test"); }
say main($fname);

sub main {
	my $sum=0;
	open(my $f, '<:encoding(UTF-8)', $_[0]) or die "Could not open test $!";
	while (my $line = <$f>){
		my @row = split ' ', $line;
		foreach(@row) {
			my $r = 0;
			my $n = $_;
			foreach(@row) {
				next if $n == $_;
				my $div = $n/$_;
				if(isint $div) {
					$sum += $div;
				}
			}
		}
	}
	return $sum;
}