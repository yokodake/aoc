#!/bin/perl

use warnings;
use strict;
use 5.010;

my $fname = "data";

foreach(@ARGV) { $fname="test1" if($_ eq "test"); }
say main($fname);

sub main {
	my $sum=0;
	open(my $f, '<:encoding(UTF-8)', $_[0]) or die "Could not open test $!";
	while (my $line = <$f>){
		# set gt and lt on first number of line
		my ($gt) = $line =~ /(\d+)/;
		my $lt = $gt;
		foreach (split ' ', $line) {
			$gt = $_ if $gt < $_;
			$lt = $_ if $lt > $_;
		}
		$sum += $gt - $lt;
	}
	return $sum;
}