#!/bin/perl

use warnings;
use strict;
use 5.024;

my %tval = ( 12 => 3, 23 => 2, 1024 => 31 );
foreach(@ARGV) { 
	if($_ eq "test") {
		foreach(keys %tval) {
			print "$_ => ". main($_);
			if(main($_) != $tval{$_}) {
				say " ($tval{$_})";
			} else {
				say " OK";
			}		
		}
		exit();
	} else { 
		say main($_);
	}
}

sub main {
	my ($input) = @_;
	my $n = 1;
	$n += 2 while($input > $n**2);

	my $corner = $n**2;
	for(0..3) {
		next if ($input > ($n**2)-$_*($n-1));
		$corner = (($n**2)-$_*($n-1));
	}
	my $osteps = int($n/2);
	my $ssteps = abs($corner - $input - int($n/2));
	return $ssteps + $osteps;
}