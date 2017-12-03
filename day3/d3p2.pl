#!/bin/perl

use warnings;
use strict;
use 5.024;

my %tval = ( 789 => 747, 129 => 133, 17 => 23 );
foreach(@ARGV) { 
	if($_ eq "test") {
		foreach(keys %tval) {
			print "$_ => ". main($_);
			if(main($_) != $tval{$_}) {
				say "   ($tval{$_})";
			} else {
				say "   OK";
			}
		}
	} else {
		my @m = main($_);
		foreach(@m) {
			print "$_ ";
		}
	}
}

sub main {
	my($input) = @_;
	my @spiral = (1,1,2,4,5,10);
	my $n = 6;
	for(6..7) {
		my $sum = $spiral[$_-1];
		# distance from closest corner :
		my ($corner, $side) = getCorner($n);
		my $lsize = getlayer($n);
		if ($lsize-2 == 1){
			$sum++;
		} elsif($side == 0) {

			# treat special case
		} else {
			my $d = $n - $corner;
			say "$d = $n - $corner - $lsize";
			for(my $j = $d+1; $j > $d-2; $j--) {
				my $tmp =0;
				# sum with the value at the corner of PREVIOUS LAYER + $j
				$tmp += ((int($lsize/2)-1)**2)-$side*(int($lsize/2)-2) if($j >= 0 && $j < $lsize);
				$sum += $tmp;
				say "d: $tmp";
			}
		} 
		if(@spiral)
		push @spiral, $sum;
		$n++;
	}
	return @spiral;
}

sub getsteps {
	my ($input) = @_;
	my $n = 1;
	$n += 2 while($input > $n**2);

	my $corner = $n**2;
	for(0..3) {
		next if ($input > ($n**2)-$_*($n-1));
		$corner = (($n**2)-$_*($n-1));
	}

	my $layer = int($n/2);
	# steps from center of side
	my $ssteps = abs($corner - $input - int($n/2));

	return ($ssteps, $layer);
}

# get the layer based on the index
sub getlayer {
	my ($index) = @_;
	my $n = 1;
	$n += 2 while($index > $n**2);
	return $n;
}

# get the index of the previous corner
sub getCorner {
	my ($val)= @_;
	my $l = getlayer($val);
	my $corner = ($l**2);
	my $side = 0;
	for(1..3) {
		if ($val < $corner) {
			$corner = (($l**2)-$_*($l-1));
			$side = $_;
		}
	}
	return ($corner, $side);
}