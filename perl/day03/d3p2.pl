#!/bin/perl

use warnings;
use strict;
use 5.024;

my $dbg = 0;
my $test = 0;
my %tval = ( 789 => 806, 129 => 133, 17 => 23 );
my $arg = 1;
foreach(@ARGV) { 
	if($_ eq "test") { $test = 1; }
	elsif($_ eq "-d") { $dbg = 1;  }
	else { $arg =$_; }
}

if($test) {
	foreach(keys %tval) {
		my $m = main($_);
		say "$_ => ". $m;
		die "not $tval{$_} but ".$m if($m != $tval{$_});
	}
} else {
	if ($dbg) {
		my @m = main($arg);
		foreach(@m) {
			print "$_ ";
		}
	} else {
		say main($arg);
	}
}

sub main {
	my($input) = @_;
	my @spiral = (1,1,2,4,5,10,11,23,25);
	foreach(@spiral) {
		return $_ if ($_ > $input);
	}
	my $idx = $#spiral +1;

	# return the spiral once the last value is bigger than input
	while($input > $spiral[$idx-1]) {
		my $n = $idx + 1; # spiral starts at 1
		my $sum = $spiral[$idx-1];

		my ($corner, $side) = getCorner($n);
		my $lsize = getlayersize($n);
		print "n:$n c:$corner($side) ls:$lsize " if($dbg);
		if ($lsize-2 == 1){
			$sum++;
		} else {
			my $d = $n - $corner;
			print "dist: $d [ " if($dbg);
			# we sum up to 3 values on the previous layer :
			# distance from previous corner (current layer) -2, -1, -0
			# in order to work on corners we need to following rules :
			# 0 <= {d-2,d-1,d-0} < layersize

			#prev layer corner :
			my $pc = (($lsize-2)**2) - $side*(($lsize-2)-1); 
			print "pc: $pc " if($dbg);
			for(0..2) {
				my $tmp = 0;
				# sum with the value at the corner of PREVIOUS LAYER + $j
				# reminder : index = n-1 & check if within bounds
				my $pln = $pc + ($d-$_);
				$tmp = $spiral[$pln-1]
					if (($d-$_) >= 0 && ($d-$_) < $lsize-2 && $pln != ($lsize-4)**2);
				$sum += $tmp;
				print "+$tmp($pln) " if($tmp && $dbg);
			}
			print "] " if($dbg);
		}
		#treat special case when previous is a corner
		if(isCorner($n-1)) {
			print "+ $spiral[$idx-2] " if($dbg);
			$sum += $spiral[$idx-2];
		}
		# and special case where next one is a corner
		if($n+1 == $lsize**2 || $n == $lsize**2){
			print "+ $spiral[$idx-2] " if($dbg);
			my $pln = ($lsize-2)**2; # index = (n-1)²+1 -1
			$sum += $spiral[$pln]
		}

		say "sum: $sum" if($dbg);
		push @spiral, $sum;
		$idx++;
	}
	
	return $spiral[$idx-1] if (!$dbg || $test);
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
sub getlayersize {
	my ($index) = @_;
	my $ls = 1;
	$ls += 2 while($index > $ls**2);
	return $ls;
}

# get the index of the previous corner
sub getCorner {
	my ($val)= @_;
	my $l = getlayersize($val);
	my $corner = ($l**2);
	my $side = 0;
	for(1..4) {
		# case $_=4 is (n-1)² so the prev layer bottom right corner will be the
		# previous corner for all the values on the right side of square.
		if ($val <= $corner) {
			$corner = (($l**2)-$_*($l-1));
			$side = $_;
		}
	}
	return ($corner, $side);
}

sub isCorner {
	my ($val)=@_;
	my $ls = getlayersize($val);
	for(1..3) {
		return 1 if($val == (($ls**2)-$_*($ls-1)));
	}
	return 1 if($val == (($ls-2)**2)+1);
	return 0;
}