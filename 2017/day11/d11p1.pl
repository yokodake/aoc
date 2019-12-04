#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

my %tests = ('ne,ne,ne' => 3, 
	'ne,ne,sw,sw' => 0,
	'ne,ne,s,s' => 2,
	'se,sw,se,sw,sw' => 3);

my @path;
my %move = (
	'ne' => [ 0, 1,-1],
	'n'  => [-1, 1, 0],
	'nw' => [-1, 0, 1],
	'sw' => [ 0,-1, 1],
	's'  => [ 1,-1, 0],
	'se' => [ 1, 0,-1]
);

if($ARGV[0] eq 'test') { test(); }
else { say main(split /,/, <>); }

sub main {
	my @path = @_;
	my @coords = (0,0,0);
	
	@coords = add(\@coords, $move{$_}) for @path;
	return dist([0,0,0], \@coords);
}

sub test {
	for(keys %tests) {
		my $result = main(split /,/, $_);
		if($result == $tests{$_}) {
			say "OK";
		} else {
			say "ERROR : $_ => $result ($tests{$_}) ";
		}
	}
}

sub add {
	my @P1 = @{$_[0]};
	my @P2 = @{$_[1]};
	return ($P1[0]+$P2[0], $P1[1]+$P2[1], $P1[2]+$P2[2]);
}
sub dist {
	my @P1 = @{$_[0]};
	my @P2 = @{$_[1]};

	# max of abs(P1.x-P2.x) | abs(P1.y-P2.y) | abs(P1.z-P2.z)
	return (abs($P2[0]-$P1[0]) + abs($P2[1]-$P1[1]) + abs($P2[2]-$P1[2]))/2
}

#  \    /    \    /
#   +--+  02  +--+
#  /    \    /    \
# +  01  +--+  12  +
#  \    /    \    /  
#   +--+  11  +--+
#  /    \    /    
#  + 10  +--+  21  +
#  \    /    \    /  
#   +--+  20  +--+
#  /    \    /    \