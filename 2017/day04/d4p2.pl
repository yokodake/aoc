#!/bin/perl

use warnings;
use strict;
use 5.024;

foreach(@ARGV) { test() if($_ eq "test"); }

my $data = "data";
open(my $f, '<:encoding(UTF-8)', $data) or die "Could not open $data: $!";

my $valid = 0;
while(<$f>) {
	chomp;
	# return false if there are dups
	$valid++ if(main($_));
}
say $valid;


sub main {
	my ($str) = @_;
	my %seen;
	for (split ' ', $str) {
		next unless $seen{hash($_)}++;
		return 0;
	}
	return 1;
}

# use the hash algo from java.lang.String
# todo : change the ugly part where I change the string into an array & sort
sub hash {
	my @char = split //, $_[0];
	my $h = 0;
	return $h if ($#char < 1);
	for(sort @char) {
		$h = 31*$h + ord($_);
	}
	return $h;
}

sub test {
	my %mocks = (
		'abcde fghij' => 1,
		'abcde xyz ecdab' => 0,
		'a ab abc abd abf abj' => 1,
		'iiii oiii ooii oooi oooo' => 1,
		'oiii ioii iioi iiio' => 0,
	);
	for(keys %mocks) {
		say "ERROR: $_" if(main($_) != $mocks{$_});
	}
	say hash("abcde") ." ". hash("ecdab");
	exit;
}