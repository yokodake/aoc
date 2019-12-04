#!/bin/perl

use warnings;
use strict;
use 5.024;

foreach(@ARGV) { test() if($_ eq "test"); }

my $data = "data";
open(my $f, '<:encoding(UTF-8)', $data) or die "Could not open $data: $!";

my $valid = 0;
while(my $line = <$f>) {
	# return false if there are dups
	$valid++ if(rgx($line));
}
say $valid;


# we need to find duplicates in a string
# 1. first solution would be to use hash a with each word being the key
sub hash1 {
	my ($str) = @_;
	my %count;
	$count{$_}++ for(split ' ', $str);
	foreach(values %count) {
		return 0 if($_ > 1);
	}
	return 1;
}
#2. efficient and cool way to use hashes
sub hash2 {
	my ($str) = @_;
	my %seen;
	for (split ' ', $str) {
		next unless $seen{$_}++;
		return 0;
	}
	return 1;
}
# 3. regex
sub rgx {
	my ($str) = @_;
	my @words;
	for(split ' ', $str) {
		return 0 if( $str =~ m/$_.*$_/);
	}
	return 1;
}

sub test {
	my %mocks = (
		'vvfl kvvfl olud wjqsqa olud frc' => 0,
		'slhm rdfm yxb rsobyt asdf' => 1,
		'pib rtdxt xyoakcu zoapeze rtdxt rikc jyeps wdyo hawr xyoakcu hawr' => 0,
		'ismtq qwoi kzt ktgzoc gnxblp dzfayil ftfx asscba ionxi dzfayil qwoi' => 0,
		'roan kqnztj edc zpjwb' => 1,
	);
	for(keys %mocks) {
		say "ERROR: $_" if(rgx($_) != $mocks{$_});
	}
	exit;
}