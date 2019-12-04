#!/usr/bin/perl

use strict;
use warnings;
use 5.022;

my @ins = <>;

my $i=0;
my $steps=0;
while($i < scalar(@ins)) {
	$i += ($ins[$i] < 3) ? $ins[$i]++ : $ins[$i]--;
	$steps++;
}
say $steps;
