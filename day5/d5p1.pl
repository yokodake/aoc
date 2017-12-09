#!/bin/perl

use strict;
use warnings;
use 5.022;

my @instructions = <>; 
my $steps = 0;
my $i=0;
while($i < scalar(@instructions)) {
	$i += $instructions[$i]++;
	$steps++;
}
say $steps;
