#!/usr/bin/perl

use strict;
use warnings;
use v5.22;

my @conn = map {
	/(?:^\d+\: )(\d+)/ or die "failed to map";
	$1;
} <>;

print "$_ \n" for (@conn);

