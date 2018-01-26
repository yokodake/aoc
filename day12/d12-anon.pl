#!/usr/bin/perl

use strict;
use warnings;
use v5.24;
use Data::Dumper qw(Dumper);

my %conn = map {
	/(\d+) <-> (.*)/ or die "failed to map";
	$1 => [split /\s*, \s*/, $2]
} <>;

sub investigate($;$){
    my($id,$ref)=(@_,{});
    
    for(@{$conn{$id}}){
        next if $ref->{$_};
        $ref->{$_}=1;
        investigate($_,$ref)
    }
    
    $ref;
}

say "Part 1: " . scalar keys %{ investigate 0 };
####################################
my $ref={};
my $groupCount=0;
for(sort keys %conn){
    next if $ref->{$_};
    
    investigate $_,$ref;
    $groupCount++;
}
say "Part 2: " . $groupCount;