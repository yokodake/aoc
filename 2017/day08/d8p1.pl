#!/usr/bin/perl

use strict;
use warnings;
use v5.22;
use Data::Dumper qw(Dumper);

my %registers;

my $dbg = 0;
while(my $line = <>){
	# smi inc 781 if epx > -2
	$line =~ /([a-z]*) ([a-z]{1,3}) (-?\d*) if ([a-z]*) ([=!><]{1,2}) (-?\d*)/s;
	ins($2, $1, $3) if(cond($5, $4, $6));
}
my $max = 0;
for(keys %registers) {
	$max = $registers{$_} if ($registers{$_} > $max);
}
say $max;

sub ins {
	my ($op, $r, $val) = @_;
	$registers{$r} = 0 if ( !exists $registers{$r});
	if   ($op eq "inc") { $registers{$r} += $val; }
	elsif($op eq "dec") { $registers{$r} -= $val; }

}
sub cond {
	my ($cond, $a, $b) = @_;
	$registers{$a} = 0 if(!exists $registers{$a});

	if    ($cond eq "==") { return $registers{$a} == $b; }
	elsif ($cond eq "!=") { return $registers{$a} != $b; }
	elsif ($cond eq ">=") { return $registers{$a} >= $b; }
	elsif ($cond eq "<=") { return $registers{$a} <= $b; }
	elsif ($cond eq ">" ) { return $registers{$a} >  $b;  }
	elsif ($cond eq "<" ) { return $registers{$a} <  $b;  }
}