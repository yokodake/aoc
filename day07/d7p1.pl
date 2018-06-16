#!/usr/bin/perl

use strict;
use warnings;
use v5.22;
use Data::Dumper qw(Dumper);

my @name = {"name", "surname"};
my %A = ( 'one' => 1, 'two' => 2 );
my %B = ( 'numbers' => \%A, 'name' => \@name );

say $B{'numbers'};
my $hell = $B{'name'};
say @$hell;
# say getBase($ARGV[0]);
# getBase($ARGV[0]);

sub getBase {
	open my $f, $_[0] or die "failed to open file $!";
	my %test;
	my @doubles;

	while(my $line = <$f>) {
		$line =~ /^(\w*)\s/;
		my $disc = $1;
		$test{$1}++;
		push @doubles, $1 if($test{$1} > 2);

		while($line =~ / ([a-z\s]*)(,|\n)/gs){
			my $m = $1;
			# yes I know, please don't judge me for this line
			$m = substr($m, 0, -1) if (ord(substr($m, -1, 1)) == 13);
			$test{$m}++;
			if($test{$m} > 1) {
				push @doubles, $m;
			}
		}
	}
	for(keys %test){
		my $s = $_;
		if($test{$_} == 1) {
			return $_;
		}
	}
}