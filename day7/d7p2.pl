#!/usr/bin/perl

use strict;
use warnings;
use v5.22;
use Data::Dumper qw(Dumper);


# the shittiest challenge I ever did, please don't read this code

my $file = shift;
my %tower;
my %weight;

traverse(getBase($file));
#sum(getBase($file));

sub traverse {
	my $disc = shift;
	my @subs = @{$tower{$disc}};

	sum_v($disc);
	for(@subs) {
		traverse($_);
	}
}

sub sum_v {
	my $disc = shift;
	my @subs = @{$tower{$disc}};
	my $weight_sum = $weight{$disc};

	my $weight_v=0;
	for(@subs) {
		my $w = sum_v($_);
		$weight_sum += $w;
		$weight_v = $w if(!$weight_v);
		if($weight_v != $w) {
			print_disc($disc);
			exit;
		}

	}
	return $weight_sum;
}

sub print_disc {
	my $disc = shift;
	my @subs = @{$tower{$disc}};

	print "$disc ($weight{$disc}) -> ";
	for(@subs) {
		print "$_ ($weight{$_}) ";
	}
	print "\n";
}

sub getBase {
	open my $f, shift or die "failed to open file $!";
	my %count;

	while(my $line = <$f>) {
		my @disc; # array containing current disc which will be pushed on %tower

		$line =~ /^(\w*)\s\((\d*)\)/;

		if(!$count{$1}) { $count{$1}++; }
		else { delete $count{$1};}
		$weight{$1} = $2;

		while($line =~ / ([a-z\s]*)(,|\n)/gs){
			my $m = $1;

			# yes I know, please don't judge me for this line
			$m = substr($m, 0, -1) if (ord(substr($m, -1, 1)) == 13);
			push @disc, $m;

			if(!$count{$m}) { $count{$m}++; }
			else { delete $count{$m};}
		}
		$tower{$1}=[@disc];
	}
	for(keys %count){
		return $_;
	}
}