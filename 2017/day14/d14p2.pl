#!/usr/bin/perl
use warnings;
use strict;
use v5.26;

use File::Basename qw(dirname);
use Cwd  qw(abs_path);
use lib dirname(dirname abs_path $0);

use My::KHash qw(knot_hash);
use List::Util qw(reduce);
use Data::Dumper;

my @test = (
    [1,1,0,1,1,0,1,0,1],
    [0,1,0,0,1,1,1,0,1],
    [0,1,0,0,0,0,1,0,1],
    [0,1,0,0,0,1,1,1,1],
    [1,1,0,1,1,1,1,1,0]
    );

# my ($cnt, $href) = traverse(@test);
my ($cnt, $href) = traverse(mkArr("flqrgnkx"));
my $example = $cnt - lenNoDups($href);
die "Test failed. Actual : $example" if ($example != 1242);

($cnt, $href) = traverse(mkArr("ugkiagan"));
say ($cnt - lenNoDups($href));

sub traverse {
    my @AoA = @_;
    my $id = 1;
    my %alias = ( 1 => 1);
    my $len = scalar @{$AoA[0]};

    for (my $y = 0; $y < scalar @AoA; $y++) {
        for (my $x = 0; $x < $len; $x++) {
            next if($AoA[$y]->[$x] == 0);
            my $p = $x > 0 ? $AoA[$y]->[$x-1] : 0; # previous on row
            my $u = $y > 0 ? $AoA[$y-1]->[$x] : 0; # previous on column
            $AoA[$y]->[$x] = chvalue($p, $u, \%alias, \$id);
        }
    }
    sub chvalue {
        my ($p, $u, $aref, $id) = @_;
        if($p != 0 && $u != 0) {
            combine($p, $u, $aref); # joining of two regions
            return $aref->{$p};
        } elsif($p != 0) {
            return $aref->{$p}; # continue previous region
        } elsif($u != 0) {
            return $aref->{$u}; # continue previous region
        } else {
            my $r = $$id++;
            $aref->{$$id} = $$id;
            return $r;
        }
    }
    return ($id-1, \%alias);
}

sub lenNoDups {
    my ($href) = @_;
    my $len = scalar keys %$href;
    for (keys %$href) {
        $len-- if($href->{$_} == $_);
    }
    return $len;
}

sub combine {
    my ($a, $b, $href) = @_;
    my ($k, $v) = (max($a, $b), min($a, $b));
    if (defined $href->{$k} && defined $href->{$v}) {
        my ($kval, $vval) = ($href->{$k}, $href->{$v});
        my ($old, $new) = (max($kval, $vval), min($kval, $vval));
        replaceall($old, $new, $href);
    } elsif(defined $href->{$k}) {
        $href->{$v} = $href->{$k};
    } elsif(defined $href->{$v}) {
        $href->{$k} = $href->{$v};
    } else {
        $href->{$k} = $v;
    }
}
sub replaceall {
    my ($old, $new, $hashref) = @_;
    for (keys %$hashref) {
        my $k = $_;
        $hashref->{$k} = $new if($hashref->{$k} eq $old);
    }
}

sub max {
    my ($a, $b) = @_;
    return $a > $b ? $a : $b;
}
sub min {
    my ($a, $b) = @_;
    return $a < $b ? $a : $b;
}

sub mkArr {
    my ($in) = @_;
    my @bins = map { convert($_) } inputArr($in);
    return @bins;

    sub inputArr {
        my ($in) = @_;
        return map { knot_hash("$in-$_") } @{[0..127]};
    }
    sub convert {
        my ($in) = @_;
        my @arr = map { sprintf "%04b", hex } split //, $in;
        my @arr2 = map { split //, $_ } @arr;
        return \@arr2;
    }
}
