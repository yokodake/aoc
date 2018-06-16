package My::KHash;

use warnings;
use strict;

use Exporter qw(import);
our @EXPORT_OK = qw(knot_hash);

my @list;
my @len_list;

sub knot_hash {
  my $idx = 0;
  my $skip = 0;
  my $input = shift;
  chomp($input);
  @list = make_list(256);
  @len_list = make_len($input);

  ($idx, $skip) = round($idx, $skip) for(0..63);
  my @hash = densify(@list);

  return sprintf "%02x" x @hash, @hash;
}
sub round {
  my $idx = shift;
  my $skip = shift;
  my $length;

  for(@len_list) {
    $length = $_;
    my @reversed = reverse ext($idx, $length, @list);
    for(0..($length-1)) {
      $list[($idx+$_) % @list] = $reversed[$_];
    }
    $idx = ($idx +($length + $skip++))% @list;
  }
  return ($idx, $skip);
}
sub densify {
  my @sparse_hash = @_;
  die "block from sparse hash is not 16 bytes" if (@sparse_hash != 256);

  my @dense_hash;
  for (0..15) {
    push @dense_hash, reduce(ext($_*16, 16, @sparse_hash));
  }
  return @dense_hash;

  sub reduce {
    my @block = @_;
    die "block from sparse hash is not 16 bytes" if (@block != 16);

    my $dense = 0;
    for(@block) {
      $dense ^= $_;
    }
    return $dense;
  }
}
# extract : return the sub-list of length
sub ext {
  my $i = shift;
  my $len = shift;
  my @olist = @_;
  my @nlist;
  for(0..($len-1)){
    push @nlist, $olist[($i+$_) % @olist];
  }
  return @nlist;
}
# insert : inserts the new list inside it
sub make_list {
  my $size = (shift) - 1;
  my @list;
  push @list, $_ for(0..$size);
  return @list;
}
sub make_len {
  my $string = shift;
  my @len_list;
  for(split //, $string) {
    push @len_list, ord($_);
  }
  push @len_list, (17,31,73,47,23);
  return @len_list;
}

1;
