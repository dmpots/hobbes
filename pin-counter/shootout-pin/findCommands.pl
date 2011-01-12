#!/usr/bin/perl

use File::Basename;

$target = "gcc";
if (@ARGV > 0) {
  $target = shift @ARGV;
}

while (<>) {
  chomp;
  my ($bm,$cmd) = split /\|/;

  if($bm =~ /$target/) {
    my $dir = dirname($cmd);
    my $exe = basename($cmd);
    print "$bm|$dir|./$exe > $bm.STDOUT\n";
  }
}



