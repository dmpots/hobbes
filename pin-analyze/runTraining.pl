#!/usr/bin/perl
use Statistics::Descriptive;

$argCount = $#ARGV + 1;
$iters = shift @ARGV;
if ($iters !~ /^\d+$/) {
  print "usage: runTraining.pl <iters>\n";
  exit 1;
}
#print "$iters\n";
open LOG, '>', "__TRAINLOG__";

my @results = ();
for(my $i = 0; $i < $iters; $i++) {
  open(TRAIN, "./trainModel.pl|") || die "unable to run training";
  while(<TRAIN>) {
    print LOG;
    if(/^Accuracy = (\d+(?:\.\d+)?)%/) {
      print;
      push @results, $1
    }
  }
  close TRAIN;
}
close LOG;

#print "@results\n";
$stat = Statistics::Descriptive::Full->new();
$stat->add_data(@results);
($min, $mean, $max) = ($stat->min(),$stat->geometric_mean(),$stat->max());
printf("STATS(min/mean/max) = %.2f/%.2f/%.2f\n", $min,$mean,$max);



