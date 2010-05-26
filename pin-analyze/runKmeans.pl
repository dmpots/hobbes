#!/usr/bin/perl
use Statistics::Descriptive;
use Time::Local;

if (@ARGV != 4) {
  print "usage: Iters Cutoff ProgSets Tool\n";
  exit 1;
}
($Iters, $Cutoff, $ProgSet, $Tool) = @ARGV;
$TmpFile = "__KMEANLOG__";
$Clusters = scalar(split /:/, $ProgSet); #get the length

# setup files
$cmd = "./trainModel.pl 0 $Cutoff $ProgSet $Tool --KMEANS";
if (system("$cmd > $TmpFile") != 0) {
  print "Error executing kmeans setup: $!\n";
  exit 1;
}

# run kmeans
print "USING Iters = $Iters, Threshold = $Cutoff, ProgSets = $ProgSet Clusters = $Clusters PinTool = $Tool\n";
$cmd = "./pinalyze -k $Clusters -i $Iters PREDICT/*";
@results = ();
open LOG, '>', $TmpFile;
open(TRAIN, "$cmd |") || die "Error executing kmeans: $!\n";
while(<TRAIN>) {
  print LOG;
  if(/^Accuracy = (\d+(?:\.\d+)?)%/) {
    print;
    push @results, $1;
    print LOG scalar(localtime), "\n";
  }
}
close TRAIN;
close LOG;

$stat = Statistics::Descriptive::Full->new();
$stat->add_data(@results);
($min, $mean, $max) = ($stat->min(),$stat->geometric_mean(),$stat->max());
printf("STATS(min/mean/max) = %.2f/%.2f/%.2f\n", $min,$mean,$max);

