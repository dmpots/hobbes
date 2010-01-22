#!/usr/bin/perl
use Statistics::Descriptive;
use Time::Local;

BEGIN {$| = 1;} #autoflush
$argCount = $#ARGV + 1;
$iters = shift @ARGV;
if ($iters !~ /^\d+$/) {
  print "usage: runTraining.pl <iters>\n";
  exit 1;
}

$TrainSize = 5;
$Threshold = 0.0;
$ProgSets  = "ALL";
$PinTool   = "opcodemix";
if (@ARGV > 0) {
  my $t = shift @ARGV;
  if ($t !~ /^\d+$/) {print "BAD TrainSize: $t\n"; exit 1;}
  $TrainSize = $t;
}
if (@ARGV > 0) {
  my $t = shift @ARGV;
  if ($t !~ /^\d+(\.\d+)?$/) {print "BAD Threshold: $t\n"; exit 1;}
  if ($t > 1.0)      {print "BAD Threshold: $t\n"; exit 1;}
  $Threshold = $t;
}
if (@ARGV > 0) {
  my $t = shift @ARGV;
  $ProgSets = $t;
}
if (@ARGV > 0) {
  my $t = shift @ARGV;
  if    ($t =~ /opcod/i){$PinTool = "opcodemix";}
  elsif ($t =~ /jump/i) {$PinTool = "jumpmix";}
  elsif ($t =~ /reg/i)  {$PinTool = "regmix";}
  else {print "BAD PinTool: $t"; exit 1;}
}
print "USING TrainSize = $TrainSize, Threshold = $Threshold, ProgSets = $ProgSets PinTool = $PinTool\n";


#print "$iters\n";
open LOG, '>', "__TRAINLOG__";

my @results = ();
my @randResults = ();
for(my $i = 0; $i < $iters; $i++) {
  open(TRAIN, "./trainModel.pl $TrainSize $Threshold $ProgSets $PinTool|") || die "unable to run training";
  while(<TRAIN>) {
    print LOG;
    if(/^Accuracy = (\d+(?:\.\d+)?)%/) {
      print;
      push @results, $1;
      print LOG scalar(localtime), "\n";
    }
    if(/RandIndx = (\d+(?:\.\d+)?)%/) {
      print;
      push @randResults, $1;
    }
  }
  close TRAIN;
}
close LOG;

# Accuracy Summary
$stat = Statistics::Descriptive::Full->new();
$stat->add_data(@results);
($min, $mean, $max) = ($stat->min(),$stat->geometric_mean(),$stat->max());
printf("STATS(min/mean/max) = %.2f/%.2f/%.2f\n", $min,$mean,$max);

# Rand Index Summary
if(@randResults) {
  $stat2 = Statistics::Descriptive::Full->new();
  $stat2->add_data(@randResults);
}
($min, $mean, $max) = ($stat2->min(),$stat2->geometric_mean(),$stat2->max());
printf("RANDStats()         = %.2f/%.2f/%.2f\n", $min,$mean,$max);



