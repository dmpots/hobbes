#!/usr/bin/perl
#
#
$LoopSize   = 100
@Tools      = qw(opcodemix jumpmix);
@TrainSizes = qw(10 05 03);
@Cutoffs    = qw(0.10 0.05 0.00)
@ProgSets   = qw(ALL HGCC HICC SPEC)

for my $tool      @Tools      {
for my $trainSize @TrainSizes {
for my $cutoff    @Cutoffs    {
for my $spec      @ProgSets   {
  $outFile = "RESULTS/svm.$tool.$trainSize.-$cutoff-.$spec";
  $cmd = "./runTraining.pl $LoopSize $trainSize $cutoff $spec $tool | tee $outFile";
  $mailCmd = "mail -s \"[TRAIN] finished\" dmp\@rice.edu < $outFile";
  print "$cmd\n";
  $rc = system($cmd);
  if ($rc != 0) {
      print "ERROR: $!";
      exit 1;
  }
  print "$mailCmd\n";
  $rc = system($mailCmd);
  if($rc != 0) {
      print "ERROR: $!";
      exit 1;
  }
}
}
}
}


